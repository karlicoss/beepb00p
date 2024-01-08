#!/usr/bin/env python3
from concurrent.futures import ProcessPoolExecutor
from dataclasses import dataclass
from pathlib import Path
import sys
from shutil import rmtree, copy
from subprocess import check_call, check_output, run
from typing import List


from loguru import logger


def ccall(*args, **kwargs):
    print(args, file=sys.stderr)
    return check_call(*args, **kwargs)

# TODO editable?
# TODO huh, that actually would be nice. I could fix stuff in-place and then apply to org-mode
# TODO search settings?
# TODO wonder if I can integrate it with blog's header?
# TODO export txt files as md as well?

from compile_org import emacs
from utils import classproperty


_root = Path(__file__).absolute().parent.parent
src  = _root / 'src'

DATA: Path

# note: need to resolve, otherwise relative links might end up weird during org-publish-cache-ctime-of-src
class cfg:
    @classproperty
    def input_dir(cls) -> Path:
        return (DATA / 'input' ).resolve()

    @classproperty
    def public_dir(cls) -> Path:
        return (DATA / 'public').resolve()

    @classproperty
    def md_dir(cls) -> Path:
        return (DATA / 'md'    ).resolve()

    @classproperty
    def html_dir(cls) -> Path:
        return (DATA / 'html'  ).resolve()


def clean_dir(path: Path) -> None:
    assert path.is_dir(), path
    for x in path.iterdir():
        if x.name == '.git':
            continue
        if x.is_file() or x.is_symlink():
            x.unlink()
        else: # dir
            rmtree(x)


def clean(*, skip_org_export: bool) -> None:
    # todo ugh, need symlink watching tool here again...
    # org-publish-timestamp-directory
    cachedir = Path('~/.org-timestamps').expanduser()
    # right, so it keeps track of modifications and doesn't publish if it wasn't modified
    # but completely unclear how it behaves wrt added/removed files etc
    # also wouldn't know if the source code was modified... so best not to use it I guess
    # also this... https://emacs.stackexchange.com/questions/44534/org-mode-sitemap-not-updated-after-re-publish
    # ugh. there is org-publish-use-timestamps-flag, but I'm not sure if it's respected
    for c in cachedir.glob('*.cache'):
        c.unlink()

    # it's really really annoying it's so hard to implement this properly
    # basically specifying all the inputs and outputs (bazel-like) is the only way to go I guess
    # but it's not really possible considering all the dynamic stuff during building. shit

    clean_dir(cfg.md_dir)
    clean_dir(cfg.html_dir)

    # TODO what about empty dirs?
    # right -- we don't want to use clean_dir because there is also gitignore/license here. ugh
    if not skip_org_export:
        for f in cfg.public_dir.rglob('*.org'):
            f.unlink()


# TODO check noexport tag; remove "hide"
def main() -> None:
    import argparse
    p = argparse.ArgumentParser()
    p.add_argument('--add'   , action='store_true')
    p.add_argument('--filter', type=str, default=None)
    p.add_argument('--no-html', action='store_false', dest='html')
    p.add_argument('--md'     , action='store_true')
    p.add_argument('--watch', action='store_true')
    p.add_argument('--under-entr', action='store_true') # ugh.
    p.add_argument('--data-dir', type=Path)
    p.add_argument('--use-new-org-export', action='store_true')
    p.add_argument('--skip-org-export', action='store_true')
    p.add_argument('--use-new-html-export', action='store_true')
    args = p.parse_args()

    assert not args.md  # broken for now

    skip_org_export: bool = args.skip_org_export

    ddir = args.data_dir
    global DATA
    if ddir is None:
        DATA = _root / 'data'
    else:
        DATA = ddir.absolute()

    # ugh. this all is pretty complicated...
    if args.watch:
        clean(skip_org_export=skip_org_export)
        nargs = [*sys.argv, '--under-entr']
        nargs.remove('--watch')
        while True:
            paths = '\n'.join(str(p) for p in cfg.input_dir.rglob('*') if '.git' not in p.parts)
            run(['entr', '-d', *nargs], input=paths.encode('utf8'))
        sys.exit(0)
    if not args.under_entr:
        clean(skip_org_export=skip_org_export)

    preprocess(args, skip_org_export=skip_org_export)
    if args.html:
        postprocess_html(use_new_html_export=args.use_new_html_export)


@dataclass
class Context:
    input_dir: Path
    public_dir: Path


def compile_org_to_org(f: Path, ctx: Context) -> None:
    rpath = f.relative_to(ctx.input_dir)
    target = ctx.public_dir / rpath
    target = target.absolute()  # emacs seems unhappy if we don't do it
    target.parent.mkdir(parents=True, exist_ok=True)
    print('exporting', f, 'to', target)
    check_call(['emacs', '--batch', '-l', Path('testdata') / 'export.el', f, target])
    tres = target.read_text()
    import orgparse
    import re
    ts_re = orgparse.date.TIMESTAMP_RE
    tres = ts_re.sub(r'[\g<inactive_year>-\g<inactive_month>-\g<inactive_day>]', tres)
    target.write_text(tres)

    from fixup_org import fixup
    target.write_text(fixup(target.read_text()))
    # TODO hiding tags from export (e.g. 'refile') -- will need to be implemented manually?
    # TODO need to test it!


def preprocess(args, *, skip_org_export: bool) -> None:
    """
    Publishies intermediate ('public') org-mode?
    """
    public_dir = cfg.public_dir
    input_dir  = cfg.input_dir

    filter = args.filter
    efilter = 'nil' if filter is None else rf"""'(:exclude "\\.*" :include ("{filter}"))"""

    assert input_dir.exists(), input_dir
    eargs = [
        '--eval', f'''(progn
            (setq exobrain/input-dir  "{input_dir}" )
            (setq exobrain/public-dir "{public_dir}")
            (setq exobrain/md-dir     "{cfg.md_dir}"    )
            (setq exobrain/html-dir   "{cfg.html_dir}"  )
            (setq exobrain/filter     {efilter}     )
        )''',
        '--directory', src / 'advice-patch',
        # '-f', 'toggle-debug-on-error', # dumps stacktrace on error
    ]
    ctx = Context(input_dir=input_dir, public_dir=public_dir)
    if not skip_org_export and args.use_new_org_export:
        inputs = sorted(input_dir.rglob('*.org'))
        with ProcessPoolExecutor() as pool:
            logger.debug(f'using {pool._max_workers} workers')
            futures = [pool.submit(compile_org_to_org, i, ctx) for i in inputs]
            for i, fut in zip(inputs, futures):
                try:
                    fut.result()
                except Exception as e:
                    raise RuntimeError(f'error while processing {i}') from e
    elif not skip_org_export:
        # TODO get rid of this..
        with emacs(
                *eargs,
                '--load'     , src / 'publish.el',  # old file
                '--eval',
                f'''(let ((org-publish-project-alist `(,exobrain/project-preprocess-org)))
                      (org-publish-all))''',
           ) as ep:
            pass
        assert ep.returncode == 0

    from check import check_org
    check_org(public_dir)

    # TODO think about commit/push/deploy logic?
    # TODO not sure why I had this? prob don't want it...
    # assert (public_dir / '.git').is_dir(), public_dir
    # ccall(['git', 'status'], cwd=public_dir)

    if args.add:
        raise RuntimeError('Temporary unsupported')
        ccall(['git', 'add', '-A', '--intent-to-add'], cwd=public_dir)
        ccall(['git', 'add', '-p'], cwd=public_dir)
        # TODO suggest to commit/push?

    if args.html or args.md:
        # TODO might want both?
        mode = 'html' if args.html else 'md'
        prj = 'exobrain/project-org2html' if args.html else 'exobrain/project-org2md'
        hargs = [
            '--load', src / 'publish_new.el',
        ] if args.use_new_html_export else [
            '--load', src / 'publish.el',
        ]
        with emacs(
                # ugh. such crap
                *([] if args.html else ['--eval', '(setq markdown t)']),
                *eargs,
                *hargs,
                '--eval',
                f'''(let ((org-publish-project-alist `(,{prj})))
                        (org-publish-all))''',
        ) as ep:
            pass
        assert ep.returncode == 0

    # for f in public_dir.rglob('*.org'):
    #     assert not f.is_symlink(), f # just in case?
    #     check_call(['chmod', '-w', f]) # prevent editing

def relativize(soup, *, path: Path, root: Path):
    depth = len(path.relative_to(root).parts) - 1
    rel = ('' if depth == 0 else '../' * depth)

    for tag, attr in [('a', 'href'), ('link', 'href'), ('script', 'src')]:
        for t in soup.find_all(tag):
            link = t.get(attr) or ''
            if not link.startswith('/'):
                continue
            t[attr] = rel + link[1:]
    return soup


def postprocess_html(*, use_new_html_export: bool) -> None:
    html_dir = cfg.html_dir

    copy(src / 'search/search.css', html_dir / 'search.css'  )
    copy(src / 'search/search.js' , html_dir / 'search.js'   )
    copy(src / 'exobrain.css'     , html_dir / 'exobrain.css')
    copy(src / 'settings.js'      , html_dir / 'settings.js' )

    from bs4 import BeautifulSoup as BS # type: ignore
    bs = lambda x: BS(x, 'lxml')  # lxml is the fastest? see https://www.crummy.com/software/BeautifulSoup/bs4/doc/#installing-a-parser

    # for fucks sake, seems that it's not possible to insert raw html?
    def ashtml(x):
        b = bs(x)
        head = list(b.head or [])
        body = list(b.body or [])
        return head + body


    sitemap = html_dir / 'sitemap.html'
    assert sitemap.exists()
    node = bs(sitemap.read_text()).find(id='content')
    node.select_one('.title').decompose()
    node.name = 'nav' # div by deafult
    node['id'] = 'exobrain-toc'
    tocs = str(node) # do not prettify to avoid too many newlines around tags
    # mm, org-mode emits relative links, but we actually want abolute here so it makes sense for any page
    tocs = tocs.replace('href="', 'href="/')

    if use_new_html_export:
        from fixup_html import fixup
        for html in html_dir.rglob('*.html'):
            text = html.read_text()
            soup = bs(text)
            try:
                fixup(soup)
            except Exception as e:
                raise RuntimeError(f'while fixing {html}') from e

            ## FIXME backwards compat with older export, can remove later
            sstr = str(soup)
            sstr = sstr.replace(
                '</div><div class="outline-2"',
                '</div>\n<div class="outline-2"',
            )
            sstr = sstr.replace(
                '<div class="properties"><div class="property" ',
                '<div class="properties">\n<div class="property" ',
            )
            sstr = sstr.replace(
                '</span></div></div>',
                '</span></div>\n</div>',
            )
            ##

            # super annoying (is this nbsp?), should be via css
            sstr = sstr.replace('\xa0\xa0\xa0', ' ')

            html.write_text(sstr)

    # todo eh.. implement this as an external site agnostic script
    (html_dir / 'documents.js').write_text(check_output([
        src / 'search/makeindex.py',
        '--root', html_dir,
    ]).decode('utf8'))

    shtml = src / 'search/search.html'
    node = bs(shtml.read_text()).find(id='search')
    searchs = node.prettify()

    # for idempotence
    MARKER = '<!-- PROCESSED BY postprocess_html -->'

    for html in html_dir.rglob('*.html'):
        text = html.read_text()
        already_handled = MARKER in text
        if already_handled:
            continue

        soup = bs(text + MARKER)

        # todo would be cool to integrate it with org-mode properly..
        settings = f'''
<span class='exobrain-settings'>
<span class='exobrain-setting'>show timestamps<input id="settings-timestamps" type="checkbox"/></span>
<span class='exobrain-setting'>show priorities<input id="settings-priorities" type="checkbox"/></span>
<span class='exobrain-setting'>show todo state<input id="settings-todostates" type="checkbox"/></span>
</span>
        '''

        # meh... this gets too complicated
        sidebar = f"""
<a id='jumptosidebar' href='#sidebar'>Jump to search, settings &amp; sitemap</a>
<div id='sidebar'>
{searchs}
{settings}
{tocs}
</div>
"""
        soup.body.extend(ashtml(sidebar))

        # ugh. very annoying... this is ought to be easier...
        depth = len(html.relative_to(html_dir).parts) - 1
        rel = ('' if depth == 0 else '../' * depth)
        rel_head = f'''
<script>
const PATH_TO_ROOT = "{rel}"
</script>
'''
        search_head = f'''
<link  href='{rel}search.css' rel='stylesheet'>
<script src='{rel}search.js'></script>
'''
        soup.head.extend(ashtml(rel_head))
        soup.head.extend(ashtml(search_head))
        soup = relativize(soup, path=html, root=html_dir)
        html.write_text(str(soup))

    if (html_dir / 'README.html').exists(): # meh, for incremental mode
        check_call(['ln', '-sf', 'README.html', html_dir / 'index.html'])

# TODO add this back
# link = '<a style="font-size: 2rem; line-height: var(--menu-bar-height);" href="https://beepb00p.xyz">back to blog</a>'

if __name__ == '__main__':
    main()
