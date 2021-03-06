#!/usr/bin/env python3
from pathlib import Path
import sys
from shutil import rmtree, copy
from subprocess import check_call, check_output, run
from typing import List

def ccall(*args, **kwargs):
    print(args, file=sys.stderr)
    return check_call(*args, **kwargs)

# TODO editable?
# TODO huh, that actually would be nice. I could fix stuff in-place and then apply to org-mode
# TODO search settings?
# TODO wonder if I can integrate it with blog's header?
# TODO export txt files as md as well?

from compile_org import emacs


root_dir = Path(__file__).absolute().parent.parent
src  = root_dir / 'src'
data = root_dir / 'data'

input_dir  = data / 'input'
public_dir = data / 'public'
md_dir     = data / 'markdown'
html_dir   = data / 'html'

input_dir   = input_dir .resolve() # ugh. otherwise relative links might end up weird during org-publish-cache-ctime-of-src
public_dir  = public_dir.resolve()
md_dir      = md_dir    .resolve()
html_dir    = html_dir  .resolve()


def clean_dir(path: Path) -> None:
    assert path.is_dir(), path
    for x in path.iterdir():
        if x.name == '.git':
            continue
        if x.is_file() or x.is_symlink():
            x.unlink()
        else: # dir
            rmtree(x)


def clean() -> None:
    # todo ugh, need symlink watching tool here again...
    cachedir = Path('~/.org-timestamps').expanduser()
    # right, so it keeps track of modifications and doesn't publish if it wasn't modified
    # but completely unclear how it behaves wrt added/removed files etc
    # also wouldn't know if the source code was modified... so best not to use it I guess
    for c in cachedir.glob('*.cache'):
        c.unlink()

    clean_dir(md_dir)
    clean_dir(html_dir)

    # TODO what about empty dirs?
    for f in public_dir.rglob('*.org'):
        f.unlink()


def main() -> None:
    import argparse
    p = argparse.ArgumentParser()
    p.add_argument('--add'   , action='store_true')
    p.add_argument('--filter', type=str, default=None)
    p.add_argument('--no-html', action='store_false', dest='html')
    p.add_argument('--watch', action='store_true')
    p.add_argument('--under-entr', action='store_true') # ugh.
    args = p.parse_args()

    # ugh. this all is pretty complicated...
    if args.watch:
        clean()
        nargs = [*sys.argv, '--under-entr']
        nargs.remove('--watch')
        while True:
            paths = '\n'.join(str(p) for p in input_dir.rglob('*') if '.git' not in p.parts)
            run(['entr', '-d', *nargs], input=paths.encode('utf8'))
        sys.exit(0)
    if not args.under_entr:
        clean()

    preprocess(args)
    if args.html:
        postprocess_html()


def preprocess(args) -> None:
    filter = args.filter
    efilter = 'nil' if filter is None else rf"""'(:exclude "\\.*" :include ("{filter}"))"""

    eargs = [
        '--eval', f'''(progn
            (setq exobrain/input-dir  "{input_dir}" )
            (setq exobrain/public-dir "{public_dir}")
            (setq exobrain/md-dir     "{md_dir}"    )
            (setq exobrain/html-dir   "{html_dir}"  )
            (setq exobrain/filter     {efilter}     )
        )''',
        '--directory', src / 'advice-patch',
        '--load'     , src / 'publish.el',
        # '-f', 'toggle-debug-on-error', # dumps stacktrace on error
    ]
    with emacs(
            *eargs,
            '--eval',
            f'''(let ((org-publish-project-alist `(,exobrain/project-preprocess-org)))
                  (org-publish-all))''',
    ) as ep:
        pass
    assert ep.returncode == 0

    from check import check_org
    check_org(public_dir)

    # TODO think about commit/push/deploy logic?
    assert (public_dir / '.git').is_dir(), public_dir
    ccall(['git', 'status'], cwd=public_dir)

    if args.add:
        ccall(['git', 'add', '-A', '--intent-to-add'], cwd=public_dir)
        ccall(['git', 'add', '-p'], cwd=public_dir)
        # TODO suggest to commit/push?

    if args.html:
        with emacs(
                *eargs,
                '--eval',
                f'''(let ((org-publish-project-alist `(,exobrain/project-org2html)))
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


def postprocess_html() -> None:
    copy(src / 'search/search.css', html_dir / 'search.css'  )
    copy(src / 'search/search.js' , html_dir / 'search.js'   )
    copy(src / 'exobrain.css'     , html_dir / 'exobrain.css')
    copy(src / 'settings.js'      , html_dir / 'settings.js' )

    from bs4 import BeautifulSoup as BS # type: ignore
    bs = lambda x: BS(x, 'lxml')

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

        # meh... this gets too complicated
        sidebar = f"""
<a id='jumptosidebar' href='#sidebar'>Jump to exobrain search &amp sitemap</a>
<div id='sidebar'>
{searchs}
{tocs}
</div>
"""
        soup.body.extend(ashtml(sidebar))

        # todo would be cool to integrate it with org-mode properly..
        settings = f'''
<span class='exobrain-settings'>
<span class='exobrain-setting'>show timestamps<input id="settings-timestamps" type="checkbox"/></span>
<span class='exobrain-setting'>show priorities<input id="settings-priorities" type="checkbox"/></span>
<span class='exobrain-setting'>show todo state<input id="settings-todostates" type="checkbox"/></span>
</span>
        '''
        soup.body.extend(ashtml(settings))

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
    # TODO allow skipping?
    check_call(['mypy', '--check-untyped', __file__])
    main()
