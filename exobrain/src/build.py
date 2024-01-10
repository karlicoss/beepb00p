#!/usr/bin/env python3
import argparse
from concurrent.futures import Executor, ProcessPoolExecutor
from copy import deepcopy
from dataclasses import dataclass
from pathlib import Path
import re
import sys
from shutil import rmtree, copy
from subprocess import check_call, check_output, run, DEVNULL
from typing import Optional


from bs4 import BeautifulSoup  # type: ignore[import]
from more_itertools import divide
from loguru import logger

from utils import make_soup
from dummy_executor import DummyExecutor


def ccall(*args, **kwargs):
    print(args, file=sys.stderr)
    return check_call(*args, **kwargs)


_root = Path(__file__).absolute().parent.parent
src = _root / 'src'


@dataclass
class Config:
    root: Path
    # note: need to resolve
    # otherwise relative links might end up weird during org-publish-cache-ctime-of-src

    @property
    def input_dir(self) -> Path:
        return (self.root / 'input').resolve()

    @property
    def public_dir(self) -> Path:
        return (self.root / 'public').resolve()

    @property
    def md_dir(self) -> Path:
        return (self.root / 'md').resolve()

    @property
    def html_dir(self) -> Path:
        return (self.root / 'html').resolve()


def clean_dir(path: Path) -> None:
    assert path.is_dir(), path
    for x in path.iterdir():
        if x.name == '.git':
            continue
        if x.is_file() or x.is_symlink():
            x.unlink()
        else:  # dir
            rmtree(x)


def clean(*, cfg: Config, skip_org_export: bool) -> None:
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
    p = argparse.ArgumentParser()
    p.add_argument('--data-dir', type=Path)
    p.add_argument('--no-html', action='store_false', dest='html')
    p.add_argument('--no-org', action='store_false', dest='org')
    p.add_argument('--md', action='store_true')
    p.add_argument('--watch', action='store_true')
    p.add_argument('--under-entr', action='store_true')  # ugh.
    p.add_argument('--add', action='store_true')
    p.add_argument('--filter', type=str, default=None)
    p.add_argument('--workers', type=int, default=None, help='by default, uses all available cores')
    args = p.parse_args()

    assert not args.md  # broken for now
    assert not args.watch  # I think broken now due to sitemap
    assert args.filter is None  # FIXME support later
    assert not args.add  # broken for now

    if (data_dir := args.data_dir) is None:
        cfg = Config(_root / 'data')
    else:
        cfg = Config(data_dir)

    # ugh. this all is pretty complicated...
    if args.watch:
        clean(cfg=cfg, skip_org_export=not args.org)
        nargs = [*sys.argv, '--under-entr']
        nargs.remove('--watch')
        while True:
            paths = '\n'.join(str(p) for p in cfg.input_dir.rglob('*') if '.git' not in p.parts)
            run(['entr', '-d', *nargs], input=paths.encode('utf8'))
        sys.exit(0)
    if not args.under_entr:
        clean(cfg=cfg, skip_org_export=not args.org)

    workers: Optional[int] = args.workers
    pool = ProcessPoolExecutor(max_workers=workers) if workers != 0 else DummyExecutor()
    with pool:
        if args.org:
            publish_org(cfg, pool=pool)

        if args.html:
            publish_html(cfg, pool=pool)


@dataclass
class Context:
    input_dir: Path
    output_dir: Path


def compile_org_to_org(ctx: Context, paths: list[Path]) -> None:
    if len(paths) == 0:
        return  # nothing to do

    rpaths = [f.relative_to(ctx.input_dir) for f in paths]
    input_dir = ctx.input_dir
    input_dir = input_dir.absolute()  # emacs seems unhappy if we don't do it
    output_dir = ctx.output_dir
    output_dir = output_dir.absolute()  # emacs seems unhappy if we don't do it

    batch = f'{str(rpaths[0])} ... {str(rpaths[-1])} ({len(rpaths)} files)'
    logger.info(f'exporting {batch}')

    for rpath in rpaths:
        # create target dirs (emacs struggles without them)
        (output_dir / rpath).parent.mkdir(parents=True, exist_ok=True)
    check_call(
        [
            'emacs', '--batch', '-l',
            src / 'publish_org.el',
            input_dir, output_dir, *rpaths,
            # '-f', 'toggle-debug-on-error', # dumps stacktrace on error
        ],
        stdout=DEVNULL,
        stderr=DEVNULL,
    )

    logger.debug(f'fixing up {batch}')
    from fixup_org import fixup

    for rpath in rpaths:
        path = output_dir / rpath
        path.write_text(fixup(path.read_text()))
    # TODO hiding tags from export (e.g. 'refile') -- will need to be implemented manually?
    # TODO need to test it!

    logger.debug(f'checking {batch}')
    from check_org import check_one

    for rpath in rpaths:
        path = output_dir / rpath
        errors = list(check_one(path))
        assert len(errors) == 0, (path, errors)

    logger.info(f'finished {batch}')


def compile_org_to_html(ctx: Context, paths: list[Path]) -> None:
    if len(paths) == 0:
        return  # nothing to do

    rpaths = [f.relative_to(ctx.input_dir) for f in paths]
    input_dir = ctx.input_dir
    input_dir = input_dir.absolute()  # emacs seems unhappy if we don't do it
    output_dir = ctx.output_dir
    output_dir = output_dir.absolute()  # emacs seems unhappy if we don't do it

    batch = f'{str(rpaths[0])} ... {str(rpaths[-1])} ({len(rpaths)} files)'
    logger.info(f'exporting {batch}')

    for rpath in rpaths:
        # create target dirs (emacs struggles without them)
        (output_dir / rpath).parent.mkdir(parents=True, exist_ok=True)
    check_call(
        [
            'emacs',
            '--batch', '-l', src / 'publish_html.el',
            input_dir, output_dir, *rpaths,
        ],
        stdout=DEVNULL,
        stderr=DEVNULL,
    )

    logger.debug(f'making soups {batch}')
    soups = []
    for rpath in rpaths:
        path = output_dir / rpath.with_suffix('.html')
        soups.append(make_soup(path.read_text()))

    logger.debug(f'fixing up {batch}')
    from fixup_html import fixup
    for soup in soups:
        fixup(soup)

    ###
    logger.debug(f'postprocesing {batch}')

    shtml = src / 'search/search.html'
    node = make_soup(shtml.read_text()).find(id='search')
    searchs = node.prettify()

    sitemap = output_dir / 'sitemap.html'
    assert sitemap.exists(), sitemap
    node = make_soup(sitemap.read_text()).find(id='content')
    node.select_one('.title').decompose()
    node.name = 'nav'  # div by deafult
    node['id'] = 'exobrain-toc'
    tocs = str(node)  # do not prettify to avoid too many newlines around tags
    # mm, org-mode emits relative links, but we actually want abolute here so it makes sense for any page
    tocs = tocs.replace('href="', 'href="/')

    # todo would be cool to integrate it with org-mode properly..
    settings = '''
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
    sidebar_html = ashtml(sidebar)

    for rpath, soup in zip(rpaths, soups):
        path = output_dir / rpath.with_suffix('.html')
        # NOTE: need deep copy because we modify nodes in postprocess_html
        soup.body.extend(deepcopy(sidebar_html))
        postprocess_html(soup=soup, html=path, html_dir=output_dir)
    ###

    logger.info(f'finished {batch}')


def postprocess_html(
    *,
    soup: BeautifulSoup,
    html: Path,
    html_dir: Path,
) -> None:
    # ugh. very annoying... this is ought to be easier...
    depth = len(html.relative_to(html_dir).parts) - 1
    rel = '' if depth == 0 else '../' * depth

    rel_head = soup.new_tag('script')
    rel_head.string = f'\nconst PATH_TO_ROOT = "{rel}"\n'

    search_css = soup.new_tag('link', attrs={
        'href': f'{rel}search.css',
        'rel': 'stylesheet',
    })
    search_js = soup.new_tag('script', attrs={
        'src': f'{rel}search.js',
    })
    soup.head.extend([
        rel_head, '\n',
        search_css, '\n',
        search_js, '\n',
    ])

    soup = relativize(soup, path=html, root=html_dir)

    # super annoying (is this nbsp?), should be via css
    sstr = str(soup).replace('\xa0\xa0\xa0', ' ')
    html.write_text(sstr)


# for fucks sake, seems that it's not possible to insert raw html?
def ashtml(x: str):
    b = make_soup(x)
    head = list(b.head or [])
    body = list(b.body or [])
    return head + body


def publish_org_sitemap(public_dir: Path) -> None:
    ## deafult org-mode sitemap export has really weird sorting
    ## ahd hard to modify the order/style etc anyway
    ## plus org-mode sitemap is only exported during html export
    def get_title(p: Path) -> str:
        import orgparse

        o = orgparse.load(p)
        title = o.get_file_property('TITLE')
        if not title:
            title = p.stem  # match org-mode behaviour
        return title

    inputs = sorted(public_dir.rglob('*.org'))
    with_titles = [(p, get_title(p)) for p in inputs]

    emoji_pattern = re.compile(
        "["
        "\U0001F600-\U0001F64F"  # emoticons
        "\U0001F300-\U0001F5FF"  # symbols & pictographs
        "\U0001F680-\U0001F6FF"  # transport & map symbols
        "\U0001F1E0-\U0001F1FF"  # flags (iOS)
        "\U00002702-\U000027B0"
        "\U000024C2-\U0001F251"
        "]+",
        flags=re.UNICODE,
    )

    def contains_emoji(s):
        return emoji_pattern.search(s) is not None

    ## export sitemap
    def sort_key(x):
        (p, title) = x
        rel = p.relative_to(public_dir)
        return rel.parent, not contains_emoji(title), title.lower()

    with (public_dir / 'sitemap.org').open('w') as fo:
        fo.write('#+TITLE: Sitemap for project exobrain-html\n')
        fo.write('\n')
        emitted = set()
        for p, title in sorted(with_titles, key=sort_key):
            rp = p.relative_to(public_dir)
            par = rp.parent
            level = len(par.parts)
            if par not in emitted and par != Path('.'):
                fo.write('  ' * (level - 1) + f'- {par.name}\n')
                emitted.add(par)
            fo.write('  ' * level + f'- [[file:{rp}][{title}]]\n')


def publish_org(cfg: Config, *, pool: Executor) -> None:
    """
    Publishies intermediate ('public') org-mode
    """
    input_dir = cfg.input_dir
    public_dir = cfg.public_dir
    assert input_dir.exists(), input_dir

    ctx = Context(input_dir=input_dir, output_dir=public_dir)
    inputs = sorted(input_dir.rglob('*.org'))

    workers = pool._max_workers  # type: ignore[attr-defined]
    logger.debug(f'using {workers} workers')
    groups = [list(group) for group in divide(workers, inputs)]
    futures = [pool.submit(compile_org_to_org, ctx, group) for group in groups]
    for group, fut in zip(groups, futures):
        try:
            fut.result()
        except Exception as e:
            raise RuntimeError(f'error while processing {group}') from e

    # TODO maybe collect titles from compile_org_to_org calls?
    publish_org_sitemap(public_dir)


def publish_html(cfg: Config, *, pool: Executor) -> None:
    """
    Publishes html from 'public' org-mode
    """
    public_dir = cfg.public_dir
    html_dir = cfg.html_dir
    assert public_dir.exists(), public_dir

    ctx = Context(input_dir=public_dir, output_dir=html_dir)
    inputs = sorted(public_dir.rglob('*.org'))

    ## first, compile sitemap (since its output is used in other htmls)
    sitemap = public_dir / 'sitemap.org'
    inputs.remove(sitemap)
    compile_org_to_html(ctx, [sitemap])
    ##

    workers = pool._max_workers  # type: ignore[attr-defined]
    logger.debug(f'using {workers} workers')
    groups = [list(group) for group in divide(workers, inputs)]
    futures = [pool.submit(compile_org_to_html, ctx, group) for group in groups]
    for group, fut in zip(groups, futures):
        try:
            fut.result()
        except Exception as e:
            raise RuntimeError(f'error while processing {group}') from e

    # for f in public_dir.rglob('*.org'):
    #     assert not f.is_symlink(), f # just in case?
    #     check_call(['chmod', '-w', f]) # prevent editing

    # fmt: off
    copy(src / 'search/search.css', html_dir / 'search.css'  )
    copy(src / 'search/search.js' , html_dir / 'search.js'   )
    copy(src / 'exobrain.css'     , html_dir / 'exobrain.css')
    copy(src / 'settings.js'      , html_dir / 'settings.js' )
    # fmt: on

    # todo eh.. implement this as an external site agnostic script
    logger.debug('generating index')  # note: takes about 5 secs atm
    (html_dir / 'documents.js').write_text(
        check_output(
            [
                src / 'search/makeindex.py',
                '--root',
                html_dir,
            ],
            text=True,
        )
    )
    logger.debug('generating index: done')

    if (html_dir / 'README.html').exists():  # meh, for incremental mode
        check_call(['ln', '-sf', 'README.html', html_dir / 'index.html'])


# def preprocess(args, *, skip_org_export: bool) -> None:
#     # TODO think about commit/push/deploy logic?
#     # TODO not sure why I had this? prob don't want it...
#     # assert (public_dir / '.git').is_dir(), public_dir
#     # ccall(['git', 'status'], cwd=public_dir)

#     if args.add:
#         raise RuntimeError('Temporary unsupported')
#         ccall(['git', 'add', '-A', '--intent-to-add'], cwd=public_dir)
#         ccall(['git', 'add', '-p'], cwd=public_dir)
#         # TODO suggest to commit/push?


def relativize(soup, *, path: Path, root: Path):
    depth = len(path.relative_to(root).parts) - 1
    rel = '' if depth == 0 else '../' * depth

    for tag, attr in [('a', 'href'), ('link', 'href'), ('script', 'src')]:
        for t in soup.find_all(tag):
            link = t.get(attr) or ''
            if not link.startswith('/'):
                continue
            t[attr] = rel + link[1:]
    return soup


# TODO add this back
# link = '<a style="font-size: 2rem; line-height: var(--menu-bar-height);" href="https://beepb00p.xyz">back to blog</a>'

if __name__ == '__main__':
    main()


# TODO wonder if I can integrate it with blog's header?
