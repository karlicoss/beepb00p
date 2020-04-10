#!/usr/bin/env python3
from pathlib import Path
from functools import lru_cache, wraps
from itertools import chain
from glob import glob
from fnmatch import fnmatch
import os
import re
import logging
from subprocess import check_call, run, check_output, PIPE
from datetime import datetime
import shutil
import sys
from tempfile import TemporaryDirectory
from typing import cast, Dict, Any, List, Optional, Union, NamedTuple, Tuple


import pytz # type: ignore


### TODO keep this in config?
ROOT    = Path(__file__).absolute().parent.parent
inputs  = ROOT / 'inputs'
content = ROOT / 'content'
output  = Path('site') # TODO relative to ROOT??

tz = pytz.utc # TODO ugh this is what Hakyll assumed
###


templates = inputs / 'templates'


class Post(NamedTuple):
    title: str
    summary: str
    date: Optional[datetime]
    body: str
    draft: bool
    special: bool
    has_math: bool
    tags: List[str]
    url: str

    @property
    def dates(self) -> Optional[str]:
        d = self.date
        if d is None:
            return None
        return d.strftime('%d %B %Y')


PathIsh = Union[Path, str]

log = logging.getLogger('blog')


from dataclasses import dataclass
@dataclass(init=False, unsafe_hash=True)
class MPath:
    path: Path
    mtime: float

    def __init__(self, path: PathIsh) -> None:
        self.path = Path(path)
        self.mtime = self.path.stat().st_mtime

    # TODO __str__?

# global temporary directory
TMP_DIR: Path = cast(Path, None)


# TODO use wraps??
cache = lambda f: lru_cache(maxsize=None)(f)


# TODO better name?..
# TODO there must be something builtin??
def sanitize(path: Path) -> str:
    pp = str(path)
    pp = pp.replace('/', '_')
    return pp


# TODO how to clean old results??
@cache
def compile_org(*, compile_script: MPath, path: MPath) -> Path:
    log.debug('compiling %s %s', compile_script, path)

    dname = sanitize(path.path)
    d = TMP_DIR / dname
    d.mkdir()

    # TODO each thread should prob. capture logs...
    log.debug('running %s %s', compile_script, d)

    # TODO not sure that compile-org should.
    # unless I copy files separately in the 'sandbox'?
    res = run(
        [
            compile_script.path,
            '--output-dir', d,
            # '--org',  # TODO
        ],
        input=path.path.read_bytes(),
        stdout=PIPE,
        stderr=PIPE,
        check=True,
    )
    out = res.stdout.decode('utf8')
    (d / 'body').write_text(out)

    err = res.stderr.decode('utf8')

    # TODO filter this in emacs/compile-org?
    filtered = []
    for line in err.splitlines():
        if any(re.match(p, line) for p in [
                "Created .* link",
                r"Loading .*\.\.\.$",
                "Can.t guess python.indent.offset",
                "executing.*code block",
                "Code block evaluation complete",
                "Setting up indent",
                "Indentation variables are",
                "Indentation setup for shell",
        ]):
            continue
        filtered.append(line)
    if len(filtered) > 0:
        err = '\n'.join(filtered)
        log.debug('%s: emacs output \n%s', path, indent(err))

    # TODO how to clean stale stuff that's not needed in output dir?
    # TODO output determined externally?
    # TODO some inputs
    return d


def indent(s: str, spaces=2) -> str:
    return ''.join(' ' * spaces + s for s in s.splitlines(keepends=True))


Meta = Dict[str, Any]


def pandoc_meta(path: Path) -> Meta:
    # wow, that's quite horrible...
    with TemporaryDirectory() as td:
        t = Path(td) / 'meta.tpl'
        t.write_text('$meta-json$')
        res = check_output(['pandoc', '--template', t, path])
        import json
        return json.loads(res.decode('utf8'))


# pip install ruamel.yaml -- temporary...
# just use json later?
def metadata(path: Path) -> Meta:
    meta = path.with_suffix(path.suffix + '.metadata')
    if not meta.exists():
        return {}
    from ruamel.yaml import YAML # type: ignore
    yaml = YAML(typ='safe')
    return yaml.load(meta)


def compile_md(*, path: Path) -> Path:
    d = TMP_DIR / path.name
    d.mkdir()

    res = run(
        ['pandoc', '--to', 'html'],
        input=path.read_bytes(),
        stdout=PIPE,
        check=True,
    )
    out = res.stdout.decode('utf8')
    (d / 'body').write_text(out)
    return d


def compile_ipynb(*, compile_script: Path, path: Path) -> Path:
    d = TMP_DIR / path.name
    d.mkdir()

    meta = metadata(path)

    # TODO make this an attribute of the notebook somehow? e.g. tag
    allow_errors = meta.get('allow_errors', False)

    # meh
    # itemid = path.absolute().relative_to(content.absolute().parent)
    itemid = 'content/' + path.name
    # TODO meeeeeh.
    res = run(
        [
            compile_script,
            '--output-dir', d,
            # TODO do we need it??
            '--item', str(itemid),
            *(['--allow-errors'] if allow_errors else []),
        ],
        input=path.read_bytes(),
        stdout=PIPE,
        check=True,
    )
    # TODO remove duplicats
    out = res.stdout.decode('utf8')
    (d / 'body').write_text(out)

    return d


def the(things):
    ss = set(things)
    assert len(ss) == 1, ss
    [x] = ss
    return x



def _move(from_: Path, ext: str):
    for f in from_.rglob('*.' + ext):
        log.debug('merging %s', f)
        rel = f.relative_to(from_)
        to = output / rel

        # TODO FIXME think how to clean up old results..
        # assert not to.exists(), to

        to.parent.mkdir(exist_ok=True) # meh
        shutil.move(f, to)


@cache
def compile_post(path: MPath) -> Tuple[Path, Post]:
    log.info('compiling %s', path)
    try:
        res = _compile_post(path)
    except Exception as e:
        raise RuntimeError(f'While compiling {path}') from e

    # TODO wonder if child logger could be a thing?
    # TODO measure time took?
    rpath, rpost = res
    log.info('compiled %s', path)

    _move(rpath, 'html')
    _move(rpath, 'png')

    for root, dirs, files in os.walk(rpath, topdown=False):
        # remove all empty dirs. meh.
        if len(files) == 0:
            for d in dirs:
                dd = (Path(root) / d)
                dd.rmdir()

    remaining = list(rpath.rglob('*'))
    if len(remaining) > 0:
        raise RuntimeError(f'remaining files: {remaining}')
    rpath.rmdir()

    return rpath, rpost


def _compile_post(mpath: MPath) -> Tuple[Path, Post]:
    path = mpath.path
    if path.is_absolute():
        path = path.relative_to(content) # meh
    apath = content / path
    # TODO not sure which path should really be used..

    suffix = path.suffix

    meta = metadata(apath)

    # TODO not sure which meta should win??
    if suffix == '.md':
        pmeta = pandoc_meta(apath)
        meta.update(**pmeta)

    hpath = meta.get('html_path')
    if hpath is None:
        path = path.with_suffix('.html')
    else:
        path = Path(hpath)

    ctx: Dict[str, Any] = {}
    #
    # TODO where to extract URL from
    ctx['url'] = '/' + str(path.with_suffix('.html'))

    # TODO FIXME check_ids??
    pb: List[str] = meta.get('pingback', [])
    # TODO meh
    pingback = [{
        'title': x.split()[0],
        'url'  : x.split()[1],
    } for x in pb]

    # TODO need to be raw??
    ctx['pingback'] = pingback

    def set_issoid(upid: Optional[str]):
        if upid is None:
            return
        ctx['issoid'] = f'isso_{upid}'

    def set_summary(summ: Optional[str]):
        if summ is None:
            return
        ctx['summary'] = summ

    def set_tags(tags: Optional[List[str]]):
        if tags is None:
            return
        ctx['tags'] = [{'body': tag, 'sep': '' if i == len(tags) - 1 else ' '} for i, tag in enumerate(tags)]

    def set_title(title: Optional[str]):
        if title is None:
            return
        ctx['title'] = title

    date = None
    def set_date(datish: Optional[Union[str, datetime]]):
        if datish is None:
            return

        nonlocal date # meh
        date = datetime.strptime(datish, '%B %d, %Y') if isinstance(datish, str) else datish
        ctx['date'] = date.strftime('%d %B %Y')


    set_title  (meta.get('title'))
    set_issoid (meta.get('upid'))
    set_summary(meta.get('summary'))
    set_tags   (meta.get('tags'))
    set_date   (meta.get('date'))

    special = meta.get('special', False)
    ctx['type_special'] = special
    ctx['is_stable'] = True
    ctx['draft'] = meta.get('draft')

    # TODO FIMXE compile_org should return a temporary directory with 'stuff'?
    outs: Path
    if suffix == '.org':
        outs = compile_org(
            compile_script=MPath('misc/compile_org.py'),
            path=MPath(apath),
        )
        ctx['style_org'] = True

        import orgparse # type: ignore
        o = orgparse.loads(apath.read_text())
        # TODO need to make public?? also can remove from porg then..
        fprops = o._special_comments

        # print(fprops)

        ttls = fprops.get('TITLE')
        if ttls is not None:
            set_title(the(ttls))

        summs = fprops.get('SUMMARY')
        if summs is not None:
            set_summary(the(summs))

        upids = fprops.get('UPID')
        if upids is not None:
            set_issoid(the(upids))

        ftagss = fprops.get('FILETAGS')
        if ftagss is not None:
            ftags = the(ftagss)
            tags = list(x for x in ftags.split(':') if len(x) > 0)
            set_tags(tags)

        datess = fprops.get('DATE')
        if datess is not None:
            dates = the(datess)[1: 1 + len('0000-00-00 Abc')]
            date = datetime.strptime(dates, '%Y-%m-%d %a')
            set_date(date)

    elif suffix == '.ipynb':
        # TODO make a mode to export to python?
        outs = compile_ipynb(
            compile_script=Path('misc/compile-ipynb'),
            path=apath,
        )
        ctx['style_ipynb'] = True

        ctx['has_math']     = meta.get('has_math'    , False)
        ctx['allow_errors'] = meta.get('allow_errors', False)
    elif suffix == '.md':
        outs = compile_md(path=apath)
        ctx['style_md'] = True
    else:
        raise RuntimeError(f"Unexpected suffix: {suffix}")


    assert ctx['title'] is not None, ctx

    opath = outs / path

    if opath.parent != outs:
        # ugh. need to move hierarchy down to preserve relative paths..
        outs_files = list(x for x in outs.rglob('*') if not x.is_dir())
        for f in outs_files:
            r = f.relative_to(outs)
            to = opath.parent / r
            to.parent.mkdir(parents=True, exist_ok=True)
            log.debug('moving %s %s', f, to)
            f.rename(to)
            # TODO remove empty f.parent??

    body_file = opath.parent / 'body'
    body = body_file.read_text()
    body_file.unlink()

    # strip private stuff
    body = ''.join(line for line in body.splitlines(keepends=True) if 'NOEXPORT' not in line)


    # TODO for org-mode, need to be able to stop here and emit whatever we compiled?
    post_t = template('post.html')
    pbody = post_t.render(
        body=body,
        **ctx,
    )
    full_t = template('default.html')
    full = full_t.render(
        body=pbody,
        **ctx,
    )

    full = relativize_urls(path=path, full=full)

    post = Post(
        title  =ctx['title'],
        summary=ctx.get('summary'),
        date   =date,
        tags   =[t['body'] for t in ctx.get('tags', [])],
        body   =full,
        draft  =ctx.get('draft') is not None,
        url    =ctx['url'],
        special=special,
        has_math=ctx.get('has_math', False),
    )


    #
    if 'name="keywords"' not in full:
        loc = '<meta content="English" name="language"/>'
        assert loc in full, full
        full = full.replace(loc, loc + '\n' + '<meta name="keywords" content>')

    opath.parent.mkdir(exist_ok=True)
    # TODO might need to move everything into the same dir??
    opath.write_text(full)

    check_call(['tree', '--noreport', outs])
    return outs, post


def relativize_urls(path: Path, full: str):
    depth = len(path.parts)
    rel = '.' * depth
    from bs4 import BeautifulSoup as bs # type: ignore
    soup = bs(full, 'html5lib')  # TODO lxml parser?
    from itertools import chain
    # a bit meh..
    for a in chain(soup.findAll('a'), soup.findAll('link')):
        href = a.get('href', None)
        if href is None:
            continue
        if href.startswith('/'):
            a['href'] = rel + href
    return str(soup)

# TODO should use Path with mtime etc


from jinja2 import Template # type: ignore
@cache
def template(name: str) -> Template:
    log.debug('loading template %s', name, )
    ts = _templates()
    return ts.get_template(name)


@cache
def _templates():
    from jinja2 import FileSystemLoader, Environment # type: ignore
    env = Environment(loader=FileSystemLoader(str(templates)))
    return env



INPUTS = list(sorted({
    *[c.relative_to(content) for c in chain(
        content.rglob('*.md'),
        # TODO make more defensive??
        [x for x in content.rglob('*.org') if 'drafts' not in x.parts],
        content.rglob('*.ipynb'),
    )],
}))


# TODO make a 'parallel' function??
# almost useless though if they are not sharing the threads...

from feedgen.feed import FeedGenerator # type: ignore

def feeds(posts: List[Post]):
    atom = feed(posts, 'atom')
    rss  = feed(posts, 'rss')
    (output / 'atom.xml').write_text(atom.atom_str(pretty=True).decode('utf8'))
    (output / 'rss.xml' ).write_text(rss .rss_str (pretty=True).decode('utf8'))


def feed(posts: List[Post], kind: str) -> FeedGenerator:
    fg = FeedGenerator()
    fg.title('beepb00p')
    fg.author(name='karlicoss', email='karlicoss@gmail.com')
    # TODO better description?
    fg.description('feed')

    bb = lambda x: f'https://beepb00p.xyz{x}'
    fg.id(bb(f'/{kind}.xml'))
    fg.link(rel='self', href=bb(f'/{kind}.xml'))
    fg.link(href=bb(''))
    if len(posts) > 0:
        fg.updated(max(tz.localize(p.date) for p in posts))

    # eh, apparnetly in adds items to the feed from bottom to top...
    for post in reversed(posts):
        fe = fg.add_entry()
        # not sure why id() doesn't allow to set permalink=True
        fe.guid(bb(post.url), permalink=True)
        fe.link(href=bb(post.url))
        fe.title(post.title)
        # TOOD FIXME meh.
        d = post.date
        assert d is not None
        td = tz.localize(d)
        fe.published(td)
        fe.updated(td)
        # TODO meh, later use proper update date...
        #
        # TODO use type=text/html for comparisons?
        fe.content(post.body, type='html')
    return fg


def posts_list(posts: List[Post], name: str, title: str):
    it = template(name)

    pbody = it.render(posts=posts)

    ctx: Dict[str, Any] = {}
    ctx['is_stable'] = True
    ctx['title'] = title
    ctx['url'] = f'/{name}'

    full_t = template('default.html')
    full = full_t.render(
        body=pbody,
        **ctx,
    )

    path = Path(name)
    full = relativize_urls(path, full)

    (output / path).write_text(full)


def compile_all(max_workers=None):
    # preload all templates
    for t in templates.glob('*.html'):
        template(t.name)

    # TODO later, move this into the content??
    # it makes sense to keep everything as intact as possible during export
    # so you could reference stuff in org-mode sources
    #
    # TODO copy here??
    # TODO this is root...

    # TODO pathish?
    def copy(from_: Path, to: PathIsh):
        if isinstance(to, str):
            top = output / to
        else:
            assert not to.is_absolute(), to
            top = to

        top = output / to
        top.parent.mkdir(exist_ok=True, parents=True)
        shutil.copy(from_, top)

    # TODO fuck. doesn't follow symlinks...
    copy(inputs / 'meta/robots.txt'    , 'robots.txt')
    copy(inputs / 'meta/robot-face.png', 'robot-face.png')
    for f in (inputs / 'css').rglob('*.css'):
        copy(f, f.relative_to(inputs))
    for f in (inputs / 'images').rglob('*.svg'):
        copy(f, f.relative_to(inputs))
    # eh. apparently glob(recursive=True) always follows symlinks??
    for p in chain.from_iterable(glob(f'{content}/**/*.{x}', recursive=True) for x in ('jpg', 'svg', 'png')):
        f = Path(p)
        copy(f, f.relative_to(content))

    posts = []
    from concurrent.futures import ThreadPoolExecutor
    with ThreadPoolExecutor(max_workers=max_workers) as pool:
        for res, post in pool.map(compile_post, map(lambda p: MPath(content / p), INPUTS)):
            posts.append(post)

    # TODO FIXME body needs to contain compiled??
    posts = list(reversed(sorted(posts, key=lambda p: datetime.min if p.date is None else p.date)))

    for_index  = [p for p in posts if not p.draft and not p.special]
    for_drafts = [p for p in posts if p.draft]
    posts_list(for_index , 'index.html' , 'Home')
    # TODO sort by date???
    posts_list(for_drafts, 'drafts.html', 'Drafts')

    # TODO also filter??
    for_feed = for_index[:9] # TODO FIXME add full feed?
    # TODO eh? not sure if necessary..
    feeds(for_feed)


def clean():
    for f in output.iterdir():
        if f.is_dir():
            shutil.rmtree(f)
        else:
            f.unlink()


def main():
    import argparse
    p = argparse.ArgumentParser()
    p.add_argument('--debug' , action='store_true', help='Debug logging')
    p.add_argument('--filter', action='append', required=False, type=str, help='glob to filter the inputs')
    args = p.parse_args()

    debug = args.debug
    setup_logging(level=getattr(logging, 'DEBUG' if debug else 'INFO'))

    filter = args.filter
    if filter is not None:
        filtered = []

        global INPUTS
        for x in INPUTS:
            if any(fnmatch(str(x), f) for f in filter):
                filtered.append(x)
            else:
                log.debug('filtered out %s', x)

        if len(filtered) == 0:
            log.warning('no files are filtered by %s', filter)

        INPUTS = filtered

    clean()
    global TMP_DIR
    # TODO create it in the output dir? so it's on the same filesystem
    with TemporaryDirectory() as tdir:
        TMP_DIR = Path(tdir)
        compile_all(max_workers=7)



def setup_logging(level):
    # TODO make optional?
    import logzero # type: ignore
    format = '%(color)s[%(levelname)1.1s %(asctime)s %(module)s:%(lineno)4d %(threadName)s]%(end_color)s %(message)s'
    logzero.setup_logger(log.name, level=level, formatter=logzero.LogFormatter(fmt=format))


if __name__ == '__main__':
    main()


# TODO self check with mypy/pylint??

# TODO feed needs to be compact to fit in 512K limit...

# TODO not sure how metadata should be handled... does every post really have
# TODO if we use templates in python (as f-strings), they can be statically checked..


# TODO use injector??
# https://github.com/alecthomas/injector

# TODO def should detect stuff automatically?
# TODO make sure it's cached for feed and index page..
# TODO use threads? most of it is going to be io bound anyway


# ok, functions are 'kinda pure'
# not completely because of filesystem use, but mostly pure
