#!/usr/bin/env python3
from pathlib import Path
from functools import lru_cache, wraps
from contextlib import contextmanager
from itertools import chain
from glob import glob
from fnmatch import fnmatch
import os
import re
import logging
from subprocess import check_call, run, check_output, PIPE
import time
from datetime import datetime
import shutil
import sys
from tempfile import TemporaryDirectory
from typing import cast, Dict, Any, List, Optional, Union, Tuple, Iterable, Iterator, Sequence, Literal
import dataclasses
from dataclasses import dataclass

import pytz # type: ignore

import orgparse


Format = Literal['html', 'raw'] # todo 'plain'?


### global vars, set in main
input  : Path = cast(Path, None)
output : Path = cast(Path, None)
TMP_DIR: Path = cast(Path, None)
FILTER: Optional[str] = None
FORMAT: Format = 'html'
###

ROOT  = Path(__file__).absolute().resolve().parent.parent
META  = ROOT / 'meta'

###
# use these dirs, otherwise it craps into notebook output
os.environ['MPLCONFIGDIR'] = '/tmp/beepb00p/matplotlib'
os.environ['SEABORN_DATA'] = '/tmp/beepb00p/seaborn'
###

log = logging.getLogger('blog')

### common utils
PathIsh = Union[Path, str]

from typing import NoReturn
def throw() -> NoReturn:
    raise AssertionError("shoudln't happen!")

from more_itertools import make_decorator
tuplify = make_decorator(wrapping_func=tuple)

def indent(s: str, spaces: int=2) -> str:
    return ''.join(' ' * spaces + s for s in s.splitlines(keepends=True))

# todo there must be something builtin??
def sanitize(path: Path) -> str:
    pp = str(path)
    pp = pp.replace('/', '_')
    return pp

# this is so site works on a subpath (e.g. domain.com/blog )
# not sure about that... ideally this would be applied at the very end, against all outputs..
def relativize_urls(*, path: Path, html: str) -> str:
    assert not path.is_absolute(), path
    depth = len(path.parts)
    rel = '.' * depth
    from bs4 import BeautifulSoup as bs # type: ignore
    soup = bs(html, 'html5lib')  # TODO lxml parser?
    # a bit meh..
    for a in chain(soup.findAll('a'), soup.findAll('link')):
        href = a.get('href', None)
        if href is None:
            continue
        if href.startswith('/'):
            a['href'] = rel + href
    return str(soup)
###

@dataclass(unsafe_hash=True)
class Tag:
    name: str

    @property
    def exists(self) -> bool:
        return self.name in blog_tags()


@dataclass(unsafe_hash=True)
class Post:
    title: str
    summary: str
    date: Optional[datetime]
    body: str
    draft: bool
    special: bool
    has_math: bool
    tags: Sequence[Tag]
    url: str
    feed: bool
    upid: Optional[str]=None # TODO FIXME

    def check(self) -> None:
        s = self
        assert isinstance(s.title  , str)
        assert isinstance(s.summary, str)
        assert isinstance(s.date , (datetime, type(None)))
        assert isinstance(s.draft   , bool)
        assert isinstance(s.special , bool)
        assert isinstance(s.has_math, bool)
        for t in s.tags:
            assert isinstance(t, Tag)
        assert isinstance(s.url , str)
        assert isinstance(s.feed, bool)
        assert isinstance(s.upid, (str, type(None)))

    @property
    def date_human(self) -> Optional[str]:
        d = self.date
        if d is None:
            return None
        return d.strftime('%d %B %Y')


def in_graph(meta: Post) -> bool:
    # dunno, I guess this sort of gives less coupling...
    # otherwise can't compile a singe post properly
    svg = (ROOT / 'misc/index.svg').read_text()
    return f'id="{meta.upid}"' in svg

    import sys
    # ugh. when this is executed as python3 -m, is doesn't set the subpackage?? ends up in importing twice
    sys.modules['beepb00p.build'] = sys.modules[__name__]
    from . import index as I
    # meh. also need to cache it?
    # m = I.maybe_meta(stem) # TODO
    # return m is not None

@dataclass(frozen=True)
class MPath:
    path: Path
    mtime: float

    def __repr__(self) -> str:
        # TODO add mtime back? not sure
        return f'{{path={self.path}}}'


def mpath(path: PathIsh) -> MPath:
    # TODO ugh. if dataclass is frozen, we can't assign mtime in __init__???
    # https://docs.python.org/3/library/dataclasses.html#frozen-instances
    p = Path(path)
    return MPath(
        path=p,
        mtime=p.stat().st_mtime,
    )

# TODO use wraps??
cache = lambda f: lru_cache(maxsize=None)(f)


def compile_org_body(*, compile_script: Path, path: Path, fmt: Format, dir_: Path, active_tags: Sequence[str], check_ids: bool=False) -> Iterable[Exception]:
    res = run(
        [
            compile_script,
            *(['--check-ids'] if check_ids else []),
            '--input', path,
            '--output-dir', dir_,
            '--active-tags', ','.join(active_tags),
            {'html': '--html', 'raw': '--org'}[fmt],
        ],
        stdout=PIPE,
        stderr=PIPE,
    )
    out = res.stdout.decode('utf8')
    err = res.stderr.decode('utf8')
    if len(err) > 0:
        (log.error if res.returncode > 0 else log.debug)('%s: emacs output \n%s', path, indent(err))

    if res.returncode > 0:
        if res.returncode == 1:
            # non-fatal error. carry on..
            # TODO add a strict mode? I guess we only want to allow
            yield RuntimeError(f'error compiling {path}')
        else:
            res.check_returncode()

    (dir_ / 'body').write_text(out)

Meta = Dict[str, Any]

def metadata(path: Path) -> Meta:
    meta = path.with_suffix(path.suffix + '.metadata')
    if not meta.exists():
        return {}
    # todo just use json later?
    from ruamel.yaml import YAML # type: ignore
    yaml = YAML(typ='safe')
    res = yaml.load(meta)
    if res is None:
        res = {} # fuck yaml...
    return res


def compile_md_body(*, path: Path, dir_: Path) -> None:
    res = run(
        ['pandoc', '--to', 'html'],
        input=path.read_bytes(),
        stdout=PIPE,
        check=True,
    )
    out = res.stdout.decode('utf8')
    (dir_ / 'body').write_text(out)


def compile_ipynb_body(*, compile_script: Path, path: Path, dir_: Path) -> None:
    meta = metadata(path) # todo instead, take in Post?
    # todo make this an attribute of the notebook somehow? e.g. tag
    allow_errors = meta.get('allow_errors', False)

    # meh
    # itemid = path.absolute().relative_to(content.absolute().parent)
    itemid = 'content/' + path.name # TODO ??
    # TODO meeeeeh.
    res = run(
        [
            compile_script,
            '--output-dir', dir_,
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
    (dir_ / 'body').write_text(out)


def _move(from_: Path, ext: str) -> None:
    for f in from_.rglob('*.' + ext):
        log.debug('merging %s', f)
        rel = f.relative_to(from_)
        to = output / rel

        # TODO FIXME think how to clean up old results..
        # assert not to.exists(), to

        to.parent.mkdir(exist_ok=True) # meh
        shutil.move(str(f), to)

# TODO hmm. iterable is not very cachable, exhausts the iterator..
Results = Iterable[Union[Post, Exception]]

def compile_post(path: MPath) -> Results:
    '''
    Ok, Iterable of posts is a bit confusing type.
    What I want in reality is: a stream of Exceptions and [optionally, depending on the fatality] a Post
    But that wouldn't be expressible in mypy, and the caller is merging everything together anyway. So whatever.
    '''
    try:
        return list(_compile_post(path))
    except Exception as e:
        ex = RuntimeError(f'While compiling {path}')
        ex.__cause__ = e
        # ugh. log here since cause doesn't propagate through process pool
        log.exception(ex)
        return [ex]


@dataclass(init=False, frozen=True)
class BaseDeps:
    ext: str = dataclasses.field(init=False)
    path: MPath

@dataclass(frozen=True)
class OrgDeps(BaseDeps):
    ext = '.org'
    compile_org: MPath
    misc: Tuple[MPath, ...]
    active_tags: Tuple[str, ...]

    @classmethod
    def make(cls, path: MPath) -> 'OrgDeps':
        return OrgDeps(
            path = path,
            compile_org = mpath(ROOT / 'src/compile_org.py'),
            misc = (
                mpath(ROOT / 'src/compile-org.el'),
            ),
            # todo active tags should be in base?
            # NOTE: damn, this is super neat... maybe write about it
            active_tags=tuple(blog_tags()),
        )

@dataclass(frozen=True)
class MdDeps(BaseDeps):
    ext = '.md'
    # todo pandoc dependency??
    @classmethod
    def make(cls, path: MPath) -> 'MdDeps':
        return MdDeps(
            path=path,
        )

@dataclass(frozen=True)
class IpynbDeps(BaseDeps):
    ext = '.ipynb'
    compile_ipynb: MPath

    @classmethod
    def make(cls, path: MPath) -> 'IpynbDeps':
        return IpynbDeps(
            path=path,
            compile_ipynb=mpath(ROOT / 'src/compile-ipynb'),
        )
Deps = Union[OrgDeps, MdDeps, IpynbDeps]

def _deps(mpath: MPath) -> Deps:
    deps: Deps = {
        C.ext: C.make # type: ignore[attr-defined]
        for C in (OrgDeps, IpynbDeps, MdDeps)
    }[mpath.path.suffix](mpath)
    return deps

def _compile_post(mpath: MPath) -> Results:
    deps = _deps(mpath)
    with compile_in_dir(mpath.path) as dir_:
        yield from _compile_with_deps(deps, dir_=dir_)


@contextmanager
def compile_in_dir(path: Path) -> Iterator[Path]:
    dname = sanitize(path)
    d = TMP_DIR / dname
    d.mkdir()
    try:
        yield d
    finally:
        if d.exists():
            shutil.rmtree(d)

# needed to make sure the iterator is replayable (because it's cached)

# todo could use for dictify/listify
@cache
@tuplify()
def _compile_with_deps(deps: Deps, dir_: Path) -> Results:
    path = deps.path

    # TODO wonder if child logger could be a thing?
    # TODO measure time taken?
    log.info('compiling %s', path)
    # TODO rpath is d??
    yield from _compile_post_aux(deps, dir_=dir_)
    log.info('compiled  %s', path)

    for ext in ('html', 'org', 'png'):
        _move(dir_, ext) # meh

    for root, dirs, files in os.walk(dir_, topdown=False):
        # remove all empty dirs. meh.
        if len(files) == 0:
            for d in dirs:
                dd = (Path(root) / d)
                dd.rmdir()

    remaining = list(dir_.rglob('*'))
    if len(remaining) > 0:
        raise RuntimeError(f'remaining files: {remaining}')
    dir_.rmdir()


def org_meta(apath: Path) -> Meta:
    # TODO extract get_meta method??
    o = orgparse.loads(apath.read_text())
    ttl_ = o.get_file_property('TITLE'); assert ttl_ is not None
    ttl: str = ttl_
    summ    = o.get_file_property('SUMMARY')
    upid    = o.get_file_property('UPID')
    tags    = o.get_file_property_list('FILETAGS')
    if len(tags) == 0:
        tags = None
    dates   = o.get_file_property('DATE')
    draft   = None if (draftp := o.get_file_property('DRAFT')) is None else True

    date: Optional[datetime]
    if dates is not None:
        dates = dates[1: 1 + len('0000-00-00 Abc')]
        date = datetime.strptime(dates, '%Y-%m-%d %a')
    else:
        date = None
    d = dict(
        title=ttl,
        summary=summ,
        date=date,
        draft=draft,
        tags=tags,
        upid=upid, # todo perhaps should always have upid?
    )
    return d

def make_meta(deps: Deps) -> Tuple[Meta, Post]:
    path = deps.path.path
    if path.is_absolute():
        path = path.relative_to(input) # meh
    apath = input / path
    # TODO not sure which path should really be used..

    meta = metadata(apath)
    # todo maybe instead, use symlinks?
    opath = path if (hpath := meta.get('html_path')) is None else Path(hpath)
    meta['url'] = '/' + str(opath.with_suffix('.html'))

    pb: List[str] = meta.get('pingback', [])
    # todo meh. just make it native dict?
    pingback = [{
        'title': x.split()[0],
        'url'  : x.split()[1],
    } for x in pb]
    meta['pingback'] = pingback

    if isinstance(deps, OrgDeps):
        ometa = {k: v for k, v in org_meta(apath).items() if v is not None}
        meta = dict(**meta, **ometa) # will throw if repeating

    meta['draft'] = md if isinstance((md := meta.get('draft', False)), bool) else md is not None

    # meh, this needs to be some sort of function converting Meta to Post??
    if 'tags' in meta:
        meta['tags'] = tuple(map(Tag, meta['tags']))

    if 'date' in meta:
        meta['date'] = datetime.strptime(datish, '%B %d, %Y') if isinstance((datish := meta['date']), str) else datish

    # todo maybe get them from Post dataclass?
    defaults = {
        'draft'    : False,
        'special'  : False,
        'has_math' : False,
        'feed'     : True,
        'is_stable': True,
        'tags'     : (),
        'summary'  : '',
    }
    meta = {**defaults, **meta}

    post = Post(**{f.name: None for f in dataclasses.fields(Post)}) # type: ignore[arg-type]
    for k, v in meta.items():
        if k in {
                'keywords',
                'pingback',
                'check_ids',
                'is_stable',
                'allow_errors',
                'html_path',
        }:
            continue
        assert hasattr(post, k), k
        setattr(post, k, v)
    post.check()
    meta['ext'] = deps.ext
    meta['in_graph'] = in_graph(post)
    return meta, post

def _compile_post_aux(deps: Deps, dir_: Path) -> Results:
    meta, post = make_meta(deps)
    ipath = deps.path.path # absolute?
    ltag = post.upid or f'<unnamed ({ipath.name})>'
    log.debug('[%s]: compiling %s', ltag, ipath)
    if isinstance(deps, OrgDeps):
        check_ids = meta.get('check_ids', post.draft is False)
        yield from compile_org_body(
            compile_script=deps.compile_org.path,
            path=ipath,
            fmt=FORMAT,
            dir_=dir_,
            active_tags=deps.active_tags,
            check_ids=check_ids,
        )
    elif isinstance(deps, IpynbDeps):
        # todo make a mode to export to python?
        compile_ipynb_body(
            compile_script=deps.compile_ipynb.path,
            path=ipath,
            dir_=dir_,
        )
        # todo assume all ipynb has math?
    elif isinstance(deps, MdDeps):
        compile_md_body(
            path=ipath,
            dir_=dir_,
        )
    else:
        raise RuntimeError(deps)

    # post.url??
    url_ = post.url
    assert url_[0] == '/', url_
    path = Path(url_[1:])
    out_ext = {'html': '.html', 'raw': ipath.suffix}[FORMAT]
    path = path.with_suffix(out_ext)
    opath = dir_ / path
    # todo can I simplify this??
    if ipath.parent != input:
        # ugh. need to move hierarchy down to preserve relative paths..
        outs_files = list(x for x in dir_.rglob('*') if not x.is_dir())
        for f in outs_files:
            r = f.relative_to(dir_)
            to = opath.parent / r
            to.parent.mkdir(parents=True, exist_ok=True)
            log.debug('moving %s %s', f, to)
            f.rename(to)
            # TODO remove empty f.parent??

    body_file = opath.parent / 'body'
    body = body_file.read_text()
    body_file.unlink()

    # strip private stuff
    # todo should strip away from the input? or both (since could be included?)
    body = ''.join(line for line in body.splitlines(keepends=True) if 'NOEXPORT' not in line)

    # TODO for org-mode, need to be able to stop here and emit whatever we compiled?
    if FORMAT == 'html':
        post_t = template('post.html')
        full_t = template('default.html')
        pbody = post_t.render(
            body=body,
            **meta,
        )
        # ugh. wtf??? I think sometimes I end up with unnecessary relative paths like href="../../../path/to/blog/page.html" (e.g. hpi post)
        if str(input) in pbody:
            # jesus.
            pbody = re.sub(r'href=".*?' + re.escape(str(input)), 'href="', pbody)
        full = full_t.render(
            body=pbody,
            **meta,
        )
        full = relativize_urls(path=path, html=full)
    else:
        full = body

    # annoying if it slips in; also linkchecker can't detect these
    # todo single quote?
    assert 'href="file://' not in full, post

    post = dataclasses.replace(post, body=full)

    opath.parent.mkdir(exist_ok=True)
    # TODO might need to move everything into the same dir??
    opath.write_text(full)


    outfiles = list(sorted(str(p.relative_to(dir_)) for p in dir_.rglob('*')))
    log.info('[%s]: outputs: %s', ltag, ' '.join(outfiles))
    yield post

# TODO should use Path with mtime etc

templates = META / 'templates'

from jinja2 import Template # type: ignore

@cache
def _template_aux(name: str, key: Tuple[MPath, ...]):
    log.debug('reloading templates %s', key)
    ts = _templates()
    return ts.get_template(name)

def template(name: str) -> Template:
    return _template_aux(name=name, key=tuple(map(mpath, sorted(templates.glob('*.html')))))

def _templates():
    from jinja2 import FileSystemLoader, Environment # type: ignore
    env = Environment(loader=FileSystemLoader(str(templates)))
    return env


@cache
def get_filtered(inputs: Tuple[Path]) -> Tuple[Path, ...]:
    if FILTER is None:
        return inputs
   
    filtered: List[Path] = []

    # TODO why isn't this covered??
    for x in inputs:
        if any(fnmatch(str(x), f) for f in FILTER):
            filtered.append(x)
        else:
            log.debug('filtered out %s', x)

    if len(filtered) == 0:
        log.warning('no files among %s are filtered by %s', list(map(str, inputs)), FILTER)
    return tuple(filtered)


def get_inputs() -> Tuple[Path]: # TODO use ...?
    assert input.exists(), input
    inputs = tuple(sorted({
        *[c.relative_to(input) for c in chain(
            input.rglob('*.md'),
            # TODO make more defensive??
            [x for x in input.rglob('*.org') if 'drafts' not in x.parts],
            input.rglob('*.ipynb'),
        ) if 'ext' not in c.parts], # ugh. otherwise picking up files from it
    }))
    return get_filtered(inputs)


def blog_tags() -> Sequence[str]:
    @cache
    @tuplify()
    def aux(tags_file: MPath) -> Iterator[str]:
        path = tags_file.path
        root = orgparse.loads(path.read_text())
        for ch in root.children:
            tag = ch.properties.get('CUSTOM_ID')
            if tag is not None:
                yield tag
            for ch2 in ch.children:
                tag = ch2.properties.get('CUSTOM_ID')
                if tag is not None:
                    yield tag
    return aux(mpath(input / 'tags.org'))


# TODO make a 'parallel' function??
# almost useless though if they are not sharing the threads...

from feedgen.feed import FeedGenerator # type: ignore

# TODO feed generation should be blog agnostic?..
@cache
def feeds(posts: Tuple[Post]) -> None:
    atom = feed(posts, 'atom')
    rss  = feed(posts, 'rss')
    afile = output / 'atom.xml'
    rfile = output / 'rss.xml'
    afile.write_text(atom.atom_str(pretty=True).decode('utf8'))
    rfile.write_text(rss .rss_str (pretty=True).decode('utf8'))
    for f in (afile, rfile):
        # some feed readers might not accept large RSS/atom feeds...
        # TODO this is also an argument against inline images I suppose
        assert f.stat().st_size < 512 * 1024, f


tz = pytz.utc # todo ugh this is what Hakyll assumed
def feed(posts: Tuple[Post], kind: str) -> FeedGenerator:
    log.debug('generating %s feed', kind)
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
        dates = (p.date for p in posts)
        fg.updated(max(tz.localize(d) if d is not None else throw() for d in dates))

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
        # TODO meh
        if post.upid == 'infra_diagram':
            content = "Sorry, this post contains a huge diagram and isn't RSS friendly. It's best viewed on the website"
        else:
            content = post.body
        fe.content(content, type='html')
    return fg


@cache
def posts_list(posts: Tuple[Post], name: str, title: str) -> None:
    log.debug('compiling %s', name)
    if FORMAT == 'html':
        name = name + '.html'
        ctx: Dict[str, Any] = {}
        ctx['is_stable'] = True
        ctx['title'] = title
        ctx['url'] = f'/{name}'
        it = template(name)
        pbody = it.render(posts=posts)
        full_t = template('default.html')
        full = full_t.render(
            body=pbody,
            **ctx,
        )
        path = Path(name)
        full = relativize_urls(path=path, html=full)
    elif FORMAT == 'raw':
        path = Path(name).with_suffix('.org')
        header = '''
This is a raw plaintext export for my blog, [[https://beepb00p.xyz][beepb00p.xyz]].

Note that due to [[https://github.com/novoid/github-orgmode-tests][Github's inconsistencies]] at rendering org-mode, not everything might render correctly.

You can also clone this repository and browse the blog in Emacs.

Extra pages: [[file:ideas.org][IDEAS]] | [[file:tags.org][TAGS]] | [[file:site.org][SITE]]
'''.strip()
        # TODO cloudmacs instance?
        lines = [header]
        maxwidth = 80
        def fmt_tag(t: Tag) -> str:
            ttl = f'=#{t.name}='
            if t.exists:
                return f'[[file:tags.org::#{t.name}][ {ttl} ]]' # ugh, spaces seem to be necessary for github??
            else:
                return ttl
        for p in posts:
            pp = Path(p.url[1:]).with_suffix('.org') # TODO ugh. need to keep orig path in the meta??
            d = p.date; assert d is not None
            tags = p.tags
            # ugh. fuck, actual tags don't work on github.
            tagss = '' if len(tags) == 0 else ('   ' + ' '.join(map(fmt_tag, tags)))
            link = f'[[file:{pp}][{p.title}]]' + ' ' * max(0, maxwidth - len(p.title))
            # use second level heading, otherwise github renders them too huge..
            lines.append(f'** [{d.strftime("%Y-%m-%d %a")}] {link}{tagss}')
            summary = p.summary
            if len(summary) > 0:
                lines.append('  ' + summary)
        full = '\n'.join(lines)
    else:
        throw()

    (output / path).write_text(full)


def compile_all(max_workers: Optional[int]=None) -> Iterable[Exception]:
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
    def copy(from_: Path, to: PathIsh) -> None:
        if isinstance(to, str):
            top = output / to
        else:
            assert not to.is_absolute(), to
            top = to

        top = output / to
        top.parent.mkdir(exist_ok=True, parents=True)
        shutil.copy(from_, top)

    # TODO fuck. doesn't follow symlinks...
    # TODO just keep them in the target repository instead.. fuck this copying
    assets = Path('assets')
    copy(META / 'meta/robots.txt'    , 'robots.txt')
    copy(META / 'meta/robot-face.png', assets / 'favicon.png')
    for f in (META / 'css').rglob('*.css'):
        copy(f, assets / f.relative_to(META))
    for f in (META / 'images').rglob('*.svg'):
        copy(f, assets / f.relative_to(META))
    for f in (ROOT / 'pic').glob('*.png'):
        copy(f, f.relative_to(ROOT)) # ugh.
    for f in input.glob('*.html'): # meh
        copy(f, f.relative_to(input))
    # eh. apparently glob(recursive=True) always follows symlinks??
    # TODO make these proper dependensies? dunno
    for p in chain.from_iterable(
            glob(f'{input}/**/*.{x}', recursive=True)
            for x in ('jpg', 'svg', 'png', 'svg')
    ):
        f = Path(p)
        if 'ext' in f.parts:
            # todo ugh... hack, need to remove..
            continue
        copy(f, f.relative_to(input))

    posts = []
    # todo hmm, why did ThreadPoolExecutor stop working?
    from concurrent.futures import ProcessPoolExecutor as Pool
    with Pool(max_workers=max_workers) as pool:
        for rpost in pool.map(compile_post, (mpath(input / p) for p in get_inputs())):
            for post in rpost:
                if isinstance(post, Exception):
                    ex = post
                    import traceback
                    parts = traceback.format_exception(Exception, ex, ex.__traceback__)
                    log.error(''.join(parts))
                    yield ex
                else:
                    posts.append(post)

    posts = list(reversed(sorted(posts, key=lambda p: datetime.min if p.date is None else p.date)))

    for_index  = tuple(p for p in posts if not p.draft and not p.special)
    for_drafts = tuple(p for p in posts if p.draft)
    posts_list(for_index , 'index' , 'Home')
    posts_list(for_drafts, 'drafts', 'Drafts')

    if FORMAT == 'html':
        for_feed = tuple((p for p in for_index if p.feed))[:9] # TODO FIXME add full feed?
        feeds(for_feed)
    else:
        (output / 'readme.org').symlink_to('index.org')


def clean() -> None:
    for f in output.iterdir():
        if f.is_dir():
            shutil.rmtree(f)
        else:
            f.unlink()

def serve() -> None:
    # TODO less logging??
    from http.server import HTTPServer, SimpleHTTPRequestHandler
    Handler = lambda *args: SimpleHTTPRequestHandler( # type: ignore[misc]
        *args,
        directory=str(output),
    )
    server_address = ('', 8000)
    log.info("serving %s", server_address)
    server = HTTPServer(server_address, Handler)
    import threading
    thread = threading.Thread(target=server.serve_forever)
    # TODO shutdown server on exit??
    thread.start()


def main() -> None:
    import argparse
    p = argparse.ArgumentParser()
    p.add_argument('--debug' , action='store_true', help='Debug logging')
    p.add_argument('--filter', action='append', required=False, type=str, help='glob to filter the inputs')
    p.add_argument('--serve' , action='store_true') # TODO host/port?
    p.add_argument('--watch' , action='store_true')
    p.add_argument('--input' , type=Path, default=ROOT / 'data/input', required=False)
    p.add_argument('--output', type=Path, default=ROOT / 'data/html' , required=False)
    p.add_argument('--format', type=str , default='html'             , required=False)
    args = p.parse_args()

    global input, output
    input  = args.input.absolute()
    output = args.output.absolute()

    debug = args.debug
    setup_logging(level=getattr(logging, 'DEBUG' if debug else 'INFO'))

    if args.serve:
        serve()

    watch = args.watch or args.serve

    global FILTER; FILTER = args.filter
    global FORMAT; FORMAT = args.format

    clean()
    global TMP_DIR
    # TODO create it in the output dir? so it's on the same filesystem
    with TemporaryDirectory() as tdir:
        TMP_DIR = Path(tdir)
        log.debug('using temporary directory: %s', TMP_DIR)
        while True:
            log.debug('detecting changes...')
            # TODO all cores - 1??
            errors = list(compile_all(max_workers=7))
            if len(errors) > 0:
                log.error('%d errors during the compilation', len(errors))

            if not watch:
                if len(errors) > 0:
                    sys.exit(1)
                break
            time.sleep(1)


def setup_logging(level) -> None:
    import logzero # type: ignore
    format = '%(color)s[%(levelname)1.1s %(asctime)s %(filename)s:%(lineno)-4d %(threadName)s]%(end_color)s %(message)s'
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

# TODO add redirects? maybe even to the source, copy htmls as is

# TODO FIXME make upid obligatory
