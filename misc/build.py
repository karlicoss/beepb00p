#!/usr/bin/env python3
from pathlib import Path
from functools import lru_cache, wraps
import os
from subprocess import check_call, run, check_output, PIPE
from datetime import datetime
import shutil
import sys
from tempfile import TemporaryDirectory
from typing import cast, Dict, Any, List, Optional, Union, NamedTuple, Tuple


import pytz # type: ignore

# TODO  meh.
# tz = pytz.timezone('Europe/London')
# ugh. this is what Hakyll assumed
tz = pytz.utc


from kython.klogging2 import LazyLogger

log = LazyLogger('build', level='debug')

# TODO use wraps??
cache = lambda f: lru_cache(1)(f)

# TODO use injector??
# https://github.com/alecthomas/injector

# TODO def should detect stuff automatically?
# TODO make sure it's cached for feed and index page..
# TODO use threads? most of it is going to be io bound anyway


# ok, functions are 'kinda pure'
# not completely because of filesystem use, but mostly pure

PathIsh = Union[Path, str]

TMP_DIR: Path = cast(Path, None)

RPath = Path

# TODO better name?..
def sanitize(path: RPath) -> str:
    pp = str(path)
    pp = pp.replace('/', '_')
    return pp

# TODO not sure if should create it first?

# TODO make emacs a bit quieter; do not display all the 'Loading' stuff
# TODO needs to depend on compile_script and path
# TODO add COMPILE_ORG to dependency?
def compile_org(*, compile_script: Path, path: Path) -> Path:
    dname = sanitize(path)
    d = TMP_DIR / dname
    d.mkdir()

    log.debug('Running %s %s', compile_script, d)

    res = run(
        [
            compile_script,
            '--output-dir', d,
            # '--org',  # TODO
        ],
        input=path.read_bytes(),
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
        print(err, file=sys.stderr)


    # TODO how to clean stale stuff that's not needed in output dir?
    # TODO output determined externally?
    # TODO some inputs
    log.debug('org-mode compile results: %s', list(map(str, sorted(d.rglob('*')))))

    return d


# TODO meh, make it relative to the script?
ROOT = Path('.').absolute()
content = Path('content').absolute()


from typing import Dict
Meta = Dict[str, Any]


def pandoc_meta(path: Path) -> Meta:
    # wow, that's quite horrible...
    with TemporaryDirectory() as td:
        t = Path(td) / 'meta.tpl'
        t.write_text('$meta-json$')
        res = check_output([
            'pandoc',
            '--template', t,
            path
        ])

        import json
        # TODO might be none??
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
        [
            'pandoc',
            '--to', 'html',
        ],
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


# TODO move out?
import re
# eh, I'm not sure why hakyll chose a different template format...
# considering I adapted it with string replacement..
def hakyll_to_jinja(body: str) -> str:
    replacements = [
        ('$if('     , '{% if '         ),
        ('$else$'   , '{% else %}'     ),
        ('$endif$'  , '{% endif %}'    ),
        ('$for('    , '{% for item in '),
        # meh.
        ('$body$$sep$ '    ,
         '{{ item.body }}{{ item.sep }}'),
        ('$body$">#$body$</a>$sep$ ',
         '{{ item.body }}">#{{ item.body }}</a> {{ item.sep }}'),

        ('$url$">$title$',
         '{{ item.url }}">{{ item.title }}'),

        ("inlineMath: [ ['$$','$$']",
         "inlineMath: [ ['$','$']"),

        ('$endfor$' , '{% endfor %}'   ),
        ('$partial(', '{% include '    ),
        (')$'       , ' %}'            ),
    ]
    for f, t in replacements:
        body = body.replace(f, t)
    body = re.sub(r'\$(\w+)\$', r'{{ \1 }}', body)

    for line in body.splitlines():
        pass
        # assert '$' not in line, line
    return body

# TODO not sure how metadata should be handled... does every post really have
# TODO if we use templates in python (as f-strings), they can be statically checked..

def the(things):
    ss = set(things)
    assert len(ss) == 1, ss
    [x] = ss
    return x



class Post(NamedTuple):
    title: str
    summary: str
    date: Optional[datetime]
    body: str
    draft: bool
    special: bool
    tags: List[str]
    url: str

    @property
    def dates(self) -> Optional[str]:
        d = self.date
        if d is None:
            return None
        return d.strftime('%d %B %Y')


def compile_post(path: Path) -> Tuple[Path, Post]:
    try:
        return _compile_post(path)
    except Exception as e:
        raise RuntimeError(f'While compiling {path}') from e


def _compile_post(path: Path) -> Tuple[Path, Post]:
    assert not path.is_absolute(), path
    apath = content / path

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
            compile_script=Path('misc/compile_org.py'),
            path=apath,
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

    # meh
    check_call(['tree', outs])

    opath = outs / path

    if opath.parent != outs:
        # ugh. need to move hierarchy down to preserve relative paths..
        outs_files = list(x for x in outs.rglob('*') if not x.is_dir())
        log.debug("OUTS: %s", outs_files)
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


    # TODO for org-mode, need to be able to stop here and emit whatever we compiled?
    post_t = template('templates/post.html')
    pbody = post_t.render(
        body=body,
        **ctx,
    )
    full_t = template('templates/default.html')
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
    )


    #
    if 'name="keywords"' not in full:
        loc = '<meta content="English" name="language"/>'
        assert loc in full, full
        full = full.replace(loc, loc + '\n' + '<meta name="keywords" content>')

    opath.parent.mkdir(exist_ok=True)
    # TODO might need to move everything into the same dir??
    opath.write_text(full)

    check_call(['tree', outs])


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
    full = soup.prettify()
    return full


from jinja2 import Template, Environment, FileSystemLoader # type: ignore


# TODO should use Path with mtime etc


import threading
# right, separate threads might load twice

def dbg(fmt: str, *args, **kwargs):
    log.debug('%s: ' + fmt, threading.current_thread().name, *args, **kwargs)



@cache
def template(name: str) -> Template:
    dbg('reloading template %s', name, )
    ts = templates()
    return ts.get_template(name)


output = Path('site2')

@cache
def templates():
    dbg('reloading all templates')

    outputs = output / 'templates'

    # ugh. a bit horrible
    bdir = output / 'tmp'
    bdir.mkdir(exist_ok=True)
    # tdir needs on the same FS as output, otherwise it's not possible to do an atomic rename...

    if not outputs.exists():
        with TemporaryDirectory(dir=bdir) as td:
            tdir = Path(td)
            for t in chain.from_iterable(Path(td).glob('*.html') for td in ('templates', 'meta')):
                out = tdir / t.name
                tt = t.read_text()
                body = hakyll_to_jinja(tt)

                if t.name == 'post-list.html':
                    # meh. TODO remove later...
                    for a, b in [
                            ('{{ url }}'     , '{{ item.url }}'    ),
                            (' draft '       , ' item.draft '      ),
                            (' title '       , ' item.title '      ),
                            (' summary '     , ' item.summary '    ),
                            (' date '        , ' item.dates '      ),
                            (' item in tags ', ' tag in item.tags '),
                            (' body '        , ' tag '             ),
                            (' if item.summary ' ,
                             ' if item.summary is not none'),
                    ]:
                        body = body.replace(a, b)

                out.write_text(body)

            try:
                tdir.rename(outputs)
            except OSError as e:
                if e.errno == 39: # 'Directory not empty'
                    # ok, someone else managed to do it first
                    pass
                else:
                    raise
            else:
                # prevent cleanup from complaining...
                tdir.mkdir()

    env = Environment(loader=FileSystemLoader(str(output)))
    return env

from itertools import chain

INPUTS = list(sorted({
    *[c.relative_to(content) for c in chain(
        # TODO md??
        content.glob('*.org'),
        content.glob('*.ipynb'),
    )],
    # Path('configs-suck.org'),
    # Path('exports.org'),
    # Path('scrapyroo.org'),
    # Path('tags.org'),
    # content / 'myinfra.org',
    # *content.glob('*.org'),
    # Path('wave.ipynb'),
    # Path('ipynb-singleline.ipynb'),
    # content / 'contemp-art.org',
    # Path('sandbox/test.org'),
    # Path('sandbox/testipython.ipynb'),
    # *content.glob('*.ipynb'),
}))


# TODO make a 'parallel' function??
# almost useless though if they are not sharing the threads...

def feed(posts: List[Post]):
    from feedgen.feed import FeedGenerator # type: ignore
    fg = FeedGenerator()
    fg.title('beepb00p')
    fg.author(name='karlicoss', email='karlicoss@gmail.com')

    bb = lambda x: f'https://beepb00p.xyz{x}'
    fg.id(bb('/atom.xml'))
    fg.link(rel='self', href=bb('/atom.xml'))
    fg.link(href=bb(''))
    fg.updated(max(tz.localize(p.date) for p in posts))

    # eh, apparnetly in adds items to the feed from bottom to top...
    for post in reversed(posts):
        fe = fg.add_entry()
        fe.id(bb(post.url))
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
        # TODO remove rss etc from contents. maybe?
        fe.content(post.body, type='CDATA')


    atomfeed = fg.atom_str(pretty=True)
    (output / 'atom.xml').write_text(atomfeed.decode('utf8'))


def posts_list(posts: List[Post], name: str, title: str):
    it = template(f'templates/{name}')

    pbody = it.render(posts=posts)

    ctx: Dict[str, Any] = {}
    ctx['is_stable'] = True
    ctx['title'] = title
    ctx['url'] = f'/{name}'

    full_t = template('templates/default.html')
    full = full_t.render(
        body=pbody,
        **ctx,
    )

    path = Path(name)
    full = relativize_urls(path, full)

    (output / path).write_text(full)


def compile_all(max_workers=None):
    def move(from_: Path, ext: str):
        for f in from_.rglob('*.' + ext):
            dbg('merging %s', f)
            rel = f.relative_to(from_)
            to = output / rel
            assert not to.exists(), to

            to.parent.mkdir(exist_ok=True) # meh
            shutil.move(f, to)

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

    from glob import glob

    # TODO fuck. doesn't follow symlinks...
    copy(ROOT / 'meta/robots.txt'    , 'robots.txt')
    copy(ROOT / 'meta/robot-face.png', 'robot-face.png')
    for f in (ROOT / 'css').rglob('*.css'):
        copy(f, f.relative_to(ROOT))
    for f in (ROOT / 'images').rglob('*.svg'):
        copy(f, f.relative_to(ROOT))
    # eh. apparently glob(recursive=True) always follows symlinks??
    for p in chain.from_iterable(glob(f'{content}/**/*.{x}', recursive=True) for x in ('jpg', 'svg', 'png')):
        f = Path(p)
        copy(f, f.relative_to(content))

    # TODO atom
    # TODO drafts page
    # TODO index page
    # TODO rss
    #
    # TODO test.png shoud go in sandbox??

    posts = []
    from concurrent.futures import ThreadPoolExecutor
    with ThreadPoolExecutor(max_workers=max_workers) as pool:
        for res, post in pool.map(compile_post, INPUTS):
            posts.append(post)
            move(res, 'html')
            move(res, 'png')

            # TODO remove all empty dirs???
            for root, dirs, files in os.walk(res, topdown=False):
                if len(files) == 0:
                    for d in dirs:
                        dd = (Path(root) / d)
                        dd.rmdir()

            if res.exists():
                remaining = list(res.rglob('*'))
                if len(remaining) > 0:
                    raise RuntimeError(f'remaining files: {remaining}')

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
    feed(for_index)



def clean():
    for f in output.iterdir():
        if f.is_dir():
            shutil.rmtree(f)
        else:
            f.unlink()


def main():
    clean()
    global TMP_DIR
    with TemporaryDirectory() as tdir:
        TMP_DIR = Path(tdir)
        compile_all()


if __name__ == '__main__':
    main()


# TODO self check with mypy/pylint??


# TODO make sure to port todo stripping
# TODO make urls relative?..

# TODO feed needs to be compact to fit in 512K limit...
