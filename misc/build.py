#!/usr/bin/env python3
from pathlib import Path
from functools import lru_cache, wraps
import os
from subprocess import check_call, run, check_output, PIPE
import shutil
import sys
from tempfile import TemporaryDirectory
from typing import cast, Dict, Any, List

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
    return d


content = Path('content')


from typing import Dict
Meta = Dict[str, Any]



# pip install ruamel.yaml -- temporary...
# just use json later?
def metadata(path: Path) -> Meta:
    meta = path.with_suffix(path.suffix + '.metadata')
    if not meta.exists():
        return {}
    from ruamel.yaml import YAML # type: ignore
    yaml = YAML(typ='safe')
    return yaml.load(meta)


def compile_ipynb(*, compile_script: Path, path: Path) -> Path:
    d = TMP_DIR / path.name
    d.mkdir()

    meta = metadata(path)

    # TODO make this an attribute of the notebook somehow? e.g. tag
    allow_errors = meta.get('allow_errors', False)

    # meh
    itemid = path.absolute().relative_to(content.absolute().parent)
    res = run(
        [
            compile_script,
            '--output-dir', d,
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


def compile_post(path: Path) -> Path:
    assert not path.is_absolute(), path
    apath = content / path

    suffix = path.suffix

    meta = metadata(apath)

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


    # TODO FIMXE compile_org should return a temporary directory with 'stuff'?
    outs: Path
    if suffix == '.org':
        outs = compile_org(
            compile_script=Path('misc/compile_org.py'),
            path=apath,
        )
        ctx['style_org'] = True
        ctx['type_special'] = False
        ctx['is_stable'] = True

        import orgparse # type: ignore
        o = orgparse.loads(apath.read_text())
        # TODO need to make public?? also can remove from porg then..
        fprops = o._special_comments

        # print(fprops)

        ttls = fprops.get('TITLE')
        if ttls is not None:
            ctx['title'] = the(ttls)

        summs = fprops.get('SUMMARY')
        if summs is not None:
            ctx['summary'] = the(summs)

        upids = fprops.get('UPID')
        if upids is not None:
            ctx['issoid'] = 'isso_' + the(upids)

        # dates = fprops.get('DATE')
        # if dates is not None:
        #     ctx['date'] = the(dates)
        from datetime import datetime
        dates = meta['date']
        date = datetime.strptime(dates, '%B %d, %Y').strftime('%d %B %Y')
        ctx['date'] = date

        ftagss = fprops.get('FILETAGS')
        if ftagss is not None:
            ftags = the(ftagss)
            tags = list(x for x in ftags.split(':') if len(x) > 0)
            ctx['tags'] = [{'body': tag, 'sep': '' if i == len(tags) - 1 else ' '} for i, tag in enumerate(tags)]

    elif suffix == '.ipynb':
        # TODO make a mode to export to python?
        outs = compile_ipynb(
            compile_script=Path('misc/compile-ipynb'),
            path=apath,
        )
        ctx['style_ipynb'] = True
    else:
        raise RuntimeError(apath)

    assert ctx['title'] is not None, ctx
    assert ctx['date']  is not None, ctx

    body_file = outs / 'body'
    body = (outs / 'body').read_text()

    body_file.unlink()

    # TODO for org-mode, need to be able to stop here and emit whatever we compiled?
    post_t = template('templates/post.html')
    post = post_t.render(
        body=body,
        **ctx,
    )
    full_t = template('templates/default.html')
    full = full_t.render(
        body=post,
        **ctx,
    )

    # relativize urls
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
    #


    opath = outs / path.with_suffix('.html')
    opath.parent.mkdir(exist_ok=True)
    opath.write_text(full)

    return outs


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

    inputs = Path('templates')
    outputs = output / 'templates'
    outputs.mkdir(exist_ok=True)

    for t in inputs.glob('*.html'):
        out = outputs / t.name
        body = hakyll_to_jinja(t.read_text())
        out.write_text(body)

    env = Environment(
        loader=FileSystemLoader(str(output)),
    )
    return env


INPUTS = list(sorted({
    # Path('configs-suck.org'),
    Path('annotating.org'),
    # content / 'myinfra.org',
    # *content.glob('*.org'),
    # content / 'wave.ipynb',
    # content / 'contemp-art.org',
    # Path('sandbox/test.org'),
    # *content.glob('*.ipynb'),
}))


def compile_all(max_workers=None):
    def move(from_: Path, ext: str):
        for f in from_.rglob('*.' + ext):
            dbg('merging %s', f)
            rel = f.relative_to(from_)
            to = output / rel
            assert not to.exists(), to


            to.parent.mkdir(exist_ok=True) # meh
            shutil.move(f, to)

    from concurrent.futures import ThreadPoolExecutor
    with ThreadPoolExecutor(max_workers=max_workers) as pool:
        for res in pool.map(compile_post, INPUTS):
            move(res, 'html')
            move(res, 'png')

            # TODO remove all empty dirs???
            for root, dirs, files in os.walk(res, topdown=False):
                if len(dirs) + len(files) == 0:
                    Path(root).rmdir()

            if res.exists():
                remaining = list(res.iterdir())
                if len(remaining) > 0:
                    raise RuntimeError(f'remaining files: {remaining}')


def clean():
    import shutil
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
