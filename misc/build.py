#!/usr/bin/env python3
from pathlib import Path
from functools import lru_cache, wraps
from subprocess import check_call, run, check_output, PIPE
import sys
from tempfile import TemporaryDirectory

from kython.klogging2 import LazyLogger

log = LazyLogger('build', level='debug')

# TODO use wraps??
cache = lambda f: lru_cache(1)(f)

# TODO use injector??
# https://github.com/alecthomas/injector

# TODO def should detect stuff automatically?
# TODO make sure it's cached for feed and index page..
# TODO use threads? most of it is going to be io bound anyway



output = Path('site2')
# TODO not sure if should create it first?

# TODO make emacs a bit quieter; do not display all the 'Loading' stuff
# TODO needs to depend on compile_script and path
def compile_org(*, compile_script: Path, path: Path) -> str:
    # TODO add COMPILE_ORG to dependency?
    with TemporaryDirectory() as tdir:
        tpath = Path(tdir)
        res = run(
            [
                compile_script,
                '--output-dir', tdir,
                # '--org',  # TODO
            ],
            input=path.read_bytes(),
            stdout=PIPE,
            stderr=PIPE,
            check=True,
        )
    out = res.stdout
    err = res.stderr

    # TODO filter this in emacs/compile-org?
    filtered = []
    for line in err.decode('utf8').splitlines():
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
    return out.decode('utf8')


content = Path('content')


from typing import Dict
Meta = Dict[str, str]



# pip install ruamel.yaml -- temporary...
# just use json later?
def metadata(path: Path) -> Meta:
    meta = path.with_suffix(path.suffix + '.metadata')
    if not meta.exists():
        return {}
    from ruamel.yaml import YAML # type: ignore
    yaml = YAML(typ='safe')
    return yaml.load(meta)


# TODO allow-errors?
def compile_ipynb(*, compile_script: Path, path: Path):
    meta = metadata(path)

    # TODO make this an attribute of the notebook somehow? e.g. tag
    allow_errors = meta.get('allow_errors', False)

    # meh
    itemid = path.absolute().relative_to(content.absolute().parent)
    with TemporaryDirectory() as tdir:
        tpath = Path(tdir)
        res = run(
            [
                compile_script,
                '--output-dir', tdir,
                '--item', str(itemid),
                *(['--allow-errors'] if allow_errors else []),
            ],
            input=path.read_bytes(),
            stdout=PIPE,
            check=True,
        )
    # TODO remove duplicats
    out = res.stdout
    outpath = output / (path.stem + '.html')
    outpath.write_bytes(res.stdout)


# TODO move out?
import re
# eh, I'm not sure why hakyll chose a different template format...
# considering I adapted it with string replacement..
def hakyll_to_jinja(body: str) -> str:
    replacements = [
        ('$if('     , '{% if '         ),
        ('$endif$'  , '{% endif %}'    ),
        ('$for('    , '{% for item in '),
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


def compile_post(path: Path):
    suffix = path.suffix

    # TODO FIMXE compile_org should return a temporary directory with 'stuff'?
    if suffix == '.org':
        body = compile_org(
            compile_script=Path('misc/compile_org.py'),
            path=path,
        )
        # TODO for org-mode, need to be able to stop here and emit whatever we compiled?
        post_t = template('templates/post.html')
        post = post_t.render(
            body=body,
        )
        full_t = template('templates/default.html')
        full = full_t.render(
            body=body,
        )
        # print(rendered)
    elif suffix == '.ipynb':
        # TODO make a mode to export to python?
        compile_ipynb(
            compile_script=Path('misc/compile-ipynb'),
            path=path,
        )
    else:
        raise RuntimeError(path)


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
    *content.glob('*.org'),
    *content.glob('*.ipynb'),
}))


def compile_all(max_workers=None):
    from concurrent.futures import ThreadPoolExecutor
    with ThreadPoolExecutor(max_workers=max_workers) as pool:
        for res in pool.map(compile_post, INPUTS):
            # need to force the iterator
            pass


def main():
    compile_all()


if __name__ == '__main__':
    main()


# TODO self check with mypy/pylint??


# TODO make sure to port todo stripping
