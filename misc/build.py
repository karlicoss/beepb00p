#!/usr/bin/env python3
from pathlib import Path
from subprocess import check_call, run, check_output, PIPE
from tempfile import TemporaryDirectory

# TODO use injector??
# https://github.com/alecthomas/injector

# TODO def should detect stuff automatically?
# TODO make sure it's cached for feed and index page..
# TODO use threads? most of it is going to be io bound anyway



output = Path('site2')
# TODO not sure if should create it first?

COMPILE_ORG = Path('misc/compile_org.py')


def compile_org(*, compile_script: Path, path: Path):
    # TODO add COMPILE_ORG to dependency?
    with TemporaryDirectory() as tdir:
        tpath = Path(tdir)
        res = run(
            [
                compile_script,
                '--output-dir', tdir,
                '--org',  # TODO
            ],
            input=path.read_bytes(),
            stdout=PIPE,
            check=True,
        )
    out = res.stdout
    # TODO how to clean stale stuff that's not needed in output dir?
    outpath = output / (path.stem + '.org')
    outpath.write_bytes(res.stdout)


def compile_post(path: Path):
    assert path.suffix == '.org'
    compile_org(
        compile_script=Path('misc/compile_org.py'),
        path=path,
    )


def main():
    posts = [Path('content/python-configs.org')]
    for post in posts:
        compile_post(post)


if __name__ == '__main__':
    main()
