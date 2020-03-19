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

# TODO make emacs a bit quieter; do not display all the 'Loading' stuff
# TODO needs to depend on compile_script and path
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
    # TODO output determined externally?
    # TODO some inputs
    outpath = output / (path.stem + '.org')
    outpath.write_bytes(res.stdout)


content = Path('content')


# TODO allow-errors?
def compile_ipynb(*, compile_script: Path, path: Path):
    # meh
    itemid = path.absolute().relative_to(content.absolute().parent)
    with TemporaryDirectory() as tdir:
        tpath = Path(tdir)
        res = run(
            [
                compile_script,
                '--output-dir', tdir,
                '--item', str(itemid),
            ],
            input=path.read_bytes(),
            stdout=PIPE,
            check=True,
        )
    # TODO remove duplicats
    out = res.stdout
    outpath = output / (path.stem + '.html')
    outpath.write_bytes(res.stdout)


def compile_post(path: Path):
    suffix = path.suffix

    if suffix == '.org':
        compile_org(
            compile_script=Path('misc/compile_org.py'),
            path=path,
        )
    elif suffix == '.ipynb':
        compile_ipynb(
            compile_script=Path('misc/compile-ipynb'),
            path=path,
        )
    else:
        raise RuntimeError(path)




INPUTS = list(sorted({
    *content.glob('*.org'),
    *content.glob('*.ipynb'),
}))


def compile_all(max_workers=None):
    from concurrent.futures import ThreadPoolExecutor
    print(INPUTS)
    with ThreadPoolExecutor(max_workers=max_workers) as pool:
        for res in pool.map(compile_post, INPUTS):
            # need to force the iterator
            pass


def main():
    compile_all()


if __name__ == '__main__':
    main()


# TODO self check with mypy/pylint??
