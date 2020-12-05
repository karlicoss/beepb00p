#!/usr/bin/env python3
from pathlib import Path
from time import sleep
import subprocess


input  = Path('testdata/input')
output = Path('testdata/output')

CMD = [
    'src/build.py',
    '--input' , str(input),
    '--output', str(output),
]


def test_basic() -> None:
    subprocess.check_call(CMD)


def wait(total_s: float, step_s: float):
    for _ in range(int(total_s / step_s)):
        yield
        sleep(step_s)
    else:
        raise TimeoutError(f'Timed out after {total_s} secs')


def mtime(p: Path):
    try:
        return p.stat().st_mtime
    except FileNotFoundError as fe:
        return None


def test_watch() -> None:
    i = input  / 'post_1.org'
    tags = input / 'tags.org'
    o = output / 'post_1.html'
    i.touch()
    if o.exists():
        o.unlink()
    mi = mtime(i)
    mo = None
    with tmp_popen(CMD + ['--watch']) as p:
        for _ in wait(5, 0.1):
            mo = mtime(o)
            if (mo or 0.0) > mi:
                break
        i.touch()
        assert mtime(o) < mtime(i)
        for _ in wait(5, 0.1):
            if mtime(o) > mtime(i):
                break



from contextlib import contextmanager
@contextmanager
def tmp_popen(*args, **kwargs):
    import psutil # type: ignore
    with psutil.Popen(*args, **kwargs) as p:
        try:
            yield p
        finally:
            for c in p.children(recursive=True):
                c.kill()
            p.kill()
            p.wait()
