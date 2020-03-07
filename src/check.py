#!/usr/bin/env python3
from pathlib import Path
from subprocess import run


# TODO !! implement a test for this (all of params)
def search(*args):
    res = run([
        'rg',
        '--follow',
        '-i',
        *args,
    ])
    if res.returncode == 1:
        return # ok, nothin is found
    else:
        raise RuntimeError(res)


import orgparse

from checks import WORD_CHECKS

def check(path: Path):
    print(f"checking {path}")
    for x in WORD_CHECKS:
        search(
            '--word-regexp',
            x,
            path,
        )

    # TODO not sure if should rely on a unit test?
    ts = orgparse.date.TIMESTAMP_RE
    for line in path.read_text().splitlines():
        m = ts.search(line)
        if m is None:
            continue
        allowed = {
            'inactive_year',
            'inactive_month',
            'inactive_day',
        }
        d = {k: v for k, v in m.groupdict().items() if v is not None and k not in allowed}
        assert len(d) == 0, (d, line)


def check_org(path: Path):
    # TODO not sure about org?
    for f in path.glob('**/*.org'):
        check(f)


def main():
    check_org(Path('intermediate'))


if __name__ == '__main__':
    main()
