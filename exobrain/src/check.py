#!/usr/bin/env python3
import os
from pathlib import Path
from subprocess import run
from typing import Iterator, List


import orgparse


class Failed(RuntimeError):
    pass


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
        yield Failed(res)


def check(path: Path) -> Iterator[Failed]:
    if 'CI' not in os.environ:
        from checks import F_CHECKS, WORD_CHECKS, TAG_CHECKS
    else:
        F_CHECKS = []
        WORD_CHECKS = []
        TAG_CHECKS = set()

    print(f"checking {path}")
    for x in F_CHECKS:
        yield from search(
            '-F',
            x,
            path,
        )
    for x in WORD_CHECKS:
        yield from search(
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
        if len(d) != 0:
            yield Failed((d, line))

    o = orgparse.loads(path.read_text())
    for n in o:
        found = n.tags.intersection(TAG_CHECKS)
        if len(found) > 0:
            yield Failed((path, n.heading, found))


def check_aux(path: Path) -> List[str]:
    # helper for multiprocessing..
    return list(map(str, check(path)))


def check_org(path: Path):
    # TODO not sure about org?
    org_files = list(sorted(path.rglob('*.org')))

    from concurrent.futures import ProcessPoolExecutor as Pool
    with Pool() as pool:
        for f, res in zip(org_files, pool.map(check_aux, org_files)):
            for x in res:
                # TODO collect errors, report once?
                raise Failed(f, x)


def main():
    raise RuntimeError


if __name__ == '__main__':
    main()
