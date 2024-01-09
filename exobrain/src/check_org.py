from concurrent.futures import ProcessPoolExecutor
import os
from pathlib import Path
from subprocess import run
from typing import Iterator, List


import orgparse


# TODO !! implement a test for this (all of params)
def search(*args) -> Iterator[Exception]:
    res = run(['rg', '--follow', '-i', *args])
    if res.returncode == 1:
        return  # ok, nothin is found
    else:
        yield RuntimeError(res)


def check_one(path: Path) -> Iterator[Exception]:
    if 'CI' not in os.environ:
        from checks import F_CHECKS, WORD_CHECKS, TAG_CHECKS
    else:
        F_CHECKS = []
        WORD_CHECKS = []
        TAG_CHECKS = set()

    for x in F_CHECKS:
        # TODO might be too many calls, maybe do it in a single regex?
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
            yield RuntimeError(d, line)

    o = orgparse.loads(path.read_text())
    for n in o:
        found = n.tags.intersection(TAG_CHECKS)
        if len(found) > 0:
            yield RuntimeError(path, n.heading, found)


def _check_one(path: Path) -> List[str]:
    # helper for multiprocessing..
    return list(map(str, check_one(path)))


def check_all(path: Path) -> None:
    # TODO not sure about org?
    org_files = sorted(path.rglob('*.org'))

    with ProcessPoolExecutor() as pool:
        for f, res in zip(org_files, pool.map(_check_one, org_files)):
            for x in res:
                # TODO collect errors, report once?
                raise RuntimeError(x)
