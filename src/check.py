#!/usr/bin/env python3
from pathlib import Path
from subprocess import run
from typing import Iterator, List


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


import orgparse # type: ignore

from checks import WORD_CHECKS, TAG_CHECKS


def check(path: Path) -> Iterator[Failed]:
    print(f"checking {path}")
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
    org_files = list(sorted(path.glob('**/*.org')))

    from concurrent.futures import ProcessPoolExecutor as Pool
    with Pool() as pool:
        for res in pool.map(check_aux, org_files):
            for f in res:
                # TODO collect errors, report once?
                raise Failed(f)


def main():
    check_org(Path('intermediate'))


if __name__ == '__main__':
    main()
