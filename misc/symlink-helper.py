#!/usr/bin/env python3
# Hakyll watching doesn't follow symlinks :( https://github.com/jaspervdj/hakyll/issues/502
# This scripts sets up watches on all symlinks and updates them (touches) every time the actual file changes
# TODO link this script to the issue
from typing import Dict
from pathlib import Path
from subprocess import check_output
from pprint import pprint

# pip install inotify
import inotify.adapters
import inotify.constants as ics


def get_symlinks(target: Path):
    # TODO what should it do if the whole dir is followed?
    out = check_output([
        'find',
        str(target),
        '-type', 'l',
        '-print0'
    ])
    symlinks = out.decode('utf8').strip('\0').split('\0')
    return list(map(Path, symlinks))


def run(target: Path):
    # TODO run inotify against target to detect *new* symlinks?
    # for now even this way is kinda ok
    i = inotify.adapters.Inotify()

    # TODO that could also be controlled by inotify?
    symlinks = get_symlinks(target.resolve())
    path2link: Dict[Path, Path] = {} # reverse lookup

    print("Tracking:")
    pprint(symlinks)

    for s in symlinks:
        try:
            res = s.resolve()
        except Exception as e:
            print(e)
            continue
        path2link[res] = s
        i.add_watch(str(res), mask=ics.IN_MODIFY | ics.IN_ATTRIB)

    def bump_mtime(path: Path):
        symlink = path2link[path]
        print("Detected change: ", path, symlink)
        check_output(["touch", "-h", str(symlink)])

    for event in i.event_gen(yield_nones=False):
        (_, type_names, path, filename) = event

        try:
            bump_mtime(Path(path))
        except Exception as e:
            print("ERROR ", e)


def main():
    import argparse
    p = argparse.ArgumentParser()
    p.add_argument('target', type=Path)
    args = p.parse_args()
    run(args.target)


if __name__ == '__main__':
    main()
