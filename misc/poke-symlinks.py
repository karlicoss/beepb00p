#!/usr/bin/env python3
# Hakyll watching doesn't follow symlinks :( https://github.com/jaspervdj/hakyll/issues/502
# This scripts compensates for it by setting up watches on all symlinks and updating them (touches) every time the referrent file changes
# Usage: run in parallel with site watch command (see ~preview~ script)

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

    print("[poke-symlinks] tracking:")
    pprint(symlinks)

    def add_watch(s: Path):
        try:
            res = s.resolve()
        except Exception as e:
            print('[poke-symlinks] ERROR ', e)
            return
        path2link[res] = s
        ps = str(res)

        try:
            i.remove_watch(ps, superficial=True)
        except:
            # ugh.. have to ignore failures from this call coming from the kernel watches
            # see https://github.com/dsoprea/PyInotify/pull/57
            pass

        mask = ics.IN_MODIFY | ics.IN_ATTRIB | ics.IN_IGNORED
        i.add_watch(ps, mask=mask)

    for s in symlinks:
        add_watch(s)

    def bump_mtime(path: Path, events=None):
        symlink = path2link[path]
        print("[poke-symlinks] detected change: ", path, symlink, events)
        check_output(["touch", "-h", str(symlink)])

    for event in i.event_gen(yield_nones=False):
        (_, tp, path, filename) = event
        path = Path(path)
        if 'IN_IGNORED' in tp:
            # some sofware like vim would try to achieve atomic writes by deleting amd moving, which would expire inotify handle..
            symlink = path2link[path]
            add_watch(symlink)
        try:
            bump_mtime(path, events=tp)
        except Exception as e:
            print("[poke-symlinks] ERROR ", e)


def main():
    import argparse
    p = argparse.ArgumentParser()
    p.add_argument('target', type=Path)
    args = p.parse_args()
    run(args.target)


if __name__ == '__main__':
    main()
