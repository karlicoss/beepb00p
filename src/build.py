#!/usr/bin/env python3
from pathlib import Path
import sys
from shutil import rmtree
from subprocess import check_call

# TODO sanity check that there are noexport entries or some private tags (e.g. people mentioned)
# ./check

# cargo install mdbook && mdbook init


def ccall(*args, **kwargs):
    print(args, file=sys.stderr)
    return check_call(*args, **kwargs)

# TODO https://rust-lang.github.io/mdBook/format/config.html

# https://github.com/rust-lang/mdBook/blob/master/book-example/book.toml
# TODO editable?
# TODO huh, that actually would be nice. I could fix stuff in-place and then apply to org-mode
# TODO search settings?
# TODO wonder if I can integrate it with blog's header?
# TODO export txt files as md as well?

from compile_org import emacs


root_dir = Path(__file__).absolute().parent.parent
input_dir  = root_dir / 'input'
public_dir = root_dir / 'public'
output_dir = root_dir / 'markdown'  # TODO FIXME


def clean_dir(path: Path):
    assert path.is_dir(), path
    for x in path.iterdir():
        if x.is_file():
            x.unlink()
        else: # dir
            rmtree(x)


def clean():
    # todo ugh, need symlink watching tool here again...
    cachedir = Path('~/.org-timestamps').expanduser()
    # TODO not sure about removing all of it...
    for c in cachedir.glob('*.cache'):
        c.unlink()

    clean_dir(output_dir)

    # TODO what about empty dirs?
    for f in public_dir.glob('**/*.org'):
        f.unlink()


def main():
    import argparse
    p = argparse.ArgumentParser()
    p.add_argument('--no-add', action='store_true')
    args = p.parse_args()

    clean()

    emacs(
        '--eval', f'''(progn
            (setq exobrain/input-dir  "{input_dir}")
            (setq exobrain/public-dir "{public_dir}")
            (setq exobrain/output-dir "{output_dir}")
        )''',
        '--load', root_dir / 'subprocess.el', # TODO
        '--load', root_dir / 'src/publish.el',
        '-f', 'org-publish-all',
    )

    from check import check_org
    check_org(public_dir)

    # TODO think about commit/push/deploy logic?
    ccall(['git', 'status'], cwd=public_dir)

    iadd = not args.no_add
    if iadd:
        ccall(['git', 'add', '-A', '--intent-to-add'], cwd=public_dir)
        ccall(['git', 'add', '-p'], cwd=public_dir)

        # TODO suggest to commit/push?

    # TODO use output_dir
    # mdbook doesn't like summary format so we fix it
    # TODO reorder index?
    ccall(r"awk -i inplace !/\[README\]/  markdown/SUMMARY.md".split())
    # TODO clean first?
    ccall(['mdbook', 'build'])


if __name__ == '__main__':
    # TODO allow skipping?
    check_call(['mypy', '--check-untyped', __file__])
    main()
