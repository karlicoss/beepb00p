#!/usr/bin/env python3
from pathlib import Path
from shutil import rmtree
from subprocess import check_call

# TODO sanity check that there are noexport entries or some private tags (e.g. people mentioned)
# ./check

# cargo install mdbook && mdbook init

# TODO https://rust-lang.github.io/mdBook/format/config.html

# https://github.com/rust-lang/mdBook/blob/master/book-example/book.toml
# TODO editable?
# TODO huh, that actually would be nice. I could fix stuff in-place and then apply to org-mode
# TODO search settings?
# TODO wonder if I can integrate it with blog's header?
# TODO export txt files as md as well?

from compile_org import emacs


root_dir = Path(__file__).absolute().parent.parent
source_dir       = root_dir / 'content'
intermediate_dir = root_dir / 'intermediate'
output_dir       = root_dir / 'markdown'  # TODO FIXME


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

    # TODO FIXME it might be under vcs??
    clean_dir(intermediate_dir)
    clean_dir(output_dir)


def main():
    clean()

    emacs(
        '--eval', f'''(progn
            (setq exobrain/intermediate-dir "{intermediate_dir}")
            (setq exobrain/source-dir       "{source_dir}")
            (setq exobrain/output-dir       "{output_dir}")
        )''',
        '--load', root_dir / 'subprocess.el', # TODO
        '--load', root_dir / 'src/publish.el',
        '-f', 'org-publish-all',
    )

    # TODO use output_dir
    # mdbook doesn't like summary format so we fix it
    check_call(r"awk -i inplace !/\[README\]/  markdown/SUMMARY.md".split())
    # TODO clean first?
    check_call(['mdbook', 'build'])

    from check import check_org
    check_org(intermediate_dir)


if __name__ == '__main__':
    main()
