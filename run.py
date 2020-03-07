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

def main():
    # todo ugh, need symlink watching tool here again...
    cachedir = Path('~/.org-timestamps').expanduser()
    # TODO not sure about removing all of it...
    for c in cachedir.glob('*.cache'):
        c.unlink()

    # TODO probably need my user config; similarly to blog
    check_call('emacs --batch -q -l publish.el -f org-publish-all'.split())

    # mdbook doesn't like summary format so we fix it
    check_call(r"awk -i inplace !/\[README\]/  markdown/SUMMARY.md".split())
    # TODO clean first?
    check_call(['mdbook', 'build'])


if __name__ == '__main__':
    main()
