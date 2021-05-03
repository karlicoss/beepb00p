from pathlib import Path
from subprocess import check_call, check_output, Popen, PIPE
from typing import NamedTuple
from shutil import copy, copytree

import pytest


def build(*args):
    return ['src/build.py', *args]


INPUT = Path('data/input')


@pytest.fixture
def tmp_data(tmp_path: Path):
    td = tmp_path / 'data'
    td.mkdir()

    # FIXME what's up with input dir??
    (td / 'md'    ).mkdir()
    (td / 'html'  ).mkdir()
    (td / 'public').mkdir()
    (td / 'input' ).mkdir()
    yield td


def test_build_empty(tmp_data: Path) -> None:
    check_call(build('--data-dir', tmp_data))


def _check_org(path: Path) -> None:
    ids = [l for l in path.read_text().splitlines() if ':ID' in l]
    assert len(ids) > 10


def test_build_some(tmp_data: Path, tmp_path: Path) -> None:
    d = tmp_data
    i      = d / 'input'
    public = d / 'public'
    html   = d / 'html'

    memex = i / 'memex.org'

    p = i / 'projects'
    p.mkdir()
    cachew = p / 'cachew.org'

    copy(INPUT / 'memex.org'          , memex)
    copy(INPUT / 'projects/cachew.org', cachew)
    # TODO add another one?

    with Popen(build('--data-dir', d)) as popen:
        pass

    _check_org(public / 'projects/cachew.org')
    _check_org(public / 'memex.org')


    sitemap = public / 'sitemap.org'
    # TODO check ordering things marked with emojis?
    assert 'file:memex.org'           in sitemap.read_text()
    assert 'file:projects/cachew.org' in sitemap.read_text()


    hc = (html / 'projects/cachew.html').read_text()
    assert 'exobrain-settings' in hc
    assert '"../memex.html"' in hc
    hm = (html / 'memex.html').read_text()
    assert '"projects/cachew.html"' in hm

    ## test idempotence
    old = tmp_path / 'old'
    copytree(public, old / 'public')
    copytree(html  , old / 'html'  )

    with Popen(build('--data-dir', d)) as popen:
        pass

    check_call(['diff', '-bur', old / 'public', public])
    # NOTE: if we don't clean properly, documents.js ends with entries from TOC.. ugh
    check_call(['diff', '-bur', old / 'html'  , html  ])
    ##



# def test_watch(tmp_data: Path) -> None:
#     # TODO ugh. probably won't be possible to kjjkk 1k
#     pass

# TODO test idempotence?
