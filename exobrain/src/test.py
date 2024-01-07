from pathlib import Path
from subprocess import run, check_call, check_output, Popen, PIPE, CalledProcessError
from time import sleep
from typing import NamedTuple, List
from shutil import copy, copytree

import pytest


from utils import tmp_popen


def build(*args):
    return ['src/build.py', *args]


TESTDATA = Path(__file__).absolute().parent.parent / 'testdata'
assert TESTDATA.exists(), TESTDATA

INPUT = TESTDATA


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


@pytest.mark.parametrize('use_new_org_export' , [True, False], ids=['org_new' , 'org_old'])
@pytest.mark.parametrize('use_new_html_export', [True, False], ids=['html_new', 'html_old'])
def test_test(use_new_org_export: bool, use_new_html_export: bool, tmp_data: Path, tmp_path: Path) -> None:
    d = tmp_data
    i      = d / 'input'
    public = d / 'public'
    html   = d / 'html'

    copy(INPUT / 'test.org', i / 'test.org')
    oargs = ['--use-new-org-export'] if use_new_org_export else []
    hargs = ['--use-new-html-export'] if use_new_html_export else []
    check_call(build('--data-dir', d, *oargs, *hargs))

    test_org_public = (public / 'test.org').read_text()

    for x in [
        '#+title: Test',
        'This is a test file',
        ':CREATED:  [2020-11-29]',   # should chop off the timestamps
        ':ID:       smtds',  # autogenerated id  # TODO test preserving original ids?
        'pinboard:tag:whatever',
        '- [2019-11-02] http://stevelosh.com/blog/2018/08/a-road-to-common-lisp',
    ]:
        assert x in test_org_public, x

    # NOTE: kinda annoying, but sitemap is dumped into the _source_ dir during html export
    assert '[[file:test.org][Test]]' in (public / 'sitemap.org').read_text()

    test_html = (html / 'test.html').read_text()
    assert 'h2 id="slbstrsslsxfbrdcmmnlsprdtcmmnlsplbstrs">' in test_html
    assert '<a href="#nil">. ' in test_html  # TODO later, implement proper ids here, asserting for now so we don't forget to test

    # TODO they are hidden in css for some reason?
    assert '<div class="filetags"><span class="tag"><span class="tag1 tag-self">tag1</span><span class="tag2 tag-self">tag2</span></span></div>' in test_html

    # shouldn't have whitespace in the timestamp
    assert '<span class="timestamp">[2019-10-18]</span>' in test_html


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

    check_call(build('--data-dir', d))

    _check_org(public / 'projects/cachew.org')
    # _check_org(public / 'memex.org')


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
    def diff() -> List[str]:
        r = run(['diff', '-brq', old / 'public', public], stdout=PIPE)
        out = r.stdout
        assert out is not None
        l1 = out.decode('utf8').strip().splitlines()

        r = run(['diff', '-brq', old / 'html'  , html  ], stdout=PIPE)
        out = r.stdout
        assert out is not None
        l2 = out.decode('utf8').strip().splitlines()

        return l1 + l2
    # NOTE: if we don't clean properly, documents.js ends with entries from TOC.. ugh

    check_call(build('--data-dir', d))
    assert diff() == []
    ##

    with tmp_popen(build('--data-dir', d, '--watch')) as popen:
        # TODO how much should we sleep?
        sleep(3)
        assert diff() == []

        memex.touch()
        sleep(3)
        assert diff() == []

        # just a santiy check
        with memex.open('a') as fo:
            fo.write('x')
        sleep(3)
        dd = diff()
        [d1, d2, d3] = dd
        assert 'memex.org'    in d1
        assert 'documents.js' in d2  # hmm
        assert 'memex.html'   in d3
