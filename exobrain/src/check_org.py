from __future__ import annotations

from dataclasses import dataclass
import os
from pathlib import Path
import re
from typing import Iterator, Optional


import orgparse


@dataclass
class Config:
    f_checks: list[str]  # exact matches
    word_checks: list[str]
    tag_checks: set[str]


def get_config() -> Config:
    if 'CI' in os.environ:
        return Config(f_checks=[], word_checks=[], tag_checks=set())
    else:
        from checks import F_CHECKS, WORD_CHECKS, TAG_CHECKS

        return Config(f_checks=F_CHECKS, word_checks=WORD_CHECKS, tag_checks=TAG_CHECKS)


def check_one(path: Path, *, cfg: Optional[Config] = None) -> Iterator[Exception]:
    if cfg is None:
        cfg = get_config()

    text = path.read_text()

    ## find exact matches
    if len(cfg.f_checks) > 0:
        rgx = '|'.join(re.escape(x) for x in cfg.f_checks)
        m = re.search(rgx, text)
        if m is not None:
            yield RuntimeError('found occurence', rgx, m.group())
    ##

    ## find 'words'
    if len(cfg.word_checks) > 0:
        rgx = r'\b(' + '|'.join(re.escape(x) for x in cfg.word_checks) + r')\b'
        m = re.search(rgx, text)
        if m is not None:
            yield RuntimeError('found occurence', rgx, m.group())
    ##

    ### detect timestamps with time
    ts = orgparse.date.TIMESTAMP_RE
    allowed = {
        'inactive_year',
        'inactive_month',
        'inactive_day',
    }
    pos = 0
    while pos < len(text):
        m = ts.search(text, pos=pos)
        if m is None:
            break
        d = {
            k: v
            for k, v in m.groupdict().items()
            if v is not None and k not in allowed
        }
        if len(d) != 0:
            yield RuntimeError(d, m.group())
        pos = m.start() + 1
    ###

    ### find forbidden tags
    o = orgparse.loads(text)
    for n in o:
        found = n.tags.intersection(cfg.tag_checks)
        if len(found) > 0:
            yield RuntimeError(path, n.heading, found)
    ###


def test_checks(tmp_path: Path) -> None:
    cfg = Config(
        f_checks=[
            'web.telegram.org',
            'mail.google.com',
        ],
        word_checks=['some', 'noth'],
        tag_checks=set(),
    )

    def do(text: str) -> list[Exception]:
        p = tmp_path / 'file.org'
        p.write_text(text)
        return list(check_one(p, cfg=cfg))

    assert len(do('''
* nothing
** [2019-11-02 Sat] wrong
with

* this file :sometag:
''')) == 0

    assert len(do('''
somethings
whoopsweb.telegram.org/accidental link
more text
''')) == 1

    assert len(do('''
somethings
oh
mail.google.com/somelink
more text
''')) == 1

    assert len(do('''
* bad link in body [2019-10-17 Thu]
alala mail.google.com/whatever
* but also bad timestamp
    [2019-10-18 Fri 02:06]
''')) == 2

    assert len(do('''
* I am ok
* I contain some forbidden words
''')) == 1

    assert len(do('''
* I end with forbidden noth
* but I am ok
''')) == 1
