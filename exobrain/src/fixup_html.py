from pathlib import Path
import re
from typing import Iterator


from bs4 import BeautifulSoup
import orgparse


TS_RE = orgparse.date.TIMESTAMP_RE


def fixup(soup: BeautifulSoup) -> None:
    for ts_span in soup.select('.timestamp'):
        ## org mode sometimes exports timestamps with extra space at the end for some reason
        ts_str = ts_span.string
        m = TS_RE.match(ts_str)
        assert m is not None, repr(ts_str)
        remaining = ts_str[m.end():]
        assert re.fullmatch(r'\s*', remaining), (remaining, ts_str)
        ts_span.string = ts_str[:m.end()]
        ##

        ## org mode emits extra timestamp-wrapper element for no reason?
        ts_parent = ts_span.parent
        assert ts_parent.attrs['class'] == ['timestamp-wrapper'], (ts_str, ts_parent)
        ts_parent.replace_with(ts_span)
        ##
