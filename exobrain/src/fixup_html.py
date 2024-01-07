from pathlib import Path
import re
from typing import Iterator


from bs4 import BeautifulSoup
import orgparse


TS_RE = orgparse.date.TIMESTAMP_RE


def fixup(soup: BeautifulSoup) -> None:
    ## org mode sometimes exports timestamps with extra space at the end for some reason
    for ts_span in soup.select('.timestamp'):
        ts_str = ts_span.string
        m = TS_RE.match(ts_str)
        assert m is not None, repr(ts_str)
        remaining = ts_str[m.end():]
        assert re.fullmatch(r'\s*', remaining), (remaining, ts_str)
        ts_span.string = ts_str[:m.end()]
    ##
