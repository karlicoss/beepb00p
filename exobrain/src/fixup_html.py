from pathlib import Path
import re
from typing import Iterator


import bs4
import orgparse


TS_RE = orgparse.date.TIMESTAMP_RE


def fixup(soup: bs4.BeautifulSoup) -> None:
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

    ## these ids are really unnecessary, just littering the anchors
    for i in range(10):
        cls = f'outline-text-{i}'
        for outline_text in soup.select(f'.{cls}'):
            del outline_text.attrs['id']
    ##


    ## reorder table of contents
    ## pretty annoying that it shows up at the very top before the document preamble
    if (toc := soup.find(id='table-of-contents')) is not None:
        for n in toc.next_siblings:
            if not isinstance(n, bs4.element.Tag):
                continue
            if n.attrs.get('class') == ['outline-2']:  # FIXME support any kind of outline?
                toc.extract()
                n.insert_before(toc)
                break
        else:
            raise RuntimeError("hmm didn't move TOC??")
    ##


    ## add headerlink anchor
    for i in range(10):
        for n in soup.select(f'h{i}'):
            if 'id' not in n.attrs:
                continue
            n_id = n.attrs['id']

            # parent should be outline?
            par = n.parent
            assert par.attrs['class'] == [f'outline-{i}'], (n, par)
            assert 'id' not in par.attrs, (n, par)  # just in case?
            par.attrs['id'] = f'outline-container-{n_id}'
            #

            a = soup.new_tag('a')
            a.attrs['class'] = 'headerlink'
            a.attrs['href'] = f'#{n_id}'
            a.string = '¶'
            n.insert(0, a)
    ##
