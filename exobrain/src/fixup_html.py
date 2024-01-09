import re


import bs4  # type: ignore[import]
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
            #

            a = soup.new_tag('a')
            a.attrs['class'] = 'headerlink'
            a.attrs['href'] = f'#{n_id}'
            a.string = '¶'
            n.insert(0, a)
    ##

    ## wrap properties into some extra classes
    ## default format for properties is pretty crap, just a text dump in .properties block
    for prop_block in soup.select('.properties'):
        plines = [l.strip() for l in prop_block.string.splitlines() if len(l.strip()) != 0]
        prop_block.string = ''
        for pline in plines:
            pline = pline.strip()
            if len(pline) == 0:
                continue
            (k, v) = pline.split(':')
            v = v.strip()
            prop_tag = soup.new_tag('div', attrs={
                'class': 'property',
                'data-property-name': k,
            })
            pname_tag = soup.new_tag('span', attrs={
                'class': 'property-name',
            })
            pname_tag.string = k
            pvalue_tag = soup.new_tag('span', attrs={
                'class': 'property-value',
            })
            pvalue_tag.string = f' {v}'  # FIXME space is just for backwards compat
            prop_tag.append(pname_tag)
            prop_tag.append(': ')
            prop_tag.append(pvalue_tag)

            prop_block.append(prop_tag)
    ##

    ## sort out tags
    for tag_block in soup.select('.tag'):
        for t in tag_block.children:
            if t == '\xa0':  # nbsp
                t.extract()
        inherited = []
        for t in tag_block.find_all('span'):
            # TODO reorder inherited and self tags?
            if len(t.attrs['class']) != 1:
                continue  # TODO?
            [tag] = t.attrs['class']
            inh = 'INHERITED_'
            if inh in tag:
                t.attrs['class'] = [tag.replace(inh, ''), 'tag-inherited']
                t.string = t.string.replace(inh, '')  # todo removeprefix?
                t.extract()
                inherited.append(t)
            else:
                t.attrs['class'] = [tag, 'tag-self']
        for t in reversed(inherited):
            tag_block.insert(0, t)
    if (filetags_block := soup.select_one('.filetags')) is not None:
        filetags = (filetags_block.string or '').split()
        filetags_block.string = ''
        tag_wrapper = soup.new_tag('span', attrs={
            'class': 'tag',
        })
        filetags_block.append(tag_wrapper)
        for filetag in filetags:
            tag = soup.new_tag('span', attrs={
                'class': f'{filetag} tag-self',
            })
            tag.string = filetag
            tag_wrapper.append(tag)
    ##
