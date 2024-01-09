import re

import bs4  # type: ignore[import]
import orgparse


TS_RE = orgparse.date.TIMESTAMP_RE


def fixup(soup: bs4.BeautifulSoup) -> None:
    for ts_span in soup.find_all('span', class_='timestamp'):
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
    for n in soup.find_all('div'):
        clss = n.attrs.get('class')
        if clss is None or len(clss) != 1:
            continue
        [cls] = clss
        if cls.startswith('outline-text-'):
            del n.attrs['id']
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
    for n in soup.find_all(re.compile(r'^h\d$')):
        if 'id' not in n.attrs:
            continue
        n_id = n.attrs['id']

        lvl = int(n.name[1:])

        # parent should be outline?
        par = n.parent
        assert par.attrs['class'] == [f'outline-{lvl}'], (n, par)
        #

        a = soup.new_tag('a')
        a.attrs['class'] = 'headerlink'
        a.attrs['href'] = f'#{n_id}'
        a.string = '¶'
        n.insert(0, a)
    ##

    ## wrap properties into some extra classes
    ## default format for properties is pretty crap, just a text dump in .properties block
    for prop_block in soup.find_all('div', class_='properties'):
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
    for tag_block in soup.find_all('span', class_='tag'):
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
