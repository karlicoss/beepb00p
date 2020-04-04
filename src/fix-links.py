#!/usr/bin/env python3
from pathlib import Path
import re


def test():
    text = '''
* TODO [#C] [2019-08-18 Sun 20:41] How to add full text search to your website - Dev Channel - Medium :search:exobrain:
 https://medium.com/dev-channel/how-to-add-full-text-search-to-your-website-4e9c80ce2bf4 misc
* TODO [#C] ok, ox-hugo generates some odd artifacts, perhaps would be easier with vanilla md export :blog:exobrain:
:PROPERTIES:
:CREATED: [2019-12-22 Sun 19:03]
:END:

    '''
    res = process(text)

    exp = '''
* TODO [#C] [2019-08-18 Sun 20:41] [[https://medium.com/dev-channel/how-to-add-full-text-search-to-your-website-4e9c80ce2bf4][How to add full text search to your website - Dev Channel - Medium]] :search:exobrain:
  misc
* TODO [#C] ok, ox-hugo generates some odd artifacts, perhaps would be easier with vanilla md export :blog:exobrain:
:PROPERTIES:
:CREATED: [2019-12-22 Sun 19:03]
:END:

    '''

    assert res == exp


def process(text: str) -> str:
    # breakpoint()
    # TODO start with entries that contain date only?
    replacements = []
    pos = 0
    while True:
        # TODO go to the next line??
        hre = r'] (?P<heading>[^\]]*?)\s*(:(\w+:)+)?\s*'
        lre = r'\w*(?P<url>http[^\s]+)'
        m = re.search(hre + lre, text[pos:], re.MULTILINE)
        if m is None:
            break
        # gd = m.groupdict()
        h, hs, he = m.group('heading'), m.start('heading'), m.end('heading')
        u, us, ue = m.group('url')    , m.start('url')    , m.end('url')

        replacements.append((pos + hs, pos + he, f'[[{u}][{h}]]'))
        replacements.append((pos + us, pos + ue, ''))
        pos += m.end()
    delta = 0
    for s, e, r in replacements:
        # TODO ugh. a bit horrible..
        text = text[:delta + s] + r + text[delta + e:]
        delta += len(r) - (e - s)
    return text


def main():
    test()
   
    import argparse
    p = argparse.ArgumentParser()
    # p.add_argument('--dry')
    p.add_argument('file', type=Path)
    args = p.parse_args()

    path = args.file
    text = path.read_text()
    res = process(text)
    print(res)


if __name__ == '__main__':
    main()
