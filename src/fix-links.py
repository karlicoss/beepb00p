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

* xxx
** TODO [#B] [2019-09-02 Mon 12:41] [[https://reddit.com/r/scifi/comments/cvy78o/searching_stories_with_super_intelligence_in/eyldzyb/][Searching stories with super intelligence in humans theme]] /r/scifi :read:
  Love and recommend "Brainchild" just for this. Explores multiple different angles of what we call intelligence.

  https://www.amazon.com/Brain-Child-Novel-George-Turner/dp/0688105955
    '''
    res = process(text)

    exp = '''
* TODO [#C] [2019-08-18 Sun 20:41] [[https://medium.com/dev-channel/how-to-add-full-text-search-to-your-website-4e9c80ce2bf4][How to add full text search to your website - Dev Channel - Medium]] :search:exobrain:
  misc
* TODO [#C] ok, ox-hugo generates some odd artifacts, perhaps would be easier with vanilla md export :blog:exobrain:
:PROPERTIES:
:CREATED: [2019-12-22 Sun 19:03]
:END:

* xxx
** TODO [#B] [2019-09-02 Mon 12:41] [[https://reddit.com/r/scifi/comments/cvy78o/searching_stories_with_super_intelligence_in/eyldzyb/][Searching stories with super intelligence in humans theme]] /r/scifi :read:
  Love and recommend "Brainchild" just for this. Explores multiple different angles of what we call intelligence.

  https://www.amazon.com/Brain-Child-Novel-George-Turner/dp/0688105955
    '''
    # print(repr(exp))
    # print(repr(res))

    # for x, y in zip(exp, res):
    #     if x != y:
    #         print(x, y)
    assert res == exp


import orgparse
tre = orgparse.date.gene_timestamp_regex('inactive')

def process(text: str) -> str:
    replacements = []
    pos = 0
    while True:
        # eh. will handle rest of them later somehow..
        hre = tre + r'\s(?P<heading>[^\]]*?)\s*(:(\w+:)+)?\s*\n'
        lre = r'\s*(?P<url>http[^\s]+)'
        rrr = re.compile(hre + lre, re.VERBOSE)
        hm = rrr.search(text[pos:])  #, re.MULTILINE)
        if hm is None:
            break

        um = hm

        h, hs, he = hm.group('heading'), hm.start('heading'), hm.end('heading')
        u, us, ue = um.group('url')    , um.start('url')    , um.end('url')

        replacements.append((pos + hs, pos + he, f'[[{u}][{h}]]'))
        replacements.append((pos + us, pos + ue, ''))
        pos += hm.end()

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
    import tempfile

    with tempfile.TemporaryDirectory() as td:
        tdir = Path(td)
        patched = tdir / 'res.org'
        patched.write_text(res)
        from subprocess import run, check_call
        run(['git', 'diff', path, patched])

        from kython.tui import yesno_or_fail
        yesno_or_fail('patch?')

        import shutil
        shutil.copy(patched, path)



if __name__ == '__main__':
    main()
