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

** TODO [#C] xxx :me:blog:exobrain:
:PROPERTIES:
:CREATED: [2018-11-23 Fri 09:29]
:END:
:LOGBOOK:
- State "DONE"       from "TODO"       [2018-11-24 Sat 15:50]
:END:
https://github.com/karlicoss/my-awesome-list

* TODO [#B] [2020-02-14 Fri 09:25] Building a Second Brain in Roam...And Why You Might Want To : RoamResearch :blog:exobrain:
:PROPERTIES:
:ID:       600e2919-7ba5-4dbe-85d5-4e4c642b3cc1
:END:
https://www.reddit.com/r/RoamResearch/comments/eho7de/building_a_second_brain_in_roamand_why_you_might

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

** TODO [#C] xxx :me:blog:exobrain:
:PROPERTIES:
:CREATED: [2018-11-23 Fri 09:29]
:END:
:LOGBOOK:
- State "DONE"       from "TODO"       [2018-11-24 Sat 15:50]
:END:
https://github.com/karlicoss/my-awesome-list

* TODO [#B] [2020-02-14 Fri 09:25] Building a Second Brain in Roam...And Why You Might Want To : RoamResearch :blog:exobrain:
:PROPERTIES:
:ID:       600e2919-7ba5-4dbe-85d5-4e4c642b3cc1
:END:
https://www.reddit.com/r/RoamResearch/comments/eho7de/building_a_second_brain_in_roamand_why_you_might

    '''
    print(res)
    print(repr(exp))
    print(repr(res))

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
        # VERBOSE because of orgparse regex
        # whitespace in character class because of VERBOSE
        hre = r'\*.*' + tre + r'[ ](?P<heading>[^\]\n]*?)[ ]*(:(\w+:)+)?[ ]*\n'
        lre = r'\s*(?P<url>http[^\s]+)'
        rrr = re.compile(hre + lre, re.VERBOSE)
        hm = rrr.search(text[pos:])  #, re.MULTILINE)
        if hm is None:
            break

        um = hm
        print(um.groupdict())

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
        if yesno_or_fail('patch?'):
            import shutil
            shutil.copy(patched, path)



if __name__ == '__main__':
    main()
