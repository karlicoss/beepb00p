from pathlib import Path
import re
from typing import Iterator

import orgparse
from orgparse.node import OrgRootNode, OrgEnv


env = OrgEnv(
    todos=['TODO', 'STRT', 'WAIT', 'START', 'NEXT'],
    dones=['DONE', 'CANCEL', 'CNCL'],
    filename='<string>',
)


def _fixup(org: str, *, add_ids: bool) -> Iterator[str]:
    assert '' not in org  # orgparse has a bug where is splits it in the body?

    # assert ' ' not in org  # weird character, exclude it later

    split = org.splitlines()
    for n in orgparse.loads(org, env=env):
        body = n.get_body(format='raw')
        if isinstance(n, OrgRootNode):
            yield body
            continue

        # TODO hmm, n.tags returns Set
        # could be kinda annoying, since it changes tags order..
        tags = list(n._tags)
        raw_tags = ':' + ':'.join(tags) + ':'
        tags = [t for t in tags if t not in {
            'refile',
            'gr', 'TODO', 'graspw', 'protocol',  # TODO phase these out from inputs gradually
        }]

        rheading = n.get_heading(format='raw')

        hparts = []
        hparts.append('*' * n.level)
        if (todo := n.todo) is not None:
            hparts.append(todo)
        if (priority := n.priority) is not None:
            hparts.append(f'[#{priority}]')
        hparts.append(rheading)

        if len(tags) != 0:
            lineno = n.linenumber - 1  # ugh, orgparse counts line numbers from 1??
            raw_line = split[lineno]
            m = re.search(r'(\s+)' + raw_tags, raw_line)
            assert m is not None
            dist = len(m.group(1))
            dist -= 1  # space will be added later
            tags_s = ':' + ':'.join(tags) + ':'
            hparts.append(' ' * dist + tags_s)
            heading_so_far = len(' '.join(hparts))
            if heading_so_far < 77:
                # FIXME compat for org-tags-column... maybe remove it later
                hparts[-1] = ' ' * (77 - heading_so_far) + hparts[-1]

        heading = ' '.join(hparts)

        properties = dict(n.properties)
        # fuck. default org-mode ids are non-deterministic (and even change inbetween emacs invocations)
        # https://github.com/alphapapa/unpackaged.el#export-to-html-with-useful-anchors looks really good
        # it worked, but then I moved some code around and it stopped for some reason...
        # too exhausted to debug it, so will use it late
        # sometimes I fucking hate emacs.
        # md5: nice that it has fixed length, but not very reversible
        # base64: might be arbirary long?
        if add_ids:
            if 'ID' not in properties and 'CUSTOM_ID' not in properties:
                rgx = '|'.join((
                    # TODO different languages??
                    ' (mon|tue|wed|thu|fri|sat|sun) ',  # remove days of week
                    'http',  # FIXME hmm looks like old export didn't handle https properly
                    r'[^a-z]',  # remove all non-alnums
                    '[aeoiu]',  # remove all vowels (for brevity)
                ))
                gen_id = re.sub(rgx, '', rheading.lower())
                if len(gen_id) > 50:
                    # if it's too long, only keep part of head and tail
                    gen_id = gen_id[:25] + gen_id[-25:]
                if len(gen_id) > 0:
                    properties['ID'] = gen_id

        parts = [heading]
        if len(properties) > 0 or add_ids:  # TODO later remove add_ids
            parts.append(':PROPERTIES:')
            for k, v in properties.items():
                kk = f':{k}:'
                kk = f'{kk:<10}'  # TODO remove later? this was just for compat with old elisp code
                parts.append(f'{kk} {v}')
            parts.append(':END:')
        if len(body) > 0:
            parts.append(body)

        yield '\n'.join(parts)


def fixup(org: str, *, add_ids: bool = True) -> str:
    # TODO adding extra \n for compat with older elisp code
    return '\n'.join(_fixup(org, add_ids=add_ids)) + '\n'


def test_basic(tmp_path: Path) -> None:
    org = \
'''
#+title: whatever
* simple heading
* TODO [#B] [2019-09-02 Mon 12:41] [[https://reddit.com/][some heading]] fewf :tag:
** heading with properties
:PROPERTIES:
:CREATED:  [2020-06-25 Thu 09:38]
:END:
* heading with a newline before body

: body
* STRT [#C] another heading
'''.rstrip()
# TODO this doen't work atm -- orgparse strips out empty body? should probably fix in orgparse..
# * heading with a newline after
#
# * other heading

    res = fixup(org, add_ids=False)
    assert res == org + '\n'


def test_tags(tmp_path: Path) -> None:
    """
    should remove some tags (e.g. "refile")
    """
    org = \
'''
#+title: whatever
* heading1
** heading11 :tag:
   some stuff
** TODO [#B] [2019-09-02 Mon 12:41] [[https://reddit.com/][some heading]] fewf :read:
** heading 12    :tag1:refile:
* heading 2    :refile:
'''.rstrip()
    res = fixup(org, add_ids=False)
    assert res == \
'''
#+title: whatever
* heading1
** heading11                                                            :tag:
   some stuff
** TODO [#B] [2019-09-02 Mon 12:41] [[https://reddit.com/][some heading]] fewf :read:
** heading 12                                                          :tag1:
* heading 2
'''


def test_add_id(tmp_path: Path) -> None:
    """
    should generate an ID if note doesn't have one
    """
    org = \
'''
* TODO a Heading with example
:PROPERTIES:
:CREATED: [2020-11-29 Sun 23:51]
:END:
* heading with custom id
:PROPERTIES:
:CUSTOM_ID: some_custom_id
:END:
* STRT [2024-01-06 Sat 10:11] head_ing 2
* heading with id
:PROPERTIES:
:ID: whatever
:CREATED: [2020-11-29 Sun 23:51]
:END:
* [[https://reddit.com/][reddit]] heading with a link and 🧘‍♂️🗺 emoji
* [[https://news.ycombinator.com/item?id=25161117][Show HN: I made an alternative to Google Alerts that listens to social media]]
* TODO [#D] [2019-07-09] [[https://twitter.com/nplusodin/status/1148645120616607745][Tweet from @nplusodin: Ученые показали, что кусок стекла с правильно размещенными внутри неоднородностями может производить «вычисления» и распознавать рукописные цифры]] :computation:
'''.rstrip()

    res = fixup(org)
    assert res == \
'''
* TODO a Heading with example
:PROPERTIES:
:CREATED:  [2020-11-29 Sun 23:51]
:ID:       hdngwthxmpl
:END:
* heading with custom id
:PROPERTIES:
:CUSTOM_ID: some_custom_id
:END:
* STRT [2024-01-06 Sat 10:11] head_ing 2
:PROPERTIES:
:ID:       hdng
:END:
* heading with id
:PROPERTIES:
:ID:       whatever
:CREATED:  [2020-11-29 Sun 23:51]
:END:
* [[https://reddit.com/][reddit]] heading with a link and 🧘‍♂️🗺 emoji
:PROPERTIES:
:ID:       srddtcmrddthdngwthlnkndmj
:END:
* [[https://news.ycombinator.com/item?id=25161117][Show HN: I made an alternative to Google Alerts that listens to social media]]
:PROPERTIES:
:ID:       snwsycmbntrcmtmdshwhnmdnlntvtggllrtsthtlstnstsclmd
:END:
* TODO [#D] [2019-07-09] [[https://twitter.com/nplusodin/status/1148645120616607745][Tweet from @nplusodin: Ученые показали, что кусок стекла с правильно размещенными внутри неоднородностями может производить «вычисления» и распознавать рукописные цифры]] :computation:
:PROPERTIES:
:ID:       stwttrcmnplsdnsttstwtfrmnplsdn
:END:
'''

# TODO oof
#  : The Quark and the Jaguar
#  ** [#C] [2020-02-07] *The Network Revolution – confessions of a computer scientist* (1982)¹ is the ti... | Hacker News
#  :PROPERTIES:
# -:ID:       thntwrkrvltncnfssnsfcmptrscntstsththckrnws
# +:ID:       thntwrkrvltncnfssnsfcmptrscntst¹sththckrnws
