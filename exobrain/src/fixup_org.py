from pathlib import Path
from typing import Iterator

import orgparse
from orgparse.node import OrgRootNode


def _fixup(org: str) -> Iterator[str]:
    for n in orgparse.loads(org):
        body = n.get_body(format='raw')
        if isinstance(n, OrgRootNode):
            yield body
            continue

        # TODO hmm, n.tags returns Set
        # could be kinda annoying, since it changes tags order..
        tags = list(n._tags)
        tags = [t for t in tags if t != 'refile']

        rheading = n.get_heading(format='raw')

        hparts = []
        hparts.append('*' * n.level)
        if (todo := n.todo) is not None:
            hparts.append(todo)
        if (priority := n.priority) is not None:
            hparts.append(f'[#{priority}]')
        hparts.append(rheading)

        if len(tags) != 0:
            hparts.append(':' + ':'.join(tags) + ':')

        heading = ' '.join(hparts)

        parts = [heading]
        if len(properties := n.properties) > 0:
            parts.append(':PROPERTIES:')
            for k, v in properties.items():
                parts.append(f':{k}: {v}')
            parts.append(':END:')
        if len(body) > 0:
            parts.append(body)

        yield '\n'.join(parts)


def fixup(org: str) -> str:
    return '\n'.join(_fixup(org))


def test_basic(tmp_path: Path) -> None:
    org = \
'''
#+title: whatever
* simple heading
* TODO [#B] [2019-09-02 Mon 12:41] [[https://reddit.com/][some heading]] fewf :tag:
** heading with properties
:PROPERTIES:
:CREATED: [2020-06-25 Thu 09:38]
:END:
'''.rstrip()
    res = fixup(org)
    assert res == org


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
** heading 12 :tag1:refile:
* heading 2 :refile:
'''.rstrip()
    res = fixup(org)
    assert res == \
'''
#+title: whatever
* heading1
** heading11 :tag:
   some stuff
** TODO [#B] [2019-09-02 Mon 12:41] [[https://reddit.com/][some heading]] fewf :read:
** heading 12 :tag1:
* heading 2
'''.rstrip()
