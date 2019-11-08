#!/usr/bin/env python3
"""
[[https://github.com/karlicoss/orger][Orger]] module to show new comments in my todo list
"""
from pathlib import Path
from datetime import datetime

import dataset # type: ignore

from orger import InteractiveView
from orger.inorganic import node, link
from orger.common import todo

class IssoComments(InteractiveView):
    def get_items(self):
        db = self.cmdline_args.db
        for c in dataset.connect(f'sqlite:///{db}')['comments'].all():
            author = c['author']
            if author == 'karlicoss':
                continue # ignore my own comments

            text   = c['text']
            cid    = str(c['id'])
            ts     = c['created']
            dt = datetime.utcfromtimestamp(ts)
            # TODO tid for isso_id and proper link?
            yield cid, todo(
                dt=dt,
                heading=f'{author}: {text}',
                tags=['blog'], # TODO maybe allow specifying : separated?
            )

def setup_parser(p):
    p.add_argument('--db', type=Path, required=True, help='Path to Isso comments database')


if __name__ == '__main__':
    IssoComments.main(setup_parser=setup_parser)
