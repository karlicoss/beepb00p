#!/usr/bin/env python3
from pathlib import Path
import re
from typing import Tuple, Iterable

import bs4 # type: ignore


File = str
Id = str
Text = str

def walk(node) -> Iterable[Tuple[Id, Text]]:
    id_ = node.get('id')
    if id_ == 'table-of-contents': # ignore it
        node.decompose()
        return

    cs = list(node.find_all(recursive=False))
    # FIXME kinda annoying that headerlink points at h2, not the enclosing container?
    # on the other hand I guess makes more sense to get the 'closest' link rather than the actually enclosing?
    for c in cs:
        yield from walk(c)

    if id_ is not None:
        text = node.text
        text = re.sub(r'\n+', '\n', text).strip()
        if len(text) > 0:
            yield (id_, text)
        node.decompose() # prevent from being processed by the parent


def walk_all(root: Path) -> Iterable[Tuple[File, Id, Text]]:
    htmls = list(sorted(root.rglob('*.html')))
    assert len(htmls) > 0, root
    for html in htmls:
        if html.name == 'sitemap.html':
            continue
        soup = bs4.BeautifulSoup(html.read_text(), 'lxml').find('body')
        rpath = html.relative_to(root)
        for key, res in walk(soup):
            yield (str(rpath), key, res)


def run(root: Path) -> None:
    import json
    documents = [{
        'file': file,
        'id'  : id,
        'text': text,
    } for file, id, text in walk_all(root)]
    print('let documents = ' + json.dumps(documents, indent=1, ensure_ascii=False))


import click
@click.command()
@click.option('--path', type=Path, required=True)
def main(path: Path) -> None:
    run(path)


if __name__ == '__main__':
    main()
