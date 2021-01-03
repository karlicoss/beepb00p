#!/usr/bin/env python3
from pathlib import Path
import sys
from shutil import rmtree, copy
from subprocess import check_call, check_output
from typing import List

# TODO sanity check that there are noexport entries or some private tags (e.g. people mentioned)
# ./check

def ccall(*args, **kwargs):
    print(args, file=sys.stderr)
    return check_call(*args, **kwargs)

# TODO editable?
# TODO huh, that actually would be nice. I could fix stuff in-place and then apply to org-mode
# TODO search settings?
# TODO wonder if I can integrate it with blog's header?
# TODO export txt files as md as well?

from compile_org import emacs


root_dir = Path(__file__).absolute().parent.parent
input_dir  = root_dir / 'input'
public_dir = root_dir / 'public'
md_dir     = root_dir / 'markdown'
html_dir   = root_dir / 'html'

input_dir   = input_dir .resolve() # ugh. otherwise relative links might end up weird during org-publish-cache-ctime-of-src
public_dir  = public_dir.resolve()
md_dir      = md_dir    .resolve()
html_dir    = html_dir  .resolve()


builtin = 'builtin' # builtin emacs export


src = root_dir / 'src'

def clean_dir(path: Path) -> None:
    assert path.is_dir(), path
    for x in path.iterdir():
        if x.name == '.git':
            continue
        if x.is_file():
            x.unlink()
        else: # dir
            rmtree(x)


def clean() -> None:
    # todo ugh, need symlink watching tool here again...
    cachedir = Path('~/.org-timestamps').expanduser()
    # right, so it keeps track of modifications and doesn't publish if it wasn't modified
    # but completely unclear how it behaves wrt added/removed files etc
    # also wouldn't know if the source code was modified... so best not to use it I guess
    for c in cachedir.glob('*.cache'):
        c.unlink()

    clean_dir(md_dir)
    clean_dir(html_dir)

    # TODO what about empty dirs?
    for f in public_dir.rglob('*.org'):
        f.unlink()


def main() -> None:
    import argparse
    p = argparse.ArgumentParser()
    p.add_argument('--add'   , action='store_true')
    p.add_argument('--target', type=str, required=True)
    args = p.parse_args()
    target = args.target

    preprocess(args)
    postprocess_builtin()


def preprocess(args) -> None:
    clean()

    target = args.target
    eargs = [
        '--eval', f'''(progn
            (setq exobrain/input-dir  "{input_dir}" )
            (setq exobrain/public-dir "{public_dir}")
            (setq exobrain/md-dir     "{md_dir}"    )
            (setq exobrain/html-dir   "{html_dir}"  )
        )''',
        '--directory', src / 'advice-patch',
        '--load'     , src / 'publish.el',
        '-f', 'toggle-debug-on-error', # dumps stacktrace on error
        # adjust this variable to change the pipeline
        '--eval', f'''
(let ((org-publish-project-alist `(
        ,exobrain/project-preprocess-org
        ,exobrain/project-org2html
       )))
  (org-publish-all))
'''.strip(),
    ]
    with emacs(*eargs) as ep:
        pass
    assert ep.returncode == 0

    # TODO need to clean public dir??
    # TODO call check_org after preprocess-org instead??
    from check import check_org
    check_org(public_dir)

    # TODO think about commit/push/deploy logic?
    assert (public_dir / '.git').is_dir(), public_dir
    ccall(['git', 'status'], cwd=public_dir)

    if args.add:
        ccall(['git', 'add', '-A', '--intent-to-add'], cwd=public_dir)
        ccall(['git', 'add', '-p'], cwd=public_dir)
        # TODO suggest to commit/push?

def postprocess_builtin() -> None:
    # todo crap. it's not idempotent...
    copy(src / 'search/search.css', html_dir / 'search.css')
    copy(src / 'search/search.js' , html_dir / 'search.js' )
    copy(src / 'exobrain.css'     , html_dir / 'exobrain.css')

    from bs4 import BeautifulSoup as BS # type: ignore

    sitemap = html_dir / 'sitemap.html'
    assert sitemap.exists()
    soup = BS(sitemap.read_text(), 'lxml')
    node = soup.find(id='content')
    node.select_one('.title').decompose()
    node.name = 'nav' # div by deafult
    node['id'] = 'exobrain-toc'
    toc = node.prettify()

    # todo eh.. implement this as an external site agnostic script
    (html_dir / 'documents.js').write_text(check_output([
        src / 'search/makeindex.py',
        '--root', html_dir,
    ]).decode('utf8'))

    shtml = src / 'search/search.html'
    soup = BS(shtml.read_text(), 'lxml')
    node = soup.find(id='search')
    search_body = node.prettify()

    for html in html_dir.rglob('*.html'):
        text = html.read_text()
        depth = len(html.relative_to(html_dir).parts) - 1
        rel = ('' if depth == 0 else '../' * depth)

        # fixme need to relativize properly
        tocr = toc.replace(
            'href="',
            'href="' + rel,
        )
        # meh... this gets too complicated
        sidebar = f"""
<a id='jumptosidebar' href='#sidebar'>Jump to exobrain search &amp sitemap</a>
<div id='sidebar'>
{search_body}
{tocr}
</div>
"""

        # ugh. very annoying... this is ought to be easier...
        rel_head = f'''
<script>
const PATH_TO_ROOT = "{rel}"
</script>
'''

        search_head = f'''
<link  href='{rel}search.css' rel='stylesheet'>
<script src='{rel}search.js'></script>
'''
        text = text.replace(
                      '\n</body>\n',
            sidebar + '\n</body>\n',
        )
        text = text.replace(
                                      '\n</head>\n',
             rel_head + search_head + '\n</head>\n',
        )

        text = text.replace("href='/exobrain.css", f"href='{rel}exobrain.css") # fixme need to relativize properly...
        html.write_text(text)

    (html_dir / 'index.html').symlink_to('README.html') # meh
    # TODO relativize in the very end maybe?

# TODO add this back
# link = '<a style="font-size: 2rem; line-height: var(--menu-bar-height);" href="https://beepb00p.xyz">back to blog</a>'

if __name__ == '__main__':
    # TODO allow skipping?
    check_call(['mypy', '--check-untyped', __file__])
    main()
