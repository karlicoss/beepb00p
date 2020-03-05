#!/usr/bin/env python3
import argparse
import re
import sys
import tempfile
from subprocess import check_call, check_output, run
from pathlib import Path
from itertools import chain
import shutil
from typing import Tuple, List, Optional

# TODO mm, might need to apt install emacs-goodies-el first for code hightlight... https://stackoverflow.com/a/24087061/706389


ED = Path('~/.emacs.d').expanduser()

def uses_doom():
    return (ED / 'bin/doom').exists() # otherwise assumes spacemacs


def get_user_package_path(module: str) -> Path:
    """
    Get the full path of emacs package specified by name

    By default, Emacs would use systemwide packages (in /usr/share), which are typically very outdated.
    """
    def emacs_version() -> str:
        head = check_output(['emacs', '--version']).decode('utf8').splitlines()[0]
        return head[len("GNU Emacs "):]

    if uses_doom():
        m = ED / '.local/straight/build' / module
        assert m.exists(), m
        matches = [m]
    else:
        version = emacs_version() # ugh
        pp = ED / 'elpa'
        # TODO ugh. this is gonna break in the year 3000...
        matches = list(x for x in pp.glob(f'{version}/*/{module}-2*/') if x.is_dir())
    assert len(matches) == 1, (module, matches)
    return matches[0]


# TODO work around this one..
def filter_private(data: str) -> str:
    lines = [l for l in data.splitlines() if 'NOEXPORT' not in l]
    return '\n'.join(lines)


def main():
    p = argparse.ArgumentParser(description='Helper script to compile Org-mode files into HTML')
    p.add_argument('--output-dir', type=Path, default=None, help='output directory for byproduct files (e.g. plots)')
    p.add_argument('--test', type=Path, default=None, help='test on a specified files (otherwise tries to read stdin)')
    p.add_argument('--check-ids', action='store_true', help='Make sure there are no autogenerated org-mode ids')
    # TODO also strip them away?
    args = p.parse_args()
    if args.test is not None:
        org_data = args.test.read_text()
    else:
        org_data = sys.stdin.read()
    html = process(
        org_data=org_data,
        outdir=args.output_dir,
        check_ids=args.check_ids,
    )
    sys.stdout.write(html)


HTML = str


def process(
        *,
        org_data: str,
        outdir: Optional[Path]=None,
        check_ids: bool=True,
) -> HTML:
    org_data = filter_private(org_data)

    # evaluate in temporary directory for deterministic runs
    with tempfile.TemporaryDirectory() as td:
        tdir = Path(td)
        html, files = org_to_html(tdir=tdir, org_data=org_data)
        body = post_process(
            html,
            check_ids=check_ids,
        )

        # TODO test for that as well
        if len(files) > 0:
            assert outdir is not None
            outdir.mkdir(exist_ok=True, parents=True)
            for f in files:
                shutil.move(str(f), str(outdir / f.name))

        return body


def org_to_html(*, tdir: Path, org_data: str) -> Tuple[str, List[Path]]:
    inp_org  = tdir / 'input.org'
    inp_org.write_text(org_data)

    out_html = tdir / 'output.html'

    compile_org_el = (Path(__file__).parent / 'compile-org.el').read_text()
    compile_org_el = compile_org_el.format(
        throw_on_babel_errors='t',
        out_html=out_html,
    )

    compile_command = f'''
(progn
    {compile_org_el}
)'''

    modules = [
        'org-mode', # present for doom, but for spacemacs I've used a custom hack..
        'htmlize', 'dash', 's',
    ]

    res = run([
        'emacs',
        '--kill',
        '--batch',
        *chain.from_iterable(['--directory', str(get_user_package_path(module))] for module in modules),
        str(inp_org),
        '--eval', compile_command,
    ])
    # using run/returncode instead of check_call to avoid lots of visual spam from the exception
    if res.returncode > 0:
        raise RuntimeError(f"Emacs failed to compile {inp_org}")

    files = list(sorted(tdir.iterdir()))
    files.remove(inp_org)
    files.remove(out_html)
    return out_html.read_text(), files

def post_process(html: str, *, check_ids: bool) -> str:
    from bs4 import BeautifulSoup # type: ignore
    soup = BeautifulSoup(html, 'lxml')

    #### somewhat hacky support for {{{aside}}} macro for sidenotes
    # convert all
    # <?>text<aside>something<aside><?>
    # to
    # <div><span>text</span><aside class='sidenote'>something</aside></div>
    for aside in soup.find_all('aside'):
        parent = aside.parent

        assert parent.name in {'p', 'li'}
        if parent.name != 'p':
            pp = soup.new_tag(parent.name)
            old = parent.replace_with(pp)
            old.name = 'p'
            pp.append(old)
            parent = old

        assert parent.name == 'p'

        aside['class'] = aside.get('class', []) + ['sidenote']
        aside.extract()

        div = soup.new_tag('div')
        oldp = parent.replace_with(div)
        div.append(oldp)
        div.append(aside)
        oldp.name = 'span'
        oldp['class'] = oldp.get('class', []) + ['before-aside']
    ####

    TOC = 'table-of-contents'

    #### Add paragraph anchors to the headings
    # convert all
    # <h? id=someid>...</h?>
    # to
    # <h? id=someid><a href='#someid'></a>...</h?>
    # TODO title?
    for lvl in [2, 3]:
        htag = f'h{lvl}'
        for hh in soup.find_all(htag):
            parent = hh.parent

            if parent.name != 'div':
                continue

            if parent.attrs.get('id') == TOC:
                continue

            pcls = parent.attrs['class']
            if f'outline-{lvl}' not in pcls:
                continue

            hid = hh.attrs['id']

            if check_ids:
                DEFAULT_ORG_ID = r'org[\da-f]{7}'
                if re.fullmatch(DEFAULT_ORG_ID, hid):
                    # TODO later exit..
                    raise RuntimeError(f"Please define custom ID for {hh}")

            newa = soup.new_tag('a', attrs={
                'href' : '#' + hid,
                'class': 'headerlink',
            })
            newa.string = '¶'
            hh.insert(0, newa)
    ####

    ### put intrapage arrows to indicate that link refers to content on the page
    toc = soup.find(id=TOC)
    id_map = {}
    for idx, tag in enumerate(soup.find_all()):
        id_ = tag.get('id')
        if id_ is None:
            continue
        # TODO hopefully, unique?
        id_map[id_] = idx

    # TODO ignore autogenerated org links?
    for idx, tag in enumerate(soup.find_all()):
        if tag.name != 'a':
            continue
        href = tag.get('href')
        if href is None:
            continue

        if toc in tag.parents:
            # do not put arrows in table of contents
            continue

        is_intrapage = href.startswith('#')
        if not is_intrapage:
            continue

        id_ = href[1:]
        to_idx = id_map[id_]

        dist = abs(to_idx - idx)
        if dist <= 1:
            # probably, 'headerlink'
            continue
        arrow = "link-down" if to_idx > idx else "link-up"
        tag['class'] = tag.get('class', []) + [arrow]
    ###


    ### remove id="outline-container-*
    ### these are simply useless and just clutter the HTML and diffs
    ### <hN> tags already have ids
    for div in soup.find_all('div'):
        id_ = div.get('id')
        if id_ is None:
            continue
        if re.match('(outline-container-|text-org00)', id_):
            del div.attrs['id']
    ###

    # TODO some whitespace in tags???

    # extract body because that's what hakyll expects
    body = str(soup.find('body'))
    #

    # ugh. didn't find a nicer way to extract contents of tag
    assert body.startswith('<body>')
    assert body.endswith('</body>')
    body = body[6: -7]
    return body


if __name__ == '__main__':
    main()


def get_src() -> str:
    path = Path(__file__).absolute().parent.parent / 'content/special/sandbox/test.org'
    return path.read_text()


# TODO literate test docs
def test_aside(tmp_path):
    src = get_src()

    # precondition
    assert 'on the right {{{aside(see' in src

    html = process(
        org_data=src,
        outdir=tmp_path,
        check_ids=False,
    )
    # TODO ?? href?? need to fix outdir?
    expected = '<aside class="sidenote">see <a class="post-tag" href="/tags.html#extendedmind">#extendedmind</a></aside>'
    assert expected in html


# TODO also tests intrapage links by accident, but ok for now..
def test_section_links(tmp_path):
    src = get_src()

    # precondition
    assert '* intrapage link to' in src

    html = process(
        org_data=src,
        outdir=tmp_path,
        check_ids=False,
    )

    # TODO use regex?
    expected = '<h2 id="past"><a class="headerlink" href="#past">¶</a>intrapage link to a <a class="link-down" href="#something">future</a> heading</h2>'
    assert expected in html


def test_removes_useless_ids(tmp_path):
    src = get_src()

    # precondition
    assert '* regular heading' in src

    html = process(
        org_data=src,
        outdir=tmp_path,
        check_ids=False,
    )

    assert re.search('<h2.*regular heading</h2>', html)  # precondition

    assert not re.search(r'id="outline-container-', html)
    assert not re.search(r'id="text-org00', html)
