#!/usr/bin/env python3
import argparse
import re
import sys
import tempfile
from subprocess import check_call, check_output, run
from pathlib import Path
from itertools import chain
import shutil
from typing import Tuple, List, Optional, Sequence, Union, Iterable

from more_itertools import ilen

import logging

def get_logger():
    return logging.getLogger('compile_org')

# TODO mm, might need to apt install emacs-goodies-el first for code hightlight... https://stackoverflow.com/a/24087061/706389

# TODO meh
DEFAULT_ORG_P_MARGIN = '16px'

ED = Path('~/.emacs.d').expanduser()

def uses_doom() -> bool:
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


EXIT_WARNING = 1


def main() -> None:
    p = argparse.ArgumentParser(
        description='Helper script to compile Org-mode files into HTML',
        epilog=f'''
EXIT CODE:
       {EXIT_WARNING} if the file compiled successfully, but with some non-fatal errors.
'''
    )
    p.add_argument('--output-dir', type=Path, default=None, help='output directory for byproduct files (e.g. plots)')
    p.add_argument('--input', type=Path, default=None, help='use the specified file (otherwise tries to read stdin)')
    p.add_argument('--check-ids', action='store_true', help='Make sure there are no autogenerated org-mode ids')
    p.add_argument('--active-tags', help='List of tags existing on beepb00p.xyz (to style it properly)', default='')

    fg = p.add_mutually_exclusive_group()
    fg.add_argument('--format', default='html')
    fg.add_argument('--html', action='store_const', const='html', dest='format')
    fg.add_argument('--org' , action='store_const', const='org' , dest='format')
    # TODO also strip them away?
    args = p.parse_args()

    active_tags = () if len(args.active_tags) == 0 else args.active_tags.split(',')

    inf: Path = args.input
    deps = []
    if inf is not None:
        # TODO FIXME meh. later, parse '#+include' properly??
        # TODO might need to be recursive...
        if inf.name == 'hpi.org':
            deps.extend([inf.parent / 'my-data.org'])
        org_data = inf.read_text()
    else:
        org_data = sys.stdin.read()
    output, ierrs = process(
        format=args.format,
        org_data=org_data,
        outdir=args.output_dir,
        check_ids=args.check_ids,
        active_tags=active_tags,
        deps=deps,
    )
    logger = get_logger()
    errs = list(ierrs)
    code = 0
    for e in errs:
        logger.exception(e)
        code = EXIT_WARNING

    sys.stdout.write(output)
    sys.exit(code)


HTML = str
Result = Tuple[HTML, Iterable[Exception]]


def post_process_org(output: str) -> str:
    # eh, not sure what's up with empty drawer exports...
    output = output.replace('nil:END:', ':END:')

    # ugh. seems that org-ruby can't handle it...
    output = output.replace('#+results:', '')

    lines = []
    # TODO use something more robust..
    # TODO control this behaviour? only need it for 'external' org-mode..
    for line in output.splitlines(): # TODO careful about whitespace?
        # TODO shit. so sometimes we do need the identifier and can't just pipe
        # ok, for now just assume it's not referring to parent and always in root...
        # TODO need to find all...
        fre = re.escape('[[file:') + r'(.+?)' + re.escape(']')
        while True:
            # well it's a bit shit, but whatever..
            m = re.search(fre, line)
            if m is None:
                break
            oname = m.group(1)
            # uhoh
            hname = oname
            hname = hname.replace('.org', '.html')
            hname = hname.replace('::#', '#')
            assert '::' not in hname, line

            # jeez... necessary because of absolute org-mode includes..
            # will probably remove after I add blog/content to path or smth??
            hname = hname.replace('../../blog/content/', '')

            hname = 'https://beepb00p.xyz/' + hname

            line = line.replace('file:' + oname, hname)

        # just in case..
        assert '[[file:' not in line, line
        lines.append(line)

    return '\n'.join(lines)


def process(
        *,
        org_data: str,
        outdir: Optional[Path]=None,
        check_ids: bool=True,
        active_tags: Sequence[str]=(),
        format: str='html',
        deps: Sequence[Path]=(),
) -> Result:
    org_data = filter_private(org_data)

    # evaluate in temporary directory for deterministic runs
    with tempfile.TemporaryDirectory() as td:
        tdir = Path(td)

        tdeps = []
        for d in deps:
            tdep = tdir / d.name
            tdeps.append(tdep)
            shutil.copy(d, tdep)

        # TODO not sure if it should return files..
        # figure out if smth else is using it and get them externally?
        output, files = org_to_html(
            tdir=tdir,
            org_data=org_data,
            format=format,
        )
        for tdep in tdeps:
            files.remove(tdep)

        errs: Iterable[Exception] = []
        if format == 'html':
            # meh
            output, errs = post_process_html(
                output,
                check_ids=check_ids,
                active_tags=active_tags,
            )
        elif format == 'org':
            output = post_process_org(output)
        else:
            raise RuntimeError(format)

        for tdep in tdeps:
            tdep.unlink()

        # TODO test for that as well
        if len(files) > 0:
            assert outdir is not None
            outdir.mkdir(exist_ok=True, parents=True)
            for f in files:
                shutil.move(str(f), str(outdir / f.name))

        return (output, errs)


def emacs(*args, **kwargs) -> None:
    modules = [
        'org-mode', # present for doom, but for spacemacs I've used a custom hack..
        'htmlize', 'dash', 's',
    ]

    check_call([
        'emacs',
        '--kill',
        '--batch',
        # TODO --no-init-file??
        *chain.from_iterable(['--directory', str(get_user_package_path(module))] for module in modules),
        *args,
    ], **kwargs)


# TODO rename??
def org_to_html(*, tdir: Path, org_data: str, format: str='html') -> Tuple[str, List[Path]]:
    inp_org  = tdir / 'input.org'
    inp_org.write_text(org_data)

    out_html = tdir / 'output.html'

    compile_org_el = Path(__file__).absolute().parent / 'compile-org.el'

    compile_command = f'''
(progn
    (setq compileorg/throw-on-babel-errors t)
    (setq compileorg/output-file           "{out_html}")
    (setq compileorg/output-format         "{format}")
)'''

    emacs(
        str(inp_org),
        '--eval', compile_command,
        '--load', compile_org_el,
    )

    files = list(sorted(tdir.iterdir()))
    files.remove(inp_org)
    files.remove(out_html)
    return out_html.read_text(), files


def make_soup(html: str):
    from bs4 import BeautifulSoup # type: ignore
    soup = BeautifulSoup(html, 'lxml')
    return soup


def post_process_html(html: str, *, check_ids: bool, active_tags: Sequence[str]) -> Result:
    soup = make_soup(html)

    errors: List[Exception] = []

    #### somewhat hacky support for {{{aside}}} macro for sidenotes
    # convert all
    # <?>text<aside>something<aside><?>
    # to
    # <div><?>text</?><aside class='sidenote'>something</aside></div>
    for aside in soup.find_all('aside'):
        parent = aside.parent

        # TODO hmm. maybe needs to support h* tags (for * something {aside})
        assert parent.name in {'p', 'li'}, parent
        if parent.name != 'p':
            pp = soup.new_tag(parent.name)
            old = parent.replace_with(pp)
            old.name = 'p'
            pp.append(old)
            parent = old

        assert parent.name == 'p'

        aside['class'] = aside.get('class', []) + ['sidenote']
        aside.extract()

        # right.. I can't use 'p' here, because <p> can only contain inline elemnts
        # and aside seems to be block element..
        div = soup.new_tag('div')
        div['style'] = f'margin-top: {DEFAULT_ORG_P_MARGIN}; margin-bottom: {DEFAULT_ORG_P_MARGIN}'
        oldp = parent.replace_with(div)
        div.append(oldp)
        div.append(aside)
        # NOTE if I use 'p' here, the margins end up expanding the parent div
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
                    errors.append(RuntimeError(f"Please define an ID for {hh}"))

            newa = soup.new_tag('a', attrs={
                'href' : '#' + hid,
                'class': 'headerlink',
            })
            newa.string = '¶'
            hh.insert(0, newa)
    ####

    toc = soup.find(id=TOC)
    # TODO not sure if it's always present?

    ### put intrapage arrows to indicate that link refers to content on the page
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
    # TODO to be fair, paragraph ids make sense, but if they are unstable, they do more harm
    # TODO some of them do not follow text-org, e.g. text-1

    ### tag handling + TOC handling

    # add links
    tag_blocks = soup.select('.tag')
    # TODO this might break exobrain?
    for tag_block in tag_blocks:

        for x in list(tag_block.strings):
            nbsp = '\xa0'
            x.replace_with(x.strip(nbsp))

        # so it's got shape like <span class="tag"><span class="tag1">tag1</span>....</span>
        for tag in tag_block.find_all('span'):
            tag_name = tag.get_text() # todo not sure if should use class instead?
            active = tag_name in active_tags
            # todo not sure if should keep tag's class?
            tag.name = 'a'
            link = './tags.html' + (f'#{tag_name}' if active else '')
            tag['href'] = link
            tag['class'] = tag.get('class', []) + ['tag-active' if active else 'tag-inactive']


    for a_elem in ([] if toc is None else toc.find_all('a')):
        ### make sure tags in TOC are outside of the link
        tag = a_elem.select_one('.tag')
        if tag is not None:
            tag.extract()
            a_elem.insert_after(tag)
        # TODO  not sure about .todo/.done states or prio..
    ###


    # extract body because that's what the generator expects
    body = str(soup.find('body'))
    #

    # ugh. didn't find a nicer way to extract contents of tag
    assert body.startswith('<body>')
    assert body.endswith('</body>')
    body = body[6: -7]
    return body, errors


if __name__ == '__main__':
    logging.basicConfig(level=logging.INFO)
    main()


def get_test_src() -> str:
    path = Path(__file__).absolute().parent.parent / 'content/sandbox/test.org'
    return path.read_text()


# TODO literate test docs
def test_aside(tmp_path: Path) -> None:
    src = get_test_src()

    # precondition
    assert 'on the right {{{aside(see' in src

    html, errs = process(
        org_data=src,
        outdir=tmp_path,
        check_ids=False,
    )
    assert ilen(errs) == 0

    # TODO ?? href?? need to fix outdir?
    expected = '<aside class="sidenote">see <a class="post-tag" href="/tags.html#extendedmind">#extendedmind</a></aside>'
    assert expected in html


# TODO also tests intrapage links by accident, but ok for now..
def test_section_links(tmp_path: Path) -> None:
    src = get_test_src()

    # precondition
    assert '* intrapage link to' in src

    html, errs = process(
        org_data=src,
        outdir=tmp_path,
        check_ids=False,
    )
    assert ilen(errs) == 0

    # TODO use regex?
    expected = '<h2 id="past"><a class="headerlink" href="#past">¶</a>intrapage link to a <a class="link-down" href="#something">future</a> heading</h2>'
    assert expected in html


def test_removes_useless_ids(tmp_path: Path) -> None:
    src = get_test_src()

    # precondition
    assert '* regular heading' in src

    html, errs = process(
        org_data=src,
        outdir=tmp_path,
        check_ids=False,
    )
    assert ilen(errs) == 0

    assert re.search('<h2.*regular heading</h2>', html)  # precondition

    assert not re.search(r'id="outline-container-', html)
    assert not re.search(r'id="text-org00', html)


def test_tag_handling(tmp_path: Path) -> None:
    src = get_test_src()

    # precondition
    assert 'heading with tags' in src

    html, errs = process(
        org_data=src,
        outdir=tmp_path,
        check_ids=False,
        active_tags=['tag2'],
    )
    assert ilen(errs) == 0

    # NOTE: there are some sneaky nbsps there..
    # (defun org-html-format-headline-default-function
	# (and tags "&#xa0;&#xa0;&#xa0;") tags)))

    ## toc handling + adding tag links
    ##                                                                                                                                           vvv NBSPS!!
    before = '<li><a href="#something"><span class="timestamp-wrapper"><span class="timestamp">[2019-09-02 19:45]</span></span> heading with tags   <span class="tag"><span class="tag1">tag1</span> <span class="tag2">tag2</span></span></a>'
    after  = '<li><a href="#something"><span class="timestamp-wrapper"><span class="timestamp">[2019-09-02 19:45]</span></span> heading with tags</a><span class="tag"><a class="tag1 tag-inactive" href="./tags.html">tag1</a><a class="tag2 tag-active" href="./tags.html#tag2">tag2</a></span>'
    # aft    = '<li><a href="#something"><span class="timestamp-wrapper"><span class="timestamp">[2019-09-02 19:45]</span></span> heading with tags</a><span class="tag"><a class="tag1 tag-inactive" href="./tags.html">tag1</a><a class="tag2 tag-inactive" href="./tags.html">tag2</a></span>'

    assert after in html