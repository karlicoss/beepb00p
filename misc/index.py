#!/usr/bin/env python3
from datetime import datetime
import textwrap

import dotpy
dotpy.init(__name__) # TODO extremely meh
# TODO could use similar trick for dron?


from dotpy import *

from . import build


INPUTS = build.get_inputs()

# todo would be nice to cache?
METAS = [
    (path.stem, build.org_meta(build.content / path))
    for path in INPUTS
    if path.suffix == '.org' and path.stem not in (
            # todo for fucks sake... fix it properly...
            'axol',
            'myinfra',
            'blog-graph',
            'contemp-art',
            'ideas',
            'kython',
            'least-action-lie',
            'site',
            'notes',
            'test',
            'scrapyroo',
            'tags',
            # todo would be nice to link to it actually?
    )
]

# TODO uhh... this is a bit circular? also need to exclude special?

# todo ok, upid is kinda irrelevant, only useful for comments
# not sure what to do...
for (url, date, summary) in [
        ('takeout_data_gone'  , '08 March 2019'   , ''),
        ('grasp'              , '09 February 2019', 'How to capture infromation from your browser and stay sane'),
        ('heartbeats_vs_kcals', '03 August 2019'  , ''),
        ('exercise_bike_model', '08 December 2019', 'How I found my exercise machine to violate laws of physics'),
]:
    METAS.append((url, build.Post(
        date=date,
        summary=summary,
        upid=url, # TODO FIXME not sure about this
        title='',
        body='',
        draft=False,
        has_math=False,
        tags=(),
        feed=True,
        url='',
        special=False,
    )))


EXISTING = {x: m for x, m in METAS}

def maybe_meta(name: str):
    # TODO meeeh. pretty horrible
    for u in [name, name.replace('_', '-'), name.replace('-', '_')]:
        r = EXISTING.get(u)
        if r is not None:
            return r
    return None


def meta(name: str):
    m = maybe_meta(name)
    assert m is not None, name
    return m


def upid(name: str) -> str:
    m = meta(name)
    return m.upid


# TODO MEH!
ALL_POSTS = []

# TODO later, add dates...
def P(label: str, tags: str='', **kwargs) -> Node:
    def make_label(self_, label=label, tags=tags) -> str:
        m = meta(self_.name)
        upid = m.upid
        post_url = m.url

        # todo links? not sure
        # todo font?

        etags = [tag.name for tag in m.tags if tag.exists]
        assert len(etags) <= 3, etags # TODO
        etags = [None] * (3 - len(etags)) + etags

        def make_tag(tag):
            if tag is None:
                return '<td> </td>'
            else:
                return f'''
                <td
                   href="https://beepb00p.xyz/tags.html#{tag}"
                   align="right"
                ><font face="monospace" color="#aa5511">#{tag}</font></td>
                '''


        mtags = ' '.join(make_tag(tag) for tag in etags)
        mdraft = '🚧wip🚧' if m.draft else ''
        # https://graphviz.gitlab.io/_pages/doc/info/shapes.html#html ok this is a good guide


        summary = m.summary
        lines = textwrap.wrap(summary, 50, break_long_words=False)
        summary = '<br/>'.join(lines) or " " # ugh

        # TODO mmm. displaying summary is nice, but it might be too long..
        dates = m.date if isinstance(m.date, str) else m.date_human
        # todo not sure about colspan 4..
        label = f'''
<table border="0">
<tr>
  <td
    colspan="4"
    href="https://beepb00p.xyz{post_url}"
    title="{m.title}"
  >
   <font color="blue"     >{label}  </font><br/>
   <font color="darkgreen">{summary}</font>
  </td>
</tr>
<tr>
  <td align="left" >{mdraft}</td>
  {mtags}
</tr>
<tr>
  <td align="left"  href="#{upid}" tooltip="Show connections">🔍</td>
  <td align="right" href="#{upid}" tooltip="Show connections" colspan="3"><font face="monospace" color="#666666">{dates}</font></td>
</tr>
</table>
'''.strip()
  # <td align="right"><font face="monospace">{mtags}</font></td>
        # todo hl year differently?
        # todo 'focus'?
        label = f'< {label} >'
        return label

    n = node(
        label=make_label,
        # set_class=True, #todo not sure about this, use lazy property?
        id=lambda self_: upid(self_.name),
        **{'class': lambda self_: upid(self_.name)},

        # TODO keep URL anyway? duno
        # URL=make_url,
        # fontcolor=INTERNAL,

        # TODO need supplemenary js?
        # TODO maybe even keep in dotpy? dunno
        **kwargs,
    )
    ALL_POSTS.append(n)
    return n


def order(*args, **kwargs):
    return edges(*args, constraint='true', **invisible, arrowhead='none', **kwargs)
    # TODO disable for neato?
    # return []



# TODO move to core!
def medge(fr, to, **kwargs):
    assert 'class' not in kwargs
    def nodeup(n: Nodish) -> str:
        if isinstance(n, str):
            name = n
        else:
            name = n.name
        return upid(name)

    kwargs['class'] = lambda self_: f'{nodeup(fr)} {nodeup(to)}'
    return edge(fr, to, **kwargs)


# TODO would be nice to add dates

# TODO could make 'auto' label (or extracting from blog)
# should be lazy then too
# todo warn about unused (unrechable?)
# TODO make it possible to simply pass dict and unpack
grasp       = P('Grasp', tags='#orgmode') # todo name 'inherit'??
promnesia   = P('Promnesia', tags='#pkm #promnesia')
# aaaa = promnesia # TODO shit, this is a bit unfortunate...
hpi         = P('HPI<br/>(Human Programming Interface)')
sad_infra   = P('The sad state of personal data and infrastructure')
exports     = P('Building data liberation infrastructure')
# TODO would be nice to display tags in boxes or something?
mypy_error_handling = P('Using mypy for error handling', tags='#python #mypy #plt')
pkm_setup   = P('How to cope with a fleshy human brain', tags='#pkm')

exercise_bike_model  = P("Analyzing accuracy of power reported by stationary bike", tags='#quantifiedself')
heartbeats_vs_kcals = P("Making sense of Endomondo's calorie estimation", tags='#quantifiedself')
cloudmacs   = P('Cloudmacs', tags='#emacs')
pkm_todos   = P('My GTD setup', tags='#gtd #orgmode')
scheduler   = P('In search of a better job scheduler')
configs_suck= P('Your configs suck? Try a real programming language')
unnecessary_db  = P('Against unnecessary databases')
orger           = P('Orger')
my_data     = P('What data on myself I collect and why?')
orger_todos     = P('Using Orger for processing infromation')
myinfra_roam= P('Extending my personal infrastructure', tags='#emacs #orgmode')
# todo could do a table for tags?
takeout_data_gone = P('Google Takeouts silently removes old data')
annotating  = P('How to annotate everything')
pkm_search  = P('Building personal search infrastructure')


G = digraph(
    '''
  ## these are for neato
  overlap=scale
  splines=true
  ##

  ## TODO hmm without size isn't so bad?
  # ratio=fill
  # size=20
  ##

  # newrank=true
  rankdir=BT
  # TODO ??? rank=min
  # searchsize=500
  ranksep=1.5

  node[shape=box style=dashed]
    '''.strip(), # todo not sure how many spaces I like

    cluster(
        orger,
        my_data,
        orger_todos,
        myinfra_roam,
        takeout_data_gone,
        promnesia,
        hpi,
        sad_infra,
        exports,
        name='main',
        label='Data liberation', # TODO and ???
        style=dotted,
    ),

    cluster(
        'edge [constraint=false]',
        scheduler,
        configs_suck,
        unnecessary_db,
        mypy_error_handling,
        *order(mypy_error_handling, unnecessary_db, scheduler, configs_suck),
        medge(scheduler, exports),
        medge(scheduler, my_data),
        medge(scheduler, orger  ),
        medge(configs_suck, hpi ),
        medge(configs_suck, promnesia),
        medge(unnecessary_db, my_data),
        medge(unnecessary_db, hpi),
        name='aux',
        label='Tools/Libraries',
        style=dotted,
    ),

    cluster(
        annotating,
        pkm_search,
        pkm_todos,
        cloudmacs,
        grasp,
        pkm_setup,
        # TODO needs to be date ordered?..
        *order(grasp, cloudmacs, annotating, pkm_todos, pkm_search),
        name='pkm',
        label='PKM',
        style=dotted,
    ),

    cluster(
        heartbeats_vs_kcals,
        exercise_bike_model,

        *order(heartbeats_vs_kcals, exercise_bike_model),

        name='qs',
        label='Quantified self',
        style=dotted,
    ),

    medge(grasp            , pkm_setup  ),
    medge(pkm_todos        , pkm_setup),
    medge(annotating       , pkm_setup  ),
    medge(cloudmacs        , pkm_setup  ),
    medge(pkm_search       , pkm_setup  ),
    medge(orger            , orger_todos),
    medge(takeout_data_gone, my_data    ),
    medge(promnesia        , pkm_setup, **noconstraint),
    medge(orger            , pkm_setup, **noconstraint),
    medge(orger_todos      , pkm_setup, **noconstraint),
    medge(exports          , hpi),
    medge(my_data          , hpi),
    medge(hpi              , exercise_bike_model, **noconstraint),
    medge(hpi              , heartbeats_vs_kcals, **noconstraint),
    medge(hpi             , myinfra_roam),
    medge(promnesia       , myinfra_roam, **noconstraint),
    medge(orger           , myinfra_roam),

    # todo maybe syntax with operators or something?
    medge(hpi, promnesia),

    medge(sad_infra, promnesia),
    medge(sad_infra, hpi),
    medge(sad_infra, my_data),
    medge(sad_infra, pkm_setup, **noconstraint),

    medge(mypy_error_handling, hpi      , **noconstraint),
    medge(mypy_error_handling, promnesia, **noconstraint),
)


from typing import Optional
from pathlib import Path


# TODO crap. first-child doesn't work?
STYLE = '''
@-moz-document url-prefix() {
  .node polygon + g a text {
     font-size: 72%;
  }
}

svg {
  zoom: 70%;
}
'''

# old js stuff
# .node.hl polygon {
#    stroke: red;
# }
#
# .edge.hl path {
#    stroke: red;
# }
# .edge.hl polygon {
#    stroke: red;
#    fill: red;
# }


# absolutely mental, but I like the idea of using just CSS for it...
def node_css(node: Node) -> str:
    cls = upid(node.name)
    # todo use fill for polygon for arrow heads?
    return f'''
g.{cls}:target polygon,
g.{cls}:target ~ .{cls}.edge path,
g.{cls}:target ~ .{cls}.edge polygon {{stroke: red; stroke-width: 2px;}}
'''


def make_style() -> str:
    return STYLE + '\n'.join(node_css(node) for node in ALL_POSTS)


# TODO hmmm. not sure what's better -- a larger CSS, or a tiny JS.. but this is interesting...


JS = ''
JS_PREV = '''
const HL_CLASS = 'hl';

const cb = (e) => {
  for (const hld of document.querySelectorAll('.' + HL_CLASS)) {
    hld.classList.remove(HL_CLASS);
  }

  const hsh = e.target.location.hash
  if (hsh == null || hsh == '')
     return

  const name = hsh.substr(1);
  const things = document.querySelectorAll('.' + name)
  for (const th of things) {
    th.classList.add('hl')
  }
}
window.addEventListener('load'      , cb, false)
window.addEventListener('hashchange', cb, false)
'''
        # <noscript>
        #     Javascript is required for edge highlights to work. Sorry!
        # </noscript>

# todo separate thing to hl deps
HTML = '''
<!doctype html>
<html lang="">
    <head>
        <meta charset="utf-8">
        <meta http-equiv="x-ua-compatible" content="ie=edge">
        <title>Index</title>
        <script>{JS}</script>
    </head>
    <body>
        {SVG}
    </body>
</html>
'''

# TODO generate HTML too? eh.

def generate(to: Optional[Path]) -> str:
    dot = render(G)
    # todo eh. dump intermediate?

    from subprocess import run, PIPE
    res = run(['dot', '-T', 'svg'], input=dot.encode('utf8'), check=True, stdout=PIPE)
    svg = res.stdout.decode('utf8')

    svg = with_style(svg=svg, style=make_style())

    html = HTML.format(SVG=svg, JS=JS)
    Path('index.html').write_text(html)


    if to is None:
        import sys
        sys.stdout.write(svg)
    else:
        to.write_text(svg)


def main():
    import argparse
    p = argparse.ArgumentParser()
    p.add_argument('file', nargs='?', default='-')
    # TODO pass engine?
    args = p.parse_args()

    # todo should end with .svg?
    file = None if args.file == '-' else Path(args.file)
    generate(to=file)
    # TODO, ok add right after svg
    #


if __name__ == '__main__':
    main()

# TODO ok, so I guess I need to keep it in sync with the frontpage...
# need to declare stuff anyway.. so
# warn on mismatches
# TODO integrate with compile script
# TODO integrate with compile script to load information about posts!
# I think specifying deps etc in python is fine.. just keep separate from the blog compiler?
# TODO mark drafts separately?

# TODO send other posts in 'misc'
# TODO identify by upid??
# TODO on click, highlight edges that lead to a node
# add classes to css

# TODO hmm, fdp isn't so different from neato... also have to remove cluster_

# TODO space a bit more between layers
# TODO somehow set gravity to top??

# TODO maybe remove boxes?

# TODO color edges?
