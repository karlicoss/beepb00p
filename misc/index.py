#!/usr/bin/env python3
from datetime import datetime

import dotpy
dotpy.init(__name__) # TODO extremely meh
# TODO could use similar trick for dron?


from dotpy import *


import build


INPUTS = build.get_inputs()

# todo would be nice to cache?
METAS = [(path.stem, build.org_meta(build.content / path)) for path in INPUTS if path.suffix == '.org']


# todo ok, upid is kinda irrelevant, only useful for comments
# not sure what to do...
for (url, ) in [
        ('takeout_data_gone'  ,),
        ('grasp'              ,),
        ('heartbeats_vs_kcals',),
        ('exercise_bike_model',),
        # ('hpi'              ,),
]:
    METAS.append((url, build.Post(
        title='',
        summary='',
        body='',
        upid='',
        draft=False,
        has_math=False,
        tags=(),
        feed=True,
        url='',
        date=datetime.min,
        special=False,
    )))


EXISTING = {x: m for x, m in METAS}

def meta(name: str):
    u = name
    # TODO meeeh. pretty horrible
    if u not in EXISTING:
        x = u.replace('_', '-')
        if x in EXISTING:
            u = x
    assert u in EXISTING, u
    return u, EXISTING[u]


def upid(name: str) -> str:
    u, _ = meta(name)
    return u


# TODO MEH!
ALL_POSTS = []

# TODO later, add dates...
def P(label: str, tags: str='', **kwargs) -> Node:
    def make_label(self_, label=label, tags=tags) -> str:
        u, m = meta(self_.name)
        mtags = '' if len(tags) == 0 else f'<td align="right">{tags}</td>'
        mdraft = '🚧wip🚧' if m.draft else ''
        # https://graphviz.gitlab.io/_pages/doc/info/shapes.html#html ok this is a good guide
        label = f'''
<table border="0">
<tr><td colspan="2" href="http://beepb00p.xyz/{u}.html">{label}</td></tr>
<tr><td align="left" >{mdraft}</td>{mtags}</tr>
<tr><td align="left" href="#{u}" color="yellow" tooltip="Show connections">🔍</td><td align="right">{m.date_human}</td></tr>
</table>
'''.strip()
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
grasp       = P('Grasp', tags='#orgmode')
promnesia   = P('Promnesia:<br/>journey in fixing browser history', tags='#pkm #promnesia')
# aaaa = promnesia # TODO shit, this is a bit unfortunate...
hpi         = P('HPI<br/>(Human Programming Interface)')
sad_infra   = P('The sad state of personal data and infrastructure')
exports     = P('Building data liberation infrastructure')
# TODO would be nice to display tags in boxes or something?
mypy_error_handling = P('Using mypy for error handling', tags='#python #mypy #plt')
pkm_setup   = P('How to cope with a fleshy human brain', tags='#pkm')

exercise_bike_model  = P("How I found my exercise bike to violate laws of physics", tags='#quantifiedself')
heartbeats_vs_kcals = P("Making sense of Endomondo's calorie estimation", tags='#quantifiedself')
cloudmacs   = P('Cloudmacs', tags='#emacs')
pkm_todos   = P('My GTD setup', tags='#gtd #orgmode')
scheduler   = P('In search of a better job scheduler')
configs_suck= P('Your configs suck? Try a real programming language')
unnecessary_db  = P('Against unnecessary databases')
orger           = P('Orger:<br/>reflect your life in org-mode')
my_data     = P('What data on myself I collect and why?')
orger_todos     = P('Using Orger for processing infromation')
myinfra_roam= P('Extending my personal infrastructure:<br/>Roam Research', tags='#emacs #orgmode')
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

  node[shape=box]
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
        medge(unnecessary_db, hpi),
        medge(unnecessary_db, my_data),
        medge(unnecessary_db, hpi),
        name='aux',
        label='Tools/Libraries',
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
    ),

    cluster(
        heartbeats_vs_kcals,
        exercise_bike_model,

        *order(heartbeats_vs_kcals, exercise_bike_model),

        name='qs',
        label='Quantified self',
    ),

    medge(grasp            , pkm_setup  ),
    medge(takeout_data_gone, my_data    ),
    medge(annotating       , pkm_setup  ),
    medge(cloudmacs        , pkm_setup  ),
    medge(pkm_search       , pkm_setup  ),
    medge(orger            , orger_todos),
    medge(promnesia        , pkm_setup, **noconstraint),
    medge(orger            , pkm_setup, **noconstraint),
    medge(orger_todos      , pkm_setup, **noconstraint),
    medge(exports          , hpi),
    medge(my_data          , hpi),
    medge(hpi              , exercise_bike_model, **noconstraint),
    medge(hpi              , heartbeats_vs_kcals, **noconstraint),
    medge(pkm_todos        , pkm_setup),
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


STYLE = '''
.node.hl polygon {
   stroke: red;
}

.edge.hl path {
   stroke: red;
}
.edge.hl polygon {
   stroke: red;
   fill: red;
}
'''


# absolutely mental, but I like the idea of using just CSS for it...
def node_css(node: Node) -> str:
    cls = upid(node.name)
    # todo use fill for polygon for arrow heads?
    return f'''
g.{cls}:target polygon,
g.{cls}:target ~ .{cls}.edge path,
g.{cls}:target ~ .{cls}.edge polygon {{stroke: red;}}
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
