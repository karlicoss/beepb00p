#!/usr/bin/env python3
import dotpy
dotpy.init(__name__) # TODO extremely meh
# TODO could use similar trick for dron?


from dotpy import *


# TODO remove later..

EXISTING = '''
myinfra-roam
configs-suck
hpi
exports
unnecessary-db
scheduler
my-data
sad-infra
exercise-bike-model
scrapyroo
pkm-search
mypy-error-handling
orger-todos
orger
cloudmacs
heartbeats_vs_kcals
annotating
pkm-setup
contemp-art
sufs
recycling-is-hard
ipynb-singleline
takeout-data-gone
quantified-mind
sleep-tracking
grasp
promnesia
kython
axol
myinfra
pkm-todos
lagrangians
wave
'''.splitlines()


# TODO later, add dates...
def P(label: str, tags: str='', **kwargs) -> Node:
    def make_label(self_, label=label, tags=tags) -> str:
        n = self_.name
        u = n
        # TODO meeeh. pretty horrible
        if u not in EXISTING:
            x = u.replace('_', '-')
            if x in EXISTING:
                u = x
        assert u in EXISTING, u
        mtags = '' if len(tags) == 0 else f'<tr><td>{tags}</td></tr>'

        # https://graphviz.gitlab.io/_pages/doc/info/shapes.html#html ok this is a good guide
        label = f'''
<table border="0">
<tr><td href="http://beepb00p.xyz/{u}.html">{label}</td></tr>
{mtags}
<tr><td href="#{u}" color="yellow"   >💡</td></tr>
</table>
'''.strip()
        label = f'< {label} >'
        return label

    return node(
        label=make_label,
        set_class=True, #todo not sure about this, use lazy property?
        id=lambda self_: self_.name,

        # TODO keep URL anyway? duno
        # URL=make_url,
        # fontcolor=INTERNAL,

        # TODO need supplemenary js?
        # TODO maybe even keep in dotpy? dunno
        **kwargs,
    )


def order(*args, **kwargs):
    return edges(*args, constraint='true', **invisible, arrowhead='none', **kwargs)
    # TODO disable for neato?
    # return []



# TODO move to core!
def medge(fr, to, **kwargs):
    assert 'class' not in kwargs
    def nodename(n: Nodish) -> str:
        if isinstance(n, str):
            return n
        else:
            return n.name

    kwargs['class'] = lambda self_: f'{nodename(fr)} {nodename(to)}'
    return edge(fr, to, **kwargs)


# TODO would be nice to add dates

# TODO could make 'auto' label (or extracting from blog)
# should be lazy then too
# todo warn about unused (unrechable?)
# TODO make it possible to simply pass dict and unpack
grasp       = P('Grasp', tags='#orgmode')
promnesia   = P('Promnesia:<br/>journey in fixing browser history', tags='#pkm #promnesia')
# aaaa = promnesia # TODO shit, this is a bit unfortunate...
hpi         = P('HPI(Human Programming Interface)')
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


length: 26

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
    '''
    {
orger       [label="Orger:\nreflect your life in org-mode"]
mydata      [label="What data on myself I collect and why?"]
orger_2     [label="Using Orger for processing infromation"]
myinfra_roam[label="Extending my personal infrastructure:\nRoam Research|#emacs #orgmode"]
# todo could do a table for tags?
takeout_removed[label="Google Takeouts silently removes old data"]
    }
''',
        promnesia,
        hpi,
        sad_infra,
        exports,
        name='main',
    ),

    'subgraph cluster_aux {',
    'edge [constraint=false]',

    '{',
    scheduler,
    configs_suck,
    unnecessary_db,
    mypy_error_handling,
    *order(mypy_error_handling, 'unnecessary_db', 'scheduler', 'configs_suck'),
    '}',
    '''
'''.strip(),
    medge('scheduler'   , 'exports'  ),
    medge('scheduler'   , 'mydata'   ),
    medge('scheduler'   , 'orger'    ),
    medge('configs_suck', 'hpi'      ),
    medge('configs_suck', 'promnesia'),
    medge('unnecessary_db'  , 'hpi'      ),
    medge('unnecessary_db'  , 'mydata'   ),
    medge('unnecessary_db'  , 'hpi'      ),
    '}',

    cluster(
        '''
        annotating  [label="How to annotate everything"]
        pkm_search  [label="Building personal search infrastructure"]
        ''',
        pkm_todos,
        cloudmacs,
        grasp,
        pkm_setup,
        # TODO needs to be date ordered?..
        *order('grasp', 'cloudmacs', 'annotating', pkm_todos, 'pkm_search'),
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

'''
grasp           -> pkm_setup;
takeout_removed -> mydata;
annotating      -> pkm_setup;
cloudmacs       -> pkm_setup;
pkm_search      -> pkm_setup;

promnesia       -> pkm_setup [constraint=false];
orger           -> pkm_setup [constraint=false];
orger_2         -> pkm_setup [constraint=false];

orger           -> orger_2;



exports         -> hpi;
mydata          -> hpi;

hpi             -> exercise_bike_model [constraint=false];

hpi             -> myinfra_roam;
promnesia       -> myinfra_roam [constraint=false];
orger           -> myinfra_roam;

pkm_todos       -> pkm_setup;
    ''',

    # todo maybe syntax with operators or something?
    medge(hpi, promnesia),

    medge(sad_infra, promnesia),
    medge(sad_infra, hpi),
    medge(sad_infra, 'mydata'),
    medge(sad_infra, pkm_setup, **noconstraint),

    # edge(mypy_error_handling, exports  , **noconstraint),
    medge(mypy_error_handling, hpi      , **noconstraint),
    medge(mypy_error_handling, promnesia, **noconstraint),
)


from typing import Optional
from pathlib import Path


STYLE = '''
.node.hl text {
   fill: red;
}

.edge.hl path {
   stroke: red;
}
.edge.hl polygon {
   stroke: red;
   fill: red;
}

'''

JS = '''
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
// TODO FIXME blink with js?
window.addEventListener('load'      , cb, false)
window.addEventListener('hashchange', cb, false)

'''

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
        <noscript>
            Javascript is required for edge highlights to work. Sorry!
        </noscript>
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

    svg = with_style(svg=svg, style=STYLE)


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
