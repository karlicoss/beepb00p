#!/usr/bin/env python3
import dotpy
dotpy.init(__name__) # TODO extremely meh
# TODO could use similar trick for dron?


from dotpy import *


def P(label: str, tags: str='', **kwargs) -> Node:
#     ts = [f'<td>{t}</td>' for t in tags.split() if len(t) > 0]
#     tags_el = '' if len(ts) == 0 else f'<tr align="left">{ts}</tr>'
#     label = f'''<
# <table border="0">
# <tr><td>{label}</td></tr>
# {tags_el}
# </table>
# >'''
    if len(tags) > 0:
        label = f'{label}<br/><b>{tags}</b>'

    # todo replace \n with br?

    if '<' in label:
        # meh..
        label = f'< {label} >'

    return node(
        label=label,
        **kwargs,
    )


def order(*args, **kwargs):
    # TODO remove arrow heads
    return edges(*args, constraint='true', **invisible, **kwargs)
    # TODO disable for neato?
    # return []


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
mypy_errors = P('Using mypy for error handling', tags='#python #mypy #plt')
pkm_setup   = P('How to cope with a fleshy human brain', tags='#pkm')

bike_power  = P("How I found my exercise bike to violate laws of physics", tags='#quantifiedself')
endo_kcal   = P("Making sense of Endomondo's calorie estimation", tags='#quantifiedself')
cloudmacs   = P('Cloudmacs', tags='#emacs')
todo_lists  = P('My GTD setup', tags='#gtd #orgmode')


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
  searchsize=500

  node[shape=box]
    '''.strip(), # todo not sure how many spaces I like

    '''
    subgraph cluster_main {
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
    '}',

    'subgraph cluster_aux {',
    'edge [constraint=false]',

    '{',
    'scheduler   [label="In search of a better job scheduler"]',
    'configs_suck[label="Your configs suck? Try a real programming language"]',
    'against_db  [label="Against unnecessary databases"]',
    mypy_errors,
    *order(mypy_errors, 'against_db', 'scheduler', 'configs_suck'),
    '}',
    '''
scheduler       -> exports
scheduler       -> mydata
scheduler       -> orger
against_db      -> mydata
against_db      -> hpi
configs_suck    -> hpi
configs_suck    -> promnesia
'''.strip(),
    '}',

    cluster(
        '''
        annotating  [label="How to annotate everything"]
        pkm_search  [label="Building personal search infrastructure"]
        ''',
        todo_lists,
        cloudmacs,
        grasp,
        pkm_setup,
        # TODO needs to be date ordered?..
        *order('grasp', 'cloudmacs', 'annotating', 'todo_lists', 'pkm_search'),
        name='pkm',
        label='PKM',
    ),

    cluster(
        endo_kcal,
        bike_power,

        *order(endo_kcal, bike_power),

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

hpi             -> bike_power [constraint=false];

hpi             -> myinfra_roam;
promnesia       -> myinfra_roam [constraint=false];
orger           -> myinfra_roam;

todo_lists      -> pkm_setup;
    ''',

    # todo maybe syntax with operators or something?
    edge(hpi, promnesia),

    edge(sad_infra, promnesia),
    edge(sad_infra, hpi),
    edge(sad_infra, 'mydata'),
    edge(sad_infra, pkm_setup),

    # edge(mypy_errors, exports  , **noconstraint),
    edge(mypy_errors, hpi      , **noconstraint),
    edge(mypy_errors, promnesia, **noconstraint),
)


def main():
    print(render(G))


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
