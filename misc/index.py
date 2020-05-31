#!/usr/bin/env python3
import dotpy
dotpy.init(__name__) # TODO extremely meh
# TODO could use similar trick for dron?


from dotpy import *


def P(label: str, *args, **kwargs) -> Node:
    return node(
        *args,
        label=label,
        **kwargs,
    )


# TODO could make 'auto' label (or extracting from blog)
# should be lazy then too
# todo warn about unused (unrechable?)
# TODO make it possible to simply pass dict and unpack
promnesia   = P('Promnesia: journey in fixing browser history|#pkm #promnesia', **record)
hpi         = P('HPI (Human Programming Interface)')
sad_infra   = P('The sad state of personal data and infrastructure')
exports     = P('Building data liberation infrastructure')
# TODO would be nice to display tags in boxes or something?
mypy_errors = P('Using mypy for error handling|#python #mypy #plt', **record)


G = digraph(
    '''
  # ???
  # overlap=scale
  # splines=true
  ratio=fill
  size=20
  # newrank=true
  rankdir=TB
  # TODO ??? rank=min

  node[shape=box]
    '''.strip(), # todo not sure how many spaces I like

    '''
    subgraph cluster_main {
    {
grasp       [label="Grasp"]
annotating  [label="How to annotate everything"]
orger       [label="Orger: reflect your life in org-mode"]
pkm_setup   [label="How to cope with a fleshy human brain"]
mydata      [label="What data on myself I collect and why?"]
pkm_search  [label="Building personal search infrastructure"]
orger_2     [label="Using Orger for processing infromation"]
myinfra_roam[label="Extending my personal infrastructure: Roam Research|#emacs #orgmode"]
cloudmacs   [label="Cloudmacs|#emacs" shape=record]
# todo could do a table for tags?
bike_power  [label="How I found my exercise bike to violate laws of physics"]
todo_lists  [label="On TODO lists"]
takeout_removed[label="Google Takeouts silently removes old data"]
    }
''',
    promnesia,
    hpi,
    sad_infra,
    exports,
    '}',

    'subgraph cluster_yyy {',
    'edge [constraint=false]',

    '{',
    'scheduler   [label="In search of a better job scheduler"]',
    'configs_suck[label="Your configs suck? Try a real programming language"]',
    'against_db  [label="Against unnecessary databases"]',
     mypy_errors,
    'mypy_errors->against_db [constraint=true]',
    'against_db->scheduler   [constraint=true]',
    'scheduler->configs_suck [constraint=true]',
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

'''
grasp           -> pkm_setup;
takeout_removed -> mydata;
annotating      -> pkm_setup;
cloudmacs       -> pkm_setup;
pkm_search      -> pkm_setup;

orger           -> pkm_setup;
orger           -> orger_2;
orger_2         -> pkm_setup;



exports         -> hpi;
mydata          -> hpi;

hpi             -> bike_power [constraint=false];

hpi             -> myinfra_roam;
promnesia       -> myinfra_roam;
orger           -> myinfra_roam;

todo_lists      -> pkm_setup;
    ''',

    # todo maybe syntax with operators or something?
    edge(hpi, promnesia),

    edge(sad_infra, promnesia),
    edge(sad_infra, hpi),
    edge(sad_infra, 'mydata'),

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
