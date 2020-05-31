#!/usr/bin/env python3
import dotpy
dotpy.init(__name__) # TODO extremely meh


from dotpy import Node, node, render, edge


def P(label: str) -> Node:
    return node(
        label=label,
    )


promnesia = P('Promnesia')
hpi       = P('HPI (Human Programming Interface)')


res = [
    promnesia,
    hpi,
    # todo maybe syntax with operators or something?
    edge(hpi, promnesia,)
]


def main():
    for r in res:
        print(render(r))


if __name__ == '__main__':
    main()

# TODO ok, so I guess I need to keep it in sync with the frontpage...
# need to declare stuff anyway.. so
# warn on mismatches
# TODO integrate with compile script
# TODO integrate with compile script to load information about posts!
# I think specifying deps etc in python is fine.. just keep separate from the blog compiler?


'''
: digraph G {
:
:   rankdir="LR";
:   overlap=scale;
:   splines = true;
:   node[shape=rectangle]
:
:
:   grasp       [label="Grasp"]
:   hpi         [label="Human Programming Interface"]
:   annotating  [label="How to annotate everything"]
:   scheduler   [label="In search of a better job scheduler"]
:   configs_suck[label="Your configs suck? Try a real programming language"]
:   against_db  [label="Against unnecessary databases"]
:   mypy_errors [label="Using mypy for error handling"]
:   orger       [label="Orger: reflect your life in org-mode"]
:   pkm_setup   [label="How to cope with a fleshy human brain"]
:   promnesia   [label="Promnesia: journey in fixing browser history"]
:   sad_infra   [label="The sad state of personal data and infrastructure"]
:   mydata      [label="What data on myself I collect and why?"]
:   pkm_search  [label="Building personal search infrastructure"]
:   orger_2     [label="Using Orger for processing infromation"]
:   myinfra_roam[label="Extending my personal infrastructure: Roam Research"]
:   cloudmacs   [label="Cloudmacs: Emacs in your browser"]
:   bike_power  [label="How I found my exercise bike to violate laws of physics"]
:   todo_lists  [label="On TODO lists"]
:   exports     [label="Building data liberation infrastructure"]
:   takeout_removed[label="Google Takeouts silently removes old data"]
:
:   grasp           -> pkm_setup;
:   takeout_removed -> mydata;
:   annotating      -> pkm_setup;
:   cloudmacs       -> pkm_setup;
:   pkm_search      -> pkm_setup;
:
:   orger           -> pkm_setup;
:   orger           -> orger_2;
:   orger_2         -> pkm_setup;
:
:
:   mypy_errors     -> hpi;
:
:   sad_infra       -> hpi;
:   sad_infra       -> mydata;
:
:   scheduler       -> exports;
:   scheduler       -> mydata;
:   scheduler       -> orger;
:   against_db      -> mydata;
:   against_db      -> hpi;
:   configs_suck    -> hpi;
:   configs_suck    -> promnesia;
:   mypy_errors     -> promnesia;
:
:   exports         -> hpi;
:   against_db      -> exports;
:   mydata          -> hpi;
:
:   hpi             -> promnesia;
:   hpi             -> bike_power;
:
:   hpi             -> myinfra_roam;
:   promnesia       -> myinfra_roam;
:   orger           -> myinfra_roam;
:
:   todo_lists      -> pkm_setup;
: }

'''
