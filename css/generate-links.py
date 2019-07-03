#!/usr/bin/env python3

SPEC = [
    (
        'file-pdf',
        '$=".pdf"',
    ),
    (
        'wikipedia',
        '*="wikipedia.org/wiki/"',
    ),
    (
        'github',
        '*="github.com"'
    ),
]

TEMPLATE = """
a[hrefLOC]::after {
    content: "";
    opacity: 0.85;

    mask-repeat: no-repeat;
    width: 0.85em;
    height: 0.75em;
    display: inline-block;
    vertical-align: top;

    margin-left: 0.1em;

    mask-image: url('../images/links/ICON.svg');
    background-color: var(--link-color);
}
a[hrefLOC]:visited::after {
    background-color: var(--visited-link-color);
}
""".lstrip()

BASE = """
/*
  Idea borrowed from Gwern https://www.gwern.net/static/css/default.css
*/


/*
    https://github.com/leungwensen/svg-icon
    TODO icons here? https://github.com/leungwensen/svg-icon/tree/master/dist/svg/simple
    https://leungwensen.github.io/svg-icon/#simple

    these look kinda cool too? https://evil-icons.io/
*/


/* sadly, that doesn't work for svg within url */
/* svg path { fill: #ff0000; } */

/*
   https://stackoverflow.com/a/46904983/706389 the mask-image trick seems to be reasonable way of coloring icons...
   not sure if base64 encoding worth it?
   one minor annoyance is that link underlining doesn't cover the icon. Gwern gets around it by using image for underlining.
*/


/* background-repeat: no-repeat; */
/* background-size: 0.75em; */
/* background-position: right 1px top; */

/*
   ugh. sadly pseudoelements can't be nested...
   https://stackoverflow.com/questions/9007546/nesting-pseudo-elements-inside-pseudo-elements/9007628#9007628
*/

""".lstrip()


def main():
    from pathlib import Path
    out = Path(__file__).parent / 'links.css'
    with out.open('w') as fo:
        fo.write(BASE)
        for icon, loc in SPEC:
            thing = TEMPLATE.replace('ICON', icon).replace('LOC', loc)
            fo.write(thing)


if __name__ == '__main__':
    main()
