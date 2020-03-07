#!/usr/bin/env python3
import sys

import orgparse


def rewrite_line(line: str) -> str:
    ts = orgparse.date.TIMESTAMP_RE
    line = ts.sub(
        r'[\g<inactive_year>-\g<inactive_month>-\g<inactive_day>]',
        line,
    )
    return line


def process(ins, outs):
    for line in ins:
        line = rewrite_line(line)
        outs.write(line)


def main():
    process(sys.stdin, sys.stdout)


if __name__ == '__main__':
    main()
