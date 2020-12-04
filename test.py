#!/usr/bin/env python3
import subprocess

def test() -> None:
    subprocess.check_call([
        'src/build.py',
        '--input' , 'testdata/input' ,
        '--output', 'testdata/output',
    ])
