import io
import re
from datetime import datetime, timedelta
from itertools import islice
from pathlib import Path
from subprocess import PIPE, Popen
from typing import Dict, Optional

import pytz


LINEFORMAT = re.compile(r"""(?P<ipaddress>\d{1,3}\.\d{1,3}\.\d{1,3}\.\d{1,3}) - (?P<remoteuser>.+) \[(?P<dt>\d{2}\/[a-z]{3}\/\d{4}:\d{2}:\d{2}:\d{2} (\+|\-)\d{4})\] ((\"(?P<method>.+) )(?P<url>.+)( HTTP\/[1-2]\.[0-9]")) (?P<statuscode>\d{3}) (?P<bytessent>\d+) (["](?P<referrer>(\-)|(.+))["]) (["](?P<useragent>.+)["])""", re.IGNORECASE)

DTFORMAT = '%d/%b/%Y:%H:%M:%S %z'

def parse_line(line: str) -> Optional[Dict]:
    zz = LINEFORMAT.search(line)
    if zz is None:
        return None
    dd = {**zz.groupdict()}
    dd['dt'] = datetime.strptime(dd['dt'], DTFORMAT) # type: ignore
    return dd


def iter_log(access_log: Path, columns=None, delta: Optional[timedelta]=None):
    """
    Passing columns might help saving a bit on memory...
    """
    # constructing pandas frame directly wouldn't help much too: it's still quite a bit of memory anyway..
    NOW = datetime.now(tz=pytz.utc)

    # TODO not sure if tac should grep -v just in case... how would it behave wrt backpressure?
    rlog = Popen(f'tac {access_log}', shell=True, stdout=PIPE)
    try:
        for line in io.TextIOWrapper(rlog.stdout, encoding='utf8'):
            pl = parse_line(line)
            if pl is None:
               yield {'error': line}
               continue

            dt = pl['dt']

            if delta is not None and NOW - dt > delta:
                return

            if columns is not None:
                pl = {c: pl[c] for c in columns if c in pl}
            yield pl
    finally:
        rlog.kill() # TODO terminate??


def get_dataframe(*args, **kwargs):
    import pandas as pd # type: ignore
    from itertools import islice, chain
    it = iter_log(*args, **kwargs)
    # it = islice(it, 0, 200000) # * 2)
    # it = chain(it, iter([{'error': None}])) # TODO ugh
    return pd.DataFrame(it)
    # TODO filter errors here?


# TODO FIXME full view should run with very low nice value?
