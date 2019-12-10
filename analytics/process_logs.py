from datetime import datetime
from pathlib import Path
import re



LINEFORMAT = re.compile( r"""(?P<ipaddress>\d{1,3}\.\d{1,3}\.\d{1,3}\.\d{1,3}) - (?P<remoteuser>.+) \[(?P<dt>\d{2}\/[a-z]{3}\/\d{4}:\d{2}:\d{2}:\d{2} (\+|\-)\d{4})\] ((\"(?P<method>.+) )(?P<url>.+)(http\/[1-2]\.[0-9]")) (?P<statuscode>\d{3}) (?P<bytessent>\d+) (["](?P<referrer>(\-)|(.+))["]) (["](?P<useragent>.+)["])""", re.IGNORECASE)

DTFORMAT = '%d/%b/%Y:%H:%M:%S %z'

def parse_line(line: str):
    zz = LINEFORMAT.search(line)
    if zz is None:
        return {'error': line}
    dd = {**zz.groupdict()}
    dd['dt'] = datetime.strptime(dd['dt'], DTFORMAT) # type: ignore
    return dd


def iter_log(access_log, columns=None):
    """ 
    Passing columns might help saving a bit on memory...
    """
    # constructing pandas frame directly wouldn't help much too: it's still quite a bit of memory anyway..
    if columns is not None:
        columns = set(['error', *columns])
    with Path(access_log).open() as fo:
        for line in fo:
             pl = parse_line(line)
             if columns is not None:
                 pl = {c: pl[c] for c in columns if c in pl}
             yield pl


def get_dataframe(*args, **kwargs):
    import pandas as pd # type: ignore
    from itertools import islice, chain
    it = iter_log(*args, **kwargs)
    # it = islice(it, 0, 200000) # * 2)
    # it = chain(it, iter([{'error': None}])) # TODO ugh
    return pd.DataFrame(it)
    # TODO filter errors here?
