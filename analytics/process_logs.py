from datetime import datetime
from pathlib import Path
import re



LINEFORMAT = re.compile( r"""(?P<ipaddress>\d{1,3}\.\d{1,3}\.\d{1,3}\.\d{1,3}) - (?P<remoteuser>.+) \[(?P<dt>\d{2}\/[a-z]{3}\/\d{4}:\d{2}:\d{2}:\d{2} (\+|\-)\d{4})\] ((\"(?P<method>.+) )(?P<url>.+)(http\/[1-2]\.[0-9]")) (?P<statuscode>\d{3}) (?P<bytessent>\d+) (["](?P<refferer>(\-)|(.+))["]) (["](?P<useragent>.+)["])""", re.IGNORECASE)

DTFORMAT = '%d/%b/%Y:%H:%M:%S %z'

def parse_line(line: str):
    zz = LINEFORMAT.search(line)
    if zz is None:
        return {'error': line}
    dd = {**zz.groupdict()}
    dd['dt'] = datetime.strptime(dd['dt'], DTFORMAT) # type: ignore
    return dd


def iter_log(access_log):
    with Path(access_log).open() as fo:
        for line in fo:
             yield parse_line(line)


def get_dataframe(access_log):
    import pandas as pd # type: ignore
    from itertools import islice
    it = iter_log(access_log)
    # it = islice(it, 5000)
    return pd.DataFrame(it)
    # TODO filter errors here?
