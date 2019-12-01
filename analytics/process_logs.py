from pathlib import Path
import re


LINEFORMAT = re.compile( r"""(?P<ipaddress>\d{1,3}\.\d{1,3}\.\d{1,3}\.\d{1,3}) - (?P<remoteuser>.+) \[(?P<dateandtime>\d{2}\/[a-z]{3}\/\d{4}:\d{2}:\d{2}:\d{2} (\+|\-)\d{4})\] ((\"(?P<method>.+) )(?P<url>.+)(http\/[1-2]\.[0-9]")) (?P<statuscode>\d{3}) (?P<bytessent>\d+) (["](?P<refferer>(\-)|(.+))["]) (["](?P<useragent>.+)["])""", re.IGNORECASE)


def parse_line(line: str):
    zz = LINEFORMAT.search(line)
    return zz.groupdict() if zz else {'error': line}


def iter_log(access_log):
    with Path(access_log).open() as fo:
        for line in fo:
             yield parse_line(line)


def get_dataframe(access_log):
    import pandas as pd # type: ignore
    return pd.DataFrame(iter_log(access_log))
    # TODO filter errors here?
