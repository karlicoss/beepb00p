from contextlib import contextmanager

@contextmanager
def tmp_popen(*args, **kwargs):
    import psutil  # type: ignore
    with psutil.Popen(*args, **kwargs) as p:
        try:
            yield p
        finally:
            for c in p.children(recursive=True):
                c.kill()
            p.kill()
            p.wait()


import bs4 # type: ignore[import]

def make_soup(x: str) -> bs4.BeautifulSoup:
    # lxml is the fastest?
    # see https://www.crummy.com/software/BeautifulSoup/bs4/doc/#installing-a-parser
    import warnings
    warnings.filterwarnings('ignore', category=bs4.XMLParsedAsHTMLWarning)
    return bs4.BeautifulSoup(x, 'lxml')
