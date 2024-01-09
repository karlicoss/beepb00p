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
