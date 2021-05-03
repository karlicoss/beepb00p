from typing import TypeVar, Callable, Generic

_C = TypeVar('_C')
_R = TypeVar('_R')

# https://stackoverflow.com/a/5192374/706389
class classproperty(Generic[_R]):
    def __init__(self, f: Callable[[_C], _R]) -> None:
        self.f = f

    def __get__(self, obj: None, cls: _C) -> _R:
        return self.f(cls)


from contextlib import contextmanager
@contextmanager
def tmp_popen(*args, **kwargs):
    import psutil # type: ignore
    with psutil.Popen(*args, **kwargs) as p:
        try:
            yield p
        finally:
            for c in p.children(recursive=True):
                c.kill()
            p.kill()
            p.wait()
