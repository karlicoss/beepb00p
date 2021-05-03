from typing import TypeVar, Callable, Generic

_C = TypeVar('_C')
_R = TypeVar('_R')

# https://stackoverflow.com/a/5192374/706389
class classproperty(Generic[_R]):
    def __init__(self, f: Callable[[_C], _R]) -> None:
        self.f = f

    def __get__(self, obj: None, cls: _C) -> _R:
        return self.f(cls)
