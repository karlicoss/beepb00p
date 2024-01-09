# https://stackoverflow.com/a/10436851/706389
from typing import Any, Optional
from concurrent.futures import Future, Executor
class DummyExecutor(Executor):
    def __init__(self, max_workers: Optional[int]=1) -> None:
        self._shutdown = False
        self._max_workers = max_workers

    def submit(self, fn, *args, **kwargs):
        if self._shutdown:
            raise RuntimeError('cannot schedule new futures after shutdown')

        f: Future[Any] = Future()
        try:
            result = fn(*args, **kwargs)
        except KeyboardInterrupt:
            raise
        except BaseException as e:
            f.set_exception(e)
        else:
            f.set_result(result)

        return f

    def shutdown(self, wait: bool=True, **kwargs) -> None:
        self._shutdown = True
