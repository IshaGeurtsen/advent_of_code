import pathlib
import contextlib
import operator
import io
import typing

T = typing.TypeVar("T")
U = typing.TypeVar("U")

class Ok(typing.Generic[T]):
    def __init__(self, value: T) -> None:
        super().__init__()
        self.__value = value

    @property
    def value(self) -> T:
        return self.__value

    def __repr__(self) -> str:
        name = type(self).__name__
        value = repr(self.value)
        if len(value) < 100:
            return f"{name}[{value}]"
        short_value = value[:100]
        return f"{name}[{short_value}...]"
            
class Step(typing.Generic[T], Ok[tuple[T, str]]):
    @property
    def step_value(self) -> T:
        return self.value[0]
    @property
    def remainder(self) -> str:
        return self.value[1]

    def __repr__(self) -> str:
        return f"{type(self).__name__}[{self.step_value} {len(self.remainder):+}]"

class Err(typing.Generic[T]):
    def __init__(self, error: T) -> None:
        super().__init__()
        self.__error = error
    @property
    def error(self) -> T:
        return self.__error

    def __repr__(self) -> str:
        error = repr(self.error)
        if len(error) > 100:
            error = error[:100] + "..."
        return f"{type(self).__name__}[{error}]"

class Parser(typing.Generic[T]):
    def __call__(self, text: str) -> Ok[tuple[T, str]] | Err[str]:
        import inspect
        import pathlib
        file = pathlib.PurePosixPath(pathlib.Path(inspect.getfile(type(self))).relative_to(pathlib.Path.cwd()))
        line = inspect.getsourcelines(type(self))[-1]
        return Err(f"not implemented: {type(self).__qualname__} :: ({file}:{line})")

class FileParser(typing.Generic[T]):
    def __init__(self, path: pathlib.Path, stack: contextlib.ExitStack, parser: type[Parser[T]]):
        self.__file = stack.enter_context(path.open("rt"))
        self.__parser = parser()

    def __call__(self) -> T:
        result = operator.call(self.__parser, self.__file.read())
        if isinstance(result, Ok):
            if result.value[1]:
                raise ValueError(result)
            return result.value[0]
        elif isinstance(result, Err):
            raise ValueError(result)
        else:
            raise TypeError(result)
