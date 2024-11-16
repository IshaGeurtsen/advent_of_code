import argparse
import contextlib
import pathlib
import sys
import traceback


class Parser:
    def __init__(self, path: pathlib.Path, stack: contextlib.ExitStack):
        self.__file = stack.enter_context(path.open("rt"))


def main(args: argparse.Namespace) -> None | int | str:
    try:
        with contextlib.ExitStack() as stack:
            parser = Parser(args.input, stack)
            raise NotImplementedError(parser)
            sys.stdout.write("part 1: {result!s}".format(result=None))
            sys.stdout.write("\n")
            sys.stdout.write("part 2: {result!s}".format(result=None))
            sys.stdout.write("\n")
            sys.stdout.flush()
    except Exception:
        if args.debug:
            raise
        return traceback.format_exc()
    else:
        return None


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("script_name")
    parser.add_argument("input", type=pathlib.Path)
    parser.add_argument("--debug", action="store_true", default=False)
    args = parser.parse_args(sys.argv)
    sys.exit(main(args))
