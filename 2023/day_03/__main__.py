import sys
import argparse
import logging
import contextlib
import pathlib
import parselib
import typing
import traceback

class Parser(parselib.Parser[None]):
    pass


def main(args: argparse.Namespace) -> None | int | str:
    try:
        if args.debug:
            logging.basicConfig(level=logging.DEBUG)
        with contextlib.ExitStack() as stack:
            parser = parselib.FileParser(args.input, stack, Parser)
            games = parser()
            result: typing.Any = None
            sys.stdout.write("part 1: {result!s}\n".format(result=result))
            result = None
            sys.stdout.write("part 2: {result!s}\n".format(result=result))
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
