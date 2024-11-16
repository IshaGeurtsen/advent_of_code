import argparse
import sys
import pathlib
import subprocess
import typing

class Parser(argparse.ArgumentParser):
    def __init__(self):
        super().__init__("runner.py")
        self.add_argument("script")
        self.add_argument("day", type=pathlib.Path)
        self.add_argument("input", type=pathlib.Path)
        self.add_argument("--debug", action="store_true", default=False)

def main() -> typing.Never:
    args = Parser().parse_args(sys.argv)
    sys.exit(subprocess.call([sys.executable, "-m", args.day, args.input] + ["--debug"] * args.debug))
    

if __name__ == "__main__":
    main()
