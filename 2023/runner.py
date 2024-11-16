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
        self.add_argument("--pdb", action="store_true", default=False)
        self.add_argument("--mypy", action="store_true", default=False)

def main() -> typing.Never:
    args = Parser().parse_args(sys.argv)
    if args.mypy:
        cmd = [sys.executable, "-m", "mypy", args.day, "--strict"]
        try:
            subprocess.check_call(cmd)
        except subprocess.CalledProcessError as e:
            sys.exit(e.returncode)
    cmd = [sys.executable]
    if args.pdb:
        cmd.extend(["-m", "pdb"])
    cmd.extend(["-m", args.day, args.input])
    if args.debug:
        cmd.append("--debug")
    sys.exit(subprocess.call(cmd))
    

if __name__ == "__main__":
    main()
