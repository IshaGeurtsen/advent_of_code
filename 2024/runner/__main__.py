import argparse
from pathlib import Path
import sys
import logging
import typing
import traceback
import re
import subprocess
import shlex
import os


program_name = "2024 runner"
parser = argparse.ArgumentParser(program_name)
parser.add_argument("program", action="store")
parser.add_argument("day", type=Path, action="store")
parser.add_argument("--debug", default=logging.WARNING, const=logging.DEBUG, action="store_const")


re.compile(r'^--include "(.+)"$', re.M)

def build(day: Path):
    cmd = shlex.split(r"ghc -i./parser/Parser.hs 'day_01\main.hs'")
    logging.debug("cwd=%s", os.getcwd())
    subprocess.check_call(cmd, cwd=os.getcwd())


def run(day: Path):
    build(day)
    exe = day/"main.exe"
    subprocess.check_call(exe)
    

def main(argv) -> typing.Never:
    args = parser.parse_args(argv)
    logging.basicConfig(level=args.debug)
    try:
        run(args.day)
    except subprocess.CalledProcessError as e:
        import shlex
        sys.exit("`{cmd}` returned status {code}".format(cmd=shlex.join(e.cmd), code=e.returncode))
    except Exception as e:
        sys.exit(traceback.format_exc(0).strip())
    else:
        sys.exit(0)

if __name__ == "__main__":
    main(sys.argv)
