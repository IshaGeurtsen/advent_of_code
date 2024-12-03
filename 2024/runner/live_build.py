import subprocess
import pathlib
import time
import os
import datetime
import traceback
import sys
import typing

cwd = pathlib.Path.cwd()
src = cwd / "app/Main.hs"
cabal = next(cwd.glob("day*.cabal"))
last_build_attempt: typing.Optional[datetime.datetime] = None

try:
    while True:
        most_recent_change = max(src.stat().st_mtime, cabal.stat().st_mtime)
        if (last_build_attempt is None or most_recent_change > last_build_attempt):
            print("\n---")
            try:
                print(subprocess.check_output(["cabal", "build"]).decode())
            except subprocess.CalledProcessError:
                traceback.print_exc(0)
            else:
                print(datetime.datetime.now().time())
                try:
                    print(subprocess.check_output(["cabal", "run"]).decode())
                except subprocess.CalledProcessError:
                    traceback.print_exc(0)
            finally:
                last_build_attempt = time.time()
        else:
            time.sleep(0.01)
            print(datetime.datetime.now(), end="\r")
except KeyboardInterrupt:
    sys.exit("\n"+traceback.format_exc(0))
