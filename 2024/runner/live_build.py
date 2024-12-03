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
cabal = cwd/"day03.cabal"
build = cwd / "dist-newstyle/build/x86_64-windows/ghc-9.2.1/day03-0.1.0.0/x/day03/build/day03/day03.exe"
last_build_attempt: typing.Optional[datetime.datetime] = None

try:
    while True:
        most_recent_change = max(src.stat().st_mtime, cabal.stat().st_mtime)
        build_time = build.stat().st_mtime if build.exists() else None
        if build_time is None or most_recent_change > build_time and (last_build_attempt is None or most_recent_change > last_build_attempt):
            print("\n---")
            try:
                print(subprocess.check_output(["cabal", "build"]).decode())
            except subprocess.CalledProcessError:
                traceback.print_exc(0)
            else:
                os.utime(build)
                print(datetime.datetime.fromtimestamp(build.stat().st_mtime).time())
                try:
                    print(subprocess.check_output([str(build)]).decode())
                except subprocess.CalledProcessError:
                    traceback.print_exc(0)
            finally:
                last_build_attempt = time.time()
        else:
            time.sleep(0.01)
            print(datetime.datetime.now(), end="\r")
except KeyboardInterrupt:
    sys.exit("\n"+traceback.format_exc(0))
