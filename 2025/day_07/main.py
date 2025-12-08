from time import sleep
from sys import getdefaultencoding, argv, stdout
from os import SEEK_SET
import lib

if __name__ == "__main__":
    with open(argv[1], encoding=getdefaultencoding()) as file:
        print("part 1:", lib.part_1(file))
        file.seek(0, SEEK_SET)
        stdout.flush()
        sleep(0.1)
        try:
            print("part 2:", lib.part_2(file))
        except Exception:
            stdout.flush()
            sleep(0.1)
            raise
