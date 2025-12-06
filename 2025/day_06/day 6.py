from lib import *

if __name__ == "__main__":
    for part in [part_1, part_2]:
        print(part.__name__, ":", part(Path("input.txt")))