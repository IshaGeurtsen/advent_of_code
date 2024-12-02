from pathlib import Path
from functools import partial
import operator
from itertools import starmap
from typing import Sequence

input = Path.home() / "code" / "advent_of_code" / "2024" / "day_01" / ".input"

data = list(map(tuple, map(partial(map, int), map(str.split, input.read_text().splitlines()))))
def first(t: Sequence[int]) -> int: return t[0]

def second(t: Sequence[int]) -> int: return t[1]

a = list(map(first, data))
b = list(map(second, data))

a.sort()
b.sort()

diff = list(map(abs, map(operator.sub, a, b)))

result = sum(diff)

assert result < 30659356
assert result == 1341714
print(result)

simularity_score = sum(ai*b.count(ai) for ai in a)
assert simularity_score == 27384707
print(simularity_score)
