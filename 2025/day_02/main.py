from pathlib import Path
from math import log10
from sys import stdout, stderr
from typing import Protocol
from time import sleep, perf_counter
from datetime import timedelta
from functools import cache

def rule_double_pattern(ranges):
    invalid_ids = []
    for r in ranges:
        for n in r:
            if int(log10(n) % 2) == 0:
                continue
            s = str(n)
            if s[:len(s)//2] == s[len(s)//2:]:
                invalid_ids.append(n)
    return invalid_ids

@cache
def get_step_sizes(n: int):
    return [
        i
        for i in range(n-1, 0, -1)
        if n % i == 0
    ]

def rule_n_pattern(ranges):
    invalid_ids = []
    for r in ranges:
        for n in r:
            s = str(n)
            step_sizes = get_step_sizes(len(s)).copy()
            if not step_sizes:
                continue
            step_size = step_sizes.pop()
            i = 0
            j = i + step_size
            while len(s) > step_size:
                if j >= len(s):
                    if i + 1 == step_size and j >= len(s):
                        invalid_ids.append(n)
                        break
                    else:
                        i += 1
                        j = i + step_size
                elif s[i] != s[j]:
                    if not step_sizes:
                        break
                    step_size = step_sizes.pop()
                    i = 0
                    j = i + step_size
                else:
                    j += step_size
    return invalid_ids

class printable(Protocol):
    def __repr__(self) -> str: ...

def error(*message: printable):
    print(*message, file=stderr)

if __name__ == "__main__":
    pstart = perf_counter()
    input = Path(__file__).with_name("input.txt")
    text = input.read_text()
    sections = text.split(",")
    ranges = []
    for section in sections:
        start, sep, end = section.partition("-")
        ranges.append(range(int(start), int(end)+1))
    part_1_answer_range = range(c:=31839939622, c+1)
    part_1 = sum(rule_double_pattern(ranges))
    if part_1 in part_1_answer_range:
        if len(part_1_answer_range) == 1:
            print("part 1 is correct")
        else:
            error("part 1 invalid check range")
    else:
        error("part 1 is incorrect")

    stdout.flush()
    stderr.flush()
    psleep = perf_counter()
    sleep(1)
    pstart += perf_counter() - psleep

    low = [9981931196, 10286834433, 41662374059-1]
    high = [596840530663003, 41662374059+1]
    part_2_answer_range = range(max(low)+1, min(high))
    part_2 = sum(rule_n_pattern(ranges))
    print(rule_n_pattern(ranges))
    if part_2 in part_2_answer_range:
        if len(part_2_answer_range) == 1:
            print("part 2 is correct")
        else:
            error("part 2 could be correct", part_2)
    else:
        error("part 2 is incorrect")
        if part_2 < part_2_answer_range[0]:
            error("part 2", part_2, "is too small", "<", part_2_answer_range[0])
        elif part_2 > part_2_answer_range[-1]:
            error("part 2", part_2, "is too big")
        else:
            error("i don't know what happend", vars())


    pend = perf_counter()

    duration = timedelta(seconds=pend - pstart)
    print("took", duration)