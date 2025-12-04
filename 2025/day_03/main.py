from pathlib import Path
from functools import partial
from sys import stderr, stdout
from typing import Callable, Any
from time import sleep, perf_counter

def after[**P, R, S](f: Callable[P, R], g: Callable[[R], S]) -> Callable[P, S]:
    def _then(*a, **k):
        return g(f(*a, **k))
    return _then

def discard[R](f: Callable[[], R]) -> Callable[[Any], R]:
    def _discarded(*_a, **_k):
        return f()
    return _discarded

log = after(
    partial(print, file=stdout, flush=True),
    discard(partial(sleep, 1)))
error = after(
    partial(print, file=stderr, flush=True),
    discard(partial(sleep, 1)))

def main(
        path: Path,
        part_1_bounds: tuple[list[int], list[int]],
        part_2_bounds: tuple[list[int], list[int]]):
    log("for", path.stem)
    text = path.read_text()
    total_output_joltage = 0
    for bank in text.splitlines():
        max_joltage = 0
        for i in range(0, len(bank)):
            for j in range(i + 1, len(bank)):
                joltage = 10 * int(bank[i]) + int(bank[j])
                max_joltage = max(max_joltage, joltage)
        total_output_joltage += max_joltage

    part_1 = total_output_joltage
    if part_1_bounds[0] and part_1 < max(part_1_bounds[0]):
        error(f"part 1 ({part_1}) is too low (<{max(part_1_bounds[0])})")
    elif part_1_bounds[1] and part_1 > min(part_1_bounds[1]):
        error(f"part 1 ({part_1}) is too high (>{min(part_1_bounds[1])}")
    elif part_1_bounds[0] and part_1_bounds[1] and 1 == len(range(max(part_1_bounds[0]) + 1, min(part_1_bounds[1]))):
        log("part 1 is correct")
    else:
        log("part 1 could be correct:", part_1)

    total_output_joltage = 0
    start = perf_counter()
    lines = text.count("\n")
    for i, bank in enumerate(text.splitlines()):
        stdout.write(f"{i} / {lines}\t{(perf_counter() - start)*lines/(i or 1) - (perf_counter()-start)}\r\n")
        stdout.flush()
        max_joltage = determine_max_joltage(bank)
        total_output_joltage += max_joltage

    part_2 = total_output_joltage
    if part_2_bounds[0] and part_2 < max(part_2_bounds[0]):
        error(f"part 2 ({part_2}) is too low (<{max(part_2_bounds[0])})")
    elif part_2_bounds[1] and part_2 > min(part_2_bounds[1]):
        error(f"part 2 ({part_2}) is too high (>{min(part_2_bounds[1])}")
    elif part_2_bounds[0] and part_2_bounds[1] and 1 == len(range(max(part_2_bounds[0]) + 1, min(part_2_bounds[1]))):
        log("part 2 is correct")
    else:
        log("part 2 could be correct:", part_2)


def determine_max_joltage(bank: str, limit: int = 12, joltage: int = 0) -> int:
    if limit <= 0:
        return joltage
    prev = 0
    max_joltage = joltage
    for i in range(len(bank)):
        cur = int(bank[i])
        if cur > prev:
            new_joltage = determine_max_joltage(bank[i+1:], limit=limit-1, joltage=joltage*10+cur)
            if new_joltage > max_joltage:
                max_joltage = new_joltage
            prev = cur
    return max_joltage

def bounds_from_exact(value: int) -> tuple[list[int], list[int]]:
    return [value - 1], [value + 1]


def adj_path(name: str):
    return Path(__file__).with_name(name)


if __name__ == "__main__":
    # noinspection PyTypeChecker
    do_example = lambda: main(adj_path("example.txt"), part_1_bounds=bounds_from_exact(357), part_2_bounds=bounds_from_exact(3121910778619))
    do_input = lambda: main(adj_path("input.txt"), part_1_bounds=bounds_from_exact(17408), part_2_bounds=([172526348882013], []))
    try:
        do_example()
    except NotImplementedError as original_error:
        try:
            do_input()
        except Exception:
            pass
        finally:
            raise original_error
    else:
        do_input()
