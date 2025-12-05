from pathlib import Path
from io import *

def irange(start, stop):
    return range(start, stop+1)

class Model:
    def __init__(self, fresh_ingredient_ID_ranges: list[range], available_ingredient_IDs):
        self.fresh_ingredient_ID_ranges = fresh_ingredient_ID_ranges
        self.available_ingredient_IDs = available_ingredient_IDs

    def __str__(self):
        def range_to_str(r: range):
            return f"{r[0]}-{r[-1]}"
        ranges = map(range_to_str, self.fresh_ingredient_ID_ranges)
        values = map(str, self.available_ingredient_IDs)
        return "\n".join([*ranges, '', *values, ''])

    def is_fresh(self, ingredient: int):
        for range in self.fresh_ingredient_ID_ranges:
            if ingredient in range:
                return True
        return False

    def count_fresh(self):
        return len([
            ingredient
            for ingredient in self.available_ingredient_IDs
            if self.is_fresh(ingredient)
        ])

    @classmethod
    def load(cls, file: TextIOBase):
        spans: list[range] = []
        for span in file:
            if not span.strip():
                break
            spans.append(irange(*map(int, span.strip().partition("-")[::2])))
        querries: list[int] = []
        for querry in file:
            querries.append(int(querry.strip()))
        return cls(spans, querries)

    def sort(self):
        def key(r: range) -> tuple[int, ...]:
            return r.start, -r.stop
        self.fresh_ingredient_ID_ranges.sort(key=key)
        return self

    def count_span(self):
        current = None
        for span in self.fresh_ingredient_ID_ranges:
            if current is None:
                current = span
            elif current.start <= span.start and current.stop >= span.stop:
                continue
            elif current.stop == span.start:
                current = range(current.start, span.stop)
            elif current.stop < span.start:
                current = range(span.start - len(current), span.stop)
            elif span.start < current.stop < span.stop:
                current = range(current.start, span.stop)
            else:
                raise NotImplementedError(current, span, len(current), len(span))
        return len(current)

def part_1(path: Path):
    with path.open() as file:
        return Model.load(file).count_fresh()

def part_2(path: Path):
    with path.open() as file:
        return Model.load(file).sort().count_span()
