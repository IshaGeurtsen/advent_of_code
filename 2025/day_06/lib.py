from pathlib import *
from io import *
from dataclasses import *
from string import *
from enum import *
from functools import *
from operator import *


class OperationType(Enum):
    mul = "*"
    add = "+"


@dataclass
class Item:
    buffer: str
    value: int | OperationType
    row: int
    col: int
    def __lt__(self, other: "Item"):
        if self.row < other.row:
            return True
        elif self.row == other.row and self.col < other.col:
            return True
        else:
            return False

@dataclass
class Padding:
    row: int
    col: int


class Model:
    def __init__(self, items: list[Item]):
        self.items = items

    @classmethod
    def load(cls, file: TextIOBase):
        items: list[Item] = []
        buffer: list[str] = []
        row = 0
        col = 0
        for c in file.read():
            if c in set(digits):
                buffer.append(c)
            elif c == " " and buffer:
                items.append(Item(
                    "".join(buffer),
                    int("".join(buffer)),
                    row,
                    col
                ))
                col += len(buffer) + 1
                buffer.clear()
            elif c == " " and not buffer:
                col += 1
            elif c == "\n" and buffer:
                items.append(Item(
                    "".join(buffer),
                    int("".join(buffer)),
                    row,
                    col
                ))
                row += 1
                col = 0
                buffer.clear()
            elif c in {*"*+"} and not buffer:
                items.append(Item(
                    c,
                    OperationType(c),
                    row,
                    col
                ))
                col += 1
            elif c == "\n" and not buffer:
                row += 1
                col = 0
            else:
                raise NotImplementedError(c, buffer)
        assert not buffer
        return cls(items)

    def __str__(self):
        buffer: list[str] = []
        prev: Item | None = None
        for item in self.items:
            if prev is None:
                buffer.append(item.buffer)
            elif prev.row == item.row:
                buffer.append(" "*(item.col-(prev.col+len(prev.buffer))))
                buffer.append(item.buffer)
            elif prev.row != item.row:
                buffer.append("\n")
                buffer.append(" "*item.col)
                buffer.append(item.buffer)
            else:
                raise NotImplementedError(item, prev)
            prev = item
        buffer.append("\n")
        return "".join(buffer)

    def get_col(self, target: int, get_item: bool = False):
        row = 0
        col = 0
        values: list[int | OperationType] = []
        items: list[Item] = []
        for item in self.items:
            if item.row > row:
                row = item.row
                col = 0
            if col == target:
                items.append(item)
                values.append(item.value)
            col += 1
        if get_item:
            return items
        return values

    def get_cols(self, get_item: bool = False):
        cols: list[list[int|OperationType]] = []
        col = 0
        while column := self.get_col(col, get_item=get_item):
            cols.append(column)
            col += 1
        return cols

    def evaluate(self, target: int | None = None):
        problems: list[list[int|OperationType]]
        if target is None:
            problems = self.get_cols()
        else:
            problems = [self.get_col(target)]
        solutions = []
        for problem in problems:
            op = problem.pop()
            if not isinstance(op, OperationType):
                raise TypeError
            values = []
            for value in problem:
                if not isinstance(value, int):
                    raise TypeError
                values.append(value)
            if op is OperationType.mul:
                solutions.append(reduce(mul, values))
            elif op is OperationType.add:
                solutions.append(reduce(add, values))
            else:
                raise NotImplementedError
        return sum(solutions)

    def rotate(self):
        cols = self.get_cols(True)
        rows: list[list[int|OperationType]] = []
        for col in cols:
            col_min = min(item.col for item in col)
            col_max = max(item.col+len(item.buffer) for item in col)
            row = []
            for i in range(col_min, col_max):
                buffer: list[str] = []
                for item in col:
                    if not isinstance(item.value, int):
                        continue
                    if i in range(item.col, item.col+len(item.buffer)):
                        buffer.append(item.buffer[i - item.col])
                row.append(int("".join(buffer)))
            for item in col:
                if isinstance(item.value, int):
                    continue
                row.append(item.value)
            assert sum(1 for i in row if isinstance(i, OperationType)) == 1
            rows.append(row)
        self.items.clear()
        col_max = 0
        rows.sort(key=len, reverse=True)
        for i, row in enumerate(rows):
            col_min = col_max
            for j, value in enumerate(row):
                if isinstance(value, int):
                    buffer = list(str(value))
                else:
                    buffer = list(value.value)
                    assert j == len(row) - 1
                col_max = max(col_max, col_min + len(buffer))
                self.items.append(Item("".join(buffer), value, col=col_min, row=j))
            col_max += 1
        self.items.sort()


def part_1(path: Path):
    with path.open() as file:
        model = Model.load(file)
    return model.evaluate()

def part_2(path: Path):
    with path.open() as file:
        model = Model.load(file)
        model.rotate()
    return model.evaluate()
