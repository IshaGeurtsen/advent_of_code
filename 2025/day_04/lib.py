from dataclasses import dataclass
from pathlib import Path


@dataclass(frozen=True, eq=True)
class Position:
    x: int
    y: int

    def up(self):
        return Position(self.x, self.y+1)

    def down(self):
        return Position(self.x, self.y-1)

    def left(self):
        return Position(self.x-1, self.y)

    def right(self):
        return Position(self.x+1, self.y)

    def reset_x(self):
        return Position(0, self.y)

    @classmethod
    def new(cls):
        return cls(0, 0)


class ContainsPaperRoll: pass


class AccessablePaperRoll(ContainsPaperRoll): pass


class Model:
    def __init__(self):
        self._data: dict[Position, ContainsPaperRoll | None] = {}

    def __getitem__(self, position: Position) -> ContainsPaperRoll | None:
        if position not in self._data:
            return None
        return self._data[position]

    def __setitem__(self, key: Position, value: ContainsPaperRoll|None):
        self._data[key] = value

    def __contains__(self, key: Position):
        return key in self._data

    def __str__(self):
        cursor = Position.new()
        buffer = []
        while cursor in self:
            value = self[cursor]
            if value is None:
                buffer.append(".")
            elif isinstance(value, AccessablePaperRoll):
                buffer.append("x")
            elif isinstance(value, ContainsPaperRoll):
                buffer.append("@")
            else:
                raise TypeError(value)
            cursor = cursor.right()
            if cursor not in self:
                buffer.append("\n")
                cursor = cursor.down().reset_x()
        return "".join(buffer)

    def __iter__(self):
        return iter(self._data)

    def adjacent(self, position: Position) -> list[Position]:
        cursor = position
        points: list[Position] = [
            (cursor:=cursor.up()),
            (cursor:=cursor.left()),
            (cursor:=cursor.down()),
            (cursor:=cursor.down()),
            (cursor:=cursor.right()),
            (cursor:=cursor.right()),
            (cursor:=cursor.up()),
            cursor.up()]
        return points


def load(path: Path) -> Model:
    cursor = Position.new()
    model = Model()
    with path.open(mode="rt", encoding="utf8") as file:
        for c in file.read():
            match c:
                case "@":
                    model[cursor] = ContainsPaperRoll()
                    cursor = cursor.right()
                case ".":
                    model[cursor] = None
                    cursor = cursor.right()
                case "\n":
                    cursor = cursor.down().reset_x()
                case default:
                    raise ValueError(default)
    return model


def mark_accessable(model: Model):
    for position in model:
        if model[position] is None:
            continue
        elif isinstance(model[position], ContainsPaperRoll):
            paper_roll_count = 0
            for adjacent in model.adjacent(position):
                if isinstance(model[adjacent], ContainsPaperRoll):
                    paper_roll_count += 1
            if paper_roll_count < 4:
                model[position] = AccessablePaperRoll()

def count_accessable(model: Model):
    count = 0
    for position in model:
        if isinstance(model[position], AccessablePaperRoll):
            count += 1
    return count

def part_1(path: Path):
    model = load(path)
    mark_accessable(model)
    return count_accessable(model)

def remove_accessable(model: Model):
    for position in model:
        if isinstance(model[position], AccessablePaperRoll):
            model[position] = None

def part_2(path: Path):
    model = load(path)
    mark_accessable(model)
    total = count_accessable(model)
    while count_accessable(model) > 0:
        remove_accessable(model)
        mark_accessable(model)
        total += count_accessable(model)
    return total
