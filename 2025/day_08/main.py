import sys
from typing import NamedTuple, Iterable, Callable
from dataclasses import dataclass, field
from math import sqrt


class JunctionBox(NamedTuple):
    X: int
    Y: int
    Z: int

    @classmethod
    def from_position(cls, position: str):
        return cls(*map(int, str.split(position, ",")))

    def __str__(self) -> str:
        return ",".join(map(str, self))


class Pair(NamedTuple):
    first: JunctionBox
    last: JunctionBox

    def straight_line_distance(self) -> float:
        return sqrt(sum((self.first[i] - self.last[i]) ** 2 for i in range(3)))

    def serialize(self):
        return JunctionBoxes([self.first, self.last]).serialize()


@dataclass
class JunctionBoxes:
    junction_boxes: list[JunctionBox] = field(default_factory=list[JunctionBox])

    def serialize(self) -> str:
        return "\n".join(map(str, self.junction_boxes))

    def append(self, box: JunctionBox):
        self.junction_boxes.append(box)

    def __getitem__(self, index: int):
        return self.junction_boxes[index]

    def pairs(self) -> Iterable[Pair]:
        for offset, box in enumerate(self.junction_boxes, 1):
            for other in self.junction_boxes[offset:]:
                yield Pair(box, other)

    def closest(self) -> Pair:
        return min(self.pairs(), key=Pair.straight_line_distance)

    def __iter__(self):
        return iter(self.junction_boxes)

    def extend(self, other: Iterable[JunctionBox]):
        self.junction_boxes.extend(other)

    def __len__(self):
        return len(self.junction_boxes)

    @classmethod
    def from_puzzle_input(cls, puzzle_input: Iterable[str]):
        self = cls([])
        for junction_box_position in puzzle_input:
            self.append(JunctionBox.from_position(junction_box_position))
        return self


class CircuitServer:
    def __init__(
        self, boxes: JunctionBoxes, connection_limit: int, count_skips: bool
    ) -> None:
        from operator import call

        @call
        def _():
            self.boxes = {box: id_ for id_, box in enumerate(boxes)}
            self.ids = {id_: JunctionBoxes([box]) for box, id_ in self.boxes.items()}

        self.boxes: dict[JunctionBox, int]
        self.ids: dict[int, JunctionBoxes]
        self.connection_count = 0
        self.connection_limit = connection_limit
        self.count_skips = count_skips

    def __getitem__(self, box: JunctionBox):
        return self.boxes[box]

    def connect(self, pair: Pair):
        id_a = self.boxes[pair.first]
        id_b = self.boxes[pair.last]
        id_min = min(id_a, id_b)
        id_max = max(id_a, id_b)
        # move boxes to id
        self.ids[id_min].extend(self.ids[id_max])
        # update the circuit id of the moved boxes
        for box in self.ids[id_max]:
            self.boxes[box] = id_min
        # remove the disconnected circuit id
        del self.ids[id_max]
        # update stats
        self.connection_count += 1

    def connection_count_guard[T](self, it: Iterable[T]) -> Iterable[T]:
        return CountGuard(it, lambda: self.connection_count < self.connection_limit - 1)

    def circuit_sizes(self):
        return [len(circuit) for circuit in self.ids.values()]

    def skip(self, pair: Pair):
        if self.count_skips:
            self.connection_count += 1


class CountGuard[T]:
    def __init__(self, it: Iterable[T], condition: Callable[[], bool]) -> None:
        self.it = iter(it)
        self.condition = condition

    def __next__(self):
        if self.condition():
            return next(self.it)
        raise StopIteration

    def __iter__(self):
        return self


def main():
    path = sys.argv[1]
    name = path.rpartition("/")[2].partition(".")[0]
    match name:
        case "example":
            debug = True
            limit = 10
            count_skips = False
        case "input":
            debug = False
            limit = 1000
            count_skips = True
        case _:
            raise NotImplementedError
    with open(path, "rt") as file:
        puzzel_input = file.read().splitlines()

    junction_boxes = JunctionBoxes.from_puzzle_input(puzzel_input)
    if debug:
        assert junction_boxes[0] == JunctionBox(X=162, Y=817, Z=812)

    if debug:
        assert junction_boxes.closest() == Pair(
            JunctionBox.from_position("162,817,812"),
            JunctionBox.from_position("425,690,689"),
        )

    circuits = CircuitServer(
        junction_boxes, connection_limit=limit, count_skips=count_skips
    )

    pairs = junction_boxes.pairs()
    pairs = list(pairs)
    pairs.sort(key=Pair.straight_line_distance)
    pairs = iter(pairs)
    for pair in circuits.connection_count_guard(pairs):
        if circuits[pair.first] == circuits[pair.last]:
            circuits.skip(pair)
            continue
        else:
            circuits.connect(pair)
            print("connect", pair.serialize(), sep="\n")
            print(circuits.connection_count, circuits.connection_limit, sep=" / ")

    circuit_sizes = circuits.circuit_sizes()
    circuit_sizes.sort(reverse=True)
    a, b, c = circuit_sizes[:3]
    result = a * b * c
    print(f"{result=}")
    last_connection: Pair | None = None
    for pair in pairs:
        if circuits[pair.first] == circuits[pair.last]:
            circuits.skip(pair)
        else:
            circuits.connect(pair)
            last_connection = pair
    assert last_connection is not None
    result = last_connection.first.X * last_connection.last.X
    print(f"{result=}")


if __name__ == "__main__":
    main()
