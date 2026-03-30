import sys, pathlib, functools, operator  # noqa


class Junction:
    def __init__(self, x: int, y: int, z: int):
        self.x = x
        self.y = y
        self.z = z

    @classmethod
    def from_str(cls, text: str):
        x, y, z = text.split(",")
        return Junction(int(x), int(y), int(z))

    def __repr__(self):
        return ",".join(map(repr, [self.x, self.y, self.z]))

    def __eq__(self, other: object):
        if isinstance(other, Junction):
            return self.x == other.x and self.y == other.y and self.z == other.z
        return NotImplemented

    def __hash__(self):
        x, y, z = self.x, self.y, self.z
        return hash((x, y, z))

    def __lt__(self, other: "Junction"):
        return (
            self.x < other.x
            or (self.x == other.x and self.y < other.y)
            or (self.x == other.x and self.y == other.y and self.z < other.z)
        )


class Circuit:
    def __init__(self, junction: Junction):
        self.junctions = {junction}
        self.parent = None

    @property
    def root(self) -> "Circuit":
        if self.parent is None:
            return self
        else:
            return self.parent.root

    def is_connected(self, other: "Circuit"):
        return self.root is other.root

    def __eq__(self, other: object):
        return isinstance(other, Circuit) and self.is_connected(other)

    def __hash__(self):
        return hash(id(self.root))

    def connect(self, other: "Circuit"):
        assert not self.is_connected(other)
        if self.parent is None and other.parent is None:
            if id(self) < id(other):
                self.parent = other
                other.junctions |= self.junctions
            else:
                other.parent = self
                self.junctions |= other.junctions

    def size(self):
        return len(self.root.junctions)


def straight_line_distance_sq(p: Junction, q: Junction):
    return pow((p.x - q.x), 2) + pow((p.y - q.y), 2) + pow((p.z - q.y), 2)


def distance(pair: tuple[Junction, Junction]):
    p, q = pair
    return straight_line_distance_sq(p, q)


def part_1(text_: str):
    junction_boxes = set(map(Junction.from_str, text_.splitlines(keepends=False)))
    circuits = {junction: Circuit(junction) for junction in junction_boxes}
    pairs = [(p, q) for p in junction_boxes for q in junction_boxes if p < q]
    pairs.sort(key=distance)
    connected_pairs = []
    for i in range(step_count):
        p, q = pair = pairs[i]
        if circuits[p].is_connected(circuits[q]):
            continue
        circuits[p].connect(circuits[q])
        if circuits[p].root.junctions == junction_boxes:
            break
        connected_pairs.append(pair)
    unique_circuits = {c.root for c in circuits.values()}
    sizes = list(map(Circuit.size, unique_circuits))
    sizes.sort(reverse=True)
    print(sizes)

    return functools.reduce(operator.mul, sizes[:3])


if __name__ == "__main__":
    target = sys.argv[1]
    target_name = pathlib.Path(target).name
    step_count: int
    if target_name == "example.txt":
        step_count = 10
    elif target_name == "input.txt":
        step_count = 1000
    with open(sys.argv[1]) as file:
        text = file.read()
        print("part 1:", part_1(text))
