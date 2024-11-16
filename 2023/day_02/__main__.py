import argparse
import contextlib
import pathlib
import sys
import traceback
import functools


@functools.total_ordering
class Count:
    def __init__(self, count: str):
        count_i = int(count)
        if count_i <= 0:
            raise ValueError
        self.count = count_i

    def __repr__(self):
        return repr(self.count)

    def __eq__(self, other):
        if isinstance(other, Count):
            return self.count == other.count
        return NotImplemented

    def __lt__(self, other):
        if isinstance(other, Count):
            return self.count < other.count
        return NotImplemented


class Color:
    def __init__(self, color: str):
        if color not in {"red", "green", "blue"}:
            raise ValueError(color)
        self.color = color

    def __repr__(self):
        return self.color

    def __hash__(self):
        return hash(self.color)

    def __eq__(self, other):
        if isinstance(other, Color):
            return self.color == other.color
        return NotImplemented


class Cube:
    def __init__(self, cube: str):
        count, sep, color = cube.strip().partition(" ")
        if sep != " ":
            raise ValueError
        self.count = Count(count)
        self.color = Color(color)

    def __repr__(self):
        return f"{self.count} {self.color}"

    def is_possible(self, start: dict[Color, Count]):
        return self.color in start and self.count <= start[self.color]
        raise NotImplementedError(vars())


class Subset:
    def __init__(self, subset: str):
        self.cubes: list[Cube] = []
        for cube in subset.split(", "):
            self.cubes.append(Cube(cube))

    def __repr__(self):
        return ", ".join(map(repr, self.cubes))

    def is_possible(self, start: dict[Color, Count]) -> bool:
        return all(map(functools.partial(Cube.is_possible, start=start), self.cubes))
        raise NotImplementedError(vars())


class Game:
    def __init__(self, line: str):
        self.id, sep, subsets = line.removeprefix("Game ").partition(": ")
        if sep != ": ":
            raise ValueError(line)
        self.subsets: list[Subset] = []
        for subset in subsets.split("; "):
            self.subsets.append(Subset(subset))

    def __repr__(self):
        return f"Game {self.id}: {"; ".join(map(repr, self.subsets))}"

    def get_id(self) -> int:
        return int(self.id)

    def is_possible(self, start: dict[Color, Count]) -> bool:
        return all(
            map(functools.partial(Subset.is_possible, start=start), self.subsets)
        )


class Parser:
    def __init__(self, path: pathlib.Path, stack: contextlib.ExitStack):
        self.__file = stack.enter_context(path.open("rt"))

    def parse(self) -> list[Game]:
        games: list[Game] = []
        for line in self.__file:
            games.append(Game(line))
        return games


def count_possible(games: list[Game], start: dict[Color, Count]) -> int:
    return sum(
        map(
            Game.get_id, filter(functools.partial(Game.is_possible, start=start), games)
        )
    )


def main(args: argparse.Namespace) -> None | int | str:
    try:
        with contextlib.ExitStack() as stack:
            parser = Parser(args.input, stack)
            games = parser.parse()
            sys.stdout.write(
                "part 1: {result!s}".format(
                    result=count_possible(
                        games,
                        {
                            Color("red"): Count("12"),
                            Color("green"): Count("13"),
                            Color("blue"): Count("14"),
                        },
                    )
                )
            )
            sys.stdout.write("\n")
            sys.stdout.write("part 2: {result!s}".format(result=None))
            sys.stdout.write("\n")
            sys.stdout.flush()
    except Exception:
        if args.debug:
            raise
        return traceback.format_exc()
    else:
        return None


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("script_name")
    parser.add_argument("input", type=pathlib.Path)
    parser.add_argument("--debug", action="store_true", default=False)
    args = parser.parse_args(sys.argv)
    sys.exit(main(args))
