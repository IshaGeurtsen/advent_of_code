import argparse
import contextlib
import pathlib
import sys
import traceback
import functools
import typing
import operator
import logging


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

    def __le__(self, other):
        return self == other or self < other

    def __rmul__(self, other):
        if isinstance(other, int):
            return other * self.count
        return NotImplemented


class Power:
    def __init__(self, value: int = 1):
        self.value = value

    def __mul__(self, other):
        if isinstance(other, Count):
            return Power(self.value * other)
        return NotImplemented

    def __add__(self, other):
        if isinstance(other, Power):
            return Power(self.value + other.value)
        return NotImplemented

    def __repr__(self) -> str:
        return f"Power<{self.value}>"


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

    def is_possible(self, start: "Subset"):
        return self.color in start and self.count <= start[self.color].count

    def match_color(self, other: "Color | Cube") -> bool:
        if isinstance(other, Color):
            return self.color == other
        elif isinstance(other, Cube):
            return self.color == other.color
        else:
            raise TypeError

    def merge_minimum(self, other: "Cube") -> "Cube":
        if self.match_color(other):
            return Cube(repr(max(self.count, other.count)) + " " + repr(self.color))
        raise ValueError

    def __rmul__(self, other: Power) -> Power:
        return other * self.count


class Subset:
    def __init__(self, subset: str):
        self.cubes: list[Cube] = []
        for cube in subset.split(", "):
            self.cubes.append(Cube(cube))

    def __repr__(self):
        return ", ".join(map(repr, self.cubes))

    def is_possible(self, start: "Subset") -> bool:
        return all(map(functools.partial(Cube.is_possible, start=start), self.cubes))
        raise NotImplementedError(vars())

    def __contains__(self, other):
        if isinstance(other, Color):
            return any(cube.match_color(other) for cube in self.cubes)
        return NotImplementedError

    def __getitem__(self, other) -> Cube:
        if isinstance(other, (Color, Cube)):
            for cube in self.cubes:
                if cube.match_color(other):
                    return cube
            raise KeyError
        raise TypeError

    def power(self) -> Power:
        return functools.reduce(operator.mul, self.cubes, Power())

    def merge_minimum(self, other: "Subset"):
        cubes = []
        for cube in self.cubes:
            try:
                cubes.append(repr(cube.merge_minimum(other[cube])))
            except KeyError:
                cubes.append(repr(cube))
        for cube in other.cubes:
            try:
                self[cube]
            except KeyError:
                cubes.append(repr(cube))
        result = Subset(", ".join(cubes))
        logging.debug("merge %s + %s -> %s", self, other, result)
        return result


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

    def is_possible(self, start: Subset) -> bool:
        return all(
            map(functools.partial(Subset.is_possible, start=start), self.subsets)
        )

    def get_minimum(self) -> Subset:
        return functools.reduce(Subset.merge_minimum, self.subsets)

    def get_power(self) -> Power:
        return self.get_minimum().power()


class Games:
    def __init__(self, lines: typing.Iterable[str]):
        self.games: list[Game] = []
        for line in lines:
            self.games.append(Game(line))

    def count_possible(self, start: Subset) -> int:
        return sum(
            map(
                Game.get_id,
                filter(functools.partial(Game.is_possible, start=start), self.games),
            )
        )

    def power_cubes(self) -> Power:
        return functools.reduce(operator.add, map(Game.get_power, self.games))

    def get_minimum(self) -> Subset:
        return functools.reduce(
            Subset.merge_minimum,
            map(Game.get_minimum, self.games),
        )


def parse(path: pathlib.Path, stack: contextlib.ExitStack) -> Games:
    file = stack.enter_context(path.open("rt"))
    return Games(file)


def main(args: argparse.Namespace) -> None | int | str:
    try:
        if args.debug:
            logging.basicConfig(level=logging.DEBUG)
        with contextlib.ExitStack() as stack:
            games = parse(args.input, stack)
            sys.stdout.write(
                "part 1: {result!s}".format(
                    result=games.count_possible(Subset("12 red, 13 green, 14 blue"))
                )
            )
            sys.stdout.write("\n")
            result = games.power_cubes()
            sys.stdout.write("part 2: {result!s}".format(result=result))
            if result.value <= 8000:
                sys.stdout.write("\ntoo low!")
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
