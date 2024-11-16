import argparse
import contextlib
import pathlib
import sys
import traceback
import functools
import typing
import operator
import logging

import parselib

@functools.total_ordering
class Count:
    class CountParser(parselib.Parser["Count"]):
        def __call__(self, text: str) -> parselib.Ok[tuple["Count", str]] | parselib.Err[str]:
            try:
                return parselib.Ok((Count(int(text)), ""))
            except ValueError as e:
                return parselib.Err(repr(e))
    def __init__(self, count: int):
        if count <= 0:
            raise ValueError
        self.count = count

    def __repr__(self) -> str:
        return repr(self.count)

    def __eq__(self, other: typing.Any) -> bool:
        if isinstance(other, Count):
            return self.count == other.count
        return NotImplemented

    def __lt__(self, other: typing.Any) -> bool:
        if isinstance(other, Count):
            return self.count < other.count
        return NotImplemented

    def __le__(self, other: typing.Any) -> bool:
        return bool(self == other or self < other)

    def __rmul__(self, other: int | typing.Any) -> int:
        if isinstance(other, int):
            return other * self.count
        return NotImplemented


class Power:
    def __init__(self, value: int = 1):
        self.value = value

    def __mul__(self, other: Count | typing.Any) -> "Power":
        if isinstance(other, Count):
            return Power(self.value * other)
        return NotImplemented

    def __add__(self, other: "Power | typing.Any") -> "Power":
        if isinstance(other, Power):
            return Power(self.value + other.value)
        return NotImplemented

    def __repr__(self) -> str:
        return f"Power<{self.value}>"


class Color:
    class ColorParser(parselib.Parser["Color"]):
        def __call__(self, color: str) -> parselib.Ok[tuple["Color", str]] | parselib.Err[str]:
            if color not in {"red", "green", "blue"}:
                return parselib.Err(f"invalid color: {color}")
            return parselib.Ok((Color(color), ""))
    def __init__(self, color: str):
        self.color = color

    def __repr__(self) -> str:
        return self.color

    def __hash__(self) -> int:
        return hash(self.color)

    def __eq__(self, other: "Color | typing.Any") -> bool:
        if isinstance(other, Color):
            return self.color == other.color
        return NotImplemented


class Cube:
    class CubeParser(parselib.Parser["Cube"]):
        def __call__(self, text: str) -> parselib.Ok[tuple["Cube", str]] | parselib.Err[str]:
            count, sep, color = text.strip().partition(" ")
            if sep != " ":
                return parselib.Err("invalid sep")
            color_result = Color.ColorParser()(color)
            if isinstance(color_result, parselib.Err):
                return color_result
            count_result = Count.CountParser()(count)
            if isinstance(count_result, parselib.Err):
                return count_result
            return parselib.Ok((Cube(count_result.value[0], color_result.value[0]), ""))


    def __init__(self, count: Count, color: Color):
        self.count = count
        self.color = color

    def __repr__(self) -> str:
        return f"{self.count} {self.color}"

    def is_possible(self, start: "Subset") -> bool:
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
            return Cube(max(self.count, other.count), self.color)
        raise ValueError

    def __rmul__(self, other: Power) -> Power:
        return other * self.count


class Subset:
    def __init__(self, cubes: list[Cube]):
        self.cubes = cubes

    def __repr__(self) -> str:
        return ", ".join(map(repr, self.cubes))

    def is_possible(self, start: "Subset") -> bool:
        return all(map(functools.partial(Cube.is_possible, start=start), self.cubes))
        raise NotImplementedError(vars())

    def __contains__(self, other: Color|typing.Any) -> bool:
        if isinstance(other, Color):
            return any(cube.match_color(other) for cube in self.cubes)
        return NotImplemented

    def __getitem__(self, other: Color | Cube) -> Cube:
        if isinstance(other, (Color, Cube)):
            for cube in self.cubes:
                if cube.match_color(other):
                    return cube
            raise KeyError
        raise TypeError

    def power(self) -> Power:
        return functools.reduce(operator.mul, self.cubes, Power())

    def merge_minimum(self, other: "Subset") -> "Subset":
        cubes: list[Cube] = []
        for cube in self.cubes:
            try:
                cubes.append(cube.merge_minimum(other[cube]))
            except KeyError:
                cubes.append(cube)
        for cube in other.cubes:
            try:
                self[cube]
            except KeyError:
                cubes.append(cube)
        result = Subset(cubes)
        logging.debug("merge %s + %s -> %s", self, other, result)
        return result


class Game:
    def __init__(self, id: int, subsets: list[Subset]):
        self.id = id
        self.subsets = subsets
    def __repr__(self) -> str:
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
    def __init__(self, games: list[Game]):
        self.games = games

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


class SubSetParser(parselib.Parser[Subset]):
    def __init__(self) -> None:
        self.cube_parser = Cube.CubeParser()
    def __call__(self, text: str) -> parselib.Ok[tuple[Subset, str]] | parselib.Err[str]:
        cube_list: list[Cube] = []
        while text:
            cube, sep, text = text.partition(", ")
            result = self.cube_parser(cube)
            if isinstance(result, parselib.Err):
                return result
            cube_list.append(result.value[0])

        return parselib.Ok((Subset(cube_list), text))

class GameParser(parselib.Parser[Game]):
    def __init__(self) -> None:
        self.subset_parser = SubSetParser()
    def __call__(self, text: str) -> parselib.Ok[tuple[Game, str]] | parselib.Err[str]:
        id, sep, subsets_s = text.removeprefix("Game ").partition(": ")
        if sep != ": ":
            raise ValueError(text)
        subsets_l: list[Subset] = []
        for subset in subsets_s.split("; "):
            result = self.subset_parser(subset)
            if isinstance(result, parselib.Err):
                return result
            if result.value[1]:
                return parselib.Err("expected empty remainder")
            subsets_l.append(result.value[0])

        return parselib.Ok((Game(int(id), subsets_l), text))

class Parser(parselib.Parser[Games]):
    def __init__(self) -> None:
        self.game_parser = GameParser()
    def __call__(self, text: str) -> parselib.Ok[tuple[Games, str]] | parselib.Err[str]:
        games = []
        while text:
            line, sep, text = text.partition("\n")
            result = self.game_parser(line)
            if isinstance(result, parselib.Ok):
                games.append(result.value[0])
            else:
                return result
        return parselib.Ok((Games(games), text))

def main(args: argparse.Namespace) -> None | int | str:
    try:
        if args.debug:
            logging.basicConfig(level=logging.DEBUG)
        with contextlib.ExitStack() as stack:
            parser = parselib.FileParser(args.input, stack, Parser)
            games = parser()
            sys.stdout.write(
                "part 1: {result!s}".format(
                    result=games.count_possible(Subset(list(map(Cube, map(Count, [12, 13, 14]), map(Color, ["red", "green", "blue"])))))
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
