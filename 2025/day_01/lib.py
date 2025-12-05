from typing import Iterable
from dataclasses import dataclass
from io import TextIOBase as TextIO
from enum import Enum
from pathlib import Path

class Direction(Enum):
    increasing = "R"
    decreasing = "L"
    def __str__(self):
        return self.value

@dataclass
class Command:
    direction: Direction
    amount: int

    def __str__(self) -> str:
        return str(self.direction) + str(self.amount)

    def __repr__(self):
        return f"{self.direction}<{self.amount}>"

class Dial:
    def __init__(self, value: int = 50):
        self._value = value
        self.min = 0
        self.max = 99
        self.span = len(range(self.min, self.max+1))
        self._c_end_on_zero = 0
        self._c_pass_zero = 0

    def __call__(self, command: Command):
        if command.direction is Direction.decreasing:
            if self._value == 0 and command.amount > 0:
                self._c_pass_zero -= 1
            self._value -= command.amount
            while self._value < self.min:
                self._c_pass_zero += 1
                self._value += 100
            if self._value == 0:
                self._c_pass_zero += 1
        elif command.direction is Direction.increasing:
            self._value += command.amount
            while self._value > self.max:
                self._c_pass_zero += 1
                self._value -= 100
        else:
            raise ValueError
        if self._value == 0:
            self._c_end_on_zero += 1

    def count_set_to_zero(self):
        return self._c_end_on_zero

    def count_passes_zero(self):
        return self._c_pass_zero

    def __repr__(self):
        return f"Dial<{self._value}>"

class Model(Iterable[Command]):
    def __init__(self, commands: Iterable[Command]):
        self._commands = commands
        self._dial = Dial()

    @classmethod
    def load(cls, file: TextIO):
        commands: list[Command] = []
        for line in file:
            dir_ = Direction(line[0])
            amount = int(line[1:-1])
            cmd = Command(dir_, amount)
            commands.append(cmd)
        return cls(commands)

    def __str__(self):
        return "\n".join(map(str, self._commands)) + "\n"

    def __iter__(self) -> Iterable[Command]:
        return iter(self._commands)

    def __call__(self, cmd: Command):
        self._dial(cmd)

    def run(self):
        for cmd in self:
            self._dial(cmd)

    def count_dial_set_to_zero(self):
        return self._dial.count_set_to_zero()

    def count_dial_passes_zero(self):
        return self._dial.count_passes_zero()


def part_1(path: Path):
    with path.open("r+", encoding="utf8") as file:
        model = Model.load(file)
        model.run()
        return model.count_dial_set_to_zero()

def part_2(path: Path):
    with path.open("r+", encoding="utf8") as file:
        model = Model.load(file)
        model.run()
        return model.count_dial_passes_zero()
