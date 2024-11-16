import argparse
import traceback
import pathlib
import contextlib
import typing
import sys
import string
import itertools


class CalibrationDigit:
    def __init__(self, digit: int):
        if digit < 0 or digit > 9:
            raise ValueError
        self.__digit = digit

    def as_integer(self) -> int:
        return self.__digit


class SpelledCalibrationDigit(CalibrationDigit):
    pass


class CalibrationValue:
    def __init__(self, digits: list[CalibrationDigit]):
        self.__digits = digits

    def as_integer(self) -> int:
        normal_digits = list(
            itertools.filterfalse(
                lambda digit: isinstance(digit, SpelledCalibrationDigit), self.__digits
            )
        )
        return normal_digits[0].as_integer() * 10 + normal_digits[-1].as_integer()

    def as_integer_from_spelled(self) -> int:
        return self.__digits[0].as_integer() * 10 + self.__digits[-1].as_integer()


class CalibrationValueSum:
    def __init__(self, value: int):
        self.__value = value

    def __str__(self):
        return f"<CalibrationValueSum = {self.__value}>"

    @classmethod
    def sum(cls, values: typing.Iterable[CalibrationValue]) -> "CalibrationValueSum":
        return cls(sum(map(CalibrationValue.as_integer, values)))


class CalibrationValueNewSum(CalibrationValueSum):
    @classmethod
    def sum(cls, values: typing.Iterable[CalibrationValue]) -> "CalibrationValueSum":
        return cls(sum(map(CalibrationValue.as_integer_from_spelled, values)))


class CalibrationValues:
    def __init__(self, values: typing.Iterable[CalibrationValue]):
        self.__values = values

    def sum(self) -> CalibrationValueSum:
        return CalibrationValueSum.sum(self.__values)

    def new_sum(self) -> CalibrationValueSum:
        return CalibrationValueNewSum.sum(self.__values)


class Line:
    def __init__(self, line: str, /):
        self.__line = line
        self.__digits = string.digits
        self.__spelled_out_digits: dict[str, int] = {
            "zero": 0,
            "one": 1,
            "two": 2,
            "three": 3,
            "four": 4,
            "five": 5,
            "six": 6,
            "seven": 7,
            "eight": 8,
            "nine": 9,
        }

    def get_calibration_value(self) -> CalibrationValue:
        digits: list[CalibrationDigit | SpelledCalibrationDigit] = []
        for i in range(len(self.__line)):
            if self.__line[i] in self.__digits:
                digits.append(CalibrationDigit(int(self.__line[i])))
            else:
                for digit in self.__spelled_out_digits:
                    if self.__line[i:].startswith(digit):
                        digits.append(
                            SpelledCalibrationDigit(self.__spelled_out_digits[digit])
                        )
        return CalibrationValue(digits)


class Parser:
    def __init__(self, path: pathlib.Path, stack: contextlib.ExitStack):
        self.__file = stack.enter_context(path.open("rt"))

    def calibration_values(self) -> CalibrationValues:
        values: list[CalibrationValue] = []
        for line in map(Line, self.__file):
            value = line.get_calibration_value()
            values.append(value)
        return CalibrationValues(values)


def main(args: argparse.Namespace) -> None | int | str:
    try:
        with contextlib.ExitStack() as stack:
            parser = Parser(args.input, stack)
            calibration_values = parser.calibration_values()
            sys.stdout.write(
                "part 1: {result!s}".format(result=calibration_values.sum())
            )
            sys.stdout.write("\n")
            sys.stdout.write(
                "part 2: {result!s}".format(result=calibration_values.new_sum())
            )
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
