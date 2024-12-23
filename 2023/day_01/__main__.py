import argparse
import traceback
import pathlib
import contextlib
import typing
import sys
import string
import itertools
import os
import io

from parselib import Parser, FileParser, Err, Ok, Step

class CalibrationDigit:
    def __init__(self, digit: int):
        if digit < 0 or digit > 9:
            raise ValueError
        self.__digit = digit

    def as_integer(self) -> int:
        return self.__digit
    
    def __repr__(self) -> str:
        return f"'{self.__digit}"


class SpelledCalibrationDigit(CalibrationDigit):
    def __repr__(self) -> str:
        return f'"{self.__digit}'


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

    def __repr__(self) -> str:
        return type(self).__name__ + repr(self.__digits)

class CalibrationValueSum:
    def __init__(self, value: int):
        self.__value = value

    def __str__(self) -> str:
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

class DigitParser(Parser[CalibrationDigit]):
    class NotADigit(Err[str]):
        pass
    def __call__(self, text: str) -> Ok[tuple[CalibrationDigit, str]] | Err[str]:
        if text[0] in string.digits:
            return Step((CalibrationDigit(int(text[0])), text[1:]))
        return self.NotADigit(repr(text[0]))

class SpelledDigitParser(Parser[CalibrationDigit]):
    def __init__(self) -> None:
        self.digits = list(enumerate("one, two, three, four, five, six, seven, eight, and nine".replace(" and ", " ").split(", "), 1))
        self.digit_parser = DigitParser()
    def __call__(self, text: str) -> Ok[tuple[CalibrationDigit, str]] | Err[str]:
        for value, digit in self.digits:
            if text.startswith(digit):
                return Step((SpelledCalibrationDigit(value), text[1:]))
        else:
            return self.digit_parser(text)

class CharacterParser(Parser[str]):
    def __call__(self, text: str) -> Ok[tuple[str, str]] | Err[str]:
        return Step((text[0], text[1:]))

class LineParser(Parser[CalibrationValue]):
    def __init__(self) -> None:
        self.digit_parser = SpelledDigitParser()
        self.char_parser = CharacterParser()
    def __call__(self, text: str) -> Ok[tuple[CalibrationValue, str]] | Err[str]:
        line, sep, remainder = text.partition("\n")
        digits: list[CalibrationDigit] = []
        while line:
            result = self.digit_parser(line)
            if isinstance(result, Ok):
                digit, line = result.value
                digits.append(digit)
                continue
            elif isinstance(result, DigitParser.NotADigit):
                char_result = self.char_parser(line)
                if isinstance(char_result, Ok):
                    _, line = char_result.value
                else:
                    return char_result
            else:
                return result
        return Step((CalibrationValue(digits), remainder))

class Day1Parser(Parser[CalibrationValues]):
    def __init__(self) -> None:
        self.line_parser = LineParser()

    def __call__(self, text: str) -> Ok[tuple[CalibrationValues, str]] | Err[str]:
        values: list[CalibrationValue] = []
        while text:
            result = self.line_parser(text)
            if isinstance(result, Ok):
                value, text = result.value
                values.append(value)
            elif isinstance(result, Err):
                return result
            else:
                raise TypeError
        return Step((CalibrationValues(values), text))

def main(args: argparse.Namespace) -> None | int | str:
    try:
        with contextlib.ExitStack() as stack:
            parser = FileParser(args.input, stack, Day1Parser)
            calibration_values = parser()
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
