import unittest
from unittest import TestCase as TC
import lib
from pathlib import Path
from typing import Literal

class TestDoubleCounting(TC):
    def compute_state(self, d: Literal["R", "L"], n: int, i: int|None = None):
        if i is None:
            dial = lib.Dial()
        else:
            raise NotImplementedError
        cmd = lib.Command(lib.Direction(d), n)
        dial(cmd)
        set_to = dial.count_set_to_zero()
        passes = dial.count_passes_zero()
        return set_to, passes

    def test_r1k(self):
        state = self.compute_state("R", 1_000)
        self.assertEqual(0, state[0])
        self.assertEqual(10, state[1])

    def test_r1050(self):
        state = self.compute_state("R", 1_050)
        self.assertEqual(1, state[0])
        self.assertEqual(11, state[1])

    def test_l1k(self):
        state = self.compute_state("L", 1_000)
        self.assertEqual(0, state[0])
        self.assertEqual(10, state[1])

    def test_l1050(self):
        state = self.compute_state("L", 1_050)
        self.assertEqual(1, state[0])
        self.assertEqual(11, state[1])

def fails(tc: type[TC]):
    loader = unittest.defaultTestLoader
    suite = loader.loadTestsFromTestCase(tc)
    suite.run(result:=unittest.TestResult())
    was_failure = not result.wasSuccessful()
    return was_failure

class TestParts(unittest.TestCase):
    @unittest.skipUnless(hasattr(lib, "part_1"), "part 1 is not implemented")
    def test_part_1(self):
        self.assertEqual(3, lib.part_1(Path("example.txt")))

    @unittest.skipUnless(hasattr(lib, "part_2"), "part 2 is not implemented")
    def test_part_2(self):
        self.assertEqual(6, lib.part_2(Path("example.txt")))

    @unittest.skipIf(fails(TestDoubleCounting), "requirement not met for part_2")
    @unittest.skipUnless(hasattr(lib, "part_2"), "part 2 is not implemented")
    def test_part_2_ne(self):
        known_bad_cases = {6738}
        self.assertNotIn(lib.part_2(Path("input.txt")), known_bad_cases)

class TestSerialisation(TC):
    def test_serialisation(self):
        with open("example.txt") as file:
            cookie = file.tell()
            expect = file.read()
            file.seek(cookie)
            model = lib.Model.load(file)
        self.assertEqual(expect, str(model))

class BlowByBlow(TC):
    def test_blow_by_blow(self):
        expected = [
            50,
            (0, 1),
            82,
            (0, 1),
            52,
            (1, 2),
            0,
            (1, 2),
            95,
            (1, 3),
            55,
            (2, 4),
            0,
            (2, 4),
            99,
            (3, 5),
            0,
            (3, 5),
            14,
            (3, 6),
            32
        ]
        with open("example.txt") as file:
            model = lib.Model.load(file)
            for cmd in model:
                dial = str(model._dial)
                self.assertEqual(expected.pop(0), model._dial._value)
                model(cmd)
                new_dial = str(model._dial)
                self.assertEqual(expected.pop(0), self.state(model), dial+"+"+str(cmd)+"="+new_dial)
            self.assertEqual(expected.pop(0), model._dial._value)
            self.assertListEqual([], expected)

    def state(self, model: lib.Model):
        return (
            model.count_dial_set_to_zero(),
            model.count_dial_passes_zero()
        )

if __name__ == '__main__':
    unittest.main()
