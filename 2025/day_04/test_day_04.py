import unittest
from pathlib import Path
import lib
from dataclasses import dataclass

class LoadAndUnload(unittest.TestCase):
    def test_serialisation(self):
        path = Path("example.txt")
        self.assertEqual(path.read_text(), str(lib.load(path)))

class LoadAndMark(unittest.TestCase):
    def test_markation(self):
        path = Path("example.txt")
        result_path = Path("example_marked.txt")
        model = lib.load(path)
        lib.mark_accessable(model)
        self.assertEqual(result_path.read_text(), str(model))

class AdjacentLocations(unittest.TestCase):
    def test_adjacent(self):
        model = lib.load(Path("example.txt"))
        k = model.adjacent(lib.Position.new().right().up())
        result = set(k)
        expected = set(lib.Position(x, y)
                       for x in range(0, 3)
                       for y in range(0, 3)
                       if x != 1 or y != 1)
        self.assertEqual(8, len(result))
        self.assertEqual(8, len(expected))
        self.assertEqual(expected, result)

class CountAccessable(unittest.TestCase):
    def test_count(self):
        path = Path("example.txt")
        model = lib.load(path)
        lib.mark_accessable(model)
        result = lib.count_accessable(model)
        self.assertEqual(13, result)

class Part1(unittest.TestCase):
    def test_part1(self):
        self.assertEqual(13, lib.part_1(Path("example.txt")))

class RemovePaper(unittest.TestCase):
    @dataclass
    class State:
        header: str
        count: int | None
        text: str

    def test_remove_paper(self):
        model = lib.load(Path("example.txt"))
        states = self.load(Path("example_removed.txt"))
        for state in states:
            self.subTest(state.header)
            self.assertEqual(state.text, str(model))
            if state.count is not None:
                self.assertEqual(state.count, lib.count_accessable(model))
            lib.remove_accessable(model)
            lib.mark_accessable(model)
        self.subTest("finally")
        self.assertEqual(0, lib.count_accessable(model))

    def load(self, path: Path):
        sections: list[tuple[str, int|None, str]] = []
        with path.open(mode="r+", encoding="utf8") as file:
            header: str|None = None
            body: list[str] = []
            for line in file:
                if line.strip():
                    if header is None:
                        header = line
                    else:
                        body.append(line)
                else:
                    sections.append((header, self.parse(header), "".join(body)))
                    header = None
                    body.clear()
            assert header and body
            sections.append((header, self.parse(header), "".join(body)))
        states = [self.State(h, n, b) for h, n, b in sections]
        return states

    def parse(self, header: str) -> int | None:
        if header == "Initial state:\n":
            return None
        else:
            num = header.strip("Remove rolls of paper:\n")
            return int(num)

class Part2(unittest.TestCase):
    def test_part_2(self):
        path = Path("example.txt")
        self.assertEqual(43, lib.part_2(path))

if __name__ == '__main__':
    unittest.main()
