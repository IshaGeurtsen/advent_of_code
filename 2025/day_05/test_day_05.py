from unittest import *
from lib import *

class TestSerialisation(TestCase):
    def setUp(self):
        self.path = Path("example.txt")

    def test_serialisation(self):
        with self.path.open() as file:
            text = file.read()
            file.seek(0)
            model = Model.load(file)
            self.assertIsInstance(model, Model)
            self.assertEqual(text, str(model))

class TestParts(TestCase):
    def setUp(self):
        self.example = Path("example.txt")

    def test_part_1(self):
        self.assertEqual(3, part_1(self.example))

    def test_part_2(self):
        self.assertEqual(14, part_2(self.example))


class TestSort(TestCase):
    def setUp(self):
        self.example = Path("example.txt")

    def test_sort(self):
        with open(self.example) as file:
            model = Model.load(file)
        model.sort()

if __name__ == '__main__':
    unittest.main()
