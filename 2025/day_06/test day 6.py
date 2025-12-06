from unittest import *
from lib import *


class Serialisation(TestCase):
    def setUp(self):
        self.file = open("example.txt")

    def tearDown(self):
        self.file.close()

    def test_serialisation(self):
        model = Model.load(self.file)
        self.file.seek(0)
        self.assertEqual(self.file.read(), str(model))

class Destructuring(TestCase):
    def setUp(self):
        with open("example.txt") as file:
            self.model = Model.load(file)

    def test_col(self):
        self.assertEqual([123, 45, 6, OperationType.mul], self.model.get_col(0))
        self.assertEqual([328, 64, 98, OperationType.add], self.model.get_col(1))
        self.assertEqual([51, 387, 215, OperationType.mul], self.model.get_col(2))
        self.assertEqual([64, 23, 314, OperationType.add], self.model.get_col(3))

    def test_cols(self):
        expected = [
            [123, 45, 6, OperationType.mul],
            [328, 64, 98, OperationType.add],
            [51, 387, 215, OperationType.mul],
            [64, 23, 314, OperationType.add]]
        actual = self.model.get_cols()
        self.assertEqual(expected, actual)

class Evaluation(TestCase):
    def setUp(self):
        with open("example.txt") as file:
            self.model = Model.load(file)

    def test_evaluate_column(self):
        self.assertEqual(33210, self.model.evaluate(0))
        self.assertEqual(490, self.model.evaluate(1))
        self.assertEqual(4243455, self.model.evaluate(2))
        self.assertEqual(401, self.model.evaluate(3))

    def test_evaluate(self):
        self.assertEqual(4277556, self.model.evaluate())

class Parts(TestCase):
    def setUp(self):
        self.path = Path("example.txt")
    def test_part_1(self):
        self.assertEqual(4277556, part_1(self.path))

    def test_part_2(self):
        self.assertEqual(3263827, part_2(self.path))

class Rotate(TestCase):
    def setUp(self):
        with open("example.txt") as file:
            self.model = Model.load(file)

    def test_rotate(self):
        self.model.rotate()
        expected = [
            [1, 24, 356, OperationType.mul],
            [369, 248, 8, OperationType.add],
            [32, 581, 175, OperationType.mul],
            [623, 431, 4, OperationType.add]
        ]
        actual = self.model.get_cols()
        self.assertEqual(expected, actual)

    def test_evaluate(self):
        self.model.rotate()
        self.assertEqual(3263827, self.model.evaluate())

if __name__ == '__main__':
    unittest.main()
