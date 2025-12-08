import unittest
import lib


class MyTestCase(unittest.TestCase):
    def test_something(self):
        self.assertEqual(21, lib.part_1("example.txt"))


if __name__ == '__main__':
    unittest.main()
