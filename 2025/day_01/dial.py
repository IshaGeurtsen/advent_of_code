class Dial:
    def __init__(self, position: int = 50):
        self.position = position
        self.zero_count = 0

    def left(self, value: int):
        assert 0 < value < 100
        next_dial = self.position - value
        assert -100 < next_dial < 98
        if next_dial == 0:
            self.zero_count += 1
        elif next_dial < 0:
            self.zero_count += 1
            next_dial += 100
            assert 0 < next_dial < 100
        assert -1 < next_dial < 100
        self.position = next_dial

