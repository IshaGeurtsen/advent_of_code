import collections
import time
import typing
from io import TextIOBase, BufferedReader
from sys import stdout, stderr, getsizeof
from os import SEEK_CUR
from heapq import heapify, heappop, heappush

def part_1(file: TextIOBase):
    active_beams = set()
    split = 0
    for ln, line in enumerate(file):
        for cn, char in enumerate(line):
            match char:
                case ".":
                    if cn + 1 in active_beams and line[cn + 1] == "^":
                        active_beams.add(cn)
                    if cn in active_beams:
                        stdout.write("|")
                    else:
                        stdout.write(char)
                case "S":
                    active_beams.add(cn)
                    stdout.write(char)
                case "^":
                    if cn in active_beams:
                        active_beams.remove(cn)
                        active_beams.add(cn+1)
                        split += 1
                    stdout.write(char)
                case "\n":
                    stdout.write(char)
                    stdout.flush()
                case default:
                    raise NotImplementedError(vars())
    stdout.flush()
    return split

class Wave:
    def __init__(self, point: int):
        self.amplitude = collections.defaultdict(int)
        self.priority = collections.deque()
        self.amplitude[point] = 1
        self.priority.append(point)
        self.front = {point}

    def __bool__(self):
        return bool(self.priority)

    def pop(self):
        particle = self.priority.popleft()
        amplitude = self.amplitude[particle]
        self.front.remove(particle)
        return particle, amplitude

    def push(self, particle, amplitude):
        self.amplitude[particle] += amplitude
        if particle not in self.front:
            self.priority.append(particle)
            self.front.add(particle)

def part_2(file: TextIOBase):
    text = file.read()
    manifold_size = text.index("\n") + 1
    wave: Wave = Wave(text.index("S"))
    timelines = 0
    while wave:
        particle, amplitude = wave.pop()
        particle += manifold_size
        if particle not in range(len(text)):
            timelines += amplitude
            continue
        match text[particle]:
            case ".":
                wave.push(particle, amplitude)
            case "^":
                wave.push(particle-1, amplitude)
                wave.push(particle+1, amplitude)
    return timelines
