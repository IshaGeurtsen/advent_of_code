from pathlib import Path
from math import log10

def rule_double_pattern(ranges):
    invalid_ids = []
    for r in ranges:
        for n in r:
            if int(log10(n) % 2) == 0:
                continue
            s = str(n)
            if s[:len(s)//2] == s[len(s)//2:]:
                invalid_ids.append(n)
    return invalid_ids

if __name__ == "__main__":
    input = Path(__file__).with_name("input.txt")
    text = input.read_text()
    sections = text.split(",")
    ranges = []
    for section in sections:
        start, sep, end = section.partition("-")
        ranges.append(range(int(start), int(end)+1))
    print(31839939622==sum(rule_double_pattern(ranges)))
