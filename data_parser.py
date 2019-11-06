import sys
import re

MATCH_DENSITY = re.compile(
    r"^rho =.*, actual density =\s+(?P<d>[0-9.E\-]+)"
)

MATCH_TIME = re.compile(
    r"^TIME (?P<type>[A-Z]+):\s+(?P<val>[0-9.E\-]+)"
)

MATCH_ITER = re.compile(
    r"^Number of changes on loop\s+(?P<iter>[0-9]+)"
)

class PercExecution:
    def __init__(self):
        self.true_density = None

        self.time_map = 0.0
        self.time_sort = 0.0
        self.time_color = 0.0
        self.time_sum = 0.0

        self.num_iter = None

    def __repr__(self):
        return "{};{};{};{};{};{}".format(
            self.true_density, self.time_map,
            self.time_sort, self.time_color,
            self.time_sum, self.num_iter
        )

    def match_density(self, line):
        s = re.match(MATCH_DENSITY, line)

        if s != None:
            self.true_density = float(s.group("d"))

    def match_times(self, line):
        s = re.match(MATCH_TIME, line)

        if s != None:
            if s.group("type") == "MAP":
                self.time_map = float(s.group("val"))
            elif s.group("type") == "SORT":
                self.time_sort = float(s.group("val"))
            elif s.group("type") == "COLOR":
                self.time_color = float(s.group("val"))

                # bad measurements
                if self.time_color < 0.0:
                    self.time_color = 0.0

            self.time_sum = ( self.time_map
                            + self.time_sort
                            + self.time_color )

    def match_iter(self, line):
        s = re.match(MATCH_ITER, line)

        if s != None:
            self.num_iter = int(s.group("iter"))

pe = None

print("DENSITY_EMPTY_CELLS;TIME_MAP;TIME_SORT;TIME_COLOR;TIME_SUM;NUM_ITER")

for i, line in enumerate(sys.stdin):
    line = line.strip()

    # reset
    if line[:4] == "SEED":
        if pe != None:
            print(pe)

        pe = PercExecution()
        continue

    elif line[:3] == "RHO":
        continue

    pe.match_density(line)
    pe.match_times(line)
    pe.match_iter(line)

print(pe)
