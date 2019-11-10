import csv
import numpy as np
from statistics import mean, median, stdev

def stats(idx):
    with open("data/clean.csv") as f:
        reader = csv.reader(f, delimiter=";")

        time_sum = np.array([row[idx] for i, row in
            enumerate(reader) if i > 0], dtype=np.float64)

        quartile_lower = np.quantile(time_sum, 0.25)
        quartile_upper = np.quantile(time_sum, 0.75)
        print(quartile_lower, quartile_upper)
        amnt_lower = len([x for x in time_sum if x <= quartile_lower])
        amnt_upper = len([x for x in time_sum if x >= quartile_upper])
        print((amnt_lower + amnt_upper) / len(time_sum))

        print(
            "mean: ", mean(time_sum),
            " median: ", median(time_sum),
            " stdev: ", stdev(time_sum),
        )

        print(min(time_sum))
        print(max(time_sum))
        print(min(time_sum) / max(time_sum))

def stats_split():
    with open("data/clean.csv") as f:
        reader = csv.reader(f, delimiter=";")

        X = np.array([[row[0], row[-2]] for i, row in
            enumerate(reader) if i > 0], dtype=np.float64)

        upper = [x[1] for x in X if x[0] > 0.58]
        print(mean(upper), stdev(upper))
        lower = [x[1] for x in X if x[0] <= 0.58]
        print(mean(lower), stdev(lower))

        print(mean(lower) / mean(upper))

if __name__ == "__main__":
    stats_split()
    #stats(-2)
    #stats(1)
    #stats(2)
    #stats(3)
