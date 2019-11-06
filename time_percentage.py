import csv
import numpy as np

from fire import Fire

def print_(X, Y):
    for x, y in zip(X, Y):
        print("{:.20f},{:.20f}".format(x, y))

def percentage_(data, idx_part, idx_whole=-1):
    return data[:, idx_part] / data[:, idx_whole]

def percentage(of):
    with open("data/clean.csv") as f:
        reader = csv.reader(f, delimiter=";")

        X_ = np.array([row[0:-1]
            for i, row in enumerate(reader) if i > 0],
            dtype=np.float64
        )

        x = X_[:,0]

        color_perc = percentage_(X_, 3)

        #color_coeffs = np.polyfit(x, color_perc, 32)

        #color_perc_smoothed = np.array([sum(c * color_coeffs)
        #    for c in color_perc])

        #for x, y in zip(color_perc, color_perc_smoothed):
        #    print(x, y)

        sort_perc  = percentage_(X_, 2) + color_perc

        #sort_coeffs = np.polyfit(x, sort_perc, 12)

        #sort_perc_smoothed = np.array([sum(c * sort_coeffs)
        #    for c in sort_perc])

        #for x, y in zip(sort_perc, sort_perc_smoothed):
        #    print(x, y)

        map_perc   = percentage_(X_, 1) + sort_perc

        if   of.upper() == "MAP":
            print_(x, map_perc)
        elif of.upper() == "SORT":
            print_(x, sort_perc)
        elif of.upper() == "COLOR":
            print_(x, color_perc)
        else: return

if __name__ == "__main__":
    Fire(percentage)
