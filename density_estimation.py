import csv
import numpy as np
import math

from scipy.stats import gaussian_kde, pearsonr

import matplotlib.pyplot as plt

from sklearn.linear_model import LinearRegression

def kde():
    with open("data/clean.csv") as f:
        reader = csv.reader(f, delimiter=";")

        # currently taking the amounts of iteration
        X_ = np.array([[float(row[0]), float(row[-1])]
            for i, row in enumerate(reader) if i > 0])

        kernel = gaussian_kde(X_[:,1])
        X_plot = np.linspace(0.0, max(X_[:,1]), num=1000)
        y_plot = kernel(X_plot)

        for x, y in zip(X_plot, y_plot):
            print("{},{}".format(x, y))

        #plt.hist(X_, bins=10, density=True)
        #plt.plot(X_[:,1], np.exp(logs))
        #plt.plot(X_plot, kernel(X_plot))
        #plt.show()

def lin_reg():
    with open("data/clean.csv") as f:
        reader = csv.reader(f, delimiter=";")

        X_ = np.array([[float(row[-2]), float(row[-1])]
            for i, row in enumerate(reader) if i > 0])

        X = X_[:,0].reshape(-1, 1)
        y = X_[:,1].reshape(-1, 1)

        reg = LinearRegression()
        reg.fit(X, y)
        #print("mean squared ", reg.score(X, y))
        #print("pearson r ", pearsonr(X, y)[0][0])

        p_ = np.array([[0.0], [650.0]])
        pred = reg.predict(p_)

        for x, y in zip(p_[:,0], pred[:,0]):
            print("{},{}".format(x, y))


        #plt.plot(p_[:,0], pred)
        #plt.show()

if __name__ == "__main__":
    #kde()
    lin_reg()
