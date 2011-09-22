#!/usr/bin/python

import numpy as np
import matplotlib.pyplot as plt
import sys

def main():
    n = 1000
    f = open("points.txt", "w")
    i = 1
    while i <= n:
        t1 = np.random.uniform(0, np.pi, 1)[0]
        t2 = np.random.uniform(0, 2*np.pi, 1)[0]
        point = np.array([[np.sin(t1)*np.cos(t2), np.sin(t1)*np.sin(t2), np.cos(t1)]])
        for j in range(0,3):
            f.write(str(point[0,j]) + " ")
        f.write("\n")
        i = i+1
    f.close()

if __name__ == "__main__":
    main()
