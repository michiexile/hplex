#!/usr/bin/python

import numpy as np
import matplotlib.pyplot as plt
import sys

def main():
    n = 400
    f = open("points.txt", "w")
    i = 1
    while i <= n:
        t1 = np.random.uniform(0, 2*np.pi, 1)[0]
        t2 = np.random.uniform(0, 2*np.pi, 1)[0]
	t3 = np.random.uniform(0, 2*np.pi, 1)[0]
        #point = np.array([[np.cos(t1), np.sin(t1), np.cos(t2), np.sin(t2), np.cos(t3), np.sin(t3)]])
	point = np.array([[np.cos(t1), np.sin(t1), np.cos(t2), np.sin(t2)]])
        for j in range(0,4):
            f.write(str(point[0,j]) + " ")
        f.write("\n")
        i = i+1
    f.close()

if __name__ == "__main__":
    main()
