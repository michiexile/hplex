#!/usr/bin/python

import numpy as np
import matplotlib.pyplot as plt
import sys

def main():
    n = 500
    f = open("points.txt", "w")
    i = 1
    while i <= n:
        point = np.random.randn(1,2)
	rad = np.sqrt((point[0,0] + 2)**2 + (point[0,1] + 0)**2) 
	rad2 = np.sqrt((point[0,0] - 2)**2 + (point[0,1] - 0)**2)
        if rad <= 1 :
            f.write(str(point[0,0]) + " " + str(point[0, 1]) + "\n")
            i = i+1
    f.close()

if __name__ == "__main__":
    main()
