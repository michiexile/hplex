#!/usr/bin/python
# -*- coding: utf-8 -*-

import numpy as np
import matplotlib.pyplot as plt
import sys

def main():
    v = dict()
    f = open("points.txt", "r")
    i = 0
    for line in f:
        splitline = line.split(" ")
        v[i] = np.array([float(splitline[0]), float(splitline[1])])
        i = i + 1
    f.close()

    t = np.arange(0, 2*np.pi, 0.001)

    fig0 = plt.figure()
    fig1 = plt.figure()
    fig2 = plt.figure()
    fig = plt.figure()
    ax0 = fig0.add_subplot(111, aspect='equal')
    ax1 = fig1.add_subplot(111, aspect='equal')
    ax2 = fig2.add_subplot(111, aspect='equal')
    ax = fig.add_subplot(111, aspect='equal')
    f = open("solution.txt", "r")
    for line in f:
        splitline = line.strip().split(" ")
        if len(splitline) == 3:
            ax2.fill([ v[int(splitline[0])][0] , v[int(splitline[1])][0], v[int(splitline[2])][0], v[int(splitline[0])][0] ],
                    [ v[int(splitline[0])][1] , v[int(splitline[1])][1], v[int(splitline[2])][1], v[int(splitline[0])][1] ],
                    color='#8080ff', linestyle='solid', lw=1, facecolor='blue')

            ax.fill([ v[int(splitline[0])][0] , v[int(splitline[1])][0], v[int(splitline[2])][0], v[int(splitline[0])][0] ],
                    [ v[int(splitline[0])][1] , v[int(splitline[1])][1], v[int(splitline[2])][1], v[int(splitline[0])][1] ],
                    color='#8080ff', linestyle='solid', lw=1, facecolor='blue')
        elif len(splitline) == 2:
            ax1.plot([ v[int(splitline[0])][0], v[int(splitline[1])][0] ],  
                     [ v[int(splitline[0])][1] , v[int(splitline[1])][1] ],
                     color='green')

            ax.plot([ v[int(splitline[0])][0], v[int(splitline[1])][0] ],  
                    [ v[int(splitline[0])][1] , v[int(splitline[1])][1] ],
                    color='green')
        elif len(splitline) == 1:
            ax0.plot([v[int(splitline[0])][0]], [v[int(splitline[0])][1]], 'o',
                     color='red')

            ax.plot([v[int(splitline[0])][0]], [v[int(splitline[0])][1]], 'o',
                    color='red')
    f.close()

    # xmin, xmax = fig.xlim()
    # ymin, ymax = fig.ylim()
    
    # ax.xlim(xmin * 1.01, xmax * 1.01)
    # ax.ylim(ymin * 1.01, ymax * 1.01)
    
    plt.show()

if __name__ == "__main__":
    main()
