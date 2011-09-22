#!/usr/bin/python
# -*- coding: utf-8 -*-

import numpy as np
import matplotlib.pyplot as plt
import sys
import re

def main():
    if len(sys.argv) != 2:
        print 'Please provide the number of filtration levels as the sole argument'
        exit(1)

    maxFilt = int(sys.argv[1])

    dimension = dict()
    reIsDim = re.compile(r"Dimension \d+")
    reDim = re.compile(r"\d+")
    reIsBar = re.compile(r"\d+,\s*\d+")
    reBar = re.compile(r"\d+")
    reIsInfBar = re.compile(r"\d+,\s*∞")
    d = 0
    f = open("out.txt", "r")
    for line in f:
        if reIsDim.search(line):
            d = int(reDim.search(line).group())
            print 'Reading dimension %d' %(d)
            dimension[d] = []
        if reIsBar.search(line):
            res = reBar.findall(line)
            dimension[d].append((int(res[0]), int(res[1])))
        if reIsInfBar.search(line):
            res = reBar.findall(line)
            dimension[d].append((int(res[0]), -1))
    
    f.close()

    for dim, bars in dimension.items():
        fig = plt.figure()
        ax = fig.add_subplot(111)
        ax.set_title('Dimension %d' %(dim))
        i = 1
        for l, u in bars:
            if u == -1:
                u = maxFilt
            ax.plot([l, u], [i, i], linewidth=2.25)
            i = i+1
        ax.set_xlabel('Persistence level')
        ax.set_ylabel('Generator number')
        ax.set_xlim(0, maxFilt)
        ax.set_ylim(0, i + 1)
        ax.set_yticks([])
        plt.savefig('%d.pdf' %(dim))
    plt.show()
    

if __name__ == "__main__":
    main()
