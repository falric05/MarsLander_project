import numpy as np
from matplotlib import pyplot as plt
from matplotlib.animation import FuncAnimation as anm 
from turtle import color
import os

from utils.args import parse_args

POINTS = []
ML = []


def __readFile(in_file):
    assert(os.path.exists(in_file))
    f = open(in_file, "r")
    n = int(f.readline())
    points = []
    for i in range(n):
        points.append(np.array([int(x) for x in f.readline().split()]))
    ml = np.array([int(x) for x in f.readline().split()])
    return points, ml

###

def main(args):
    def __animate_surface(i):
        ax.clear()
        ax.set_ylim([0, 3000])
        ax.set_xlim([0, 7000])
        ax.plot([p[0] for p in POINTS],
                [p[1] for p in POINTS], color='r')
        ax.scatter(ML[0], ML[1], marker='*', s=85, color='purple')

    file = os.path.join('MarsLander_project', 'input', args.data + '.txt')
    POINTS, ML = __readFile(file)

    fig, ax = plt.subplots(1, 1)
    fig.set_size_inches(8,4)
    
    ani = anm(fig, __animate_surface, frames=1, interval=500, repeat=False)
    ani.save('simple_animation.gif')
    plt.show()
    plt.close()

###

if __name__ == '__main__':
    main(parse_args())