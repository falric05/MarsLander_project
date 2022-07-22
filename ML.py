import numpy as np
from matplotlib import pyplot as plt
from matplotlib.animation import FuncAnimation as anm 
from svgpathtools import svg2paths
from svgpath2mpl import parse_path
from turtle import color
import os

from utils.args import parse_args

POINTS = []
ML = []
lander_marker = None


def __getLanderMarker():
    lander_path = os.path.join('MarsLander_project', 'Images', 'lander.svg')
    assert(os.path.exists(lander_path))
    lander_path, attributes = svg2paths(lander_path)
    lander_marker = parse_path(attributes[0]['d'])
    return lander_marker

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
        ax.scatter(ML[0], ML[1], marker=lander_marker, s=1500, color='purple')
        ax.set_title(args.data)

    file = os.path.join('MarsLander_project', 'input', args.data + '.txt')
    POINTS, ML = __readFile(file)
    lander_marker = __getLanderMarker()

    fig, ax = plt.subplots(1, 1)
    fig.set_size_inches(8,4)
    
    ani = anm(fig, __animate_surface, frames=1, interval=500, repeat=False)
    ani.save('simple_animation.gif')
    plt.show()
    plt.close()

###

if __name__ == '__main__':
    main(parse_args())