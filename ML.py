import numpy as np
from matplotlib import pyplot as plt
from matplotlib.animation import FuncAnimation as anm 
from svgpathtools import svg2paths
from svgpath2mpl import parse_path
from turtle import color
import os

from utils.args import parse_args
from utils.parser import parse

POINTS = []
ML = []
lander_marker = None
lander_marker_size = 900

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

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
    ml = [[int(x) for x in f.readline().split()]]
    return points, ml

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

def main(args):
    def __animate_surface(i):
        ax.clear()
        ax.set_ylim([0, 3200])
        ax.set_xlim([0, 7000])
        ax.set_facecolor('black')
        ax.plot([p[0] for p in POINTS],
                [p[1] for p in POINTS], color='r')
        ax.scatter(ML[i][0], ML[i][1], marker=lander_marker, 
                   s=lander_marker_size, color='purple')
        ax.set_title(args.data)

    ### get the lander png image to use it as marker for plot
    lander_marker = __getLanderMarker()

    ### read input file
    file = os.path.join('MarsLander_project', 'data', args.data + '.txt')
    POINTS, ML = __readFile(file)
    parse(file)
    print(ML)

    ### game loop
    mlander_url = os.path.join('MarsLander_project', 'modules', 'mlander.pl')
    for i in range(1):
        ### write game informations in the file mlander.pl
        ### read from mlander
        file = open(mlander_url, 'r')
        lines = file.readlines()
        lines = lines[:-1]
        lines.append('lander('+str(ML[-1][0])+', ' +str(ML[-1][1])+', ' +\
            str(ML[-1][2])+', ' +str(ML[-1][3])+', ' +str(ML[-1][4])+', ' +\
                str(ML[-1][5])+', ' +str(ML[-1][6])+').')
        file.close()

        ### make prediction

        ### print results
        file = open(mlander_url,'w')
        file.writelines(lines)
        
    fig, ax = plt.subplots(1, 1)
    fig.set_size_inches(8,4)
    
    ani = anm(fig, __animate_surface, frames=1, interval=500, repeat=False)
    ani.save('simple_animation.gif')
    plt.show()
    plt.close()

###

if __name__ == '__main__':
    main(parse_args())