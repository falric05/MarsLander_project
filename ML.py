import numpy as np
import time
import matplotlib as mpl
from matplotlib import pyplot as plt
from matplotlib.animation import FuncAnimation as anm 
from svgpathtools import svg2paths
from svgpath2mpl import parse_path
from turtle import color
import os

from utils.args import parse_args
from utils.parser import parse
from utils.storage import Storage


POINTS = []
ML = []
F_land = False
lander_marker = None
lander_marker_size = 900
MAX_Y = 3000
MAX_X = 7000

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

def __getLanderMarker(r):
    ### get the lander png image to use it as marker for plot
    lander_path = os.path.join('MarsLander_project', 'Images', 'lander.svg')
    assert(os.path.exists(lander_path))
    lander_path, attributes = svg2paths(lander_path)
    lander_marker = parse_path(attributes[0]['d'])
    ### rotate the marker according to the rotation action
    lander_marker = lander_marker.transformed(mpl.transforms.Affine2D().rotate_deg(r))
    return lander_marker

def __readFile(in_file):
    assert(os.path.exists(in_file))
    f = open(in_file, "r")
    n = int(f.readline())
    points = []
    flatArea = []
    for _ in range(n):
        points.append(np.array([int(x) for x in f.readline().split()]))
    for i in range(1, n, 1):
        if points[i-1][1] == points[i][1]:
            flatArea.append(points[i-1][0])
            flatArea.append(points[i][0])
            flatArea.append(points[i][1])
    ml = [[int(x) for x in f.readline().split()]]
    return points, ml, flatArea

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
### This function returns next game vector
def next_round(ml, r, p):
    ### converts degree in radians
    r1 = (r + 90) * np.pi/180
    ### takes position and speed of the ship
    s0 = [ml[0], ml[1]]
    v0 = [ml[2], ml[3]]
    ### takes fuel
    f = ml[4]
    #
    f1 = 0
    if f > p:           ### if the ship has still fuel
        amod = p
        f1 += f - p
    elif f > 0:
        amod = f
    else:               ### otherwise
        amod = 0
    ### decomposes the acceleration in components subtracting 
    ### mars gravity to y component
    a = [amod*np.cos(r1), amod*np.sin(r1) - 3.711]
    print('acc:',a)
    ### returns new game vector
    return [
        int(.5 * a[0] + v0[0] + s0[0]),     # new x position
        int(.5 * a[1] + v0[1] + s0[1]),     # new y position
        int(a[0] + v0[0]),                  # new horizontal speed
        int(a[1] + v0[1]),                  # new vertical speed
        f1,                                 # new amount of liters
        r,
        p
    ]

### This function return the landing status
### @retval  1  for correct landing
###          0  for in game status
###         -1  for destructive landing
def land_status(ps, ml, flatArea):
    if (ml[0] < 0 or ml[1] < 0) or (ml[0] >= MAX_X or ml[1] >= MAX_Y):
        return -1
    i = 0
    F_esc = False
    while i < len(ps)-1 and not F_esc:
        ### search the points in which the lander is located at each time t
        ### such that they don't corresponds to a flat area
        if ps[i][0] <= ml[0] <= ps[i+1][0] and not ps[i][1] == ps[i+1][1]: 
            ### compute the 
            land_y_coord = ((ml[0]-ps[i][0]) / (ps[i+1][0]-ps[i][0])) * (ps[i+1][1]-ps[i][1]) + ps[i][1]
            ### if the lander reach the land y coord 
            if ml[1] < land_y_coord:
                ### it will explode
                return -1
            else:
                ### otherwisw we can exit because it is still flying
                F_esc = True
        i += 1
    ### get position
    s0 = [ml[0], ml[1]]
    ### get speed
    v0 = [ml[2], ml[3]]
    ### get rotation
    r = ml[5]
    ### if the lander is over the flat area
    if flatArea[0] <= s0[0] <= flatArea[1]:
        ### if the lander reached the land
        if s0[1] <= flatArea[2]:
            ### if the landing satisfies the landing constraints
            if r == 0 and abs(v0[0]) <= 20 and abs(v0[1]) <= 40:
                return 1
            else:
                return -1
    return 0

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

def main(args):
    ### animate surface is used to plot a single frame of the game, taking into account
    ### the i-th ML vector line 
    def __animate_surface(i):
        ax.clear()
        ax.set_ylim([0, 3200])
        ax.set_xlim([0, 7000])
        ax.set_facecolor('black')
        ### plots the mars surface points
        ax.plot([p[0] for p in POINTS],
                [p[1] for p in POINTS], color='r')
        ### get the lander marker considering its rotation
        lander_marker = __getLanderMarker(ML[i][5])
        ### plots the marker
        ax.scatter(ML[i][0], ML[i][1], marker=lander_marker, 
                   s=lander_marker_size, color='w')
        ### plots game info
        ax.text(0.05, 0.9, 't: '+str(i)+'\nhspeed: '+str(round(ML[i][2], 2))+'\nvspeed: '+str(round(ML[i][3], 2)),
                horizontalalignment='left', verticalalignment='center', transform=ax.transAxes,
                color='w')
        ax.text(0.75, 0.9, 'fuel: '+str(ML[i][4])+'\nrotate: '+str(ML[i][5])+'\npower: '+str(ML[i][6]),
                horizontalalignment='left', verticalalignment='center', transform=ax.transAxes,
                color='w')
        ### if this is the last turn 
        if i == len(ML)-1:
            if F_land:
                ax.text(0.05, 0.8, 'landing success!',
                        horizontalalignment='left', verticalalignment='center', transform=ax.transAxes,
                        color='g')
            else:
                ax.text(0.05, 0.8, 'landing failed!',
                        horizontalalignment='left', verticalalignment='center', transform=ax.transAxes,
                        color='r')
        ### set the title to the plot
        ax.set_title(args.data)

    ### read input file
    print(Storage.data_dir)
    input_file = Storage.data_file_url(args.data)
    POINTS, ML, flatArea = __readFile(input_file)

    ### parse input file
    parse(input_file)

    ### game loop
    mlander_url = os.path.join('MarsLander_project', 'modules', 'mlander.pl')
    t = 0
    F_esc = False
    while t < 1000 and not F_esc:
        print('t=',t)

        ### read from mlander.pl
        mlander_file = open(mlander_url, 'r')
        lines = mlander_file.readlines()
        ### overwrite last line until game informations
        lines = lines[:-1]
        lines.append('lander('+\
            str(int(round(ML[-1][0], 0)))+', ' +\
            str(int(round(ML[-1][1], 0)))+', ' +\
            str(int(round(ML[-1][2], 0)))+', ' +\
            str(int(round(ML[-1][3], 0)))+', ' +\
            str(ML[-1][4])+', ' +\
            str(ML[-1][5])+', ' +\
            str(ML[-1][6])+').')
        mlander_file.close()
        ### overwrite the file mlander.pl
        mlander_file = open(mlander_url,'w')
        mlander_file.writelines(lines)
        mlander_file.close()

        print(ML[-1])

        ### make prediction
        out_line = os.popen('swipl -s .\MarsLander_project\modules\msolve.pl -g predict').read().split()

        print(out_line)

        ### extract new game actions
        r = int(out_line[0])
        p = int(out_line[1])
        ### assert consistency of new actions
        assert((abs(r - ML[-1][5]) <= 15) and (abs(p- ML[-1][6]) <= 1))

        ### compute next round
        ML.append(next_round(ML[-1], r, p))
        ### check the land status
        lnd_status = land_status(POINTS, ML[-1], flatArea)

        print("land status", lnd_status)
        print("")

        t+=1
        if lnd_status == 1:             ### if landing had success
            F_esc = True
            F_land = True
            ML[-1][1] = flatArea[2]
        elif lnd_status == -1:          ### if landing failed
            F_land = False
            F_esc = True
    
    ### if F_esc is still false it means that the lander is till flying
    ### for the sake of consistency:
    ###     not(F_esc) => not(F_land)
    if not F_esc:
        F_land = False

    fig, ax = plt.subplots(1, 1)
    fig.set_size_inches(8,4)
    
    if args.plot:
        ani = anm(fig, __animate_surface, frames=t+1, interval=500, repeat=False)
        ani.save(str(Storage.out_file_url(args.data, None).resolve()))
        plt.show()
        plt.close()

###

if __name__ == '__main__':
    main(parse_args())