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


POINTS = []
ML = []
F_land = False
lander_marker = None
lander_marker_size = 900
MAX_Y = 3000
MAX_X = 7000

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

def __getLanderMarker(r):
    lander_path = os.path.join('MarsLander_project', 'Images', 'lander.svg')
    assert(os.path.exists(lander_path))
    lander_path, attributes = svg2paths(lander_path)
    lander_marker = parse_path(attributes[0]['d'])
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

def next_round(ml, r, p):
    r1 = (r + 90) * np.pi/180
    # r1 = r * np.pi/180
    s0 = [ml[0], ml[1]]
    v0 = [ml[2], ml[3]]
    f = ml[4]
    f1 = 0
    if f > p:
        amod = p
        f1 += f - p
    elif f > 0:
        amod = f
    else:
        amod = 0
    a = [amod*np.cos(r1), amod*np.sin(r1) - 3.711]
    # a = [amod+3.711, amod]
    print('acc:',a)
    return [
        int(.5 * a[0] + v0[0] + s0[0]),     # same x as before
        int(.5 * a[1] + v0[1] + s0[1]),     # new y position
        int(a[0] + v0[0]),                  # 0 horizontal speed
        int(a[1] + v0[1]),                  # new vertical speed
        f1,                            # new amount of liters
        r,
        p
    ]

### This function return the landing status
### @retval 1  for correct landing
###         0  for in game status
###         -1 for destructive landing
def land_status(ps, ml, flatArea):
    if (ml[0] <= 0 or ml[1] <= 0) or (ml[0] >= MAX_X or ml[1] >= MAX_Y):
        return -1
    i = 0
    F_esc = False
    while i < len(ps)-1 and not F_esc:
        if ps[i][0] <= ml[0] <= ps[i+1][0] and not ps[i][1] == ps[i+1][1]: ### and not in a flatarea
            safe_alt = ((ml[0]-ps[i][0]) / (ps[i+1][0]-ps[i][0])) * (ps[i+1][1]-ps[i][1])
            if ml[1] < safe_alt + ps[i][1]:
                return -1
            else:
                F_esc = True
        i += 1
    s0 = [ml[0], ml[1]]
    v0 = [ml[2], ml[3]]
    r = ml[5]
    ### if the lander is over the flat area
    if s0[0] > flatArea[0] and s0[0] < flatArea[1]:
        ### if the lander reached the land
        if s0[1] <= flatArea[2]:
            ### if the land satisfy the landing constraints
            if r == 0 and abs(v0[0]) <= 20 and abs(v0[1]) <= 40:
                return 1
            else:
                return -1
    ### if the lander go under 0
    elif s0[1] < 0:
        return -1
    return 0

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

def main(args):
    def __animate_surface(i):
        ax.clear()
        ax.set_ylim([0, 3200])
        ax.set_xlim([0, 7000])
        ax.set_facecolor('black')
        ax.plot([p[0] for p in POINTS],
                [p[1] for p in POINTS], color='r')
        lander_marker = __getLanderMarker(ML[i][5])
        ax.scatter(ML[i][0], ML[i][1], marker=lander_marker, 
                   s=lander_marker_size, color='w')
        ax.text(0.05, 0.9, 't: '+str(i)+'\nhspeed: '+str(round(ML[i][2], 2))+'\nvspeed: '+str(round(ML[i][3], 2)),
                horizontalalignment='left', verticalalignment='center', transform=ax.transAxes,
                color='w')
        ax.text(0.75, 0.9, 'fuel: '+str(ML[i][4])+'\nrotate: '+str(ML[i][5])+'\npower: '+str(ML[i][6]),
                horizontalalignment='left', verticalalignment='center', transform=ax.transAxes,
                color='w')
        if i == len(ML)-1:
            if F_land:
                ax.text(0.05, 0.8, 'landing success!',
                        horizontalalignment='left', verticalalignment='center', transform=ax.transAxes,
                        color='g')
            else:
                ax.text(0.05, 0.8, 'landing failed!',
                        horizontalalignment='left', verticalalignment='center', transform=ax.transAxes,
                        color='r')
        ax.set_title(args.data)

    ### get the lander png image to use it as marker for plot
    # lander_marker = __getLanderMarker()

    ### read input file
    input_file = os.path.join('MarsLander_project', 'data', args.data + '.txt')
    POINTS, ML, flatArea = __readFile(input_file)

    ### parse input file
    parse(input_file)

    ### game loop
    mlander_url = os.path.join('MarsLander_project', 'modules', 'mlander.pl')
    t = 0
    F_esc = False
    while t < 1000 and not F_esc:
        print('t=',t)
        ### write game informations in the file mlander.pl
        ### read from mlander
        mlander_file = open(mlander_url, 'r')
        lines = mlander_file.readlines()
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
        mlander_file = open(mlander_url,'w')
        mlander_file.writelines(lines)
        mlander_file.close()
        print(ML[-1])

        ### make prediction
        # os.system('swipl -s .\MarsLander_project\modules\msolve.pl -g predict')
        # time.sleep(1)
        out_line = os.popen('swipl -s .\MarsLander_project\modules\msolve.pl -g predict').read().split()
        print(out_line)
        r = int(out_line[0])
        p = int(out_line[1])
        assert((abs(r - ML[-1][5]) <= 15) and (abs(p- ML[-1][6]) <= 1))
        # assert( (abs(r - ML[-1][5]) <= 15) )

        ### print results
        ML.append(next_round(ML[-1], r, p))
        lnd_status = land_status(POINTS, ML[-1], flatArea)
        print("land status", lnd_status)
        print("")
        t+=1
        if lnd_status == 1:
            F_esc = True
            F_land = True
            ML[-1][1] = flatArea[2]
        elif lnd_status == -1:
            F_land = False
            F_esc = True
    
    if not F_esc:
        F_land = False
    fig, ax = plt.subplots(1, 1)
    fig.set_size_inches(8,4)
    
    
    ani = anm(fig, __animate_surface, frames=t+1, interval=125, repeat=False)
    ani.save('marsLander.gif')
    plt.show()
    plt.close()

###

if __name__ == '__main__':
    main(parse_args())