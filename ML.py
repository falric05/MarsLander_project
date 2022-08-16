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
F_land = False
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

def next_round(ml, r, p, t):
    t = 1
    s0 = [ml[0], ml[1]]
    v0 = [ml[2], ml[3]]
    a  = [0, p-3.711]
    f = ml[4]
    return [
        s0[0],                              # same x as before
        int(.5 * a[1] + v0[1] + s0[1]),     # new y position
        0,                                  # 0 horizontal speed
        int(a[1] + v0[1]),                  # new vertical speed
        f - p,                              # new amount of liters
        r,
        p
    ]

### This function return the landing status
### @retval 1  for correct landing
###         0  for in game status
###         -1 for destructive landing
def land_status(ps, ml, flatArea):
    i = 0
    F_esc = False
    while i < len(ps) and not F_esc:
        if ml[0] <= ps[i][0] and not ps[i-1][1] == ps[i][1]: ### and not in a flatarea
            safe_alt = ((ml[0]-ps[i][0]) / (ps[i-1][0]-ps[i][0])) * (ps[i-1][1]-ps[i][1])
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
        ax.scatter(ML[i][0], ML[i][1], marker=lander_marker, 
                   s=lander_marker_size, color='w')
        ax.text(0.05, 0.9, 't: '+str(i)+'\nhspeed: '+str(ML[i][2])+'\nvspeed: '+str(ML[i][3]),
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
    lander_marker = __getLanderMarker()

    ### read input file
    input_file = os.path.join('MarsLander_project', 'data', args.data + '.txt')
    POINTS, ML, flatArea = __readFile(input_file)

    ### parse input file
    parse(input_file)

    ### game loop
    mlander_url = os.path.join('MarsLander_project', 'modules', 'mlander.pl')
    t = 0
    F_esc = False
    while t < 100 and not F_esc:
        print('t=',t)
        ### write game informations in the file mlander.pl
        ### read from mlander
        input_file = open(mlander_url, 'r')
        lines = input_file.readlines()
        lines = lines[:-1]
        lines.append('lander('+str(ML[-1][0])+', ' +str(ML[-1][1])+', ' +\
            str(ML[-1][2])+', ' +str(ML[-1][3])+', ' +str(ML[-1][4])+', ' +\
                str(ML[-1][5])+', ' +str(ML[-1][6])+').')
        input_file.close()
        print(ML[-1])

        ### make prediction
        # os.system('swipl -s .\MarsLander_project\modules\msolve.pl -g predict')
        out_line = os.popen('swipl -s .\MarsLander_project\modules\msolve.pl -g predict').read().split()
        print(out_line)
        r = int(out_line[0])
        p = int(out_line[1])

        ### print results
        input_file = open(mlander_url,'w')
        input_file.writelines(lines)
        ML.append(next_round(ML[-1], r, p, t))
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
        
    fig, ax = plt.subplots(1, 1)
    fig.set_size_inches(8,4)
    
    
    ani = anm(fig, __animate_surface, frames=t+1, interval=500, repeat=False)
    ani.save('marsLander.gif')
    plt.show()
    plt.close()

###

if __name__ == '__main__':
    main(parse_args())