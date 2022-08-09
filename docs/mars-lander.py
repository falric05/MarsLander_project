import numpy as np
import sys

def eprint(*args, **kwargs):
    print(*args, file=sys.stderr, **kwargs)


g = -3.711
safe_dist = 30

def __getPlist(ml, srf, landl, landr):
    p = [ml]
    if ml[0] < landl[0]:
        i = 0
        while i < len(srf) and not srf[i] == landl:
            p.append([
                srf[i][0],
                srf[i][1] + safe_dist])
            i += 1
    elif ml[0] > landr[0]:
        i = len(srf) - 1
        while i > 0 and not srf[i] == landr:
            p.append([
                srf[i][0],
                srf[i][1] + safe_dist])
            i -= 1
    else:
        p.append((landl + landr) / 2)
    return p
        

# def __getk():

# def __bezier():
#     return True

def main():
    # read the number of segments for the game surface
    n = int(input())
    srf = []
    landl = []
    landr = []
    for i in range(n):
        srf.append([int(x) for x in input().split()])
        if(i  > 0) and (srf[i][1] == srf[i-1][1]):
            landr = srf[i]
            landl = srf[i-1]
    
    eprint('Left Landing Point =', landl)
    eprint('Right Landing Point =', landr)

    landing = np.zeros(2)
    while True:
        inp_line = input().split()
        ml = [int(inp_line[0]), int(inp_line[1])]
        s = [int(inp_line[2]), int(inp_line[3])]
        f = int(inp_line[4])
        r = int(inp_line[5])
        p = int(inp_line[6])
        eprint('P points', __getPlist(ml, srf, landl, landr))


		# Finding Landing Point
        t = 0
        x1 = 0
        y1 = 0
        F_esc = False
        while t < 1000 and not F_esc:
            x0 = ml[0]
            y0 = ml[1]
            sx = s[0]
            sy = s[1]
            x1 = x0 + sx*t
            y1 = y0 + sy*t + (g*t*t)/2 
            if (y1 < landr[1]):
                landing[0] = x1
                landing[1] = y1 + 1
                eprint('Landing in', t,'sec')
                F_esc = True
                break
            t += 1
        eprint('Landing at x =', x1)
        eprint('Landing at y =', y1)
        print('0 4')

if __name__ == "__main__":
    main()
