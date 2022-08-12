import os

def parse(url_inputFile):
    with open(url_inputFile, 'r') as f:
        lines = f.readlines()
        n_segments = int(lines[0])
        segments = []
        for i in range(1, n_segments+1, 1):
            segments.append(lines[i].split())
        lander = lines[n_segments+1].split()
        f.close()
    
    ### build minput.pl lines
    minput_lines = [
        ":- module(input, [zone/2, bl_landing_site/1, mars_zone/1, g/1]).\n",
        "\n",
        "%%% The zone is 7000m wide and 3000m high.\n",
        "zone(7000, 3000).\n",
        "\n",
        "%%% The limit for the landing site of the shuttle must be at least\n",
        "bl_landing_site(1000).\n",
        "\n",
        "%%% The game simulates a free fall without atmosphere. Gravity on Mars is 3.711 m/s².\n",
        "g(3.711).\n"
        "\n",
        "%%% Mars surface segments anchor points\n",
        "mars_zone([\n"
        ]
    for i in range(n_segments):
        line = "\tsurface("+str(segments[i][0])+", "+str(segments[i][1])+")"
        if i < n_segments - 1:
            line += ","
        line += "\n" 
        minput_lines.append(line)
    minput_lines.append("]).")

    ### store minput.pl
    minput_url = os.path.join('MarsLander_project', 'modules', 'minput.pl')
    with open(minput_url, 'w') as f:
        f.writelines(minput_lines)
        f.close()
    
    # # #

    ### construct mlander.pl
    mlander_lines = [
        ":- module(lander, [lander/7]).\n",
        "%% Start position of the lander and amount of carburants in litres\n",
        "lander("
    ]
    for i in range(len(lander)):
        mlander_lines[-1] += lander[i]
        if i < len(lander) - 1:
            mlander_lines[-1] += ", "
    mlander_lines[-1] += ").\n"

    ### store mlander.pl
    mlander_url = os.path.join('MarsLander_project', 'modules', 'mlander.pl')
    with open(mlander_url, 'w') as f:
        f.writelines(mlander_lines)
        f.close()