:- use_module(minput).
:- use_module(mcheck).

%:- zone(_, _), write('zone size fixed').
% +------------------------------------------+
% |                  TEST                    |
% +------------------------------------------+
testCheckSurface:-mars_zone(S), checkSurface(S).

testCheckLandingSite:-
    mars_zone(S), 
    bl_landing_site(BL), 
    checkLandingsite(S, BL, _, _),
    landing_site(X1, Y1, X, Y), write("landing site: "),
    write("("), write(X1), write(", "), write(Y1), write(") to "),
    write("("), write(X),  write(", "),  write(Y),  write(") ").

%testInput0:- checkInput(7000, 2000, 4500, _, _). 
%testInput1:- checkInput(3000, -1, 4500, _, _). 
%testInput2:- checkInput(3000, 2000, -3000, _, _).
%testInput5:- checkInput(3000, 2000, 4500, _, _).

%mars_lander([X, Y, HS, VS, F, R, P]):-
% By linking all the points together in a sequential fashion, 
% you form the surface of Mars which is composed of several segments. 
% For the first point, landX = 0 and for the last point, landX = 6999
%   - S: list of points as sequential fashion 
%   - X: x coordinate for the position of Mars Lander 
%   - Y: y coordinate for the position of Mars Lander 
%   - F: amount of fuel
checkInput:-
    lander(X, Y, F),
    checkX(X),
    checkY(Y),
    checkF(F),
    mars_zone(S),
    checkSurface(S),
    bl_landing_site(BL), 
    checkLandingsite(S, BL, _, _).


:- testCheckSurface, write("Surface checked\n").
:- testCheckLandingSite, write("Landing Site checked\n").
:- checkInput, write("Input checked\n").