:- dynamic landing_site/4.

% The zone is 7000m wide and 3000m high.
zone(7000, 3000).

% The limit for the landing site of the shuttle must be at least
bl_landing_site(1000).

% The number surfaceN of points used to draw the surface of Mars.
% a couple of integers landX landY providing the coordinates of a ground point. 
% By linking all the points together in a sequential fashion, you form the surface of Mars which is composed 
% of several segments. 
% For the first point, landX = 0 and for the last point, landX = 6999
mars_zone([
     surface(0, 1500),
     surface(1000, 2000),
     surface(2000, 500),
     surface(3500, 500),
     surface(5000, 1500),
     surface(6999, 1000)
]).

% Start position of the lander and amount of carburants in litres
lander(500, 2000, 4500).

% +------------------------------------------+
% |             INPUT CHECKS                 |
% +------------------------------------------+
% checks if the start location of the shuttle (X coordinate) is lower or equal then the
%   width of the zone and positive
checkX(X):-
    zone(W, _), W-1 < X, !, throw("X must be less or equal to the width of the zone").
checkX(X):-
    X < 0, !, throw("X must be positive").
checkX(_).

% checks if the start location of the shuttle (y coordinate) is lower or equal then the
%   height of the zone and positive
checkY(Y):-
    zone(_, H), H-1 < Y, !, throw("Y must be less or equal to the height of the zone").
checkY(Y):-
    Y < 0, !, throw("Y must be positive").
checkY(_).

% checks if the fuel at input is positive
checkF(F):-
    F < 0, !, throw("F must be positive").
checkF(_).

% checks the consistence of the surfaces in the zone game
checkSurface([]).
checkSurface([surface(X, _)|_]):-
    zone(W, _), X > W-1, !,
    throw("Surfaces must be coherent with the zone dimensions").
checkSurface([surface(_, Y)|_]):-
    zone(_, H), Y > H-1, !,
    throw("Surfaces must be coherent with the zone dimensions").
% checks if surfaces have positive coordinates
checkSurface([surface(X, _)|_]):-
    X < 0, !,
    throw("Surfaces must have positive coordinates").
checkSurface([surface(_, Y)|_]):-
    Y < 0, !,
    throw("Surfaces must have positive coordinates").
checkSurface([surface(_, _)|T]):-
    checkSurface(T).

% checks the existence and the consistence with the minimum lenght of the landing site 
checkLandingsite([], _, _, _):-
    throw("Surfaces must contain a consistent landing site").
checkLandingsite([surface(X, Y)|_], BL, X1, Y1):-
    Y1 == Y, LLS is X - X1, BL < LLS + 1, !, 
    asserta(landing_site(X1, Y1, X, Y)).
checkLandingsite([surface(X, Y)|T], BL, _, _):-
    checkLandingsite(T, BL, X, Y).
%checkSurface.
%



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
