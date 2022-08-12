:- use_module(minput).
:- use_module(mlander).
:- use_module(mcheck).
:- use_module(mbezier).
:- use_module(mutil).

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
    lander(X, Y, _, _, F, _, _),
    checkX(X),
    checkY(Y),
    checkF(F),
    mars_zone(S),
    checkSurface(S),
    bl_landing_site(BL), 
    checkLandingsite(S, BL, _, _).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%              INPUT tests                 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- testCheckSurface, write("<=> Surface checked\n\n").
:- testCheckLandingSite, write("\n<=> Landing Site checked\n\n").
:- checkInput, write("<=> Input checked\n\n").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%              BEZIER tests                 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% test the extraction of of timestamp in [0,1]
:- get_kInterval([H1, H2, H3| K]), tail(K, R), write("["), 
   write(H1), write(", "), write(H2), write(", "), write(H3), 
   write(", ..., "), write(R), write("]"),
   write("\n<=> k interval for Bezier curve approximation checked\n\n").

%%% test the extraction of points P from the surface
:- get_Plist(P), write(P), write("\n<=> p ponts for Bezier curve approximation checked\n\n").

%%% test the bezier curve in t = T over the points P
:- get_kInterval([K|_]), get_Plist(P), bezier(K, P, R),
   write(R),write("\n<=> point at k = "),writeln(K).

testBezier :- 
    mars_zone(S),                   % get the zone
    bl_landing_site(BL),            % get the landing site
    checkLandingsite(S, BL, _, _),  % check if it is correct
    get_Plist(P), write(P).         % returns the P list

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- write("test completed! - success!").