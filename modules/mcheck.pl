:- module(mcheck, [checkX/1, checkY/1, checkF/1, checkSurface/1, checkLandingsite/4, landing_site/3]).
:- use_module(minput).
:- use_module(mhelper).
:- dynamic landing_site/3.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%              INPUT CHECKS                 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
/* checks if the start location of the shuttle (X coordinate) is lower or equal then the
 *   width of the zone and positive
 */
checkX(X):-
    zone(W, _), W-1 < X, !, throw("X must be less or equal to the width of the zone").
checkX(X):-
    X < 0, !, throw("X must be positive").
checkX(_).

/* checks if the start location of the shuttle (y coordinate) is lower or equal then the
 *   height of the zone and positive
 */
checkY(Y):-
    zone(_, H), H-1 < Y, !, throw("Y must be less or equal to the height of the zone").
checkY(Y):-
    Y < 0, !, throw("Y must be positive").
checkY(_).

/* checks if the fuel at input is positive
 */
checkF(F):-
    F < 0, !, throw("F must be positive").
checkF(_).

%%% checks the consistence of the surfaces in the zone gamE
checkSurface([]).
checkSurface([surface(X, _)|_]):-
    zone(W, _), X > W-1, !,
    throw("Surfaces must be coherent with the zone dimensions").
checkSurface([surface(_, Y)|_]):-
    zone(_, H), Y > H-1, !,
    throw("Surfaces must be coherent with the zone dimensions").
%%% checks if surfaces have positive coordinates
checkSurface([surface(X, _)|_]):-
    X < 0, !,
    throw("Surfaces must have positive coordinates").
checkSurface([surface(_, Y)|_]):-
    Y < 0, !,
    throw("Surfaces must have positive coordinates").
checkSurface([surface(_, _)|T]):-
    checkSurface(T).

%%% checks the existence and the consistence with the minimum lenght of the landing site 
checkLandingsite([], _, _, _):-
    throw("Surfaces must contain a consistent landing site").
checkLandingsite([surface(X, Y)|_], BL, X1, Y1):-
    Y1 == Y, LLS is X - X1, BL < LLS + 1, !, 
    asserta(landing_site(X1, X, Y)).
checkLandingsite([surface(X, Y)|T], BL, _, _):-
    checkLandingsite(T, BL, X, Y).
