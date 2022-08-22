:- module(mcheck, [checkLandingsite/4, landing_site/3]).
:- use_module(minput).
:- use_module(mutils).
:- dynamic landing_site/3.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%              INPUT CHECKS                 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% checks the existence and the consistence with the minimum lenght of the landing site 
checkLandingsite([], _, _, _):-
    throw("Surfaces must contain a consistent landing site").
checkLandingsite([surface(X, Y)|_], BL, X1, Y1):-
    Y1 == Y, LLS is X - X1, BL < LLS + 1, !, 
    asserta(landing_site(X1, X, Y)).
checkLandingsite([surface(X, Y)|T], BL, _, _):-
    checkLandingsite(T, BL, X, Y).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%              GAME CHECKS CASE CONDITIONS                 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
