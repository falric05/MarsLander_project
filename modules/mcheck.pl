:- module(mcheck, [checkLandingsite/4, 
                   landing_site/3,
                   %%%
                   checkOverLandingSite/1, 
                   checkLanding/1,
                   checkSpeed/2,
                   checkDirection/2,
                   checkHighSpeedH/1,
                   checkLowSpeedH/1]).
:- use_module(minput).
:- use_module(mconfigs).
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
%%% true if the lander is passing over the landing site
checkOverLandingSite(Xl):-
    landing_site(X0, X1, _), 
    X0 < Xl, Xl < X1.

%%% true if the lander is close to the land
checkLanding(Yl):-
    landing_site(_, _, Y), y_eps(Yeps), 
    YplusEps is Y + Yeps,  
    Yl < YplusEps.

%%% true if speed is under speed limits 
checkSpeed(Sh, Sv):-
    abs(Sh, ShAbs), abs(Sv, SvAbs),
    danger_Hspeed(DSh), danger_Vspeed(DSv), speed_eps(Eps),
    DSh1 is DSh - Eps, DSv1 is DSv - Eps,
    ShAbs =< DSh1, SvAbs =< DSv1.

%%% true if the speed direction is opposite to the to the direction 
%%% from the lander to the landing site
checkDirection(Xl, Sh):-
    landing_site(X0, _, _), Xl < X0, Sh < 0, !. 
checkDirection(Xl, Sh):-
    landing_site(_, X1, _), X1 < Xl, Sh > 0.

%%% true if the horizontal speed is over the horizontal speed limit
checkHighSpeedH(Sh):-
    abs(Sh, ShAbs), 
    danger_Hspeed(DSh), DSh1 is DSh * 4,
    ShAbs > DSh1.

%%% true if the horizontal speed is under the horizontal speed limit
checkLowSpeedH(Sh):-
    abs(Sh, ShAbs), 
    danger_Hspeed(DSh), DSh1 is DSh * 2,
    ShAbs < DSh1.
