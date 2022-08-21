:- module(mhelper, [isOverLandingSite/1, 
                    isFinishing/1,
                    hasSafeSpeed/2,
                    goesInWrongDirection/2,
                    goesTooFastH/1,
                    goesTooSlowH/1,
                    angleToSlow/3,
                    angleToLandingSite/2,
                    powerToHover/2,
                    getNextAngle/3]).
:- use_module(minput).
:- use_module(mlander).
:- use_module(mcheck).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 			CONFIGS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
speed_eps(5).
y_eps(20).
p_max_step(1).
r_max_step(15).
r_max_degree(90).
danger_Vspeed(40).
danger_Hspeed(20).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

isOverLandingSite(Xl):-
    landing_site(X0, X1, _), 
    X0 < Xl, Xl < X1.

isFinishing(Yl):-
    landing_site(_, _, Y), y_eps(Yeps), 
    YplusEps is Y + Yeps,  
    Yl < YplusEps.

hasSafeSpeed(Sh, Sv):-
    abs(Sh, ShAbs), abs(Sv, SvAbs),
    danger_Hspeed(DSh), danger_Vspeed(DSv), speed_eps(Eps),
    DSh1 is DSh - Eps, DSv1 is DSv - Eps,
    ShAbs =< DSh1, SvAbs =< DSv1.

goesInWrongDirection(Xl, Sh):-
    landing_site(X0, _, _), Xl < X0, Sh < 0, !. 
goesInWrongDirection(Xl, Sh):-
    landing_site(_, X1, _), X1 < Xl, Sh > 0.

goesTooFastH(Sh):-
    abs(Sh, ShAbs), 
    danger_Hspeed(DSh), DSh1 is DSh * 4,
    ShAbs > DSh1.

goesTooSlowH(Sh):-
    abs(Sh, ShAbs), 
    danger_Hspeed(DSh), DSh1 is DSh * 2,
    ShAbs < DSh1.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% returns the best angle to slow down marse lander
%%% (the angle directing thrust in the opposition direction to the mvmt)
angleToSlow(Sh, Sv, Rround):-
    S2 is (Sh * Sh) + (Sv * Sv),
    sqrt(S2, S), Sh_over_S is Sh / S,
    asin(Sh_over_S, Rreal),
    Rdeg is Rreal * 180 / pi, Rround is round(Rdeg).
    % Rdeg is 0 * pi. 

%%% returns the exact angle to compensate gravity while
%%% going toward target
get_angle(Xl, R, R1):-
    landing_site(X0, _, _), Xl < X0, !,
    R1 is -R.
get_angle(Xl, R, R1):-
    landing_site(_, X1, _), Xl > X1, !,
    R1 is R.
get_angle(_, _, 0).

angleToLandingSite(Xl, R):-
    g(G), G_over_4 is G / 4,
    acos(G_over_4, Rreal),
    Rdeg is Rreal * 180 / pi, 
    Rround is round(Rdeg),
    get_angle(Xl, Rround, R).

%%%  returns the thrust power needed to aim a null vertical speed
powerToHover(Sv, P):-
    Sv > 0, !, 
    P is 3.
powerToHover(_, 4).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
getNextAngle(Rprev, Rdes, Rdes):-
    Rdiff is Rprev - Rdes,
    abs(Rdiff, RdiffAbs), 
    r_max_step(RMaxStep),
    RdiffAbs =< RMaxStep, !.
getNextAngle(Rprev, Rdes, Rnew):-
    Rdes < Rprev, !,
    r_max_step(RMaxStep),
    Rnew is Rprev - RMaxStep.
getNextAngle(Rprev, _, Rnew):-
    r_max_step(RMaxStep),
    Rnew is Rprev + RMaxStep.
