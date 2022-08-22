:- module(mutils, [
                   angleToSlow/3,
                   angleToLandingSite/2,
                   powerToHover/2,
                   getNextAngle/3,
                   getNextTPower/3]).
:- use_module(minput).
:- use_module(mlander).
:- use_module(mconfigs).
:- use_module(mcheck).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% returns the best angle to slow down marse lander
%%% (the angle directing thrust in the opposite direction to the mvmt)
angleToSlow(Sh, Sv, Rround):-
    S2 is (Sh * Sh) + (Sv * Sv),
    sqrt(S2, S), Sh_over_S is Sh / S,
    asin(Sh_over_S, Rrad),
    Rdeg is Rrad * 180 / pi, Rround is round(Rdeg).

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
    acos(G_over_4, Rrad),
    Rdeg is Rrad * 180 / pi, 
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

getNextTPower(Pprev, Pdes, Pdes):-
    Pdiff is Pprev - Pdes,
    abs(Pdiff, PdiffAbs), 
    p_max_step(PMaxStep),
    PdiffAbs =< PMaxStep, !.
getNextTPower(Pprev, Pdes, Pnew):-
    Pdes < Pprev, !,
    p_max_step(PMaxStep),
    Pnew is Pprev - PMaxStep.
getNextTPower(Pprev, _, Pnew):-
    p_max_step(PMaxStep),
    Pnew is Pprev + PMaxStep.