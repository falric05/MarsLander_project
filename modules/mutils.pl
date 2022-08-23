:- module(mutils, [angleDecelerate/3,
                   angleToLandingSite/2,
                   regulateTPower/2,
                   getNextAngle/3,
                   getNextTPower/3]).
:- use_module(minput).
:- use_module(mlander).
:- use_module(mconfigs).
:- use_module(mcheck).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% computes the norm 2 of a vector
norm2_helper([], S, U):-
    sqrt(S, U).
norm2_helper([H|T], S, U):-
    HS is (H * H) + S,
    norm2_helper(T, HS, U).

norm2(V, U):-
    norm2_helper(V, 0, U).

%%% converts an angle in radiants to degree
radToDeg(Rrad, Rdeg):-
    Rdeg is Rrad * 180 / pi.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% returns the best angle to slow down marse lander
angleDecelerate(Sh, Sv, Rround):-
    norm2([Sh, Sv], Snorm),
    Rrad is asin(Sh / Snorm),
    radToDeg(Rrad, Rdeg),
    Rround is round(Rdeg).

%%% returns the exact angle to compensate gravity while
%%% the leander is going to the landing site
get_angle(Xl, R, R1):-
    landing_site(X0, _, _), Xl < X0, !,
    R1 is -R.
get_angle(Xl, R, R1):-
    landing_site(_, X1, _), Xl > X1, !,
    R1 is R.
get_angle(_, _, 0).

angleToLandingSite(Xl, R):-
    g(G), 
    Rrad is acos(G / 4),
    radToDeg(Rrad, Rdeg),
    Rround is round(Rdeg),
    get_angle(Xl, Rround, R).

%%% returns the thrust power needed to aim a null vertical speed
regulateTPower(Sv, P):-
    Sv > 0, !, 
    P is 3.
regulateTPower(_, 4).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% compute the right angle to deliver to the game, according to the limit of +/-15°
%%% among two next game turns, the previous angle and the desired one
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

%%% compute the right power to deliver to the game, according to the limit of +/-1
%%% among two next game turns, the previous power and the desired one
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