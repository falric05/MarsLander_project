:- module(msolve, [solve/0, predict/0]).
:- use_module(minput).
:- use_module(mlander).
:- use_module(mcheck).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% checks if the lander reached the goal
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% solve predicate
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
solve:- 
	testCheckSurface, write("Surface checked\n"),
	testCheckLandingSite, write("Landing Site checked\n"),
	checkInput, write("Input checked\n")
	.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Simple case episode 1 when lander is over flat area
predict_ep1(Sv, P) :- 
	Sv < -40, P < 4, !,
	Pup is P + 1,
	write(0), write(" "), write(Pup).

predict_ep1(Sv, P) :- 
	Sv > -39, P == 4, !,
	Pdown is P - 1,
	write(0), write(" "), write(Pdown).

predict_ep1(_, P) :- 
	write(0), write(" "), write(P).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Episode 2 case
predict_ep2 :-
	write(0), write(" "), write(0).

predict_ep2(P, Sv, Sh) :-
	write(0), write(" "), write(0).

predict:-
	mars_zone(S), 
    bl_landing_site(BL), 
    checkLandingsite(S, BL, _, _),
	landing_site(X1, _, X2, _), 
	lander(Xl, _, _, Sv, _, _, P),
	Xl > X1, Xl < X2, !,
	% write("Episode 1!"),
	predict_ep1(Sv, P), 
	halt.

predict:-
	mars_zone(S), 
    bl_landing_site(BL), 
    checkLandingsite(S, BL, _, _),
	landing_site(X1, Y1, X2, Y1), 
	lander(Xl, _, _, Sv, _, _, P),
	% write("Episode 2-3!"), 
	predict_ep2, 
	halt.