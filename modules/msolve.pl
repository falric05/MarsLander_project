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

predict:-
	mars_zone(S), 
    bl_landing_site(BL), 
    checkLandingsite(S, BL, _, _),
	landing_site(X1, _, X2, _), 
	lander(Xl, _, _, _, _, _, _),
	Xl > X1, Xl < X2, !,
	write("Episode 1!"), halt.

predict:-
	mars_zone(S), 
    bl_landing_site(BL), 
    checkLandingsite(S, BL, _, _),
	write("Episode 2-3!"), halt.