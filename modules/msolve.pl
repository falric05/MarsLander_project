:- module(msolve, [solve/0]).
:- use_module(minput).
:- use_module(mlander).

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
	landing_site(X1, _, X2, _), write(X1), write(" "), write(X2)
	.