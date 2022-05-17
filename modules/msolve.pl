:- module(msolve, lander_curr_pos/2).
:- use_module(minput).
:- dynamic lander_curr_pos/2.



%	if instead the lander didn't started already we can initialize it
updateLanderPosition(X, Y):-
	lander(X, Y), asserta(lander_curr_pos(X, Y)).

%% computes p list
compute_Plist(P):- mars_zone(L), landing_site(X1, Y1, X, Y)

.

.

%% computes intervals of t (as a list)

%% computes the bezier curve

%% checks if the lander reached the goal

%% solve predicate
solve:- 
	testCheckSurface, write("Surface checked\n"),
	testCheckSurface, write("Surface checked\n"),
	testCheckLandingSite, write("Landing Site checked\n"),
	checkInput, write("Input checked\n"),
	
	.