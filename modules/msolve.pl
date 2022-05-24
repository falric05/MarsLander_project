:- module(msolve, lander_curr_pos/2).
:- use_module(minput).
:- dynamic lander_curr_pos/2.


%% computes p list
get_Plist(Pout):- mars_zone(L), 
    lander(Xl, Yl, _), landing_site(X0, Y, _, Y), bl_landing_site(BL),
    Xl < X0, !, Xend is BL / 2 + X0, Yend is Y + (Y / 10),
    get_Plist(L, [point(Xl, Yl)], Xl, Xend, P),
    append(P, [point(Xend, Yend), point(Xend, Y)], Pout).

get_Plist(Pout):- 
    lander(Xl, Yl, _), landing_site(X0, Y, X1, Y), bl_landing_site(BL),
    Xl < X1, !, Xend is (BL / 2) + X0, Yend is Y + (Y / 10),
    append([], [point(Xl, Yl), point(Xend, Yend), point(Xend, Y)], Pout).

get_Plist(Pout):- mars_zone(L), 
    lander(Xl, Yl, _), landing_site(X0, Y, _, Y), bl_landing_site(BL),
 	Xend is BL / 2 + X0, Yend is (Y / 10),
    get_Plist(L, [point(Xend, Yend), point(Xend, Y)], Xl, Xend, P),
    append(P, [point(Xl, Yl)], Pout).

get_Plist([], Pin, _, _, Pin):- !.

get_Plist([surface(X,Y)|T], Pin, Xstart, Xend, Pout):- 
    X > Xstart, X < Xend, !, Ydb is Y * 2,
    append(Pin,[point(X, Ydb)],P), 
    get_Plist(T, P, Xstart, Xend, Pout).

get_Plist([surface(_, _)|T], Pin, Xstart, Xend, Pout):- 
    get_Plist(T, Pin, Xstart, Xend, Pout).

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