:- module(mhelper, [danger_Vspeed/1, danger_Hspeed/2,
                    get_Plist/1, safe_alt/2, 
                    p_up/2, p_down/2, r_left/2, r_right/2, r_to_zero/2]).
:- use_module(minput).
:- use_module(mlander).
:- use_module(mcheck).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 			CONFIGS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
p_max_step(1).
r_max_step(15).
% r_max_degree(45).
% danger_Vspeed(-40).
r_max_degree(30).
danger_Vspeed(-30).
danger_Hspeed(-20, 20).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 			P POINTS
%% this return the P list of keypoints for computing the bezier curve
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% _case 1_: if the lander is on the left of landing site
get_Plist(Pout):- 
    mars_zone(L), 
    lander(Xl, Yl, _, _, _, _, _), 
    landing_site(X0, _, Y),
    bl_landing_site(BL), 
    Xl < X0, !,           
    Xend is BL / 2 + X0, Yend is Y + 1,
    get_Plist(L, [point(Xl, Yl)], Xl, Xend, P),
    append(P, [point(Xend, Yend), point(Xend, Y)], Pout).
%%% _case 2_: if the lander is inside of the landing site
get_Plist(Pout):- 
    lander(Xl, Yl, _, _, _, _, _), landing_site(X0, X1, Y), bl_landing_site(BL),
    Xl < X1, !, Xend is (BL / 2) + X0, Yend is Y + (Y / 10),
    append([], [point(Xl, Yl), point(Xend, Yend), point(Xend, Y)], Pout).
%%% _case 3_: if the lander is on the right of landing site
get_Plist(Pout):- mars_zone(L), 
    lander(Xl, Yl, _, _, _, _, _), landing_site(X0, _, Y), bl_landing_site(BL),
 	Xend is BL / 2 + X0, Yend is (Y / 10),
    get_Plist(L, [point(Xend, Yend), point(Xend, Y)], Xl, Xend, P),
    append(P, [point(Xl, Yl)], Pout).


%%% helper procedure for get_Plist(Pout)
%%% foreach surface point if it is between the landing surface and the lander position
%%% I should consider it. A safe distance of 10 is considered
get_Plist([], Pin, _, _, Pin):- !.
get_Plist([surface(X,Y)|T], Pin, Xstart, Xend, Pout):- 
    X > Xstart, X < Xend, !, Ysafe is Y + 10,
    append(Pin,[point(X, Ysafe)],P), 
    get_Plist(T, P, Xstart, Xend, Pout).
get_Plist([surface(_, _)|T], Pin, Xstart, Xend, Pout):- 
    get_Plist(T, Pin, Xstart, Xend, Pout).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 			SAFE ALTITUDE
%% this return the Y coordinate where the lander should fly to stay safe
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
safe_alt_helper([], L, SafeAlt):-
	max_list(L, SafeAlt).

safe_alt_helper([point(_, Y)|T], L, SafeAlt):-
	safe_alt_helper(T, [Y|L], SafeAlt).

safe_alt(Plist, SafeAlt) :-
	safe_alt_helper(Plist, _, SafeAlt).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 			ACTIONS
%% this return the Y coordinate where the lander should fly to stay safe
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
p_up(4, 4).
p_up(P, Pnew):- 
    p_max_step(S),
    Pnew is P + S.

p_down(0, 0).
p_down(P, Pnew):- 
    p_max_step(S),
    Pnew is P - S.


% r_left(90, 90).
r_left(D, D):-
    r_max_degree(D).
r_left(R, Rnew):- 
    r_max_step(S),
    Rnew is R + S.

% r_right(-90, -90).
r_right(D1, D1):-
    r_max_degree(D), D1 is -1 * D.
r_right(R, Rnew):- 
    r_max_step(S),
    Rnew is R - S.

r_to_zero(0, 0).
r_to_zero(R, Rnew):-
    r_max_step(S),
    R >= S, !,
    Rnew is R - S.
r_to_zero(R, Rnew):-
    r_max_step(S), 
    Rnew is R + S.