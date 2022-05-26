:- module(mbezier, [get_Plist/1, get_tInterval/1]).
:- use_module(minput).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 			P POINTS
%% computes p list
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 			T INTERVALS
%% computes intervals of t (as a list)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% The size of t interval for approximate the Beziér curve
t_size(100).

get_step(S):- t_size(Size), S is 1 / Size.

range(H, H, _, [H]):- !.
range(L, H, _, [H]):- L > H, !.
range(L, H, S, Out1):- L1 is L + S, range(L1, H, S, Out0), append([L], Out0, Out1).

get_tInterval(T):- get_step(S), L is 0.0 + S, range(L, 1.0, S, T).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% computes the bezier curve
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
