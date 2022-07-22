:- module(mbezier, [get_Plist/1, get_kInterval/1, bezier/3]).
:- use_module(minput).


%%% The size of k interval for approximate the Beziér curve
k_size(20).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 			P POINTS
%% this return the P list of keypoints for computing the bezier curve
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% _case 1_: if the lander is on the left of landing site
get_Plist(Pout):- mars_zone(L), 
    lander(Xl, Yl, _), landing_site(X0, Y, _, Y), bl_landing_site(BL),
    Xl < X0, !, Xend is BL / 2 + X0, Yend is Y + (Y / 10),
    get_Plist(L, [point(Xl, Yl)], Xl, Xend, P),
    append(P, [point(Xend, Yend), point(Xend, Y)], Pout).
%%% _case 2_: if the lander is inside of the landing site
get_Plist(Pout):- 
    lander(Xl, Yl, _), landing_site(X0, Y, X1, Y), bl_landing_site(BL),
    Xl < X1, !, Xend is (BL / 2) + X0, Yend is Y + (Y / 10),
    append([], [point(Xl, Yl), point(Xend, Yend), point(Xend, Y)], Pout).
%%% _case 3_: if the lander is on the right of landing site
get_Plist(Pout):- mars_zone(L), 
    lander(Xl, Yl, _), landing_site(X0, Y, _, Y), bl_landing_site(BL),
 	Xend is BL / 2 + X0, Yend is (Y / 10),
    get_Plist(L, [point(Xend, Yend), point(Xend, Y)], Xl, Xend, P),
    append(P, [point(Xl, Yl)], Pout).


%%% helper procedure for get_Plist(Pout)
get_Plist([], Pin, _, _, Pin):- !.
get_Plist([surface(X,Y)|T], Pin, Xstart, Xend, Pout):- 
    X > Xstart, X < Xend, !, Ydb is Y * 2,
    append(Pin,[point(X, Ydb)],P), 
    get_Plist(T, P, Xstart, Xend, Pout).
get_Plist([surface(_, _)|T], Pin, Xstart, Xend, Pout):- 
    get_Plist(T, Pin, Xstart, Xend, Pout).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 			k INTERVALS
%% computes intervals of k (as a list)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
/* This is used to extract the steps according to the size of the ivervalt in [0,1]
 * chosen. 
 * e.g. k_size(100). => get_step(0.01)
 */
get_step(S):- k_size(Size), S is 1 / Size.

/* This returns a list with values v s.t. L < v <= H of a size S that can be obtained
 * using get_step(S)
 */
range(H, H, _, [H]):- !.
range(L, H, _, [H]):- L > H, !.
range(L, H, S, Out1):- L1 is L + S, range(L1, H, S, Out0), append([L], Out0, Out1).

/* This returns a list with values v s.t. 0 < v <= 1 of a size s where get_step(s)
 */
get_kInterval(K):- get_step(S), L is (0 + S), range(L, 1, S, K).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% computes the bezier curve
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/* this returns the point over the bezier curve defined by the keypoints P 
 * corresponding to timestamp T.
 * the computation of the curve is peeformed by the recursive definition (you can 
 * find in 
 * https://en.wikipedia.org/wiki/B%C3%A9zier_curve#Constructing_B%C3%A9zier_curves)
 */
bezier(_, [point(X, Y)], [point(X, Y)]).
bezier(K, PList, B):- 
    tailLeft(PList, Pl), tailRight(PList, Pr),
    bezier(K, Pl, Bl), bezier(K, Pr, Br), 
    Kc is 1-K,
    scalarDot(Kc, Bl, KcBl), scalarDot(K, Br, KBr), vectorSum(KcBl, KBr, B).
