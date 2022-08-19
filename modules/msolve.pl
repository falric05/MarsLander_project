:- module(msolve, [solve/0, predict/0]).
:- use_module(minput).
:- use_module(mlander).
:- use_module(mcheck).
:- use_module(mhelper).

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
	danger_Vspeed(Dv), 
	Sv < Dv, P < 4, !,
	% Pup is P + 1,
	p_up(P, Pup),
	write(0), write(" "), write(Pup).

predict_ep1(Sv, P) :- 
	danger_Vspeed(Dv),
	Sv > Dv, P == 4, !,
	% Pdown is P - 1,
	p_down(P, Pdown),
	write(0), write(" "), write(Pdown).

predict_ep1(_, P) :- 
	write(0), write(" "), write(P).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Episode 2 case
get_rotation(_, Sh, R, Rnew):-
	danger_Hspeed(DS1, _), Sh < DS1, !,
	r_right(R, Rnew).
get_rotation(_, Sh, R, Rnew):-
	danger_Hspeed(_, DS2), Sh > DS2, !,
	r_left(R, Rnew).
get_rotation(Xl, _, R, Rnew):-
	landing_site(X1, _, _), Xl < X1, !,
	r_right(R, Rnew).
get_rotation(Xl, _, R, Rnew):-
	landing_site(_, X2, _), Xl > X2, !,
	r_left(R, Rnew).
get_rotation(_, _, R, Rnew):-
	r_to_zero(R, Rnew).

% danger horizontal speed or danger vertical speed
speed_danger(Sh, _, Dsh, _):-
	Sh > Dsh, !.
speed_danger(_, Sv, _, Dsv):-
	Sv < Dsv.

get_tpower(_, Sh, Sv, P, Pnew):-
	abs(Sh, ShAbs), 
	danger_Hspeed(_, Dh), danger_Vspeed(Dv), 
	speed_danger(ShAbs, Sv, Dh, Dv), P < 4, !,
	p_up(P, Pnew).
get_tpower(_, Sh, Sv, P, Pnew):- 
	abs(Sh, ShAbs), 
	danger_Hspeed(_, Dh), danger_Vspeed(Dv),
	ShAbs < Dh, Sv > Dv, P == 4, !,
	p_down(P, Pnew).
get_tpower(R, _, _, P, Pnew):-
	R =\= 0, !, p_up(P, Pnew).
get_tpower(_, _, _, P, P).

predict_ep2(_) :-
	% safe_alt(Plist, SafeAlt), 
	lander(Xl, _, Sh, Sv, _, R, P),
	%%% if the lander altitude is safe
	% Yl == SafeAlt, !,
	get_rotation(Xl, Sh, R, Rnew), !,
	get_tpower(Rnew, Sh, Sv, P, Pnew),
	write(Rnew), write(" "), write(Pnew).

predict_ep2(_) :- 
	lander(_, _, _, Sv, _, _, P),
	predict_ep1(Sv, P).

% predict_ep2(_) :-
% 	% safe_alt(Plist, SafeAlt), 
% 	lander(_, _, _, _, _, R, P),
% 	r_left(R, Rnew),
% 	write(R), write(" "), write(P).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% predict:-
% 	mars_zone(S), 
%     bl_landing_site(BL), 
%     checkLandingsite(S, BL, _, _),
% 	landing_site(X1, X2, _), 
% 	lander(Xl, _, Sh, Sv, _, _, P),
% 	Xl > X1, Xl < X2, Sh == 0, !,
% 	predict_ep1(Sv, P), 
% 	write(" Episode1!"),
% 	halt.

predict:-
	mars_zone(S), 
    bl_landing_site(BL), 
    checkLandingsite(S, BL, _, _),
	% landing_site(X1, X2, Y1), 
	get_Plist(Plist),
	% write("Episode 2-3!"), 
	predict_ep2(Plist), 
	halt.