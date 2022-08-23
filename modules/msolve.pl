:- module(msolve, [predict/0]).
:- use_module(minput).
:- use_module(mlander).
:- use_module(mcheck).
:- use_module(mutils).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% solve predicate
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

predict_HPA(Yl, _, _, Rprev, Pprev):- 
	checkLanding(Yl), !,
	getNextAngle(Rprev, 0, Rout),
	getNextTPower(Pprev, 3, Pout),
	write(Rout), write(" "), write(Pout), 
	write(" predict_HPA_HP1").

predict_HPA(_, Sh, Sv, Rprev, Pprev):- 
	checkSpeedLimit(Sh, Sv), !,
	getNextAngle(Rprev, 0, Rout),
	getNextTPower(Pprev, 2, Pout),
	write(Rout), write(" "), write(Pout), 
	write(" predict_HPA_HP2").

predict_HPA(_, Sh, Sv, Rprev, Pprev):- 
	angleDecelerate(Sh, Sv, Rdes),
	getNextAngle(Rprev, Rdes, Rout),
	getNextTPower(Pprev, 4, Pout),
	write(Rout), write(" "), write(Pout), 
	write(" predict_HPA_HP3").

%%

predict_HPB_HP1(Sh, Sv, Rprev, Pprev):-
	angleDecelerate(Sh, Sv, Rdes),
	getNextAngle(Rprev, Rdes, Rout),
	getNextTPower(Pprev, 4, Pout),
	write(Rout), write(" "), write(Pout), 
	write(" predict_HPB_HP1").

predict_HPB_HP2(Xl, Rprev, Pprev):-
	angleToLandingSite(Xl, Rdes),
	getNextAngle(Rprev, Rdes, Rout),
	getNextTPower(Pprev, 4, Pout),
	write(Rout), write(" "), write(Pout), 
	write(" predict_HPB_HP2").

predict_HPB_HP3(Sv, Rprev, Pprev):-
	regulateTPower(Sv, P),
	getNextAngle(Rprev, 0, Rout),
	getNextTPower(Pprev, P, Pout),
	write(Rout), write(" "), write(Pout), 
	write(" predict_HPB_HP3").

%

predict_HPB(Xl, Sh, Sv, R, P):- 
	checkOppositeDirection(Xl, Sh), !,
	predict_HPB_HP1(Sh, Sv, R, P).
predict_HPB(_, Sh, Sv, R, P):- 
	checkHighSpeedH(Sh), !,
	predict_HPB_HP1(Sh, Sv, R, P).

predict_HPB(Xl, Sh, _, R, P):- 
	checkLowSpeedH(Sh), !,
	predict_HPB_HP2(Xl, R, P).

predict_HPB(_, _, Sv, R, P):-
	predict_HPB_HP3(Sv, R, P).

%%%

predict:-
	mars_zone(S),
	bl_landing_site(BL), 
    checkLandingsite(S, BL, _, _),
	lander(Xl, Yl, Sh, Sv, _, R, P),
	%%%
	checkOverLandingSite(Xl), !,
	predict_HPA(Yl, Sh, Sv, R, P),
	halt.

predict:-
	lander(Xl, _, Sh, Sv, _, R, P),
	predict_HPB(Xl, Sh, Sv, R, P),
	halt.