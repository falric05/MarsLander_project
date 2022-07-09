:- module(mutil, [tail/2, tailLeft/2, tailRight/2, scalarDot/3, vectorSum/3]).

tail([H], H):- !.
tail([_|T], R):- tail(T, R).

%%% This function return the right tail of a list
%%% e.g. tailRight([1,2,3,4], [2,3,4])
tailRight([_], []):- !.
tailRight([_|T], T).

%%% This function return the left tail of a list
%%% e.g. tailLeft([1,2,3,4], [1,2,3])
tailLeft([_], []):- !.
tailLeft(L, T):- 
    reverse(L, Linv), tailRight(Linv, Tinv), reverse(Tinv, T).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                   Linear Algebra
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% This function return the scalar product with a vector
%%% e.g. scalarDot(2, [point(1,2), point(3,2)], 
%%%                   [point(2,4), point(6,4)]).
scalarDot(_, [], []).
scalarDot(Lmb, [point(X, Y)|T], [point(X2, Y2)| R1]):-
    scalarDot(Lmb, T, R1), 
    X2 is (Lmb*X), Y2 is (Lmb*Y).

%%% This function return the vector sum of two vectors
%%% e.g. vectorSum([point(1,2),point(3,2)], 
%%%                [point(3,2),point(1,2)], 
%%%                [point(4,4), point(4,4)]).
vectorSum([], [], []).
vectorSum([point(X1, Y1)|T1], [point(X2, Y2)|T2], [point(XR, YR)|R]):-
    vectorSum(T1, T2, R), 
    XR is (X1+X2), YR is (Y1+Y2).