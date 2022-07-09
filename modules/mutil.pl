:- module(mutil, [tail/2, tailLeft/2, tailRight/2, scalarDot/3, vectorSum/3]).

tail([H], H):- !.
tail([_|T], R):- tail(T, R).

%%% This function return the right tail of a list
%%% e.g. [1,2,3,4] -> [2,3,4]
tailRight([_], []):- !.
tailRight([_|T], T).

%%% This function return the left tail of a list
%%% e.g. [1,2,3,4] -> [1,2,3]
tailLeft([_], []):- !.
tailLeft(L, T):- 
    reverse(L, Linv), tailRight(Linv, Tinv), reverse(Tinv, T).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                   Linear Algebra
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% This function return the scalar product with a vector
%%% e.g. scalarDot(2, [1,2], [2,4]).
scalarDot(_, [], []).
scalarDot(Lmb, [H|T], [H2| R1]):-
    scalarDot(Lmb, T, R1), H2 is (Lmb*H).

%%% This function return the vector sum of two vectors
%%% e.g. scalarDot(2, [1,2], [2,4]).
vectorSum([], [], []).
vectorSum([H1|T1], [H2|T2], [HR|R]):-
    vectorSum(T1, T2, R), HR is (H1+H2).