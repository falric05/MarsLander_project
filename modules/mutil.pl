:- module(mutil, [tail/2]).

tail([H], H):- !.
tail([_|T], R):- tail(T, R).