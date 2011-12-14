p(1,3,5).
p(2,4,1).
p(3,5,2).
p(4,3,1).
p(5,2,4).

z = [4, 6, 3, 7, 8].

subsetN( _, S, N, S ) :-
        length( S, N ), !.

subsetN( [H|T], Sofar, N, S ) :-
        M is N - 1,
        subsetN( T, [], M, S1 ),
        append( [H|Sofar], S1, S ).

subsetN( [_|T], Sofar, N, S ) :-
        subsetN( T, Sofar, N, S ).
