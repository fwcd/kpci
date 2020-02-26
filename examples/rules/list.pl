append([], L, L).
append([E|R], L, [E|RL]) :- append(R, L, RL).

last(L, E) :- append(_, [E], L).

reverse([], []).
reverse([E|R], L) :- reverse(R, UR), append(UR, [E], L).

member(E, [E|_]).
member(E, [_|R]) :- member(E,R).

perm([], []).
perm(L, [E|R]) :- delete(E, L, LwithoutE), perm(LwithoutE, R).

delete(E, L, R) :- append(L1, [E|L2], L), append(L1, L2, R).

sort(L, S) :- perm(L, S), sorted(S).

sorted([]).
sorted([_]).
sorted([E1|[E2|L]]) :- =<(E1, E2), sorted([E2|L]).

length([], 0).
length([_|Xs], N) :- length(Xs, N1), is(N, +(N1, 1)).

lengthP([], o).
lengthP([_|Xs], s(N)) :- lengthP(Xs, N).
