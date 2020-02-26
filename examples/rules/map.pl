not(true, false).
not(false, true).

map(_, [], []).
map(P, [E|L], [F|M]) :- call(P, E, F), map(P, L, M).
