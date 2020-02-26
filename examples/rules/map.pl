% Maps a predicate over a list.
map(_, [], []).
map(P, [E|L], [F|M]) :- call(P, E, F), map(P, L, M).

% Utility predicates, useful for testing map.
not(true, false).
not(false, true).
constThree(X, 3).
