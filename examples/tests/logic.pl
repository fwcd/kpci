% Assert that double negation works properly
not(X, Y), not(Y, X).

% Assert that expressions work as intended
ex1(_, _, true, true).
ex1(true, true, _, true).
ex1(false, false, false, false).
