% Definition of boolean values.
bool(true).
bool(false).

% Definition of logical AND.
and(true, true, true).
and(true, false, false).
and(false, true, false).
and(false, false, false).

% Definition of logical OR.
or(true, true, true).
or(true, false, true).
or(false, true, true).
or(false, false, false).

% Definition of logical NOT.
not(true, false).
not(false, true).

% Expression 1: (X AND Y) OR Z.
ex1(X, Y, Z, Res) :- and(X, Y, A), or(A, Z, Res).

% Expression 2: (X AND Y) OR ((Y AND Z) AND Z).
ex2(X, Y, Z, Res) :- and(X, Y, A), and(Y, Z, B), and(B, Z, C), or(A, C, Res).

% Expression 3: (X AND (NOT Y) AND Z) AND ((Z AND Y) OR Z).
ex3(X, Y, Z, Res) :- not(Y, A), and(X, A, B), and(B, Z, C), and(Z, Y, D), or(D, Z, E), or(C, E, Res).
