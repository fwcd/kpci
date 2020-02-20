q(a, b).
p(X, X).
p(X,Z) :- q(X, Y), p(Y,Z).
