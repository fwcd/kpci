father(bob, alice).
notFather(X, Y) :- \+(father(X, Y)).
