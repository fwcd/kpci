reverseAccumulator([], Acc, Acc).
reverseAccumulator([E|L], Acc, Ys) :- reverseAccumulator(L, [E|Acc], Ys).

reverse(Xs, Ys) :- reverseAccumulator(Xs, [], Ys).
