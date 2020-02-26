append(Xs,Ys,[2,1]), append(Ys,Xs,[1,2]).
% Expected result: One solution, "{Xs -> [1], Ys -> [2]}".
% Tests whether the mgu found in a resolution step is applied to the whole goal.

append(X,Y,[1,2]).
% Expected result: Three solutions, "{X -> [], Y -> [1, 2]}",
% "{X -> [1], Y -> [2]}", and "{X -> [1, 2], Y -> []}".

append(_,_,[1,2]).
% Expected result: Three times the empty substitution.

last([1,2,3],X).
% Expected result: One solution, "{X -> 3}".

last(Xs,3).
% Expected result: Infinite solutions, lists with 3 as a last element.

reverse([1,2,3],Xs).
% Expected result: One solution, "{Xs -> [3, 2, 1]}".

reverse(Xs,[1,2,3]).
% Expected result: One solution, "{Xs -> [3, 2, 1]}", but non-termination.

reverse(Xs,Xs).
% Expected result: Infinite solutions, all palindroms.

member(X,[1,2,3]).
% Expected result: Three solutions, "{X -> 1}", "{X -> 2}", and "{X -> 3}".

member(X, Xs).
% Expected result: Infinite solutions, where X is within the lists.

delete(X,[1,2,L],Y).
% Expected result: Three solutions, "{X -> 1, Y -> [2, L]}",
% "{X -> 2, Y -> [1, L]}", and "{X -> L, Y -> [1, 2]}".
% Tests renaming during SLD resolution.

sort([3,1,2],Xs).
% Expected result: One solution, "{Xs -> [1, 2, 3]}".

append(X,Y,X).
% Expected result: Infinite solutions, where Y is the empty list.
% Tests strategies and REPL for infinite number of solutions.

length(Xs,2).
% Expected result: One solution, "{Xs -> [_, _]}", but non-termination.

lengthP(Xs,s(s(o))).
% Expected result: One solution, "{Xs -> [_, _]}".
