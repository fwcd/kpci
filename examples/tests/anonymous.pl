p(A, B).
% Expected result: One solution, "{A -> a, B -> b}".

p(_, _).
% Expected result: One solution, "{}".
% Because the variables of answers should be restricted to non-anonymous
% variables, the solution should be the empty substitution.
% Note that if anonymous variables are not correctly handled, there probably
% will not be any solutions.

q.
% Because the variables of answers should be restricted to variables occuring
% in the goal, the solution should be the empty substitution.
% Note that if anonymous variables are not correctly handled, there probably
% will not be any solutions.
