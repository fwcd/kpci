% IGNORE

is(42,+(21,21)).
% Expected result: One solution, "{}".

\+(is(+(21,21),42)).
% Expected result: No solutions.

\+(is(+(21,21),+(21,21))).
% Expected result: No solutions.

=(+(21,21),+(21,21)).
% Expected result: One solution, "{}".

\+(=(+(21,21),42)).
% Expected result: No solutions.

\+(is(_,mod(10,0))).
% Expected result: No solutions.
% This test case checks whether division by zero is properly handled.

factorial(0,F).
% Expected result: One solution, "{F -> 1}".

factorial(N,F).
% Expected result: One solution, "{N -> 0, F -> 1}".
% Because the comparison operation within the second rule fails for
% non-instantiated variables, only the first rule leads to a solution.

factorial(30,F).
% Expected result: One solution, "{F -> 265252859812191058636308480000000}".
% If the solution contains a negative number (-8764578968847253504), it is most
% likely the case that no unbound integers have been used in the implementation.
