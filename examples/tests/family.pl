% IGNORE

% vorfahre(X,Y).
% Expected result (with dfs): No solutions and non-termination.
% Expected result (with bfs or iddfs): Multiple solutions, but still
% non-termination.
% Tests completeness of bfs and iddfs.

geschwister(X,Y).
% Expected result: 4 solutions, "{X -> herbert, Y -> angelika}",
% "{X -> angelika, Y -> herbert}", "{X -> susanne, Y -> norbert}", and
% "{X -> norbert, Y -> susanne}".
% Tests implementation of negation as a failure.

grossvaeter(Xs).
% Expected result: One solution, "{Xs -> [[susanne, heinz], [norbert, heinz],
% [andreas, fritz], [andreas, heinz]]}".
% Tests implementation of encapsulation.
