% Test map predicate
map(not, [], []).
map(not, [true, true, false], [false, false, true]).
map(constThree, [true, false, 9, 3, false, true], [3, 3, 3, 3, 3, 3]).

% Test partial evaluation
call(map, not, [true, true], [false, false]).
call(map(not), [true, true], [false, false]).
call(map(not, [true, true]), [false, false]).
call(map(not, [true, true], [false, false])).
