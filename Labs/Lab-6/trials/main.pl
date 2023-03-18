read_pipeline(X) :- 
see('/home/adrian/Desktop/Python/Personal/Master/Master-Projects/First-Year/Knowledge-Representation-and-Reasoning/Labs/Lab-6/input.txt'),
read(X), seen.

print(MSG) :- write(MSG), nl.

poor(Score, Percent) :- Score < 2, Percent is 1.0, !.
poor(Score, Percent) :- Score < 3, Percent is 0.8, !.
poor(Score, Percent) :- Score < 4, Percent is 0.4, !.
poor(Score, Percent) :- Score < 5, Percent is 0.2, !.
poor(_, Percent) :- Percent is 0.0, !.

good(Score, Percent) :- Score < 3, Percent is 0.0, !.
good(Score, Percent) :- Score < 4, Percent is 0.3, !.
good(Score, Percent) :- Score < 5, Percent is 0.6, !.
good(Score, Percent) :- Score < 6, Percent is 1.0, !.
good(Score, Percent) :- Score < 7, Percent is 0.6, !.
good(Score, Percent) :- Score < 8, Percent is 0.3, !.
good(_, Percent) :- Percent is 0.0, !.


excellent(Score, Percent) :- Score < 7,  Percent is 0.0, !.
excellent(Score, Percent) :- Score < 8,  Percent is 0.3, !.
excellent(Score, Percent) :- Score < 9,  Percent is 0.6, !.
excellent(Score, Percent) :- Score < 10, Percent is 1.0, !.
excellent(_, Percent) :- Percent is 0.0, !.

rancid(Score, Percent) :- Score < 2, Percent is 1.0, !.
rancid(Score, Percent) :- Score < 4, Percent is 0.8, !.
rancid(Score, Percent) :- Score < 6, Percent is 0.4, !.
rancid(Score, Percent) :- Score < 8, Percent is 0.2, !.
rancid(_, Percent) :- Percent is 0.0, !.


delicious(Score, Percent) :- Score < 7, Percent is 0.0, !.
delicious(Score, Percent) :- Score < 8, Percent is 0.3, !.
delicious(Score, Percent) :- Score < 9, Percent is 0.6, !.
delicious(Score, Percent) :- Score < 10, Percent is 1.0, !.
delicious(_, Percent) :- Percent is 0.0, !.



stingy(Tip, Percent) :- Tip < 1, Percent  is 0.2, !.
stingy(Tip, Percent) :- Tip < 2, Percent  is 0.4, !.
stingy(Tip, Percent) :- Tip < 3, Percent  is 0.6, !.
stingy(Tip, Percent) :- Tip < 4, Percent  is 0.8, !.
stingy(Tip, Percent) :- Tip < 5, Percent  is 1.0, !.
stingy(Tip, Percent) :- Tip < 6, Percent  is 0.8, !.
stingy(Tip, Percent) :- Tip < 7, Percent  is 0.6, !.
stingy(Tip, Percent) :- Tip < 8, Percent  is 0.4, !.
stingy(Tip, Percent) :- Tip < 9, Percent  is 0.2, !.
stingy(Tip, Percent) :- Tip < 10, Percent is 0.0, !.
stingy(_, Percent) :- Percent is 0.0, !.

normal(Tip, Percent) :- Tip < 10, Percent is 0.0, !.
normal(Tip, Percent) :- Tip < 11, Percent is 0.2, !.
normal(Tip, Percent) :- Tip < 12, Percent is 0.4, !.
normal(Tip, Percent) :- Tip < 13, Percent is 0.6, !.
normal(Tip, Percent) :- Tip < 14, Percent is 0.8, !.
normal(Tip, Percent) :- Tip < 15, Percent is 1.0, !.
normal(Tip, Percent) :- Tip < 16, Percent is 0.8, !.
normal(Tip, Percent) :- Tip < 17, Percent is 0.6, !.
normal(Tip, Percent) :- Tip < 18, Percent is 0.4, !.
normal(Tip, Percent) :- Tip < 19, Percent is 0.2, !.
normal(_, Percent) :- Percent is 0.0, !.

generous(Tip, Percent) :- Tip < 19, Percent is 0.0, !.
generous(Tip, Percent) :- Tip < 20, Percent is 0.3, !.
generous(Tip, Percent) :- Tip < 21, Percent is 0.6, !.
generous(Tip, Percent) :- Tip < 22, Percent is 1.0, !.
generous(Tip, Percent) :- Tip < 23, Percent is 1.0, !.
generous(Tip, Percent) :- Tip < 24, Percent is 0.6, !.
generous(Tip, Percent) :- Tip < 25, Percent is 0.3, !.
generous(_, Percent) :- Percent is 0.0, !.



or(List, Result)  :- max_list(List, Result).
and(List, Result) :- min_list(List, Result).

decomposition([Operator, Conditions, Conclusion], Operator, Conditions, Conclusion).

iterate_conditions([], _, []).
iterate_conditions([(_ / Degree) | Conditions], [Score | Scores], [Percent | Result]) :- print(Degree), print(Score), call(Degree, Score, Percent), print(Percent), iterate_conditions(Conditions, Scores, Result).

compute_intervals([], _, []).
compute_intervals([X | Xs], Degree, [(X, Percent) | Results]) :- call(Degree, X, Percent), compute_intervals(Xs, Degree, Results).


argmax([], _, R, R). 
argmax([(X, Y) | Points], Max, Idx, R):- Y >= Max, argmax(Points, Y, X, R).
argmax([(X, Y) | Points], Max, Idx, R):- Y <  Max, argmax(Points, Max, Idx, R).
argmax([(X, Y) | Points], R):- argmax(Points, Y, X, R). 

iterate_conclusions([], _, []).
iterate_conclusions([(_ / Degree) | Conclusions], Percent, Tip) :- Interval = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 25, 26],
																	compute_intervals(Interval, Degree, Solutions), argmax(Solutions, Idx), print(Idx), Tip is Percent * Idx.

iterate([], _, []).
iterate([Premise | Premises], Scores, [Result | Results]) :- print(Premise), decomposition(Premise, Operator, Conditions, Conclusions), 
															iterate_conditions(Conditions, Scores, Percentages), print(Percentages), 
															call(Operator, Percentages, Result), print(Result),
															iterate(Premises, Scores, Results).

second_iterate([], _, []).
second_iterate([Premise | Premises], [Percentage | Percentages], [Tip | Tips]) :- decomposition(Premise, _, _, Conclusion), iterate_conclusions(Conclusion, Percentage, Tip), print(Tip), second_iterate(Premises, Percentages, Tips).

main :- read_pipeline(Premises), print(Premises), iterate(Premises, [8, 5], Results), print(Results), second_iterate(Premises, Results, Tips), print(Tips), sumlist(Tips, Sum), sumlist(Results, Weight), FinalTip is Sum / Weight, print(FinalTip).