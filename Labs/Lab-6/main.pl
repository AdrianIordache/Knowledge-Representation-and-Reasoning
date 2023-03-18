read_pipeline(X) :- 
see('/home/adrian/Desktop/Python/Personal/Master/Master-Projects/First-Year/Knowledge-Representation-and-Reasoning/Labs/Lab-6/input.txt'),
read(X), seen.

print(Msg, X) :- write(Msg), write(X), nl.
print(X) :- write(X), nl.

ask(Question, Prompt, Response) :- write(Question), prompt(_, Prompt), nl, read(Response), nl.
round(X,Y,D) :- Z is X * 10^D, round(Z, ZA), Y is ZA / 10^D.

do_continue(Function) :- ask('Do you want to continue?', 'Type answer (yes/stop): ', Stop), Stop \= stop, call(Function).
do_continue(_) :- ask('Are you sure?', 'Type answer (yes/no): ', Stop), Stop = yes.

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


interval([0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 25, 26]).

or(List, Result)  :- max_list(List, Result).
and(List, Result) :- min_list(List, Result).

decomposition([Operator, Conditions, Conclusion], Operator, Conditions, Conclusion).

iterate_conditions([], _, []).
iterate_conditions([(_ / Degree) | Conditions], [Score | Scores], [Percent | Result]) :- print("Function Applied: ", Degree), print("X: ", Score), call(Degree, Score, Percent), print("Y: ", Percent), iterate_conditions(Conditions, Scores, Result).

compute_intervals([], _, []).
compute_intervals([X | Xs], Degree, [(X, Percent) | Results]) :- call(Degree, X, Percent), compute_intervals(Xs, Degree, Results).

argmin([], _, R, R). 
argmin([(X, Y) | Points], Min, _, R):- Y <  Min, argmin(Points, Y, X, R).
argmin([(_, Y) | Points], Min, Idx, R):- Y >= Min, argmin(Points, Min, Idx, R).
argmin([(X, Y) | Points], R):- argmin(Points, Y, X, R). 


iterate([], _, []).
iterate([Premise | Premises], Scores, [Result | Results]) :- 
							print(Premise), decomposition(Premise, Operator, Conditions, _), 
							iterate_conditions(Conditions, Scores, Percentages), 
							call(Operator, Percentages, Result), print("Operator Applied: ", Operator), print("Result: ", Result),
							iterate(Premises, Scores, Results).


remap_function(Function, Threshold, X, R) :- call(Function, X, Y), R is min(Y, Threshold).

construct_function_(_, [], [], []).
construct_function_(X, [Premise | Premises], [Threshold | Thresholds], [Y | Ys]) :- 
										decomposition(Premise, _, _, [_ / Degree]), 
										remap_function(Degree, Threshold, X, Y),
										construct_function_(X, Premises, Thresholds, Ys). 

construct_function([], _, _, []).
construct_function([X | Xs], Premises, Thresholds, [(X, Y) | Points]) :- construct_function_(X, Premises, Thresholds, Values), or(Values, Y), construct_function(Xs, Premises, Thresholds, Points).

extract_ys([], []).
extract_ys([(_, Y) | Points], [Y | Ys]) :- extract_ys(Points, Ys).

compute_distances([], _, []).
compute_distances([(X, Y) | Points], Objective, [(X, Rounded) | Distances]) :- Aux is Objective - Y, abs(Aux, Distance), round(Distance, Rounded, 2), compute_distances(Points, Objective, Distances).

main :- 
	ask('How would you score the services?', 'Type the score (answer is a number between 0 - 10): ', ServiceScore), ServiceScore \= stop,
	ask('How would you score the food?', 'Type the score (answer is a number 0 - 10): ', FoodScore), FoodScore \= stop,

	read_pipeline(Premises), print(Premises),
	iterate(Premises, [ServiceScore, FoodScore], Thresholds), print("Thresholds for the cut: ", Thresholds),
	interval(Domain), print("Tip Domain: ", Domain),
	construct_function(Domain, Premises, Thresholds, Points), print("Points After Remapping: ", Points),
	extract_ys(Points, Ys),
	sumlist(Ys, Sum), Objective is Sum / 2, round(Objective, Rounded, 2), print("Centroid: ", Rounded),
	compute_distances(Points, Objective, Distances), print("Distances from centroid: ", Distances),
	argmin(Distances, Price), print("Recommended Tip: ", Price),
	do_continue(main).