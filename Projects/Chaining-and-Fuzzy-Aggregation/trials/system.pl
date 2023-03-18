read_pipeline_fuzzy(X) :- 
see('Input-2.txt'),
read(X), seen.

print(Msg, X) :- write(Msg), write(X), nl.
print(X) :- write(X), nl.

ask(Question, Prompt, Response) :- write(Question), prompt(_, Prompt), nl, read(Response), nl.
round(X,Y,D) :- Z is X * 10^D, round(Z, ZA), Y is ZA / 10^D.

small(Surface, Percent) :- Surface < 10, Percent is 1.0, !.
small(Surface, Percent) :- Surface < 20, Percent is 0.8, !.
small(Surface, Percent) :- Surface < 30, Percent is 0.6, !.
small(Surface, Percent) :- Surface < 40, Percent is 0.4, !.
small(Surface, Percent) :- Surface < 50, Percent is 0.2, !.
small(_, Percent) :- Percent is 0.0, !.


medium(Surface, Percent) :- Surface < 30, Percent is 0.0, !.
medium(Surface, Percent) :- Surface < 40, Percent is 0.3, !.
medium(Surface, Percent) :- Surface < 50, Percent is 0.6, !.
medium(Surface, Percent) :- Surface < 60, Percent is 1.0, !.
medium(Surface, Percent) :- Surface < 70, Percent is 0.6, !.
medium(Surface, Percent) :- Surface < 80, Percent is 0.3, !.
medium(_, Percent) :- Percent is 0.0, !.


large(Surface, Percent) :- Surface < 50,  Percent is 0.0, !.
large(Surface, Percent) :- Surface < 60,  Percent is 0.2, !.
large(Surface, Percent) :- Surface < 70,  Percent is 0.4, !.
large(Surface, Percent) :- Surface < 80,  Percent is 0.6, !.
large(Surface, Percent) :- Surface < 90,  Percent is 0.8, !.
large(Surface, Percent) :- Surface < 100, Percent is 1.0, !.
large(_, Percent) :- Percent is 1.0, !.


closer(Distance, Percent) :- Distance < 1, Percent is 1.0, !.
closer(Distance, Percent) :- Distance < 2, Percent is 0.8, !.
closer(Distance, Percent) :- Distance < 3, Percent is 0.6, !.
closer(Distance, Percent) :- Distance < 4, Percent is 0.4, !.
closer(Distance, Percent) :- Distance < 5, Percent is 0.2, !.
closer(Distance, Percent) :- Distance < 6, Percent is 0.1, !.
closer(_, Percent) :- Percent is 0.0, !.


far(Distance, Percent) :- Distance < 4, Percent is 0.0, !.
far(Distance, Percent) :- Distance < 5, Percent is 0.1, !.
far(Distance, Percent) :- Distance < 6, Percent is 0.2, !.
far(Distance, Percent) :- Distance < 7, Percent is 0.4, !.
far(Distance, Percent) :- Distance < 8, Percent is 0.6, !.
far(Distance, Percent) :- Distance < 9, Percent is 0.8, !.
far(Distance, Percent) :- Distance < 10, Percent is 1.0, !.
far(_, Percent) :- Percent is 1.0, !.



cheap(Price, Percent) :- Price < 20, Percent is 1.0, !.
cheap(Price, Percent) :- Price < 30, Percent is 0.8, !.
cheap(Price, Percent) :- Price < 40, Percent is 0.6, !.
cheap(Price, Percent) :- Price < 50, Percent is 0.4, !.
cheap(Price, Percent) :- Price < 60, Percent is 0.2, !.
cheap(_, Percent) :- Percent is 0.0, !.

normal(Price, Percent) :- Price < 40, Percent is 0.0, !.
normal(Price, Percent) :- Price < 50, Percent is 0.3, !.
normal(Price, Percent) :- Price < 60, Percent is 0.6, !.
normal(Price, Percent) :- Price < 70, Percent is 1.0, !.
normal(Price, Percent) :- Price < 80, Percent is 0.6, !.
normal(Price, Percent) :- Price < 90, Percent is 0.3, !.
normal(_, Percent) :- Percent is 0.0, !.

expensive(Price, Percent) :- Price < 70, Percent is 0.0, !.
expensive(Price, Percent) :- Price < 80, Percent is 0.3, !.
expensive(Price, Percent) :- Price < 90, Percent is 0.6, !.
expensive(Price, Percent) :- Price < 100, Percent is 1.0, !.
expensive(Price, Percent) :- Price < 110, Percent is 0.6, !.
expensive(Price, Percent) :- Price < 120, Percent is 0.3, !.
expensive(_, Percent) :- Percent is 0.0, !.

interval([20, 30, 40, 50, 60, 70, 80, 90, 100, 110, 120]).

or(List, Result)  :- max_list(List, Result).
and(List, Result) :- min_list(List, Result).

decomposition([Operator, Conditions, Conclusion], Operator, Conditions, Conclusion).

iterate_conditions([], _, []).
iterate_conditions([(_ / Degree) | Conditions], [Score | Scores], [Percent | Result]) :- print("Function Applied: ", Degree), print("X: ", Score), call(Degree, Score, Percent), print("Y: ", Percent), iterate_conditions(Conditions, Scores, Result).

argmin([], _, R, R). 
argmin([(X, Y) | Points], Min, Idx, R):- Y <  Min, argmin(Points, Y, X, R).
argmin([(X, Y) | Points], Min, Idx, R):- Y >= Min, argmin(Points, Min, Idx, R).
argmin([(X, Y) | Points], R):- argmin(Points, Y, X, R). 

iterate([], _, []).
iterate([Premise | Premises], Scores, [Result | Results]) :- print(Premise), decomposition(Premise, Operator, Conditions, _), 
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

compute_distances([], _, []).
compute_distances([(X, Y) | Points], Objective, [(X, Rounded) | Distances]) :- Aux is Objective - Y, abs(Aux, Distance), round(Distance, Rounded, 2), compute_distances(Points, Objective, Distances).

extract_ys([], []).
extract_ys([(_, Y) | Points], [Y | Ys]) :- extract_ys(Points, Ys).

main_fuzzy :- 
			ask('What surface do you want the apartment to be?', 'Type the surface in squared meters (answer is a number between 0 - 100): ', Surface), Surface \= stop,
			ask('How far from the center of the city you want the apartment?', 'Type the distance from the center in km (answer is a number 0 - 10): ', Distance), Distance \= stop,

			read_pipeline_fuzzy(Premises), print(Premises),
			iterate(Premises, [Surface, Distance], Thresholds), print("Thresholds for the cut: ", Thresholds),
			interval(Domain), print("Price Domain: ", Domain),
			construct_function(Domain, Premises, Thresholds, Points), print("Points After Remapping: ", Points),
			extract_ys(Points, Ys),
			sumlist(Ys, Sum), Objective is Sum / 2, round(Objective, Rounded, 2), print("Centroid: ", Rounded),
			compute_distances(Points, Objective, Distances), print("Distances from centroid: ", Distances),
			argmin(Distances, Price), print("Recommended Price: ", Price).

