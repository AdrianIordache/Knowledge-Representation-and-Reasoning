read_pipeline_back_chain(X) :- 
see('/home/adrian/Desktop/Python/Personal/Master/Master-Projects/First-Year/Knowledge-Representation-and-Reasoning/Labs/Lab-5/InputBack.txt'),
read(X), seen.


read_pipeline_forward_chain(X) :- 
see('/home/adrian/Desktop/Python/Personal/Master/Master-Projects/First-Year/Knowledge-Representation-and-Reasoning/Labs/Lab-5/InputForward.txt'),
read(X), seen.

do_continue(Function) :- ask('Do you want to continue?', 'Type answer (yes/stop): ', Stop), Stop \= stop, call(Function).
do_continue(_) :- ask('Are you sure?', 'Type answer (yes/no): ', Stop), Stop = yes.

print(Msg, X) :- write(Msg), write(X), nl.
print(X) :- write(X), nl.

ask(Question, Prompt, Response) :- write(Question), prompt(_, Prompt), nl, read(Response), nl.

temperature_higher_than_38(Temperature, true) :- Temperature > 38, !.
temperature_higher_than_38(_, false).

patient_sick_for_at_least_2_days(Days, true) :- Days > 2, !.
patient_sick_for_at_least_2_days(_, false).
 
has_muscle_pain(yes, true).
has_muscle_pain(no, false).

has_cough(yes, true).
has_cough(no, false).


update_answer_list(true, Value, List, Out) :- append(List, Value, Out), !.
update_answer_list(false, _, List, List).

add_answears_to_list(TemperatureAnswer, DaysAnswer, HasMusclePainAnswer, HasCoughAnswer, List) :- add_answears_to_list_(TemperatureAnswer, DaysAnswer, HasMusclePainAnswer, HasCoughAnswer, [], List).

add_answears_to_list_(TemperatureAnswer, DaysAnswer, HasMusclePainAnswer, HasCoughAnswer, Vid, List) :- 
	update_answer_list(TemperatureAnswer, [[temperature]], Vid, AddedTemperature),
	update_answer_list(DaysAnswer, [[sick]], AddedTemperature, AddedDays),
	update_answer_list(HasMusclePainAnswer, [[musclepain]], AddedDays, AddedMusclePain),
	update_answer_list(HasCoughAnswer, [[cough]], AddedMusclePain, AddedCough),
	List = AddedCough.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

backward_chaining([], _, "YES") :- !.
backward_chaining(Literals, KB, Answer) :- for_each_clause(Literals, KB, KB, Answer).

for_each_clause(_, [], _, "NO") :- !.

for_each_clause([Lit | Lits], [Clause | _], KB, Answer) :- member(Lit, Clause), print(Clause), 
													select(Lit, Clause, Removed), print(Removed),
													negate_list(Lits, Removed, Resulted), print(Resulted), 
													backward_chaining(Resulted, KB, Answer).

                                     	
for_each_clause([Lit | Lits], [Clause | Clauses], KB, Answer) :- not(member(Lit, Clause)), 
																for_each_clause([Lit | Lits], Clauses, KB, Answer). 

negate_list(Sentences, [], Sentences) :- !. 
negate_list(Sentences, [n(P) | Tail], [P | Resulted]) :- negate_list(Sentences, Tail, Resulted). 


main_backward :- 
		ask('What is patient temperature?', 'Type a temperature (answer is a number): ', Temperature), Temperature \= stop, 
		ask('For how many days has the patient been sick?', 'Type the number of days (answer is a number): ', Days), Days \= stop,
		ask('Has patient muscle pain?', 'Type answer (yes/no): ', HasMusclePain), HasMusclePain \= stop,
		ask('Has patient cough?', 'Type answer (yes/no): ', HasCough), HasCough \= stop,

		temperature_higher_than_38(Temperature, TemperatureAnswer),
		patient_sick_for_at_least_2_days(Days, DaysAnswer),
		has_muscle_pain(HasMusclePain, HasMusclePainAnswer),
		has_cough(HasCough, HasCoughAnswer),

		read_pipeline_back_chain(ClausesKB),
		add_answears_to_list(TemperatureAnswer, DaysAnswer, HasMusclePainAnswer, HasCoughAnswer, Clauses),
		union(Clauses, ClausesKB, KB),

		print("Backward Chaining Solution"),
		print(KB),
		backward_chaining([pneumonia], KB, Ans), 
		print("Answer: ", Ans),
		do_continue(main_backward).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_literals_from_clause_([], Literals, Literals).
get_literals_from_clause_([n(Lit) | Lits], Literals, Aux) :- get_literals_from_clause_(Lits, Literals, [Lit | Aux]).
get_literals_from_clause_([Lit | Lits], Literals, Aux) :- get_literals_from_clause_(Lits, Literals, [Lit | Aux]).

get_literals_from_clause([], []).
get_literals_from_clause(Clause, Literals) :- get_literals_from_clause_(Clause, Literals, []).

get_literals_from_clauses_([], Literals, Aux) :- sort(Aux, Literals).
get_literals_from_clauses_([Clause | Clauses], Literals, Aux) :- get_literals_from_clause(Clause, Lits), append(Lits, Aux, Union), get_literals_from_clauses_(Clauses, Literals, Union).

get_literals_from_clauses([], []).
get_literals_from_clauses(Clauses, Lits) :- get_literals_from_clauses_(Clauses, Lits, []).

initialize_unsolved([], []) :- !.
initialize_unsolved([Lit | Lits], [(Lit, 0) | Result]) :- initialize_unsolved(Lits, Result).

check_all_goals_are_solved([], _).
check_all_goals_are_solved([Lit | Lits], Solved) :- member((Lit, 1), Solved), check_all_goals_are_solved(Lits, Solved), !.

how_many_negatives([], 0) :- !.
how_many_negatives([n(_) | Lits], Result) :- how_many_negatives(Lits, Aux), Result is Aux + 1, !.
how_many_negatives([_ | Lits], Result) :- how_many_negatives(Lits, Result), !.

get_negative_literals([], []).
get_negative_literals([n(Lit) | Lits], [Lit | Result]) :- get_negative_literals(Lits, Result), !.
get_negative_literals([_ | Lits], Result) :- get_negative_literals(Lits, Result), !.


get_positive_literals([], []).
get_positive_literals([n(_) | Lits], Result) :- get_positive_literals(Lits, Result), !.
get_positive_literals([Lit | Lits], [Lit | Result]) :- get_positive_literals(Lits, Result), !.


get_positive_literal([], []).
get_positive_literal([n(_) | Lits], Result) :- get_positive_literal(Lits, Result), !.
get_positive_literal([Lit | _], Lit) :- !.

check_negatives_solved([], _, "YES").
check_negatives_solved([Lit | Lits], Unsolved, Ans) :- member((Lit, 1), Unsolved), check_negatives_solved(Lits, Unsolved, Ans), !. 
check_negatives_solved([Lit | _], Unsolved, "NO") :- not(member((Lit, 1), Unsolved)), !. 


check_positive_unsolved([], _, "YES").
check_positive_unsolved([Lit | Lits], Unsolved, Ans) :- member((Lit, 0), Unsolved), check_positive_unsolved(Lits, Unsolved, Ans), !. 
check_positive_unsolved([Lit | _], Unsolved, "NO") :- not(member((Lit, 0), Unsolved)), !. 


number_condition(Clause, "YES") :- how_many_negatives(Clause, NegativesCounter), length(Clause, Counter), 
									AssumedNegative is Counter - 1, NegativesCounter == AssumedNegative, !.

number_condition(_, "NO").

solved_condition(Clause, Unsolved, "YES") :- get_negative_literals(Clause, Lits), check_negatives_solved(Lits, Unsolved, "YES"),
										get_positive_literals(Clause, Pos), check_positive_unsolved(Pos, Unsolved, "YES"), !.

solved_condition(_, _, "NO").

check_conditions(Clause, Unsolved, "YES") :- number_condition(Clause, "YES"), solved_condition(Clause, Unsolved, "YES"), !. 
check_conditions(_, _, "NO").

select_clause([], _, []).
select_clause([Clause | _], Unsolved, Clause) :- check_conditions(Clause, Unsolved, "YES"), !.
select_clause([_ | Clauses], Unsolved, Result) :- select_clause(Clauses, Unsolved, Result), !.

forward_chaining_(Goal, Unsolved, _, "YES") :- check_all_goals_are_solved(Goal, Unsolved), !.
forward_chaining_(Goal, Unsolved, KB, Ans) :- select_clause(KB, Unsolved, Selected), Selected \= [],
												print("Selected Clause: ", Selected), 
												get_positive_literal(Selected, Pos), 
												print("Positive Literal: ", Pos),
												delete(Unsolved, (Pos, 0), Aux1), append(Aux1, [(Pos, 1)], Aux2),
												print("Unsolved: ", Aux2),
                             					forward_chaining_(Goal, Aux2, KB, Ans), !.

forward_chaining_(_, Unsolved, KB, "NO") :-  select_clause(KB, Unsolved, Selected), print("Selected Vid -> Out: ", Selected), Selected == [], !.

forward_chaining(Goal, KB, Ans) :- append([Goal], KB, KBComplete), print("Complete KB: ", KBComplete),
									  get_literals_from_clauses(KBComplete, Literals), print("Literals: ", Literals),
									  initialize_unsolved(Literals, Unsolved), print("Init Unsolved: ", Unsolved), 
									  forward_chaining_(Goal, Unsolved, KB, Ans), !.

main_forward :- 
		ask('What is patient temperature?', 'Type a temperature (answer is a number): ', Temperature), Temperature \= stop,
		ask('For how many days has the patient been sick?', 'Type the number of days (answer is a number): ', Days), Days \= stop,
		ask('Has patient muscle pain?', 'Type answer (yes/no): ', HasMusclePain), HasMusclePain \= stop,
		ask('Has patient cough?', 'Type answer (yes/no): ', HasCough), HasCough \= stop,

		temperature_higher_than_38(Temperature, TemperatureAnswer),
		patient_sick_for_at_least_2_days(Days, DaysAnswer),
		has_muscle_pain(HasMusclePain, HasMusclePainAnswer),
		has_cough(HasCough, HasCoughAnswer),

		read_pipeline_forward_chain(ClausesKB),
		add_answears_to_list(TemperatureAnswer, DaysAnswer, HasMusclePainAnswer, HasCoughAnswer, Clauses),
		union(Clauses, ClausesKB, KB),

		print("Forward Chaining Solution"),
		print("Input KB: ", KB),
		forward_chaining([pneumonia], KB, Ans),
		print("Answer: ", Ans),
		do_continue(main_forward).
