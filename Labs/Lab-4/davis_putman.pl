read_pipeline(X) :- see('/home/adrian/Desktop/Python/Personal/Master/Master-Projects/First-Year/Knowledge-Representation-and-Reasoning/Labs/Lab-4/input.txt'),
                    read(X), seen.

write_pipeline(X) :- tell('/home/adrian/Desktop/Python/Personal/Master/Master-Projects/First-Year/Knowledge-Representation-and-Reasoning/Labs/Lab-4/output.txt'),
                     write(X), told. 


% get literals from clause, after from set 

get_literals_from_clause_([], Res, Res).
get_literals_from_clause_([n(H) | T], Res, Aux) :- get_literals_from_clause_(T, Res, [H | Aux]).
get_literals_from_clause_([H | T], Res, Aux) :- get_literals_from_clause_(T, Res, [H | Aux]).

get_literals_from_clause([], []).
get_literals_from_clause(Clause, Res) :- get_literals_from_clause_(Clause, Res, []).

get_literals_from_set_([], Res, Aux) :- sort(Aux, Res).
get_literals_from_set_([H | T], Res, Aux) :- get_literals_from_clause(H, Lits), append(Lits, Aux, Z), get_literals_from_set_(T, Res, Z).

get_literals_from_set([], []).
get_literals_from_set(Cs, Lits) :- get_literals_from_set_(Cs, Lits, []).

% remove trivial clauses -> Example [[a, n(a)]]

is_trivial_clause([], 'No').
is_trivial_clause(C, 'Yes') :- member(A, C), member(n(A), C).
is_trivial_clause(_, 'No').

remove_trivial_clauses([], []).
remove_trivial_clauses([H | T], Res) :- is_trivial_clause(H, 'Yes'), remove_trivial_clauses(T, Res).
remove_trivial_clauses([H | T], [H | Res]) :- is_trivial_clause(H, 'No'), remove_trivial_clauses(T, Res).

% davis putman 

clauses_with_literal(_, [], []).
clauses_with_literal(Lit, [C | T], Result) :- not(member(Lit, C)), clauses_with_literal(Lit, T, Result).
clauses_with_literal(Lit, [C | T], [C | Result]) :- member(Lit, C), clauses_with_literal(Lit, T, Result).

delete_from_clauses(_, [], []).
delete_from_clauses(Lit, [C | T], [R | Result]) :- delete(C, Lit, R), delete_from_clauses(Lit, T, Result).

clauses_without_literal(_, [], []).
clauses_without_literal(Lit, [C | T], Result) :- member(Lit, C), clauses_without_literal(Lit, T, Result).
clauses_without_literal(Lit, [C | T], Result) :- member(n(Lit), C), clauses_without_literal(Lit, T, Result).
clauses_without_literal(Lit, [C | T], [C | Result]) :- not(member(Lit, C)), not(member(n(Lit), C)), clauses_without_literal(Lit, T, Result).

negation(n(Lit), Lit) :- !.
negation(Lit, n(Lit)).

dot(Clauses, Lit, OutputClauses) :- write("Dot Operation"), nl,
					write("Input Clauses: "), write(Clauses), nl,
					negation(Lit, LitN),
					clauses_without_literal(Lit, Clauses, Without), write("Without -> "), write(Without), nl,
					clauses_with_literal(LitN, Clauses, With), write("With Negation -> "), write(With), nl,
					delete_from_clauses(LitN, With, Removed), write("Removed Negation -> "), write(Removed), nl,
					union(Without, Removed, OutputClauses), write("New Clauses -> "), write(OutputClauses), nl.





davis_putman([], _, [], "Satisfiable") :- !.

davis_putman(Clauses, _, _, "Unsatisfiable") :- member([], Clauses), !.

davis_putman(Clauses, [Lit | Lits], [(Lit, "True") | Solution], "Satisfiable") :- 
																	write("Input Clauses: "), write(Clauses), nl,
																	write("Chosen Literal: "), write(Lit), nl,
																	write("Condition 1, DP(C * p)"), nl,
																	dot(Clauses, Lit, OutputClauses),
																	davis_putman(OutputClauses, Lits, Solution, "Satisfiable"), !.

																	
davis_putman(Clauses, [Lit | Lits], [(Lit, "False") | Solution], "Satisfiable") :- 
																	write("Input Clauses: "), write(Clauses), nl,
																	write("Chosen Literal: "), write(Lit), nl,
																	write("Condition 2, DP(C * n(p))"), nl,
																 	dot(Clauses, n(Lit), OutputClauses),
																 	davis_putman(OutputClauses, Lits, Solution, "Satisfiable"), !.

davis_putman(_, _, [], "Unsatisfiable").

find_frequency_literal(_, [], 0).
find_frequency_literal(Lit, [Clause | Clauses], Freq) :- member(Lit, Clause),  find_frequency_literal(Lit, Clauses, Aux), Freq is Aux + 1.
find_frequency_literal(Lit, [Clause | Clauses], Freq) :- member(n(Lit), Clause),  find_frequency_literal(Lit, Clauses, Aux), Freq is Aux + 1.
find_frequency_literal(Lit, [Clause | Clauses], Freq) :- not(member(Lit, Clause)), not(member(n(Lit), Clause)), find_frequency_literal(Lit, Clauses, Freq).

find_frequency(_, [], []).
find_frequency(Clauses, [Lit | Lits], [(Lit, Freq) | Result]) :- find_frequency_literal(Lit, Clauses, Freq), find_frequency(Clauses, Lits, Result).


extract_literals([], []).
extract_literals([(Lit, _) | Rest], [Lit | Extracted]) :- extract_literals(Rest, Extracted).

main :- read_pipeline(Clauses), remove_trivial_clauses(Clauses, Filtered), get_literals_from_set(Filtered, Lits), 
		davis_putman(Filtered, Lits, Solution, Ans), write(Ans), nl, write(Solution), nl.

main_2 :- read_pipeline(Clauses), remove_trivial_clauses(Clauses, Filtered), get_literals_from_set(Filtered, Lits), 
		find_frequency(Filtered, Lits, Preprocessed), write("Extracted Frequencies: "), write(Preprocessed), nl,
		sort(2, @>=, Preprocessed, Sorted), write("Sorted Frequencies: "), write(Sorted), nl,
		extract_literals(Sorted, SortedLiterals), write("Sorted Literals: "), write(SortedLiterals), nl,
		davis_putman(Filtered, SortedLiterals, Solution, Ans), write(Ans), nl, write(Solution), nl.


main_3 :- read_pipeline(Clauses), remove_trivial_clauses(Clauses, Filtered), get_literals_from_set(Filtered, Lits), 
		find_frequency(Filtered, Lits, Preprocessed), write("Extracted Frequencies: "), write(Preprocessed), nl,
		sort(2, @=<, Preprocessed, Sorted), write("Sorted Frequencies: "), write(Sorted), nl,
		extract_literals(Sorted, SortedLiterals), write("Sorted Literals: "), write(SortedLiterals), nl,
		davis_putman(Filtered, SortedLiterals, Solution, Ans), write(Ans), nl, write(Solution), nl.


