read_pipeline_resolution(X) :- 
see('input_resolution.txt'),
read(X), seen.

read_pipeline_davis_putman(X) :- 
see('input_davis_putman.txt'),
read(X), seen.


is_trivial_clause([], 'No').

is_trivial_clause(Clause, 'Yes') :- 
member(Lit, Clause), 
member(n(Lit), Clause).

is_trivial_clause(_, 'No').

remove_trivial_clauses([], []).
remove_trivial_clauses([Clause | Clauses], Filtered) :-
is_trivial_clause(Clause, 'Yes'),
remove_trivial_clauses(Clauses, Filtered).

remove_trivial_clauses([Clause | Clauses], [Clause | Filtered]) :- 
is_trivial_clause(Clause, 'No'),
remove_trivial_clauses(Clauses, Filtered).



get_literals_from_clause_([], Literals, Literals).
get_literals_from_clause_([n(Lit) | Lits], Literals, Aux) :- 
get_literals_from_clause_(Lits, Literals, [Lit | Aux]).

get_literals_from_clause_([Lit | Lits], Literals, Aux) :- 
get_literals_from_clause_(Lits, Literals, [Lit | Aux]).

get_literals_from_clause([], []).
get_literals_from_clause(Clause, Literals) :- 
get_literals_from_clause_(Clause, Literals, []).

get_literals_from_clauses_([], Literals, Aux) :- 
sort(Aux, Literals).

get_literals_from_clauses_([Clause | Clauses], Literals, Aux) :- 
get_literals_from_clause(Clause, Lits),
append(Lits, Aux, Union),

get_literals_from_clauses_(Clauses, Literals, Union).

get_literals_from_clauses([], []).
get_literals_from_clauses(Clauses, Lits) :- 
get_literals_from_clauses_(Clauses, Lits, []).


find_literal_in_clauses([], _, []).
find_literal_in_clauses([Clause | _], Lit, Clause) :- 
member(Lit, Clause), !.

find_literal_in_clauses([Clause | Clauses], Lit, Result) :- 
not(member(Lit, Clause)), 
find_literal_in_clauses(Clauses, Lit, Result).

find_resolvent(_, [], [], [], _).
find_resolvent(Clauses, [Lit | _], PositiveClause, NegativeClause, Lit) :- 
find_literal_in_clauses(Clauses, Lit, PositiveClause), 
find_literal_in_clauses(Clauses, n(Lit), NegativeClause),
PositiveClause \= [], NegativeClause \= [], !.

find_resolvent(Clauses, [_ | Lits], PositiveClause, NegativeClause, Lit) :- 
find_resolvent(Clauses, Lits, PositiveClause, NegativeClause, Lit).

resolution([], "Satisfiable") :- !.

resolution(Clauses, 'Unsatisfiable') :- member([], Clauses), !.

resolution(Clauses, 'Satisfiable') :- 
remove_trivial_clauses(Clauses, Filtered),
get_literals_from_clauses(Filtered, Lits),
find_resolvent(Filtered, Lits, PositiveClause, NegativeClause, _),
PositiveClause = [], NegativeClause = [].


resolution(Clauses, Answer) :- 
remove_trivial_clauses(Clauses, Filtered), 
get_literals_from_clauses(Filtered, Lits), 
find_resolvent(Filtered, Lits, PositiveClause, NegativeClause, Chosen),
PositiveClause \= [], NegativeClause \= [],
delete(PositiveClause, Chosen, RemovedPos),
delete(NegativeClause, n(Chosen), RemovedNeg),
union(RemovedPos, RemovedNeg, Result),
delete(Filtered, PositiveClause, RemovedPositives),
delete(RemovedPositives, NegativeClause, RemovedNegatives),
union(RemovedNegatives, [Result], NewClauses),
resolution(NewClauses, Answer).


main_resolution :- 
read_pipeline_resolution(Clauses), 
write("Input Clauses: "), nl, write(Clauses), nl, 
resolution(Clauses, Answer), 
write("Answer: "), write(Answer), nl.







clauses_with_literal(_, [], []).
clauses_with_literal(Lit, [Clause | Clauses], Result) :- 
not(member(Lit, Clause)), 
clauses_with_literal(Lit, Clauses, Result).

clauses_with_literal(Lit, [Clause | Clauses], [Clause | Result]) :- 
member(Lit, Clause), 
clauses_with_literal(Lit, Clauses, Result).

delete_from_clauses(_, [], []).
delete_from_clauses(Lit, [Clause | Clauses], [Removed | Result]) :- 
delete(Clause, Lit, Removed), 
delete_from_clauses(Lit, Clauses, Result).

clauses_without_literal(_, [], []).
clauses_without_literal(Lit, [Clause | Clauses], Result) :-
member(Lit, Clause), 
clauses_without_literal(Lit, Clauses, Result).

clauses_without_literal(Lit, [Clause | Clauses], Result) :- 
member(n(Lit), Clause), 
clauses_without_literal(Lit, Clauses, Result).

clauses_without_literal(Lit, [Clause | Clauses], [Clause | Result]) :- 
not(member(Lit, Clause)), 
not(member(n(Lit), Clause)), 
clauses_without_literal(Lit, Clauses, Result).

negation(n(Lit), Lit) :- !.
negation(Lit, n(Lit)).


dot(Clauses, Lit, OutputClauses) :- 
negation(Lit, LitN),
clauses_without_literal(Lit, Clauses, Without), 
clauses_with_literal(LitN, Clauses, With),
delete_from_clauses(LitN, With, Removed),
union(Without, Removed, OutputClauses).


davis_putman([], _, [], "Satisfiable") :- !.

davis_putman(Clauses, _, _, "Unsatisfiable") :- member([], Clauses), !.

davis_putman(Clauses, [Lit | Lits], [(Lit, "True") | Solution], "Satisfiable") :- 
dot(Clauses, Lit, OutputClauses),
davis_putman(OutputClauses, Lits, Solution, "Satisfiable"), !.

																	
davis_putman(Clauses, [Lit | Lits], [(Lit, "False") | Solution], "Satisfiable") :- 
dot(Clauses, n(Lit), OutputClauses),
davis_putman(OutputClauses, Lits, Solution, "Satisfiable"), !.

davis_putman(_, _, [], "Unsatisfiable").


find_frequency_literal(_, [], 0).
find_frequency_literal(Lit, [Clause | Clauses], Freq) :- 
member(Lit, Clause),  
find_frequency_literal(Lit, Clauses, Aux), 
Freq is Aux + 1.

find_frequency_literal(Lit, [Clause | Clauses], Freq) :- 
member(n(Lit), Clause), 
find_frequency_literal(Lit, Clauses, Aux),
Freq is Aux + 1.

find_frequency_literal(Lit, [Clause | Clauses], Freq) :- 
not(member(Lit, Clause)),
not(member(n(Lit), Clause)), 
find_frequency_literal(Lit, Clauses, Freq).


find_frequency(_, [], []).
find_frequency(Clauses, [Lit | Lits], [(Lit, Freq) | Result]) :- 
find_frequency_literal(Lit, Clauses, Freq), 
find_frequency(Clauses, Lits, Result).


extract_literals([], []).
extract_literals([(Lit, _) | Rest], [Lit | Extracted]) :- 
extract_literals(Rest, Extracted).




main_davis_putman :- 
read_pipeline_davis_putman(Clauses), 
remove_trivial_clauses(Clauses, Filtered), 
get_literals_from_clauses(Filtered, Lits), 
davis_putman(Filtered, Lits, Solution, Ans), 
write(Ans), nl, write(Solution), nl.



main_davis_putman_max_frequency :- 
read_pipeline_davis_putman(Clauses), 
remove_trivial_clauses(Clauses, Filtered), 
get_literals_from_clauses(Filtered, Lits), 
find_frequency(Filtered, Lits, Preprocessed),
sort(2, @>=, Preprocessed, Sorted),
extract_literals(Sorted, SortedLiterals),
davis_putman(Filtered, SortedLiterals, Solution, Ans), 
write(Ans), nl, write(Solution), nl.


main_davis_putman_min_frequency :- 
read_pipeline_davis_putman(Clauses), 
remove_trivial_clauses(Clauses, Filtered),
get_literals_from_clauses(Filtered, Lits), 
find_frequency(Filtered, Lits, Preprocessed),
sort(2, @=<, Preprocessed, Sorted),
extract_literals(Sorted, SortedLiterals),
davis_putman(Filtered, SortedLiterals, Solution, Ans),
 write(Ans), nl, write(Solution), nl.



