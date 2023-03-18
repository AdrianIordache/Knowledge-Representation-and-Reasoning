read_pipeline_resolution(X) :- see('/home/adrian/Desktop/Python/Personal/Master/Master-Projects/First-Year/Knowledge-Representation-and-Reasoning/Labs/Project-1/KB.txt'),
								read(X), seen.


is_trivial_clause([], 'No').
is_trivial_clause(Clause, 'Yes') :- member(Lit, Clause), member(n(Lit), Clause).
is_trivial_clause(_, 'No').

remove_trivial_clauses([], []).
remove_trivial_clauses([Clause | Clauses], Filtered) :- is_trivial_clause(Clause, 'Yes'), remove_trivial_clauses(Clauses, Filtered).
remove_trivial_clauses([Clause | Clauses], [Clause | Filtered]) :- is_trivial_clause(Clause, 'No'), remove_trivial_clauses(Clauses, Filtered).



get_literals_from_clause_([], Literals, Literals).
get_literals_from_clause_([n(Lit) | Lits], Literals, Aux) :- get_literals_from_clause_(Lits, Literals, [Lit | Aux]).
get_literals_from_clause_([Lit | Lits], Literals, Aux) :- get_literals_from_clause_(Lits, Literals, [Lit | Aux]).

get_literals_from_clause([], []).
get_literals_from_clause(Clause, Literals) :- get_literals_from_clause_(Clause, Literals, []).

get_literals_from_clauses_([], Literals, Aux) :- sort(Aux, Literals).
get_literals_from_clauses_([Clause | Clauses], Literals, Aux) :- get_literals_from_clause(Clause, Lits), append(Lits, Aux, Union), get_literals_from_clauses_(Clauses, Literals, Union).

get_literals_from_clauses([], []).
get_literals_from_clauses(Clauses, Lits) :- get_literals_from_clauses_(Clauses, Lits, []).


% get_positive_clause([], _, []).
% get_positive_clause([Clause | _], Lit, Clause) :- member(Lit, Clause).
% get_positive_clause([_ | Clauses], Lit, Result) :- get_positive_clause(Clauses, Lit, Result).

% get_negative_clause([], _, []).
% get_negative_clause([Clause | _], Lit, Clause) :- member(n(Lit), Clause).
% get_negative_clause([_ | Clauses], Lit, Result) :- get_negative_clause(Clauses, Lit, Result).

find_literal_in_clauses([], _, []).
find_literal_in_clauses([Clause | _], Lit, Clause) :- member(Lit, Clause), !.
find_literal_in_clauses([Clause | Clauses], Lit, Result) :- not(member(Lit, Clause)), find_literal_in_clauses(Clauses, Lit, Result).


find_resolvent(_, [], [], [], _).
find_resolvent(Clauses, [Lit | _], PositiveClause, NegativeClause, Lit) :- find_literal_in_clauses(Clauses, Lit, PositiveClause), find_literal_in_clauses(Clauses, n(Lit), NegativeClause), PositiveClause \= [], NegativeClause \= [], !.
find_resolvent(Clauses, [_ | Lits], PositiveClause, NegativeClause, Lit) :- find_resolvent(Clauses, Lits, PositiveClause, NegativeClause, Lit).


resolution([], "Satisfiable") :- write("End Case 0 -> []"), nl, !.

resolution(Clauses, 'Unsatisfiable') :- member([], Clauses), write("End Case 1 -> [[]]"),  nl, !.

resolution(Clauses, 'Satisfiable') :- remove_trivial_clauses(Clauses, Filtered),
			                         get_literals_from_clauses(Filtered, Lits),
			                         find_resolvent(Filtered, Lits, PositiveClause, NegativeClause, _),
			                         PositiveClause = [], NegativeClause = [], write('End Case 2 -> [[], []]'), nl.


resolution(Clauses, Answer) :- remove_trivial_clauses(Clauses, Filtered), get_literals_from_clauses(Filtered, Lits), 
                                write("Recursive Case"), nl,
                                % write("Search resolvent in clauses: "), write(Filtered), nl,
                                find_resolvent(Filtered, Lits, PositiveClause, NegativeClause, Chosen),
                                write("Opposite Clauses Selected: "), nl, write(PositiveClause), nl, write(NegativeClause), nl,
                                PositiveClause \= [], NegativeClause \= [],
                                delete(PositiveClause, Chosen, RemovedPos),
                                delete(NegativeClause, n(Chosen), RemovedNeg),
                                union(RemovedPos, RemovedNeg, Result),
                                write("Resulted Clause: "), write(Result), nl,
                                delete(Filtered, PositiveClause, RemovedPositives),
                                delete(RemovedPositives, NegativeClause, RemovedNegatives),
                                union(RemovedNegatives, [Result], NewClauses),
                                % write("New set of clauses: "), write(NewClauses), nl,
                                resolution(NewClauses, Answer).


main_resolution :- read_pipeline_resolution(Clauses), resolution(Clauses, Answer), write(Answer).





