read_pipeline(X) :- see('/home/adrian/Desktop/Python/Personal/Master/Master-Projects/First-Year/Knowledge-Representation-and-Reasoning/Labs/Lab-3/input.txt'),
                    read(X), seen.

write_pipeline(X) :- tell('/home/adrian/Desktop/Python/Personal/Master/Master-Projects/First-Year/Knowledge-Representation-and-Reasoning/Labs/Lab-3/output.txt'),
                     write(X), told. 


% get oposite clauses 

get_positive_clause([], _, []).
get_positive_clause([H | _], Lit, H) :- member(Lit, H), !.
get_positive_clause([_ | T], Lit, Res) :- get_positive_clause(T, Lit, Res).

get_negative_clause([], _, []).
get_negative_clause([H | _], Lit, H) :- member(n(Lit), H), !.
get_negative_clause([_ | T], Lit, Res) :- get_negative_clause(T, Lit, Res).

find_resolvent(_, [], [], [], _).
find_resolvent(Clauses, [Lit | _], PositiveClause, NegativeClause, Lit) :- get_positive_clause(Clauses, Lit, PositiveClause), get_negative_clause(Clauses, Lit, NegativeClause), PositiveClause \= [], NegativeClause \= [], !.
find_resolvent(Clauses, [_ | T], PositiveClause, NegativeClause, Lit) :- find_resolvent(Clauses, T, PositiveClause, NegativeClause, Lit).

% get literals from clause, after from set 

get_literals_from_clause_([], Res, Res).
get_literals_from_clause_([n(H) | T], Res, Aux) :- get_literals_from_clause_(T, Res, [H | Aux]).
get_literals_from_clause_([H | T], Res, Aux) :- get_literals_from_clause_(T, Res, [H | Aux]).

get_literals_from_clause([], []).
get_literals_from_clause(Clause, Res) :- get_literals_from_clause_(Clause, Res, []).

get_literals_from_set_([], Res, Aux) :- sort(Aux, Res).
get_literals_from_set_([H | T], Res, Aux) :- get_literals_from_clause(H, Lits), append(Lits, Aux, Z), get_literals_from_set_(T, Res, Z).

get_literals_from_set([], []) :- !.
get_literals_from_set(Cs, Lits) :- get_literals_from_set_(Cs, Lits, []).

% remove trivial clauses -> Example [[a, n(a)]]

is_trivial_clause([], 'No').
is_trivial_clause(C, 'Yes') :- member(A, C), member(n(A), C).
is_trivial_clause(_, 'No').

remove_trivial_clauses([], []).
remove_trivial_clauses([H | T], Res) :- is_trivial_clause(H, 'Yes'), remove_trivial_clauses(T, Res).
remove_trivial_clauses([H | T], [H | Res]) :- is_trivial_clause(H, 'No'), remove_trivial_clauses(T, Res).

% start resolution pipeline

resolution_([], 'Satifiable') :- write("Trivial Case, Case 0"), nl, !.

resolution_([[]], 'Unsatisfiable') :- write("End Case 1 ([[]])"), nl.

% resolution_(C, 'Satisfiable') :- remove_trivial_clauses(C, Cs),
%                         get_literals_from_set(Cs, Lits),
%                         find_resolvent(Cs, Lits, PositiveClause, NegativeClause, _),
%                         PositiveClause = [], NegativeClause \= [], write("End Case 1 ([[], [n(p)]])"), nl.


% resolution_(C, 'Satisfiable') :- remove_trivial_clauses(C, Cs),
%                         get_literals_from_set(Cs, Lits),
%                         find_resolvent(Cs, Lits, PositiveClause, NegativeClause, _),
%                         PositiveClause \= [], NegativeClause = [], write('End Case 2 ([[p], []])'), nl.

resolution_(C, 'Unsatisfiable') :- member([], C), write('End Case 2 ([[p], []])'), nl, !.

resolution_(C, 'Satisfiable') :- remove_trivial_clauses(C, Cs),
                         get_literals_from_set(Cs, Lits),
                         find_resolvent(Cs, Lits, PositiveClause, NegativeClause, _),
                         PositiveClause = [], NegativeClause = [], write('End Case 3 ([[], []])'), nl.


resolution_(C, Ans) :- remove_trivial_clauses(C, Cs), get_literals_from_set(Cs, Lits), 
                                write("Recursive Case"), nl,
                                write("Search resolvent in clauses: "), write(Cs), nl,
                                find_resolvent(Cs, Lits, PositiveClause, NegativeClause, Chosen),
                                write("Opposite Clauses Selected: "), write(PositiveClause), write(NegativeClause), nl,
                                PositiveClause \= [], NegativeClause \= [],
                                delete(PositiveClause, Chosen, Result1),
                                delete(NegativeClause, n(Chosen), Result2),
                                union(Result1, Result2, Result),
                                write("Resulted Clause: "), write(Result), nl,
                                delete(Cs, PositiveClause, Result11),
                                delete(Result11, NegativeClause, Result22),
                                union(Result22, [Result], NewClauses),
                                write("New set of clauses: "), write(NewClauses), nl,
                                resolution_(NewClauses, Ans).

resolution(C, Ans) :- resolution_(C, Ans).

main :- read_pipeline(C), resolution(C, Ans), write(Ans).



