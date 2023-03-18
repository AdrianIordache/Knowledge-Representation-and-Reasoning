backwardChaining([], _, "YES") :- !.
backwardChaining(Literals, KB, Answer) :- forEachClause(Literals, KB, KB, Answer).


forEachClause(_, [], _, "NO") :- !.

forEachClause([Lit | Lits], [Clause | _], KB, Answer) :- 
								member(Lit, Clause), 
								write(Clause), nl, select(Lit, Clause, Removed),
								write(Removed), nl,
								negateList(Lits, Removed, Resulted), 
								write(Resulted), nl, backwardChaining(Resulted, KB, Answer).

                                     	
forEachClause([Lit | Lits], [Clause | Clauses], KB, Answer) :- not(member(Lit, Clause)), forEachClause([Lit | Lits], Clauses, KB, Answer). 

negateList(Sentences, [], Sentences) :- !. 
negateList(Sentences, [n(P) | Tail], [P | Resulted]) :- negateList(Sentences, Tail, Resulted).  


$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$


checkAllNegativeClause([]).
checkAllNegativeClause([n(_)|T]) :- checkAllNegativeClause(T).

singleNegativeClause(R, L) :- member(R, L), 
    						  delete(L, R, NL),
                              checkAllNegativeClause(NL), !.

negateList([], []).
negateList([n(U)|T], [U|R]) :- negateList(T, R).

backwardChaining([], _, _) :- !.

backwardChaining([H|T], KB, [KBH|_]) :- singleNegativeClause(H, KBH),
                                        delete(KBH, H, Aux),
                                        negateList(Aux, Aux2),
                                        append(Aux2, T, Aux3),
                                        backwardChaining(Aux3, KB, KB), !.

backwardChaining([H|T], KB, [_|KBT]) :- backwardChaining([H|T], KB, KBT), !.


hasPneumonia(KB) :- backwardChaining([pneumonia], KB, KB),
					write('Backward Chaining: '), nl, 
    				write('The patient has pneumonia'), nl, !.

hasPneumonia(_) :- write('Backward Chaining: '), nl, 
                   write('patient is ok'), nl.


[[n(cough),n(infection), pneumonia], [n(temperature),fever], [n(fever),n(sick),infection]].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

               
allGoalsAreSolved([], _) :- !.                                                              
allGoalsAreSolved([Lit | Literals], Solved) :- member(Lit, Solved), solvedGoal(Literals, Solved).      

allNegativesAreSolved([], _) :- !.
allNegativesAreSolved([n(Lit) | Literals], Solved) :- member(Lit, Solved), allNegativesAreSolved(Literals, Solved).

forClause(_, _, [], _, "NO") :- !.
forClause(Question, Solved, [[Lit | Clause] | Clauses], KB, Ans) :- allNegativesAreSolved(Clause, Solved), 
																not(member(Lit, Solved)),
																write("Lit "), write(Lit), nl,
																write("Clause "), write(Clause), nl,
																union(Lit, Solved, Resolved),
																write("Resolved "), write(Resolved), nl,
																forwardChaining(Question, Resolved, KB, Ans).

forClause(Question, Solved, [_ | Clauses], KB, Ans) :- forClause(Question, Solved, Clauses, KB, Ans). 

forwardChaining(Lits, Solved, _, "YES") :- allGoalsAreSolved(Lits, Solved).
forwardChaining(Lits, Solved, KB, Ans)  :- forClause(Lits, Solved, KB, KB, Ans).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


how_many_negatives([], 0) :- !.
how_many_negatives([n(_) | Lits], Result) :- how_many_negatives(Lits, Aux), Result is Aux + 1, !.
how_many_negatives([_ | Lits], Result) :- how_many_negatives(Lits, Result), !.

condition_1(Clause, "YES") :- how_many_negatives(Clause, NegativesCounter), print("Negative: ", NegativesCounter), length(Clause, Counter), print("Counter: ", Counter), NegativesCounter + 1 == Counter, !.
condition_1(_, "NO").

condition_2(Clause, "YES") :- 
condition_2(_, "NO").

check_conditions(Clause, "YES") :- condition_1(Clause, "YES"), condition_2(Clause, "YES"), !. 
check_conditions(_, "NO").

select_clause([], _, []).
select_clause([Clause | _], Unsolved, Clause):- print("Clause YES: ", Clause), check_conditions(Clause, "YES"), print("Selected YES: ", Clause), !.
select_clause([_ | Clauses], Unsolved, Result) :- select_clause(Clauses, Unsolved, Result), !.

forwardChaining_(Question, Unsolved) :- check_all_goals_are_solved(Question, Unsolved), !.
forwardChaining_(Question, Unsolved, KB) :- select_clause(KB, Unsolved, Selected), print("Selected: ", Selected).



select_clause([], _, []).
select_clause([Clause | _], Unsolved, Clause)	:- print("Clause YES: ", Clause),  
													how_many_negatives(Clause, NegativesCounter), 
													print("Negative: ", NegativesCounter), 
													length(Clause, Counter), 
													print("Counter: ", Counter), 
													A is Counter - 1, 
													print("A: ", A), 
													NegativesCounter == A,
													print("CEVA"), !.