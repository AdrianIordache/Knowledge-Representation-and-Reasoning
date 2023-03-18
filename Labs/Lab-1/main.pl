parent(ion, maria).
parent(ana, maria).
parent(ana, dan).
parent(maria, elena).
parent(maria, radu).
parent(elena, nicu).
parent(radu, george).
parent(radu, dragos).

child(X, Y) :- parent(Y, X).
brother(X, Y) :- parent(X, Z), parent(Y, Z).
grandparent(X, Y) :- parent(X, Z), parent(Z, Y).

pred(X, Y) :- parent(X, Y).
pred(X, Z) :- parent(X, Y), pred(Y, Z).

% 1)
maxim(X, Y, Z) :- X < Y, Z is Y.
maxim(X, Y, Z) :- X >= Y, Z is X.

% 2)
member_([H | _], H).
member_([_ | T], X) :- member_(T, X).

concatenare_([], [], []).
concatenare_([], T, T).
concatenare_(T, [], T).
concatenare_([H1 | T1], [H2 | T2], [H1, H2 | Result]) :- concatenare_(T1, T2, Result).

concatenare__([], X, X).
concatenare__([H | T], X, [H | Result]) :- concatenare__(T, X, Result).

% 3)
alternate_sum([], 0).
alternate_sum([X], X).
alternate_sum([H1, H2 | T], R) :- alternate_sum(T, Z), R is Z + H1 - H2.

% 4)
just_add(L, L).
eliminate_one([], _, []).
eliminate_one([X | T], X, Result) :- just_add(T, Result).
eliminate_one([H | T], X, [H | Result]) :- eliminate_one(T, X, Result).

eliminate_all([], _, []).
eliminate_all([X | T], X, Result) :- eliminate_all(T, X, Result).
eliminate_all([H | T], X, [H | Result]) :- eliminate_all(T, X, Result).

% 5)
reverse_([], []).
reverse_([H | T], Res) :- reverse_(T, Result), concatenare__(Result, [H], Res). 

% 6)
frequency([], _, 0).
frequency([X | T], X, C) :- frequency(T, X, Z), C is Z + 1.
frequency([_ | T], X, C) :- frequency(T, X, C).

