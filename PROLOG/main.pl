

:- use_module(library(lists)).
:- use_module(check_predicates).
:- use_module(tables).
:- use_module(library(apply)).


%TEMA2

%functie de formatare string

plus5(X,Y):- Y is X + 5.
make_format_str(MaxRowLen,Str) :-
maplist(plus5,MaxRowLen,Rp), aux_format(Rp,Str),!.

aux_format([H],R) :- string_concat("~t~w~t~",H,R1),
string_concat(R1,"+~n",R),
!.
aux_format([H|T],R) :- string_concat("~t~w~t~",H,R1), string_concat(R1,"+ ",R2), 
aux_format(T,Rp), string_concat(R2,Rp,R),!.


%matrice transpusa

get_head([H|_], H).
get_tail([_|T],T).

transpose([[]|_], []).
transpose(M, [H|R]) :- maplist(get_head, M, H), maplist(get_tail, M, T), transpose(T,R).

%functie de calcul lungime maxima string


maxlist([], 0).
maxlist([H], R) :-  string_length(H, R).
maxlist([X, Y|L],R) :- string_length(X,R1), string_length(Y, R2),  R1 >= R2,  maxlist([X|L],R).
maxlist([X, Y|L],R) :- string_length(X,R1), string_length(Y, R2),  R1 < R2,  maxlist([Y|L],R).

%functie ce returneaza vectorul cu lungimi maxime

maxMatrix([], []).
maxMatrix([H], [R1|R]) :- maxlist(H, R1), maxMatrix([], R).
maxMatrix([H|L], [R1|R]) :- maxlist(H, R1), maxMatrix(L, R).

%functie ce imi afla pentru o matrice vectorul de lungimi maxime

final([],[]) :- !.
final(L,R) :- transpose(L,R1), maxMatrix(R1,R),!.


%afisare

print_table_op(L) :-  print_table(L,L), !.
print_table(L,[H]) :- final(L,R), make_format_str(R, Str), format(Str, H),!.
print_table(L,[H|Ls]) :- final(L,R), make_format_str(R, Str), format(Str, H), print_table(L,Ls),!.


%select
select1([],_,[]).
select1([[H1|H2]|L],Cols, [[H1|H2]|R]) :- member(H1,Cols) , select1(L,Cols,R). 
select1([_|L],Cols, R) :-  select1(L,Cols,R).
select_op(T, Cols, R) :- transpose(T, R1), select1(R1,Cols,R2), transpose(R2,R). 

%join_op

modify([_|R], Cols, [Cols|R]).

joinP(_, _, [],[],[]).
joinP(Op, Cols, [H1], [H2], [H3]) :- call(Op, H1,H2,H3), joinP(Op,Cols, [], [], []).
joinP(Op, Cols, [H1|Ls1], [H2|Ls2], [H3|R]) :- call(Op, H1,H2,H3), joinP(Op,Cols, Ls1, Ls2, R).
join_op(Op,Cols,L1,L2, R2) :- joinP(Op,Cols, L1,L2,R1), modify(R1, Cols, R2), !.  


%csp 

csp(_,_,[],[]).
csp(X, Pr, [H|T], R) :- not((X = H, Pr)), csp(X,Pr,T,R), !.
csp(X, Pr, [H|T], [H|R]) :- csp(X,Pr,T,R).

%filter

filter_op(T,Vars,Pred, R) :- get_head(T,H), get_tail(T,H1), csp(Vars, Pred, H1, R1), R = [H|R1].

%complex_query_1

complex_query_op(Table, R) :- filter_op(Table, [_,L_NAME,_,_,_,_,_], (sub_string(L_NAME,_, _, _, "escu")), R1),
							  filter_op(R1, [_,_,AA,PP,_,_,_], ((AA + PP)/2 > 6), R2),
							  filter_op(R2, [_,_,AA,PP,PC,PA,POO], ((AA + PP + PC + PA + POO)/5 > 5), R).
							  

%all evals

eval(table(S), T) :- table_name(S,T).
eval(tprint(Q), R) :- eval(Q, R), print_table_op(R), !.
eval(select(Cols, Q), R) :- eval(Q, R1), select_op(R1,Cols,R), !.
eval(join(Op, Cols, Q1, Q2), R) :- eval(Q1, T1), eval(Q2, T2), join_op(Op, Cols, T1, T2, R), !.
eval(tfilter(Vars, Pred, Q), R) :- eval(Q, R1), filter_op(R1, Vars, Pred, R), !.
eval(complex_query1(Q), R) :- eval(Q, R1), complex_query_op(R1, R), !.

 


















