/*
https://stackoverflow.com/questions/27878382/how-to-do-a-parser-in-prolog

:- set_prolog_flag(double_quotes, chars).
*/

/*
parser([]) --> [].
parser(Tree) --> assign(Tree).

assign(assignment(ident(X), '=', Exp)) --> id(X), [=], expr(Exp), [;].

id(X) --> [X], { atom(X) }.

expr(expression(Term)) --> term(Term).
expr(expression(Term, Op, Exp)) --> term(Term), add_sub(Op), expr(Exp).

term(term(F)) --> factor(F).
term(term(F, Op, Term)) --> factor(F), mul_div(Op), term(Term).

factor(factor(int(N))) --> num(N).
factor(factor(Exp)) --> ['('], expr(Exp), [')'].

add_sub(Op) --> [Op], { memberchk(Op, ['+', '-']) }.
mul_div(Op) --> [Op], { memberchk(Op, ['*', '/']) }.

num(N) --> [N], { number(N) }.


main :-
    open('test2.txt', read, Str),
    read_file(Str,Lines),
    close(Str),
    write(Lines), nl.
	
*/	

read_file(Stream,[]) :-
    at_end_of_stream(Stream).

read_file(Stream,[X|L]) :-
    \+ at_end_of_stream(Stream),
    read(Stream,X),
    read_file(Stream,L).

:- use_module(library(pio)).

lines([])           --> call(eos), !.
lines([Line|Lines]) --> line(Line), lines(Lines).

eos([], []).

line([])     --> ( "\n" ; call(eos) ), !.
line([L|Ls]) --> [L], line(Ls).


test1(File) :-
	open(File, read, Input),
	read_line_to_string(Input, S),
	write('ligne:'),write(S),
	close(Input).

/*
parcourt()
*/




stream_lines0(File,Lines) :-
	open(File, read, Input),
	stream_lines(Input,Lines),
	close(Input).
	

stream_lines(In, Lines) :-
    read_string(In, _, Str),
    split_string(Str, "\n", "\n", Lines).


parseLines([], []).

parseLines([H|T], [R2|T2]) :- 
				write('H:'),write(H),nl,
				string_chars(H, H2),
				write('H2:'),write(H2),nl,
				write('T:'),write(T),nl,
				phrase(parseGame(R2), H2), 
				write('res:'),write(R2),nl,
				parseLines(T, T2).

parseGame(game(N,List)) --> 
				"Game ", num0(N),deux_point, " ", listeGames(List)  , 
				{write('game:'),write(N),write('liste:'),write(List),nl} 
				.

deux_point --> ":".


/*
num0(N) --> [N], { number(N) }.
*/
/*
num0(N) --> [N].
num0(N) --> [N], { integer(N) }.
*/
/*
num0 --> integer.
*/


num3(N) --> ['0'], { N is 0 }.
num3(N) --> ['1'], { N is 1 }.
num3(N) --> ['2'], { N is 2 }.
num3(N) --> ['3'], { N is 3 }.
num3(N) --> ['4'], { N is 4 }.
num3(N) --> ['5'], { N is 5 }.
num3(N) --> ['6'], { N is 6 }.
num3(N) --> ['7'], { N is 7 }.
num3(N) --> ['8'], { N is 8 }.
num3(N) --> ['9'], { N is 9 }.


num2(N) --> num3(N1), num2(N2), { N is N1*10+N2 }.
num2(N) --> num3(N).

num0(N) --> num2(N).

listeGames(L) --> listeGames0(L).
			
listeGames0([X|L]) --> 
			listeGames1(X), 
			"; ", 
			/*{write('listeGames,'),write(X),nl}, */
			listeGames0(L).
			
listeGames0([X]) --> 
			listeGames1(X).

listeGames1([X|L]) --> 
			listeGames3(X), 
			", ", 
			/*{write('listeGames,'),write(X),nl}, */
			listeGames1(L).
			
listeGames1([X]) --> 
			listeGames3(X).


listeGames3(X) --> 
			game0(X) 
			.



game0(bleu(N)) --> num0(N), " blue".
game0(rouge(N)) --> num0(N), " red".
game0(vert(N)) --> num0(N), " green".


test1(game(A,L),B,R,G) :- 
	parcourt(A,L,B,R,G), 
	write('Le jeux '), write(A), write(' a : '),
	write(B), write(' bleu, '),
	write(R), write(' rouge, '),
	write(G), write(' vert'), nl.

parcourt(_,[],0,0,0) :- !.

parcourt(A,[H|T],B,R,G) :- 
	parcourt2(A,H,B,R,G),
	parcourt(A,T,B,R,G).


parcourt2(_,[],0,0,0) :-  !.

parcourt2(A,[cube(X,blue)|T],B,R,G) :- 
	B is B + X,
	parcourt(A,T,B,R,G).

parcourt2(A,[cube(X,red)|T],B,R,G) :- 
	R is R + X,
	parcourt(A,T,B,R,G).

parcourt2(A,[cube(X,green)|T],B,R,G) :- 
	G is G + X,
	parcourt(A,T,B,R,G).


test2(A,B,R,G) :- 
	test1(game(A,_),B,R,G).

calculValeurJeux(N,R,G,B,V) :- R =< 12,
		G =< 13,
		B =< 14,
		V is N.
		
calculValeurJeux(_,_,_,_,0).

calcul02(game(N,List), V, N) :-
		somme00(List,14,12,13,V),
		/*R =< 12,
		G =< 13,
		B =< 14,*/
		write('Le jeux '), write(N), write(' a : '),
		/*write(B), write(' bleu, '),
		write(R), write(' rouge, '),
		write(G), write(' vert '), */
		/*calculValeurJeux(N,R,G,B,V),*/
		write(V), write(' valeur'),nl
		.
	
	
calcul(L, N) :- 
		calcul0(L,N),
		write('N'),write(N),nl
		.


	
calcul0([H|T], N) :- calcul02(H, N0, N2), calcul0(T, N1), N3 is (1-N0)*N2, N is N3+N1.
calcul0([H], N) :- calcul02(H, N0, N2), N is (1-N0)*N2.


somme00(L,BM,RM,GM,N) :-
		somme01(L,BM,RM,GM,N).

somme01([H|T],BM,RM,GM,N3) :-
		somme02(H,B,R,G),
		a_prendre(B,R,G,BM,RM,GM,N1),
		write('jeu '),write(N),write('B'),write(B),write('R'),write(R),write('G'),write(G),write('N'),write(N),nl,
		somme01(T,BM,RM,GM,N2),
		N3 is max(N1,N2)
		.


somme01([H],BM,RM,GM,N2) :-
		somme02(H,B,R,G),
		a_prendre(B,R,G,BM,RM,GM,N2)
		.

a_prendre(A,B,C,A2,B2,C2, N) :-
		A=<A2,
		B=<B2,
		C=<C2,
		N is 0.
		
		
a_prendre(_,_,_,_,_,_, N) :-
		N is 1.
		
		

		
somme02([H|T],B,R,G) :-
		somme2(H,B1,R1,G1),
		somme02(T,B2,R2,G2), 
		B is B1 + B2,
		R is R1 + R2,
		G is G1 + G2
		.
		
somme02([H],B,R,G) :-
		somme2(H,B,R,G)
		.		

somme([],B,R,G) :- 
		B is 0,
		R is 0,
		G is 0.
		
somme([H|T],B,R,G) :- 
		somme2(H,B1,R1,G1),
		somme(T,B2,R2,G2), 
		B is B1 + B2,
		R is R1 + R2,
		G is G1 + G2.
		

		
somme2(bleu(X),B,0,0) :- 
		B is X.
		

somme2(rouge(X),0,R,0) :-
		R is X.
		
	
somme2(vert(X),0,0,G) :- 
		G is X.
		
/*		
calcul2(N) :- 
		findall(A, calcul(A), R),
		somme3(R,N).
*/		
		
somme3([],R) :- R is 0.
somme3([H|T],R) :- 
		somme3(T,R2),
		R is H + R2.


afficheJeux(L, R) :- somme3(L, R).

parse0(File, R) :- 
		stream_lines0(File, Lines),write('Lines du fichier : '), 
		write(Lines), nl, 
		parseLines(Lines, R).

parse1(File, R) :- 
		stream_lines0(File, Lines),write('Lines du fichier : '), 
		write(Lines), nl, 
		parseLines(Lines, R0),
		/*afficheJeux(R0,R)*/
		calcul(R0, R)
		.

/*


parse0('chemin vers/test3.txt',R).

phrase(parser(T), [a, =, 3, +, '(', 6, *, 11, ')', ;]).



https://stackoverflow.com/questions/66840573/program-in-swi-prolog


https://www.swi-prolog.org/pldoc/doc_for?object=read_util:read_line_to_string/2


https://swi-prolog.discourse.group/t/read-lines-of-stdin-until-eof-into-a-list/6502

*/