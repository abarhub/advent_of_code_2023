/*
https://stackoverflow.com/questions/27878382/how-to-do-a-parser-in-prolog

:- set_prolog_flag(double_quotes, chars).
*/

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

parseGame(game(N,List)) --> "Game ", num0(N), " ", listeGames(List), {write('game:'),write(N),write('liste:'),write(List),nl} .

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


listeGames([X|L]) --> game0(X), separateur, {write('listeGames,'),write(X),nl}, listeGames(L).
/*
listeGames([X|L]) --> game0(X), "; ", {write('listeGames;'),nl}, listeGames(L).
*/
listeGames([X]) --> game0(X), {write('listeGames game0 '),write(X), nl} .
/*
listeGames([]) --> [], {write('listeGames []'),nl}.
*/

separateur --> ", ".
separateur --> "; ".

game0(bleu(N)) --> num0(N), " blue".
game0(rouge(N)) --> num0(N), " red".
game0(vert(N)) --> num0(N), " green".


parse0(File, R) :- 
		stream_lines0(File, Lines),write('Lines du fichier : '), 
		write(Lines), nl, 
		parseLines(Lines, R).

/*


parse0('chemin vers/test3.txt',R).

phrase(parser(T), [a, =, 3, +, '(', 6, *, 11, ')', ;]).



https://stackoverflow.com/questions/66840573/program-in-swi-prolog


https://www.swi-prolog.org/pldoc/doc_for?object=read_util:read_line_to_string/2


https://swi-prolog.discourse.group/t/read-lines-of-stdin-until-eof-into-a-list/6502

*/