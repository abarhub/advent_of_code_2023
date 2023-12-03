
game(1,[[cube(3,blue),cube(4,red)],[cube(1,red),cube(2,green),cube(6,blue)],[cube(2,green)]]).
game(2,[[cube(1,blue),cube(2,green)],[cube(3,green),cube(4,blue),cube(1,red)],[cube(1,green),cube(1,blue)]]).
game(3,[[cube(8,green),cube(6,blue),cube(20,red)],[cube(5,blue),cube(4,red),cube(13,green)],[cube(5,green),cube(1,red)]]).
game(4,[[cube(1,green),cube(3,red),cube(6,blue)],[cube(3,green),cube(6,red)],[cube(3,green),cube(15,blue),cube(14,red)]]).
game(5,[[cube(6,red),cube(1,blue),cube(3,green)],[cube(2,blue),cube(1,red),cube(2,green)]]).


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


