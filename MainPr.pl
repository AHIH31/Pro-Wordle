is_category(L):- word(_,L).
is_word(W):-word(W,_).


is_length_category(L,C,L2):- word(W,C),(atom_length(W,L)->(L2 is L,!)
;(write('This length is not available in this category'),nl,write('Choose a length: '),nl,read(L1),is_length_category(L1,C,L2))).
categories(L2):- findall(X,word(_,X),L),sort(L,L2).

available_length(L):- 
	findall(W,word(W,_),M),
	helper_al(M, L).

	helper_al([],0).
	helper_al([H|T], L):-
		atom_chars(H,H1),
		length(H1, L2),L==L2->!;
		helper_al(T, L).

pick_word(W, L, C):- available_length(L),word(W,C).
					 
remove_duplicates([],[]).

remove_duplicates([H | T], L) :-    
     member(H, T),
     remove_duplicates( T, L).

remove_duplicates([H | T], [H|T1]) :- 
      \+member(H, T),
      remove_duplicates( T, T1).

correct_letters(L1,L2,CL):- intersection(L2,L1,CL2),remove_duplicates(CL2,CL).

correct_positions([],[],[]).
correct_positions([H|T], [H|T1], R) :- append([H], R1, R), correct_positions(T, T1, R1).
correct_positions([H|T], [X|T1], R) :- H\=X , correct_positions(T, T1, R).



build_kb:-start.
start:- write('Welcome to Pro-Wordle!'), nl ,write('----------------------'),nl,nl,input.
                                         
input:-write('Please enter a word and its category on separate lines: '), nl ,read(Word),(Word = done -> write('Done building the words database...');
		read(Category),assert(word(Word,Category)),input).
 



play:- write('the available categories are: '),categories(Avail_categ),write(Avail_categ)
		,nl,write('Choose a category:'),nl,read(Category), 
		is_category(Category)->(write('Choose a length: '),nl,read(Length1),
		available_length(Length1)-> is_length_category(Length1,Category,Length),Guesses is Length+1,write('Game started. You have '),write(Guesses),write(' guesses.'),
		pick_word(Word_Picked,Length,Category),atom_chars(Word_Picked,Word),play1(Word,Guesses,Length)
		;(pick_word(Word_Picked,Length,Category),atom_chars(Word_Picked,Word),write('There are no words of this length.'),wrong(Guesses,Word)))
		
		;(write('No such category is available'),nl,play).
		
play1(Word,Guesses,Length):-write('Enter a word composed of ')
	,write(Length),write(' letters:'),nl,read(Entered),atom_length(Entered,Entered_Len),
	Entered_Len==Length-> (is_word(Entered)->(atom_chars(Entered,Entered_List),(Entered_List == Word->(write('You Won!'))
	;play2(Guesses,Word,Length,Entered_List)));(write('No such word in the KB!'),nl,play1(Word,Guesses,Length)))
	;(write('Word is not composed of '),write(Length), write('letters. Try again.'),nl,write('Remaining Guesses are: '),write(Guesses)),nl,play1(Word,Guesses,Length).
		
play2(Guesses,Word,Length,Entered_List):- Guesses==1->(write('You Lost!'));(write('correct letters are: '),correct_letters(Entered_List,Word,CL),write(CL),
	nl,nl,write('Correct letters in correct positions are: '),correct_positions(Entered_List,Word,CP),write(CP),nl,nl,write('Remaining Guesses are: ')
	,Guesses1 is Guesses-1,write(Guesses1),nl,
	play1(Word,Guesses1,Length)).
 
wrong(Guesses,Word):- write('Choose a length: '),nl,read(Length1),available_length(Length1)->(is_length_category(Length1,Category,Length),Guesses is Length+1,write('Game started. You have ')
	,write(Guesses),write('guesses.'),play1(Word,Guesses,Length));
	(write('There are no words of this length.'), wrong(Guesses,Word)).



main:- 
	build_kb , play.




