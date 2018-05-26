
% ===========================================================
%  Respond
%  For each input type, react appropriately.
% ===========================================================

% Declarative true in the model
respond(Evaluation) :- 
		Evaluation = [true_in_the_model], 
		write('  '), write("That's right"),!.

% Declarative false in the model
respond(Evaluation) :- 
		Evaluation = [not_true_in_the_model],  
		write('  '), write('That is not correct'),!.

% Yes-No interrogative true in the model
respond(Evaluation) :- 
		Evaluation = [yes_to_question],			
		write('  '), write('Yes').

% Yes-No interrogative false in the model		
respond(Evaluation) :- 
		Evaluation = [no_to_question], 	
        write('  '), write('No').

% wh-interrogative true in the model
respond(Evaluation) :-
		Evaluation \= [wh_false_in_model],
		prettyprinting(Evaluation).   % Print the answe nicely

% wh-interrogative false in the model
respond(Evaluation) :-
		Evaluation = [wh_false_in_model],
		write('  '), write("Answer not found!!").

% =============================================================================================
% rule for priting the answer - ( example - [banana,banana,apple] is printed [1 apple 2 banana]
% =============================================================================================
% base case for last 2 elements
prettyprinting([Current,Next|[]], Count) :-
	((Current = Next, Temp is Count + 2, write(Temp), write(' '),write(Current));
	 (Temp is Count + 1, write(Temp), write(' '),write(Current), nl, write(1), write(' '),write(Next))).

prettyprinting([Current,Next|LIST], Count) :-
	((Current = Next, Temp is Count + 1, prettyprinting([Next|LIST], Temp));
	  (not(Current = Next), Temp is Count + 1, write(Temp), write(' '),write(Current),write("  "), prettyprinting([Next|LIST], 0))).

prettyprinting(List):- 
	sort(0, @=<, List,  Sorted),
	write('  '),
	prettyprinting(Sorted, 0).

prettyprinting([X]):- write(X).