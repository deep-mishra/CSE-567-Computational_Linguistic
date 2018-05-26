

% ===========================================================
%  Modelchecker:
%  1. If input is a declarative, check if true
%  2. If input is a yes-no question, check if true
%  3. If input is a content question, find answer
% ===========================================================

modelchecker(q(_,FOL),Evaluation) :-
	%Running find all to get all the possible result. Extracting last element of list everytime and find the corresponding symbol
	findall(Symbol,(sat([],FOL,List),last(List,[_,Value]),f(Symbol,Value)),Answer),
	Answer \= [] -> Evaluation = Answer; Evaluation = [wh_false_in_model].

modelchecker(ynq(FOL),Evaluation) :- 	
	sat([],FOL,_) -> Evaluation = [yes_to_question]; Evaluation = [no_to_question].

modelchecker(s(FOL,[]),Evaluation) :- 
	sat([],FOL,_) -> Evaluation = [true_in_the_model]; Evaluation = [not_true_in_the_model].
	
% ===========================================================
%  Model Data
% ===========================================================
	
%modelchecker(SemanticRepresentation,Evaluation)

% b1 is almond milk
% a1 is blue box
% a2 is white box
% a3 is green box containing 3 bananas
% a1 is blue box containing ham and 3 eggs
% f1 is a blue container on the top shelf containing a sandwich which has no meat
% f2 is a white container on the bottom shelf containing a banana
% f3 is a white container on the bottom shelf NOT containing a banana (contains egg)
% g1 is the bottom shelf
% g2 is the top shelf
% g3 is the middle shelf
% h1 is a sandwich without meat
% j1 is a yellow bowl on the middle shelf containing 2 eggs

% have to handle 'belongs to sue', 'sam drank the almond milk', 'box of popsicles',

model([a1, a2, a3, a4, a5, b1, b2, c1, d1, d2, d3, d4, d5, d6, e1, f1, f2, f3, g1, g2, g3, h1, i1, i2, i3, j1, j2, k1, k2, m1, n1, o1, p1, q1, r1, s1, t1, u1, v1, x1, z1,z2, z3],
          [[box, [a1, a2, a3, a4, a5]],  
		   [milk, [b1, b2]], 
		   [ham, [c1]],
		   [egg, [d1, d2, d3, d4, d5,d6]],	
		   [cake, [e1]],
		   [container, [f1, f2, f3]],
		   [shelf, [g1, g2, g3]],
		   [sandwich, [h1, h2]],
		   [banana, [i1, i2, i3]],
		   [bowl, [j1, j2]],
		   [watermelon, [k1, k2]],
		   [fridge, [m1]],
		   [popsicle, [n1]],
		   [freezer, [o1]],
		   [flour,[p1]],
		   [sugar,[r1]],
		   [water,[s1]],
		   [cola,[t1]],
		   [coffee,[u1]],
		   [icecream,[v1]],
		   [sue, [z1]],
		   [sam, [z2]],
		   [mia, [z3]],
		   [cheese, [x1]],
		   
		   
		   [last, [k1]],
		   
		   [ingredient, [p1, r1]],
		   
		   [almond, [b1]],
		   [blue, [a1, f1]],
		   [white, [a2, f2, f3, j2]],
		   [green, [a3]],
		   [yellow, [j1, a5]],
		   [bottom, [g1]],
		   [top, [g2]],
		   [middle, [g3]],
		   [empty, [a4]],
		   
		   [belong, [[a2,z1]]],
		   
		   [contain,[[m1,a1],[m1,a2],[m1,a3],[],[m1,b2],[m1,k1],[m1,k2],
					 [a3, i1],[a3, i2],[a3, i3],                                                         
					 [a1,c1],[a1, d1],[a1,d4],
					 [f1, h1], [f3,d1],
					 [f2,i1], [f1,i2],
					 % [f3,i2],       % Uncomment this for (Every white container on the bottom shelf contains a banana - to be true)
					 [j1,d2], [j1,d3],
					 [e1,p1],[e1,d6],[e1,r1],
					 [o1,a2],
					 [g2,k1],
					 [e1,p1],
					 [h1,x1]
					 ]],
		   
		    [inside,[[a1,m1],[a2,m1],[a3,m1],[b2,m1],[k1,m1],[k2,m1],
					 [i1,a3],[i2,a3],[i3,a3],                                                         
					 [c1,a1],[d1,a1],[d4,a1],[d3,a1],
					 [h1,f1], [i1,f2], [d1,f3],
					 [d2,j1], [d3,j1],
					 [p1,e1],[d6,e1],[r1,e1],
					 [a2,o1],
					 [v1,o1]
					 ]],
					 
		    [in, [[a1,m1],[a2,m1],[a3,m1],[b2,m1],[k1,m1],[k2,m1],
				 [i1,a3],[i2,a3],[i3,a3],                                                         
				 [c1,a1],[d1,a1],[d4,a1],
				 [h1,f1], [i1,f2], [d1,f3],
				 [d2,j1], [d3,j1],
				 [p1,e1],[q1,e1],[r1,e1],
				 [n1,o1],
				 [v1,o1]
				 ]],
			
			[on, [[f1,g2],[f2,g1],[f3,g1],[j1,g3]]],
			
			[has, [[h1,v1]]], % add [h1,c1] for (Every blue container on the top shelf contains a sandwich that has no meat - to be false)
			
			[drank, [[z1,b1]]],
			
			[of, [[a4,n1],[o1,m1]]],
			
			[put, [[z2,a5,j2]]],
			
			[took, [[z3, k1]]]
			
		]).

	
		   
% ==================================================
% Word net
% Is a relationship - hypernym implementation
% ==================================================

is_a(ham,meat).
is_a(ham,pork).
is_a(cake,desert).
is_a(X,X).

% ==================================================
% Function i
% Determines the value of a variable/ constant in an assignment G
% ==================================================

i(Var,G,Value):- 
    var(Var),
    member([Var2,Value],G), 
    Var == Var2.   

i(C,_,Value):- 
   nonvar(C),
   f(C,Value).

% ==================================================
% Function F
% Determines if a value is in the denotation of a Predicate/ Relation
% ==================================================
	
f(Symbol,Value):- 
   is_a(ModelSymbol,Symbol),  % Added the code to support hypernym
   model(_,F),
   member([ModelSymbol,ListOfValues],F),
   member(Value,ListOfValues).

% ==================================================
% Extension of a variable assignment
% ==================================================

extend(G,X,[ [X,Val] | G]):-
   model(D,_),
   member(Val,D).

% ==================================================
% Existential quantifier
% ==================================================

sat(G1,exists(X,Formula),G3):-
   extend(G1,X,G2),
   sat(G2,Formula,G3).

sat(G1,two(X,Formula),G3):-
    findall(G3, (sat(G1,quantifier(X,Formula),G3)), Lst),
    length(Lst,L),
	%write(L),
	L >= 2,!.

sat(G1,three(X,Formula),G3):-
    findall(G3, (sat(G1,quantifier(X,Formula),G3)), Lst),
    length(Lst,L),
	%write(L),
	L >= 3,!.
	
sat(G1,four(X,Formula),G3):-
    findall(G3, (sat(G1,quantifier(X,Formula),G3)), Lst),
    length(Lst,L),
	%write(L),
	L >= 4,!.
   
sat(G1,quantifier(X,Formula),G3):-
   extend(G1,X,G2),
   sat(G2,Formula,G3).

% ==================================================
% Wh-question
% ==================================================

sat(G1,thing(X),G3):-
   extend(G1,X,G3).

sat(G1,person(X),G3):-
   extend(G1,X,G3).
   
sat(G1,place(X),G3):-
   extend(G1,X,G3).

% ==================================================
% Definite quantifier (semantic rather than pragmatic account)
% ==================================================
 
sat(G1,the(X,and(A,B)),G3):-
   sat(G1,exists(X,and(A,B)),G3),
   i(X,G3,Value), 
   \+ ( ( sat(G1,exists(X,A),G2), i(X,G2,Value2), \+(Value = Value2)) ).


% ==================================================
% Negation 
% ==================================================

sat(G,not(Formula2),G):-
   \+ sat(G,Formula2,_).

% ==================================================
% Universal quantifier
% ==================================================

sat(G, forall(X,Formula2),G):-
  sat(G,not( exists(X,not(Formula2) ) ),G).

% ==================================================
% Conjunction
% ==================================================

sat(G1,and(Formula1,Formula2),G3):-
  sat(G1,Formula1,G2), 
  sat(G2,Formula2,G3). 

% ==================================================
% Disjunction
% ==================================================

sat(G1,or(Formula1,Formula2),G2):-
  ( sat(G1,Formula1,G2) ;
    sat(G1,Formula2,G2) ).

% ==================================================
% Implication
% ==================================================

sat(G1,imp(Formula1,Formula2),G2):-
   sat(G1,or(not(Formula1),Formula2),G2).

% ==================================================
% Predicates
% ==================================================

sat(G,Predicate,G):-
   Predicate =.. [P,Var],
   \+ (P = not),
   i(Var,G,Value),
   f(P,Value).

% ==================================================
% Two-place Relations
% ==================================================

sat(G,Rel,G):-
   Rel =.. [R,Var1,Var2],
   \+ ( member(R,[exists,forall,and,or,imp,the]) ),
   i(Var1,G,Value1),
   i(Var2,G,Value2),
   f(R,[Value1,Value2]).	