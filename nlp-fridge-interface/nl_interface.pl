% ===========================================================
% Main loop:
% 1. Repeat "input-response" cycle until input starts with "bye"
%    Each "input-response" cycle consists of:
% 		1.1 Reading an input string and convert it to a tokenized list
% 		1.2 Processing tokenized list
% ===========================================================

chat:-
 repeat,
   readinput(Input),
   process(Input), 
  (Input = [bye| _] ),!.
  


% ===========================================================
% Read input:
% 1. Read char string from keyboard. 
% 2. Convert char string to atom char list.
% 3. Convert char list to lower case.
% 4. Tokenize (based on spaces).
% ===========================================================

readinput(TokenList):-
   read_line_to_codes(user_input,InputString),
   string_to_atom(InputString,CharList),
   string_lower(CharList,LoweredCharList),
   tokenize_atom(LoweredCharList,TokenList).


% ===========================================================
%  Process tokenized input
% 1. Parse morphology and syntax, to obtain semantic representation
% 2. Evaluate input in the model
% If input starts with "bye" terminate.
% ===========================================================

process(Input):-
	subtract(Input,['?','.'],CleanInput),
	parse(CleanInput,SemanticRepresentation),
	modelchecker(SemanticRepresentation,Evaluation),
	respond(Evaluation),!,
	nl,nl.
	
process([bye|_]):-
   write('> bye!').


% ===========================================================
%  Parse:
% 1. Morphologically parse each token and tag it.
% 2. Add semantic representation to each tagged token
% 3. Obtain FOL representation for input sentence
% ===========================================================

%parse(Input, SemanticRepresentation):-
% ...
parse(Input,Formula):-
        srparse([],Input,Formula).

srparse([F],[],F).

srparse([Z,Y,X|MoreStack],Words,F):-
       rule(LHS,[X,Y,Z]),
       srparse([LHS|MoreStack],Words,F). 

srparse([Y,X|MoreStack],Words,F):-
       rule(LHS,[X,Y]),
       srparse([LHS|MoreStack],Words,F).

srparse([X|MoreStack],Words,F):-
       rule(LHS,[X]),
       srparse([LHS|MoreStack],Words,F).

srparse(Stack,[Word|Words],F):-
        lex(X,Word),
        srparse([X|Stack],Words,F).


% ===========================================================
% Grammar
% 1. List of lemmas
% 2. Lexical items
% 3. Phrasal rules
% ===========================================================

% --------------------------------------------------------------------
% Lemmas are uninflected, except for irregular inflection
% lemma(+Lemma,+Category)
% --------------------------------------------------------------------
lemma(a,dtexists).
lemma(an,dtexists).
lemma(the,dtexists). 
lemma(some, dtexists).

lemma(each,dtforall).
lemma(all,dtforall).
lemma(every,dtforall).

lemma(no,dtfornone).  
lemma(not,dtfornone).  

lemma(meat,n).
lemma(ham,n).
lemma(box,n).
lemma(container,n).
lemma(shelf,n).
lemma(sandwich,n).
lemma(banana,n).
lemma(bowl,n).
lemma(watermelon,n).
lemma(fridge,n).
lemma(milk,n).
lemma(popsicle,n).  
lemma(freezer,n).
lemma(almond,n).
lemma(cake,n).
lemma(egg,n).
lemma(pork,n).
lemma(desert,n).


lemma(icecream,n).
lemma(cheese,n).

lemma(tom,pn).
lemma(mia,pn).
lemma(sam,pn).
lemma(sue,pn).

lemma(red,adj).
lemma(blue,adj).
lemma(top,adj).
lemma(white,adj).
lemma(bottom,adj).
lemma(almond,adj).
lemma(middle,adj).
lemma(empty,adj).
lemma(yellow,adj).
lemma(green,adj).
lemma(last,adj).

lemma(is,be).
lemma(was,be).
lemma(has,be).
lemma(had,be).
lemma(are,be).
lemma(does,be).
lemma(do,be).
lemma(did,be).

lemma(has,tv).
lemma(eat,tv).
lemma(contain,tv). 
lemma(drink,tv).  
lemma(drank,tv).
lemma(saw,tv).
lemma(see,tv).
lemma(belong,tv).
lemma(ate,tv).
lemma(took,tv).

lemma(put,dtv).

lemma(in,p).
lemma(under,p).
lemma(inside,p).
lemma(of,p).
lemma(with,p).

%considering there as vacuous
lemma(there,vac).

%below are vacuous preoposition
lemma(on,vacp).
lemma(on,p).
lemma(to,vacp).

lemma(that, rel).

lemma(who,wh).
lemma(what,wh).
lemma(where,wh).
lemma(which,wh).

lemma(two,two).
lemma(three,three).
lemma(four,four).

lemma(ate,tv).
lemma(good,adj).
lemma(boy,n).
lemma(book,n).
lemma(read,vt).
lemma(expired,iv).

lemma(box_of_popsicles,n).


% --------------------------------------------------------------------
% Constructing lexical items:
% word = lemma + suffix (for "suffix" of size 0 or bigger)
% --------------------------------------------------------------------

%n
lex(n(X^P),Lemma):-
	lemma(Lemma,n),
	P=.. [Lemma,X].
%pn
lex(pn((Word^X)^X),Word):-
	lemma(Word,pn).	
%dtforall
lex(dt((X^P)^(X^Q)^forall(X,imp(P,Q))), Word):-
	lemma(Word,dtforall).	
%dtexists
lex(dt((X^P)^(X^Q)^exists(X,and(P,Q))),Word):-
	lemma(Word,dtexists).
	
%adj
lex(adj((X^P)^X^and(P,Q)),Lemma):-
	lemma(Lemma,adj),
	Q=.. [Lemma,X].
%iv
lex(iv(X^P,[]),Lemma):-
	lemma(Lemma,iv),
	P=.. [Lemma,X].
%tv
lex(tv(K^W^P,[]),Lemma):-
	lemma(Lemma,tv),
	P=.. [Lemma,K,W].
%dtv	
lex(dtv(P^Q^R^S, []),Lemma):-
	lemma(Lemma,dtv),
	S=.. [Lemma,P,Q,R].
%p
lex(p((Y^K)^Q^(X^P)^and(P,Q)),Lemma):-
	lemma(Lemma,p),
	K=.. [Lemma,X,Y].
	
%ambp - ambiguous preposition - This is to allow the ambiguity in some prepositions, so that when they appear in the 'be' rule the prepositions is like tv - (p2(X^Y^inside(X,Y)))
lex(p2(K^W^P),Lemma):-
	lemma(Lemma,p),
	P=.. [Lemma,K,W].
%dtfornone
lex(dt((X^P)^(X^Q)^not(exists(X,and(P,Q)))), Word):-
	lemma(Word,dtfornone).
%aux
lex(be, Lemma) :-
	lemma(Lemma, be).
%rel
lex(rel, Lemma) :-
	lemma(Lemma, rel).
%vacp
lex(vacp, Lemma) :-
	lemma(Lemma, vacp).
	
%vac there
lex(vac, Lemma) :-
	lemma(Lemma, vac).
%two
lex(dt((X^P)^(X^Q)^two(X,and(P,Q))),Word):-
	lemma(Word,two).
%three
lex(dt((X^P)^(X^Q)^three(X,and(P,Q))),Word):-
	lemma(Word,three).
%four
lex(dt((X^P)^(X^Q)^four(X,and(P,Q))),Word):-
	lemma(Word,four).

%questions
lex(whpr((X^P)^q(X,and(person(X),P))),Word):-
	Word == 'who',
	lemma(Word,wh).
lex(whpr((X^P)^q(X,and(thing(X),P))),Word):-
	Word == 'what',
	lemma(Word,wh).
lex(whpr((X^P)^q(X,and(place(X),P))),Word):-
	Word == 'where',
	lemma(Word,wh).
lex(whpr((X^P)^q(X,and(thing(X),P))),Word):-
	Word == 'which',
	lemma(Word,wh).

% ...

% --------------------------------------------------------------------
% Suffix types
% --------------------------------------------------------------------
% ...
lex(X, Word):-
	atom_concat(A,B,Word),
	lemma(A,_),
	suffix(B),
	lex(X,A).
	
suffix(s).
suffix(es).
suffix(ed).
suffix(ing).

% --------------------------------------------------------------------
% Phrasal rules
% rule(+LHS,+ListOfRHS)
% --------------------------------------------------------------------
%Ynq rule - pp2 is to allow ambiguity in the preposition for BE case
rule(ynq(Y),[be,np(X^Y),pp2(X)]).
%rule(ynq(Y),[be,np(Y)]).

%To allow some ambiguity in some prepositions with be
rule(pp2(X^Y),[p2(X^Z),np(Z^Y)]).

%Ignore vac(there) case
rule(np(X),[vac,np(X)]).
rule(s(Y,[]),[vac,ynq(Y)]).

%Main rule
rule(np((X^P)^exists(X,and(Q,P))),[n(X^Q)]).
rule(np(Y),[dt(X^Y),n(X)]).
rule(np(X),[pn(X)]).
rule(n(X^Z),[n(X^Y),pp((X^Y)^Z)]).
rule(n(X),[adj(Y^X),n(Y)]).
rule(pp(Z),[p(X^Y^Z),np(X^Y)]).
rule(pp(X),[vacp,np(X)]).
rule(vp(X,WH),[iv(X,WH)]).
rule(vp(X^Y,[]),[tv(X^Z,[]),np(Z^Y)]).
rule(vp(X^Y,[]),[tv(X^Z,[]),pp(Z^Y)]).
rule(s(Y,WH),[np(X^Y),vp(X,WH)]).

%New rules
rule(vp(K,[WH]),[tv(Y,[WH]),np(Y^K)]).
rule(s(X,[WH]),[vp(X,[WH])]).

%Complement interrogatives
rule(iv(X^P,[Y]) ,[tv(X^Y^P,[])]).
%Subject interrogatives
rule(tv(Y^P,[X]), [tv(X^Y^P,[])]).

%WhQuestion rules
rule(Y,[whpr(X^Y),vp(X,[])]).
rule(ynq(Y),[be,np(X^Y),vp(X,[])]).
rule(Z,[whpr((X^Y)^Z),inv_s(Y,[X])]).
rule(inv_s(Y,[WH]),[be,np(X^Y),vp(X,[WH])]).

% dtv = np pp ( we need from pp = vac + np )
rule(vacprepp(X),[vacp,np(X)]).
rule(vp(P^U,[]),[dtv(P^Q^R^S,[]),np((Q^T)^U),vacprepp((R^S)^T)]).

%RC
rule(rc(P,[X]),[rel,s(P,[X])]).
rule(n(X^and(Y,Z)),[n(X^Y),rc(X^Z,[])]).
rule(n(X^and(Y,Z)),[n(X^Y),rc(Z,[X])]).

%For cases like 'is there milk' - ignoring (BE and VAC) in front of the noun
%this would be the last option
rule(ynq(exists(X,Y)),[be,vac,n(X^Y)]).

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


cls :- write('\e[H\e[2J').