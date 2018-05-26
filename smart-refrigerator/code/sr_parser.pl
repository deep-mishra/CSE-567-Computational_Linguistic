

% ===========================================================
%  SR - Shift Reduce Parse:
% 1. Morphologically parse each token and tag it.
% 2. Add semantic representation to each tagged token
% 3. Obtain FOL representation for input sentence
% ===========================================================

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