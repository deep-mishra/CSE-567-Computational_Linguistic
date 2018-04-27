/*Final state*/
q8([],[]).

/*Start states*/
q0([i|L],N) :- q1(L,N).
q0([x|L],N) :- q2(L,N).
q0([v|L],N) :- q3(L,N).

/*Handle cases which involve strings like i, iii, iv, ix*/
q1([],[1|N]) :- q8([],N).
q1([i|L],N) :- q4(L,N).
q1([v|L],[4|N]) :- q8(L,N).
q1([x|L],[9|N]) :- q8(L,N).
q4([],[2|N]) :- q8([],N).
q4([i|L],[3|N]) :- q8(L,N).

/*Handle cases which involve strings like x, xx*/
q2([i|L],[1|N]) :- q1(L,N).
q2([],[1|N]) :- q7([],N).
q2([x|L],[2|N]) :- q7(L,N).
q2([v|L],[1|N]) :- q3(L,N).
q7([],[0|N]) :- q8([],N).

/*Handle cases which involve strings like v, vi, vii, viii*/
q3([],[5|N]) :- q8([],N).
q3([i|L],N) :- q5(L,N).
q5([],[6|N]) :- q8([],N).
q5([i|L],N) :- q6(L,N).
q6([],[7|N]) :- q8([],N).
q6([i|L],[8|N]) :- q8(L,N).