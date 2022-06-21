


% myprog("add(X,Y) :- add(Y,X), foo(bar).").
myprog("add(X,Y) :- add(X,add(Y,Z))").

% myprog(Prog), open_string(Prog, S),

clause(A :- B, A, B).

% flatten(T, [Atom | R] - R) :- atom(T), term_string(T, Atom).

/*
chew([], [], R-R).
chew([T | Args], [X | Xs], L-R) :- flatten(T, X, L - R1), chew(Args, Xs, R1-R)

flatten(T, T, L) :- var(T). 
flatten(T, Ret, [T1 | L] - Tl) :- 
    T =.. [F | Args],
    chew(Args, Args1, L - Tl),
    T1 =.. [F | Args1].
*/
:- debug.
chew([], [], []).
chew([T | Args], [X | Xs], L1) :- flatten(T, X, L), chew(Args, Xs, Rest), append(L,Rest,L1).

% flatten(T, foo, []) :- atom(T).

% it is nice to unify X here for talking to the head.
% but now we need to catch gensymed vars.
% Hmm.
% Maybe we're better off grounding as a post pass.

flatten(T, T, []) :- var(T). %, gensym(v, T). 
flatten(T, Fresh, [T1 | Flat]) :- 
    nonvar(T),
    T =.. [F | Args],
    chew(Args, Args1, Flat),
    gensym(v, Fresh),
    append(Args1, [Fresh], Args2), 
    T1 =.. [F | Args2].

% build_body()

groundit(T) :- var(T), gensym(v,T).
groundit(T) :-
    nonvar(T),
    T =.. [F | Args],
    maplist(groundit, Args).


compile_datalog(Res) :- 
    myprog(Prog), 
    term_string(T,Prog),
    clause(T, Head, Body),
    comma_list(Body, [BodyL]),
    flatten(BodyL, EId, FlatL),
    groundit(FlatL),
    comma_list(Flat,FlatL),
    Res = (Head :- Flat).


/*
file_lines(File, Lines) :-
    setup_call_cleanup(open(File, read, In),
       stream_lines(In, Lines),
       close(In)).

stream_lines(In, Lines) :-
    read_string(In, _, Str),
    split_string(Str, "\n", "", Lines).
*/



