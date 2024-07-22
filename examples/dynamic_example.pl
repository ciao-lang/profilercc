:- module(_, _, [profilercc]).

:- use_module(library(profilercc/graph_to_tex)).

:- cost_center p1/0, p2/0, q/1.

p1 :-
    q(a),
    q(b),
    q(c),
    q(d).

p2 :- 
    q(a).

q(a).
q(b).
q(c).
q(d).

main2 :-
    p1,
    p2.

:- use_module(library(profilercc/profiler_utils)).
:- use_module(library(profilercc/profiler_base)).
t0 :-
    profile_reset,
    profile(main2),
    profile_dump.

t1 :-
    profile_info(A),
    A = p(Freq, _, _),
    get_profile_cc_graph(time(Freq), 
        [call_exits_c, call_fails_c,
         redo_exits_c, redo_fails_c], A, Graph),
    graph_to_tex(time(Freq), 
        [call_exits_c, call_fails_c,
         redo_exits_c, redo_fails_c], Graph).
