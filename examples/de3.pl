:- module(_, _, [profilercc]).

:- cost_center p2/0, p3/0, q/1.

p2 :-
    q(_A),
    fail.
p2.
p2 :-
    q(f).

p3 :- q(e).

q(a).
q(b).
q(c) :- r(_, d).


r(_, a).
r(_, b).
r(_, c).

main2 :-
    p2,
    p3.

:- use_module(library(profilercc/profiler_utils)).
t0 :-
    profile_reset,
    profile(main2);
    profile_dump,
    profile_info(_A).
