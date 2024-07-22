:- module(profcuts, _, [assertions, profilercc]).

:- use_module(library(lists), [member/2]).
:- use_module(library(profilercc/profiler_utils)).

:- cost_center p2/0.

p1 :-
    member(a, [a, b, a, b, a, b, a]).

p2 :-
    p3.

p3 :-
    p1,
    p1,
    p1,
    p1,
    !,
    p1,
    p1,
    !,
    !,
    !,
    !.
p3.

t0 :-
    profile_reset,
    profile(p2),
    profile_dump.
