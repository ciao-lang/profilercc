:- module(all_test, [], [assertions, hiord]).

:- discontiguous proftest/2.

:- use_module(.(bignums)).
proftest(bignums, do_bignums, 2).

:- use_module(.(color_map)).
proftest(color_map, color_map(_, _, _, _, _), 2).

:- use_module(.(flat)).
proftest(flat, flat(f(a,b,g(c)), _), 1).

:- use_module(.(guardians)).
proftest(guardians, guardians(10, 6, _), 3).

:- use_module(.(hanoi)).
proftest(hanoi, hanoi(5, a, b, c, _), 2).

:- use_module(.(jugs)).
proftest(jugs, solve_jugs(_), 4).

:- use_module(.(knights)).
proftest(knights, main(5), 4).

:- use_module(.(module3), [main3/1]).
proftest(module3, multi([module1, module2, module3], main3(_A)), 2).

:- use_module(.(mqu)). % TODO: :- use_module(.(mqu_ov_test)). ??
proftest(mqu, queens(s(s(s(s(0))))), 2).

:- use_module(.(qsort)).
proftest(qsort, qsort([5,6,2,3,4,1,7], _), 2).

% :- use_module(.(schedule)).
% proftest(schedule, schedule(10000), 2).

:- use_module(.(size)).
proftest(size, mem, 2).

:- use_module(.(subst_exp)).
proftest(subst_exp, substitute((a + (b * c) + (c * d)) - e, [a = 2, b = 3, c = 4, d = 5, e = 6], _), 2).

:- use_module(.(sudoku)).
proftest(sudoku, test1, 2). % You can also test tes2, test3 and test4

:- use_module(.(zebra_argnames)).
proftest(zebra_argnames, zebra(_, _, _), 2).

% :- use_module(engine(stream_basic), [absolute_file_name/2]).
% :- use_module(lpdoc(docmaker)).
% % TODO: This example is outdated (synchronize with more recent LPdoc versions)
% proftest(prof_lpdoc, multi([lpdoc, autodoc, lpdoclib], doc_cmd(F, [], gen(all))), 3) :-
%     absolute_file_name(lpdocsrc(doc/'SETTINGS'), F).

:- use_module(.(wumpus)).
proftest(wumpus, w, 3).

% ===========================================================================

:- use_module(library(write), [write/1]).
:- use_module(library(streams), [nl/0]).
:- use_module(library(profilercc/profiler_auto_conf)).
:- use_module(library(profilercc/profiler_utils)).

:- use_module(library(compiler/global_module_options)). % TODO: why?

do_t0(M) :-
    write(proftest_t0(M)), nl,
    get_proftest(M, Mods, Goal, Cond),
    ( Mods = [] -> cc_auto_conf(ticks, Goal, Cond, Goals, Tree)
    ; cc_auto_conf(ticks, Mods, Goal, Cond, Goals, Tree)
    ),
    write(Goals),
    nl,
    write(Tree),
    nl,
    tree_to_tex(ticks,Tree). % TODO: optional?

do_t1(M) :- 
    write(proftest_t1(M)), nl,
    get_proftest(M, _, Goal, _),
    ( M = module3 -> % TODO: why?
        glbmod_add_package(module2, profilercc),
        glbmod_add_package(module3, profilercc)
    ; true
    ),
    profile_reset,
    ( M = module3 -> % TODO: why?
        (\+ profile(Goal) -> true ; true)
    ; profile(Goal)
    ),
    profile_dump.

get_proftest(M, Mods, Goal, Cond) :-
    proftest(M, Goal0, Cond), !,
    ( Goal0 = multi(Mods, Goal) -> true ; Goal = Goal0, Mods = [] ).

:- export(t0/0). % TODO: document
t0 :- do_all(t0).
:- export(t1/0). % TODO: document
t1 :- do_all(t1).

:- export(do_phase/2). % TODO: document
do_phase(t0, M) :- ( do_t0(M) -> true ; true ).
do_phase(t1, M) :- ( do_t1(M) -> true ; true ).

do_all(Phase) :-
    do_phase(Phase, bignums),
    do_phase(Phase, color_map),
    do_phase(Phase, flat),
    do_phase(Phase, guardians),
    do_phase(Phase, hanoi),
    do_phase(Phase, jugs),
    do_phase(Phase, knights),
    do_phase(Phase, module3),
    do_phase(Phase, mqu),
    do_phase(Phase, qsort),
%       do_phase(Phase, schedule),
    do_phase(Phase, size),
    do_phase(Phase, subst_exp),
    do_phase(Phase, sudoku),
    do_phase(Phase, zebra_argnames),
%       do_phase(Phase, prof_lpdoc),
    do_phase(Phase, wumpus).

