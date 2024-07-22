:- module(profiler_rt, [], [assertions, nativeprops]).

:- doc(title, "Runtime support for profiling cost centers").
:- doc(author, "Edison Mera").

:- use_package(library(profilercc/profiler_decl)).
:- use_module(library(profilercc/profiler_c), [
    add_node_cc/2,
    cc_redo_1/3,
    cc_redo_1_nf/2,
    cc_redo_2/1,
    cc_fail_1/1,
    cc_fail_2/1,
    cc_exit_nc/2
    ]).

:- reexport(library(profilercc/profiler_c), [
    cc_call/5,
    cc_call_nf/5,
    cc_call_ncnf/4,
    cc_exit/3,
    cc_exit_nc/2,
    cc_exit_ncnf/1,
    profile_init/0,
    get_profile_active/1
    ]).

:- export(profile_module_init/1).
profile_module_init(M) :-
    '$cc$'(M, F, N),
    atom_concat(M,  ':', M0),
    atom_concat(M0, F,   P),
    add_node_cc(P, N),
    fail.
profile_module_init(_).

:- export(cc_fail/2).
cc_fail(_, ChPt) :-
    cc_fail_1(ChPt).
cc_fail(PrevECC, _) :-
    cc_fail_2(PrevECC).

:- export(cc_redo/4).
cc_redo(_, ChPt0, ChPt1, CutTo) :-
    cc_redo_1(ChPt0, ChPt1, CutTo).
cc_redo(ActiveCC, _, _, _) :-
    cc_redo_2(ActiveCC).

:- export(cc_redo_nf/3).
cc_redo_nf(_, ChPt1, CutTo) :-
    cc_redo_1_nf(ChPt1, CutTo).
cc_redo_nf(ActiveCC, _, _) :-
    cc_redo_2(ActiveCC).

% (2)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  Specialized versions of hooks used when some static properties has      %%
%%  been inferred (like non failure or determinism).                        %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- export(cc_fail_nc/1).
cc_fail_nc(_).
cc_fail_nc(PrevECC) :-
    cc_fail_2(PrevECC).
