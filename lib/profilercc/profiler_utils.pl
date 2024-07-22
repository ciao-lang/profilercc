:- module(profiler_utils, [
            profile/1,
            profile_call/1,
            profile_reset/0,
            profile_info/1,
            activate_trace/0,
            deactivate_trace/0,
            activate_hooks/0,
            deactivate_hooks/0
        ], [assertions, nativeprops]).

:- doc(title, "Profiler utilities").

:- doc(module, "This module provides predicates to interact with the
   profiler for particular calls.").

:- use_module(engine(io_basic)).
:- use_module(engine(hiord_rt), [call/1]).
:- use_module(library(read)).
:- use_module(library(stream_utils), [open_output/2, close_output/1, open_input/2, close_input/1]).
:- use_module(library(system)).
:- use_module(library(profilercc/profiler_rt),   [profile_module_init/1]).
:- use_module(library(profilercc/profiler_type), [profile_info_type/1]).

:- reexport(library(profilercc/profiler_cc)).
:- reexport(library(profilercc/profiler_utils_native)).
:- use_module(library(profilercc/profiler_utils_base)).

:- doc(bug, "Perhaps the time for preparing the call to a predicate is
   accumulated in the last called (or retried) predicate (this could
   be a bug or a behavior) (EMM)").

:- doc(bug, "The hash table implementation used by the profiler must
   be unified with the hash tables of the engine").

:- doc(bug, "This profiler currently do not support exception nor
   signal handling (EMM)").

% :- use_module(engine(internals), [term_to_meta/2]).

:- meta_predicate profile_call(goal).

% Note that undo/1 can not be used here (we need to cut the choice points).

profile_call(Goal) :-
%       term_to_meta(Goal0, Goal),
    '$metachoice'(CP),
    profile_leave_fail,
    '$metachoice'(C0),
    profile_enter_call,
    call(Goal),
%       '$meta_call'(Goal0),
    profile_leave_exit,
    '$metachoice'(C1),
    profile_enter_redo,
    (C0==C1 -> '$metacut'(CP) ; true).

:- meta_predicate profile(goal).

:- pred profile(Goal) :: cgoal # "Evaluates @var{Goal}, collect
   profile information related with the evaluation and dump the
   information.".

profile(Goal) :-
    catch(profile_call(Goal), E, profile_error(E)).

:- pred profile_info/1 => profile_info_type.

% TODO: Wrong! buffer size of pipe/2 is limited
%
%% profile_info(ProfileInfo) :-
%%      pipe(ReadFrom, WriteTo),
%%      current_output(CO),
%%      set_output(WriteTo),
%%      profile_dump,
%%      display('.'),
%%      close(WriteTo),
%%      set_output(CO),
%%      read_term(ReadFrom, ProfileInfo, []),
%%      close(ReadFrom).

% TODO: implement a profile_dump/1 that generates terms
profile_info(ProfileInfo) :-
    mktemp_in_tmp('profinfoXXXXXX', FileName),
    open_output(FileName, CO),
    profile_dump,
    display('.'),
    close_output(CO),
    open_input(FileName, CI),
    read_term(ProfileInfo, []),
    close_input(CI),
    delete_file(FileName).

profile_reset :-
    do_profile_reset,
    profile_module_init(_).

activate_trace :- set_trace_active(1).
deactivate_trace :- set_trace_active(0).

activate_hooks :- set_hooks_active(1).
deactivate_hooks :- set_hooks_active(0).
