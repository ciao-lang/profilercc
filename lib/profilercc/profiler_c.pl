:- module(profiler_c, [], [assertions, nativeprops, foreign_interface]).

:- use_foreign_source(library(hashtable/hashtab)).
:- use_foreign_source(library(hashtable/lookupa)).
:- use_foreign_source(library(hashtable/recycle)).
:- use_foreign_source(library(hrtime/hrtime)).
% Add library(hashtable) and library(hrtime) to .h include paths
% (see profilercc:prepare_build_bin at profilercc.hooks.pl)
:- include(.(profilercc_decl_auto)).

:- use_foreign_source(profiler).
:- use_foreign_source(library(profilercc/profiler_c)).

/* for profiler_rt.pl */

:- initialization(profile_init).



:- export(get_profile_active/1).
:- trust pred get_profile_active(go(Active)) :: int
    + (foreign_low(prolog_get_profile_active)) #
    "Unifies Active with 1 if the profiler is turned on, or with 0 otherwise".

:- export(add_node_cc/2).
:- trust pred add_node_cc(in(Name), in(Arity)) :: atm * int
    + (foreign_low(prolog_add_node_cc), not_fails).

:- export(profile_init/0).
:- trust pred profile_init + (foreign_low(prolog_profile_init)).

:- export(cc_call/5).
:- trust pred cc_call(in(Name), in(Arity), in(Hooks), go(PrevECC), go(CutTo))
    :: atm * int * int * int * int + ( foreign_low(prolog_cc_call),
        not_fails ).

:- export(cc_redo_1/3).
:- trust pred cc_redo_1(in(ChPt0), in(ChPt1), in(CutTo)) :: int * int * int
    + (foreign_low(prolog_cc_redo_1), not_fails).

:- export(cc_redo_1_nf/2).
:- trust pred cc_redo_1_nf(in(ChPt1), in(CutTo)) :: int * int
    + (foreign_low(prolog_cc_redo_1_nf), not_fails).

:- export(cc_redo_2/1).
:- trust pred cc_redo_2(in(ActiveCC)) :: int
    + (foreign_low(prolog_cc_redo_2), fails).

:- export(cc_exit/3).
:- trust pred cc_exit(in(PrevECC), go(ActiveCC), go(ChPt))
    :: int * int * int
    + (foreign_low(prolog_cc_exit), not_fails).

:- export(cc_fail_1/1).
:- trust pred cc_fail_1(go(ChPt)) :: int
    + (foreign_low(prolog_cc_fail_1), not_fails).

:- export(cc_fail_2/1).
:- trust pred cc_fail_2(in(PrevECC)) :: int
    + (foreign_low(prolog_cc_fail_2), fails).


:- export(cc_exit_nc/2).
:- trust pred cc_exit_nc(in(PrevECC), in(CutTo)) :: int * int
    + (foreign_low(prolog_cc_exit_nc), not_fails).

:- export(cc_call_ncnf/4).
:- trust pred cc_call_ncnf(in(Name), in(Arity), in(Hooks), go(PrevECC))
    :: atm * int * int * int
    + (foreign_low(prolog_cc_call_ncnf), not_fails).

:- export(cc_exit_ncnf/1).
:- trust pred cc_exit_ncnf(in(PrevECC)) :: int
    + (foreign_low(prolog_cc_exit_ncnf), not_fails).

:- export(cc_call_nf/5).
:- trust pred cc_call_nf(in(Name), in(Arity), in(Hooks), go(PrevECC), go(CutTo))
    :: atm * int * int * int * int
    + (foreign_low(prolog_cc_call_nf), not_fails).


/* for profiler_utils_native */

:- export(dump_node_table_cc/0).
:- trust pred dump_node_table_cc + (foreign_low(prolog_dump_node_table_cc)).


:- export(have_overhead/2).
:- trust pred have_overhead(in(Name), in(Arity)) :: atm * int +
    (foreign_low(prolog_have_overhead)).

:- export(profile_dump/0).
:- trust pred profile_dump + (foreign_low(prolog_profile_dump)) # "Show the
   information collected by the profiler.".

:- export(do_profile_reset/0).
:- trust pred do_profile_reset + (foreign_low(prolog_profile_reset)) #
"Restart the profiler.  This option erases previously collected
   information.".

:- export(get_trace_active/1).
:- trust pred get_trace_active(go(Active)) :: int +
    (foreign_low(prolog_get_trace_active)) #
    "Return 1 if the trace is active, or 0 otherwise.".

:- export(set_trace_active/1).
:- trust pred set_trace_active(in(Active)) :: int +
    (foreign_low(prolog_set_trace_active)) #
    "If @var{Active} is 0, turn off the trace, otherwise it is turned on.".


:- export(get_hooks_active/1).
:- trust pred get_hooks_active(go(Active)) :: int +
    (foreign_low(prolog_get_hooks_active)) #
    "Return 1 if the hooks are active, or 0 otherwise.".

:- export(set_hooks_active/1).
:- trust pred set_hooks_active(in(Active)) :: int +
    (foreign_low(prolog_set_hooks_active)).

:- export(using_timestamp/1).
:- trust pred using_timestamp(go(UsingTimeStamp)) :: int +
    (foreign_low(prolog_using_timestamp)).

:- export(total_time/1).
:- trust pred total_time(go(TotalTime)) :: c_double +
    (foreign(total_time), returns(TotalTime)).

:- export(cost_center_edge_counts/7).
:- trust pred cost_center_edge_counts(Name0, Arity0, Name, Arity, Enter,
        Leave, Counts) :: atm * int * atm* int * int * int * int
    + (foreign_low(prolog_cost_center_edge_counts)).

:- export(cost_center_edge_ticks/7).
:- trust pred cost_center_edge_ticks(Name0, Arity0, Name, Arity, Enter,
        Leave, Ticks) :: atm * int * atm* int * int * int * int
    + (foreign_low(prolog_cost_center_edge_ticks)).


:- export(cost_center_node_counts/5).
:- trust pred cost_center_node_counts(Name, Arity, Enter,
        Leave, Counts) :: atm* int * int * int * int
    + (foreign_low(prolog_cost_center_node_counts)).

:- export(cost_center_node_ticks/5).
:- trust pred cost_center_node_ticks(Name, Arity, Enter,
        Leave, Ticks) :: atm* int * int * int * int
    + (foreign_low(prolog_cost_center_node_ticks)).

:- export(cost_center_global_counts/3).
:- trust pred cost_center_global_counts(Enter, Leave, Counts) :: int * int * int
    + (foreign_low(prolog_cost_center_global_counts)).

:- export(cost_center_global_ticks/3).
:- trust pred cost_center_global_ticks(Enter, Leave, Counts) :: int * int * int
    + (foreign_low(prolog_cost_center_global_ticks)).

:- export(profile_enter_call/0).
:- trust pred profile_enter_call
    + (foreign_low(prolog_profile_enter_call), not_fails)
#
    "Turn on the profiler.".

:- export(profile_enter_redo_1/0).
:- trust pred profile_enter_redo_1
    + (foreign_low(prolog_profile_enter_redo_1), fails)
#
    "Turn on the profiler.".

:- export(profile_leave_exit/0).
:- trust pred profile_leave_exit
    + (foreign_low(prolog_profile_leave_exit), not_fails)
# "Turn off the profiler.".

:- export(profile_leave_fail_1/0).
:- trust pred profile_leave_fail_1
    + (foreign_low(prolog_profile_leave_fail_1), fails)
# "Turn off the profiler.".

:- export(profile_leave_error/0).
:- trust pred profile_leave_error + (foreign_low(prolog_profile_leave_error)) #
    "Turn off the profiler.".
