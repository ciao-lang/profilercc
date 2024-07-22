/* Global declaration */

#include <stdio.h>
#include <ciao_prolog.h>
#include <ciao/eng.h>
#include <ciao/eng_profile.h>
#include <ciao/timing.h>
#include <ciao/eng_registry.h>
#include "profiler.h"

/* get_profile_active/1 */

#if !defined(ABSMACH_OPT__profilecc) && !defined(ABSMACH_OPT__profile_calls)
intmach_t profile_flags = 0;
#endif

CBOOL__PROTO(prolog_get_profile_active)
{
  CBOOL__LASTUNIFY(MakeSmall(profile_flags == 0 ? 0 : 1),X(0));
}

/* add_node_cc/2 */

void add_node_cc(struct ht_tab *vl, definition_t *functor);

CBOOL__PROTO(prolog_add_node_cc)
{
  definition_t *f;
  DEREF(X(0),X(0));
  DEREF(X(1),X(1));
  f=GET_DEFINITION(GetString(X(0)), TaggedToIntmach(X(1)));
  add_node_cc(active_frame[1].node_table_cc, f);
  return TRUE;
}

/* profile_init/0 */

CBOOL__PROTO(prolog_profile_init)
{
  profile_init();
  return TRUE;
}

/* cc_call/5 */

CBOOL__PROTO(prolog_cc_call_ncnf);

CBOOL__PROTO(prolog_cc_call)
{
  CBOOL__UnifyCons(ChoiceToTagged(w->choice),X(4));
  return prolog_cc_call_ncnf(Arg);
}

/* cc_redo_1/3 */

CBOOL__PROTO(prolog_cc_redo_1)
{
  if (profile_flags != 0) {
    PROFILE__TIME_INI;
    choice_t *n1, *n2;
    DEREF(X(0),X(0));
    DEREF(X(1),X(1));
    n1=ChoiceFromTagged(X(0));
    n2=ChoiceFromTagged(X(1));
    if (n1==n2) {
      DEREF(X(2),X(2));
      choice_t *b=ChoiceFromTagged(X(2));
      SetChoice(b);
    }
    PROFILE__TIME_END;
  } else {
    w->choice=w->previous_choice; /* DOCUT */
  }
  return TRUE;
}

/* cc_redo_1_nf/2 */

CBOOL__PROTO(prolog_cc_redo_1_nf)
{
  if (profile_flags != 0) {
    PROFILE__TIME_INI;
    choice_t *n1, *n2;
    DEREF(X(0),X(0));
    DEREF(X(1),X(1));
    n1=ChoiceFromTagged(X(1));
    n2=ChoiceFromTagged(X(0));
    if (n1==n2) {
      SetChoice(n1);
    }
    PROFILE__TIME_END;
  } else {
    w->choice=w->previous_choice; /* DOCUT */
  }
  return TRUE;
}

/* cc_redo_2/1 */

CBOOL__PROTO(prolog_cc_redo_2)
{
  if (profile_flags != 0) {
    inttime_t d;
    PROFILE__TIME_INI;
    DEREF(X(0),X(0));
#if defined(PROFILE__WITH_HOOKS)
    if (profile_hooks && active_ecc->hooks) {
      d=tick_last_addition-tick_profiling;
      tick_profiling+=tick_ini_profiling-tick_last_addition;
    } else
#endif
      d=tick_ini_profiling-tick_profiling;
    active_ecc->entry_time-=d;
    active_ecc=TermToPointer(edge_cc_t, X(0));
#if defined(PROFILE__WITH_HOOKS)
    if (profile_hooks) {
      if (active_ecc->hooks)
        ENABLE_HOOKS(active_ecc->functor[0])
      else
        DISABLE_HOOKS
    }
#endif
    active_ecc->entry_port=ENTER_REDO;
    active_ecc->entry_time+=d;
    ShowFuncPoint2(Output_Stream_Ptr->streamfile,"cc-redo",0,0,
      active_ecc->functor[0],active_ecc->functor[1]);
    tick_last_addition=tick_ini_profiling;
    PROFILE__TIME_END;
  }
  return FALSE;
}

/* cc_exit/3 */

CVOID__PROTO(prolog_cc_exit_common)
{
  inttime_t d;
  DEREF(X(0),X(0));
  ShowFuncPoint2(Output_Stream_Ptr->streamfile,"cc-exit",0,0,
    active_ecc->functor[0],active_ecc->functor[1]);
#if defined(PROFILE__WITH_HOOKS)
  if (profile_hooks && active_ecc->hooks) {
    d=tick_last_addition-tick_profiling;
    tick_profiling+=tick_ini_profiling-tick_last_addition;
  } else
#endif
    d=tick_ini_profiling-tick_profiling;
  active_ecc->counts[active_ecc->entry_port][LEAVE_EXIT]++;
  active_ecc->times[active_ecc->entry_port][LEAVE_EXIT]+=d-active_ecc->entry_time;
  active_ecc->entry_time=0;
#if defined(PROFILE__WITH_HOOKS)
  active_ecc->cuts[active_ecc->entry_port][LEAVE_EXIT]+=active_ecc->entry_cuts;
  active_ecc->scuts[active_ecc->entry_port][LEAVE_EXIT]+=active_ecc->entry_scuts;
  active_ecc->entry_cuts=0;
  active_ecc->entry_scuts=0;
#endif
  active_ecc=TermToPointer(edge_cc_t, X(0));
#if defined(PROFILE__WITH_HOOKS)
  if (profile_hooks) {
    if (active_ecc->hooks)
      ENABLE_HOOKS(active_ecc->functor[0])
    else
      DISABLE_HOOKS
  }
#endif
  active_ecc->entry_time+=d;
  tick_last_addition=tick_ini_profiling;
}

CBOOL__PROTO(prolog_cc_exit)
{
  CBOOL__UnifyCons(ChoiceToTagged(w->choice),X(2));
  if (profile_flags != 0) {
    bool_t r;
    edge_cc_t *cc;
    PROFILE__TIME_INI;
    cc=active_ecc;
    prolog_cc_exit_common(Arg);
    r=cunify(Arg,PointerToTerm(cc),X(1));
    PROFILE__TIME_END;
    return r;
  }
  else
    return TRUE;
}

/* cc_fail_1/1 */

CBOOL__PROTO(prolog_cc_fail_1)
{
  if (profile_flags != 0) {
    CBOOL__UnifyCons(ChoiceToTagged(w->choice),X(0));
  } else {
    w->choice=w->previous_choice; /* DOCUT */
  }
  return TRUE;
}

/* cc_fail_2/1 */

CBOOL__PROTO(prolog_cc_fail_2)
{
  if (profile_flags != 0) {
    inttime_t d;
    PROFILE__TIME_INI;
    DEREF(X(0),X(0));
    ShowFuncPoint2(Output_Stream_Ptr->streamfile,"cc-fail",0,0,
      active_ecc->functor[0],active_ecc->functor[1]);
#if defined(PROFILE__WITH_HOOKS)
    if (profile_hooks && active_ecc->hooks) {
      d=tick_last_addition-tick_profiling;
      tick_profiling+=tick_ini_profiling-tick_last_addition;
    } else
#endif
      d=tick_ini_profiling-tick_profiling;
    active_ecc->counts[active_ecc->entry_port][LEAVE_FAIL]++;
    active_ecc->times[active_ecc->entry_port][LEAVE_FAIL]+=d-active_ecc->entry_time;
    active_ecc->entry_time=0;
#if defined(PROFILE__WITH_HOOKS)
    active_ecc->cuts[active_ecc->entry_port][LEAVE_FAIL]+=active_ecc->entry_cuts;
    active_ecc->scuts[active_ecc->entry_port][LEAVE_FAIL]+=active_ecc->entry_scuts;
    active_ecc->entry_cuts=0;
    active_ecc->entry_scuts=0;
#endif
    active_ecc=TermToPointer(edge_cc_t, X(0));
#if defined(PROFILE__WITH_HOOKS)
    if (profile_hooks) {
      if (active_ecc->hooks)
        ENABLE_HOOKS(active_ecc->functor[0])
      else
        DISABLE_HOOKS
    }
#endif
    active_ecc->entry_time+=d;
    tick_last_addition=tick_ini_profiling;
    PROFILE__TIME_END;
  }
  return FALSE;
}

/* cc_exit_nc/2 */

CBOOL__PROTO(prolog_cc_exit_nc)
{
  if (profile_flags != 0) {
    PROFILE__TIME_INI;
    prolog_cc_exit_common(Arg);
    DEREF(X(1),X(1));
    choice_t *b=ChoiceFromTagged(X(1));
    SetChoice(b);
    PROFILE__TIME_END;
  }
  return TRUE;
}

/* cc_call_ncnf/4 */

CBOOL__PROTO(prolog_cc_call_ncnf)
{
  if (profile_flags != 0) {
    bool_t r;
    inttime_t d;
    definition_t *f[2];
    PROFILE__TIME_INI;
    DEREF(X(0),X(0));
    DEREF(X(1),X(1));
    f[0]=GET_DEFINITION(GetString(X(0)), TaggedToIntmach(X(1)));
    f[1]=active_ecc->functor[0];
#if defined(PROFILE__WITH_HOOKS)
    if (profile_hooks && active_ecc->hooks) {
      d=tick_last_addition-tick_profiling;
      tick_profiling+=tick_ini_profiling-tick_last_addition;
    } else
#endif
      d=tick_ini_profiling-tick_profiling;
    active_ecc->entry_time-=d;
    r=cunify(Arg,PointerToTerm(active_ecc),X(3));
    active_ecc=get_edge_cc(active_frame->edge_table_cc, f);
#if defined(PROFILE__WITH_HOOKS)
    DEREF(X(2),X(2));
    active_ecc->hooks=TaggedToIntmach(X(2));
    if (profile_hooks) {
      if (active_ecc->hooks) {
        ENABLE_HOOKS(f[0]);
      }
      else {
        DISABLE_HOOKS;
      }
    }
#endif
    active_ecc->entry_port=ENTER_CALL;
    active_ecc->entry_time+=d;
    ShowFuncPoint2(Output_Stream_Ptr->streamfile,"cc-call",0,0,
      active_ecc->functor[0],active_ecc->functor[1]);
    tick_last_addition=tick_ini_profiling;
    PROFILE__TIME_END;
    return r;
  }
  else
    return TRUE;
}

/* cc_exit_ncnf/1 */

CBOOL__PROTO(prolog_cc_exit_ncnf)
{
  if (profile_flags != 0) {
    PROFILE__TIME_INI;
    prolog_cc_exit_common(Arg);
    PROFILE__TIME_END;
  }
  return TRUE;
}

/* cc_call_nf/5 */

CBOOL__PROTO(prolog_cc_call_nf)
{
  CBOOL__UnifyCons(ChoiceToTagged(w->choice),X(4));
  return prolog_cc_call_ncnf(Arg);
}

/* dump_node_table_cc/0 */

CBOOL__PROTO(prolog_dump_node_table_cc)
{
  if (ht_first(active_frame[1].node_table_cc)) do {
      definition_t *r;
      char buffer[STATICMAXATOM+MAXSTRNUMBERSIZE+1];
      r=*(definition_t **)ht_stuff(active_frame[1].node_table_cc);
      fprintf(Output_Stream_Ptr->streamfile, "%s\n", functor_name(buffer, r));
    } while (ht_next(active_frame[1].node_table_cc));
  return TRUE;
}

/* have_overhead/2 */

CBOOL__PROTO(prolog_have_overhead)
{
  bool_t result;
  definition_t *f;
  DEREF(X(0),X(0));
  DEREF(X(1),X(1));
  f=GET_DEFINITION(GetString(X(0)), TaggedToIntmach(X(1)));
  result=functor_have_overhead(active_frame+1,f);
  return result;
}

/* profile_dump/0 */

CBOOL__PROTO(prolog_profile_dump)
{
  profile_dump(active_frame+1, Output_Stream_Ptr->streamfile);
  return TRUE;
}


/* do_profile_reset/0 */

CBOOL__PROTO(prolog_profile_reset)
{
  profile_reset(active_frame+1);
  return TRUE;
}

/* get_trace_active/1 */

CBOOL__PROTO(prolog_get_trace_active)
{
#if defined(PROFILE__TRACER)
  CBOOL__LASTUNIFY(MakeSmall(profile_trace),X(0));
#else
  CBOOL__LASTUNIFY(MakeSmall(0),X(0));
#endif
}

/* set_trace_active/1 */

CBOOL__PROTO(prolog_set_trace_active)
{
#if defined(PROFILE__TRACER)
  ERR__FUNCTOR("set_trace_active", 1);
  DEREF(X(0),X(0));
  if(!IsInteger(X(0)))
    BUILTIN_ERROR(TYPE_ERROR(INTEGER),X(0),1);
  profile_trace=TaggedToIntmach(X(0));
  return TRUE;
#else
  return FALSE;
#endif
}

/* get_hooks_active/1 */

CBOOL__PROTO(prolog_get_hooks_active)
{
#if defined(PROFILE__WITH_HOOKS)
  CBOOL__LASTUNIFY(MakeSmall(profile_hooks),X(0));
#else
  CBOOL__LASTUNIFY(MakeSmall(0),X(0));
#endif
}

/* set_hooks_active/1 */

CBOOL__PROTO(prolog_set_hooks_active)
{
#if defined(PROFILE__WITH_HOOKS)
  ERR__FUNCTOR("set_hooks_active", 1);
  DEREF(X(0),X(0));
  if(!IsInteger(X(0)))
    BUILTIN_ERROR(TYPE_ERROR(INTEGER),X(0),1);
  profile_hooks=TaggedToIntmach(X(0));
  return TRUE;
#else
  return FALSE;
#endif
}

/* using_timestamp/1 */

CBOOL__PROTO(prolog_using_timestamp)
{
  int using_timestamp;
#if defined(USE_TIMESTAMP)
  using_timestamp=1;
#else
  using_timestamp=0;
#endif
  CBOOL__LASTUNIFY(MakeSmall(using_timestamp),X(0));
}

/* total_time/1 */

double total_time(void)
{
  return (double)(tick_last_addition - tick_start);
}

/* cost_center_edge_counts/7 */

CBOOL__PROTO(prolog_cost_center_edge_counts)
{
  edge_cc_t *ecc;
  definition_t *f[2];
  int enter, leave;

  DEREF(X(0),X(0));
  DEREF(X(1),X(1));
  DEREF(X(2),X(2));
  DEREF(X(3),X(3));
  DEREF(X(4),X(4));
  DEREF(X(5),X(5));

  f[0]=GET_DEFINITION(GetString(X(0)), TaggedToIntmach(X(1)));
  f[1]=GET_DEFINITION(GetString(X(2)), TaggedToIntmach(X(3)));
  ecc=get_edge_cc(active_frame->edge_table_cc, f);

  enter=TaggedToIntmach(X(4));
  leave=TaggedToIntmach(X(5));
  CBOOL__LASTUNIFY(MakeSmall(ecc->counts[enter][leave]),X(6));
}

/* cost_center_edge_ticks/7 */

CBOOL__PROTO(prolog_cost_center_edge_ticks)
{
  edge_cc_t *ecc;
  definition_t *f[2];
  int enter, leave;

  DEREF(X(0),X(0));
  DEREF(X(1),X(1));
  DEREF(X(2),X(2));
  DEREF(X(3),X(3));
  DEREF(X(4),X(4));
  DEREF(X(5),X(5));

  f[0]=GET_DEFINITION(GetString(X(0)), TaggedToIntmach(X(1)));
  f[1]=GET_DEFINITION(GetString(X(2)), TaggedToIntmach(X(3)));
  ecc=get_edge_cc(active_frame->edge_table_cc, f);

  enter=TaggedToIntmach(X(4));
  leave=TaggedToIntmach(X(5));
  CBOOL__LASTUNIFY(BoxFloat(ecc->times[enter][leave]),X(6));
}

/* cost_center_node_counts/5 */

CBOOL__PROTO(prolog_cost_center_node_counts)
{
  edge_cc_t *ecc;
  ht_tab *cct = active_frame->edge_table_cc;
  definition_t *f;
  int enter, leave;
  unsigned long int counts = 0;

  DEREF(X(0),X(0));
  DEREF(X(1),X(1));
  DEREF(X(2),X(2));
  DEREF(X(3),X(3));

  f=GET_DEFINITION(GetString(X(0)), TaggedToIntmach(X(1)));
  enter=TaggedToIntmach(X(2));
  leave=TaggedToIntmach(X(3));
  
  if (ht_first(cct)) do {
      ecc=(edge_cc_t *)ht_stuff(cct);
      if (ecc->functor[0]==f) {
        counts+=ecc->counts[enter][leave];
      }
    }
  while (ht_next(cct));
  CBOOL__LASTUNIFY(MakeSmall(counts),X(4));
}

/* cost_center_node_ticks/5 */

CBOOL__PROTO(prolog_cost_center_node_ticks)
{
  edge_cc_t *ecc;
  ht_tab *cct = active_frame->edge_table_cc;
  definition_t *f;
  int enter, leave;
  inttime_t times = 0;

  DEREF(X(0),X(0));
  DEREF(X(1),X(1));
  DEREF(X(2),X(2));
  DEREF(X(3),X(3));

  f=GET_DEFINITION(GetString(X(0)), TaggedToIntmach(X(1)));
  enter=TaggedToIntmach(X(2));
  leave=TaggedToIntmach(X(3));
  
  if (ht_first(cct)) do {
      ecc=(edge_cc_t *)ht_stuff(cct);
      if (ecc->functor[0]==f) {
        times+=ecc->times[enter][leave];
      }
    }
  while (ht_next(cct));
  CBOOL__LASTUNIFY(BoxFloat(times),X(4));
}

/* cost_center_global_counts/3 */

CBOOL__PROTO(prolog_cost_center_global_counts)
{
  edge_cc_t *ecc;
  ht_tab *cct = active_frame->edge_table_cc;
  definition_t *f;
  int enter, leave;
  unsigned long int counts = 0;

  DEREF(X(0),X(0));
  DEREF(X(1),X(1));

  enter=TaggedToIntmach(X(0));
  leave=TaggedToIntmach(X(1));
  
  if (ht_first(cct)) do {
      ecc=(edge_cc_t *)ht_stuff(cct);
      counts+=ecc->counts[enter][leave];
    }
  while (ht_next(cct));
  CBOOL__LASTUNIFY(MakeSmall(counts),X(2));
}

/* cost_center_global_ticks/3 */

CBOOL__PROTO(prolog_cost_center_global_ticks)
{
  edge_cc_t *ecc;
  ht_tab *cct = active_frame->edge_table_cc;
  definition_t *f;
  int enter, leave;
  inttime_t times = 0;

  DEREF(X(0),X(0));
  DEREF(X(1),X(1));

  enter=TaggedToIntmach(X(0));
  leave=TaggedToIntmach(X(1));
  
  if (ht_first(cct)) do {
      ecc=(edge_cc_t *)ht_stuff(cct);
      times+=ecc->times[enter][leave];
    }
  while (ht_next(cct));
  CBOOL__LASTUNIFY(BoxFloat(times),X(2));
}

/* For profiler_utils_base.pl */

static void profile_enter(int entry_port, definition_t *functor) {
  definition_t *f[2]={NULL, NULL};
  inttime_t d;
  if (active_frame<prof_frames) {
    tick_start+=tick_ini_profiling-tick_last_addition;
    profile_flags=1;
    d=tick_ini_profiling-tick_profiling;
  } else {
#if defined(PROFILE__WITH_HOOKS)
    if (profile_hooks && active_ecc->hooks) {
      d=tick_last_addition-tick_profiling;
      tick_profiling+=tick_ini_profiling-tick_last_addition;
    } else
#endif
      d=tick_ini_profiling-tick_profiling;
  }
  active_frame++;
  active_ecc=get_edge_cc(active_frame->edge_table_cc, f);
  active_ecc->entry_port=ENTER_CALL;
#if defined(PROFILE__WITH_HOOKS)
  active_ecc->hooks=profile_rcc; /* Don't accumulate costs in rcc */
  if (profile_hooks) {
    if (active_ecc->hooks) {
      ENABLE_HOOKS(functor);
    } else {
      DISABLE_HOOKS; 
    }
  }
#endif
  tick_last_addition=tick_ini_profiling;
  active_ecc->entry_time=d;
}

static void profile_leave(int output_port) {
  inttime_t d;
#if defined(PROFILE__WITH_HOOKS)
  if (profile_hooks && active_ecc->hooks) {
    d=tick_last_addition-tick_profiling;
    tick_profiling+=tick_ini_profiling-tick_last_addition;
  } else
#endif
    d=tick_ini_profiling-tick_profiling;
  active_ecc->counts[active_ecc->entry_port][output_port]++;
  active_ecc->times[active_ecc->entry_port][output_port]+=d-active_ecc->entry_time;
  active_ecc->entry_time=0;
  active_ecc=NULL;
#if defined(PROFILE__WITH_HOOKS)
  if (profile_hooks) {
    DISABLE_HOOKS;
  }
#endif
  if (active_frame==prof_frames) {
    profile_flags=0;
  }
  tick_last_addition=tick_ini_profiling;
  active_frame--;
}

/* profile_enter_call/0 */

void profile_enter_call_(void)
{
  PROFILE__TIME_INI;
  profile_enter(ENTER_CALL, profile_enter_call);
  PROFILE__TIME_END;
}

CBOOL__PROTO(prolog_profile_enter_call)
{
  profile_enter_call_();
  return TRUE;
}

/* profile_enter_redo_1/0 */

CBOOL__PROTO(prolog_profile_enter_redo_1)
{
  PROFILE__TIME_INI;
  profile_enter(ENTER_REDO, profile_enter_redo_1);
  PROFILE__TIME_END;
  return FALSE;
}

/* profile_leave_exit/0 */

void profile_leave_exit_(void)
{
  PROFILE__TIME_INI;
  profile_leave(LEAVE_EXIT);
  PROFILE__TIME_END;
  PROFILE__TIME_FLUSH;
}

CBOOL__PROTO(prolog_profile_leave_exit)
{
  profile_leave_exit_();
  return TRUE;
}

/* profile_leave_fail_1/0 */

CBOOL__PROTO(prolog_profile_leave_fail_1)
{
  PROFILE__TIME_INI;
  profile_leave(LEAVE_FAIL);
  PROFILE__TIME_END;
  PROFILE__TIME_FLUSH;
  return FALSE;
}

/* profile_leave_error/0 */

CBOOL__PROTO(prolog_profile_leave_error)
{
  PROFILE__TIME_INI;
  profile_leave(LEAVE_EXIT);
  PROFILE__TIME_END;
  PROFILE__TIME_FLUSH;
  return TRUE;
}
