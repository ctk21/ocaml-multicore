/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*                 Stephen Dolan, University of Cambridge                 */
/*                      Enguerrand Decorne, Tarides                       */
/*                                                                        */
/*   Copyright 2020 University of Cambridge                               */
/*   Copyright 2020 Tarides                                               */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

#ifndef CAML_EVENTLOG_H
#define CAML_EVENTLOG_H

typedef enum {
    EV_ENTRY,
    EV_EXIT,
    EV_COUNTER,
    EV_ALLOC,
    EV_FLUSH
} ev_type;

typedef enum {
    EV_DOMAIN_SPAWN,
    EV_DOMAIN_SEND_INTERRUPT,
    EV_DOMAIN_IDLE_WAIT,
    EV_EXPLICIT_GC_SET,
    EV_EXPLICIT_GC_STAT,
    EV_EXPLICIT_GC_MINOR,
    EV_EXPLICIT_GC_MAJOR,
    EV_EXPLICIT_GC_MAJOR_SLICE,
    EV_EXPLICIT_GC_FULL_MAJOR,
    EV_EXPLICIT_GC_COMPACT,
    EV_FINALISE_UPDATE_FIRST,
    EV_FINALISE_UPDATE_LAST,
    EV_INTERRUPT_GC,
    EV_INTERRUPT_REMOTE,
    EV_MAJOR,
    EV_MAJOR_EPHE_MARK,
    EV_MAJOR_EPHE_SWEEP,
    EV_MAJOR_FINISH_MARKING,
    EV_MAJOR_GC_CYCLE_DOMAINS,
    EV_MAJOR_GC_PHASE_CHANGE,
    EV_MAJOR_GC_STW,
    EV_MAJOR_MARK_ROOTS,
    EV_MAJOR_MARK_MAIN,
    EV_MAJOR_MARK_FINAL,
    EV_MAJOR_MARK,
    EV_MAJOR_MARK_GLOBAL_ROOTS_SLICE,
    EV_MAJOR_MARK_OPPORTUNISTIC,
    EV_MAJOR_ROOTS,
    EV_MAJOR_ROOTS_GLOBAL,
    EV_MAJOR_ROOTS_DYNAMIC_GLOBAL,
    EV_MAJOR_ROOTS_LOCAL,
    EV_MAJOR_ROOTS_C,
    EV_MAJOR_ROOTS_FINALISED,
    EV_MAJOR_ROOTS_HOOK,
    EV_MAJOR_SLICE,
    EV_MAJOR_SWEEP,
    EV_MINOR,
    EV_MINOR_CLEAR,
    EV_MINOR_FINALIZED,
    EV_MINOR_FINALIZERS_OLDIFY,
    EV_MINOR_GLOBAL_ROOTS,
    EV_MINOR_LEAVE_BARRIER,
    EV_MINOR_LOCAL_ROOTS,
    EV_MINOR_REF_TABLES,
    EV_MINOR_UPDATE_WEAK,
    EV_MINOR_REPLENISH_BUFFER,
    EV_STW_API_BARRIER,
    EV_STW_HANDLER,
    EV_STW_LEADER,

} ev_gc_phase;

typedef enum {
    EV_C_ALLOC_JUMP,
    EV_C_FORCE_MINOR_ALLOC_SMALL,
    EV_C_FORCE_MINOR_MAKE_VECT,
    EV_C_FORCE_MINOR_SET_MINOR_HEAP_SIZE,
    EV_C_MAJOR_MARK_SLICE_REMAIN,
    EV_C_MAJOR_MARK_SLICE_FIELDS,
    EV_C_MAJOR_MARK_SLICE_POINTERS,
    EV_C_MAJOR_WORK_EXTRA,
    EV_C_MAJOR_WORK_MARK,
    EV_C_MAJOR_WORK_SWEEP,
    EV_C_MINOR_PROMOTED,
    EV_C_REQUEST_MAJOR_ALLOC_SHR,
    EV_C_REQUEST_MAJOR_ADJUST_GC_SPEED,
    EV_C_REQUEST_MINOR_REALLOC_REF_TABLE,
    EV_C_REQUEST_MINOR_REALLOC_EPHE_REF_TABLE,
    EV_C_REQUEST_MINOR_REALLOC_CUSTOM_TABLE
} ev_gc_counter;

#ifdef CAML_INSTR

#define CAML_EVENTLOG_DO(f) if (Caml_state->eventlog_enabled &&\
                                 !Caml_state->eventlog_paused) f

#define CAML_EVENTLOG_INIT() caml_eventlog_init()
#define CAML_EVENTLOG_DISABLE() caml_eventlog_disable()
#define CAML_EV_BEGIN(p) caml_ev_begin(p)
#define CAML_EV_END(p) caml_ev_end(p)
#define CAML_EV_COUNTER(c, v) caml_ev_counter(c, v)
#define CAML_EV_ALLOC(s) caml_ev_alloc(s)
#define CAML_EV_ALLOC_FLUSH() caml_ev_alloc_flush()
#define CAML_EV_FLUSH() caml_ev_flush()

/* General note about the public API for the eventlog framework
   The caml_ev_* functions are no-op when called with the eventlog framework
   paused or disabled.
   caml_eventlog_* functions on the other hand may introduce side effects
   (such as write buffer flushes, or side effects in the eventlog internals.)

   All these functions should be called while holding the runtime lock.
*/

void caml_eventlog_init(void);
void caml_eventlog_disable(void);
void caml_ev_begin(ev_gc_phase phase);
void caml_ev_end(ev_gc_phase phase);
void caml_ev_counter(ev_gc_counter counter, uint64_t val);
void caml_ev_alloc(uint64_t size);
void caml_ev_alloc_flush(void);
void caml_ev_flush(void);

#else

#define CAML_EVENTLOG_DO(f) /**/

#define CAML_EVENTLOG_INIT() /**/
#define CAML_EVENTLOG_DISABLE() /**/
#define CAML_EV_BEGIN(p) /**/
#define CAML_EV_END(p) /**/
#define CAML_EV_COUNTER(c, v) /**/
#define CAML_EV_ALLOC(S) /**/
#define CAML_EV_ALLOC_FLUSH() /**/
#define CAML_EV_FLUSH() /**/

#endif /*CAML_INSTR*/

#endif /*CAML_EVENTLOG_H*/
