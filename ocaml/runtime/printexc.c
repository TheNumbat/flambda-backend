/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           */
/*                                                                        */
/*   Copyright 1996 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

#define CAML_INTERNALS

/* Print an uncaught exception and abort */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "caml/backtrace.h"
#include "caml/callback.h"
#include "caml/debugger.h"
#include "caml/fail.h"
#include "caml/misc.h"
#include "caml/mlvalues.h"
#include "caml/printexc.h"
#include "caml/memory.h"
#include "caml/memprof.h"

struct stringbuf {
  char * ptr;
  char * end;
  char data[256];
};

static void add_char(struct stringbuf *buf, char c)
{
  if (buf->ptr < buf->end) *(buf->ptr++) = c;
}

static void add_string(struct stringbuf *buf, const char *s)
{
  size_t len = strlen(s);
  if (buf->ptr + len > buf->end) len = buf->end - buf->ptr;
  if (len > 0) memmove(buf->ptr, s, len);
  buf->ptr += len;
}

CAMLexport char * caml_format_exception(value exn)
{
  Caml_check_caml_state();
  mlsize_t start, i;
  value bucket, v;
  struct stringbuf buf;
  char intbuf[64];
  char * res;

  buf.ptr = buf.data;
  buf.end = buf.data + sizeof(buf.data) - 1;
  if (Tag_val(exn) == 0) {
    add_string(&buf, String_val(Field(Field(exn, 0), 0)));
    /* Check for exceptions in the style of Match_failure and Assert_failure */
    if (Wosize_val(exn) == 2 &&
        Is_block(Field(exn, 1)) &&
        Tag_val(Field(exn, 1)) == 0 &&
        caml_is_special_exception(Field(exn, 0))) {
      bucket = Field(exn, 1);
      start = 0;
    } else {
      bucket = exn;
      start = 1;
    }
    add_char(&buf, '(');
    for (i = start; i < Wosize_val(bucket); i++) {
      if (i > start) add_string(&buf, ", ");
      v = Field(bucket, i);
      if (Is_long(v)) {
        snprintf(intbuf, sizeof(intbuf),
                 "%" ARCH_INTNAT_PRINTF_FORMAT "d", Long_val(v));
        add_string(&buf, intbuf);
      } else if (Tag_val(v) == String_tag) {
        add_char(&buf, '"');
        add_string(&buf, String_val(v));
        add_char(&buf, '"');
      } else {
        add_char(&buf, '_');
      }
    }
    add_char(&buf, ')');
  } else
    add_string(&buf, String_val(Field(exn, 0)));

  *buf.ptr = 0;              /* Terminate string */
  i = buf.ptr - buf.data + 1;
  res = caml_stat_alloc_noexc(i);
  if (res == NULL) return NULL;
  memmove(res, buf.data, i);
  return res;
}


#ifdef NATIVE_CODE
#  define DEBUGGER_IN_USE 0
#else
#  define DEBUGGER_IN_USE caml_debugger_in_use
#endif

/* Default C implementation in case the OCaml one is not registered. */
static void default_fatal_uncaught_exception(value exn, const char *msg2)
{
  char * msg;
  const value * at_exit;
  int saved_backtrace_active, saved_backtrace_pos;

  /* Build a string representation of the exception */
  msg = caml_format_exception(exn);
  /* Perform "at_exit" processing, ignoring all exceptions that may
     be triggered by this */
  saved_backtrace_active = Caml_state->backtrace_active;
  saved_backtrace_pos = Caml_state->backtrace_pos;
  Caml_state->backtrace_active = 0;
  at_exit = caml_named_value("Pervasives.do_at_exit");
  /* In the event of an asynchronous exception occurring, it will still get
     caught here, because of the semantics of [caml_callback_exn]. */
  if (at_exit != NULL) caml_callback_exn(*at_exit, Val_unit);
  Caml_state->backtrace_active = saved_backtrace_active;
  Caml_state->backtrace_pos = saved_backtrace_pos;
  /* Display the uncaught exception */
  if (msg2)
    fprintf(stderr, "Fatal error: exception (from %s) %s\n", msg2, msg);
  else
    fprintf(stderr, "Fatal error: exception %s\n", msg);
  caml_stat_free(msg);
  /* Display the backtrace if available */
  if (Caml_state->backtrace_active && !DEBUGGER_IN_USE)
    caml_print_exception_backtrace();
}

int caml_abort_on_uncaught_exn = 0; /* see afl.c */

void caml_fatal_uncaught_exception_with_message(value exn, const char *msg)
{
  const value *handle_uncaught_exception;

  handle_uncaught_exception =
    caml_named_value("Printexc.handle_uncaught_exception");

  if (handle_uncaught_exception != NULL)
    /* [Printexc.handle_uncaught_exception] does not raise exception. */
    caml_callback2(*handle_uncaught_exception, exn, Val_bool(DEBUGGER_IN_USE));
  else
    default_fatal_uncaught_exception(exn, msg);
  /* Terminate the process */
  if (caml_abort_on_uncaught_exn) {
    abort();
  } else {
    exit(2);
  }
}

void caml_fatal_uncaught_exception(value exn)
{
  caml_fatal_uncaught_exception_with_message(exn, NULL);
}
