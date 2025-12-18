/*
 * Heartbeat scheduling C stubs for domainslib
 *
 * Uses the runtime's fiber-local state and external interrupt mechanism.
 * Based on the parallel library's implementation.
 */

#define CAML_INTERNALS

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/callback.h>
#include <caml/domain.h>
#include <caml/fiber.h>

#include <stdbool.h>
#include <pthread.h>
#include <errno.h>
#include <signal.h>
#include <stdatomic.h>

/* External runtime functions we added */
extern value caml_dynamic_make(value val);
extern value caml_dynamic_get(value dyn);

/* Forward declare the type from fiber.h */
typedef struct dynamic_thread_s *dynamic_thread_t;
extern void caml_dynamic_flush_thread(dynamic_thread_t thread);
extern void caml_external_interrupt_all_signal_safe(uintnat flags);
extern void (*caml_domain_external_interrupt_hook)(void);

/* Heartbeat interrupt flag - must not conflict with ST_INTERRUPT_FLAG (1) */
#define HB_INTERRUPT_FLAG ((uintnat)(1 << 1))

/* Reference counting for heartbeat thread management */
typedef struct {
  pthread_mutex_t mut;
  pthread_cond_t cond;
  uint64_t count;
} heartbeat_refcount_t;

static heartbeat_refcount_t heartbeat_refcount = {
  PTHREAD_MUTEX_INITIALIZER,
  PTHREAD_COND_INITIALIZER,
  0
};

/* Global configuration */
static value heartbeat_fls_key = Val_unit;      /* The dynamic variable key */
static value heartbeat_callback = Val_unit;      /* OCaml callback to invoke */
static uintnat heartbeat_interval_us = 500;     /* Interval in microseconds */

/* Previous hook to chain */
static void (*existing_domain_external_interrupt_hook)(void) = NULL;

/* Statistics */
static atomic_int heartbeat_count = 0;
static atomic_int callbacks_invoked = 0;

/*
 * External interrupt hook - called when HB_INTERRUPT_FLAG is set.
 * This runs in the domain that was interrupted, at a safe point.
 */
static void domainslib_external_interrupt_hook(void) {
  uintnat mask = ~HB_INTERRUPT_FLAG;
  atomic_uintnat *request = &Caml_state->requested_external_interrupt;

  /* Atomically check and clear the heartbeat flag */
  if (atomic_fetch_and_explicit(request, mask, memory_order_seq_cst) & HB_INTERRUPT_FLAG) {
    /* Get the fiber-local heartbeat state */
    value state = caml_dynamic_get(heartbeat_fls_key);
    /* Invoke the OCaml callback with the state */
    caml_callback(heartbeat_callback, state);
    atomic_fetch_add(&callbacks_invoked, 1);
  }

  /* Chain to any existing hook */
  if (existing_domain_external_interrupt_hook) {
    existing_domain_external_interrupt_hook();
  }
}

/*
 * Heartbeat thread - periodically interrupts all domains
 */
static void *heartbeat_thread_loop(void *param) {
  (void)param;

  struct timespec interval = {0};
  interval.tv_nsec = heartbeat_interval_us * 1000;

  while (true) {
    /* Wait for at least one pool to be active */
    pthread_mutex_lock(&heartbeat_refcount.mut);
    while (heartbeat_refcount.count == 0) {
      pthread_cond_wait(&heartbeat_refcount.cond, &heartbeat_refcount.mut);
    }
    pthread_mutex_unlock(&heartbeat_refcount.mut);

    /* Sleep for the interval */
    struct timespec remain = {0};
#ifdef __APPLE__
    int err = nanosleep(&interval, &remain);
    while (err == -1 && errno == EINTR) {
      err = nanosleep(&remain, &remain);
    }
#else
    int err = clock_nanosleep(CLOCK_MONOTONIC, 0, &interval, &remain);
    while (err == EINTR) {
      err = clock_nanosleep(CLOCK_MONOTONIC, 0, &remain, &remain);
    }
#endif

    if (err && err != -1) {
      caml_fatal_error("Heartbeat thread failed to sleep: %d\n", err);
    }

    /* Interrupt all domains - signal-safe! */
    caml_external_interrupt_all_signal_safe(HB_INTERRUPT_FLAG);
    atomic_fetch_add(&heartbeat_count, 1);
  }

  return NULL;
}

static void heartbeat_incref(void) {
  pthread_mutex_lock(&heartbeat_refcount.mut);
  if (heartbeat_refcount.count++ == 0) {
    pthread_cond_signal(&heartbeat_refcount.cond);
  }
  pthread_mutex_unlock(&heartbeat_refcount.mut);
}

static void heartbeat_decref(void) {
  pthread_mutex_lock(&heartbeat_refcount.mut);
  heartbeat_refcount.count--;
  pthread_mutex_unlock(&heartbeat_refcount.mut);
}

/*
 * Setup the heartbeat system - called once at startup.
 * Registers the callback and starts the heartbeat thread.
 */
CAMLprim value domainslib_heartbeat_setup(value v_interval_us, value v_fls_key, value v_callback) {
  CAMLparam3(v_interval_us, v_fls_key, v_callback);

  heartbeat_interval_us = Long_val(v_interval_us);
  heartbeat_fls_key = v_fls_key;
  heartbeat_callback = v_callback;

  /* Register as GC roots */
  caml_register_generational_global_root(&heartbeat_fls_key);
  caml_register_generational_global_root(&heartbeat_callback);

  CAMLreturn(Val_unit);
}

/*
 * Acquire heartbeat - called when a pool starts.
 * Spawns the heartbeat thread on first call and installs the hook.
 */
CAMLprim value domainslib_heartbeat_acquire(value unit) {
  CAMLparam1(unit);

  static atomic_int heartbeat_running = 0;
  int running = 0;

  /* Start thread only once, using CAS */
  if (atomic_load(&heartbeat_running) == 0 &&
      atomic_compare_exchange_strong(&heartbeat_running, &running, 1)) {

    /* Install our interrupt hook, chaining any existing one */
    existing_domain_external_interrupt_hook = caml_domain_external_interrupt_hook;
    caml_domain_external_interrupt_hook = &domainslib_external_interrupt_hook;

    /* Block signals in heartbeat thread */
    sigset_t mask, old_mask;
    sigfillset(&mask);
    pthread_sigmask(SIG_BLOCK, &mask, &old_mask);

    pthread_t thread;
    int err = pthread_create(&thread, NULL, heartbeat_thread_loop, NULL);
    if (err) {
      caml_failwith("Failed to create heartbeat thread");
    }

    pthread_sigmask(SIG_SETMASK, &old_mask, NULL);
  }

  heartbeat_incref();
  CAMLreturn(Val_unit);
}

/*
 * Release heartbeat - called when a pool stops.
 */
CAMLprim value domainslib_heartbeat_release(value unit) {
  CAMLparam1(unit);
  heartbeat_decref();
  CAMLreturn(Val_unit);
}

/*
 * Create a fiber-local dynamic variable with initial value.
 */
CAMLprim value domainslib_create_dynamic(value initial) {
  CAMLparam1(initial);
  CAMLlocal1(dyn);
  dyn = caml_dynamic_make(initial);
  CAMLreturn(dyn);
}

/*
 * Set fiber-local state directly on current fiber.
 * This is the fast path - no allocation, just writes to the fiber.
 */
CAMLprim value domainslib_set_fiber_state(value dyn, value val) {
  CAMLparam2(dyn, val);

  /* Write directly to current fiber's dyn/val slots */
  Caml_state->current_stack->dyn = dyn;
  Caml_state->current_stack->val = val;

  /* Flush any cached lookup (no-op in our simplified implementation) */
  caml_dynamic_flush_thread(Caml_state->dynamic_bindings);

  CAMLreturn(Val_unit);
}

/*
 * Get fiber-local state via dynamic lookup.
 */
CAMLprim value domainslib_get_fiber_state(value dyn) {
  return caml_dynamic_get(dyn);
}

/* Statistics */
CAMLprim value domainslib_heartbeat_stats(value unit) {
  (void)unit;
  CAMLparam0();
  CAMLlocal1(result);
  result = caml_alloc_tuple(2);
  Store_field(result, 0, Val_int(atomic_load(&heartbeat_count)));
  Store_field(result, 1, Val_int(atomic_load(&callbacks_invoked)));
  CAMLreturn(result);
}

CAMLprim value domainslib_heartbeat_reset_stats(value unit) {
  (void)unit;
  atomic_store(&heartbeat_count, 0);
  atomic_store(&callbacks_invoked, 0);
  return Val_unit;
}

/* Legacy stubs for compatibility - no longer used */
CAMLprim value domainslib_heartbeat_init(value v1, value v2, value v3) {
  (void)v1; (void)v2; (void)v3;
  return Val_unit;
}

CAMLprim value domainslib_heartbeat_take_tokens(value v1) {
  (void)v1;
  return Val_int(0);
}

CAMLprim value domainslib_heartbeat_set_tokens(value v1, value v2) {
  (void)v1; (void)v2;
  return Val_unit;
}
