/*
 * Heartbeat scheduling C stubs for domainslib
 *
 * This version uses a token-based approach that works with any OCaml 5 runtime.
 * It periodically deposits tokens into per-domain slots.
 */

#include <pthread.h>
#include <stdatomic.h>
#include <unistd.h>
#include <stdlib.h>
#include <stdbool.h>
#include <signal.h>
#include <errno.h>

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/callback.h>

/* Maximum domains supported */
#define MAX_DOMAINS 128

/* Per-domain token array */
static _Atomic int* tokens = NULL;
static int num_slots = 0;

/* Heartbeat thread configuration */
static int interval_us = 500;
static int tokens_per_beat = 50;
static atomic_int running = 0;
static pthread_t heartbeat_tid;

/* Statistics */
static atomic_int heartbeat_count = 0;
static atomic_int tokens_used = 0;

/* Reference counting for pool management */
static pthread_mutex_t heartbeat_mutex = PTHREAD_MUTEX_INITIALIZER;
static int refcount = 0;
static bool initialized = false;

/* Heartbeat thread: adds tokens to all slots at regular intervals */
static void* heartbeat_loop(void* arg) {
    (void)arg;
    while (atomic_load(&running)) {
        usleep(interval_us);
        for (int i = 0; i < num_slots; i++) {
            atomic_fetch_add(&tokens[i], tokens_per_beat);
        }
        atomic_fetch_add(&heartbeat_count, 1);
    }
    return NULL;
}

/* Initialize token array (called once globally) */
CAMLprim value domainslib_heartbeat_init(value v_num_slots, value v_interval_us, value v_tokens_per_beat) {
    CAMLparam3(v_num_slots, v_interval_us, v_tokens_per_beat);

    pthread_mutex_lock(&heartbeat_mutex);
    if (!initialized) {
        num_slots = Int_val(v_num_slots);
        interval_us = Int_val(v_interval_us);
        tokens_per_beat = Int_val(v_tokens_per_beat);
        tokens = calloc(num_slots, sizeof(_Atomic int));
        if (!tokens) {
            pthread_mutex_unlock(&heartbeat_mutex);
            caml_failwith("Failed to allocate token array");
        }
        initialized = true;
    }
    pthread_mutex_unlock(&heartbeat_mutex);

    CAMLreturn(Val_unit);
}

/* Acquire heartbeat - increments refcount and starts thread if needed */
CAMLprim value domainslib_heartbeat_acquire(value unit) {
    CAMLparam1(unit);

    pthread_mutex_lock(&heartbeat_mutex);
    if (refcount == 0 && initialized) {
        /* Start heartbeat thread */
        atomic_store(&running, 1);

        /* Block signals in heartbeat thread */
        sigset_t mask, old_mask;
        sigfillset(&mask);
        pthread_sigmask(SIG_BLOCK, &mask, &old_mask);

        int err = pthread_create(&heartbeat_tid, NULL, heartbeat_loop, NULL);
        if (err) {
            pthread_mutex_unlock(&heartbeat_mutex);
            caml_failwith("Failed to create heartbeat thread");
        }

        pthread_sigmask(SIG_SETMASK, &old_mask, NULL);

        /* Seed initial tokens for all domains */
        for (int i = 0; i < num_slots; i++) {
            atomic_store(&tokens[i], 100);
        }
    }
    refcount++;
    pthread_mutex_unlock(&heartbeat_mutex);

    CAMLreturn(Val_unit);
}

/* Release heartbeat - decrements refcount and stops thread when zero */
CAMLprim value domainslib_heartbeat_release(value unit) {
    CAMLparam1(unit);

    pthread_mutex_lock(&heartbeat_mutex);
    refcount--;
    if (refcount == 0) {
        atomic_store(&running, 0);
        pthread_join(heartbeat_tid, NULL);
    }
    pthread_mutex_unlock(&heartbeat_mutex);

    CAMLreturn(Val_unit);
}

/* Take all tokens from a domain's slot (atomic exchange with 0) */
/* [@@noalloc] - hot path, must not allocate */
CAMLprim value domainslib_heartbeat_take_tokens(value v_slot) {
    int slot = Int_val(v_slot);
    if (slot < 0 || slot >= num_slots) return Val_int(0);
    int val = atomic_exchange(&tokens[slot], 0);
    if (val > 0) {
        atomic_fetch_add(&tokens_used, val);
    }
    return Val_int(val);
}

/* Set tokens for a domain's slot */
/* [@@noalloc] */
CAMLprim value domainslib_heartbeat_set_tokens(value v_slot, value v_val) {
    int slot = Int_val(v_slot);
    if (slot >= 0 && slot < num_slots) {
        atomic_store(&tokens[slot], Int_val(v_val));
    }
    return Val_unit;
}

/* Get heartbeat statistics for debugging */
CAMLprim value domainslib_heartbeat_stats(value unit) {
    (void)unit;
    CAMLparam0();
    CAMLlocal1(result);
    result = caml_alloc_tuple(2);
    Store_field(result, 0, Val_int(atomic_load(&heartbeat_count)));
    Store_field(result, 1, Val_int(atomic_load(&tokens_used)));
    CAMLreturn(result);
}

/* Reset statistics */
CAMLprim value domainslib_heartbeat_reset_stats(value unit) {
    (void)unit;
    atomic_store(&heartbeat_count, 0);
    atomic_store(&tokens_used, 0);
    return Val_unit;
}

/* Stub for new API - not used in this version */
CAMLprim value domainslib_heartbeat_setup(value v1, value v2) {
    (void)v1; (void)v2;
    return Val_unit;
}
