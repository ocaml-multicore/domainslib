## Unreleased

* Make ~num_domains argument for Task.setup_pool optional (#87, #91)

## v0.5.0

This release includes:

* Bug fix for `parallel_for_reduce` on empty loops.
* Make Chan.t and Task.promise injective #69
* Add lockfree dependency #70
* CI fixes (#73, #76)
* Breaking change: Rename `num_additional_domains` to `num_domains` for setup_pool
* Documentation updates (#80, #81, #82)

## v0.4.2

Includes Effect.eff -> Effect.t change from OCaml trunk. (#65)

## v0.4.1

This release fixes compatibility with OCaml 5.00.0+trunk in #61. Breaks compatibility with older Multicore variants 4.12.0+domains and 4.12.0+domains+effects

## v0.4.0

This release includes:

* Usage of effect handlers for task creation. This introduces a breaking change; all computations need to be enclosed in a Task.run function. See #51.
* Multi_channel uses a per-channel domain-local key, removing the global key. #50
* Bug fixes in parallel_scan. #60

## v0.3.2

Corresponding updates for breaking changes introduced in ocaml-multicore/ocaml-multicore#704

* Updated with the new interface Domain.cpu_relax
* Domain.timer_ticks replaced with Mirage clock.

## v0.3.1

* #45 adds support for named pools. This is a breaking change with setup_pool taking an optional name parameter and an extra unit parameter.
* A minor bug fix in parallel_for_reduce.

## v0.3.0

This release includes:

* A breaking change for Task pools where the num_domains argument has been renamed num_additional_domains to clear up potential confusion; see #31.
* A new work-stealing scheduler for Task pools using domain local Chase Lev deques #29; this can improve performance significantly for some workloads.
* A removal of closure allocation in Chan #28.
* A move to using the Mutex & Condition modules for the implementation of Chan #24.
* Various documentation and packaging improvements (#21, #27, #30, #32).

## v0.2.2

Updates to:

* parallel_for to use new task distribution algorithm and allow default chunk_size (#16)
* parallel_for_reduce to use new task distribution algorithm and allow default chunk_size parameter (#18)

## v0.2.1

* `recv_poll` made non-allocating
* Addition of parallel_scan #5

## v0.2.0

* New Tasks library with support for async/await parallelism and parallel for loops.
* Adds support for non-blocking Chan.send_poll and Chan.recv_poll.

Thanks to @gasche for API design discussions.

## v0.1.0

Initial release