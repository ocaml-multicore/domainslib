# Changes

## Next version

- ensure `cleanup` is run in the presence of exceptions in
  - `STM_sequential.agree_prop` and `STM_domain.agree_prop_par`
  - `Lin_thread.lin_prop` and `Lin_effect.lin_prop`

## 0.1.1

- #263: Cleanup resources after each domain-based `Lin` test
- #281: Escape and quote strings printed with `STM`'s `string` combinator

## 0.1

The initial opam release of `qcheck-lin`, `qcheck-stm`, and
`qcheck-multicoretests-util`.

The `multicoretests` package is not released on opam, as it is of
limited use to OCaml developers.
