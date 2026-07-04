# Set-theoretic types integration — roadmap

Status assessment and forward plan for the `Module.Types` (native set-theoretic
typesystem) integration. Written 2026-07-04, at the point where PR #334 is
feature-complete and green across the Elixir 1.16–1.20 CI matrix.

## Architecture contract

These invariants are the merge-time agreement; the first two are enforced by
CI boundary tests, the rest by review:

1. **Native descriptors are authoritative when available.** Where a descr and
   a shape disagree, the descr wins (trust ranking in `TypeHints`:
   `:native_exck > :native_inferred > :spec > :shape`).
2. **`ElixirTypes` is the only allowed native adaptor.** No module outside it
   may call `Module.Types.*` — enforced by
   `test/elixir_sense/core/elixir_types_boundary_test.exs` against compiled
   BEAM import tables.
3. **`TypeHints` is the only LSP-facing type API.** elixir-ls providers must
   not reach into `Binding` or `TypePresentation` — enforced by
   `inlay_hints_boundary_test.exs` on the elixir-ls side.
4. **Shapes are an editor approximation** for uncompiled/incomplete code and
   pre-1.18 Elixirs — a fallback, not a parallel type system.
5. **Translation is lossy by design and must degrade to weaker hints, never
   stronger claims.** `to_shape/1` returns `nil` rather than guess; the shape
   algebra over-approximates rather than fabricate disjointness (checked by
   the property harness, I1/I2/U1/C1).

## Where we are

The integration deliberately runs **two data models**:

- **Shapes** — Binding's runtime-shape grammar (`{:map, fields, tail}`,
  `{:struct, ...}`, `{:union, ...}`, ...). Works on every supported Elixir
  version, on non-compiling code, and mid-edit. Set operations
  (`combine_intersection`, `normalize_union`, `covers?`) are a hand-rolled
  conservative approximation.
- **Descrs** — the compiler's private `Module.Types.Descr` descriptors,
  available behind version gates (1.18+, full capability on 1.20). Exact, but
  opaque and API-unstable.

Translation lives in `ElixirSense.Core.ElixirTypes` (the single adaptor coupled
to compiler internals): `to_shape/1` (descr → shape, lossy by design) and
`coerce_var_type_public/1` (shape → descr, lossless only for `descr_exact?`
shapes). Binding's intersection/covers dispatchers already try a descr-backed
path first (`with_descr_backing/3`) and fall back to the custom algebra when
the backend is off or an operand is not exact.

Known structural debts:

1. Data flow is descr→shape-first: precise native information is widened into
   shapes at most boundaries, then approximate ops run on the widened form.
2. The custom set algebra (~1.4k lines in `binding.ex`) duplicates what Descr
   answers authoritatively; it is the highest bug-density code in the
   integration.
3. Textual parity between `TypePresentation` and `Descr.to_quoted_string/2`
   output is churn-prone coupling.

## Done (pre-merge gate for PR #334)

- [x] **Descr funnel** — all direct `Module.Types.Descr` call sites outside the
  adaptor now go through `ElixirTypes.descr_dynamic/0`, `descr_dynamic?/1`,
  `descr_union/2`, `descr_intersection/2`, `descr_empty?/1`, `descr_subtype?/2`.
  The adaptor is the only module referencing compiler internals, so a future
  public-API switch (or internals drift) is a single-file change.
- [x] **Property harness for the approximate algebra**
  (`test/elixir_sense/core/binding_descr_property_test.exs`, StreamData,
  `:requires_native_types`-gated). Checks the custom shape algebra against
  Descr as ground truth: no false disjointness (I1), intersection
  over-approximation (I2), union coverage (U1), `covers?` soundness (C1), and
  crash-freedom on the full shape grammar including domain keys, optional
  fields, map tail markers, and improper lists (T1). This validates exactly
  the code paths production uses when native types are unavailable
  (Elixir < 1.18) or shapes are not descr-exact. Tunable via `PROP_MAX_RUNS`.
- [x] **CI boundary checks** — beam-import-table tests enforcing contract
  items 2 and 3 above (one per repo).
- [x] **Short negative-cache TTL in ExCkReader** — failures expire after 1s
  (`:exck_negative_cache_ttl`) so a module compiled seconds after being
  queried isn't hidden for the full 5-minute success TTL.

## Next (post-merge, in priority order)

1. **Descr as first-class carrier in the Binding lattice.** Allow an opaque
   `{:descr, d}` member instead of eagerly `to_shape`-ing at every boundary.
   Ops between two descr-carriers go straight to Descr (exact); mixed ops
   coerce via the existing pair. Shrinks the custom algebra's responsibility
   incrementally — no rewrite. When Elixir ships a public types API, adoption
   is the wrapper + adaptor only.
2. **Grow `descr_exact?` coverage** (closed maps already coerce exactly;
   literal-free structs are candidates) so the descr-backed fast path fires
   more often — every widened case is one the property harness can't fully
   pin.
3. **Stop chasing `to_quoted_string` textual parity** in `TypePresentation`.
   Treat rendering mismatches as acceptable drift, not bugs; keep parity tests
   as non-blocking documentation of divergence.
4. **Expand the property harness to lossy cases.** The current properties pin
   the exact fragment; add explicit tests that lossy translations degrade the
   right way — `to_shape/1` returning `nil` (not a wrong shape) for
   unconvertible descrs, non-exact operands falling to the custom algebra
   rather than a spurious descr verdict, and trust downgrades to `:shape`
   where native info is absent.
5. **Split the big files once behavior is pinned** (no behavior changes,
   separate PRs): `ElixirTypes` into capability probing / descriptor
   conversion+rendering / apply semantics / local signatures / pattern typing;
   the Binding shape algebra into its own module. The property harness and
   boundary tests make these refactors safe to verify.
6. **Deletion map.** When Elixir < 1.18 support is dropped: the shape-only
   fallback paths, the pre-1.18 signature-tuple shims, and much of the custom
   combine algebra become dead. When a public types API arrives: the
   version-gated `of_match`/`apply` shims and `descr_exact?` gating collapse
   into direct API calls. Track these so the ~10k production lines shrink
   instead of ossifying.

## Explicitly out of scope for now

- Splitting the PR (evaluated, declined — reviewed as one unit).
- First-class `{:dynamic, inner}` in the Binding algebra (display-only today;
  see the note in `binding.ex` moduledoc).
