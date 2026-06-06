# Elixir 1.19 → 1.20 type-system changes: impact and backport plan

Status: analysis + plan (2026-06-06). Companion to `TYPES.md`, `TYPES_M1..M3`.

This document maps the type-system changes Elixir made between **v1.19.0** and
**v1.20.0**, assesses what ElixirSense's set-theoretic integration
(`lib/elixir_sense/core/elixir_types.ex` + `type_inference*`) already does, and
proposes how to backport / implement the features we are missing — with the
emphasis the request called out: **native guard typing** and **reverse arrows /
occurrence typing in `case` / `cond` / `with` branches**.

The branch was originally written against an Elixir 1.20-*dev* build. Rebasing
onto `master` (which carries 1.20.0 support) surfaced a wave of internal-API
drift; that drift is already fixed (see "Rebase fallout", below) and the whole
suite is green (1485 passed). This document is about the *next* step: using the
features 1.20.0 actually shipped.

---

## 1. What 1.20 shipped (headline features)

From the 1.20 CHANGELOG ("Type system improvements") and the diff of
`lib/elixir/lib/module/types/**` (`+8039 / -3178` lines across 8 files; `descr.ex`
alone `+5324`, plus a new `traverse.ex`):

1. **Type inference of guards.** `is_list/1`, `is_integer/1`, `is_map_key/2`,
   `tuple_size/1 < n`, `not is_map_key/2`, etc. now refine the types of the
   variables they constrain — including negative information
   (`%{..., foo: not_set()}`) and size bounds on tuples.
2. **Whole-body inference.** Argument types are inferred from how the body uses
   them (`data.foo + data.bar` ⇒ `data` is a map with `:foo`/`:bar` of
   `integer() | float()`), and inferred return types flow into callers.
3. **Typing across clauses + occurrence typing** for `case`, `cond`, `with`
   (and multi-clause heads). A later clause knows what earlier clauses already
   matched (`nil -> ...; value -> ...` ⇒ `value` is non-`nil`). This is the
   **reverse-arrow** mechanism.
4. **Domain (non-atom) keys in maps.** `%{123 => "x", 456.0 => :ok}` types as
   `%{integer() => binary(), float() => :ok}`; atom and domain keys can mix.
   (Castagna 2023, "Typing Records, Maps, and Structs".)
5. **Typed `Map` operations.** `Map.put/delete/replace/fetch!/pop!/update!`
   propagate per-key set/not_set/if_set information and emit errors for keys
   proven absent.

### Internal API drift (1.19 → 1.20.0) — verified against the local clone

These are the entry points ElixirSense calls into. All confirmed by diffing
`v1.19.0..v1.20.0`:

| Symbol | 1.19 | 1.20.0 | Note |
|---|---|---|---|
| `Pattern.of_match` | `/5` | `/6` | new `meta` arg (4th) |
| `Pattern.of_head` | `/7` | `/8` | new `previous` arg + split `info` tag; returns a **5-tuple** `{trees, precise?, no_prev_args, previous, ctx}` |
| `Pattern.of_domain` | `(trees, expected, ctx)` | `(trees, stack, ctx)` | takes `stack` not expected |
| `Pattern.of_guard` | `/5` | `/5` | same arity, but `of_guards/4` now *refines* (`{precise?, changed, ctx}`) |
| `Descr.open_map` | `/0 /1 /2` | `/0 /1` | the `(pairs, default)` "default" form removed → domain keys |
| `stack` fields | `…, refine_vars` (dev) | `…, reverse_arrow` | `refine_vars` gone; **`reverse_arrow: nil` added** |
| context var entry | `%{type,name,context,off_traces}` | `+ paths: [], deps: %{}` | needed by `Of`/`of_changed` |
| ExCk chunk tag | `elixir_checker_v3` | `elixir_checker_v8` | `%{exports, mode}` / per-export `:sig` layout stable |
| `Expr.of_expr` | `/5` | `/5` | **unchanged** (our capability probe still valid) |
| `Module.Types.stack/7`, `context/0` | — | — | **unchanged** |

The single most important structural addition is `stack.reverse_arrow`
(`nil | :cache | :use`) + `context.reverse_arrows` — this *is* the
occurrence-typing engine (see §4).

---

## 2. Rebase fallout (already fixed)

Because the dev-era calls were wrapped in `try/rescue`, the drift did **not**
fail loudly — it silently degraded inference to fallbacks while tests stayed
green. Commit `core/types: adapt Module.Types integration to Elixir 1.20 API`
fixes:

- `of_match/5 → /6`, `of_head/7 → /8` (+ new 5-tuple destructure + `init_previous/0`
  + `info` tag), `of_domain` arg order.
- Removed the dead `stack.refine_vars` write (refinement is now unconditional in
  `of_match`).
- Seeded context vars now carry `paths`/`deps` (matches `Module.Types.Of`).
- `ensure_body_var_versions` now versions `_` patterns (1.20 `of_pattern` does
  `Keyword.fetch!(meta, :version)` for underscores).
- `ExCkReader` accepts any `elixir_checker_v*` tag.

**Lesson that shapes the plan:** Module.Types is `@moduledoc false` and its
internals churn *every* release (v3→v8 of the checker in one minor; three
signature changes in the pattern module). Any deep coupling must sit behind a
probed adaptor and be covered by tests that *assert real results* (not
`:error`-tolerant fallbacks that mask drift).

---

## 3. Where ElixirSense stands today

- **Forward pattern typing in `case`** exists: `Clauses.case/4` computes
  `match_context = TypeInference.type_of(scrutinee)` and binds each clause's
  pattern vars from it via `find_typed_vars/3`, then refines with
  `enhance_case_pattern_vars` → `ElixirTypes.of_match`
  (`lib/elixir_sense/core/compiler/clauses.ex:256-297`).
- **Custom guard typing** exists and is independent of Elixir's:
  `TypeInference.Guard.type_information_from_guards/1`
  (`lib/elixir_sense/core/type_inference/guard.ex`, 316 lines), called from
  `compiler.ex:2064` and `clauses.ex:223`. It also understands Erlang/Dialyzer
  guard idioms Elixir's engine does not target.
- **Local signature inference** (`ElixirTypes.infer_local_signature/5`) already
  routes through `of_head`, so it *already* benefits from 1.20 native guard
  refinement and gives results like `helper/0 :: :success | :failure`.
- **ExCk remote signatures** are read for compiled deps (`ExCkReader`).

What is **missing** relative to 1.20:

| 1.20 feature | ElixirSense today | Gap |
|---|---|---|
| Guard inference | custom `guard.ex` (forward only, no negative/size info), not unified with Descr | unify with / augment by `of_guards` |
| Reverse arrows in `case`/`cond`/`with` | forward only (pattern←scrutinee); scrutinee var **not** refined per branch; no cross-clause subtraction | **primary gap** |
| Whole-body arg inference | only via `infer_local_signature` (signature path), not surfaced at cursor binding | wire into binding |
| Domain map keys | `to_shape`/`quoted_to_shape` lose/ignore non-atom keys | extend shape model |
| Typed `Map` ops | rely on hand-written expansions in `binding.ex` | optionally defer to Descr |

---

## 4. Reverse arrows / occurrence typing — the primary ask

### How 1.20 does it (verified in `expr.ex`)

For `case`/`cond`/`with` and `=`/`<-`, the scrutinee is typed twice:

1. **Cache pass:** `of_expr(scrutinee, term(), …, %{stack | reverse_arrow: :cache}, ctx)`
   records the inference "arrows" keyed by the scrutinee variable's `version`
   into `context.reverse_arrows[version]` (`cache_result`/`cache_arrows`,
   `expr.ex:941-966`).
2. **Use pass, per clause:** for clause head type `t`,
   `of_expr(scrutinee, t, …, %{stack | reverse_arrow: :use}, ctx)` looks up the
   cached arrows and *reverse-propagates* `t` back onto the scrutinee variable —
   so inside the branch the scrutinee (and any vars derived from it) are
   narrowed to what that clause actually matched (`expr.ex:385-387`, 432-486).
3. **Cross-clause:** `of_head` threads `previous` (`init_previous/0` →
   `concat_previous/2`) so clause *n* subtracts what clauses `1..n-1` matched;
   redundant/dead clauses fall out (`pattern.ex:183-220`).

### Plan

ElixirSense does not need warnings or dead-clause detection — it needs the
**refined variable types per branch** for completion/hover. Two layers:

- **L1 (cheap, no upstream coupling) — extend the existing forward path.**
  In `Clauses.expand_case/expand_cond/expand_with`, after binding pattern vars,
  also **narrow the scrutinee variable(s)** within the branch:
  - For `case x do pat -> … end`: in each branch set `x`'s inferred type to
    `intersection(type_of(x), type_of(pat))`, and for later clauses subtract the
    union of earlier patterns (`difference`). This reproduces the
    "value is non-`nil`" example without calling Module.Types.
  - Requires knowing the scrutinee's *variable identity*; we already compute
    `match_context` from the scrutinee AST — extend `type_of` to also return the
    `{:variable, name, version}` when the scrutinee is a var, then update
    `State.merge_inferred_types` for that version inside the branch.
  - `cond` clauses: a truthy/falsy split on the tested variable (`@truthy`/`@falsy`
    analog) — narrow to non-`nil`/non-`false` in the matched branch.
  - This is self-contained, testable, and survives Module.Types churn.

- **L2 (precise, opt-in) — borrow the real arrows.** Behind `enabled?()`, run the
  cache/use two-pass via `Module.Types.Expr.of_expr` with
  `%{stack | reverse_arrow: :cache}` then `:use`, seeding `context.reverse_arrows`,
  and read the refined `context.vars[version]` back through `to_shape`. Gate by
  probing `Map.has_key?(Module.Types.stack(...), :reverse_arrow)` so older/newer
  Elixirs degrade to L1. Use L2 to *upgrade* L1's result (intersect), never to
  replace the always-on L1 behavior.

Start with **L1** — it directly delivers the requested case-branch reverse-arrow
behavior with zero coupling, and L2 layers on precision later.

---

## 5. Guard typing

`infer_local_signature` already gets native guard refinement for free (guards
flow into `of_head` → `of_guards`). The remaining work is at **cursor/binding**
time, where the custom `guard.ex` runs:

- **Keep `guard.ex`** as the always-on, version-independent path. It is the only
  thing that handles Dialyzer/Erlang guard idioms and works on partial code, and
  it is decoupled from Module.Types drift.
- **Augment, don't replace.** Add the negative/size facts 1.20 introduced but
  `guard.ex` lacks: `not is_map_key/2` ⇒ key `not_set`; `tuple_size(x) op n` ⇒
  tuple arity bound; `is_map_key/2` ⇒ key present. These are pure extensions to
  `type_information_from_guards/1` and need no upstream calls.
- **Optional L2:** when `enabled?()`, intersect `guard.ex`'s result with the
  refinement `of_head`/`of_guards` produced for the same clause (we already build
  the head context there). Same probe-and-degrade discipline as §4.

---

## 6. Smaller items

- **Domain map keys (§1.4).** Extend the ElixirSense shape model (`to_shape` /
  `quoted_to_shape`, `coerce_var_type`) to carry non-atom key domains rather than
  dropping them. The `Descr.to_quoted` output already encodes them
  (`%{integer() => …}`); today we only translate atom-keyed maps. Medium effort,
  isolated to the shape translation layer.
- **Typed `Map` ops (§1.5).** Low priority for completion. Optionally, when
  `enabled?()`, defer `Map.put/delete/replace/fetch!` result typing to Descr
  instead of the hand-written `binding.ex` expansions; keep the expansions as the
  always-on fallback.
- **Whole-body arg inference (§1.2).** Surface `infer_local_signature` domains at
  the cursor: when completing inside a def whose args lack annotations, use the
  inferred domain to type the parameter variables. Reuses existing machinery.

---

## 7. Recommended sequencing

1. **(done)** Rebase + 1.20.0 API adaptation; suite green.
2. **Harden the adaptor against drift:** add `enabled?()` integration tests that
   assert *real* inferred shapes for guards / case branches / local sigs, so the
   next Elixir bump fails loudly instead of silently degrading. (This rebase only
   passed because such assertions were thin.)
3. **L1 reverse arrows** in `case`/`cond`/`with` (§4) — the headline ask, no
   coupling.
4. **Guard augmentation** (§5) — negative + size facts in `guard.ex`.
5. **L2 precision passes** (reverse-arrow + guard intersection) behind the probe.
6. Domain keys → whole-body arg inference → typed `Map` ops, as appetite allows.

**Architectural recommendation:** stay with the **hybrid adaptor** strategy of
`TYPES.md` (call into Module.Types behind a probed boundary; never fork). The
v3→v8 / triple-signature churn observed in a *single* minor release is decisive
evidence against vendoring or replacing. ElixirSense's always-on, drift-immune
path (custom `guard.ex`, forward pattern typing, hand-written remote expansions,
L1 reverse arrows) must remain the floor; Module.Types is the precision *upgrade*
when present and matching the expected version.
