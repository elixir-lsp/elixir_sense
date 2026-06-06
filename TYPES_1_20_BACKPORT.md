# ElixirSense type-system integration — consolidated plan (1.17 → 1.20)

Status: consolidated & verified, 2026-06-06. **Supersedes** the two earlier
drafts and merges them:

- this file's prior 1.19→1.20 draft, and
- `TYPES_1_20_INDEPENDENT_PLAN.md` (the independent multi-version plan).

The independent plan's architecture (a probed, per-version capability layer with
ElixirSense-native occurrence typing as the floor and Elixir 1.20 reverse arrows
as a precision upgrade) is **adopted**. Its version/API matrix was
**independently verified against the local clone** (`/Users/lukaszsamson/elixir`,
tags `v1.17.0`–`v1.20.0`) and is accurate; the verified table is below. The only
additions here are the hard-verified API surface, the corrected details, and the
fact that the 1.20 drift fixes have **already landed** on this branch.

Goal (per request): support Elixir releases **progressively** — deliver
**1.20 + 1.19 first**, then add **1.18**, then **1.17** (each smaller). Because
`Module.Types.*` is `@moduledoc false` and unstable, all coupling sits behind an
abstraction layer dispatched by **capability probing**, not version numbers.

---

## 0. Status on this branch (Elixir 1.20)

**Delivered (always-on, dialyzer/format/test clean on 1.20):**

- 1.20.0 API drift fixed (`of_match/5→/6`, `of_head/7→/8` + `init_previous/0` +
  5-tuple return, `of_domain` arg order, `stack.refine_vars` dropped, context
  vars carry `paths`/`deps`, `_` patterns versioned, `ExCkReader` accepts any
  `elixir_checker_v*` tag).
- **Capability layer + version-dispatched calls** — `ElixirTypes.capabilities/0`
  + `available?/1` probe the `Module.Types` surface via `function_exported?/3`,
  and the adapter dispatches `Pattern.of_match` (`/6` vs `/5`), `of_head`
  (`/8` + 5-tuple vs `/7` + 2-tuple) and `of_domain` (`stack` vs `expected`) on
  the exported arity. So `enabled?/0` being true on 1.19 (it has `Expr.of_expr/5`)
  no longer means the pattern paths raise — they take the 1.19 forms. (The 1.19
  forms run through `apply/3` so Dialyzer's `:unknown` flag stays happy on 1.20;
  they're exercised once the system Elixir is 1.19.)
- **Call-shape preservation** — when native typing returns `nil` for a remote
  (`{:call, …}`) or local (`{:local_call, …}`) call it can't type, the engine
  keeps the legacy shape Binding depends on instead of erasing it.
- **L1 occurrence typing** — `TypeInference.Occurrence`, wired into
  `Compiler.Clauses` for `case`/`cond`/`with`: a variable scrutinee/condition is
  narrowed to the matched pattern's type within each branch. Always-on,
  version-independent.
- **Guard augmentation** — strict equality (`===`/Erlang `=:=`) and `x in
  [literals]` now refine (the latter yields a union); `<=`/`=<` normalized.
- ExCk `:beam_lib` error-handling bugs fixed; dialyxir/erlex bumped for OTP 28.

**Deferred (out of this slice — larger / higher-coupling):**

- **Shape-vocabulary rework (§5)** — `:term` vs `nil`, `:not_set`, non-empty
  list, tuple arity bounds, `{:domain_key, …}`. Prerequisite for the *negative*
  guard facts (`not is_map_key`, size bounds) and for cross-clause subtraction
  (e.g. inferring non-`nil` in a later clause). Cross-cutting (binding.ex,
  completion reducers) and risky against the strict dialyzer flags, so kept
  separate. L1 deliberately does positive narrowing only until this lands.
- **L2 reverse arrows (§4)** — Elixir's 1.20 reverse-arrow machinery does not
  expose *per-branch* refined variable states from `Expr.of_expr` (vars reset
  between clauses); surfacing them would mean re-implementing private
  `Module.Types` orchestration — precisely the fragile coupling that broke
  during the rebase. Pattern-variable precision is already taken from
  `Module.Types` via `enhance_*_pattern_vars` when `enabled?()`. The remaining
  L2 value (cross-clause negative narrowing) is also gated on the shape rework
  above, so both move together as a follow-up.

---

## 1. Verified cross-version API matrix

Hard-verified by diffing the four release tags. **This corrects guesswork** — use
these signatures when building per-version adapters:

| | 1.17.0 | 1.18.0 | 1.19.0 | 1.20.0 |
|---|---|---|---|---|
| `Expr.of_expr` | **/3** `(ast, stack, ctx)` — no expected type | **/3** `(ast, stack, ctx)` — no expected type | **/5** `(ast, expected, expr, stack, ctx)` | **/5** (same) |
| `Module.Types.stack` | **/5** `(file, module, function, no_warn, cache)` — no mode, no handler | **/7** `(mode, file, module, function, no_warn, cache, handler)` | /7 | /7 |
| `Module.Types.context` | /0 | /0 | /0 | /0 |
| Pattern match entry | `of_pattern/3,4` (no `of_match`) | `of_match/7` `(pattern, guards\\[], expected, expr, tag, stack, ctx)` | `of_match/5` `(pattern, expected_fun, expr, stack, ctx)` | `of_match/6` (+ `meta`) |
| `Pattern.of_head` | **/5** `(patterns, guards, meta, stack, ctx)` | **/7** | **/7** | **/8** (+ `previous`, split `info`/`tag`, 5-tuple return) |
| `Pattern.init_previous` | no | no | no | **yes** |
| `stack.reverse_arrow` / `context.reverse_arrows` | no | no | no | **yes** |
| `Apply` module | **no** | yes | yes | yes |
| ExCk chunk tag | `elixir_checker_v1` | `v1` | `v3` | `v8` |
| Descr map ops | basic | `map_put/3`, `map_fetch/2`, `map_fetch_and_put/3`, `map_delete` | + `map_get`, `map_refresh`; `map_fetch_and_put` still present | **domain-aware**: `map_update/5`, `map_get`; `map_fetch`/`map_fetch_and_put`/`map_delete` **removed** |
| domain (non-atom) keys, `not_set()` reverse facts | no | no | partial helpers, not 1.20 semantics | **yes** |

Implications:

- **1.17 and 1.18 have no expected-type expression typing** (`of_expr/3`), so
  bidirectional/expected-directed inference and most pattern refinement that the
  1.19/1.20 path relies on simply do not exist there. Treat 1.17/1.18 as
  literal/container/expression typing + ExCk reads, with refinement supplied by
  ElixirSense-native code.
- **1.17 has no `Apply` and `stack/5`** (no `mode`, no `local_handler`) → no
  local-signature inference machinery. 1.17 is the smallest "expression-only"
  adapter; pattern entry is `of_pattern`, not `of_match`.
- Descr map-op names churn hard (e.g. `map_fetch_and_put/3` exists in 1.18/1.19
  but is **gone** in 1.20; `map_update/5` is 1.20-only). These make excellent
  capability probes (and are why version-number gating is wrong).

---

## 2. Abstraction layer (adopted from the independent plan)

Stable ElixirSense-facing facade; implementation swapped by capability:

```elixir
defmodule ElixirSense.Core.ElixirTypes do
  def capabilities()                                   # %{expr: :v5|:v3, head: :v8|:v7|:v5, reverse_arrow: bool, ...}
  def available?(capability)
  def expr(ast, opts)                                  # -> {:ok, %{descr, shape, vars, caps}} | {:error, reason}
  def pattern_match(pattern, value, opts)
  def head(args, guards, opts)
  def local_signature(module, fun_arity, clauses, opts)
  def descr_to_shape(descr, opts) / shape_to_descr(shape, opts)
end
```

Backends: `ElixirTypes.V20`, `.V19`, `.V18`, `.V17`, `.Noop`. **Dispatch by
probing**, verified to work:

- `function_exported?(Module.Types.Expr, :of_expr, 5)` ⇒ 1.19/1.20 expr API;
  `…, :of_expr, 3)` ⇒ 1.17/1.18.
- `function_exported?(Module.Types.Pattern, :init_previous, 0)` ⇒ 1.20
  previous/reverse-arrow support.
- `Map.has_key?(Module.Types.stack(...), :reverse_arrow)` ⇒ 1.20 reverse arrows.
- `function_exported?(Module.Types.Descr, :map_update, 5)` ⇒ 1.20 domain map ops;
  `…, :map_fetch_and_put, 3)` ⇒ 1.18/1.19 atom-key map ops.
- `function_exported?(Module.Types, :stack, 7)` ⇒ 1.18+ (vs `/5` for 1.17);
  `Code.ensure_loaded?(Module.Types.Apply)` ⇒ 1.18+.

Rules:
- Return structured `{:ok, %{...}} | {:error, reason}` — **no broad
  `rescue _ -> :error`** around a path whose capability was detected. Drift must
  fail a test with an actionable reason, not silently degrade (this is exactly
  how the dev-vs-1.20.0 drift hid until this rebase).
- Probe results cached per-VM; `Noop` backend keeps everything working with the
  custom engine when no usable capability is present.

---

## 3. Occurrence typing (case / cond / with) — the headline feature

Two layers; **L1 is always-on and version-independent**, L2 upgrades precision.

### L1 — ElixirSense-native (all versions, no Module.Types coupling)

New `ElixirSense.Core.TypeInference.Occurrence` with
`case_refinements/cond_refinements/with_refinements`. For `case x do pat -> body`:

- identify the scrutinee variable+version when the scrutinee is a var;
- pattern vars keep the existing forward inferred types (`find_typed_vars/3`);
- inside the branch, narrow the scrutinee var to
  `intersection(original, branch_pattern_type)`;
- track a running union of earlier patterns and, for later branches, apply a
  **conservative difference** (only for cases we can subtract safely): literal
  atoms/booleans, `nil` vs non-`nil`, tagged tuples (`{:ok, _}`/`{:error, _}`),
  struct tags; otherwise do not subtract.

`cond`/`if`: truthy branch narrows the tested var to "not `nil`/`false`"; negated
condition flips; predicate conditions reuse guard inference. `with`: type each
`<-` RHS, bind pattern vars in the success path, carry the union of LHS failures
to `else`.

This delivers the requested `nil -> …; value -> …` ⇒ `value` non-`nil` behavior
on **every** supported Elixir, including 1.17/1.18.

### L2 — Elixir 1.20 reverse arrows (precision upgrade)

When `:reverse_arrow` is present (1.20): type scrutinee with
`%{stack | reverse_arrow: :cache}`, type the branch pattern, re-type with
`:use`, read refined `context.vars[version]` back through `descr_to_shape`,
**intersect into L1**. Thread `Pattern.init_previous/0` for cross-clause
subtraction. L2 must only ever improve L1; if it fails, L1 stands.

---

## 4. Guard typing — keep the floor, augment with 1.20 facts

`TypeInference.Guard` stays always-on (works on partial code; covers
Dialyzer/Erlang idioms outside Elixir's checker goals; immune to drift).
`infer_local_signature` already gets native guard refinement for free via
`of_head` (1.18+). Augment the custom path with the facts 1.20 surfaces that
matter for completion:

- `is_map_key(k, m)` ⇒ positive: `%{..., k: term()}`; negative branch:
  `%{..., k: not_set()}` (and `not is_map_key` ⇒ `not_set`).
- `tuple_size(x) == n` ⇒ exact arity; `<,<=,>,>=` ⇒ arity bound.
- `map_size(x) > 0`, `length(x) > 0` ⇒ non-empty facts.
- `Map.fetch!/map_get` ⇒ key-present + value type.

This needs a slightly richer internal fact format than today's
`%{{var,version} => shape}` — carry `%{positive, negative, descr}` and convert
back at the call boundary until the rest of the compiler consumes it.

---

## 5. Shape / descriptor boundary cleanup (prerequisite for precision)

Before leaning harder on Descr, fix the lossy boundary (from the independent
plan):

- introduce `:term` as **top**, keep `nil` = "unknown / no useful shape", keep
  `:none` = bottom (today `term()`/`dynamic()` all collapse to `nil`, conflating
  top/unknown/no-result);
- keep Binding projection thunks (`{:map_key,…}`, `{:tuple_nth,…}`,
  `{:list_head/tail,…}`) **private** — never let them cross the native boundary;
- standardize shapes: `{:float, nil}`, `:pid/:port/:reference`, `{:fun, arity}`
  → `{:fun, args, return}`, non-empty list, `{:map, fields, :open|:closed|:unknown}`,
  `:not_set` / `{:if_set, shape}`, and **`{:domain_key, key_shape, value_shape}`**
  so 1.20 non-atom keys are preserved (start by not dropping them).

---

## 6. ExCk reader (done; extend coverage)

Generic `elixir_checker_v*` parsing already landed. Validate the
`%{exports: …, mode: …}` / per-export `:sig` payload **per version in tests**
(v1 for 1.17/1.18, v3 for 1.19, v8 for 1.20) since the inner layout is only
"stable enough".

---

## 7. Sequencing

1. **Stabilize 1.19/1.20 adapter** — *partly done* (1.20 drift fixed, ExCk
   generic). Remaining: introduce the capability module + dispatch; replace broad
   silent fallbacks with structured errors on detected capabilities.
2. **Fix the shape boundary** (`:term`/`:not_set`/openness/non-empty; keep
   projections private; descriptor-backed union/intersection/difference helpers).
3. **L1 occurrence typing** — `case` (nil/non-nil, booleans, tagged tuples,
   structs, literal atoms) → `cond` → `with`.
4. **Augment guards** — positive/negative `is_map_key`, tuple-size bounds,
   non-empty facts.
5. **L2 1.20 precision** — reverse-arrow cache/use, previous-clause threading,
   native domain map-op descriptors where conversion preserves facts.
6. **Add 1.18** — `Expr.of_expr/3` adapter; `of_match/7`, `of_head/7` only after
   tests prove the context shape; ExCk v1.
7. **Add 1.17** — minimal `of_expr/3` + `of_pattern/3,4`; `stack/5`; no local
   signature inference; rely on L1 + custom guards for user-visible refinement.

### Recommended first PR
Capability dispatch scaffold + structured (non-silent) results + per-version ExCk
tests + **L1 `case` occurrence typing for nil/non-nil and tagged tuples**. This
is visible value on 1.19/1.20 today and the foundation for everything else,
without deepening coupling.

---

## 8. Why not vendor or replace

The verified churn — checker chunk v1→v3→v8, `of_head/5→/7→/8`,
`of_expr/3→/5`, `stack/5→/7`, Descr map-op functions appearing and vanishing
across **three consecutive minors** — is decisive. Stay hybrid: the always-on,
drift-immune ElixirSense path (custom guards, forward + L1 occurrence typing,
hand-written remote expansions) is the floor; `Module.Types` is the precision
upgrade only when a probed capability is present and matches the expected shape.
See [[elixir-types-internal-api-drift]] in memory.
