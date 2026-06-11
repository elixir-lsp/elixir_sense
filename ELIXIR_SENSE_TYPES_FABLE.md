# ElixirSense types integration — audit tasks (Fable)

## Status after the 2026-06-11 fix pass

CI gates on this branch: `mix test` 1693/1693 ✅ · `mix compile --force
--warnings-as-errors` ✅ · `mix format --check-formatted` ✅ ·
`mix credo --all --strict` exit 0 ✅.

**Done (P0):** #1–#16 all fixed. Notes: #1/#2 via a `precise_pattern_type/1` gate
mirroring the compiler's `precise?`; #3 conservative `combine_intersection` fallback with
kind-disjointness; #10 every coercion `dynamic()`-wrapped, structs/maps open (full
defstruct field-set closed_map when the module is loaded); #14 redesigned during
integration — owner candidate registers FIRST, only the winner creates the table (the
agent's first design could kill the table-owning loser).

**Done (P1):** #17 (`descr_to_string/1`+`/2` → `to_quoted_string(collapse_structs: true)`),
 #18 (decision: compiler `or` dialect everywhere, goldens updated), #19–#24, #25
(`render_hint/3` with `max_length:`, returns `%{label, full}`, smart elision at ` or `
boundaries), #27, #28. #26 done via the #1 precision helper.
Policy refinement vs the original task text for #20: bare `dynamic()` / `dynamic(inner)`
are unwrapped AT THE SHAPE BOUNDARY (shapes feed the Binding algebra, which has no gradual
marker; bare dynamic() = unknown → structural fallback); compiler-fidelity display of
`dynamic(...)` is preserved via `descr_to_string`. Bare `"dynamic()"` is hint noise
(skipped) like `"term()"`.

**Done (P2/P3 partial):** #37 all quick items. New Binding terminals added for the new
shape atoms (`:empty_map`, `:empty_list` → `{:list, :empty}`, `:non_struct_map`,
`{:tuple_open, _}`).

**Deferred (architecture/perf, not regressions):** #29 (pattern-refinement engine still
overrides native results), #30 (apply_infer mirroring), #31–#36 (operator-table routing,
spec table retirement, `:strong` labeling, descr opaque-wrapping, declare_var, shape-tuple
contract), #38–#40 (generated-var filter, O(n²) clause re-inference, sigs-map memoization).
 #41 partially: regression tests added for every fixed item; the golden
descr-vs-to_quoted_string corpus and the coercion round-trip property remain open.

---

Audit of branch `claude/trusting-wu-d1f603` against the Elixir 1.20.1 typesystem
(`~/elixir/lib/elixir/lib/module/types/`, commit `2beac7740`). Findings verified against the
real source (and tags v1.18.4 / v1.19.4 for back-version claims), several reproduced
empirically. Ordered by priority within each section.

Goals audited against:
- displayed types use the same textual representation as compiler warnings (`Descr.to_quoted_string/2`)
- Binding representation/algebra as compatible with the real typesystem as possible
- no reimplementation of typesystem rules; thin, drift-tolerant adaptor over private APIs
- abstraction layer that can be dropped when Elixir ships a public typesystem API

---

## P0 — Soundness bugs (wrong types claimed)

### 1. Cross-clause subtraction over-subtracts (imprecise pattern types)
`lib/elixir_sense/core/compiler/clauses.ex` (`clause_pattern_type` → `{:difference, ...}`),
`binding.ex` `difference/2` / `covers?/2`.
`TypeInference.type_of/2` types `[h|t]` and `[x]` as `{:list, _}` (includes `[]`) and
`<<c, _::binary>>` as `{:binary, nil}` (includes `""`). Subtraction needs an
**under**-approximation of the matched set. Reproduced: with `xs :: list(integer) | nil`,
`case xs do [h|t] -> ...; other -> ...` claims `other :: nil` although `[]` reaches it.
Ground truth: `pattern.ex:212,250` — the compiler only adds a clause to `previous` when
`pattern_precise? and guard_precise?`.
**Fix:** contribute a clause to the subtracted set only when its pattern type is exact
(atom/literal patterns, tagged tuples of exact-or-wildcard elements). Cons patterns may
contribute at most `{:nonempty_list, _}`; fixed-length list and binary patterns contribute
nothing. Mirror the compiler's `precise?` gate.

### 2. Subtraction ignores guards (case else / with-else failure space)
`clauses.ex` — `clause_pattern_type/1` does `strip_guard` then subtracts the full pattern
type; same in `expand_with` failure computation.
`case v do {:ok, x} when x > 10 -> ...; other -> ...` wrongly removes all `{:ok, _}` from
`other`; `with {:ok, x} when x > 0 <- f()` wrongly excludes `{:ok, 0}` from the else space.
Ground truth: `expr.ex:883` — `else_type = if precise?, do: difference(...), else: type`,
and guarded clauses are never `precise?`.
**Fix:** skip the subtraction entirely for clauses with a non-trivial guard.

### 3. `combine_intersection/2` fallback collapses overlapping shapes to `:none`
`binding.ex:2535-2541`. `covers?/2` is subsumption, not overlap; the `true -> :none`
fallback declares any non-subsuming pair disjoint. Reproduced:
`{:nonempty_list, nil} ∩ {:list, {:integer,nil}}` → `:none`;
`{:list, {:atom,:a}} ∩ {:list, {:atom,:b}}` → `:none` (both contain `[]`).
Consequences: guard+pattern intersections bottom out vars; `clause_feasible?` drops live
clauses so `{:case_result, ...}` omits reachable branch types.
**Fix:** fallback must be conservative — return the narrower side or `nil` unless top-level
kinds are provably disjoint; add explicit list∩list / nonempty∩list clauses.

### 4. Flipped `length/1` comparisons don't invert the operator
`lib/elixir_sense/core/type_inference/guard.ex:235-238`. The `[size, length_call]` clause
delegates without inverting: reproduced `5 > length(x)` ⇒ `x :: nonempty_list` (false for
`x = []`). Also `0 < length(x)` (the common spelling) produces no fact since `:<` isn't in
the direct clause's operator list.
**Fix:** map `:<`→`:>`, `:<=`→`:>=` (and inverses) when the size is on the left.

### 5. `++` typed as "always a proper list"
`binding.ex` `:++` clause. Ground truth `apply.ex:221-225`:
`[empty_list, term] -> term`, `[non_empty_list, term] -> non_empty_list(term, term)` —
`[] ++ :a` is `:a`; `[1] ++ 2` is improper. Current code returns `{:list, union}`
unconditionally and `:none` for non-list LHS.
**Fix:** return `{:list, elem}` only when RHS is a known proper list; otherwise `nil`
(or model the `[] -> rhs` union). `--` is correct as-is.

### 6. Out-of-scope pattern vars expand as 0-arity local calls inside thunks
`binding.ex:127-132` — `do_expand({:variable, ...})` falls back to
`{:local_call, name, ...}` when not in `env.vars`. `{:case_result}` / `{:difference}`
thunks embed clause-local vars never in scope at the cursor; a same-named 0-arity function
silently injects its return type into feasibility/subtraction.
**Fix:** restrict the local-call fallback to `version == :any` (Code.Fragment
misclassification), never versioned vars.

### 7. Open tuples become exact-arity tuples (renders `tuple()` as `{}`)
`lib/elixir_sense/core/elixir_types.ex:1610-1614` — `to_shape` strips the trailing
`{:..., [], nil}` marker (`descr.ex:5471-5476`), so `{atom(), ...}` shows as `{atom()}` and
the top type `tuple()` displays as the empty tuple `{}`.
**Fix:** map open tuples to `:tuple` or add an open-tuple shape rendering `{a, ...}`.

### 8. `{:optional, _}` map fields vanish from display
`elixir_types.ex:1653-1654` produces `{:optional, shape}` for `if_set(t)`, but
`type_presentation.ex` has no segment clause for it — falls to the `term()` catch-all
(line 241) and is then dropped by `uninformative_field?` (line 253) inside structs.
Known optional fields disappear.
**Fix:** add `segment({:optional, inner})` → `"if_set(" <> ... <> ")"`; in
`coerce_map_descr` use `Descr.if_set/1` in field position instead of dropping
optionality (`elixir_types.ex:1981`).

### 9. Unconvertible union members silently dropped (displayed type narrower than truth)
`elixir_types.ex:1621-1631, 1649-1650` — negations map to `nil` and nil members are
rejected, so `integer() or not binary()` displays as `integer()`. Types the compiler would
print in negated form (negation score > 8, `descr.ex:822-841`) lose members.
**Fix:** when any union member is unconvertible, degrade the whole union to `nil`/`term()`
rather than dropping the member. Dual: non-fun intersections take the first convertible
branch (`elixir_types.ex:1635-1646`) — displayed type broader than reality; degrade those
to `nil` too (or render the full `and`/`not` form).

### 10. Statically-coerced descrs: no `dynamic()` wrapping, closed maps, partial closed structs
`elixir_types.ex:1899-1996` (`coerce_var_type`, `coerce_map_descr`).
- Best-effort Binding shapes are seeded as **static** descrs; the typesystem treats them as
  certain — matches collapse to `none()`, `filter_clauses_by_args` intersections go falsely
  empty. The compiler wraps inferred values in `dynamic/1`.
- `{:struct, fields, ...}` (partial fields) → `closed_map` asserts all other fields absent;
  intersection with the real struct descr (closed map over the FULL `__info__(:struct)`
  field set) is empty → valid clauses filtered out.
- Plain map shapes → `closed_map` has the same over-precision.
**Fix:** wrap every coerced shape in `Module.Types.Descr.dynamic/1`; build structs as
`open_map([{:__struct__, atom([mod])} | known_fields])` (or expand defstruct fields when
the module is loaded); prefer `open_map` (or `dynamic(closed_map(...))`) for plain maps.

### 11. Unversioned named vars all stamped `version: 0` — variable aliasing
`elixir_types.ex:2251-2257` — `Module.Types` context vars are keyed by version only
(`of.ex` `declare_var`: `Map.put(vars, version, data)`); two distinct unversioned names
collapse into one slot, cross-contaminating refinements.
**Fix:** assign unique counter versions per name (as underscores already do), keeping a
`name => version` map so repeated occurrences share one version.

### 12. ExCk chunk version not pinned — foreign descrs ingested
`lib/elixir_sense/core/exck_reader.ex:185-192` accepts any `elixir_checker_v*` tag;
`parallel_checker.ex:105/428` pins exactly `@elixir_checker_version`
(`:elixir_checker_v8` on 1.20.1). Descr internals changed 1.18→1.20 (DNF lists → BDDs);
a beam from another Elixir yields descrs the running Descr misinterprets — mostly rescued
into **silent wrong types**.
**Fix:** compare the tag to the running runtime's checker version
(`:elixir_erl.checker_version()` or a probed constant); return
`{:error, :version_mismatch}` otherwise.

### 13. Manual BEAM chunk scan uses 2-byte padding; IFF is 4-byte aligned
`exck_reader.ex:143-163` — `scan_chunks` pads `rem(size, 2)`; correct is
`rem(4 - rem(size, 4), 4)`. Chunks with size ≢ 0 (mod 4) but ≡ 0 (mod 2) desync the scan.
This path runs exactly when `:beam_lib` already failed. Fix padding; add a malformed-beam
test.

### 14. ETS table lifecycle races in ExCkReader
`exck_reader.ex:216-261` — named table owned by a transient request process: dies with it
mid-flight; concurrent `ensure_table` race makes the second `:ets.new` raise uncaught
`ArgumentError` out of `lookup_signature`; deletion between `ensure_table` and
lookup/insert raises `badarg`.
**Fix:** own the table from a supervised process, or `:ets.whereis` + try/rescue + single
retry around all table ops.

### 15. Stale `ParallelChecker` cache in pdict — silent native shutdown
`elixir_types.ex:557-575` — `checker_cache` validates shape, not liveness. Dead
checker/ETS ⇒ `Apply.export` → `fetch_export` (`apply.ex:1511`) raises `badarg`, rescued
to `:error`: native typing silently stops for the process. Each fresh process also spawns
its own checker and re-reads all chunks.
**Fix:** check `Process.alive?(pid)` and `:ets.info(table) != :undefined` before reuse;
consider one shared supervised checker.

### 16. `Application.fetch_env/get_env` expansion rescues unknown to `:none`
`binding.ex` (~diff line 213-235): `elem(nil, 1)` raise is rescued to `:none` (bottom),
silently dropped from unions — over-claiming. Rescue to `nil` (unknown).

---

## P1 — Fidelity mismatches vs compiler-warning output

The compiler formats warning types via `Descr.to_quoted_string(type, collapse_structs: true)`
(`helpers.ex:119,195`). Current display path is descr → lossy `to_shape` → custom renderer,
so output is **not** the compiler's text. Decide the dialect explicitly; tasks below assume
the stated goal (compiler fidelity) wins.

### 17. Prefer `Descr.to_quoted_string/2` for descr-backed rendering
`type_presentation.ex:137-151`. Add `ElixirTypes.descr_to_string/1` delegating (probed,
rescued, with shape-render fallback) to `to_quoted_string`. This single change fixes most
of #18–#24 for the native path and shrinks the drift surface.

### 18. Union separator: `|` vs compiler's `or`
`type_presentation.ex:214` joins with `" | "`; compiler prints `a or b` (`descr.ex:774`).
Golden tests (`type_presentation_test.exs:64,75,104`, `type_presentation_golden_test.exs:39`)
codify `|` — the tests currently enforce a typespec dialect against the stated goal.
Decide and document; update goldens accordingly.

### 19. Struct quoted form `{:%, [], [alias, map]}` unhandled — structs become unknown
`elixir_types.ex:1520-1684` — `map_literal_to_quoted` (`descr.ex:4823-4844`) renders
loaded, complete structs as `%Struct{...}`; `quoted_to_shape` has no `{:%, ...}` clause so
every such type converts to `nil`. Biggest single display loss.
**Fix:** map `{:%, _, [mod, {:%{}, _, fields}]}` → `{:struct, field_shapes, {:atom, mod}, nil}`.

### 20. `dynamic()` wrapper hidden
`elixir_types.ex:1529-1534` — `dynamic()` → `nil`, `dynamic(inner)` unwrapped; the compiler
prominently prints it (`descr.ex:2691-2710`). Add a `{:dynamic, shape}` shape + segment
clause, or document the unwrapping policy.

### 21. Missing `to_shape` clauses: `bitstring()`, `non_struct_map()`, `not_set()`
`elixir_types.ex:1681-1682` fallthrough → unknown. Add clauses (`:bitstring` shape exists
in TypePresentation already but is unreachable from the native path; `:not_set` renders).
Also `:bitstring` coercion narrows to `binary()` (`elixir_types.ex:1911`) — probe
`Descr.bitstring/0`.

### 22. `boolean()` decomposed to `false | true`; `empty_map()`/`map()` conflated;
list forms collapsed
- `elixir_types.ex:1564-1565` — keep a `:boolean` shape (compiler prints `boolean()`).
- `elixir_types.ex:1561-1569` — `empty_map()` and `map()` both → `{:map, [], nil}`.
- `elixir_types.ex:1599-1607` — `list/1`, `non_empty_list/1,2` all → `{:list, a}`;
  non-emptiness and improper tails dropped; `{:nonempty_list, _}` segment clause is dead.
- `elixir_types.ex:1713-1714` — open-map `...` marker dropped (open ⇒ looks closed); the
  explicit `not match?({:..., ...}, key)` filter is dead code (marker is a 3-tuple, never a
  2-tuple key).

### 23. Fallback-dialect spellings absent from descr syntax
`type_presentation.ex` renders `number()`, `struct()`, `[a]`/`[a, ...]`/`[]`, integer/
float/string literals (`5`, `5.0`, `"x"`), `(term(), ... -> term())` for `fun(n)` (compiler
prints `none()` args, `descr.ex:2078-2082`). The 1.20.1 descr has no literal number/string
types and no `number()`. Acceptable as custom-engine extra precision, but document the
dialect and keep it out of native-path output.

### 24. Non-identifier atom map keys render invalid text
`type_presentation.ex:249` — `"#{key}: ..."` breaks for `:"foo bar"` / `Elixir.Mod` keys;
compiler uses keyword detection + `literal_to_quoted` (`descr.ex:4862-4875`). Also breaks
the `Code.string_to_quoted` round-trip in `completion_engine.rendered_field_type/1` —
better: expose a quoted-AST variant from TypePresentation instead of parsing rendered text
back.

### 25. No length-limited rendering API for inlay hints
1.20.1 descr printing has no `:limited` option and no union truncation (only width-98
formatting + the >8-negations heuristic). A 100-atom union or 40-field map renders in full.
**Add:** `render_hint(binding, var, max_length: n)` (or `:depth` / `:max_union_members`)
with `...` elision — ElixirSense must own this policy. Return the full text alongside so
the LSP can put it in the hint tooltip.

### 26. `with` without else: failure space is full RHS types
`type_inference.ex` `clause_result_branches({:with, ...})` unions whole RHS types as
failures; compiler subtracts the pattern when precise (`expr.ex:644-665,883`). Sound but
wider than compiler output — tighten using the (fixed, #1/#2) precise-subtraction helper.

### 27. `:andalso`/`:orelse` table entries typed `:boolean`
`true and 5` is `5`; only the left operand must be boolean. Drop the entries or type as the
union of `false`/left and right. Also `numeric_result/3` yields `:number` for known
non-numeric operands where the compiler errors (`apply.ex:106-111` has no such clause) —
prefer `nil` or `:none`.

### 28. Guard map-key facts vs domain-key representation are two spellings
`guard.ex` stores `{"a", nil}` / `{1, :not_set}` raw; `type_inference.ex` and
`binding.ex` `{:map_key, ...}` use `{{:domain, key_type}, value}`. `%{"a" => v}["a"]`
resolves from a literal but not from a guard fact; `covers?`/`same_keys?` see them as
different. Normalize guard facts to the `{:domain, ...}` form.

---

## P2 — Architecture / drift-tolerance

### 29. Remove or quarantine the hand-rolled pattern-refinement engine
`elixir_types.ex:1000-1460` — `apply_pattern_refinements` re-derives var types from raw AST
and `Map.merge`s its output **over** native results (1007-1008), duplicating
`Pattern.of_match` with bugs the real engine lacks (all list elements take the first
element's shape, 1194-1210; unknown binary segments default `{:integer, nil}`, 1245;
struct-pattern module var forced `{:atom, nil}`, 1059-1066). Violates the
no-reimplementation constraint. **Fix:** drop it, or apply it only where
`descr_to_shape` returned `nil`.

### 30. Clause selection reimplements `Apply.apply_infer` — and invents fallbacks
`elixir_types.ex:2404-2484` — per-arg non-empty intersection instead of domain subtyping
(`apply.ex:1687-1693`), and on zero matching clauses falls back to the union of every
clause return (2416) — inventing types for ill-typed calls. Mirror `apply_infer` via
`Descr.args_to_domain/subtype?`, or better build `Descr.fun_from_inferred_clauses/1` and
apply it so selection stays inside the typesystem.

### 31. Route operator typing through the typesystem when native is on
`binding.ex` operator tables (arith/bitwise/div/rem/float-div/comparisons/raising-funs)
duplicate `apply.ex`'s `:erlang` signature table (currently consistent except #5/#6/#27).
When native is enabled, resolve via ExCk/`Apply` signatures for `:erlang`; keep tables only
as the ≤1.17/native-off fallback. Same for `guard.ex` narrowing tables (documented
version-independent — fine, but track drift; #4 is the first instance) and the
`impl_for_shape` protocol-target table in `compiler.ex` (duplicates
`Protocol.__built_in__/0`).

### 32. Retire `spec_ast_to_shape` + the ~90-entry well-known remote-type table
`state.ex` (~500 lines) reparses `@spec` strings via `Code.string_to_quoted` and
hand-converts typespec AST, duplicating `Binding.parse_type`. Entries already diverge
(`bitstring()` → `{:binary, nil}`; `Exception.kind()` omits `{:EXIT, pid}`;
`String.pattern` → nil). Funnel through the existing typespec expansion.

### 33. Don't label `@spec`-derived signatures `:strong`
`state.ex` `build_elixir_types_spec_sig` returns `{:strong, ...}`; in Module.Types
`:strong` means compiler-verified. Untrusted spec domains then prune overloads in
`extract_return_type_from_sig`. Use a distinct tag (or `:infer`) and dynamic-wrap returns.

### 34. Call `Of.declare_var/3` instead of cloning the var-data literal
`elixir_types.ex:1869-1891` hand-builds `%{type, name, context, off_traces, paths, deps}`
(matches 1.20 `of.ex:60-67` today); any future required key ⇒ KeyError ⇒ silent fallback.
`declare_var` is a public def — call it behind a probe.

### 35. Opaque-wrap descrs at the module boundary
Public surface returns raw descrs throughout (`of_expr`, `of_match` var_descrs,
`infer_local_signature`'s `{:infer, domain, clauses}`, `coerce_var_type_public`,
`VarInfo.elixir_types_descr` persisted in State). Callers mostly treat them as opaque, but
persisted raw descrs couple cache validity to descr internals. Wrap as `{:descr, term}` (or
document "never pattern-match inside"); keep `to_shape`/`available?` as the only swap
points for the eventual public Elixir API. Also fix doc examples hardcoding descr internals
(`elixir_types.ex:43-49, 461-477` — `%{bitmap: 4}` etc.).

### 36. `resolve_shape/2` freezes internal shape tuples into the LSP contract
`type_presentation.ex` — returns raw `{:map, ...}`/`{:struct, ...}` tuples to consumers.
ElixirSense-owned so survivable, but document the shape grammar as public or narrow the
entry point to rendered text + small metadata.

### 37. Misc API-drift / dead code
- 1.18 `of_match` tag: pass `{:match, expected_descr}` not the full match AST
  (`elixir_types.ex:261-272`; v1.18.4 `expr.ex:135`).
- `of_head` info: thread the real `{fun, arity}` instead of `{:def, :def, :infer, expected}`
  (`elixir_types.ex:292`; 1.20 `types.ex:332`).
- Remove `maybe_put_remote_sig` — `local_sigs[{:__remote__, arity}]` has no reader and
  plants a wrongly-shaped entry into a structure `Module.Types` reads (`elixir_types.ex:2362-2378`).
- Remove unconsumed capability probes `:domain_map_ops`, `:reverse_arrow`
  (`elixir_types.ex:158-160`).
- `normalize_var_versions` synthesizing a `{var, 1}` alias from the lowest version can
  attach a type to a shadowed binding (`elixir_types.ex:1283-1302`) — have the consumer ask
  for the version it holds.
- `render_descr` gates on `ElixirTypes.available?()` not `enabled?()`
  (`type_presentation.ex:139`).
- `render_var` prefers the structural shape over the native descr whenever structural ≠
  `term()` (`type_presentation.ex:63-74`) — deliberate precision trade-off, but it means
  native output rarely shows; document in the module contract (interacts with #17/#18).

---

## P3 — State hygiene / performance

### 38. `generated_var?` over-filters macro-introduced variables
`state.ex` `add_var_write` drops any var with non-nil hygiene context — correct for
`if`/`&&` temps, but also all vars from user macro expansions (previously recorded), which
then can't be seeded for native typing. Filter only `generated: true` plus known
`:elixir_expand`-style contexts; verify references/hover in macro-generated code didn't
regress.

### 39. Per-clause re-inference is O(n²) and retains full clause ASTs
`state.ex` `add_clause_ast` keeps every clause's expanded body in `ModFunInfo`
(`elixir_types_clauses`), never pruned; `maybe_infer_local_signature` reruns over the
accumulated list after each clause. Defer inference until the def completes; clear clause
ASTs once the sig is computed.

### 40. Memoize `build_local_sigs_map`
`Binding.from_env` rebuilds it on every Binding construction (every completion/hover/inlay
request). Memoize on metadata identity.

### 41. Tests to add
- Golden tests comparing TypePresentation output to actual `to_quoted_string` output for a
  descr corpus (catches #17–#24 and future drift).
- Subtraction soundness: guarded clauses, cons/fixed-list/binary patterns (#1/#2).
- `combine_intersection` overlap matrix (#3).
- ExCk: malformed beam (padding), foreign checker version (#12/#13).
- Property: every shape→descr coercion round-trips to a shape that `covers?` the original.

## Verified correct (no action needed)
Probe-based version dispatch (`of_expr/3 vs /5`, `of_match/6 vs /7`, 1.19 `/5`), 1.20
`of_head/8` destructure + `of_domain` recompute, `local_handler` contract, `stack/7` arity
across 1.18–1.20, all used `Descr` constructors exist on 1.20, `fun/1` gating,
`tuple_nth` bounds, thunk cycle guard (no non-termination found), positive occurrence
narrowing (intersection with guard-stripped pattern is sound on the positive side),
`:not_set` negative facts (sound extra precision; consumers filter it), ExCk structural
parsing of the v8 payload, hover/completion integration (additive, nil-safe).
