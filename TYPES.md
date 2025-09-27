Context:

ElixirSense has a mini compiler module @/Users/lukaszsamson/elixir_sense/lib/elixir_sense/core/compiler.ex that traverses elixir AST similar to what elixir compiler does but instead of compiling the code it collects metadata and code inteligence. It has a custom type engine (@/Users/lukaszsamson/elixir_sense/lib/elixir_sense/core/type_inference.ex which collects type info and @/Users/lukaszsamson/elixir_sense/lib/elixir_sense/core/binding.ex which evaluates types of expressions). This type engine is not formally verified and mathematically sound. The design was to make it best effort and covering types interesting in computing code completions. It is far from covering the whole spectrum of elixir types and not all operations are fully implemented. Elixir is developing it's own type engine based on set theoretic types (@/Users/lukaszsamson/elixir/lib/elixir/lib/module/types.ex). I want you to analyze how types are used in elixir compiler and propose a plan how to incorporate elixir types in ElixirSense. The viable options (non exhaustive) are 1. calling into elixir Module.Types during extract and evaluation, 2. vendoring elixir type system in ElixirSense, possibly with changes or simplifications 3. replacing the custom type system with elixir one. You should evaluate pros and cons. Note that the custom type engine has support for guards (not yet implemented in elixir) and erlang dialyzer typespecs in evaluation (not planned in elixir)

Here’s a grounded plan to bring Elixir’s set-theoretic types into ElixirSense, with an analysis of how the compiler uses types and the trade-offs of three viable integration strategies.

How Elixir Uses Types

Inference and signatures
elixir_module.erl:199 calls Module.Types.infer/8 to infer function signatures and find unreachable privates; the result is stored in the module map and included in BEAM artifacts (lib/elixir/src/elixir_module.erl:194,223).
elixir_erl.erl packages signatures into debug/docs and into the “ExCk” checker chunk for downstream use (lib/elixir/src/elixir_erl.erl:446,644,649).
Warnings and diagnostics
Module.Types.warnings/6 traverses all defs and records unused clauses and undefined calls, integrated by the parallel checker (lib/elixir/lib/module/parallel_checker.ex:280).
Type operations and entry points
Types are “descr” maps with dynamic/static parts and set-theoretic operations (union, intersection, subtype, disjoint, etc.) in Module.Types.Descr (lib/elixir/lib/module/types/descr.ex).
Expression and pattern typing go through Module.Types.Expr.of_expr/5 and Module.Types.Pattern.of_head/of_match/of_guard (lib/elixir/lib/module/types/expr.ex, lib/elixir/lib/module/types/pattern.ex).
Built-in remote call signatures are embedded in Module.Types.Apply.signature and exported fetches go through Module.ParallelChecker to pick up other modules’ signatures from “ExCk” (lib/elixir/lib/module/types/apply.ex).
This design lets the compiler:

Infer local function signatures and attach them to the BEAM.
Produce precise diagnostics (e.g., unused clauses).
Check remote calls when it can fetch signatures from compiled modules via the checker cache.
In ElixirSense today

The type engine is pragmatic and completion-focused, with custom shapes and evaluation logic:
Core inference and shapes in elixir_sense/lib/elixir_sense/core/type_inference.ex:1 and binding/expression evaluation in elixir_sense/lib/elixir_sense/core/binding.ex:1.
It supports guard refinements and leverages Dialyzer typespecs, which Elixir’s new engine doesn’t target.
It often works on partial, unsaved code with a simulated env and no compile-time cache of remote signatures.
Options for Incorporation

Call into Elixir’s Module.Types during extract/evaluation
Pros
Immediate leverage of precise set-theoretic types (Descr), especially for lists/tuples/maps and arithmetic/comparison operators.
Access to a rich and composable type algebra (union/intersection/subtype) for better narrowing and completions.
No fork to maintain; stays aligned with Elixir’s evolving engine.
Cons

Module.Types.* are internal APIs (@moduledoc false) and can change across Elixir versions.
Remote calls rely on Module.ParallelChecker cache; without it, many remotes degrade to :none or dynamic(). Builtins and inlined ops are fine; user/library MFAs won’t have types unless you provide a cache.
Performance tuning is needed (modes: :dynamic/:infer vs :traversal) for interactive workflows.
Guard-driven refinement in ElixirSense must remain; Elixir’s guard refinements are still incomplete.
Fit

Best near-term ROI if used as a secondary engine: call Module.Types for precision where available, fallback to ElixirSense where not. Keep guard/spec support.
Vendor Elixir’s set-theoretic engine into ElixirSense
Pros
Full control; you can adapt Apply.signature to consult Dialyzer typespecs or your metadata and avoid Module.ParallelChecker.
Stable behavior across user Elixir versions; no runtime coupling.
Cons

High maintenance cost to track upstream semantics and bug fixes; you’d be shipping a fork of a rapidly evolving engine.
You become responsible for correctness of a complex system.
Hard to keep in sync with “ExCk” format changes and compiler-side improvements.
Fit

Viable only if you can commit to long-term maintenance or you freeze to a subset. Otherwise risky.
Replace the custom type system with Elixir’s
Pros
Single, principled model; fewer dueling semantics.
Future-proof as Elixir adds user type signatures and more checks.
Cons

Loss of guard-refinement behavior you already rely on (until Elixir catches up).
Loss of Dialyzer typespec-driven evaluation not in Elixir’s roadmap.
Requires building a remote-signature cache or losing remotes’ precision.
A large migration that could regress completion quality initially.
Fit

Longer-term target once Elixir’s engine supports more of the guard/use-case matrix and once you can source remote signatures reliably.
Recommended Approach: Hybrid, Phased Integration
Goal: Adopt Module.Types for precise, local expression/pattern types while preserving ElixirSense’s strengths (guards, typespecs) and responsiveness. Avoid forking; interop through a compatibility/adaptor layer.

Phase 1 — Adaptor and Coexistence

Add a Types adaptor module in ElixirSense, e.g., ElixirSense.Core.ElixirTypes:
Encapsulate Module.Types calls and keep all interaction in one place.
Build minimal stacks/contexts:
Module.Types.stack(mode, file, module, function, :all, nil, handler) with mode defaulting to :dynamic for expression under cursor and to :traversal for larger traversals.
Module.Types.context/0 to start; pass through and ignore warnings.
Provide a local_handler/finder that uses ElixirSense’s current module definitions so local calls can be typed.
Provide conversion helpers:
to_shape(descr) → ElixirSense shapes for lists/tuples/maps/atoms/ints, enough to influence Binding.expand decisions.
from_shape(shape) → coarse expected types when needed.
Wire points of use:
Expression typing: when computing a result shape in binding, also call Module.Types.Expr.of_expr/5 and combine:
Prefer existing ElixirSense result if it is specific; else, use converted descr shape.
For conflicts, intersect if possible; otherwise pick the most specific non-:none.
Pattern/match typing: use Module.Types.Pattern.of_match to refine variables when matching, then map refinements into ElixirSense.VarInfo types.
Guards: keep ElixirSense’s current guard refinement, optionally intersect with Pattern.of_guard where it helps and is fast.
Remote calls:
Accept that Module.Types may return :none/dynamic for remotes without cache; keep your current remote evaluation via typespecs and handwritten expansions. Use Module.Types primarily for builtins and inlined :erlang/Kernels where it is strong.
Phase 2 — Remote Signatures via ExCk (no forking)

Implement a lightweight signature cache that reads “ExCk” chunks from compiled BEAMs:
:code.which(module) → :beam_lib.chunks(..., ['ExCk']), decode {elixir_checker_v2, contents} and map MFA → signature.
Cache in ETS to avoid repeated IO; refresh on code reload.
Use this cache in ElixirSense adaptor for remote calls:
Before falling back to Dialyzer/typespecs, check the ExCk cache and, if found, convert signatures to Descr and use them to type remote calls.
Note: Module.Types.Apply.export calls Module.ParallelChecker directly; don’t change it. Instead, in your adaptor, short-circuit remote call handling by consulting your cache and returning a descr, then only call Module.Types for local evaluation and operations.
Phase 3 — Broaden coverage and tuning

Performance: default to :traversal for whole-file walks; switch to :dynamic for expression under cursor or small windows. Reuse context across a cursor session to avoid recomputation.
Fallbacks and composition:
Keep using Dialyzer typespecs for third-party libraries when ExCk is absent.
Maintain ElixirSense’s detailed expansions for collections (Map/Enum/Tuple) where those give immediate value; only replace when Module.Types coverage matches or exceeds.
Diagnostics (optional): offer experimental “types diagnostics” using Module.Types.warnings for users who want it, gated behind a config flag.
Phase 4 — Migration path (optional replacement later)

Once Elixir implements more guard semantics and user type signatures, you can gradually shift shape computations to Module.Types first and use ElixirSense only as a last resort.
Keep the adaptor boundary so rolling back is easy and Elixir version quirks are centralized.
Pros/Cons Summary

Option 1: Call Module.Types

Pros: minimal code, best local precision, leverages upstream; Cons: internal API coupling, remote cache gap; Fit: recommended now via adaptor/fallbacks.
Option 2: Vendor

Pros: control and custom remotes; Cons: heavy maintenance, drift risk; Fit: not recommended unless you freeze a subset and accept divergence.
Option 3: Replace

Pros: a single principled engine; Cons: loses guard/spec features now, needs remote cache, large migration; Fit: future milestone once Elixir matures guard/signature support.
Key Implementation Notes

Use precise, narrow integration points:
Expression typing: elixir_sense/lib/elixir_sense/core/binding.ex: expand/2 family.
Pattern/match/guards: elixir_sense/lib/elixir_sense/core/type_inference.ex:1 for variable discovery and intersections.
Plan for Elixir version skew:
Gate adaptor by feature/probe (e.g., presence of Module.Types.Descr.fun_from_inferred_clauses/1).
Keep a no-op adaptor fallback on older Elixir.
Don’t fork Module.Types; instead, encapsulate calls and convert to ElixirSense shapes.
Preserve strengths:
Keep guard refinements and Dialyzer typespec integration; use them to handle remotes when ExCk is missing.
Proposed Milestones

M1: Adaptor scaffolding + local Expr/Pattern hook-in
M2: Shape conversion and hybrid composition with fallbacks
M3: ExCk cache and remote call typing for compiled deps
M4: Performance tuning and guard intersection experiments
M5: Optional diagnostics surface + config
