Scope

Add a thin adaptor over Module.Types.
Use it in TypeInference for expressions (fallback), and in Compiler for pattern matches (augment).
Keep local calls and remotes as dynamic for M1 (no cache); keep your guard + Dialyzer behaviors intact.
Deliverables

New ElixirSense.Core.ElixirTypes module with a minimal, stable API.
Optional config flag to enable adaptor (:elixir_sense, :use_elixir_types).
Two surgical hooks:
Expression typing fallback in ElixirSense.Core.TypeInference.type_of/2.
Pattern refinement augmentation in ElixirSense.Core.Compiler.do_expand/3 for =.
Narrow, conservative shape conversion from Descr → ElixirSense shapes.
Adaptor Module

File: elixir_sense/lib/elixir_sense/core/elixir_types.ex

Responsibilities:

Availability/version guard.
Stack/context init.
of_expr wrapper and error safety.
Pattern match wrapper for optional variable refinement.
Conservative shape conversion.
Suggested signatures:

available?() :: boolean
Returns true iff Module.Types.Expr is loaded and has of_expr/5.
enabled?() :: boolean
Application.get_env(:elixir_sense, :use_elixir_types, false) and available?()
init_stack(module :: atom | nil, function :: {atom, non_neg_integer} | nil, file :: binary | nil, mode \\ :dynamic)
Returns Module.Types.stack/7 with:
file: file || "nofile"
module: module || ElixirSense.ElixirTypes
function: function || {:__info__, 1}
no_warn_undefined: :all (no warnings in tooling)
cache: nil
local_handler: &__MODULE__.local_handler/4
init_context() :: Module.Types.context()
local_handler(_meta, _fun_arity, _stack, _context) :: false
M1: always false so locals remain dynamic; replaced in M2.
of_expr(ast, module, function, file, mode \\ :dynamic) :: {:ok, descr} | :error
Builds stack/context; calls Module.Types.Expr.of_expr(ast, Module.Types.Descr.term(), ast, stack, context), returns {:ok, descr} or :error.
of_match(pattern_ast, expected_descr, expr_ast, module, function, file, mode \\ :dynamic) :: {:ok, refined_vars :: map()} | :error
Optional. Calls Module.Types.Pattern.of_match/5 to drive variable refinement; extracts context.vars to a simple map of var-version → descr. For M1, you can stub to :error and implement in M1.5 if time.
to_shape(descr) :: shape | nil | :none
Conservative conversion only when specific:
Integer: if Module.Types.Descr.equal?(descr, Module.Types.Descr.integer()) → {:integer, nil} (keep “specific integer” literal handling in TypeInference)
Float: {:float, nil}
Binary: {:binary, nil}
Empty list: {:list, :empty} when equal?(descr, Module.Types.Descr.empty_list())
Non-empty/list of type: if descr matches non_empty_list(T, _tail) or list(T) with clear element T (no dynamic, not top), map to {:list, to_shape(T) || nil}
Tuple: for small, closed tuples with concrete element descrs, map to {:tuple, arity, Enum.map(elems, &to_shape/1)}
Atom: only when atom set is a single atom (no dynamic, no negation) → {:atom, literal_atom}
Map: only when closed map with only atom keys and concrete values, map to {:map, [{atom_key, to_shape(v)}, ...], nil}
Otherwise: return nil (unknown) — never fabricate precision.
merge_shapes(existing :: shape | nil | :none, new :: shape | nil | :none) :: shape | nil | :none
Rules:
If existing == :none, keep :none.
If existing == nil, use new.
If new == nil or new == :none, keep existing.
Prefer more specific:
Prefer literal integer {:integer, value} over {:integer, nil}
For {:list, type}, prefer the one where type is not nil
For tuples, prefer one with more concrete element shapes
For maps, prefer one with more fields
Otherwise, keep existing to avoid surprises.
Integration 1: Expression Typing (TypeInference)

File: elixir_sense/lib/elixir_sense/core/type_inference.ex:230

Change: in the catch-all fallback, if enabled, call adaptor and convert result.

Current:

type_inference.ex:230 def type_of(_, _), do: nil
Replace with:

Call ElixirTypes.enabled?(), and if true:
case ElixirTypes.of_expr(ast, binding.module, binding.function, binding_file) do
Convert via to_shape/1, then return it if not nil.
Else return nil.
Note: you need access to module/function/file here. If not directly available in this function, add an optional third param or thread minimal env through the existing call sites that ask for type_of/2. Minimal M1 alternative: only use adaptor for a few targeted AST clauses (e.g., arithmetic, comparisons, lists, tuples) where you already have ast in scope; otherwise leave fallback as nil.
Concrete suggestion: add a private helper type_of_with_elixir_types(ast, context) and call it:
In number/binary/list/tuple cases, after existing calculation, merge with adaptor result via merge_shapes/2.
In the catch-all clause (230), return adaptor result if any.
Where to pass AST:

In most clauses you already have ast:
tuples: type_inference.ex:146
lists: type_inference.ex:150
maps: type_inference.ex:76
ranges/sigils: those are clear cases.
For local/remote calls, you only have shapes; in M1, do not attempt adaptor typing (no AST), keep current behavior.
Integration 2: Pattern Matches (Compiler)

File: elixir_sense/lib/elixir_sense/core/compiler.ex:130

Your current flow:

Match in guard context: compiler.ex:130-138
General =: compiler.ex:140-151
You compute vars_with_inferred_types = TypeInference.find_typed_vars(...) and merge.
M1 augmentation:

After computing vars_with_inferred_types, if ElixirTypes.enabled?(), optionally call ElixirTypes.of_match(pattern_ast, Module.Types.Descr.term(), full_match_ast, module, function, file):
Extract refined_vars as [{ {var, version}, descr }].
For each, convert descr via to_shape/1 and merge with vars_with_inferred_types using your existing “intersection” shape (ElixirSense.Core.TypeInference.intersect/2) or a small helper that prefers more specific values.
Insert right after TypeInference.find_typed_vars(...):
compiler.ex:133 and compiler.ex:147 are exact spots to add optional merge before State.merge_inferred_types.
Example pseudo-merge:

mt = for {{name, version}, descr} <- refined_vars, do: {{name, version}, ElixirTypes.to_shape(descr)}
merged = combine(map_from(mt), map_from(vars_with_inferred_types)) with “prefer more specific” rule.
Config Flag

Add :use_elixir_types under your app env.
Read via Application.get_env(:elixir_sense, :use_elixir_types, false).
Gate both integrations on ElixirTypes.enabled?().
Optional :use_elixir_types_mode to choose :traversal for larger AST vs :dynamic for focused nodes. Default :dynamic in M1.
Shape Conversion Details

Atoms
Only map when descr is a single atom (no dynamic). Detect via:
case descr[:atom] do {:union, set} when :sets.size(set) == 1 -> {:atom, hd(:sets.to_list(set))} ; _ -> nil end
Numbers/binaries
Compare with constructors via Module.Types.Descr.equal?/2:
integer/0, float/0, binary/0.
Lists
Extract element type with list_hd/1-like logic is internal; in M1: match only when you see list(term) vs non_empty_list(T, Tail) from known constructors you produce (e.g., arithmetic cases).
Tuples
Only closed tuples with concrete, small arities; map each element recursively to shape.
Maps
Only closed maps with atom keys and convertible values; ignore optional/dynamic parts for M1.
Otherwise return nil.
Merging Rules

Use ElixirTypes.merge_shapes/2 as described to keep current results unless adaptor offers strictly-better signal.
For pattern vars, use TypeInference.intersect/2 when both sides are present and compatible; else keep the more specific one (non-nil, non-:none).
Testing

Unit: Adaptor
available?/enabled?, init_stack/init_context, safe of_expr/4.
to_shape/1 for integer/float/binary/empty_list/list(tuple)/closed tuple.
Integration:
Add target tests that assert no regressions when flag disabled.
With flag enabled, assert improved detail for:
1 + 2 returns integer shape merged correctly.
hd([1,2]) returns integer shape (where existing may be nil).
{1, :ok} tuple returns element shapes.
Pattern {:ok, x} = call() refines x to atom or specific shape when available (optional M1.5).
Performance:
Bench a few large ASTs with flag off vs on; default to :dynamic mode on leaf nodes only.
Risks/Limits in M1

No local/remote call typing (locals dynamic, remotes no cache). That’s fine for M1; you still gain precision for literals, tuples, lists, many built-ins.
Shape conversion intentionally conservative; won’t regress behavior by overcommitting.
Internal Module.Types APIs are subject to change; adaptor boundary keeps blast radius low.
Next Steps (M2 preview)

Implement local_handler that reads current module defs (from your compiler state) so local calls can be typed.
Add ExCk BEAM reader cache to type remote calls.
Expand shape conversion to maps and lists with optional/dynamic parts more accurately.
