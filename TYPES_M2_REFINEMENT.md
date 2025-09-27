Think how to type local calls. ElixirSense is not doing real compilation and AST traversal is best effort with limited code execution during macro expansion. Particulary, dynamic code in module body is not executed, local macros are not executed in most cases, code compilation callbacks are not executed. The type extracted from local function body is then not guaranteed to be correct as the final expanded AST may not be correct. We need to store the infered signature in ModFunInfo. We may need to combine it with Dialyzer typespec stored in SpecInfo

Here’s a concrete M2 plan focused on local-call typing, storing inferred signatures in state, and combining them with Dialyzer specs pragmatically under ElixirSense’s best-effort expansion model.

Goals

Infer best-effort local function signatures from the AST ElixirSense sees during expansion.
Store those signatures in State.ModFunInfo for reuse.
Use them at call sites (via Module.Types’ local_handler) to type local calls.
Combine inferred signatures with Dialyzer specs (from SpecInfo) for user-facing features; keep Module.Types integration clean until we can convert specs to set-theoretic types.
Data Model

Update ElixirSense.Core.State.ModFunInfo:

Add field: elixir_types_sig (default nil)
Format: {:infer, domain :: Module.Types.Descr.t | nil, clauses :: [{[Module.Types.Descr.t], Module.Types.Descr.t}]}.
Example: {:infer, nil, [ {[integer(), integer()], integer()}, {[binary()], integer()} ]}
Optional flag: elixir_types_status :: :ok | :partial | :skipped (to track quality).
Optional field: clauses (default []) to accumulate per-clause AST in this shape:
%{meta: meta, args: [ast], guards: guards_ast_or_nil, body: ast}
Use this only for inference; it won’t be executed.
No change to SpecInfo in M2; we’ll combine at consumption time.

Inference Algorithm

New API in adaptor: ElixirSense.Core.ElixirTypes.infer_local_signature/5

Signature: infer_local_signature(module, {fun, arity}, clauses :: [clause], file, mode \\ :infer) :: {:infer, domain, clauses_types} | :error
For each clause:
Build stack: stack = init_stack(module, {fun, arity}, file, :infer) |> put_local_handler(false) (local handler returns false during inference to avoid recursion).
context = init_context()
Pattern typing: Pattern.of_head(args, guards, expected_types, {:infer, expected_types}, meta, stack, context), where expected_types = List.duplicate(Descr.dynamic(), arity)
Body typing: Expr.of_expr(body, Descr.term(), body, stack, context)
Compute args types for the clause:
If mode != :traversal: Pattern.of_domain(trees, expected_types, context), else use expected_types.
Accumulate {args_types, return_type} per clause.
Domain:
If multiple clauses, compute union-per-position:
Enum.zip_with(clauses_types, fn {args, _} -> args end) |> Enum.zip_with(&Enum.reduce(&1, &Descr.union/2))
If single clause, domain = nil.
Return {:infer, domain, clauses_types}.
New helper to set a custom local_handler:

init_stack/5 overload:
init_stack(module, function, file, mode, local_sigs_map \\ nil)
After calling Module.Types.stack/7, set:
stack = %{stack | local_handler: local_handler_from(local_sigs_map)}
local_handler_from/1 builds fn meta, fun_arity, stack, context -> ... end capturing a map like %{{fun, arity} => {kind, {:infer, domain, clauses}}}.
Returns {kind, info, context} or false.
Compiler Hooks

Capture clauses during def expansion (best-effort AST) and store in ModFunInfo:

File: elixir_sense/lib/elixir_sense/core/compiler.ex in def/defp/defmacro handling:
You already compute args (with defaults expanded), e_guard, and have the expr (body AST).
After expand(expr, state, env_for_expand) and before State.add_func_to_index or immediately after it:
Append this clause to ModFunInfo in State.mods_funs_to_positions[{module, fun, arity}], e.g.:
State.add_clause_ast(state, env, {fun, arity}, %{meta: meta, args: args, guards: e_guard, body: expr})
Add State.add_clause_ast/4 helper to update the corresponding %ModFunInfo{meta: ..., clauses: [...]}.
After a def clause is processed (returning {{name, arity}, state, env}), run inference opportunistically:
ElixirTypes.infer_local_signature(module, {name, arity}, collected_clauses, file)
Store result in ModFunInfo.elixir_types_sig and set elixir_types_status accordingly:
:ok if all clause pieces were valid; :partial if any clause was skipped due to unquotes/macro; :skipped if none was valid.
Add State API helpers:

State.add_clause_ast(state, env, {fun, arity}, clause_map) :: state
State.put_elixir_types_sig(state, {module, fun, arity}, {:infer, domain, clauses_types}, status) :: state
Local Calls Typing

Build a local signatures map on demand for the current module:
ElixirTypes.build_local_sigs_map(metadata, module) :: %{ {fun, arity} => {kind, {:infer, domain, clauses}} }
kind = ModFunInfo.get_def_kind(info) (returns :def or :defp as Module.Types expects)
Skip macros in M2.
Ignore entries with elixir_types_sig == nil.
When calling ElixirTypes.of_expr/… at a call site:
Pass local_sigs_map so stack.local_handler can resolve locals.
For M2, only do this where we call Module.Types ourselves (e.g., in TypeInference.type_of_with_elixir_types/2 for ASTs representing calls). Keep remote calls as dynamic.
Order of use at call sites:
If local_sigs_map has the function: type via Module.Types (local handler).
Else: fallback to current ElixirSense inference.
Combining With Specs

Storage
Keep SpecInfo as-is, alongside ModFunInfo.elixir_types_sig.
Consumption
For completion/signature help, present:
Prefer spec (user-provided) in UI; provide inferred as additional hint.
Where possible, intersect shapes for argument positions (ElixirSense shape system), not Module.Types.Descr, in M2.
Module.Types integration
Do not feed specs into Module.Types in M2 (no conversion to Descr yet).
In the future, add a conversion layer (M3) to map Erlang typespec ASTs to a conservative Descr when feasible.
Best-Effort Guarantees

Mark elixir_types_status = :partial if:
Any clause had unquotes, dynamic guards, or macro calls that failed to expand.
Any clause typing fell back to dynamic due to unknown constructs.
Never throw; on errors, store :skipped and proceed.
Do not execute dynamic module-body code, compile-time callbacks, or local macros; this mirrors current ElixirSense behavior.
Performance and Caching

Infer signatures per def occurrence and update incrementally; keep latest in ModFunInfo.
Only compute local sigs for the current open buffer/module.
Memoize build_local_sigs_map/2 per module per buffer revision.
Use :infer mode for inference (skips remotes); :traversal as fallback if inference gets too costly.
API Changes Summary

ElixirSense.Core.ElixirTypes
infer_local_signature(module, {fun, arity}, clauses, file, mode \\ :infer) :: {:infer, domain, clauses_types} | :error
init_stack(module, function, file, mode, local_sigs_map \\ nil) :: stack
build_local_sigs_map(metadata, module) :: %{ {fun, arity} => {kind, {:infer, domain, clauses}} }
ElixirSense.Core.Compiler.State
add_clause_ast(state, env, {fun, arity}, clause_map) :: state
put_elixir_types_sig(state, {module, fun, arity}, sig, status) :: state
ElixirSense.Core.State.ModFunInfo
Add fields: elixir_types_sig, elixir_types_status, and optional clauses.
Integration Steps

Capture clauses in compiler.ex def/defp path and store them (M2.1).
Implement infer_local_signature/… and call it after updating clauses (M2.2).
Populate ModFunInfo.elixir_types_sig and status (M2.3).
Update ElixirTypes.of_expr/… to accept local_sigs_map and install a closure-based local_handler (M2.4).
At call sites where we use of_expr (TypeInference fallback), pass a local_sigs_map built from current metadata (M2.5).
UI/consumers: when both spec and inferred exist, display both and prefer spec for docs/signature help (M2.6).
Limitations in M2

No recursive local inference; local calls inside inferred bodies resolve to dynamic (handler returns false during inference).
Macros not inferred.
Specs not fed into Module.Types (merge only at UI level).
Milestones

M2.1: Add clause capture + state fields
M2.2: Implement infer_local_signature/… and domain assembly
M2.3: Store sigs in ModFunInfo with status
M2.4: local_handler closure and of_expr hook
M2.5: Pass local sigs at call sites
M2.6: Combine with specs for presentation
If you’d like, I can sketch infer_local_signature/… and the minimal State.add_clause_ast/4 + State.put_elixir_types_sig/4 functions to make this plug-and-play.


ElixirSense.Core.ElixirTypes (elixir_sense/lib/elixir_sense/core/elixir_types.ex)

  @doc """
  Best-effort signature inference for local functions.

  Each `clause` entry should be a map with at least:
    %{meta: meta, args: [ast], guards: guards_ast | nil, body: ast}

  Returns {:infer, domain, clauses} or :error when nothing useful could be inferred.
  """
  def infer_local_signature(module, {fun, arity} = fun_arity, clauses, file, mode \\ :infer)
      when is_atom(module) and is_atom(fun) and is_integer(arity) do
    with true <- enabled?(),
         true <- arity >= 0,
         false <- clauses == [] do
      expected = List.duplicate(Module.Types.Descr.dynamic(), arity)

      stack =
        init_stack(module, fun_arity, file || "nofile", mode)
        |> maybe_disable_local_handler()

      case stack do
        nil ->
          :error

        _stack ->
          context = init_context()
          reduced = do_infer_local_signature(stack, context, clauses, expected)

          case reduced do
            [] ->
              :error

            clause_types ->
              domain = build_domain(clause_types)
              {:infer, domain, clause_types}
          end
      end
    else
      _ -> :error
    end
  end

  defp maybe_disable_local_handler(nil), do: nil

  defp maybe_disable_local_handler(stack) do
    # prevent recursive lookups while we are inferring the function itself
    %{stack | local_handler: fn _, _, _, context -> {:def, :none, context} end}
  end

  defp do_infer_local_signature(stack, context, clauses, expected) do
    Enum.reduce_while(clauses, [], fn clause, acc ->
      %{meta: meta, args: args, guards: guards, body: body} = normalise_clause(clause)

      try do
        {trees, clause_ctx} =
          Module.Types.Pattern.of_head(args, guards || [], expected, {:infer, expected}, meta, stack, context)

        {return_type, clause_ctx} =
          Module.Types.Expr.of_expr(body, Module.Types.Descr.term(), body, stack, clause_ctx)

        arg_types =
          case stack.mode do
            :traversal -> expected
            _ -> Module.Types.Pattern.of_domain(trees, expected, clause_ctx)
          end

        {:cont, [{arg_types, return_type} | acc]}
      rescue
        _ -> {:cont, acc}
      catch
        _ -> {:cont, acc}
      end
    end)
    |> Enum.reverse()
  end

  defp build_domain([]), do: nil
  defp build_domain([_]), do: nil

  defp build_domain(clause_types) do
    clause_types
    |> Enum.map(&elem(&1, 0))
    |> Enum.zip()
    |> Enum.map(fn tuple ->
      tuple
      |> Tuple.to_list()
      |> Enum.reduce(&Module.Types.Descr.union/2)
    end)
  end

  defp normalise_clause(%{meta: meta, args: args, guards: guards, body: body}) do
    %{meta: meta || [], args: args || [], guards: guards, body: body || {:__block__, [], []}}
  end


The helper keeps Module.Types’ local handler inert during inference, walks each clause via Pattern.of_head/… + Expr.of_expr/…, and assembles the clause list / domain. Returning :error lets callers fall back gracefully when bodies cannot be typed.

ElixirSense.Core.Compiler.State (elixir_sense/lib/elixir_sense/core/compiler/state.ex)

  @doc """
  Accumulate the raw clause AST for later signature inference.
  """
  def add_clause_ast(%__MODULE__{} = state, %{module: module}, {fun, arity}, clause)
      when is_atom(module) and is_atom(fun) and is_integer(arity) do
    key = {module, fun, arity}

    update_mod_fun(state, key, fn %ModFunInfo{} = info ->
      clauses = info.elixir_types_clauses || []
      %ModFunInfo{info | elixir_types_clauses: [clause | clauses]}
    end)
  end

  def add_clause_ast(state, _env, _fun_arity, _clause), do: state

  @doc """
  Persist the inferred Module.Types signature (or clear it after failure).
  """
  def put_elixir_types_sig(%__MODULE__{} = state, {module, fun, arity}, sig, status)
      when is_atom(module) and is_atom(fun) and is_integer(arity) do
    key = {module, fun, arity}

    update_mod_fun(state, key, fn %ModFunInfo{} = info ->
      %ModFunInfo{
        info
        | elixir_types_sig: sig,
          elixir_types_status: status,
          elixir_types_clauses: info.elixir_types_clauses || []
      }
    end)
  end

  defp update_mod_fun(state, key, fun) do
    case state.mods_funs_to_positions do
      %{^key => %ModFunInfo{} = info} ->
        updated_info = fun.(info)
        put_in(state.mods_funs_to_positions[key], updated_info)

      _ ->
        state
    end
  end


This assumes ModFunInfo carries three new fields with sensible defaults:

  defstruct ...,
            elixir_types_clauses: [],
            elixir_types_sig: nil,
            elixir_types_status: :skipped


The helper update_mod_fun/3 keeps the state update logic centralised. add_clause_ast/4 simply prepends each clause map for later inference; put_elixir_types_sig/4 writes both the inferred signature and a status marker (:ok | :partial | :skipped) after running infer_local_signature/…
