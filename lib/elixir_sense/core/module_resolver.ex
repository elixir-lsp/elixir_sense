defmodule ElixirSense.Core.ModuleResolver do
  @moduledoc """
  Pure resolution of a module from an AST node or atom against an environment's
  aliases and current `__MODULE__`.

  This consolidates the alias / `__MODULE__` / `Elixir.`-prefixed / dotted-nesting
  resolution logic that previously lived (duplicated) in
  `ElixirSense.Core.ElixirTypes.module_from_ast/2` and
  `ElixirSense.Core.Binding.resolve_type_module/2`.

  ## Scope

  Handled receiver forms:

    * a bare atom module (returned as-is)
    * `{:__aliases__, _, parts}` including `__MODULE__`-prefixed and
      `Elixir`-prefixed parts, expanded against `env.aliases` / `env.module`
    * a bare `{:__MODULE__, _, _}` resolving to `env.module`
    * a dotted nesting `{{:., _, [base, nested]}, _, []}` where `base` itself
      resolves to a module

  Explicitly OUT OF SCOPE (returns `:error`): attribute receivers
  (`{:@, _, [...]}`) and variable receivers (`{var, _, ctx}`). Those require
  shape/binding expansion of stored types, which is the responsibility of the
  caller (e.g. `Binding`), not pure AST→module resolution. Callers that can
  resolve those forms keep doing so and only delegate the pure forms here.

  Alias semantics are NOT reimplemented here: expansion is delegated to
  `ElixirSense.Core.Introspection.expand_alias/2` and
  `ElixirSense.Core.Normalized.Macro.Env.expand_alias/4`.

  ## Alias fallback semantics (empirically pinned to the real compiler)

  This module is the single canonical alias-fallback path. `Binding` previously
  carried a divergent `resolve_same_root_alias`/`resolve_parent_alias` heuristic;
  it has been removed and `Binding` now delegates here. The unified rules below
  were checked against what the real Elixir compiler resolves (compiling tiny
  modules and reading back `__MODULE__`-derived atoms):

    * `alias Foo.Bar` then `Bar.Baz` -> `Foo.Bar.Baz`
      (aliases-of-aliases and aliased submodules; handled by
      `Introspection.expand_alias` / `Macro.Env.expand_alias`).
    * `alias Foo.Bar, as: B` then `B.Baz` -> `Foo.Bar.Baz`.
    * `__MODULE__`-relative submodules (`__MODULE__.Sub`) -> `<module>.Sub`.
    * `alias MyApp.String` then `String` -> `MyApp.String` (an alias wins over a
      same-named top-level module).
    * A nested `defmodule Inner` inside `Outer` records a real alias entry
      `{Inner, Outer.Inner}`, so `Inner` -> `Outer.Inner` via normal alias
      expansion (NOT via any parent/sibling heuristic).

  Crucially, when NO alias entry applies the real compiler does NOT invent a
  parent/sibling module:

    * In module `Sib.A`, an unaliased single `B` -> `Elixir.B`, NOT `Sib.B`.
    * In module `Rooty.Child`, an unaliased `Rooty.Sibling` -> `Rooty.Sibling`
      (kept fully-qualified as written), regardless of whether the first part
      matches the current module's root segment.

  `Macro.Env.expand_alias/4` returns `:error` for both of those, and the real
  compiler then keeps the reference fully-qualified as written. Therefore the
  no-alias fallback here is simply `Module.concat(parts)` for every arity. An
  earlier `resolve_parent_alias` fallback that turned a single unaliased part
  into a parent-sibling module (`B` -> `Sib.B`) was WRONG against the compiler
  and has been removed.
  """

  alias ElixirSense.Core.Introspection
  alias ElixirSense.Core.Normalized.Macro.Env, as: NormalizedMacroEnv

  require Logger

  @typedoc """
  Minimal environment shape required for resolution. Any map (or struct, such as
  `ElixirSense.Core.Binding` or `ElixirSense.Core.State.Env`) exposing
  `:module` (a module atom or `nil`) and `:aliases` (a list of
  `{alias_module, expansion}` tuples) is accepted.
  """
  @type env :: %{optional(:module) => module() | nil, optional(:aliases) => list()}

  @doc """
  Resolves `ast_or_atom` to a module against `env`.

  Returns `{:ok, module}` or `:error`.

  ## Options

  None currently. Present for forward-compatibility and signature stability.
  """
  @spec resolve(term(), env(), keyword()) :: {:ok, module()} | :error
  def resolve(ast_or_atom, env, opts \\ [])

  def resolve(atom, _env, _opts) when is_atom(atom) do
    {:ok, atom}
  end

  def resolve({:__MODULE__, _, _}, env, _opts) do
    case env_module(env) do
      module when is_atom(module) and not is_nil(module) -> {:ok, module}
      _ -> :error
    end
  end

  def resolve({:__aliases__, _, parts}, env, _opts) when is_list(parts) do
    case expand_module_prefix(parts, env) do
      [_ | _] = expanded ->
        try do
          {:ok, resolve_alias(expanded, env)}
        rescue
          ArgumentError -> :error
        end

      _ ->
        :error
    end
  end

  def resolve({{:., _, [base, nested]}, _, []}, env, opts) when is_atom(nested) do
    case resolve(base, env, opts) do
      {:ok, module} when is_atom(module) -> {:ok, Module.concat(module, nested)}
      _ -> :error
    end
  end

  def resolve(_ast, _env, _opts), do: :error

  # `{:__aliases__, _, [{:__MODULE__, _, _} | rest]}` -> prepend current module's
  # parts so downstream `Module.concat`/alias expansion sees a fully-qualified
  # list. Leaves all other lists untouched (incl. `Elixir`-prefixed, which
  # `Module.concat` already handles).
  defp expand_module_prefix([{:__MODULE__, _, _} | rest], env) do
    case env_module(env) do
      module when is_atom(module) and not is_nil(module) ->
        Module.split(module) ++ rest

      _ ->
        rest
    end
  rescue
    _ -> rest
  end

  defp expand_module_prefix(parts, _env), do: parts

  defp resolve_alias(parts, env) when is_list(parts) do
    aliases = env_aliases(env)
    current_module = env_module(env)
    mod = Module.concat(parts)

    case Introspection.expand_alias(mod, aliases) do
      ^mod ->
        # No alias substitution applied. Defer to the real compiler's alias
        # expansion; if THAT also finds nothing, the reference stays exactly as
        # written (`mod`). See the "Alias fallback semantics" moduledoc: the
        # compiler never invents a parent/sibling module for an unaliased part.
        case expand_alias_from_env(current_module, aliases, parts) do
          resolved when is_atom(resolved) and not is_nil(resolved) -> resolved
          _ -> mod
        end

      resolved ->
        resolved
    end
  end

  defp expand_alias_from_env(module, aliases, list)
       when is_atom(module) and is_list(aliases) and is_list(list) do
    env = %Macro.Env{module: module, aliases: aliases}

    case NormalizedMacroEnv.expand_alias(env, [], list, trace: false) do
      {:alias, resolved} when is_atom(resolved) -> resolved
      _ -> nil
    end
  rescue
    e ->
      Logger.debug("expand_alias_from_env failed: #{Exception.format(:error, e, __STACKTRACE__)}")
      nil
  end

  defp expand_alias_from_env(_, _, _), do: nil

  defp env_module(env) when is_map(env), do: Map.get(env, :module)
  defp env_module(_), do: nil

  defp env_aliases(env) when is_map(env) do
    case Map.get(env, :aliases) do
      aliases when is_list(aliases) -> aliases
      _ -> []
    end
  end

  defp env_aliases(_), do: []
end
