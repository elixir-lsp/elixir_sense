defmodule ElixirSense.Core.Ast do
  @moduledoc """
  Abstract Syntax Tree support
  """

  alias ElixirSense.Core.Introspection
  alias ElixirSense.Core.State

  @empty_env_info %{
    requires: [],
    imports: [],
    behaviours: [],
    aliases: [],
    attributes: [],
    mods_funs: [],
    types: [],
    specs: []
  }

  @partials [
    :def,
    :defp,
    :defmodule,
    :defprotocol,
    :defimpl,
    :defstruct,
    :defexception,
    :@,
    :defmacro,
    :defmacrop,
    :defguard,
    :defguardp,
    :defdelegate,
    :defoverridable,
    :fn,
    :__ENV__,
    :__CALLER__,
    :raise,
    :throw,
    :reraise,
    :send,
    :if,
    :unless,
    :with,
    :case,
    :cond,
    :try,
    :for,
    :receive,
    :in
  ]

  @type_kinds [:type, :typep, :opaque]
  @spec_kinds [:spec, :callback, :macrocallback]
  @fun_kinds [:def, :defp, :defmacro, :defmacrop, :defguard, :defguardp]

  @max_expand_count 30_000

  def extract_use_info(use_ast, module, state) do
    current_aliases = State.current_aliases(state)
    env = Map.merge(__ENV__, %{module: module, function: nil, aliases: current_aliases})

    {expanded_ast, _requires} = Macro.prewalk(use_ast, {env, 1}, &do_expand/2)
    {_ast, env_info} = Macro.prewalk(expanded_ast, @empty_env_info, &pre_walk_expanded/2)
    env_info
  catch
    {:expand_error, _} ->
      IO.puts(:stderr, "Info: ignoring recursive macro")
      @empty_env_info
  end

  def expand_partial(ast, env) do
    {expanded_ast, _} = Macro.prewalk(ast, {env, 1}, &do_expand_partial/2)
    expanded_ast
  rescue
    _e -> ast
  catch
    e -> e
  end

  def expand_all(ast, env) do
    try do
      {expanded_ast, _} = Macro.prewalk(ast, {env, 1}, &do_expand_all/2)
      expanded_ast
    rescue
      _e -> ast
    catch
      e -> e
    end
  end

  def set_module_for_env(env, module) do
    Map.put(env, :module, module)
  end

  def add_requires_to_env(env, modules) do
    add_directive_modules_to_env(env, :require, modules)
  end

  def add_imports_to_env(env, modules) do
    add_directive_modules_to_env(env, :import, modules)
  end

  defp add_directive_modules_to_env(env, directive, modules) do
    directive_string =
      modules
      |> Enum.filter(&(&1 != Elixir and match?({:module, _}, Code.ensure_compiled(&1))))
      |> Enum.map(&"#{directive} #{inspect(&1)}")
      |> Enum.join("; ")

    {new_env, _} = Code.eval_string("#{directive_string}; __ENV__", [], env)
    new_env
  end

  defp do_expand_all(ast, acc) do
    do_expand(ast, acc)
  end

  defp do_expand_partial({name, _, _} = ast, acc) when name in @partials do
    {ast, acc}
  end

  defp do_expand_partial(ast, acc) do
    do_expand(ast, acc)
  end

  # TODO should we add imports here as well?

  defp do_expand({:require, _, _} = ast, {env, count}) do
    # TODO is it ok to loose alias_tuples here?
    {modules, _alias_tuples} = extract_directive_modules(:require, ast)
    new_env = add_requires_to_env(env, modules)
    {ast, {new_env, count}}
  end

  defp do_expand(ast, acc) do
    do_expand_with_fixes(ast, acc)
  end

  # Fix inexpansible `use ExUnit.Case`
  defp do_expand_with_fixes({:use, _, [{:__aliases__, _, [:ExUnit, :Case]} | _]}, acc) do
    ast =
      quote do
        import ExUnit.Callbacks
        import ExUnit.Assertions
        import ExUnit.Case
        import ExUnit.DocTest
      end

    {ast, acc}
  end

  defp do_expand_with_fixes(ast, {env, count}) do
    if count > @max_expand_count do
      throw({:expand_error, "Cannot expand recursive macro"})
    end

    try do
      expanded_ast = Macro.expand(ast, env)
      {expanded_ast, {env, count + 1}}
    rescue
      _e ->
        {ast, {env, count + 1}}
    end
  end

  defp pre_walk_expanded({:__block__, _, _} = ast, acc) do
    {ast, acc}
  end

  defp pre_walk_expanded({:require, _, _} = ast, acc) do
    {modules, alias_tuples} = extract_directive_modules(:require, ast)
    {nil, %{acc | requires: acc.requires ++ modules, aliases: acc.aliases ++ alias_tuples}}
  end

  defp pre_walk_expanded({:import, _, _} = ast, acc) do
    {modules, alias_tuples} = extract_directive_modules(:import, ast)
    {nil, %{acc | imports: acc.imports ++ modules, aliases: acc.aliases ++ alias_tuples}}
  end

  defp pre_walk_expanded({:alias, _, ast}, acc) do
    alias_tuples = extract_aliases(ast)
    {nil, %{acc | aliases: acc.aliases ++ alias_tuples}}
  end

  defp pre_walk_expanded(
         {:@, _,
          [
            {kind, _,
             [
               {:when, _,
                [
                  {:"::", _,
                   [
                     {name, _, args},
                     _
                   ]},
                  _
                ]} = spec
             ]}
          ]},
         acc
       )
       when kind in @spec_kinds do
    {nil,
     %{acc | specs: [{name, get_args(args), typespec_to_string(kind, spec), kind} | acc.specs]}}
  end

  defp pre_walk_expanded({:@, _, [{kind, _meta, [{:"::", _, [{name, _, args}, _]} = spec]}]}, acc)
       when kind in @spec_kinds do
    {nil,
     %{acc | specs: [{name, get_args(args), typespec_to_string(kind, spec), kind} | acc.specs]}}
  end

  defp pre_walk_expanded({:@, _, [{kind, _meta, [{:"::", _, [{name, _, args}, _]} = spec]}]}, acc)
       when kind in @type_kinds do
    {nil,
     %{acc | types: [{name, get_args(args), typespec_to_string(kind, spec), kind} | acc.types]}}
  end

  defp pre_walk_expanded({:@, _, [{:behaviour, _, [behaviour]}]}, acc) do
    raise ArgumentError
    # TODO is it needed? no tests cover reach this branch
    {nil, %{acc | behaviours: [behaviour | acc.behaviours]}}
  end

  defp pre_walk_expanded({:@, _, [{attribute, _, _}]}, acc) do
    raise ArgumentError
    # TODO is it needed? no tests cover reach this branch
    {nil, %{acc | attributes: [attribute | acc.attributes]}}
  end

  # Elixir < 1.9
  defp pre_walk_expanded(
         {{:., _, [Module, :put_attribute]}, _, [_module, :behaviour, behaviour | _]},
         acc
       ) do
    {nil, %{acc | behaviours: [behaviour | acc.behaviours]}}
  end

  defp pre_walk_expanded(
         {{:., _, [Module, :put_attribute]}, _, [_module, attribute | _]},
         acc
       ) do
    {nil, %{acc | attributes: [attribute | acc.attributes]}}
  end

  # Elixir >= 1.9
  defp pre_walk_expanded(
         {{:., _, [Module, :__put_attribute__]}, _, [_module, :behaviour, behaviour | _]},
         acc
       ) do
    {nil, %{acc | behaviours: [behaviour | acc.behaviours]}}
  end

  defp pre_walk_expanded(
         {{:., _, [Module, :__put_attribute__]}, _, [_module, attribute | _]},
         acc
       ) do
    {nil, %{acc | attributes: [attribute | acc.attributes]}}
  end

  defp pre_walk_expanded({type, _, [{:when, _, [{name, _, args}, _]} | _]}, acc)
       when type in @fun_kinds do
    {nil, %{acc | mods_funs: [{name, get_args(args), type} | acc.mods_funs]}}
  end

  defp pre_walk_expanded({type, _, [{name, _, args} | _]}, acc)
       when type in @fun_kinds do
    {nil, %{acc | mods_funs: [{name, get_args(args), type} | acc.mods_funs]}}
  end

  defp pre_walk_expanded({{:., _, [:elixir_module, :compile]}, _, [mod | _]} = ast, acc)
       when is_atom(mod) do
    {ast, %{acc | mods_funs: [mod | acc.mods_funs]}}
  end

  defp pre_walk_expanded({_name, _meta, _args}, acc) do
    {nil, acc}
  end

  defp pre_walk_expanded(ast, acc) do
    {ast, acc}
  end

  defp get_args(args) when is_list(args), do: args
  defp get_args(_), do: []

  # credo:disable-for-lines:40
  defp extract_directive_modules(directive, ast) do
    case ast do
      # v1.2 notation
      {^directive, _, [{{:., _, [{:__aliases__, _, prefix_atoms}, :{}]}, _, aliases}]} ->
        list =
          aliases
          |> Enum.map(fn {:__aliases__, _, mods} ->
            Module.concat(prefix_atoms ++ mods)
          end)

        {list, []}

      # with options
      {^directive, _, [module, opts]} when is_atom(module) ->
        alias_tuples =
          case opts |> Keyword.get(:as) do
            nil -> []
            alias -> [{alias, module}]
          end

        {[module], alias_tuples}

      # with options
      {^directive, _, [{:__aliases__, _, module_parts}, _opts]} ->
        {[module_parts |> Module.concat()], []}

      # without options
      {^directive, _, [{:__aliases__, _, module_parts}]} ->
        {[module_parts |> Module.concat()], []}

      # without options
      {^directive, _, [module]} when is_atom(module) ->
        {[module], []}

      {^directive, _, [{{:., _, [prefix, :{}]}, _, suffixes} | _]} when is_list(suffixes) ->
        list = for suffix <- suffixes, do: Module.concat(prefix, suffix)
        {list, []}
    end
  end

  defp extract_aliases([mod, opts]) when is_list(opts) do
    case Keyword.get(opts, :as) do
      nil -> extract_aliases([mod])
      alias -> [{alias, mod}]
    end
  end

  defp extract_aliases([mod]) when is_atom(mod) do
    if Introspection.elixir_module?(mod) do
      alias = Module.split(mod) |> Enum.take(-1) |> Module.concat()
      [{alias, mod}]
    else
      [{mod, mod}]
    end
  end

  defp extract_aliases([{{:., _, [prefix, :{}]}, _, suffixes}]) when is_list(suffixes) do
    for suffix <- suffixes do
      alias = Module.split(suffix) |> Enum.take(-1) |> Module.concat()
      mod = Module.concat(prefix, suffix)
      {alias, mod}
    end
  end

  def typespec_to_string(kind, spec) do
    "@#{kind} #{spec |> Macro.to_string() |> String.replace("()", "")}"
  end
end
