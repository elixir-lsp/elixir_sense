defmodule ElixirSense.Core.Ast do
  @moduledoc """
  Abstract Syntax Tree support
  """

  # TODO the code in this module is broken and probably violates GPL license
  # TODO replace

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

  @max_expand_count 30_000

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
      |> Enum.map(&format_module(directive, &1))
      |> Enum.filter(&(&1 != nil))
      |> Enum.join("; ")

    {new_env, _} = Code.eval_string("#{directive_string}; __ENV__", [], env)
    new_env
  end

  defp format_module(_directive, Elixir), do: nil

  defp format_module(directive, module) when is_atom(module) do
    if match?({:module, _}, Code.ensure_compiled(module)) do
      "#{directive} #{inspect(module)}"
    end
  end

  defp format_module(directive, {module, options}) when is_atom(module) do
    if match?({:module, _}, Code.ensure_compiled(module)) do
      formatted_options =
        if options != [] do
          ", " <> Macro.to_string(options)
        else
          ""
        end

      "#{directive} #{inspect(module)}#{formatted_options}"
    end
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

  defp extract_directive_modules(directive, ast) do
    case ast do
      # multi notation
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
end
