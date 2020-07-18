defmodule ElixirSense.Plugins.Util do
  @moduledoc false

  alias ElixirSense.Core.Introspection
  alias ElixirSense.Core.Metadata
  alias ElixirSense.Core.Source
  alias ElixirSense.Core.State

  def actual_mod_fun({mod, fun}, elixir_prefix, env, buffer_metadata) do
    %State.Env{imports: imports, aliases: aliases, module: module} = env
    %Metadata{mods_funs_to_positions: mods_funs, types: metadata_types} = buffer_metadata

    Introspection.actual_mod_fun(
      {mod, fun},
      imports,
      if(elixir_prefix, do: [], else: aliases),
      module,
      mods_funs,
      metadata_types
    )
  end

  def partial_func_call(code, env, buffer_metadata) do
    %State.Env{module: module} = env
    func_info = Source.which_func(code, module)

    with %{candidate: {mod, fun}, npar: npar} <- func_info,
         mod_fun <- actual_mod_fun({mod, fun}, func_info.elixir_prefix, env, buffer_metadata),
         {actual_mod, actual_fun, _} <- mod_fun do
      {actual_mod, actual_fun, npar, func_info}
    else
      _ ->
        :none
    end
  end

  def func_call_chain(code, env, buffer_metadata) do
    func_call_chain(code, env, buffer_metadata, [])
  end

  defp func_call_chain(code, env, buffer_metadata, chain) do
    case partial_func_call(code, env, buffer_metadata) do
      :none ->
        Enum.reverse(chain)

      {_mod, _fun, _npar, %{pos: {{line, col}, _}}} = func_call ->
        code_before = Source.text_before(code, line, col)
        func_call_chain(code_before, env, buffer_metadata, [func_call | chain])
    end
  end
end
