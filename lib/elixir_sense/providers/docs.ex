defmodule ElixirSense.Providers.Docs do
  @moduledoc """
  Doc Provider
  """
  alias ElixirSense.Core.Binding
  alias ElixirSense.Core.BuiltinAttributes
  alias ElixirSense.Core.Introspection
  alias ElixirSense.Core.ReservedWords
  alias ElixirSense.Core.State
  alias ElixirSense.Core.State.VarInfo
  alias ElixirSense.Core.SurroundContext

  @spec all(
          any,
          pos_integer,
          pos_integer,
          State.Env.t(),
          State.mods_funs_to_positions_t(),
          State.types_t()
        ) ::
          {actual_mod_fun :: String.t(), docs :: Introspection.docs()} | nil
  def all(
        context,
        line,
        column,
        %State.Env{
          imports: imports,
          requires: requires,
          aliases: aliases,
          module: module,
          scope: scope,
          attributes: attributes,
          vars: vars
        },
        mods_funs,
        metadata_types
      ) do
    binding_env = %Binding{
      attributes: attributes,
      variables: vars,
      current_module: module
    }

    type = SurroundContext.to_binding(context, module)

    case type do
      nil ->
        nil

      {:keyword, keyword} ->
        docs = ReservedWords.docs(keyword)

        {Atom.to_string(keyword),
         %{
           docs: """
           > #{keyword}

           reserved word

           #{docs}
           """
         }}

      {:attribute, attribute} ->
        docs = BuiltinAttributes.docs(attribute) || ""

        {"@" <> Atom.to_string(attribute),
         %{
           docs: """
           > @#{attribute}

           module attribute

           #{docs}
           """
         }}

      {:variable, variable} ->
        var_info =
          vars
          |> Enum.find(fn
            %VarInfo{name: name, positions: positions} ->
              name == variable and {line, column} in positions
          end)

        if var_info != nil do
          {Atom.to_string(variable),
           %{
             docs: """
             > #{variable}

             variable
             """
           }}
        else
          mod_fun_docs(
            type,
            binding_env,
            imports,
            requires,
            aliases,
            module,
            mods_funs,
            metadata_types,
            scope
          )
        end

      _ ->
        mod_fun_docs(
          type,
          binding_env,
          imports,
          requires,
          aliases,
          module,
          mods_funs,
          metadata_types,
          scope
        )
    end
  end

  defp mod_fun_docs(
         {:variable, name} = type,
         binding_env,
         imports,
         requires,
         aliases,
         module,
         mods_funs,
         metadata_types,
         scope
       ) do
    case Binding.expand(binding_env, type) do
      :none ->
        mod_fun_docs(
          {nil, name},
          binding_env,
          imports,
          requires,
          aliases,
          module,
          mods_funs,
          metadata_types,
          scope
        )

      _ ->
        nil
    end
  end

  defp mod_fun_docs(
         {mod, fun},
         binding_env,
         imports,
         requires,
         aliases,
         module,
         mods_funs,
         metadata_types,
         scope
       ) do
    actual =
      {Binding.expand(binding_env, mod), fun}
      |> SurroundContext.expand(aliases)
      |> Introspection.actual_mod_fun(
        imports,
        requires,
        aliases,
        module,
        scope,
        mods_funs,
        metadata_types
      )

    case actual do
      {mod, fun, true, kind} ->
        {mod_fun_to_string({mod, fun}), Introspection.get_all_docs({mod, fun}, kind, scope)}

      _ ->
        nil
    end
  end

  defp mod_fun_to_string({nil, nil}), do: ""

  defp mod_fun_to_string({nil, fun}) do
    Atom.to_string(fun)
  end

  defp mod_fun_to_string({mod, nil}) do
    inspect(mod)
  end

  defp mod_fun_to_string({mod, fun}) do
    inspect(mod) <> "." <> Atom.to_string(fun)
  end
end
