defmodule ElixirSense.Providers.Docs do
  @moduledoc """
  Doc Provider
  """
  alias ElixirSense.Core.Binding
  alias ElixirSense.Core.BuiltinAttributes
  alias ElixirSense.Core.Introspection
  alias ElixirSense.Core.Metadata
  alias ElixirSense.Core.ReservedWords
  alias ElixirSense.Core.State
  alias ElixirSense.Core.State.VarInfo
  alias ElixirSense.Core.SurroundContext

  @spec all(
          any,
          State.Env.t(),
          Metadata.t()
        ) ::
          {actual_mod_fun :: String.t(), String.t() | nil} | nil
  def all(
        context,
        %State.Env{
          module: module,
          vars: vars
        } = env,
        metadata
      ) do
    binding_env = Binding.from_env(env, metadata)

    type = SurroundContext.to_binding(context.context, module)

    case type do
      nil ->
        nil

      {:keyword, keyword} ->
        docs = ReservedWords.docs(keyword)

        markdown = """
        > #{keyword}

        reserved word

        #{docs}
        """

        {Atom.to_string(keyword), markdown}

      {:attribute, attribute} ->
        docs = BuiltinAttributes.docs(attribute) || ""

        markdown = """
        > @#{attribute}

        module attribute

        #{docs}
        """

        {"@" <> Atom.to_string(attribute), markdown}

      {:variable, variable} ->
        {line, column} = context.begin

        var_info =
          vars
          |> Enum.find(fn
            %VarInfo{name: name, positions: positions} ->
              name == variable and {line, column} in positions
          end)

        if var_info != nil do
          markdown = """
          > #{variable}

          variable
          """

          {Atom.to_string(variable), markdown}
        else
          mod_fun_docs(
            type,
            context,
            binding_env,
            env,
            metadata
          )
        end

      _ ->
        mod_fun_docs(
          type,
          context,
          binding_env,
          env,
          metadata
        )
    end
  end

  defp mod_fun_docs(
         {mod, fun},
         context,
         binding_env,
         env,
         metadata
       ) do
    actual =
      {Binding.expand(binding_env, mod), fun}
      |> SurroundContext.expand(env.aliases)
      |> Introspection.actual_mod_fun(
        env.imports,
        env.requires,
        env.aliases,
        env.module,
        env.scope,
        metadata.mods_funs_to_positions,
        metadata.types,
        context.begin
      )

    case actual do
      {mod, fun, true, kind} ->
        {line, column} = context.end
        call_arity = Metadata.get_call_arity(metadata, mod, fun, line, column) || :any
        markdown = Introspection.get_all_docs({mod, fun, call_arity}, metadata, env, kind)

        if markdown do
          {mod_fun_to_string({mod, fun}), markdown}
        end

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
