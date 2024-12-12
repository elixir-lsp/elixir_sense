defmodule ElixirSense.Providers.Completion.Reducers.Params do
  @moduledoc false

  alias ElixirSense.Core.Binding
  alias ElixirSense.Core.Introspection
  alias ElixirSense.Core.Metadata
  alias ElixirSense.Core.Source
  alias ElixirSense.Providers.Utils.Matcher

  @type param_option :: %{
          type: :param_option,
          subtype: :keyword | :atom,
          name: String.t(),
          origin: String.t(),
          type_spec: String.t()
        }

  @doc """
  A reducer that adds suggestions of keyword list options.
  """
  def add_options(hint, env, buffer_metadata, cursor_context, acc) do
    prefix = cursor_context.text_before

    binding_env = Binding.from_env(env, buffer_metadata)

    %Metadata{mods_funs_to_positions: mods_funs, types: metadata_types} = buffer_metadata

    with %{
           candidate: {mod, fun},
           elixir_prefix: elixir_prefix,
           options_so_far: options_so_far,
           cursor_at_option: cursor_at_option,
           npar: npar
         } <-
           Source.which_func(prefix, binding_env),
         {mod, fun, true, :mod_fun} <-
           Introspection.actual_mod_fun(
             {mod, fun},
             env,
             mods_funs,
             metadata_types,
             cursor_context.cursor_position,
             not elixir_prefix
           ) do
      list =
        for opt <-
              ElixirSense.Core.Options.get_param_options(mod, fun, npar + 1, env, buffer_metadata) do
          case opt do
            {name, type} ->
              # match on atom: 
              if Matcher.match?(to_string(name) <> ":", hint) do
                expanded_spec = Introspection.to_string_with_parens(type)

                %{
                  name: name |> Atom.to_string(),
                  origin: "#{inspect(mod)}.#{fun}",
                  type: :param_option,
                  subtype: :keyword,
                  type_spec: expanded_spec
                }
              end

            name ->
              # match on :atom
              if options_so_far == [] and cursor_at_option == true and
                   Matcher.match?(inspect(name), hint) do
                %{
                  name: name |> Atom.to_string(),
                  origin: "#{inspect(mod)}.#{fun}",
                  type: :param_option,
                  subtype: :atom,
                  type_spec: ""
                }
              end
          end
        end
        |> Enum.reject(&is_nil/1)

      {:cont, %{acc | result: acc.result ++ list}}
    else
      _ ->
        {:cont, acc}
    end
  end
end
