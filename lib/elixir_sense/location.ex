defmodule ElixirSense.Location do
  @moduledoc """
  A location in a source file or buffer
  """

  alias ElixirSense.Core.Metadata
  alias ElixirSense.Core.Parser
  alias ElixirSense.Core.Source
  alias ElixirSense.Core.State.ModFunInfo
  alias ElixirSense.Location

  @type t :: %Location{
          type: :module | :function | :variable | :typespec | :macro | :attribute,
          file: String.t() | nil,
          line: pos_integer,
          column: pos_integer
        }
  defstruct [:type, :file, :line, :column]

  defguardp file_exists(file) when file not in ["non_existing", nil, ""]

  @spec find_mod_fun_source(module, atom, non_neg_integer | nil) :: Location.t() | nil
  def find_mod_fun_source(mod, fun, arity) do
    case find_mod_file(mod) do
      {mod, file} when file_exists(file) ->
        find_fun_position({mod, file}, fun, arity)

      _ ->
        nil
    end
  end

  @spec find_type_source(module, atom, non_neg_integer | nil) :: Location.t() | nil
  def find_type_source(mod, fun, arity) do
    case find_mod_file(mod) do
      {mod, file} when file_exists(file) ->
        find_type_position({mod, file}, fun, arity)

      _ ->
        nil
    end
  end

  defp find_mod_file(Elixir), do: nil

  defp find_mod_file(module) do
    file =
      if Code.ensure_loaded?(module) do
        case module.module_info(:compile)[:source] do
          nil -> nil
          source -> List.to_string(source)
        end
      end

    file =
      if file && File.exists?(file, [:raw]) do
        file
      else
        with {_module, _binary, beam_filename} <- :code.get_object_code(module),
             erl_file =
               beam_filename
               |> to_string
               |> String.replace(
                 ~r/(.+)\/ebin\/([^\s]+)\.beam$/,
                 "\\1/src/\\2.erl"
               ),
             true <- File.exists?(erl_file, [:raw]) do
          erl_file
        else
          _ -> nil
        end
      end

    {module, file}
  end

  defp find_fun_position({mod, file}, fun, arity) do
    result =
      if String.ends_with?(file, ".erl") do
        # erlang function docs point to `-spec` instead of function
        # module docs point to the begin of a file
        # we get better results by regex
        # the downside is we don't handle arity well
        find_fun_position_in_erl_file(file, fun)
      else
        %Metadata{mods_funs_to_positions: mods_funs_to_positions} =
          Parser.parse_file(file, false, false, nil)

        case mods_funs_to_positions[{mod, fun, arity}] do
          %ModFunInfo{} = mi ->
            # assume function head or first clause is last in metadata
            {List.last(mi.positions), ModFunInfo.get_category(mi)}

          nil ->
            # not found in metadata, fall back to docs
            get_function_position_using_docs(mod, fun, arity)
        end
      end

    case result do
      {{line, column}, category} ->
        %Location{type: category, file: file, line: line, column: column}

      _ ->
        nil
    end
  end

  defp find_fun_position_in_erl_file(file, nil) do
    case find_line_by_regex(file, ~r/^-module/) do
      nil -> nil
      position -> {position, :module}
    end
  end

  defp find_fun_position_in_erl_file(file, name) do
    escaped =
      name
      |> Atom.to_string()
      |> Regex.escape()

    case find_line_by_regex(file, ~r/^#{escaped}\b/) do
      nil -> nil
      position -> {position, :function}
    end
  end

  defp find_type_position_in_erl_file(file, name) do
    escaped =
      name
      |> Atom.to_string()
      |> Regex.escape()

    find_line_by_regex(file, ~r/^-(typep?|opaque)\s#{escaped}\b/)
  end

  defp find_line_by_regex(file, regex) do
    index =
      file
      |> File.read!()
      |> Source.split_lines()
      |> Enum.find_index(&String.match?(&1, regex))

    case index do
      nil -> nil
      i -> {i + 1, 1}
    end
  end

  defp find_type_position(_, nil, _), do: nil

  defp find_type_position({mod, file}, name, arity) do
    result =
      if String.ends_with?(file, ".erl") do
        find_type_position_in_erl_file(file, name)
      else
        file_metadata = Parser.parse_file(file, false, false, nil)
        get_type_position(file_metadata, mod, name, arity)
      end

    case result do
      {line, column} ->
        %Location{type: :typespec, file: file, line: line, column: column}

      _ ->
        nil
    end
  end

  defp get_function_position_using_docs(module, nil, _) do
    case Code.fetch_docs(module) do
      nil ->
        nil

      {_, line, _, _, _, _, _} when is_integer(line) ->
        {{line, 1}, :module}

      {_, keyword, _, _, _, _, _} when is_list(keyword) ->
        {{Keyword.get(keyword, :location, 1), 1}, :module}
    end
  end

  defp get_function_position_using_docs(module, function, arity) do
    case Code.fetch_docs(module) do
      nil ->
        nil

      {_, _, _, _, _, _, docs} ->
        docs
        |> Enum.filter(fn
          {{category, ^function, doc_arity}, _line, _, _, meta}
          when category in [:function, :macro] ->
            default_args = Map.get(meta, :defaults, 0)
            arity in (doc_arity - default_args)..doc_arity

          _ ->
            false
        end)
        |> Enum.map(fn
          {{category, _function, _arity}, line, _, _, _} when is_integer(line) ->
            {{line, 1}, category}

          {{category, _function, _arity}, keyword, _, _, _} when is_list(keyword) ->
            {{Keyword.get(keyword, :location, 1), 1}, category}
        end)
        |> Enum.min_by(fn {{line, 1}, _category} -> line end, &<=/2, fn -> nil end)
    end
  end

  def get_type_position(metadata, module, type, arity) do
    case Map.get(metadata.types, {module, type, arity}) do
      nil ->
        get_type_position_using_docs(module, type, arity)

      %ElixirSense.Core.State.TypeInfo{positions: positions} ->
        List.last(positions)
    end
  end

  def get_type_position_using_docs(module, type_name, arity) do
    case Code.fetch_docs(module) do
      nil ->
        nil

      {_, _, _, _, _, _, docs} ->
        docs
        |> Enum.filter(fn
          {{:type, ^type_name, ^arity}, _line, _, _, _meta} ->
            true

          _ ->
            false
        end)
        |> Enum.map(fn
          {{_category, _function, _arity}, line, _, _, _} when is_integer(line) ->
            {line, 1}

          {{_category, _function, _arity}, keyword, _, _, _} when is_list(keyword) ->
            {Keyword.get(keyword, :location, 1), 1}
        end)
        |> Enum.min_by(fn {line, 1} -> line end, &<=/2, fn -> nil end)
    end
  end
end
