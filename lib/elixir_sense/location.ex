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

  @spec find_source({module, atom}, atom) :: Location.t() | nil
  def find_source({mod, fun}, current_module) do
    with(
      {mod, file} when file not in ["non_existing", nil, ""] <- find_mod_file(mod),
      nil <- find_fun_position({mod, file}, fun),
      nil <- find_type_position({mod, file}, fun),
      nil <- find_type_position({current_module, file}, fun)
    ) do
      nil
    else
      %Location{} = location ->
        location

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
                 Regex.recompile!(~r/(.+)\/ebin\/([^\s]+)\.beam$/),
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

  defp find_fun_position({mod, file}, fun) do
    {position, category} =
      if String.ends_with?(file, ".erl") do
        # no macros in erlang modules, assume :function when fun != nil
        category = fun_to_type(fun)
        {find_fun_position_in_erl_file(file, fun), category}
      else
        %Metadata{mods_funs_to_positions: mods_funs_to_positions} =
          file_metadata = Parser.parse_file(file, false, false, nil)

        category =
          case mods_funs_to_positions[{mod, fun, nil}] do
            %ModFunInfo{} = mi ->
              ModFunInfo.get_category(mi)

            nil ->
              # not found, fall back to :function when fun != nil
              # TODO use docs?
              fun_to_type(fun)
          end

        {Metadata.get_function_position(file_metadata, mod, fun), category}
      end

    case position do
      {line, column} ->
        %Location{type: category, file: file, line: line, column: column}

      _ ->
        nil
    end
  end

  defp fun_to_type(nil), do: :module
  defp fun_to_type(_), do: :function

  defp find_fun_position_in_erl_file(file, nil) do
    find_line_by_regex(file, Regex.recompile!(~r/^-module/))
  end

  defp find_fun_position_in_erl_file(file, name) do
    escaped =
      name
      |> Atom.to_string()
      |> Regex.escape()

    find_line_by_regex(file, Regex.recompile!(~r/^#{escaped}\b/))
  end

  defp find_type_position_in_erl_file(file, name) do
    escaped =
      name
      |> Atom.to_string()
      |> Regex.escape()

    find_line_by_regex(file, Regex.recompile!(~r/^-(typep?|opaque)\s#{escaped}\b/))
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

  defp find_type_position(_, nil), do: nil

  defp find_type_position({mod, file}, name) do
    position =
      if String.ends_with?(file, ".erl") do
        find_type_position_in_erl_file(file, name)
      else
        file_metadata = Parser.parse_file(file, false, false, nil)
        Metadata.get_type_position(file_metadata, mod, name, file)
      end

    case position do
      {line, column} ->
        %Location{type: :typespec, file: file, line: line, column: column}

      _ ->
        nil
    end
  end
end
