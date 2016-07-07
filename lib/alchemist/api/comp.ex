defmodule Alchemist.API.Comp do

  @moduledoc false

  def request(args) do
    {{hint, buffer_file, line}, _} =  Code.eval_string(args)
    buffer = File.read!(buffer_file)

    ElixirSense.suggestions(hint, buffer, line)
    |> Enum.each(&print_suggestion/1)
    IO.puts "END-OF-COMP"
  end

  def print_suggestion(%{type: :callback} = suggestion) do
    %{name: name, arity: arity, args: args, origin: mod_name, summary: desc, spec: spec} = suggestion
    IO.puts "#{name}/#{arity};callback;#{args};#{mod_name};#{desc};#{spec}"
  end

  def print_suggestion(%{type: :return} = suggestion) do
    %{description: description, spec: spec, snippet: snippet} = suggestion
    IO.puts "#{description};return;#{spec};#{snippet}"
  end

  def print_suggestion(%{type: :attribute, name: name}) do
    IO.puts "@#{name};attribute"
  end

  def print_suggestion(%{type: :variable, name: name}) do
    IO.puts "#{name};var"
  end

  def print_suggestion(suggestion) do
    IO.puts suggestion
  end

end
