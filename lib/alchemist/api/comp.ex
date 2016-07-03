defmodule Alchemist.API.Comp do

  @moduledoc false

  def request(args) do
    {{hint, buffer_file, line}, _} =  Code.eval_string(args)
    buffer = File.read!(buffer_file)

    ElixirSense.suggestions(hint, buffer, line) |> Enum.map(&IO.puts/1)
    IO.puts "END-OF-COMP"
  end

end
