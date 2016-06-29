defmodule Alchemist.API.Docl do

  @moduledoc false

  import IEx.Helpers, warn: false

  def request(args) do
    Application.put_env(:iex, :colors, [enabled: true])

    {{expr, buffer_file, line}, _} = Code.eval_string(args)
    buffer = File.read!(buffer_file)

    IO.puts ElixirSense.docs(expr, buffer, line)
    IO.puts "END-OF-DOCL"
  end

end
