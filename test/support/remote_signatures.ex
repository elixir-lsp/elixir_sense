defmodule ElixirSenseExample.RemoteSignatures do
  def helper() do
    case 1 do
      1 -> :success
      _ -> :failure
    end
  end
end
