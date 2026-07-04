defmodule ElixirSense.Core.ElixirTypesBoundaryTest do
  @moduledoc """
  Architecture boundary check: `ElixirSense.Core.ElixirTypes` is the ONLY
  module allowed to call the compiler's private `Module.Types` API.

  Enforced against compiled BEAM import tables (actual remote-call targets),
  so doc/comment mentions don't count and string-based evasion doesn't help.
  If this test fails, route the new call through an `ElixirTypes` delegate
  (see the "Descr delegation" section there) instead of widening the
  allow-list — keeping the private-API coupling in one file is what makes a
  future switch to a public typesystem API a single-file change.
  """
  use ExUnit.Case, async: true

  @allowed [ElixirSense.Core.ElixirTypes]

  test "only ElixirTypes calls into Module.Types.*" do
    offenders =
      for module <- Application.spec(:elixir_sense, :modules),
          module not in @allowed,
          lib_module?(module),
          target <- module_types_call_targets(module),
          do: {module, target}

    assert offenders == [],
           "Direct Module.Types calls outside the ElixirTypes adaptor:\n" <>
             Enum.map_join(offenders, "\n", fn {m, {tm, tf, ta}} ->
               "  #{inspect(m)} -> #{inspect(tm)}.#{tf}/#{ta}"
             end)
  end

  # Only enforce the boundary for production modules (test/support fixtures
  # such as DescrCompat legitimately poke at Descr).
  defp lib_module?(module) do
    case module.module_info(:compile)[:source] do
      source when is_list(source) -> to_string(source) =~ "/lib/"
      _ -> false
    end
  rescue
    _ -> false
  end

  defp module_types_call_targets(module) do
    with path when is_list(path) <- :code.which(module),
         {:ok, {^module, [{:imports, imports}]}} <- :beam_lib.chunks(path, [:imports]) do
      Enum.filter(imports, fn {target_mod, _f, _a} ->
        target_mod
        |> Atom.to_string()
        |> String.starts_with?("Elixir.Module.Types")
      end)
    else
      _ -> []
    end
  end
end
