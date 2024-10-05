defmodule ElixirSense.Core.Compiler.Rewrite do
  def inline(module, fun, arity) do
    :elixir_rewrite.inline(module, fun, arity)
  end

  def rewrite(context, receiver, dot_meta, right, meta, e_args, s) do
    do_rewrite(context, receiver, dot_meta, right, meta, e_args, s)
  end

  defp do_rewrite(_, :erlang, _, :+, _, [arg], _s) when is_number(arg), do: {:ok, arg}

  defp do_rewrite(_, :erlang, _, :-, _, [arg], _s) when is_number(arg), do: {:ok, -arg}

  defp do_rewrite(:match, receiver, dot_meta, right, meta, e_args, _s) do
    :elixir_rewrite.match_rewrite(receiver, dot_meta, right, meta, e_args)
  end

  if Version.match?(System.version(), "< 1.14.0") do
    defp do_rewrite(:guard, receiver, dot_meta, right, meta, e_args, s) do
      :elixir_rewrite.guard_rewrite(receiver, dot_meta, right, meta, e_args)
    end
  else
    defp do_rewrite(:guard, receiver, dot_meta, right, meta, e_args, _s) do
      # elixir uses guard context for error messages
      :elixir_rewrite.guard_rewrite(receiver, dot_meta, right, meta, e_args, "guard")
    end
  end

  defp do_rewrite(_, receiver, dot_meta, right, meta, e_args, _s) do
    {:ok, :elixir_rewrite.rewrite(receiver, dot_meta, right, meta, e_args)}
  end
end
