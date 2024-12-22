defmodule ElixirSense.Core.Compiler.Rewrite do
  def inline(module, fun, arity) do
    :elixir_rewrite.inline(module, fun, arity)
  end

  # TODO this is gone on 1.18

  def rewrite(context, receiver, dot_meta, right, meta, e_args, s) do
    do_rewrite(context, receiver, dot_meta, right, meta, e_args, s)
  end

  defp do_rewrite(_, :erlang, _, :+, _, [arg], _s) when is_number(arg), do: {:ok, arg}

  defp do_rewrite(_, :erlang, _, :-, _, [arg], _s) when is_number(arg), do: {:ok, -arg}

  defp do_rewrite(:match, receiver, dot_meta, right, meta, e_args, s) do
    if function_exported?(:elixir_rewrite, :match_rewrite, 5) do
      :elixir_rewrite.match_rewrite(receiver, dot_meta, right, meta, e_args)
    else
      :elixir_rewrite.match(receiver, dot_meta, right, meta, e_args, s)
    end
  end

  if Version.match?(System.version(), "< 1.14.0") do
    defp do_rewrite(:guard, receiver, dot_meta, right, meta, e_args, s) do
      :elixir_rewrite.guard_rewrite(receiver, dot_meta, right, meta, e_args)
    end
  else
    defp do_rewrite(:guard, receiver, dot_meta, right, meta, e_args, s) do
      if function_exported?(:elixir_rewrite, :match_rewrite, 5) do
        # elixir uses guard context for error messages
        :elixir_rewrite.guard_rewrite(receiver, dot_meta, right, meta, e_args, "guard")
      else
        :elixir_rewrite.guard(receiver, dot_meta, right, meta, e_args, s)
      end
    end
  end

  defp do_rewrite(_, receiver, dot_meta, right, meta, e_args, _s) do
    {:ok, :elixir_rewrite.rewrite(receiver, dot_meta, right, meta, e_args)}
  end
end
