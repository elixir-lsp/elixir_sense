defmodule ElixirSense.Core.SurroundContext.Toxic do
  @moduledoc false

  # Toxic2 range-based "symbol under cursor" classifier for the NAVIGATION providers
  # (definition / references / implementation / declaration / hover / call_hierarchy).
  #
  # It returns the SAME shape as `Code.Fragment.surround_context/2`
  # (`:none | %{begin: {line, col}, end: {line, col}, context: context}`) so that
  # `ElixirSense.Core.SurroundContext.to_binding/2`, `Metadata.get_cursor_env/3` and the
  # locators themselves stay unchanged - only the call site flips to this module.
  #
  # The classification is derived from toxic2's `range:` node metadata. A handful of shapes
  # carry no range/meta in the AST (bare `:atom` literals, `key:`/keyword keys) or cannot be
  # disambiguated from the tree (cursor left of a meta-less dot operand); for those we fall back
  # to `Code.Fragment.surround_context/2`, which is purely lexical and exactly what it is good at.
  # This keeps the function total and never worse than the previous behavior.
  #
  # NOTE: completion (`Code.Fragment.cursor_context` / `container_cursor_to_quoted`) is out of
  # scope and stays on `Code.Fragment`.

  @spec surround_context(String.t(), {pos_integer, pos_integer}) :: :none | map()
  def surround_context(source, {_line, _column} = position) when is_binary(source) do
    # Stage 0: delegate entirely to Code.Fragment. Classification is added incrementally
    # (one symbol-kind group at a time), each guarded behind the toxic path with a fallback here.
    Code.Fragment.surround_context(source, position)
  end
end
