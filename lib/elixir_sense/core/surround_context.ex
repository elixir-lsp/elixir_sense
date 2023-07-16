defmodule ElixirSense.Core.SurroundContext do
  @moduledoc false
  alias ElixirSense.Core.Introspection

  # TODO structs from 1.14
  def to_binding({:alias, charlist}, _current_module) do
    {{:atom, :"Elixir.#{charlist}"}, nil}
  end

  def to_binding({:dot, inside_dot, charlist}, current_module) do
    {inside_dot_to_binding(inside_dot, current_module), :"#{charlist}"}
  end

  def to_binding({:local_or_var, ~c"__MODULE__"}, current_module) do
    if current_module not in [nil, Elixir] do
      {:atom, current_module}
    end
  end

  def to_binding({:local_or_var, charlist}, _current_module) do
    {:variable, :"#{charlist}"}
  end

  def to_binding({:local_arity, charlist}, _current_module) do
    {nil, :"#{charlist}"}
  end

  def to_binding({:local_call, charlist}, _current_module) do
    {nil, :"#{charlist}"}
  end

  def to_binding({:module_attribute, charlist}, _current_module) do
    {:attribute, :"#{charlist}"}
  end

  def to_binding({:operator, charlist}, _current_module) do
    {nil, :"#{charlist}"}
  end

  def to_binding({:unquoted_atom, charlist}, _current_module) do
    {{:atom, :"#{charlist}"}, nil}
  end

  def to_binding({:sigil, charlist}, _current_module) do
    {nil, :"sigil_#{charlist}"}
  end

  def to_binding({:struct, charlist}, _current_module) when is_list(charlist) do
    {{:atom, :"Elixir.#{charlist}"}, nil}
  end

  def to_binding({:keyword, charlist}, _current_module) do
    {:keyword, :"#{charlist}"}
  end

  defp inside_dot_to_binding({:module_attribute, inside_charlist}, _current_module) do
    {:attribute, :"#{inside_charlist}"}
  end

  defp inside_dot_to_binding({:var, inside_charlist}, _current_module) do
    {:variable, :"#{inside_charlist}"}
  end

  defp inside_dot_to_binding({:unquoted_atom, inside_charlist}, _current_module) do
    {:atom, :"#{inside_charlist}"}
  end

  defp inside_dot_to_binding({:alias, inside_charlist}, _current_module)
       when is_list(inside_charlist) do
    {:atom, :"Elixir.#{inside_charlist}"}
  end

  defp inside_dot_to_binding(
         {:alias, {:local_or_var, ~c"__MODULE__"}, inside_charlist},
         current_module
       ) do
    if current_module not in [nil, Elixir] do
      {:atom, :"#{current_module |> Atom.to_string()}.#{inside_charlist}"}
    end
  end

  # TODO attr?
  defp inside_dot_to_binding({:alias, _other, _inside_charlist}, _current_module) do
    nil
  end

  defp inside_dot_to_binding({:dot, inside_dot, inside_charlist}, current_module) do
    {:call, inside_dot_to_binding(inside_dot, current_module), :"#{inside_charlist}", []}
  end

  def expand({{:atom, module}, func}, aliases) do
    {Introspection.expand_alias(module, aliases), func}
  end

  def expand({nil, func}, _aliases) do
    {nil, func}
  end

  def expand({:none, func}, _aliases) do
    {nil, func}
  end
end
