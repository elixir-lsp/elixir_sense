defmodule ElixirSense.Core.SurroundContext do
  @moduledoc false

  def to_binding(item, current_module) do
    try do
      to_binding_impl(item, current_module)
    rescue
      _e in SystemLimitError ->
        nil
    end
  end

  defp to_binding_impl({:alias, charlist}, _current_module) do
    {{:atom, :"Elixir.#{charlist}"}, nil}
  end

  # do not handle any other local_or_var
  defp to_binding_impl({:alias, {:local_or_var, ~c"__MODULE__"}, charlist}, current_module) do
    if current_module != nil do
      {{:atom, :"#{current_module}.#{charlist}"}, nil}
    end
  end

  defp to_binding_impl({:alias, {:local_or_var, _charlist1}, _charlist}, _current_module), do: nil

  # TODO handle this case?
  defp to_binding_impl({:alias, {:module_attribute, _charlist1}, _charlist}, _current_module),
    do: nil

  # this probably only existed on 1.14
  defp to_binding_impl({:alias, {:dot, _, _}, _charlist}, _current_module),
    do: nil

  defp to_binding_impl({:dot, inside_dot, charlist}, current_module) do
    {inside_dot_to_binding(inside_dot, current_module), :"#{charlist}"}
  end

  defp to_binding_impl({:local_or_var, ~c"__MODULE__"}, current_module) do
    if current_module != nil do
      {{:atom, current_module}, nil}
    end
  end

  defp to_binding_impl({:local_or_var, charlist}, _current_module) do
    {:variable, :"#{charlist}", :any}
  end

  defp to_binding_impl({:capture_arg, charlist}, _current_module) do
    {:variable, :"#{charlist}", :any}
  end

  defp to_binding_impl({:local_arity, charlist}, _current_module) do
    {nil, :"#{charlist}"}
  end

  defp to_binding_impl({:local_call, charlist}, _current_module) do
    {nil, :"#{charlist}"}
  end

  defp to_binding_impl({:module_attribute, charlist}, _current_module) do
    {:attribute, :"#{charlist}"}
  end

  defp to_binding_impl({:operator, charlist}, _current_module) do
    {nil, :"#{charlist}"}
  end

  defp to_binding_impl({:sigil, charlist}, _current_module) do
    {nil, :"sigil_#{charlist}"}
  end

  defp to_binding_impl({:struct, charlist}, _current_module) when is_list(charlist) do
    {{:atom, :"Elixir.#{charlist}"}, nil}
  end

  # handles
  # {:alias, inside_alias, charlist}
  # {:local_or_var, charlist}
  # {:module_attribute, charlist}
  # {:dot, inside_dot, charlist}
  defp to_binding_impl({:struct, inside_struct}, current_module) do
    to_binding_impl(inside_struct, current_module)
  end

  defp to_binding_impl({:unquoted_atom, charlist}, _current_module) do
    {{:atom, :"#{charlist}"}, nil}
  end

  defp to_binding_impl({:key, charlist}, _current_module) do
    {{:atom, :"#{charlist}"}, nil}
  end

  defp to_binding_impl({:keyword, charlist}, _current_module) do
    {:keyword, :"#{charlist}"}
  end

  defp inside_dot_to_binding({:alias, inside_charlist}, _current_module)
       when is_list(inside_charlist) do
    {:atom, :"Elixir.#{inside_charlist}"}
  end

  defp inside_dot_to_binding(
         {:alias, {:local_or_var, ~c"__MODULE__"}, inside_charlist},
         current_module
       ) do
    if current_module != nil do
      {:atom, :"#{current_module |> Atom.to_string()}.#{inside_charlist}"}
    end
  end

  # TODO handle {:alias, {:module_attribute, charlist1}, charlist}?
  defp inside_dot_to_binding({:alias, _other, _inside_charlist}, _current_module) do
    nil
  end

  defp inside_dot_to_binding({:dot, inside_dot, inside_charlist}, current_module) do
    {:call, inside_dot_to_binding(inside_dot, current_module), :"#{inside_charlist}", []}
  end

  defp inside_dot_to_binding({:module_attribute, inside_charlist}, _current_module) do
    {:attribute, :"#{inside_charlist}"}
  end

  defp inside_dot_to_binding({:unquoted_atom, inside_charlist}, _current_module) do
    {:atom, :"#{inside_charlist}"}
  end

  defp inside_dot_to_binding({:var, ~c"__MODULE__"}, current_module) do
    if current_module != nil do
      {:atom, current_module}
    end
  end

  defp inside_dot_to_binding({:var, inside_charlist}, _current_module) do
    {:variable, :"#{inside_charlist}", :any}
  end

  defp inside_dot_to_binding(:expr, _current_module) do
    nil
  end
end
