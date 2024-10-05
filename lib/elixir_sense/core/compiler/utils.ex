defmodule ElixirSense.Core.Compiler.Utils do
  def generated([{:generated, true} | _] = meta), do: meta
  def generated(meta), do: [{:generated, true} | meta]

  def split_last([]), do: {[], []}

  def split_last(list), do: split_last(list, [])

  defp split_last([h], acc), do: {Enum.reverse(acc), h}

  defp split_last([h | t], acc), do: split_last(t, [h | acc])

  def split_opts(args) do
    case split_last(args) do
      {outer_cases, outer_opts} when is_list(outer_opts) ->
        case split_last(outer_cases) do
          {inner_cases, inner_opts} when is_list(inner_opts) ->
            {inner_cases, inner_opts ++ outer_opts}

          _ ->
            {outer_cases, outer_opts}
        end

      _ ->
        {args, []}
    end
  end

  def get_line(opts) when is_list(opts) do
    case Keyword.fetch(opts, :line) do
      {:ok, line} when is_integer(line) -> line
      _ -> 0
    end
  end

  def extract_guards({:when, _, [left, right]}), do: {left, extract_or_guards(right)}
  def extract_guards(term), do: {term, []}

  def extract_or_guards({:when, _, [left, right]}), do: [left | extract_or_guards(right)]
  def extract_or_guards(term), do: [term]

  def select_with_cursor(ast_list) do
    Enum.find(ast_list, &has_cursor?/1)
  end

  def has_cursor?(ast) do
    # TODO rewrite to lazy prewalker
    {_, result} =
      Macro.prewalk(ast, false, fn
        _node, true ->
          {nil, true}

        {:__cursor__, _, list}, _state when is_list(list) ->
          {nil, true}

        node, false ->
          {node, false}
      end)

    result
  end

  def defdelegate_each(fun, opts) when is_list(opts) do
    # TODO Remove on elixir v2.0
    append_first? = Keyword.get(opts, :append_first, false)

    {name, args} =
      case fun do
        {:when, _, [_left, right]} ->
          raise ArgumentError,
                "guards are not allowed in defdelegate/2, got: when #{Macro.to_string(right)}"

        _ ->
          case Macro.decompose_call(fun) do
            {_, _} = pair -> pair
            _ -> raise ArgumentError, "invalid syntax in defdelegate #{Macro.to_string(fun)}"
          end
      end

    as = Keyword.get(opts, :as, name)
    as_args = build_as_args(args, append_first?)

    {name, args, as, as_args}
  end

  defp build_as_args(args, append_first?) do
    as_args = :lists.map(&build_as_arg/1, args)

    case append_first? do
      true -> tl(as_args) ++ [hd(as_args)]
      false -> as_args
    end
  end

  # elixir validates arg
  defp build_as_arg({:\\, _, [arg, _default_arg]}), do: arg
  defp build_as_arg(arg), do: arg
end
