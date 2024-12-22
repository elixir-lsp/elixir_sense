defmodule ElixirSense.Core.Compiler.Bitstring do
  alias ElixirSense.Core.Compiler
  alias ElixirSense.Core.Compiler.Utils
  alias ElixirSense.Core.Compiler.State

  defp expand_match(expr, {s, original_s}, e) do
    {e_expr, se, ee} = Compiler.expand(expr, s, e)
    {e_expr, {se, original_s}, ee}
  end

  def expand(meta, args, s, e, require_size) do
    case Map.get(e, :context) do
      :match ->
        {e_args, alignment, {sa, _}, ea} =
          expand(meta, &expand_match/3, args, [], {s, s}, e, 0, require_size)

        # elixir validates if there is no nested match

        {{:<<>>, [{:alignment, alignment} | meta], e_args}, sa, ea}

      _ ->
        pair_s = {State.prepare_write(s), s}

        {e_args, alignment, {sa, _}, ea} =
          expand(meta, &Compiler.expand_arg/3, args, [], pair_s, e, 0, require_size)

        {{:<<>>, [{:alignment, alignment} | meta], e_args}, State.close_write(sa, s), ea}
    end
  end

  def expand(_bitstr_meta, _fun, [], acc, s, e, alignment, _require_size) do
    {Enum.reverse(acc), alignment, s, e}
  end

  def expand(
        bitstr_meta,
        fun,
        [{:"::", meta, [left, right]} | t],
        acc,
        s,
        e,
        alignment,
        require_size
      ) do
    {e_left, {sl, original_s}, el} = expand_expr(left, fun, s, e)

    match_or_require_size = require_size or is_match_size(t, el)
    e_type = expr_type(e_left)

    expect_size =
      case e_left do
        _ when not match_or_require_size -> :optional
        {:^, _, [{_, _, _}]} -> {:infer, e_left}
        _ -> :required
      end

    {e_right, e_alignment, ss, es} =
      expand_specs(e_type, meta, right, sl, original_s, el, expect_size)

    e_acc = concat_or_prepend_bitstring(meta, e_left, e_right, acc)

    expand(
      bitstr_meta,
      fun,
      t,
      e_acc,
      {ss, original_s},
      es,
      alignment(alignment, e_alignment),
      require_size
    )
  end

  def expand(bitstr_meta, fun, [h | t], acc, s, e, alignment, require_size) do
    meta = extract_meta(h, bitstr_meta)
    {e_left, {ss, original_s}, es} = expand_expr(h, fun, s, e)

    e_type = expr_type(e_left)
    e_right = infer_spec(e_type, meta)

    inferred_meta = [{:inferred_bitstring_spec, true} | meta]

    e_acc =
      concat_or_prepend_bitstring(
        inferred_meta,
        e_left,
        e_right,
        acc
      )

    expand(meta, fun, t, e_acc, {ss, original_s}, es, alignment, require_size)
  end

  defp expand_expr(
         {{:., _, [mod, :to_string]}, _, [arg]} = ast,
         fun,
         s,
         %{context: context} = e
       )
       when context != nil and (mod == Kernel or mod == String.Chars) do
    case fun.(arg, s, e) do
      {ebin, se, ee} when is_binary(ebin) -> {ebin, se, ee}
      _ -> fun.(ast, s, e)
    end
  end

  defp expand_expr(component, fun, s, e), do:
    fun.(component, s, e)

  defp expand_specs(expr_type, meta, info, s, original_s, e, expect_size) do
    default =
      %{size: :default, unit: :default, sign: :default, type: :default, endianness: :default}

    {specs, ss, es} =
      expand_each_spec(meta, unpack_specs(info, []), default, s, original_s, e)

    merged_type = type(expr_type, specs.type)

    # elixir validates if unsized binary is not on the end

    size_and_unit = size_and_unit(expr_type, specs.size, specs.unit)
    alignment = compute_alignment(merged_type, specs.size, specs.unit)

    maybe_inferred_size =
      case {expect_size, merged_type, size_and_unit} do
        {{:infer, pinned_var}, :binary, []} ->
          [{:size, meta, [{{:., meta, [:erlang, :byte_size]}, meta, [pinned_var]}]}]

        {{:infer, pinned_var}, :bitstring, []} ->
          [{:size, meta, [{{:., meta, [:erlang, :bit_size]}, meta, [pinned_var]}]}]

        _ ->
          size_and_unit
      end

    [h | t] =
      build_spec(
        specs.size,
        specs.unit,
        merged_type,
        specs.endianness,
        specs.sign,
        maybe_inferred_size
      )

    {Enum.reduce(t, h, fn i, acc -> {:-, meta, [acc, i]} end), alignment, ss, es}
  end

  defp type(:default, :default), do: :integer
  defp type(expr_type, :default), do: expr_type

  defp type(:binary, type) when type in [:binary, :bitstring, :utf8, :utf16, :utf32],
    do: type

  defp type(:bitstring, type) when type in [:binary, :bitstring], do: type

  defp type(:integer, type) when type in [:integer, :float, :utf8, :utf16, :utf32],
    do: type

  defp type(:float, :float), do: :float
  defp type(:default, type), do: type

  defp type(_other, _type) do
    # elixir raises here bittype_mismatch
    type(:default, :default)
  end

  defp expand_each_spec(meta, [{:__cursor__, _, args} = h | t], map, s, original_s, e)
       when is_list(args) do
    {h, s, e} = Compiler.expand(h, s, e)

    args =
      case h do
        nil -> t
        h -> [h | t]
      end

    expand_each_spec(meta, args, map, s, original_s, e)
  end

  defp expand_each_spec(meta, [{expr, meta_e, args} = h | t], map, s, original_s, e)
       when is_atom(expr) do
    case validate_spec(expr, args) do
      {key, arg} ->
        {value, se, ee} = expand_spec_arg(arg, s, original_s, e)
        # elixir validates spec arg here
        # elixir raises bittype_mismatch in some cases
        expand_each_spec(meta, t, Map.put(map, key, value), se, original_s, ee)

      :none ->
        ha =
          if args == nil do
            {expr, meta_e, []}
          else
            h
          end

        # TODO how to check for cursor here?
        case Compiler.Macro.expand(ha, Map.put(e, :line, Utils.get_line(meta))) do
          ^ha ->
            # elixir raises here undefined_bittype
            # we omit the spec
            expand_each_spec(meta, t, map, s, original_s, e)

          new_types ->
            expand_each_spec(meta, unpack_specs(new_types, []) ++ t, map, s, original_s, e)
        end
    end
  end

  defp expand_each_spec(meta, [_expr | tail], map, s, original_s, e) do
    # elixir raises undefined_bittype
    # we skip it
    expand_each_spec(meta, tail, map, s, original_s, e)
  end

  defp expand_each_spec(_meta, [], map, s, _original_s, e), do: {map, s, e}

  defp compute_alignment(_, size, unit) when is_integer(size) and is_integer(unit),
    do: rem(size * unit, 8)

  defp compute_alignment(:default, size, unit), do: compute_alignment(:integer, size, unit)
  defp compute_alignment(:integer, :default, unit), do: compute_alignment(:integer, 8, unit)
  defp compute_alignment(:integer, size, :default), do: compute_alignment(:integer, size, 1)
  defp compute_alignment(:bitstring, size, :default), do: compute_alignment(:bitstring, size, 1)
  defp compute_alignment(:binary, size, :default), do: compute_alignment(:binary, size, 8)
  defp compute_alignment(:binary, _, _), do: 0
  defp compute_alignment(:float, _, _), do: 0
  defp compute_alignment(:utf32, _, _), do: 0
  defp compute_alignment(:utf16, _, _), do: 0
  defp compute_alignment(:utf8, _, _), do: 0
  defp compute_alignment(_, _, _), do: :unknown

  defp alignment(left, right) when is_integer(left) and is_integer(right) do
    rem(left + right, 8)
  end

  defp alignment(_, _), do: :unknown

  defp extract_meta({_, meta, _}, _), do: meta
  defp extract_meta(_, meta), do: meta

  defp infer_spec(:bitstring, meta), do: {:bitstring, meta, nil}
  defp infer_spec(:binary, meta), do: {:binary, meta, nil}
  defp infer_spec(:float, meta), do: {:float, meta, nil}
  defp infer_spec(:integer, meta), do: {:integer, meta, nil}
  defp infer_spec(:default, meta), do: {:integer, meta, nil}

  defp expr_type(integer) when is_integer(integer), do: :integer
  defp expr_type(float) when is_float(float), do: :float
  defp expr_type(binary) when is_binary(binary), do: :binary
  defp expr_type({:<<>>, _, _}), do: :bitstring
  defp expr_type(_), do: :default

  defp concat_or_prepend_bitstring(_meta, {:<<>>, _, []}, _e_right, acc),
    do: acc

  defp concat_or_prepend_bitstring(
         meta,
         {:<<>>, parts_meta, parts} = e_left,
         e_right,
         acc
       ) do
    # elixir raises unsized_binary in some cases

    case e_right do
      {:binary, _, nil} ->
        alignment = Keyword.fetch!(parts_meta, :alignment)

        if is_integer(alignment) do
          # elixir raises unaligned_binary if alignment != 0
          Enum.reverse(parts, acc)
        else
          [{:"::", meta, [e_left, e_right]} | acc]
        end

      {:bitstring, _, nil} ->
        Enum.reverse(parts, acc)
    end
  end

  defp concat_or_prepend_bitstring(meta, e_left, e_right, acc) do
    [{:"::", meta, [e_left, e_right]} | acc]
  end

  defp unpack_specs({:-, _, [h, t]}, acc), do: unpack_specs(h, unpack_specs(t, acc))

  defp unpack_specs({:*, _, [{:_, _, atom}, unit]}, acc) when is_atom(atom),
    do: [{:unit, [], [unit]} | acc]

  defp unpack_specs({:*, _, [size, unit]}, acc),
    do: [{:size, [], [size]}, {:unit, [], [unit]} | acc]

  defp unpack_specs(size, acc) when is_integer(size), do: [{:size, [], [size]} | acc]

  defp unpack_specs({expr, meta, args}, acc) when is_atom(expr) do
    list_args =
      cond do
        is_atom(args) -> nil
        is_list(args) -> args
        true -> args
      end

    [{expr, meta, list_args} | acc]
  end

  defp unpack_specs(other, acc), do: [other | acc]

  defp validate_spec(spec, []), do: validate_spec(spec, nil)
  defp validate_spec(:big, nil), do: {:endianness, :big}
  defp validate_spec(:little, nil), do: {:endianness, :little}
  defp validate_spec(:native, nil), do: {:endianness, :native}
  defp validate_spec(:size, [size]), do: {:size, size}
  defp validate_spec(:unit, [unit]), do: {:unit, unit}
  defp validate_spec(:integer, nil), do: {:type, :integer}
  defp validate_spec(:float, nil), do: {:type, :float}
  defp validate_spec(:binary, nil), do: {:type, :binary}
  defp validate_spec(:bytes, nil), do: {:type, :binary}
  defp validate_spec(:bitstring, nil), do: {:type, :bitstring}
  defp validate_spec(:bits, nil), do: {:type, :bitstring}
  defp validate_spec(:utf8, nil), do: {:type, :utf8}
  defp validate_spec(:utf16, nil), do: {:type, :utf16}
  defp validate_spec(:utf32, nil), do: {:type, :utf32}
  defp validate_spec(:signed, nil), do: {:sign, :signed}
  defp validate_spec(:unsigned, nil), do: {:sign, :unsigned}
  defp validate_spec(_, _), do: :none

  defp expand_spec_arg(expr, s, _original_s, e) when is_atom(expr) or is_integer(expr) do
    {expr, s, e}
  end

  defp expand_spec_arg(expr, s, original_s, %{context: :match} = e) do
    %{prematch: {pre_read, pre_cycle, _} = old_pre} = s
    %{vars: {original_read, _}} = original_s
    new_pre = {pre_read, pre_cycle, {:bitsize, original_read}}

    {e_expr, se, ee} =
      Compiler.expand(expr, %{s | prematch: new_pre}, %{e | context: :guard})

    {e_expr, %{se | prematch: old_pre}, %{ee | context: :match}}
  end

  defp expand_spec_arg(expr, s, original_s, e) do
    Compiler.expand(expr, State.reset_read(s, original_s), e)
  end

  defp size_and_unit(type, size, unit)
       when type in [:bitstring, :binary] and (size != :default or unit != :default) do
    # elixir raises here bittype_literal_bitstring or bittype_literal_string
    # we don't care
    size_and_unit(type, :default, :default)
  end

  defp size_and_unit(_expr_type, size, unit) do
    add_arg(:unit, unit, add_arg(:size, size, []))
  end

  defp build_spec(_size, _unit, type, endianness, _sign, spec)
       when type in [:utf8, :utf16, :utf32] do
    # elixir raises bittype_signed if signed
    # elixir raises bittype_utf if size specified
    # we don't care

    add_spec(type, add_spec(endianness, spec))
  end

  defp build_spec(_size, _unit, type, _endianness, _sign, spec)
       when type in [:binary, :bitstring] do
    # elixir raises bittype_signed if signed
    # elixir raises bittype_mismatch if bitstring unit != 1 or default
    # we don't care

    add_spec(type, spec)
  end

  defp build_spec(size, unit, type, endianness, sign, spec)
       when type in [:integer, :float] do
    number_size = number_size(size, unit)

    cond do
      type == :float and is_integer(number_size) ->
        if valid_float_size(number_size) do
          add_spec(type, add_spec(endianness, add_spec(sign, spec)))
        else
          # elixir raises here bittype_float_size
          # we fall back to 64
          build_spec(64, :default, type, endianness, sign, spec)
        end

      size == :default and unit != :default ->
        # elixir raises here bittype_unit
        # we fall back to default
        build_spec(size, :default, type, endianness, sign, spec)

      true ->
        add_spec(type, add_spec(endianness, add_spec(sign, spec)))
    end
  end

  defp add_spec(:default, spec), do: spec
  defp add_spec(key, spec), do: [{key, [], nil} | spec]

  defp number_size(size, :default) when is_integer(size), do: size
  defp number_size(size, unit) when is_integer(size), do: size * unit
  defp number_size(size, _), do: size

  defp valid_float_size(16), do: true
  defp valid_float_size(32), do: true
  defp valid_float_size(64), do: true
  defp valid_float_size(_), do: false

  defp add_arg(_key, :default, spec), do: spec
  defp add_arg(key, arg, spec), do: [{key, [], [arg]} | spec]

  defp is_match_size([_ | _], %{context: :match}), do: true
  defp is_match_size(_, _), do: false
end
