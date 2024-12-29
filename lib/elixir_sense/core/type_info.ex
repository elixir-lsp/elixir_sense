defmodule ElixirSense.Core.TypeInfo do
  @moduledoc false

  alias ElixirSense.Core.Behaviours
  alias ElixirSense.Core.BuiltinTypes
  require ElixirSense.Core.Introspection, as: Introspection
  alias ElixirSense.Core.Normalized.Code, as: NormalizedCode
  alias ElixirSense.Core.Normalized.Typespec
  alias ElixirSense.Core.Source
  alias ElixirSense.Core.TypeAst

  require Logger

  @doc_spec_line_length 75
  @param_option_spec_line_length 35

  def find_all(module, filter \\ & &1) do
    case NormalizedCode.get_docs(module, :type_docs) do
      docs when is_list(docs) ->
        for(
          {{name, arity}, _, _, doc, metadata} <- docs,
          typedef = get_type_spec(module, name, arity),
          type_ast = TypeAst.from_typedef(typedef),
          spec = format_type_spec(typedef, line_length: @param_option_spec_line_length),
          signature = TypeAst.extract_signature(type_ast),
          info = %{
            name: name,
            arity: arity,
            doc: Introspection.extract_summary_from_docs(doc),
            spec: spec,
            signature: signature,
            metadata: metadata
          },
          filter.(info)
        ) do
          info
        end

      nil ->
        app = ElixirSense.Core.Applications.get_application(module)

        for {kind, {name, _type, args}} = typedef <- Typespec.get_types(module),
            kind in [:type, :opaque],
            spec = format_type_spec(typedef, line_length: @param_option_spec_line_length),
            type_ast = TypeAst.from_typedef(typedef),
            signature = TypeAst.extract_signature(type_ast),
            info = %{
              name: name,
              arity: length(args),
              doc: "",
              metadata: %{app: app},
              spec: spec,
              signature: signature
            },
            filter.(info) do
          info
        end
    end
  end

  @spec get_signatures(module | nil, atom, nil | [NormalizedCode.doc_entry_t()]) :: [
          ElixirSense.Core.Metadata.signature_t()
        ]
  def get_signatures(mod, type, code_docs \\ nil)

  def get_signatures(mod, type, code_docs) when not is_nil(mod) and not is_nil(type) do
    case code_docs || NormalizedCode.get_docs(mod, :type_docs) do
      docs when is_list(docs) ->
        for {{t, arity}, _, _, text, _metadata} <- docs, t == type do
          {_kind, {_name, _def, args}} = get_type_spec(mod, type, arity)
          type_args = Enum.map(args, &(&1 |> elem(2) |> Atom.to_string()))
          type_str = Atom.to_string(type)
          doc = Introspection.extract_summary_from_docs(text)
          typedef = get_type_spec(mod, type, arity)
          spec = format_type_spec(typedef, line_length: @param_option_spec_line_length)
          %{name: type_str, params: type_args, documentation: doc, spec: spec}
        end

      nil ->
        for {kind, {name, _type, args}} = typedef <- Typespec.get_types(mod),
            name == type,
            kind in [:type, :opaque] do
          type_args = Enum.map(args, &(&1 |> elem(2) |> Atom.to_string()))

          %{
            name: Atom.to_string(name),
            params: type_args,
            documentation: "",
            spec: format_type_spec(typedef)
          }
        end
    end
  end

  def get_signatures(nil, type, _code_docs) when not is_nil(type) do
    for ti <- BuiltinTypes.get_builtin_type_info(type) do
      %{
        name: Atom.to_string(type),
        params: ti.params |> Enum.map(&Atom.to_string/1),
        documentation: ti.doc,
        spec:
          "@type " <>
            case ti do
              %{spec: ast} -> spec_ast_to_string(ast)
              %{signature: signature} -> signature
              _ -> "#{type}"
            end
      }
    end
  end

  def find_all_builtin(filter \\ & &1) do
    extract_name_and_arity = fn key ->
      name_parts = String.split(key, "/")
      arity = name_parts |> Enum.at(1, "0") |> String.to_integer()
      {Enum.at(name_parts, 0), arity}
    end

    for(
      {key, value} <- BuiltinTypes.all(),
      spec =
        case value do
          %{spec: spec} ->
            format_type_spec_ast(spec, :type, line_length: @param_option_spec_line_length)

          %{signature: signature} ->
            "@type #{signature}"

          _ ->
            "@type #{key}()"
        end,
      signature <- [
        value[:signature] || TypeAst.extract_signature(value[:spec]) || "#{key}()"
      ],
      {name, arity} = extract_name_and_arity.(key),
      doc = value[:doc] || "",
      info = %{
        name: String.to_atom(name),
        arity: arity,
        doc: doc,
        spec: spec,
        signature: signature,
        metadata: %{builtin: true}
      },
      filter.(info)
    ) do
      info
    end
  end

  def get_type_spec(module, type_name) do
    module
    |> Typespec.get_types()
    |> Enum.filter(fn {_, {name, _, _}} -> name == type_name end)
    |> Enum.sort_by(fn {_, {_, _, args}} -> length(args) end)
    |> Enum.at(0)
  end

  def get_type_spec(module, type_name, n_args) do
    module
    |> Typespec.get_types()
    |> Enum.find(fn {_, {name, _, args}} ->
      name == type_name && length(args) == n_args
    end)
  end

  def get_type_ast(module, type_name) do
    {_kind, type} = get_type_spec(module, type_name)
    Typespec.type_to_quoted(type)
  end

  def spec_ast_to_string(ast) do
    ast |> Macro.to_string()
  end

  def type_spec_to_string({kind, type}) do
    binary = Typespec.type_to_quoted(type) |> Macro.to_string()
    "@#{kind} #{binary}"
  end

  def get_type_spec_as_string(module, type, arity) do
    get_type_spec(module, type, arity)
    |> type_spec_to_string
  end

  def format_type_spec(spec) do
    format_type_spec(spec, [])
  end

  def format_type_spec({:opaque, type}, opts) do
    {:"::", _, [ast, _]} = Typespec.type_to_quoted(type)

    ast
    |> format_type_spec_ast(:opaque, opts)
  end

  def format_type_spec({kind, type_spec}, opts) do
    type_spec
    |> Typespec.type_to_quoted()
    |> format_type_spec_ast(kind, opts)
  end

  def format_type_spec(_, _) do
    ""
  end

  def format_type_spec_ast(spec_ast, kind) do
    format_type_spec_ast(spec_ast, kind, [])
  end

  def format_type_spec_ast(nil, _kind, _opts) do
    ""
  end

  def format_type_spec_ast(spec_ast, kind, opts) do
    line_length = opts[:line_length] || @doc_spec_line_length
    kind_size = kind |> to_string() |> String.length()

    {sanitized, original} = sanitize_type_name(spec_ast)

    string =
      sanitized
      |> Macro.to_string()
      |> (&"@#{kind} #{&1}").()

    string =
      try do
        string
        |> Code.format_string!(line_length: line_length)
        |> to_string()
      rescue
        e ->
          if Version.match?(System.version(), ">= 1.18.0-dev") do
            Logger.error(
              "Macro.to_string(#{inspect(sanitized)}) returned invalid code. Please report that to elixir project."
            )

            reraise e, __STACKTRACE__
          else
            string
          end
      end

    string
    |> String.replace("__replace_me__", "#{original}")
    |> Source.split_lines()
    |> Enum.with_index()
    |> Enum.map_join("\n", fn
      {l, i} when i > 0 -> String.slice(l, (kind_size + 2)..-1//1)
      {l, _} -> l
    end)
  end

  defp sanitize_type_name({:"::", meta1, [{type, meta2, args}, rest]}) do
    {{:"::", meta1, [{:__replace_me__, meta2, args}, rest]}, type}
  end

  defp sanitize_type_name({type, meta, args}) do
    {{:__replace_me__, meta, args}, type}
  end

  @spec get_type_doc(
          [ElixirSense.Core.Normalized.Code.doc_entry_t()],
          atom,
          non_neg_integer | :any
        ) :: ElixirSense.Core.Normalized.Code.doc_entry_t() | nil
  def get_type_doc(docs, type, type_n_args) do
    Enum.find(docs, fn {{name, n_args}, _, _, _, _} ->
      type == name && (type_n_args == n_args || type_n_args == :any)
    end)
  end

  @spec get_type_doc_desc(
          [ElixirSense.Core.Normalized.Code.doc_entry_t()],
          atom,
          non_neg_integer
        ) :: {String.t(), map}
  def get_type_doc_desc(docs, type, type_n_args) do
    case get_type_doc(docs, type, type_n_args) do
      nil -> {BuiltinTypes.get_builtin_type_doc(type), %{}}
      doc -> get_doc_description(doc)
    end
  end

  defp get_doc_description({{_, _}, _, _, desc, metadata}) when is_binary(desc) do
    {desc, metadata}
  end

  defp get_doc_description(_) do
    {"", %{}}
  end

  def get_spec(module, function, arity)
      when is_atom(module) and is_atom(function) and is_integer(arity) do
    # does not drop MACRO- prefix
    module
    |> get_module_specs()
    |> Map.get({function, arity})
  end

  def get_callback(module, function, arity)
      when is_atom(module) and is_atom(function) and is_integer(arity) do
    # does not drop MACRO- prefix
    module
    |> get_module_callbacks()
    |> Map.get({function, arity})
  end

  # does not drop MACRO- prefix
  def get_function_specs(module, function, arity) when is_atom(module) and is_atom(function) do
    module_specs = module |> get_module_specs()

    function_specs =
      for {{f, a}, spec} <- module_specs,
          f == function,
          Introspection.matches_arity?(a, arity) do
        spec
      end

    if function_specs != [] do
      {nil, function_specs}
    else
      # TODO this will not work correctly for :any arity in case many functions with the same name and different arities
      # are implement different behaviours
      callback_specs =
        module
        |> Behaviours.get_module_behaviours()
        |> Enum.reduce_while(nil, fn behaviour, acc ->
          behaviour_specs = behaviour |> get_module_callbacks()

          callback_specs =
            for {{f, a}, spec} <- behaviour_specs,
                f == function,
                Introspection.matches_arity?(a, arity) do
              spec
            end

          if callback_specs != [] do
            {:halt, {behaviour, callback_specs}}
          else
            {:cont, acc}
          end
        end)

      if callback_specs do
        callback_specs
      else
        {nil, []}
      end
    end
  end

  def get_function_spec(module, function, arity)
      when is_atom(module) and is_atom(function) and is_integer(arity) do
    function_spec = get_spec(module, function, arity)

    if function_spec != nil do
      function_spec
    else
      module
      |> Behaviours.get_module_behaviours()
      |> Enum.reduce_while(nil, fn behaviour, acc ->
        callback_spec = get_callback(behaviour, function, arity)

        if callback_spec != nil do
          {:halt, callback_spec}
        else
          {:cont, acc}
        end
      end)
    end
  end

  def get_module_specs(module) do
    Typespec.get_specs(module)
    |> Map.new(fn
      {{f, a}, _spec} = spec ->
        {{f, a}, spec}

      {{^module, f, a}, spec} ->
        # spec with module - transform it to moduleless form
        {{f, a}, {{f, a}, spec}}
    end)
  end

  def get_module_callbacks(module) do
    Typespec.get_callbacks(module)
    |> Map.new(fn
      {{f, a}, _spec} = spec ->
        {{f, a}, spec}

      {{^module, f, a}, spec} ->
        # spec with module - transform it to moduleless form
        {{f, a}, {{f, a}, spec}}
    end)
  end

  # Workaround since Code.Typespec.typespec_to_quoted/1 is private
  def typespec_to_quoted(type) do
    {:"::", [], [_, quoted]} = Typespec.type_to_quoted({:fake_var, type, []})
    quoted
  end

  def type_str(type) do
    typespec_to_quoted(type) |> Introspection.to_string_with_parens()
  end

  def extract_params(type) do
    quoted = Typespec.spec_to_quoted(:dummy, type)

    case quoted do
      {:when, _, [{:"::", _, [{:dummy, _, args}, _res]}, _var_args]} -> args
      {:"::", _, [{:dummy, _, args}, _res]} -> args
    end
    |> Enum.map(fn arg ->
      case arg do
        {:"::", _, [{atom, _, _}, _right]} when is_atom(atom) -> to_string(atom)
        {:|, _, [_left, _right]} -> "term"
        [_ | _] -> "list"
        {_, _} -> "tuple"
        {:{}, _, _} -> "tuple"
        %{} -> "map"
        {atom, _, _} when is_atom(atom) -> to_string(atom)
        _other -> "term"
      end
    end)
  end

  def typespec_to_string(kind, spec) do
    "@#{kind} #{spec |> Macro.to_string()}"
  end
end
