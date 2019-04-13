defmodule ElixirSense.Core.TypeInfo do

  alias ElixirSense.Core.Normalized.Typespec
  alias ElixirSense.Core.Normalized.Code, as: NormalizedCode

  @basic_types %{
    "any" => "The top type, the set of all terms",
    "none" => "The bottom type, contains no terms",
    "atom" => "An atom is a constant whose name is its own value. Some other languages call these symbols",
    "map" => "Any map",
    "pid" => "A process identifier, pid, identifies a process",
    "port" => "A port identifier identifies an Erlang port",
    "reference" => "A reference is a term that is unique in an Erlang runtime system, created by calling `make_ref/0`",
    "struct" => "Any struct",
    "tuple" => "Tuple of any size",
    "float" => "A floating-point number",
    "integer" => "An integer number",
    "neg_integer" => "A negative integer",
    "non_neg_integer" => "A non-negative integer",
    "pos_integer" => "A positive integer",
    "list/1" => "Proper list ([]-terminated)",
    "nonempty_list/1" => "Non-empty proper list",
    "maybe_improper_list/2" => "Proper or improper list (type1=contents, type2=termination)",
    "nonempty_improper_list/2" => "Improper list (type1=contents, type2=termination)",
    "nonempty_maybe_improper_list/2" => "Non-empty proper or improper list"
  }

  @builtin_types %{
    "term" => %{spec: (quote do: term :: any()), doc: "Same as `any()`"},
    "arity" => %{
      spec: (quote do: arity :: 0..255),
      doc: "The number of arguments that a function takes"
    },
    "as_boolean/1" => %{
      spec: (quote do: as_boolean(t) :: t),
      doc: "A type `t` whose value will be used as a _truthy_ value"
    },
    "binary" => %{spec: (quote do: binary :: <<_::_*8>>), doc: "A blob of binary data"},
    "bitstring" => %{spec: (quote do: bitstring :: <<_::_*1>>), doc: "A bunch of bits"},
    "boolean" => %{spec: (quote do: boolean :: false | true), doc: "`true` or `false`"},
    "byte" => %{spec: (quote do: byte :: 0..255), doc: "A valid byte (0..255)" },
    "char" => %{spec: (quote do: char :: 0..0x10FFFF), doc: "A valid char (0..0x10ffff)"},
    "charlist" => %{spec: (quote do: charlist :: [char()]), doc: "A list of `char()`"},
    "nonempty_charlist" => %{
      spec: (quote do: nonempty_charlist :: [char(), ...]),
      doc: "A non-empty list of `char()`"
    },
    "fun" => %{spec: (quote do: fun :: (... -> any)), doc: "A function"},
    "function" => %{spec: (quote do: function :: fun()), doc: "Same as `fun()`"},
    "identifier" => %{
      spec: (quote do: identifier :: pid() | port() | reference()),
      doc: "A `pid()`, `port()` or `reference()`"
    },
    "iodata" => %{
      spec: (quote do: iodata :: iolist() | binary()),
      doc: "An `iolist()` or a `binary()`"
    },
    "iolist" => %{
      spec: (quote do: iolist :: maybe_improper_list(byte() | binary() | iolist(), binary() | [])),
      doc: "A list whose elements are either bytes, binaries or other iolists"
    },
    "keyword" => %{spec: (quote do: keyword :: [{atom(), any()}]), doc: "A keyword list"},
    "keyword/1" =>%{spec:  (quote do: keyword(t) :: [{atom(), t}]), doc: "A keyword list with values of type `t`"},
    "list" => %{spec: (quote do: list :: [any()]), doc: "A list"},
    "nonempty_list" => %{spec: (quote do: nonempty_list :: nonempty_list(any())), doc: "A non-empty list"},
    "maybe_improper_list" => %{
      spec: (quote do: maybe_improper_list :: maybe_improper_list(any(), any())),
      doc: "An alias for `maybe_improper_list(any(), any())`"
    },
    "nonempty_maybe_improper_list" => %{
      spec: (quote do: nonempty_maybe_improper_list :: nonempty_maybe_improper_list(any(), any())),
      doc: "An alias for `nonempty_maybe_improper_list(any(), any())`"
    },
    "mfa" => %{spec: (quote do: mfa :: {module(), atom(), arity()}), doc: "A tuple with {module, function, arity}"},
    "module" => %{spec: (quote do: module :: atom()), doc: "A module name. An alias for `atom()`"},
    "no_return" => %{
      spec: (quote do: no_return :: none()),
      doc: "A return type indicating that a function throws exceptions or loops forever and never terminates. An alias for `none()`"
    },
    "node" => %{spec: (quote do: node :: atom()), doc: "An atom representing a node name"},
    "number" => %{spec: (quote do: number :: integer() | float()), doc: "An integer or a float"},
    "struct" => %{spec: (quote do: struct :: %{:__struct__ => atom(), optional(atom()) => any()}), doc: "A struct"},
    "timeout" => %{spec: (quote do: timeout :: :infinity | non_neg_integer()), doc: "A non-negative integer or `:infinity`"},
  }

  def spec_ast_to_string(ast) do
    ast |> Macro.to_string |> String.replace("()", "")
  end

  def get_type_doc(module, type, type_n_args \\ 0) do
    # TODO: Use `with`
    case NormalizedCode.get_docs(module, :type_docs) do
      nil  -> ""
      docs ->
        case Enum.find(docs, fn({{name, n_args}, _, _, _}) -> type == name && type_n_args == n_args end) do
          {{_, _}, _, _, description} ->
            description || ""
          _ ->
            get_builtin_type_doc(type)
        end
    end
  end

  def get_builtin_type_doc(type, n_args \\ 0) do
    case @builtin_types[type_key(type, n_args)][:doc] do
      nil -> ""
      doc -> doc
    end
  end

  def get_builtin_type_spec(type, n_args \\ 0) do
    @builtin_types[type_key(type, n_args)][:spec]
  end

  def get_basic_type_doc(type, n_args \\ 0) do
    @basic_types[type_key(type, n_args)]
  end

  def get_spec(module, function, arity) when is_atom(module) and is_atom(function) and is_integer(arity) do
    module
    |> get_module_specs()
    |> Map.get({function, arity})
  end

  def get_function_specs(module, function) when is_atom(module) and is_atom(function) do
    specs = module |> get_module_specs()
    for {{f, _}, spec} <- specs, f == function do
      spec
    end
  end

  def get_module_specs(module) do
    case Typespec.beam_specs(module) do
      nil   -> %{}
      specs ->
        for {_kind, {{f, a}, _spec}} = spec <- specs, into: %{} do
          {{f, a}, spec}
        end
    end
  end

  # Workaround since Code.Typespec.typespec_to_quoted/1 is private
  def typespec_to_quoted(type) do
    {:::, [], [_, quoted]} = Typespec.type_to_quoted({:fake_var, type, []})
    quoted
  end

  def extract_param_options(mod, fun, npar) do
    get_function_specs(mod, fun)
    |> get_param_type_specs(npar)
    |> expand_type_specs(mod)
    |> Enum.filter(&list_type_spec?/1)
    |> extract_list_type_spec_options()
  end

  def get_type_info(module, type) do
    type_str = typespec_to_quoted(type) |> Macro.to_string()
    case extract_type_def_info(module, type) do
      {nil, name, n_args} ->
        {_full_name, spec, doc} =
          case get_builtin_type_spec(name, n_args) do
            # Basic type
            nil ->
              doc = get_basic_type_doc(to_string(name), n_args) || ""
              {type_str, "", doc}

            # Built-in type
            spec_ast ->
              doc = get_builtin_type_doc(to_string(name), n_args)
              {type_str, format_type_spec_ast(spec_ast, :type), doc}
          end
          %{
            origin: "",
            type_spec: type_str,
            doc: doc,
            expanded_spec: spec
          }

      {module, name, n_args} ->
        {_mod, expanded_type} = expand_type_spec(type, module)
        %{
          origin: inspect(module),
          type_spec: type_str,
          doc: get_type_doc(module, name, n_args),
          expanded_spec: expanded_type |> format_type_spec()
        }

      # Inline, non-existent
      _ ->
        %{
          origin: "",
          type_spec: type_str,
          doc: "",
          expanded_spec: ""
        }
    end
  end

  defp extract_list_type_spec_options(list_type_specs) do
    Enum.flat_map(list_type_specs, fn type_spec ->
      type_spec
      |> expand_list_type_spec()
      |> extract_union_options_name_and_type()
    end)
  end

  defp get_param_type_specs(func_specs, npar) do
    for func_spec <- func_specs,
        params_types = extract_params_types(func_spec),
        length(params_types) > npar do
      params_types |> Enum.at(npar)
    end
  end

  defp expand_type_specs(types, module) do
    types |> Enum.map(fn type -> expand_type_spec(type, module) end)
  end

  defp expand_type_spec({:ann_type, _, [{:var, _, _}, type]}, module) do
    expand_type_spec(type, module)
  end

  defp expand_type_spec({:user_type, _, type_name, type_args}, module) do
    type =
      module
      |> Typespec.get_types()
      |> Enum.find(fn {_, {name, _, args}} ->
        name == type_name && length(args) == length(type_args)
      end)
    {module, type}
  end

  defp expand_type_spec({:type, _, :list, [_|_]} = type, module) do
    {module, type}
  end

  defp expand_type_spec({:type, _, _, _} = type, module) do
    {module, {:not_found, {nil, type, []}}}
  end

  defp expand_type_spec({:remote_type, _, [{:atom, _, remote_mod}, {:atom, _, type_name}, []]} = type, _module) do
    remote_mod
    |> Typespec.get_types()
    |> Enum.find(fn {_, {name, _, _}} -> name == type_name end)
    |> case do
        nil -> {:not_found, type}
        type_found ->
          {remote_mod, type_found}
      end
  end

  defp expand_type_spec(type, module) do
    {module, type}
  end

  defp expand_list_type_spec({mod, {:type, _, :list, [type]}}) do
    expand_type_spec(type, mod)
  end

  defp expand_list_type_spec({mod, {_kind, {_name, {:type, _, :list, [type]}, _}}}) do
    expand_type_spec(type, mod)
  end

  # More than one option (union)
  defp extract_union_options_name_and_type({mod, {_kind, {_name, {:type, _, :union, options_types}, _}}}) do
    options_types
    |> Enum.map(&extract_tagged_tuple_name_and_type({mod, &1}))
    |> List.flatten()
  end

  # Only one option (not actually a union)
  defp extract_union_options_name_and_type({mod, {_kind, {_name, {:type, _, :tuple, _} = type, _}}}) do
    extract_tagged_tuple_name_and_type({mod, type})
  end

  defp extract_tagged_tuple_name_and_type({mod, {:type, _, :tuple, [{:atom, _, name}, type]}}) do
    [{mod, name, type}]
  end

  defp extract_tagged_tuple_name_and_type({mod, type}) do
    case expand_type_spec(type, mod) do
      {_mod, {_kind, {_name, {:type, _, :union, _}, _}}} = expanded_type ->
        extract_union_options_name_and_type(expanded_type)
      _ -> []
    end
  end

  defp extract_type_def_info(_mod, {:type, _, :list, [_|_]}) do
    :inline_list
  end

  defp extract_type_def_info(_mod, {:type, _, type_name, args}) do
    {nil, type_name, length(args)}
  end

  defp extract_type_def_info(_mod, {:type, _, type_name}) do
    {nil, type_name, 0}
  end

  defp extract_type_def_info(_mod, {:remote_type, _, [{:atom, _, remote_mod}, {:atom, _, name}, args]}) do
    remote_mod = if remote_mod == :elixir, do: nil, else: remote_mod
    {remote_mod, name, length(args)}
  end

  defp extract_type_def_info(mod, {_, _, type_name, args}) do
    {mod, type_name, length(args)}
  end

  defp extract_type_def_info(_, _) do
    :not_found
  end

  defp list_type_spec?({_mod, {:type, _, :list, [_]}}) do
    true
  end

  defp list_type_spec?({_mod, {_, {_, {:type, _, :list, [_]}, _}}}) do
    true
  end

  defp list_type_spec?(_) do
    false
  end

  defp format_type_spec(nil) do
    ""
  end

  defp format_type_spec({kind, type_spec}) do
    type_spec
    |> Typespec.type_to_quoted()
    |> format_type_spec_ast(kind)
  end

  defp format_type_spec(_) do
    ""
  end

  defp format_type_spec_ast(spec_ast, kind) do
    kind_size = kind |> to_string() |> String.length()

    spec_ast
    |> spec_ast_to_string()
    |> (&"@#{kind} #{&1}").()
    |> Code.format_string!(line_length: 35)
    |> to_string()
    |> String.split("\n")
    |> Enum.with_index()
    |> Enum.map(fn {l, i} when i > 0 -> String.slice(l, (kind_size + 2)..-1); {l, _} -> l end)
    |> Enum.join("\n")
  end

  defp extract_params_types({:spec, {_, [{:type, _, :fun, [{:type, _, :product, params_types}, _]}]}}) do
    params_types
  end

  defp extract_params_types({:spec, {_, [{:type, _, :bounded_fun, [type, constraints]}]}}) do
    {:type, _, :fun, [{:type, _, :product, params}, _]} = type

    vars_types =
      for {:type, _, :constraint, [{:atom, _, :is_subtype}, [{:var, _, var}, var_type]]} <- constraints, into: %{} do
        {var, var_type}
      end

    Enum.map(params, fn
      {:var, _, name} ->
        vars_types[name]
      {:type, l, :list, [{:var, _, name}]} ->
        {:type, l, :list, [vars_types[name]]}
      type -> type
    end)
  end

  defp type_key(type, n_args) do
    if n_args > 0 do
      "#{type}/#{n_args}"
    else
      "#{type}"
    end
  end
end
