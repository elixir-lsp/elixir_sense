defmodule ElixirSense.Plugins.Ecto do
  @moduledoc false

  alias ElixirSense.Core.Introspection
  alias ElixirSense.Core.Metadata
  alias ElixirSense.Core.Source
  alias ElixirSense.Core.State
  alias ElixirSense.Providers.Suggestion.Complete

  @ecto_types [
    ":string",
    ":boolean",
    ":integer",
    ":float",
    ":decimal",
    ":id",
    ":date",
    ":time",
    ":time_usec",
    ":naive_datetime",
    ":naive_datetime_usec",
    ":utc_datetime",
    ":utc_datetime_usec",
    "{:array\\, inner_type}",
    ":map",
    "{:map\\, inner_type}",
    ":binary_id",
    ":binary"
  ]

  @type_choices Enum.join(@ecto_types, ",")

  @joins [
    :join,
    :inner_join,
    :cross_join,
    :left_join,
    :right_join,
    :full_join,
    :inner_lateral_join,
    :left_lateral_join
  ]

  @from_join_opts [
    as: "A named binding for the from/join.",
    prefix: "The prefix to be used for the from/join when issuing a database query.",
    hints: "A string or a list of strings to be used as database hints."
  ]

  @join_opts [on: "A query expression or keyword list to filter the join."]

  @var_r "[a-z][a-zA-Z0-9_]*"
  @mod_r "[A-Z][a-zA-Z0-9_]*"
  @binding_r "(#{@var_r}) in (#{@mod_r}|assoc\\(\\s*#{@var_r},\\s*\\:#{@var_r}\\s*\\))"

  @doc """
  A reducer that adds suggestions of available associations for `assoc/2`.

  The suggestions are only added when inside a `from/2` call.
  """
  def add_associations(hint, prefix, env, buffer_metadata, acc) do
    func_call_chain = func_call_chain_until({Ecto.Query, :from}, prefix, env, buffer_metadata)

    with [{nil, :assoc, 1, assoc_info}, {_, :from, 1, from_info}] <- func_call_chain,
         %{pos: {{line, col}, _}} <- assoc_info,
         assoc_code <- Source.text_after(prefix, line, col),
         [_, var] <- Regex.run(~r/^assoc\(\s*(#{@var_r})\s*,/, assoc_code),
         %{^var => %{type: type}} <- extract_bindings(prefix, from_info, env, buffer_metadata),
         true <- function_exported?(type, :__schema__, 1) do
      {:halt, %{acc | result: find_assoc_suggestions(type, hint)}}
    else
      _ ->
        {:cont, acc}
    end
  end

  @doc """
  A reducer that adds suggestions for `Ecto.Query.from/2`.

  Available Suggestions:

  * From clauses - e.g. `select`, `where`, `join`, etc.
  * Query bindings
  * Query bindings' fields

  """
  def add_from_options(hint, prefix, env, buffer_metadata, acc) do
    func_call_chain = func_call_chain_until({Ecto.Query, :from}, prefix, env, buffer_metadata)
    from_call = List.last(func_call_chain)

    case from_call do
      {_mod, _fun, 1, %{cursor_at_option: false} = from_info} ->
        bindings = extract_bindings(prefix, from_info, env, buffer_metadata)
        bindings_list = bindings_suggestions(hint, bindings)
        {:cont, %{acc | result: acc.result ++ bindings_list}}

      {_mod, _fun, 1, _from_info} ->
        clauses_list =
          clauses_suggestions(hint) ++ joins_suggestions(hint) ++ join_opts_suggestions(hint)

        {:halt, %{acc | result: clauses_list}}

      _ ->
        {:cont, acc}
    end
  end

  @doc """
  A decorator that adds customized snippets for:

  * `Ecto.Schema.field`
  * `Ecto.Migration.add`

  """
  def decorate(%{origin: "Ecto.Schema", name: "field", arity: arity} = item)
      when arity in 1..3 do
    snippet = snippet_for_field(arity, @type_choices)
    Map.put(item, :snippet, snippet)
  end

  def decorate(%{origin: "Ecto.Migration", name: "add", arity: arity} = item)
      when arity in 2..3 do
    snippet = snippet_for_add(arity, @type_choices)
    Map.put(item, :snippet, snippet)
  end

  def decorate(item) do
    item
  end

  defp clauses_suggestions(hint) do
    funs = Complete.get_module_funs(Ecto.Query, false)

    for {name, arity, arity, :macro, {doc, _}, _, "query," <> _} <- funs,
        clause = to_string(name),
        String.starts_with?(clause, hint) do
      clause_to_suggestion(clause, doc, "from clause")
    end
  end

  defp joins_suggestions(hint) do
    for name <- @joins -- [:join],
        clause = to_string(name),
        String.starts_with?(clause, hint) do
      join_kind = String.replace(clause, "_", " ")
      doc = "A #{join_kind} query expression."
      clause_to_suggestion(clause, doc, "from clause")
    end
  end

  defp join_opts_suggestions(hint) do
    for {name, doc} <- @join_opts ++ @from_join_opts,
        clause = to_string(name),
        String.starts_with?(clause, hint) do
      type = if Keyword.has_key?(@join_opts, name), do: "join", else: "from/join"
      clause_to_suggestion(clause, doc, "#{type} option")
    end
  end

  defp find_fields(type, hint) do
    with {:module, _} <- Code.ensure_compiled(type),
         true <- function_exported?(type, :__schema__, 1) do
      for field <- Enum.sort(type.__schema__(:fields)),
          name = to_string(field),
          String.starts_with?(name, hint) do
        %{name: field, type: type.__schema__(:type, field)}
      end
    else
      _ ->
        []
    end
  end

  defp find_assoc_suggestions(type, hint) do
    for assoc <- type.__schema__(:associations),
        assoc_str = inspect(assoc),
        String.starts_with?(assoc_str, hint) do
      assoc_mod = type.__schema__(:association, assoc).related
      {doc, _} = Introspection.get_module_docs_summary(assoc_mod)

      %{
        type: :generic,
        kind: :field,
        label: assoc_str,
        detail: "(Ecto association) #{inspect(assoc_mod)}",
        documentation: doc
      }
    end
  end

  defp find_field_relations(field, type) do
    associations = type.__schema__(:associations)

    for assoc_name <- associations,
        assoc = type.__schema__(:association, assoc_name),
        assoc.owner == type,
        assoc.owner_key == field.name do
      assoc
    end
  end

  defp bindings_suggestions(hint, bindings) do
    case String.split(hint, ".") do
      [var, field_hint] ->
        type = bindings[var][:type]

        type
        |> find_fields(field_hint)
        |> Enum.map(fn f -> field_to_suggestion(f, type) end)

      _ ->
        for {name, %{type: type}} <- bindings,
            String.starts_with?(name, hint) do
          binding_to_suggestion(name, type)
        end
    end
  end

  defp snippet_for_field(1, _choices), do: "field :${1:name}"
  defp snippet_for_field(2, choices), do: "field :${1:name}, ${2|#{choices}|}"
  defp snippet_for_field(3, choices), do: "field :${1:name}, ${2|#{choices}|}, ${3:opts}"

  defp snippet_for_add(2, choices), do: "add :${1:column}, ${2|#{choices}|}"
  defp snippet_for_add(3, choices), do: "add :${1:column}, ${2|#{choices}|}, ${3:opts}"

  defp clause_to_suggestion(option, doc, detail) do
    doc_str =
      doc
      |> doc_sections()
      |> Enum.filter(fn {k, _v} -> k in [:summary, "Keywords examples", "Keywords example"] end)
      |> Enum.map_join("\n\n", fn
        {:summary, text} ->
          text

        {_, text} ->
          [first | _] = String.split(text, "\n\n")
          if first == "", do: "", else: "### Example\n\n#{first}"
      end)

    %{
      type: :generic,
      kind: :property,
      label: option,
      insert_text: "#{option}: ",
      detail: "(#{detail}) Ecto.Query",
      documentation: doc_str
    }
  end

  defp binding_to_suggestion(binding, type) do
    {doc, _} = Introspection.get_module_docs_summary(type)

    %{
      type: :generic,
      kind: :variable,
      label: binding,
      detail: "(query binding) #{inspect(type)}",
      documentation: doc
    }
  end

  defp field_to_suggestion(field, origin) do
    type_str = inspect(field.type)
    associations = find_field_relations(field, origin)

    relations =
      Enum.map_join(associations, ", ", fn assoc ->
        "`#{inspect(assoc.related)} (#{inspect(assoc.related_key)})`"
      end)

    related_info = if relations == "", do: "", else: "* **Related:** #{relations}"

    doc = """
    The `#{inspect(field.name)}` field of `#{inspect(origin)}`.

    * **Type:** `#{type_str}`
    #{related_info}
    """

    %{
      type: :generic,
      kind: :field,
      label: to_string(field.name),
      detail: "Ecto field",
      documentation: doc
    }
  end

  defp doc_sections(doc) do
    [summary_and_detail | rest] = String.split(doc, "##")
    summary_and_detail_parts = Source.split_lines(summary_and_detail, parts: 2)
    summary = summary_and_detail_parts |> Enum.at(0, "") |> String.trim()
    detail = summary_and_detail_parts |> Enum.at(1, "") |> String.trim()

    sections =
      Enum.map(rest, fn text ->
        [title, body] = Source.split_lines(text, parts: 2)
        {String.trim(title), String.trim(body, "\n")}
      end)

    [{:summary, summary}, {:detail, detail}] ++ sections
  end

  defp actual_mod_fun({mod, fun}, elixir_prefix, env, buffer_metadata) do
    %State.Env{imports: imports, aliases: aliases, module: module} = env
    %Metadata{mods_funs_to_positions: mods_funs, types: metadata_types} = buffer_metadata

    Introspection.actual_mod_fun(
      {mod, fun},
      imports,
      if(elixir_prefix, do: [], else: aliases),
      module,
      mods_funs,
      metadata_types
    )
  end

  defp infer_type({:__aliases__, _, _} = mod_ast, _vars, env, buffer_metadata) do
    mod = Macro.expand_once(mod_ast, %Macro.Env{})
    {actual_mod, _, _} = actual_mod_fun({mod, nil}, false, env, buffer_metadata)
    actual_mod
  end

  defp infer_type({:assoc, _, [{var, _, _}, assoc]}, vars, _env, _buffer_metadata) do
    var_type = vars[to_string(var)][:type]

    if var_type && function_exported?(var_type, :__schema__, 2) do
      var_type.__schema__(:association, assoc).related
    end
  end

  defp infer_type(_, _vars, _env, _buffer_metadata) do
    nil
  end

  defp extract_bindings(prefix, %{pos: {{line, col}, _}} = func_info, env, buffer_metadata) do
    func_code = Source.text_after(prefix, line, col)

    from_matches = Regex.scan(~r/^.+\(?\s*(#{@binding_r})/, func_code)

    join_matches =
      for {join, {line, col, _}} when join in @joins <- func_info.options_so_far,
          code = Source.text_after(prefix, line, col),
          match <- Regex.scan(~r/^#{join}\:\s*(#{@binding_r})/, code) do
        match
      end

    matches = from_matches ++ join_matches

    Enum.reduce(matches, %{}, fn [_, _, var, expr], bindings ->
      case Code.string_to_quoted(expr) do
        {:ok, expr_ast} ->
          type = infer_type(expr_ast, bindings, env, buffer_metadata)
          Map.put(bindings, var, %{type: type})

        _ ->
          bindings
      end
    end)
  end

  defp extract_bindings(_prefix, _func_info, _env, _buffer_metadata) do
    %{}
  end

  defp partial_func_call(code, env, buffer_metadata) do
    %State.Env{module: module} = env
    func_info = Source.which_func(code, module)

    with %{candidate: {mod, fun}, npar: npar} <- func_info,
         mod_fun <- actual_mod_fun({mod, fun}, func_info.elixir_prefix, env, buffer_metadata),
         {actual_mod, actual_fun, _} <- mod_fun do
      {actual_mod, actual_fun, npar, func_info}
    else
      _ ->
        :none
    end
  end

  defp func_call_chain_until(mod_fun, code, env, buffer_metadata) do
    func_call_chain_until(mod_fun, code, env, buffer_metadata, [])
  end

  defp func_call_chain_until(mod_fun, code, env, buffer_metadata, chain) do
    {actual_mod, actual_fun} = mod_fun

    case partial_func_call(code, env, buffer_metadata) do
      :none ->
        []

      {^actual_mod, ^actual_fun, _npar, _info} = func_call ->
        Enum.reverse([func_call | chain])

      {_mod, _fun, _npar, %{pos: {{line, col}, _}}} = func_call ->
        code_before = Source.text_before(code, line, col)
        func_call_chain_until(mod_fun, code_before, env, buffer_metadata, [func_call | chain])
    end
  end
end
