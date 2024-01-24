defmodule ElixirSense.Core.ErlangHtml do
  @moduledoc false

  # those typedefs mimic erl_docgen types (as of OTP 26) to not introduce dependency
  @type chunk_elements() :: [chunk_element()]
  @type chunk_element() ::
          {chunk_element_type(), chunk_element_attrs(), chunk_elements()}
          | :unicode.unicode_binary()
  @type chunk_element_attrs() :: [chunk_element_attr()]
  @type chunk_element_attr() ::
          {atom, :unicode.unicode_binary()}
  @type chunk_element_type() :: chunk_element_inline_type() | chunk_element_block_type()
  @type chunk_element_inline_type() :: :a | :code | :strong | :b | :em | :i
  @type chunk_element_block_type() ::
          :p
          | :div
          | :br
          | :pre
          | :ul
          | :ol
          | :li
          | :dl
          | :dt
          | :dd
          | :h1
          | :h2
          | :h3
          | :h4
          | :h5
          | :h6

  @doc """
  Transform application/erlang+html AST into markdown string.

  Document AST is defined in http://erlang.org/doc/apps/erl_docgen/doc_storage.html
  """

  @spec to_markdown(chunk_element(), module(), atom()) :: String.t()
  def to_markdown(ast, module, app), do: to_markdown(ast, [], :normal, module, app)

  def to_markdown(binary, _parents, sanitize_mode, _module, _app) when is_binary(binary) do
    sanitize(binary, sanitize_mode)
  end

  def to_markdown(list, parents, sanitize_mode, module, app) when is_list(list) do
    Enum.map_join(list, "", &to_markdown(&1, parents, sanitize_mode, module, app))
  end

  def to_markdown({:br, _attrs, _inner}, parents, _sanitize_mode, _module, _app) do
    "  \n" <> build_prefix(parents)
  end

  def to_markdown({:p, _attrs, inner}, parents, sanitize_mode, module, app) do
    prefix = build_prefix(parents)
    to_markdown(inner, parents, sanitize_mode, module, app) <> "\n" <> prefix <> "\n" <> prefix
  end

  def to_markdown({tag, _attrs, inner}, parents, sanitize_mode, module, app)
      when tag in [:em, :i] do
    "*" <> to_markdown(inner, parents, sanitize_mode, module, app) <> "*"
  end

  def to_markdown({tag, _attrs, inner}, parents, sanitize_mode, module, app)
      when tag in [:strong, :b] do
    "**" <> to_markdown(inner, parents, sanitize_mode, module, app) <> "**"
  end

  def to_markdown({:ul, _attrs, inner}, parents, sanitize_mode, module, app) do
    prefix = build_prefix(parents)

    items =
      inner
      |> Enum.map_join("", fn {:li, _attrs, li_inner} ->
        "- #{to_markdown(li_inner, [:li, :ul | parents], sanitize_mode, module, app)}\n" <> prefix
      end)

    items <> "\n" <> prefix
  end

  def to_markdown({:ol, _attrs, inner}, parents, sanitize_mode, module, app) do
    prefix = build_prefix(parents)

    items =
      inner
      |> Enum.with_index(1)
      |> Enum.map_join("", fn {{:li, _attrs, li_inner}, index} ->
        "#{index}. #{to_markdown(li_inner, [:li, :ol | parents], sanitize_mode, module, app)}\n" <>
          prefix
      end)

    items <> "\n" <> prefix
  end

  def to_markdown({:dl, _attrs, inner}, parents, sanitize_mode, module, app) do
    prefix = build_prefix(parents)
    to_markdown(inner, parents, sanitize_mode, module, app) <> "\n" <> prefix
  end

  def to_markdown({:dt, _attrs, inner}, parents, sanitize_mode, module, app) do
    "**" <> to_markdown(inner, parents, sanitize_mode, module, app) <> ":** "
  end

  def to_markdown({:dd, _attrs, inner}, parents, sanitize_mode, module, app) do
    prefix = build_prefix(parents)
    to_markdown(inner, parents, sanitize_mode, module, app) <> "  \n" <> prefix
  end

  def to_markdown(
        {:pre, _attrs1, [{:code, _attrs2, inner}]},
        parents,
        _sanitize_mode,
        module,
        app
      ) do
    prefix = build_prefix(parents)
    # TODO should we fence it as erlang?
    "```\n" <>
      prefix <>
      to_markdown(inner, parents, :none, module, app) <> "\n" <> prefix <> "```\n" <> prefix
  end

  for i <- 1..6,
      tag = :"h#{i}",
      prefix = for(_ <- 1..i, into: "", do: "#") do
    def to_markdown({unquote(tag), _attrs, inner}, parents, sanitize_mode, module, app) do
      unquote(prefix) <> " " <> to_markdown(inner, parents, sanitize_mode, module, app) <> "\n\n"
    end
  end

  def to_markdown({:div, attrs, inner}, parents, sanitize_mode, module, app) do
    class = attrs[:class]
    prefix = build_prefix(parents)
    maybe_class = if class != nil, do: String.upcase(class) <> ":  \n" <> prefix, else: ""

    "\n" <>
      prefix <>
      "\n" <>
      prefix <>
      "---" <>
      "\n" <>
      prefix <>
      "\n" <>
      prefix <>
      maybe_class <>
      to_markdown(inner, parents, sanitize_mode, module, app) <>
      "\n" <> prefix <> "\n" <> prefix <> "---" <> "\n" <> prefix <> "\n" <> prefix
  end

  def to_markdown({:code, _attrs, inner}, parents, _sanitize_mode, module, app) do
    "`" <> to_markdown(inner, parents, :backtick, module, app) <> "`"
  end

  def to_markdown({:a, attrs, inner}, parents, sanitize_mode, module_fallback, app_fallback) do
    href = Keyword.get(attrs, :href, "")

    rel = Keyword.get(attrs, :rel)

    href =
      if rel do
        [base, hash] =
          case String.split(href, "#", parts: 2) do
            [base, hash] -> [base, hash |> String.replace(" ", "%20")]
            [base] -> [base, ""]
          end

        [app, rest] =
          case String.split(base, ":", parts: 2) do
            [app, rest] -> [app, rest]
            [rest] -> ["", rest]
          end

        # based on
        # https://www.erlang.org/doc/apps/erl_docgen/inline_tags#%3Csee*%3E---see-tags
        case Keyword.get(attrs, :rel) do
          "https://erlang.org/doc/link/seemfa" ->
            # we need to transform
            # stdlib:gen_server#Module:handle_call/3
            # gen_server#Module:handle_call/3
            # #Module:handle_call/3
            # into
            # https://www.erlang.org/doc/man/gen_server#Module:handle_call-3

            module =
              if rest != "" do
                rest
              else
                to_string(module_fallback)
              end

            "https://www.erlang.org/doc/man/#{module}#" <> String.replace(hash, "/", "-")

          "https://erlang.org/doc/link/seeerl" ->
            # stdlib:string -> https://www.erlang.org/doc/man/string
            # stdlib:string#oldapi -> https://www.erlang.org/doc/man/string#oldapi

            module =
              if rest != "" do
                rest
              else
                to_string(module_fallback)
              end

            "https://www.erlang.org/doc/man/" <>
              module <> if(hash != "", do: "#" <> hash, else: "")

          "https://erlang.org/doc/link/seetype" ->
            # stdlib:gen_server#server_ref
            # https://www.erlang.org/doc/man/gen_server#type-server_ref

            module =
              if rest != "" do
                rest
              else
                to_string(module_fallback)
              end

            "https://www.erlang.org/doc/man/#{module}#type-#{hash}"

          "https://erlang.org/doc/link/seeapp" ->
            # stdlib:STDLIB_app -> https://www.erlang.org/doc/man/stdlib_app
            # stdlib:index -> https://www.erlang.org/doc/apps/stdlib/

            if app == "" do
              app = rest |> String.downcase() |> String.replace_suffix("_app", "")

              app =
                if app != "" do
                  app
                else
                  to_string(app_fallback)
                end

              "https://www.erlang.org/doc/man/#{app}_app"
            else
              if rest == "index" do
                "https://www.erlang.org/doc/apps/#{app}/"
              else
                "https://www.erlang.org/doc/man/#{app}_app"
              end
            end <> if(hash != "", do: "#" <> hash, else: "")

          "https://erlang.org/doc/link/seecom" ->
            # erts:epmd -> https://www.erlang.org/doc/man/epmd
            # erts:erl#remsh
            # erl
            # erl#async_thread_pool_size
            # #environment_variables

            module =
              if rest != "" do
                rest
              else
                to_string(module_fallback)
              end

            "https://www.erlang.org/doc/man/" <>
              module <> if(hash != "", do: "#" <> hash, else: "")

          "https://erlang.org/doc/link/seecref" ->
            # erts:erl_nif -> https://www.erlang.org/doc/man/erl_nif
            # erl_driver#driver_alloc_binary
            # #call

            module =
              if rest != "" do
                rest
              else
                to_string(module_fallback)
              end

            "https://www.erlang.org/doc/man/" <>
              module <> if(hash != "", do: "#" <> hash, else: "")

          "https://erlang.org/doc/link/seefile" ->
            # kernel:config -> https://www.erlang.org/doc/man/config
            # figures/perf-beamasm.svg
            # diameter_dict#MESSAGE_RECORDS
            # #FILE_FORMAT
            # diameter_dict

            file =
              if rest != "" do
                rest
              else
                to_string(module_fallback)
              end

            "https://www.erlang.org/doc/man/" <> file <> if(hash != "", do: "#" <> hash, else: "")

          "https://erlang.org/doc/link/seeguide" ->
            # kernel:index -> https://www.erlang.org/doc/apps/kernel/users_guide
            # kernel:logger_chapter -> https://www.erlang.org/doc/apps/kernel/logger_chapter

            # #listen
            # erl_dist_protocol#dflags
            # stdlib:unicode_usage#unicode_file_names
            # system/design_principles:gen_server_concepts -> https://www.erlang.org/doc/design_principles/gen_server_concepts

            if String.starts_with?(app, "system/") do
              page =
                app
                |> String.replace_prefix("system/", "")

              "https://www.erlang.org/doc/#{page}/#{if(rest == "index", do: "users_guide", else: rest)}"
            else
              app =
                if app == "" do
                  to_string(app_fallback)
                else
                  app
                end

              "https://www.erlang.org/doc/apps/#{app}/#{if(rest == "index", do: "users_guide", else: rest)}"
            end <> if(hash != "", do: "#" <> hash, else: "")
        end
      else
        href
      end

    "[" <>
      to_markdown(inner, parents, sanitize_mode, module_fallback, app_fallback) <>
      "](" <> href <> ")"
  end

  defp build_prefix(list), do: build_prefix(list, "")
  defp build_prefix([], acc), do: acc
  defp build_prefix([:li | rest], acc), do: build_prefix(rest, " " <> acc)
  defp build_prefix([:ul | rest], acc), do: build_prefix(rest, " " <> acc)
  defp build_prefix([:ol | rest], acc), do: build_prefix(rest, "  " <> acc)
  defp build_prefix([_other | rest], acc), do: build_prefix(rest, acc)

  @special_chars [
    "\\",
    "`",
    "*",
    "_",
    "{",
    "}",
    "[",
    "]",
    "<",
    ">",
    "(",
    ")",
    "#",
    "+",
    "-",
    ".",
    "!",
    "|"
  ]

  defp sanitize(binary, :normal) do
    Enum.reduce(@special_chars, binary, fn pattern, acc ->
      String.replace(acc, pattern, "\\" <> pattern)
    end)
  end

  defp sanitize(binary, :backtick) do
    if String.contains?(binary, "`") do
      "`" <> binary <> "`"
    else
      binary
    end
  end

  defp sanitize(binary, :none), do: binary
end
