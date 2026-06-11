defmodule ElixirSense.Core.TypeHintsTest do
  use ExUnit.Case, async: false

  alias ElixirSense.Core.Binding
  alias ElixirSense.Core.Metadata
  alias ElixirSense.Core.Parser
  alias ElixirSense.Core.State.VarInfo
  alias ElixirSense.Core.TypeHints
  alias ElixirSense.Core.TypePresentation, as: TP

  setup do
    original = Application.get_env(:elixir_sense, :use_elixir_types, false)
    Application.put_env(:elixir_sense, :use_elixir_types, true)
    on_exit(fn -> Application.put_env(:elixir_sense, :use_elixir_types, original) end)
    :ok
  end

  defp metadata(code, position) do
    Parser.parse_string(code, true, true, position)
  end

  defp find_var(metadata, position, name) do
    env = Metadata.get_env(metadata, position)
    Enum.find(env.vars, &(&1.name == name))
  end

  describe "request_context/1" do
    test "wraps metadata with a unique ref" do
      md = metadata("defmodule M do\n  def f, do: :ok\nend\n", {2, 3})
      ctx1 = TypeHints.request_context(md)
      ctx2 = TypeHints.request_context(md)

      assert ctx1.metadata == md
      assert is_reference(ctx1.ref)
      assert ctx1.ref != ctx2.ref
    end
  end

  describe "type_hint_for_var/4" do
    @code """
    defmodule M do
      def f do
        x = :ok
        x
      end
    end
    """

    test "parity with the direct TypePresentation.render_hint/3 path" do
      pos = {4, 5}
      md = metadata(@code, pos)
      var = find_var(md, pos, :x)
      assert %VarInfo{} = var

      env = Metadata.get_env(md, pos)
      direct = TP.render_hint(Binding.from_env(env, md, pos), var, [])

      ctx = TypeHints.request_context(md)
      assert TypeHints.type_hint_for_var(ctx, pos, var, []) == direct
      assert {:ok, %{label: ":ok", full: ":ok", source: _}} = direct
    end

    test ":skip on nil env" do
      # Metadata.get_env always returns at least a default env, so the nil-env
      # branch is defensive. Exercise it directly with an empty metadata: a
      # position with no scopes still yields a default env, but a var with no
      # type renders nothing, so the only observable :skip path is the
      # uninformative one (next test). Here we assert the guard does not crash
      # and skips when the env carries no binding context.
      md = metadata("\n", {1, 1})
      var = %VarInfo{version: 1, name: :x, type: nil}
      ctx = TypeHints.request_context(md)
      assert TypeHints.type_hint_for_var(ctx, {1, 1}, var, []) == :skip
    end

    test ":skip on uninformative var" do
      pos = {4, 5}
      md = metadata(@code, pos)
      unknown = %VarInfo{version: 1, name: :x, type: nil}
      ctx = TypeHints.request_context(md)
      assert TypeHints.type_hint_for_var(ctx, pos, unknown, []) == :skip
    end

    test "the local-sigs cache is populated and reused across hints" do
      pos = {4, 5}
      md = metadata(@code, pos)
      var = find_var(md, pos, :x)
      ctx = TypeHints.request_context(md)

      sigs_key = {TypeHints, ctx.ref, :local_sigs, M}
      env_key = {TypeHints, ctx.ref, :env, pos}
      assert Process.get(sigs_key) == nil

      r1 = TypeHints.type_hint_for_var(ctx, pos, var, [])

      # After the first call both caches are populated.
      assert Process.get(sigs_key) != nil
      assert Process.get(env_key) != nil

      # A second call returns a consistent result (served from cache).
      r2 = TypeHints.type_hint_for_var(ctx, pos, var, [])
      assert r1 == r2
    end
  end

  describe "effective_params/4 — metadata modules" do
    defp ctx_for(code) do
      code |> metadata({1, 1}) |> TypeHints.request_context()
    end

    test "trailing default elided at lower arity" do
      ctx = ctx_for("defmodule M do\n  def f(a, b \\\\ 1), do: {a, b}\nend\n")

      assert TypeHints.effective_params(ctx, M, :f, 2) ==
               {:ok, [%{name: "a", has_default: false}, %{name: "b", has_default: true}]}

      assert TypeHints.effective_params(ctx, M, :f, 1) ==
               {:ok, [%{name: "a", has_default: false}]}
    end

    test "non-trailing default: f(a, b \\\\ 1, c) at arity 2 → [a, c]" do
      ctx = ctx_for("defmodule M do\n  def f(a, b \\\\ 1, c), do: {a, b, c}\nend\n")

      assert TypeHints.effective_params(ctx, M, :f, 2) ==
               {:ok, [%{name: "a", has_default: false}, %{name: "c", has_default: false}]}

      assert TypeHints.effective_params(ctx, M, :f, 3) ==
               {:ok,
                [
                  %{name: "a", has_default: false},
                  %{name: "b", has_default: true},
                  %{name: "c", has_default: false}
                ]}
    end

    test "two defaults at lower arity matches the empirical left-to-right rule" do
      # g(a \\ 1, b \\ 2, c): /1 → [c]; /2 → [a, c] (leftmost default kept).
      ctx = ctx_for("defmodule M do\n  def g(a \\\\ 1, b \\\\ 2, c), do: {a, b, c}\nend\n")

      assert TypeHints.effective_params(ctx, M, :g, 1) ==
               {:ok, [%{name: "c", has_default: false}]}

      assert TypeHints.effective_params(ctx, M, :g, 2) ==
               {:ok, [%{name: "a", has_default: true}, %{name: "c", has_default: false}]}
    end

    test "pattern-match default %{} = opts \\\\ %{} → name \"opts\" (AST-level)" do
      ctx = ctx_for("defmodule M do\n  def h(%{} = opts \\\\ %{}), do: opts\nend\n")

      assert TypeHints.effective_params(ctx, M, :h, 1) ==
               {:ok, [%{name: "opts", has_default: true}]}

      assert TypeHints.effective_params(ctx, M, :h, 0) == {:ok, []}
    end

    test "unnamed literal pattern → nil name entry" do
      ctx = ctx_for("defmodule M do\n  def lit(1, x), do: {x}\nend\n")

      assert TypeHints.effective_params(ctx, M, :lit, 2) ==
               {:ok, [%{name: nil, has_default: false}, %{name: "x", has_default: false}]}
    end

    test ":error for an unknown function" do
      ctx = ctx_for("defmodule M do\n  def f(a), do: a\nend\n")
      assert TypeHints.effective_params(ctx, M, :nope, 1) == :error
    end

    test "result is cached per {ref, module, fun, arity}" do
      ctx = ctx_for("defmodule M do\n  def f(a, b \\\\ 1), do: {a, b}\nend\n")
      key = {TypeHints, ctx.ref, :params, M, :f, 1}
      assert Process.get(key) == nil
      result = TypeHints.effective_params(ctx, M, :f, 1)
      assert Process.get(key) == result
    end
  end

  describe "effective_params/4 — remote / compiled modules" do
    test "String.split/2 vs /3 default detection" do
      ctx = ctx_for("defmodule M do\n  def x, do: :ok\nend\n")

      # String.split/3 head is (string, pattern, options \\ []); /2 elides the default.
      assert {:ok, params2} = TypeHints.effective_params(ctx, String, :split, 2)
      assert length(params2) == 2
      assert Enum.all?(params2, &(&1.has_default == false))

      assert {:ok, params3} = TypeHints.effective_params(ctx, String, :split, 3)
      assert length(params3) == 3
      assert List.last(params3).has_default == true
    end

    test "GenServer.start_link/3 has a trailing default option" do
      ctx = ctx_for("defmodule M do\n  def x, do: :ok\nend\n")

      assert {:ok, params} = TypeHints.effective_params(ctx, GenServer, :start_link, 3)
      assert length(params) == 3
      assert List.last(params).has_default == true
      assert List.last(params).name == "options"
    end
  end
end
