defmodule ElixirSense.Plugins.EctoTest do
  use ExUnit.Case
  import ExUnit.CaptureIO
  import TestHelper

  @ecto_types """
  :string,\
  :boolean,\
  :integer,\
  :float,\
  :decimal,\
  :id,\
  :date,\
  :time,\
  :time_usec,\
  :naive_datetime,\
  :naive_datetime_usec,\
  :utc_datetime,\
  :utc_datetime_usec,\
  {:array\\, inner_type},\
  :map,\
  {:map\\, inner_type},\
  :binary_id,\
  :binary\
  """

  describe "decorate" do
    test "update snippets to add type choices for Ecto.Schema.field/1..3" do
      buffer = """
      import Ecto.Schema
      fiel
      #   ^
      """

      [cursor] = cursors(buffer)

      result = suggestions(buffer, cursor)

      assert [
               %{name: "field", arity: 1, snippet: snippet1},
               %{name: "field", arity: 2, snippet: snippet2},
               %{name: "field", arity: 3, snippet: snippet3}
             ] = result

      assert snippet1 == "field :${1:name}"
      assert snippet2 == "field :${1:name}, ${2|#{@ecto_types}|}"
      assert snippet3 == "field :${1:name}, ${2|#{@ecto_types}|}, ${3:opts}"
    end

    test "update snippets to add type choices for Ecto.Migration.add/2..3" do
      buffer = """
      import Ecto.Migration
      ad
      # ^
      """

      [cursor] = cursors(buffer)

      result = suggestions(buffer, cursor)

      assert [
               %{name: "add", arity: 2, snippet: snippet2},
               %{name: "add", arity: 3, snippet: snippet3}
             ] = result

      assert snippet2 == "add :${1:column}, ${2|#{@ecto_types}|}"
      assert snippet3 == "add :${1:column}, ${2|#{@ecto_types}|}, ${3:opts}"
    end
  end

  describe "suggestions" do
    test "list clauses (macros of Ecto.Query with first argument as `query`" do
      buffer = """
      import Ecto.Query

      from(
        u in User,
        where: is_nil(u.id),
        s
      #  ^
      )
      """

      [cursor] = cursors(buffer)

      assert capture_io(:stderr, fn ->
               result = suggestions(buffer, cursor)
               send(self(), {:result, result})
             end) =~ "trailing commas are not allowed inside function/macro call arguments"

      assert_received {:result, result}

      detail = "(from clause) Ecto.Query"

      assert [
               %{
                 documentation: doc1,
                 label: "select",
                 detail: ^detail,
                 kind: :property,
                 insert_text: "select: "
               },
               %{documentation: doc2, label: "select_merge", detail: ^detail}
             ] = result

      assert doc1 == """
             A select query expression.

             ### Example

                 from(c in City, select: c) # returns the schema as a struct
                 from(c in City, select: {c.name, c.population})
                 from(c in City, select: [c.name, c.county])\
             """

      assert doc2 =~ "Mergeable select query expression."
    end

    test "list different available join types" do
      buffer = """
      import Ecto.Query

      from(
        u in User,
        where: is_nil(u.id),
        l
      #  ^
      )
      """

      [cursor] = cursors(buffer)

      assert capture_io(:stderr, fn ->
               result = suggestions(buffer, cursor)
               send(self(), {:result, result})
             end) =~ "trailing commas are not allowed inside function/macro call arguments"

      assert_received {:result, result}

      detail = "(from clause) Ecto.Query"

      assert [
               %{documentation: doc1, label: "left_join", detail: ^detail, kind: :property},
               %{documentation: doc2, label: "left_lateral_join", detail: ^detail}
             ] = result

      assert doc1 == "A left join query expression."
      assert doc2 =~ "A left lateral join query expression."
    end

    test "join options" do
      buffer = """
      import Ecto.Query

      from(
        u in User,
        where: is_nil(u.id),
        prefix: "pre",
      #  ^
        o
      #  ^
      )
      """

      [cursor_1, cursor_2] = cursors(buffer)

      assert capture_io(:stderr, fn ->
               results = {suggestions(buffer, cursor_1), suggestions(buffer, cursor_2)}
               send(self(), {:results, results})
             end) =~ "trailing commas are not allowed inside function/macro call arguments"

      assert_received {:results, {result_1, result_2}}

      assert [%{documentation: doc, label: "prefix", detail: detail, kind: kind}] = result_1
      assert kind == :property
      assert detail == "(from/join option) Ecto.Query"
      assert doc == "The prefix to be used for the from/join when issuing a database query."

      assert [%{documentation: doc, label: "on", detail: detail, kind: kind}] = result_2
      assert kind == :property
      assert detail == "(join option) Ecto.Query"
      assert doc == "A query expression or keyword list to filter the join."
    end

    test "list available bindings" do
      buffer = """
      import Ecto.Query
      alias ElixirSense.Plugins.Ecto.FakeSchemas.User, as: User

      from(
        u in User,
        join: m1 in Mod1,
        join: m2 in Mod2,
        left_join: a1 in assoc(u, :assoc1),
        inner_join: a2 in assoc(u, :assoc2),
        where: a2 in subquery(from(s in Sub, limit: 1)),
        where: u.id == m
      #        ^        ^
      """

      [cursor_1, cursor_2] = cursors(buffer)

      assert [
               %{label: "a1"},
               %{label: "a2"},
               %{label: "m1"},
               %{label: "m2"},
               %{label: "u", kind: :variable, detail: detail, documentation: doc}
             ] = suggestions(buffer, cursor_1, :generic)

      assert detail == "(query binding) ElixirSense.Plugins.Ecto.FakeSchemas.User"
      assert doc == "Fake User schema."

      assert [%{label: "m1"}, %{label: "m2"}] = suggestions(buffer, cursor_2, :generic)
    end

    test "list binding's fields" do
      buffer = """
      import Ecto.Query
      alias ElixirSense.Plugins.Ecto.FakeSchemas.Post
      alias ElixirSense.Plugins.Ecto.FakeSchemas.Comment

      def query() do
        from(
          p in Post,
          join: u in assoc(p, :user),
          left_join: c in Comment,
          select: {u.id, c.id, p.t, p.u}
          #          ^     ^      ^    ^
        )
      end
      """

      [cursor_1, cursor_2, cursor_3, cursor_4] = cursors(buffer)

      assert [
               %{label: "email", detail: "Ecto field", kind: :field},
               %{label: "id"},
               %{label: "name"}
             ] = suggestions(buffer, cursor_1)

      assert [%{label: "content"}, %{label: "date"}] = suggestions(buffer, cursor_2)

      assert [%{label: "text"}, %{label: "title"}] = suggestions(buffer, cursor_3)

      assert [%{label: "user_id", documentation: doc}] = suggestions(buffer, cursor_4)

      assert doc == """
             The `:user_id` field of `ElixirSense.Plugins.Ecto.FakeSchemas.Post`.

             * **Type:** `:id`
             * **Related:** `ElixirSense.Plugins.Ecto.FakeSchemas.User (:id)`
             """
    end

    test "list binding's fields even without any hint after `.`" do
      buffer = """
      import Ecto.Query
      alias ElixirSense.Plugins.Ecto.FakeSchemas.Post

      def query() do
        from(
          p in Post,
          join: c in assoc(p, :comments),
          select: {p., c.id}
          #          ^
        )
      end
      """

      [cursor] = cursors(buffer)

      assert [
               %{label: "date"},
               %{label: "id"},
               %{label: "text"},
               %{label: "title"},
               %{label: "user_id"}
             ] = suggestions(buffer, cursor)
    end

    test "list associations from assoc/2" do
      buffer = """
      import Ecto.Query
      alias ElixirSense.Plugins.Ecto.FakeSchemas.Post

      def query() do
        from(
          p in Post,
          join: c in assoc(p,
          #                  ^
        )
      end
      """

      [cursor] = cursors(buffer)

      assert [
               %{
                 label: ":user",
                 detail: detail,
                 documentation: doc,
                 kind: :field,
                 type: :generic
               },
               %{label: ":comments"}
             ] = suggestions(buffer, cursor)

      assert doc == "Fake User schema."
      assert detail == "(Ecto association) ElixirSense.Plugins.Ecto.FakeSchemas.User"
    end

    test "list bindings and binding fields inside nested functions" do
      buffer = """
      import Ecto.Query
      alias ElixirSense.Plugins.Ecto.FakeSchemas.Post

      def query() do
        from(
          p in Post,
          where: is_nil(p.t
          #             ^  ^
        )
      end
      """

      [cursor_1, cursor_2] = cursors(buffer)

      assert [%{label: "p"} | _] = suggestions(buffer, cursor_1)
      assert [%{label: "text"}, %{label: "title"}] = suggestions(buffer, cursor_2)
    end

    test "from/2 without parens" do
      buffer = """
      import Ecto.Query
      alias ElixirSense.Plugins.Ecto.FakeSchemas.Post

      def query() do
        from p in Post, se
          #               ^
      end
      """

      [cursor] = cursors(buffer)

      assert [%{label: "select"}, %{label: "select_merge"}] = suggestions(buffer, cursor)

      buffer = """
      import Ecto.Query
      alias ElixirSense.Plugins.Ecto.FakeSchemas.Post

      def query() do
        from p in Post, where: p.id
          #                       ^
      end
      """

      [cursor] = cursors(buffer)

      assert [%{label: "id"}] = suggestions(buffer, cursor)

      buffer = """
      import Ecto.Query
      alias ElixirSense.Plugins.Ecto.FakeSchemas.Post

      def query() do
        from p in Post,
          join: u in User,
          se
          # ^
      end
      """

      [cursor] = cursors(buffer)

      assert [%{label: "select"}, %{label: "select_merge"}] = suggestions(buffer, cursor)

      buffer = """
      import Ecto.Query
      alias ElixirSense.Plugins.Ecto.FakeSchemas.Post

      def query() do
        from p in Post,
          join: u in User,

        # ^
      end
      """

      [cursor] = cursors(buffer)

      assert [%{detail: "(from clause) Ecto.Query"} | _] = suggestions(buffer, cursor)
    end
  end
end
