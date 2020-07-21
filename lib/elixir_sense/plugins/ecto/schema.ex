defmodule ElixirSense.Plugins.Ecto.Schema do
  @moduledoc false

  alias ElixirSense.Core.Introspection
  alias ElixirSense.Plugins.Option
  alias ElixirSense.Plugins.Util

  @options %{
    has_many: [
      %{
        name: :foreign_key,
        doc: """
        Sets the foreign key, this should map to a field on the
        other schema, defaults to the underscored name of the current module
        suffixed by `_id`.
        """
      },
      %{
        name: :references,
        doc: """
        Sets the key on the current schema to be used for the
        association, defaults to the primary key on the schema.
        """
      },
      %{
        name: :through,
        doc: """
        If this association must be defined in terms of existing
        associations. Read the section in `has_many/3` for more information.
        """
      },
      %{
        name: :on_delete,
        doc: """
        The action taken on associations when parent record
        is deleted. May be `:nothing` (default), `:nilify_all` and `:delete_all`.
        Using this option is DISCOURAGED for most relational databases. Instead,
        in your migration, set `references(:parent_id, on_delete: :delete_all)`.
        Opposite to the migration option, this option cannot guarantee integrity
        and it is only triggered for `c:Ecto.Repo.delete/2` (and not on
        `c:Ecto.Repo.delete_all/2`) and it never cascades. If posts has many comments,
        which has many tags, and you delete a post, only comments will be deleted.
        If your database does not support references, cascading can be manually
        implemented by using `Ecto.Multi` or `Ecto.Changeset.prepare_changes/2`.
        """
      },
      %{
        name: :on_replace,
        doc: """
        The action taken on associations when the record is
        replaced when casting or manipulating parent changeset. May be
        `:raise` (default), `:mark_as_invalid`, `:nilify`, `:update`, or
        `:delete`. See `Ecto.Changeset`'s section on related data for more info.
        """,
        # Based on Ecto's official docs at:
        # https://github.com/elixir-ecto/ecto/blob/v3.4.5/lib/ecto/changeset.ex#L140-L154
        values: [
          raise: """
          (default) - do not allow removing association or embedded
          data via parent changesets
          """,
          mark_as_invalid: """
          If attempting to remove the association or
          embedded data via parent changeset - an error will be added to the parent
          changeset, and it will be marked as invalid
          """,
          nilify: """
          Sets owner reference column to `nil` (available only for
          associations). Use this on a `belongs_to` column to allow the association
          to be cleared out so that it can be set to a new value. Will set `action`
          on associated changesets to `:replace`
          """,
          update: """
          Updates the association, available only for `has_one` and `belongs_to`.
          This option will update all the fields given to the changeset including the id
          for the association
          """,
          delete: """
          Removes the association or related data from the database.
          This option has to be used carefully (see below). Will set `action` on associated
          changesets to `:replace`
          """
        ]
      },
      %{
        name: :defaults,
        doc: """
        Default values to use when building the association.
        It may be a keyword list of options that override the association schema
        or a `{module, function, args}` that receive the struct and the owner as
        arguments. For example, if you set `Post.has_many :comments, defaults: [public: true]`,
        then when using `Ecto.build_assoc(post, :comments)` that comment will have
        `comment.public == true`. Alternatively, you can set it to
        `Post.has_many :comments, defaults: {__MODULE__, :update_comment, []}`
        and `Post.update_comment(comment, post)` will be invoked.
        """
      },
      %{
        name: :where,
        doc: """
        A filter for the association. See "Filtering associations"
        in `has_many/3`. It does not apply to `:through` associations.
        """
      }
    ]
  }

  def find_options(hint, fun) do
    @options[fun] |> Option.find(hint, fun)
  end

  def find_option_values(hint, option, fun) do
    for {value, doc} <- Enum.find(@options[fun], &(&1.name == option))[:values] || [],
        value_str = inspect(value),
        String.starts_with?(value_str, hint) do
      %{
        type: :generic,
        kind: :enum_member,
        label: value_str,
        insert_text: Util.insert_text(hint, value_str),
        detail: "#{inspect(option)} value",
        documentation: doc
      }
    end
  end

  def find_schemas(hint) do
    for {module, _} <- :code.all_loaded(),
        function_exported?(module, :__schema__, 1),
        mod_str = inspect(module),
        Util.match_module?(mod_str, hint) do
      {doc, _} = Introspection.get_module_docs_summary(module)

      %{
        type: :generic,
        kind: :class,
        label: mod_str,
        detail: "Ecto schema",
        documentation: doc
      }
    end
    |> Enum.sort_by(& &1.label)
  end
end
