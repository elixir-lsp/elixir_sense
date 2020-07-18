defmodule ElixirSense.Plugins.Ecto.Schema do
  @moduledoc false

  alias ElixirSense.Core.Introspection

  @has_many_options [
    %{
      name: "foreign_key",
      doc: """
      Sets the foreign key, this should map to a field on the
      other schema, defaults to the underscored name of the current module
      suffixed by `_id`.
      """
    },
    %{
      name: "references",
      doc: """
      Sets the key on the current schema to be used for the
      association, defaults to the primary key on the schema.
      """
    },
    %{
      name: "through",
      doc: """
      If this association must be defined in terms of existing
      associations. Read the section in `has_many/3` for more information.
      """
    },
    %{
      name: "on_delete",
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
      name: "on_replace",
      doc: """
      The action taken on associations when the record is
      replaced when casting or manipulating parent changeset. May be
      `:raise` (default), `:mark_as_invalid`, `:nilify`, `:update`, or
      `:delete`. See `Ecto.Changeset`'s section on related data for more info.
      """,
      snippet: "on_replace: ${1|:raise,:mark_as_invalid,:nilify,:update,:delete|}"
    },
    %{
      name: "defaults",
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
      name: "where",
      doc: """
      A filter for the association. See "Filtering associations"
      in `has_many/3`. It does not apply to `:through` associations.
      """
    }
  ]

  def find_has_many_options(hint) do
    @has_many_options |> find_options(hint, :has_many)
  end

  def find_schemas(hint) do
    for {module, _} <- :code.all_loaded(),
        function_exported?(module, :__schema__, 1),
        mod_str = inspect(module),
        match_module?(mod_str, hint) do
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

  defp find_options(options, hint, fun) do
    for option <- options, String.starts_with?(option.name, hint) do
      option_to_suggestion(option.name, option[:doc], option[:snippet], fun)
    end
    |> Enum.sort_by(& &1.label)
  end

  defp option_to_suggestion(name, doc, snippet, fun) do
    %{
      type: :generic,
      kind: :property,
      label: name,
      insert_text: "#{name}: ",
      snippet: snippet,
      detail: "#{fun} option",
      documentation: doc
    }
  end

  defp match_module?(mod_str, hint) do
    hint = String.downcase(hint)
    mod_full = String.downcase(mod_str)
    mod_last = mod_full |> String.split(".") |> List.last()
    Enum.any?([mod_last, mod_full], &String.starts_with?(&1, hint))
  end
end
