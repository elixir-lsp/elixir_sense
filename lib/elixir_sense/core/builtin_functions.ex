defmodule ElixirSense.Core.BuiltinFunctions do
  @functions %{
    {:exception, 1} => %{specs: [quote(do: exception(term) :: Exception.t())], args: ["msg"]},
    {:message, 1} => %{
      specs: [quote(do: message(Exception.t()) :: String.t())],
      args: ["exception"]
    },
    {:__struct__, 0} => %{
      specs: [quote(do: __struct__() :: %{required(:__struct__) => module, optional(any) => any})],
      args: []
    },
    {:__struct__, 1} => %{
      specs: [
        quote(do: __struct__(keyword) :: %{required(:__struct__) => module, optional(any) => any})
      ],
      args: ["kv"]
    },
    {:impl_for, 1} => %{specs: [quote(do: impl_for(term) :: atom | nil)], args: ["data"]},
    {:impl_for!, 1} => %{specs: [quote(do: impl_for!(term) :: atom)], args: ["data"]},
    {:__protocol__, 1} => %{
      specs: [
        quote(do: __protocol__(:module) :: module),
        quote(do: __protocol__(:functions) :: [{atom, non_neg_integer}]),
        quote(do: __protocol__(:consolidated?) :: boolean),
        quote(do: __protocol__(:impls) :: :not_consolidated | {:consolidated, [module]})
      ],
      args: ["atom"]
    },
    {:__impl__, 1} => %{
      specs: [quote(do: __impl__(:for | :target | :protocol) :: module)],
      args: ["atom"]
    },
    {:__info__, 1} => %{
      specs: [
        quote(do: __info__(:attributes) :: keyword()),
        quote(do: __info__(:compile) :: [term()]),
        quote(do: __info__(:functions) :: [{atom, non_neg_integer}]),
        quote(do: __info__(:macros) :: [{atom, non_neg_integer}]),
        quote(do: __info__(:md5) :: binary()),
        quote(do: __info__(:module) :: module())
      ],
      args: ["atom"]
    },
    {:module_info, 0} => %{
      specs: [
        quote(
          do:
            module_info :: [{:module | :attributes | :compile | :exports | :md5 | :native, term}]
        )
      ],
      args: []
    },
    {:module_info, 1} => %{
      specs: [
        quote(do: module_info(:module) :: atom),
        quote(do: module_info(:attributes | :compile) :: [{atom, term}]),
        quote(do: module_info(:md5) :: binary),
        quote(do: module_info(:exports | :functions | :nifs) :: [{atom, non_neg_integer}]),
        quote(do: module_info(:native) :: boolean)
      ],
      args: ["key"]
    },
    {:behaviour_info, 1} => %{
      specs: [
        quote(do: behaviour_info(:callbacks | :optional_callbacks) :: [{atom, non_neg_integer}])
      ],
      args: ["key"]
    }
  }

  @all Map.keys(@functions)

  for {{f, a}, %{specs: specs}} <- @functions do
    def get_specs({unquote(f), unquote(a)}),
      do: unquote(specs |> Enum.map(&"@spec #{Macro.to_string(&1)}"))
  end

  for {{f, a}, %{args: args}} <- @functions do
    def get_args({unquote(f), unquote(a)}),
      do: unquote(args)
  end

  def all, do: @all
end
