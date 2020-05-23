defmodule ElixirSense.Providers.Suggestion.Reducer do
  @moduledoc false

  @type suggestion ::
          Reducers.Common.attribute
          | Reducers.Common.variable
          | Reducers.StructFields.field
          | Reducers.Returns.return
          | Reducers.Callbacks.callback
          | Reducers.ProtocolFunctions.protocol_function
          | Reducers.Common.func
          | Reducers.Common.mod
          | Reducers.ParamOptions.param_option
          | Reducers.TypeSpecs.type_spec

  @type acc :: %{result: [suggestion], reducers: [atom], context: map}

  @spec put_context(acc, key :: atom, value: any) :: acc
  def put_context(acc, key, value) do
    updated_context = Map.put(acc.context, key, value)
    put_in(acc.context, updated_context)
  end

  @spec get_context(acc, key :: atom) :: any
  def get_context(acc, key) do
    get_in(acc, [:context, key])
  end
end
