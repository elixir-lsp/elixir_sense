defmodule ElixirSense.Core.BuiltinAttributes do
  @moduledoc false
  @list ~w(
    optional_callbacks
    behaviour
    impl
    derive
    enforce_keys
    compile
    deprecated
    dialyzer
    file
    external_resource
    on_load
    on_definition
    vsn
    after_compile
    after_verify
    before_compile
    fallback_to_any
    type
    typep
    opaque
    spec
    callback
    macrocallback
    typedoc
    doc
    moduledoc
    for
    protocol
    nifs
  )a

  def all, do: @list

  def docs(attribute) when attribute in @list do
    case Module.reserved_attributes() do
      %{^attribute => %{doc: doc}} ->
        doc

      _ ->
        # Older Elixir versions haven't document these attributes
        # Reference: https://github.com/elixir-lang/elixir/commit/14c55d15afbf08b0d8289a4399a15b4109b6ac5a
        case attribute do
          :enforce_keys ->
            "Ensures the given keys are always set when building the struct defined in the current module."

          :fallback_to_any ->
            "If set to `true` generates a default protocol implementation " <>
              "for all types (inside `defprotocol`)."

          :for ->
            "The current module/type a protocol implementation is being defined for (inside `defimpl`)."

          :protocol ->
            "The current protocol being implemented (inside `defimpl`)."

          _ ->
            nil
        end
    end
  end

  def docs(_), do: nil
end
