defmodule ElixirSense.Core.BuiltinAttributes do
  @moduledoc false
  @list ~w(
    optional_callbacks
    behaviour
    impl
    derive
    enforce_keys
    struct
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
  )a

  def all, do: @list
end
