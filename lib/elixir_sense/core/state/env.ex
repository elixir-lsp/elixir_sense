defmodule ElixirSense.Core.State.Env do
  @moduledoc """
  Line environment
  """

  @type t :: %ElixirSense.Core.State.Env{
          functions: [{module, [{atom, arity}]}],
          macros: [{module, [{atom, arity}]}],
          requires: list(module),
          aliases: list(ElixirSense.Core.Compiler.State.alias_t()),
          macro_aliases: [{module, {term, module}}],
          context: nil | :match | :guard,
          module: nil | module,
          function: nil | {atom, arity},
          protocol: nil | ElixirSense.Core.Compiler.State.protocol_t(),
          versioned_vars: %{optional({atom, atom}) => non_neg_integer},
          vars: list(ElixirSense.Core.State.VarInfo.t()),
          attributes: list(ElixirSense.Core.State.AttributeInfo.t()),
          behaviours: list(module),
          context_modules: list(module),
          typespec: nil | {atom, arity},
          scope_id: nil | ElixirSense.Core.Compiler.State.scope_id_t()
        }
  defstruct functions: [],
            macros: [],
            requires: [],
            aliases: [],
            macro_aliases: [],
            # NOTE for protocol implementation this will be the first variant
            module: nil,
            function: nil,
            # NOTE for protocol implementation this will be the first variant
            protocol: nil,
            versioned_vars: %{},
            vars: [],
            attributes: [],
            behaviours: [],
            context_modules: [],
            context: nil,
            typespec: nil,
            scope_id: nil

  def to_macro_env(%__MODULE__{} = env, file \\ "nofile", line \\ 1) do
    # we omit lexical_tracker and tracers
    %Macro.Env{
      line: line,
      file: file,
      context: env.context,
      module: env.module,
      function: env.function,
      context_modules: env.context_modules,
      macros: env.macros,
      functions: env.functions,
      requires: env.requires,
      aliases: env.aliases,
      macro_aliases: env.macro_aliases,
      versioned_vars: env.versioned_vars
    }
  end

  def update_from_macro_env(%__MODULE__{} = env, macro_env = %Macro.Env{}) do
    # we omit lexical_tracker and tracers
    %__MODULE__{
      env
      | context: macro_env.context,
        module: macro_env.module,
        function: macro_env.function,
        context_modules: macro_env.context_modules,
        macros: macro_env.macros,
        functions: macro_env.functions,
        requires: macro_env.requires,
        aliases: macro_env.aliases,
        macro_aliases: macro_env.macro_aliases,
        versioned_vars: macro_env.versioned_vars
    }
  end
end
