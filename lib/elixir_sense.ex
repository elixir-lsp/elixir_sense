defmodule ElixirSense do

  alias ElixirSense.Core.State
  alias ElixirSense.Core.Metadata
  alias ElixirSense.Core.Parser
  alias ElixirSense.Providers.Docs
  alias ElixirSense.Providers.Definition
  alias ElixirSense.Providers.Suggestion
  alias ElixirSense.Providers.Signature

  @spec docs(String.t, String.t, pos_integer) :: String.t
  def docs(expr, code, line) do
    metadata = Parser.parse_string(code, true, true, line)
    %State.Env{
      imports: imports,
      aliases: aliases
    } = Metadata.get_env(metadata, line)

    Docs.all(expr, imports, aliases)
  end

  @spec find_definition(module, atom, String.t, pos_integer) :: Definition.location
  def find_definition(mod, fun, code, line) do
    buffer_file_metadata = Parser.parse_string(code, true, true, line)
    %State.Env{
      imports: imports,
      aliases: aliases,
      module: module
    } = Metadata.get_env(buffer_file_metadata, line)

    Definition.find(mod, fun, [module|imports], aliases)
  end

  @spec suggestions(String.t, String.t, non_neg_integer) :: [Suggestion.suggestion]
  def suggestions(hint, code, line) do
    buffer_file_metadata = Parser.parse_string(code, true, true, line)
    %State.Env{
      imports: imports,
      aliases: aliases,
      vars: vars,
      attributes: attributes,
      behaviours: behaviours,
      module: module,
      scope: scope
    } = Metadata.get_env(buffer_file_metadata, line)

    Suggestion.find(hint, [module|imports], aliases, vars, attributes, behaviours, scope)
  end

  @spec find_signature(String.t, String.t, pos_integer) :: Signature.signature_info
  def find_signature(prefix, code, line) do
    buffer_file_metadata = Parser.parse_string(code, true, true, line)
    %State.Env{
      imports: imports,
      aliases: aliases,
    } = Metadata.get_env(buffer_file_metadata, line)

    Signature.find(prefix, imports, aliases)
  end

end
