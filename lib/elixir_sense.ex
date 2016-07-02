defmodule ElixirSense do

  alias ElixirSense.Core.State
  alias ElixirSense.Core.Metadata
  alias ElixirSense.Core.Parser
  alias ElixirSense.Providers.Docs
  alias ElixirSense.Providers.Definition

  @spec docs(String.t, String.t, pos_integer) :: String.t
  def docs(expr, buffer, line) do
    metadata = Parser.parse_string(buffer, true, true, line)
    %State.Env{
      imports: imports,
      aliases: aliases
    } = Metadata.get_env(metadata, line)

    Docs.all(expr, imports, aliases)
  end

  @spec find_definition(module, atom, String.t, pos_integer) :: Definition.location
  def find_definition(mod, fun, buffer, line) do
    buffer_file_metadata = Parser.parse_string(buffer, true, true, line)
    %State.Env{
      imports: imports,
      aliases: aliases,
      module: module
    } = Metadata.get_env(buffer_file_metadata, line)

    Definition.find(mod, fun, [module|imports], aliases)
  end

end
