defmodule ElixirSense.Mixfile do
  @moduledoc false
  use Mix.Project

  def project do
    [
      app: :elixir_sense,
      version: "1.0.1",
      elixir: "~> 1.6",
      elixirc_paths: elixirc_paths(Mix.env()),
      build_embedded: Mix.env() == :prod,
      start_permanent: Mix.env() == :prod,
      test_coverage: [tool: ExCoveralls],
      preferred_cli_env: [coveralls: :test, "coveralls.detail": :test, "coveralls.html": :test],
      dialyzer: [
        flags: [:unmatched_returns, :error_handling, :race_conditions, :unknown, :underspecs]
      ],
      deps: deps(),
      docs: docs(),
      description: description(),
      package: package()
    ]
  end

  def application do
    [applications: [:logger]]
  end

  defp elixirc_paths(:test), do: ["lib", "test/support"]
  defp elixirc_paths(_), do: ["lib"]

  defp deps do
    [
      {:excoveralls, "~> 0.6", only: :test},
      {:dialyxir, "~> 1.0.0-rc.4", only: [:dev], runtime: false},
      {:credo, "~> 1.0", only: [:dev]},
      {:ex_doc, "~> 0.14", only: [:dev]}
    ]
  end

  defp docs do
    [
      main: "ElixirSense",
      nest_modules_by_prefix: [ElixirSense.Core, ElixirSense.Providers, ElixirSense.Server]
    ]
  end

  defp description do
    """
    An API/Server for Elixir projects that provides context-aware information
    for code completion, documentation, go/jump to definition, signature info
    and more.
    """
  end

  defp package do
    [
      maintainers: ["Marlus Saraiva (@msaraiva)"],
      licenses: ["MIT"],
      links: %{"GitHub" => "https://github.com/elixir-lsp/elixir_sense"}
    ]
  end
end
