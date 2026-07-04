defmodule ElixirSense.MixProject do
  @moduledoc false
  use Mix.Project

  def project do
    [
      app: :elixir_sense,
      version: "2.0.0",
      elixir: "~> 1.16",
      elixirc_paths: elixirc_paths(Mix.env()),
      build_embedded: Mix.env() == :prod,
      start_permanent: Mix.env() == :prod,
      prune_code_paths: Mix.env() == :prod,
      compilers: Mix.compilers(),
      dialyzer: [
        flags: [
          :unmatched_returns,
          :error_handling,
          :unknown,
          :underspecs,
          :extra_return,
          :missing_return
        ]
      ],
      deps: deps(),
      docs: docs(),
      description: description(),
      package: package()
    ]
  end

  def application do
    [extra_applications: [:logger]]
  end

  defp elixirc_paths(:test), do: ["lib", "test/support"]
  defp elixirc_paths(_), do: ["lib"]

  defp deps do
    [
      {:toxic2, github: "lukaszsamson/toxic2", ref: "c47c911dac5aafa860206d222a781c3d71afc843"},
      {:credo, "~> 1.7", only: [:dev], runtime: false},
      {:dialyxir, "~> 1.0", only: [:dev], runtime: false},
      {:ex_doc, "~> 0.18", only: [:dev], runtime: false}
    ]
  end

  defp docs do
    [
      main: "ElixirSense",
      nest_modules_by_prefix: [ElixirSense.Core, ElixirSense.Providers]
    ]
  end

  defp description do
    """
    An API for Elixir projects that provides context-aware information
    for code completion, documentation, go/jump to definition, signature info
    and more.
    """
  end

  defp package do
    [
      maintainers: [
        "Marlus Saraiva (@msaraiva)",
        "Łukasz Samson (@lukaszsamson)",
        "Jason Axelson (@axelson)"
      ],
      licenses: ["MIT"],
      links: %{"GitHub" => "https://github.com/elixir-lsp/elixir_sense"}
    ]
  end
end
