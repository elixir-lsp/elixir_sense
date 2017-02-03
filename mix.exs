defmodule ElixirSense.Mixfile do
  use Mix.Project

  def project do
    [app: :elixir_sense,
     version: "0.1.0",
     elixir: "~> 1.2",
     build_embedded: Mix.env == :prod,
     start_permanent: Mix.env == :prod,
     test_coverage: [tool: ExCoveralls],
     preferred_cli_env: ["coveralls": :test, "coveralls.detail": :test, "coveralls.html": :test],
     dialyzer: [
       flags: ["-Wunmatched_returns", "-Werror_handling", "-Wrace_conditions", "-Wunderspecs", "-Wno_match"]
     ],
     deps: deps(),
     docs: docs()
   ]
  end

  def application do
    [applications: [:logger]]
  end

  defp deps do
    [{:excoveralls, "~> 0.6", only: :test},
    {:dialyxir, "~> 0.4", only: [:dev]},
    {:ex_doc, "~> 0.14", only: [:dev]}]
  end

  defp docs do
    [main: "ElixirSense"]
  end

end
