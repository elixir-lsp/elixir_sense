import Config

config :elixir_sense,
  logging_enabled: true

import_config "#{Mix.env()}.exs"
