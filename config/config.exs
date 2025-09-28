import Config

config :elixir_sense,
  logging_enabled: true,
  use_elixir_types: false,
  exck_cache_ttl: 300_000

import_config "#{Mix.env()}.exs"
