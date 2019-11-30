[
  inputs:
    Enum.flat_map(
      ["{mix,.formatter,.credo,run,run_test}.exs", "{config,lib,test}/**/*.{ex,exs}"],
      &Path.wildcard(&1, match_dot: true)
    ) -- ["test/support/modules_with_references.ex"]
]
