# Repository Guidelines

## Project Structure & Module Organization
- lib/: Elixir source (primary namespace: ElixirSense.*). Keep providers/utilities under lib/elixir_sense/providers/** and lib/elixir_sense/providers/utils/**.
- src/: Erlang sources (tokenizer/parser). Edit grammar in src/elixir_sense_parser.yrl; .erl files are generated.
- test/: ExUnit tests with support helpers in test/support/.
- config/: Mix/Runtime config.
- Tooling: .formatter.exs, .credo.exs, .dialyzer_ignore.exs.

## Build, Test, and Development Commands
- mix deps.get — install dependencies.
- mix compile — compile Elixir + yecc-generated parser.
- mix test — run the test suite.
- mix test --include requires_source — run tests that need an Elixir source install.
- mix coveralls — coverage via ExCoveralls.
- mix dialyzer — run static analysis (dialyxir).
- mix format — format codebase; use --check-formatted in CI/local pre-checks.
- mix docs — build docs (ExDoc) for local review.

## Coding Style & Naming Conventions
- Use mix format (2-space indentation, 100–120 cols per .credo/.formatter settings).
- Module names: PascalCase under ElixirSense.* (e.g., ElixirSense.Providers.Completion.Suggestion).
- Private helpers first or grouped near usage; prefer explicit @specs where meaningful.
- For Erlang in src/: keep snake_case modules and do not hand-edit generated .erl files.

## Testing Guidelines
- Framework: ExUnit. Place tests mirroring lib/ paths (e.g., lib/foo/bar.ex → test/foo/bar_test.exs).
- Prefer focused unit tests; add integration tests under test/elixir_sense/core/** when interacting with parser/typing.
- Aim to maintain or increase coverage; verify with mix coveralls.

## Commit & Pull Request Guidelines
- Commits: imperative mood, concise scope (e.g., providers/completion: handle records). Group mechanical changes (format/rename) separately.
- PRs: clear description, motivation, and approach; link issues; include tests and docs updates; note any breaking changes. Provide reproduction steps and before/after when fixing bugs.

## Notes: Parser/Erlang Sources
- Edit src/elixir_sense_parser.yrl and run mix compile to regenerate parser.
- If parser or tokenizer changes, add/update tests under test/elixir_sense/core/parser*_test.exs and tokenizer*_test.exs.

## Code search
Use ast-grep command line util when searching for complex code patterns.
