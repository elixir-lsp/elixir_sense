# ElixirSense

An API for Elixir projects that provides context-aware information for code completion, documentation, go/jump to definition, signature info and more.

## Usage

```
defp deps do
  [
    {:elixir_sense, github: "elixir-lsp/elixir_sense"},
  ]
end
```

## Testing

```
$ mix deps.get
$ mix test
```

A few of the tests require a source installation of Elixir which you can accomplish with [asdf](https://github.com/asdf-vm/asdf-elixir) (use `ref:v1.12.3`) or [kiex](https://github.com/taylor/kiex)

To run the tests that require a source installation of Elixir run:
```
mix test --include requires_source
```

For coverage:

```
mix coveralls
```

## Credits

- This project probably wouldn't even exist without all the work done by Samuel Tonini and all contributors from [alchemist-server](https://github.com/tonini/alchemist-server).
- The Expand feature was inspired by the [mex](https://github.com/mrluc/mex) tool by Luc Fueston. There's also a very nice post where he describes the whole process of [Building A Macro-Expansion Helper for IEx](http://blog.maketogether.com/building-a-macro-expansion-helper/).
- This project includes modified source code from Elixir project [IEx autocomplete](https://github.com/elixir-lang/elixir/tree/v1.9/lib/iex) Copyright (c) 2012 Plataformatec, which powers introspection and suggestions features
- This project includes modified source code from ExDoc project [ExDoc](https://github.com/elixir-lang/ex_doc) Copyright (c) 2012 Plataformatec, which powers documentation retrieval and formatting
