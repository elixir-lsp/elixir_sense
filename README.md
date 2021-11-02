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

## License (The MIT License)

Copyright (c) 2017 Marlus Saraiva

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the 'Software'), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED 'AS IS', WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
