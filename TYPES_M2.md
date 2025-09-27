# M2 Implementation Plan: Local Functions & Remote Signatures

## Overview

M2 builds upon the M1 scaffolding to add real type inference capabilities:
1. **Local function typing** via ElixirSense's module metadata
2. **Remote function typing** via ExCk BEAM chunk reading
3. **Enhanced shape conversion** for more complex types
4. **Pattern matching refinement** (partial implementation)

## Success Criteria

- [ ] Local function calls return typed results from module metadata
- [ ] Remote function calls return types from ExCk chunks
- [ ] Shape conversion handles 80%+ of common type patterns
- [ ] Pattern matching refines variable types in simple cases
- [ ] No performance regression in normal usage
- [ ] All M1 tests continue passing

## Implementation Tasks

### 1. Local Function Handler (~3-4 hours)

**File:** `lib/elixir_sense/core/elixir_types.ex`

**Task:** Implement `local_handler/4` to use ElixirSense's metadata

```elixir
def local_handler(meta, {fun, arity}, stack, context) do
  # Extract module from stack
  module = Module.Types.stack_module(stack)

  # Look up function in ElixirSense metadata
  case lookup_local_function(module, fun, arity, meta) do
    {:ok, type_info} ->
      # Convert ElixirSense type to Module.Types descriptor
      convert_to_descriptor(type_info)

    :error ->
      false  # Let Module.Types handle it
  end
end
```

**Implementation details:**
- Use `State.ModFunInfo` to find function specs/types
- Convert from ElixirSense shapes to Module.Types descriptors
- Cache conversions for performance
- Handle recursive functions gracefully

**Test cases:**
- Local function with @spec
- Local function with inferred type
- Private function
- Macro
- Undefined function (fallback)

### 2. ExCk BEAM Chunk Reader (~4-5 hours)

**New file:** `lib/elixir_sense/core/exck_reader.ex`

```elixir
defmodule ElixirSense.Core.ExCkReader do
  @moduledoc """
  Reads ExCk chunks from BEAM files for remote type signatures.
  """

  def read_chunk(module) when is_atom(module) do
    # Get BEAM file path
    # Read ExCk chunk
    # Parse and cache signatures
  end

  def lookup_signature(module, function, arity) do
    # Return cached signature or read on demand
  end
end
```

**Implementation details:**
- Use `:beam_lib` or `:code.get_object_code/1`
- Parse ExCk format (ETF encoded)
- Cache per module with TTL
- Handle missing chunks gracefully
- Support both .beam files and in-memory modules

**Test cases:**
- Read ExCk from stdlib module (e.g., `Enum`)
- Module without ExCk chunk
- Invalid/corrupted chunk
- Performance with large modules
- Cache invalidation

### 3. Remote Function Integration (~2-3 hours)

**File:** `lib/elixir_sense/core/elixir_types.ex`

**Task:** Add remote function support to `of_expr/5`

```elixir
def remote_handler(module, function, arity, _meta, stack, context) do
  case ExCkReader.lookup_signature(module, function, arity) do
    {:ok, signature} ->
      # Apply signature to arguments
      apply_remote_signature(signature, stack, context)

    :error ->
      # Fall back to ElixirSense's type info
      lookup_remote_in_metadata(module, function, arity)
  end
end
```

**Integration points:**
- Hook into Module.Types remote call handling
- Merge with ElixirSense's existing remote type info
- Handle standard library functions
- Support user-defined modules

**Test cases:**
- `Enum.map/2` with typed list
- `String.length/1` with binary
- User module with ExCk
- Module without ExCk (fallback)
- Cross-module type propagation

### 4. Enhanced Shape Conversion (~3-4 hours)

**File:** `lib/elixir_sense/core/elixir_types.ex`

**Task:** Extend `to_shape/1` for more types

```elixir
def to_shape(descr) do
  cond do
    # ... existing simple types ...

    # Unions
    union_types = extract_union(descr) ->
      {:union, Enum.map(union_types, &to_shape/1)}

    # Structs
    struct_info = extract_struct(descr) ->
      {:struct, struct_info.module, convert_struct_fields(struct_info)}

    # PIDs, ports, refs
    Module.Types.Descr.equal?(descr, Module.Types.Descr.pid()) ->
      :pid

    # Functions
    function_info = extract_function(descr) ->
      {:fun, function_info.arity}

    # Bounded integers/floats
    integer_range = extract_integer_range(descr) ->
      {:integer, integer_range}

    # String literals (binaries with known value)
    string_literal = extract_string_literal(descr) ->
      {:binary, string_literal}
  end
end
```

**New extractors needed:**
- `extract_union/1` - Handle type unions
- `extract_struct/1` - Detect and parse struct types
- `extract_function/1` - Function type info
- `extract_integer_range/1` - Bounded integers
- `extract_string_literal/1` - String literals

**Test cases:**
- Union types: `{:ok, term} | {:error, String.t}`
- Structs: `%User{name: String.t, age: integer}`
- Function types: `(integer, integer -> integer)`
- Bounded types: `1..10`, `0..255`
- Complex nested: `list(%{String.t => atom | integer})`

### 5. Pattern Matching Refinement (~4-5 hours)

**File:** `lib/elixir_sense/core/elixir_types.ex`

**Task:** Implement `of_match/7` for basic patterns

```elixir
def of_match(pattern, expected_descr, expr_ast, module, function, file, mode) do
  if available?() do
    try do
      stack = init_stack(module, function, file, mode)
      context = init_context()

      # Use Module.Types.Pattern.of_match
      {vars, context} = Module.Types.Pattern.of_match(
        pattern,
        expected_descr,
        expr_ast,
        %{},
        stack,
        context
      )

      {:ok, convert_pattern_vars(vars)}
    rescue
      _ -> :error
    end
  else
    :error
  end
end
```

**File:** `lib/elixir_sense/core/compiler.ex`

**Task:** Implement `merge_elixir_types_pattern_vars/3`

```elixir
defp merge_elixir_types_pattern_vars(pattern, value_type, vars) do
  case ElixirTypes.of_match(pattern, value_type, pattern) do
    {:ok, refined_vars} ->
      merge_refined_vars(vars, refined_vars)
    :error ->
      vars
  end
end
```

**Supported patterns (M2):**
- Literals: `42`, `:atom`, `"string"`
- Variables: `x`, `_ignored`
- Tuples: `{a, b}`
- Lists: `[h | t]`, `[a, b, c]`
- Maps: `%{key: value}`

**Test cases:**
- `case x do 42 -> ... end` (refines x to integer)
- `{:ok, value} = result` (refines value based on result)
- `[h | _] = list` (refines h to list element type)
- `%{name: n} = map` (refines n to map field type)
- Guards: `when is_binary(x)` (refines x to binary)

### 6. Performance Optimization (~2-3 hours)

**Tasks:**
- Add caching layer for ExCk lookups
- Implement lazy descriptor conversion
- Add :traversal mode support for better performance
- Profile and optimize hot paths

**Cache design:**
```elixir
defmodule ElixirSense.Core.TypeCache do
  use GenServer

  # ETS tables:
  # - :exck_cache - {module, chunk_data, timestamp}
  # - :shape_cache - {descr_hash, shape}
  # - :remote_cache - {{mod, fun, arity}, signature}
end
```

**Metrics to track:**
- Cache hit rate
- Conversion time
- Memory usage
- Request latency impact

### 7. Integration Testing (~2-3 hours)

**New test file:** `test/elixir_sense/core/elixir_types_m2_test.exs`

**Test scenarios:**
- End-to-end typing of real code snippets
- Cross-module type propagation
- Pattern matching refinement in action
- Performance benchmarks
- Compatibility with existing ElixirSense features

## Implementation Order

1. **Week 1:**
   - ExCk reader implementation
   - Basic remote function support
   - Initial tests

2. **Week 2:**
   - Local function handler
   - Enhanced shape conversion
   - Integration with TypeInference

3. **Week 3:**
   - Pattern matching (basic cases)
   - Performance optimization
   - Comprehensive testing

## Risk Mitigation

### Risk 1: ExCk Format Changes
- **Mitigation:** Version detection and format adapters
- **Fallback:** Use ElixirSense's existing type data

### Risk 2: Performance Impact
- **Mitigation:** Aggressive caching, lazy evaluation
- **Fallback:** Feature flag for disabling expensive operations

### Risk 3: Module.Types API Changes
- **Mitigation:** Version guards, compatibility layer
- **Fallback:** Maintain working M1 version as baseline

## Testing Strategy

### Unit Tests
- Each new function gets dedicated tests
- Mock Module.Types when testing integration points
- Property-based tests for shape conversion

### Integration Tests
- Real Elixir modules with ExCk chunks
- Complex type inference scenarios
- Performance regression tests

### Compatibility Tests
- Run full ElixirSense test suite with feature enabled
- Verify no degradation in existing functionality
- Test with multiple Elixir versions

## Documentation Updates

### Code Documentation
- Add @moduledoc for ExCkReader
- Document cache behavior and TTLs
- Add examples for each public function

### User Documentation
- Update CLAUDE.md with M2 features
- Add configuration options for caching
- Document limitations and known issues

## Configuration Options

New configuration keys:
```elixir
config :elixir_sense,
  use_elixir_types: true,
  elixir_types_opts: [
    cache_ttl: 300_000,  # 5 minutes
    max_cache_size: 100, # modules
    enable_remote: true,
    enable_patterns: true
  ]
```

## Definition of Done

- [ ] All unit tests passing
- [ ] Integration tests passing
- [ ] No performance regression (< 5% impact)
- [ ] Documentation complete
- [ ] Code review completed
- [ ] Manual testing with real editor

## Estimated Timeline

- **Total effort:** 20-25 hours
- **Calendar time:** 2-3 weeks (part-time)
- **Review cycles:** 2-3 iterations

## Dependencies

- Elixir 1.17+ with Module.Types.Pattern support
- ElixirSense M1 implementation complete
- Access to BEAM files for ExCk reading

## Open Questions

1. Should we cache ExCk data on disk or only in memory?
2. How to handle modules compiled without +ExCk?
3. Should pattern matching be opt-in initially?
4. What's the cache eviction strategy for large projects?

## Future Considerations (M3+)

- Gradual typing integration
- Type checking mode (not just inference)
- Integration with Dialyzer specs
- Custom type annotations
- IDE-specific optimizations