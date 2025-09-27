# M1 Implementation Plan: Adaptor Scaffolding + Local Expr/Pattern Hook-in

Based on the analysis of both Elixir's Module.Types and ElixirSense's type system, here's the implementation plan for M1:

## 1. Create ElixirSense.Core.ElixirTypes Adaptor Module

**Location:** `lib/elixir_sense/core/elixir_types.ex`

This adaptor will encapsulate all Module.Types interactions with:
- Stack/context initialization
- Expression/pattern typing wrappers
- Conversion helpers between Descr and ElixirSense shapes
- Version guards for compatibility

## 2. Core Integration Points

**Primary integration in ElixirSense.Core.Binding:**
- Hook into `expand/3` at line 65 to optionally call Module.Types for expression typing
- Add parallel type computation using Module.Types.Expr.of_expr/5
- Implement shape conversion to merge with existing type system

**Secondary integration in ElixirSense.Core.Compiler:**
- Hook into `do_expand/3` for pattern matching (lines 131-151)
- After TypeInference.find_typed_vars, also compute types via Module.Types.Pattern
- Merge results before State.merge_inferred_types

## 3. Implementation Steps

1. **Create adaptor module structure:**
   - `init_stack/4` - Create Module.Types stack with :dynamic mode by default
   - `init_context/0` - Create minimal Module.Types context
   - `type_of_expr/4` - Wrapper for Module.Types.Expr.of_expr with error handling
   - `type_of_pattern/5` - Wrapper for Module.Types.Pattern operations
   - `to_shape/1` - Convert Descr to ElixirSense shape format
   - `available?/0` - Check if Module.Types modules exist (version guard)

2. **Add configuration flag:**
   - Add `:use_elixir_types` config option (default false initially)
   - Allow gradual rollout and testing

3. **Implement local_handler:**
   - Create a simple local function handler that returns :none for now
   - Will be expanded in M2 to use ElixirSense's module metadata

4. **Wire minimal integration:**
   - In Binding.expand, after existing expansion, optionally call ElixirTypes adaptor
   - Use intersection logic to combine results (prefer more specific type)
   - Log/track usage for debugging

## 4. Shape Conversion Mapping

Initial conversions (Descr → ElixirSense):
- `%{atom: atoms}` → `{:atom, atom}` (for single atoms)
- `%{bitmap: @bit_integer}` → `:integer`
- `%{bitmap: @bit_binary}` → `:binary`
- `%{tuple: [{:closed, elems}]}` → `{:tuple, length, elems}`
- `%{map: fields}` → `{:map, fields, nil}`
- `%{list: ...}` → `{:list, type}`

## 5. Testing Strategy

- Unit tests for adaptor module
- Integration tests comparing ElixirSense types with Module.Types output
- Regression tests ensuring existing functionality unchanged when disabled
- Performance benchmarks

## 6. Success Criteria

- Adaptor module compiles and passes basic tests
- Can type simple expressions (literals, variables, basic operations)
- Shape conversion works for common types
- No regression in existing type inference when disabled
- Clear logging/debugging path for type computation

This incremental approach allows testing Module.Types integration without disrupting existing functionality, setting the foundation for M2-M4 phases.

## API Analysis Summary

### Elixir Module.Types Key APIs:
- `Module.Types.stack/7` - Creates typing stack with mode (:dynamic, :infer, :traversal)
- `Module.Types.context/0` - Creates initial context
- `Module.Types.Expr.of_expr/5` - Types expressions, returns `{descr, context}`
- `Module.Types.Pattern.of_head/7` - Types patterns with guards
- `Module.Types.Descr.*` - Set-theoretic type operations (union, intersection, etc)

### ElixirSense Integration Points:
- `ElixirSense.Core.Binding.expand/3` - Main type expansion entry point
- `ElixirSense.Core.TypeInference.find_typed_vars/3` - Variable type discovery
- `ElixirSense.Core.Compiler.do_expand/3` - AST expansion with type tracking
- Shape formats: `{:atom, val}`, `{:tuple, size, elems}`, `{:map, fields, updated}`, etc.