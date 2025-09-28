M3 Implementation Plan - Next Steps                               │ │
│ │                                                                   │ │
│ │ Based on the TYPES_M3.md roadmap and current state analysis,      │ │
│ │ here's the plan for the next phase of Module.Types integration:   │ │
│ │                                                                   │ │
│ │ Phase 1: Struct Shape Standardization 🏗                          │ │
│ │                                                                   │ │
│ │ Priority: HIGH - Blocking proper type flow                        │ │
│ │                                                                   │ │
│ │ 1. Standardize struct shape format                                │ │
│ │ (lib/elixir_sense/core/elixir_types.ex:936)                       │ │
│ │   - Change from: {:struct, module, field_shapes}                  │ │
│ │   - To: {:struct, fields, {:atom, Module}, nil}                   │ │
│ │   - This aligns with Binding's expectations for seamless type     │ │
│ │ flow                                                              │ │
│ │   - Update extract_struct and related functions                   │ │
│ │   - Update any tests that expect the old format                   │ │
│ │                                                                   │ │
│ │ Phase 2: Pattern Refinement for Additional Constructs 🔄          │ │
│ │                                                                   │ │
│ │ Priority: HIGH - Major coverage gaps                              │ │
│ │                                                                   │ │
│ │ 2. Add refinement hooks for cond clauses                          │ │
│ │   - Mirror the case enhancement pattern                           │ │
│ │   - Extract and refine pattern variables in cond branches         │ │
│ │   - Location: lib/elixir_sense/core/compiler/clauses.ex           │ │
│ │ 3. Add refinement hooks for with clauses                          │ │
│ │   - Handle pattern matching in with expressions                   │ │
│ │   - Refine variables based on pattern matches in with clauses     │ │
│ │   - Consider both <- and = patterns                               │ │
│ │ 4. Add refinement hooks for try/rescue/catch                      │ │
│ │   - Handle pattern variables in rescue clauses                    │ │
│ │   - Refine exception types where possible                         │ │
│ │   - Conservative approach for catch-all patterns                  │ │
│ │ 5. Add refinement hooks for receive patterns                      │ │
│ │   - Similar to case handling but for message patterns             │ │
│ │   - Time-sensitive, so keep lightweight                           │ │
│ │ 6. Add refinement hooks for for comprehensions                    │ │
│ │   - Handle generator patterns (pattern <- enumerable)             │ │
│ │   - Refine loop variables based on enumerable type                │ │
│ │                                                                   │ │
│ │ Phase 3: Enhanced Merge Algorithms 🔀                             │ │
│ │                                                                   │ │
│ │ Priority: MEDIUM - Improves precision                             │ │
│ │                                                                   │ │
│ │ 7. Implement element-wise merge for tuples                        │ │
│ │ (lib/elixir_sense/core/elixir_types.ex:1027+)                     │ │
│ │   - When both tuples have same arity, merge element-by-element    │ │
│ │   - Preserve more type information during merges                  │ │
│ │   - Fall back to current "prefer more specific" for incompatible  │ │
│ │ arities                                                           │ │
│ │ 8. Implement field-wise merge for maps                            │ │
│ │   - Merge maps field-by-field when keys overlap                   │ │
│ │   - Keep unique fields from both sides                            │ │
│ │   - Preserve field type information                               │ │
│ │ 9. Improve list element type merging                              │ │
│ │   - When both lists have element types, find common type          │ │
│ │   - Prefer narrower/more specific element types                   │ │
│ │   - Keep :none absorption semantics                               │ │
│ │                                                                   │ │
│ │ Phase 4: Remote Signature Enhancement 🌐                          │ │
│ │                                                                   │ │
│ │ Priority: LOW - Nice to have for M3                               │ │
│ │                                                                   │ │
│ │ 10. Enhance remote handler to consider arguments                  │ │
│ │   - In remote_handler_from/1, use argument descriptors            │ │
│ │   - Match arguments to clause domains to select specific returns  │ │
│ │   - Instead of plain union of all clause returns                  │ │
│ │   - Location: lib/elixir_sense/core/elixir_types.ex:1255+         │ │
│ │                                                                   │ │
│ │ Phase 5: Performance & Configuration 🚀                           │ │
│ │                                                                   │ │
│ │ Priority: MEDIUM - Production readiness                           │ │
│ │                                                                   │ │
│ │ 11. Profile and optimize hot paths                                │ │
│ │   - Measure adaptor call costs                                    │ │
│ │   - Implement selective :traversal vs :dynamic mode switching     │ │
│ │   - Optimize cache usage for frequently accessed types            │ │
│ │   - Keep feature gated for safety                                 │ │
│ │ 12. Unify configuration                                           │ │
│ │   - Consolidate cache settings under :elixir_types_opts           │ │
│ │   - Add documentation for all configuration options               │ │
│ │   - Already have defaults in config/config.exs                    │ │
│ │                                                                   │ │
│ │ Phase 6: Testing & Documentation 📚                               │ │
│ │                                                                   │ │
│ │ Priority: ONGOING - Throughout implementation                     │ │
│ │                                                                   │ │
│ │ 13. Add comprehensive tests                                       │ │
│ │   - Test struct shape standardization                             │ │
│ │   - Test each new construct's pattern refinement                  │ │
│ │ (cond/with/try/receive/for)                                       │ │
│ │   - Test element-wise merge behaviors                             │ │
│ │   - Add integration tests showing improved type flow              │ │
│ │ 14. Update documentation                                          │ │
│ │   - Document configuration options                                │ │
│ │   - Add examples of supported patterns                            │ │
│ │   - Document conservative fallback behaviors                      │ │
│ │                                                                   │ │
│ │ Implementation Order                                              │ │
│ │                                                                   │ │
│ │ Week 1-2: Critical Foundation                                     │ │
│ │ 1. Struct shape standardization (blocking issue)                  │ │
│ │ 2. Cond and with pattern refinement (high impact)                 │ │
│ │ 3. Element-wise tuple merge (precision improvement)               │ │
│ │                                                                   │ │
│ │ Week 2-3: Coverage Expansion                                      │ │
│ │ 4. Try/rescue/catch refinement                                    │ │
│ │ 5. Receive pattern refinement                                     │ │
│ │ 6. For comprehension refinement                                   │ │
│ │ 7. Field-wise map merge                                           │ │
│ │                                                                   │ │
│ │ Week 3-4: Polish & Optimization                                   │ │
│ │ 8. List element merging improvements                              │ │
│ │ 9. Performance profiling and optimization                         │ │
│ │ 10. Remote signature enhancement (if time permits)                │ │
│ │ 11. Comprehensive test suite                                      │ │
│ │ 12. Documentation updates                                         │ │
│ │                                                                   │ │
│ │ Success Metrics                                                   │ │
│ │                                                                   │ │
│ │ - ✅ All struct shapes follow Binding's expected format            │ │
│ │ - ✅ Pattern refinement works for all major Elixir constructs      │ │
│ │ - ✅ Merge algorithms preserve maximum type information            │ │
│ │ - ✅ Performance impact < 10% on typical workloads                 │ │
│ │ - ✅ 90%+ test coverage for new functionality                      │ │
│ │ - ✅ Feature remains safely gated and non-breaking                 │ │
│ │                                                                   │ │
│ │ Notes                                                             │ │
│ │                                                                   │ │
│ │ - Local handler wiring and env threading are already complete ✓   │ │
│ │ - Pattern refinement TODOs for maps/tuples/lists are resolved ✓   │ │
│ │ - Config defaults already added ✓                                 │ │
│ │ - Test assertions already improved ✓                              │ │
│ │                                                                   │ │
│ │ This plan focuses on the remaining high-impact items from         │ │
│ │ TYPES_M3.md that will significantly improve Module.Types          │ │
│ │ integration coverage and precision.   
