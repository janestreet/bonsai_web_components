# Bonsai Codemirror

## Codebase Organization

- `lib/bindings` contains `gen_js_api`-generated bindings to the Codemirror 6 API.
- `lib` re-exports `lib/bindings`, but also bundles in the Codemirror source code.
- `src` provides a Bonsai widget wrapper for creating Codemirror instances as Bonsai
  UI components.
