# Unison AI Agent Guide

This guide provides context and instructions for AI agents when using the Unison MCP server.

## When to load additional context

**Complex tasks?** -> Load `./instructions.md`
**Tests?** -> Load `./testing.md`
**Detailed syntax?** -> Load `./unison-language-guide.md`

## Overview

Unison is a statically-typed functional language with **content-addressed code storage**. Unlike traditional languages, Unison code is NOT stored in files on the filesystem - it lives in a codebase managed by the UCM (Unison Codebase Manager). Code is written in ephemeral files (with the `.u` extension) and then typechecked and added to the codebase.

**This fundamentally changes how you interact with code.** File operations like `cat`, `grep`, `ls`, or `find` will NOT work for code discovery.

A Unison codebase does not use Git. Unison has its own version control system. Do not assume files or external sources reflect the current codebase.

You **must** use the Unison MCP server tools to interact with a Unison codebase.

## Unison project structure 

Unison code is organized into **namespaces** within projects and branches. Namespaces are delimited by periods (`.`). For example, `api.models`.

The `lib` namespace is reserved for dependencies. DO NOT modify code in the `lib` namespace via any MCP tools. 

## Development Workflow

1. Write code in scratch file (`.u` extension)
2. Add tests and docs if requested 
3. `typecheck-code` with the UCM MCP server
4. `run-tests` with the UCM MCP server

Always check that your code typechecks and tests pass. 

### Naming

- Namespace prefixes may be used to group related code, e.g. `api.models`, `api.controllers`, versus `userApiModel`. 
- `Optional` uses `None`/`Some` (not `Nothing`/`Just`)
- Strings are called `Text`
- Helper functions: use `go` or `loop`
- Short variable names: `acc`, `rem`, `f`, `g`

### Basic Style

- Prefer `cases` over `match ... with` when applicable
- Write tail-recursive functions with accumulators
- Use ability polymorphism: `(a ->{g} b)` not `(a -> b)`
- Use implicit quantification, do not use explicit `forall`

## Testing Unison code

1. Write tests in the same `.u` files as the code being tested.
2. The tests must follow the naming convention: for a function called `foo`, the test is `foo.tests.<test-name>`. 
3. Use the `run-tests` MCP server tool to run tests.
