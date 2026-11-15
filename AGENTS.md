# lutil contributor instructions

Use the assigned dedicated source worktree for source edits; the owner checkout
and canonical planning worktree have separate owners. CDC owns plans, ledgers,
verification and acceptance. Preserve other checkout and repository ownership.
Release refs, default settings and publication state may change only through
the separately approved release workflow.

The accepted LFE style and reusable-library guidance is pinned at lfe-manual
`af0de7ff8c66da25cc43b8e0732ab5b897ebf17a`. Build through the declared
Rebar3 configuration and Make targets. `make check-native` is the native
compile/test gate; `make xref` and `make dialyzer` are separate diagnostics.
`make docs` writes the managed site under `_build/default/ldoc/current/site`;
explicit output must be fresh. Publish targets stage docs under `_build` and
remove only their owned stage. Never treat a successful partial docs render as
complete API documentation; comments without supported native doc carriers do
not count as rendered entries.

Keep runtime functions, exports, headers and test bodies unchanged unless a
separately approved source slice authorizes those files. Keep runtime
applications separate from test and author plugins. Do not hide native command
failures or delete `rebar.lock` during checks. Do not guess provider options.
Before any provider side effect, inspect the exact installed pinned source.
A help flag on a mutating Hex publish provider may execute publication, so
never use it to inspect options. Candidate work permits only
`make publish-dry-run`, after reading its recipe and confirming it always
supplies `--dry-run --yes`. Actual publication requires Operator-approved
concrete release notes and exact release actions.

Assistant-authored commits require exactly these consecutive final trailers,
with a blank line before the block:

Co-authored-by: Codex <noreply@openai.com>
Co-authored-by: Billo AI <ai-engineering@billo.systems>
