PROJECT = lutil
REBAR3 ?= rebar3
ERL ?= erl
DOCS_OVERVIEW := $(CURDIR)/docs/overview.md
DOCS_MANAGED := $(CURDIR)/_build/default/ldoc/current/site

.PHONY: help compile check-native check xref dialyzer docs clean publish \
        publish-dry-run hex-publish

help:
	@printf '%s\n' \
	  'compile          Compile the application' \
	  'check-native     Compile, compile tests, run EUnit and all ltest groups' \
	  'check            Alias for check-native' \
	  'xref             Run Rebar3 xref diagnostics' \
	  'dialyzer         Run Rebar3 Dialyzer analysis' \
	  'docs             Build or refresh the ldoc site' \
	  'clean            Remove generated _build output' \
	  'publish-dry-run  Stage docs and run the Hex dry-run (no upload)' \
	  'publish          Stage docs and publish through the Hex provider' \
	  'hex-publish      Compatibility alias for publish'

compile:
	@"$(REBAR3)" compile

check-native:
	@"$(REBAR3)" compile
	@"$(REBAR3)" as test compile
	@"$(REBAR3)" as test eunit
	@"$(ERL)" -pa _build/test/lib/*/ebin -noshell -eval \
	  'case ltest:all() of ok -> halt(0); _ -> halt(1) end.'

check: check-native

xref:
	@"$(REBAR3)" xref

dialyzer:
	@"$(REBAR3)" dialyzer

ifeq ($(strip $(DOCS_OUTPUT)),)
docs:
	@echo "Building managed ldoc reference at $(DOCS_MANAGED)"
	@"$(REBAR3)" ldoc --overview "$(DOCS_OVERVIEW)" --namespace hexpm
else
docs:
	@docs_parent="$$(dirname -- "$(DOCS_OUTPUT)")"; \
	test -d "$$docs_parent" || { echo "docs output parent must already exist: $$docs_parent" >&2; exit 1; }; \
	test ! -e "$(DOCS_OUTPUT)" || { echo "docs output already exists; choose a fresh DOCS_OUTPUT: $(DOCS_OUTPUT)" >&2; exit 1; }; \
	echo "Building ldoc reference at $(DOCS_OUTPUT)"; \
	"$(REBAR3)" ldoc --overview "$(DOCS_OVERVIEW)" --namespace hexpm --output "$(DOCS_OUTPUT)"
endif

clean:
	rm -rf -- _build

publish-dry-run:
	@set -eu; \
	parent="$(CURDIR)/_build"; \
	mkdir -p "$$parent"; \
	stage="$$(mktemp -d "$$parent/lutil-docs-dry-run.XXXXXX")"; \
	trap 'rm -rf -- "$$stage"' EXIT; \
	trap 'exit 1' HUP INT TERM; \
	"$(REBAR3)" ldoc --overview "$(DOCS_OVERVIEW)" --namespace hexpm --output "$$stage/reference"; \
	"$(REBAR3)" hex publish --doc-dir "$$stage/reference/site" --dry-run --yes

publish:
	@set -eu; \
	parent="$(CURDIR)/_build"; \
	mkdir -p "$$parent"; \
	stage="$$(mktemp -d "$$parent/lutil-docs-publish.XXXXXX")"; \
	trap 'rm -rf -- "$$stage"' EXIT; \
	trap 'exit 1' HUP INT TERM; \
	"$(REBAR3)" ldoc --overview "$(DOCS_OVERVIEW)" --namespace hexpm --output "$$stage/reference"; \
	"$(REBAR3)" hex publish --doc-dir "$$stage/reference/site"

hex-publish: publish
