PROJECT = lutil
ROOT_DIR = $(shell pwd)
REPO = $(shell git config --get remote.origin.url)
REBAR_PROFILE ?= dev
LFE = _build/$(REBAR_PROFILE)/lib/lfe/bin/lfe

check:
	-@rebar3 lfe clean
	@rebar3 lfe compile
	@rebar3 xref
	-@rebar3 dialyzer
	@rebar3 as test lfe ltest

clean:
	@rm -rf _build rebar.lock

hex-publish:
	@echo "\nPublishing to hex.pm ...\n"
	@rm -rf doc
	@mkdir doc
	@cp priv/html/docs-redirect.html doc/index.html
	@rebar3 hex publish
	@rm -rf doc
