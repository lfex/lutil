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
