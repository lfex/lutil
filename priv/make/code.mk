compile:
	rebar3 compile

check:
	@echo
	@echo "=================================="
	@echo "Running tests using Github Sources ..."
	@echo "=================================="
	@echo
	rebar3 as test lfe test -t unit

check-gitlab:
	@echo
	@echo "=================================="
	@echo "Running tests using Gitlab Sources ..."
	@echo "=================================="
	@echo
	rebar3 as gitlab lfe test -t unit

check-hexpm: clean
	@echo
	@echo "==================================="
	@echo "Running tests using Hex.pm Packages ..."
	@echo "==================================="
	@echo
	@rebar3 as hexpm lfe clean
	rebar3 as hexpm lfe test -t unit

check-all: check check-gitlab check-hexpm

repl:
	@rebar3 as dev compile
	@$(LFE) -pa `rebar3 as dev path -s " -pa "`

shell:
	@rebar3 shell

clean:
	@rebar3 clean
	@rm -rf ebin/* _build/default/lib/$(PROJECT) rebar.lock \
		rebar3.crashdump \
		$(HOME)/.cache/rebar3/hex/default/packages/*

clean-all: clean
	@rebar3 as dev lfe clean

push:
	git push github master
	git push gitlab master

push-tags:
	git push github --tags
	git push gitlab --tags

push-all: push push-tags

build-github: clean
	@echo
	@echo "============================="
	@echo "Building using Github Sources ..."
	@echo "============================="
	@echo
	rebar3 compile
	rebar3 lock

build-gitlab: clean
	@echo
	@echo "============================="
	@echo "Building using Gitlab Sources ..."
	@echo "============================="
	@echo
	rebar3 as gitlab compile
	rebar3 as gitlab lock

build-hexpm: clean
	@echo
	@echo "=============================="
	@echo "Building using Hex.pm Packages ..."
	@echo "=============================="
	@echo
	rebar3 as hexpm compile
	rebar3 as hexpm lock

build-all: build-github build-gitlab build-hexpm

publish: clean
	rebar3 as hexpm lock
	rebar3 as hexpm hex publish
