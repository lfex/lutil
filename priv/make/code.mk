compile:
	rebar3 compile

check:
	rebar3 as test lfe test -t unit

repl:
	@rebar3 as dev compile
	@$(LFE) -pa `rebar3 as dev path -s " -pa "`

shell:
	@rebar3 shell

clean:
	@rebar3 clean
	@rm -rf ebin/* _build/default/lib/$(PROJECT) rebar.lock

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
	rebar3 compile

build-gitlab: clean
	rebar3 as gitlab compile

build-hexpm: clean
	rebar3 as hexpm compile

build-all: build-github build-gitlab build-hexpm

publish: clean
	rebar3 as hexpm hex publish
