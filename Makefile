.PHONY: test

ERL=erl
BEAMDIR=./deps/*/ebin ./ebin
REBAR=./rebar
DIALYZER=dialyzer
EDOWN=./make_doc

all: update-deps get-deps clean compile xref edoc

update-deps:
	@$(REBAR) update-deps

get-deps:
	@$(REBAR) get-deps

compile:
	@$(REBAR) compile

xref:
	@$(REBAR) xref skip_deps=true

clean:
	@$(REBAR) clean

test:
	@$(REBAR) skip_deps=true eunit

edoc:
	@$(EDOWN)

dialyzer: compile
	@$(DIALYZER) ebin deps/serial/ebin

setup-dialyzer:
	@$(DIALYZER) --build_plt \
                     --apps kernel stdlib mnesia eunit erts crypto
