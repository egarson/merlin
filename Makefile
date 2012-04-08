REBAR=`which rebar || ./rebar`

all: clean build test

deps:
	@$(REBAR) get-deps

clean:
	@$(REBAR) clean

build:
	@$(REBAR) get-deps compile

test:
	@$(REBAR) skip_deps=true eunit

.PHONY: deps test clean build
