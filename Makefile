REBAR=$(CURDIR)/rebar

run: compile
	ERL_LIBS=deps:apps erl +P 1000000 -name gol-dev@127.0.0.1 -s gol_app

compile: get-deps
	$(REBAR) compile

get-deps:
	$(REBAR) get-deps
