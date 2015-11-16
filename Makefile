REBAR=./rebar

all: clean deps compile

deps: get-deps update-deps

clean:
	$(REBAR) clean

compile:
	$(REBAR) compile

compile-app:
	$(REBAR) compile skip_deps=true

get-deps:
	$(REBAR) get-deps

update-deps:
	$(REBAR) update-deps

console: compile-app
	erl -pa deps/*/ebin -pa deps/*/include -pa ebin -s deliverly -config files/app.config

test: eunit ct

eunit:
	$(REBAR) eunit skip_deps=true

ct: compile-app
	$(REBAR) ct skip_deps=true
