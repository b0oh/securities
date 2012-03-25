APP_NAME=securities_app
CONFIG=priv/securities

ERL_ARGS=-pa ebin deps/*/ebin -config $(CONFIG) -sname $(APP_NAME) -s $(APP_NAME)

all: compile

get-deps:
	[ -d deps/yaws ] || git clone git://github.com/klacke/yaws deps/yaws

build-deps: get-deps
	[ -f deps/yaws/configure] || (cd deps/yaws && autoconf)
	(cd deps/yaws && ./configure && make)

compile:
	@erl -make

clean:
	rm -fv ebin/*.beam
	rm -fv erl_crash.dump

run: compile
	@erl $(ERL_ARGS) -boot start_sasl -noshell -noinput -detached
	@echo "securities application started"

run-dev: compile
	erl $(ERL_ARGS)

test: compile
	@erl -pa ebin -s securities_tests test -noshell -noinput -s init stop