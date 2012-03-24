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
	erl -pa ebin deps/*/ebin -s securities start_link -s securities_http start_link -sname securities

test: compile
	@erl -pa ebin -s securities_tests test -noshell -noinput -s init stop