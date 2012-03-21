all: compile

compile: 
	@erl -make

clean:
	rm -fv ebin/*.beam
	rm -fv erl_crash.dump

run: compile
	erl -pa ebin -s securities start_link -sname securities

test: compile
	@erl -pa ebin -s securities_tests test -noshell -noinput -s init stop