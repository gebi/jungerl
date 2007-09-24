include ../../support/subdir.mk

tests/memcached_tests.beam: tests/memcached_tests.erl
	/usr/bin/erlc -W +debug_info -o tests/ tests/memcached_tests.erl

# Run the test suite.
test: all tests/memcached_tests.beam
	cp ./ebin/memcached.beam tests/
	erl -noshell -pa tests/ -run memcached_tests -s init stop