include ../../support/subdir.mk

tests/memcached_tests.beam: tests/memcached_tests.erl
	cp ./ebin/memcached.beam tests/
	/usr/bin/erlc -W +debug_info -o tests/ tests/memcached_tests.erl

# Run the test suite.
test: tests/memcached_tests.beam
	erl -noshell -pa tests/ -run memcached_tests