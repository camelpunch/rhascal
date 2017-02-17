all:
	stack build
run: install
	rhascal-exe
install: lint
	stack install
lint:
	hlint app/* src/* test/*
test::
	stack test
