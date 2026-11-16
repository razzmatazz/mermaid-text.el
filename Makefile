.PHONY: ci compile lint test clean

ci: compile lint test

compile:
	eldev compile --warnings-as-errors

lint:
	eldev lint

test:
	eldev test

clean:
	eldev clean
