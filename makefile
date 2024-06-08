# this makefile is a template documenting what useful commands run here

# basic building and test
build:
	stack build

run:
	stack run

test:
	stack test

# collecting test coverage metrics to assess the quality of the test suite
coverage:
	stack build
	stack --coverage test 

save-coverage:
	mkdir -p test-coverage
	cp -r `stack path --local-hpc-root` test-coverage/`date "+%Y-%m-%d-%H%M%S"`

# building productiction version for steam
# TBD

linecounts:
	wc -l src/*.hs app/*.hs
