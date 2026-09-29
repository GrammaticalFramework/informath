# Informath regression tests; see README.md.
# Run from this directory, or from the root with `make -C regression ...`.

RUN_TESTS = python3 run.py

all:
	$(RUN_TESTS)

demo:
	$(RUN_TESTS) --tier demo

quick:
	$(RUN_TESTS) --tier demo,multi,dev

check:
	$(RUN_TESTS) --tier check

accept:
	$(RUN_TESTS) --accept

list:
	$(RUN_TESTS) --list

report:
	@cat results/latest/report.md

.PHONY: all demo quick check accept list report
