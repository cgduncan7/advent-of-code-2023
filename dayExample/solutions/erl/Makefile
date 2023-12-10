solution_name = "Example solution"

all: build run

build:
	@erlc ../../../lib/erl/harness.erl dayExample.erl

run:
	@echo $(solution_name)
	@erl -noshell -eval 'dayExample:run("../../data/$(data).txt")' -s init stop