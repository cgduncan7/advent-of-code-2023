solution_name = "Example solution"

all: build run

build:
	@erlc ../../../lib/erl/harness.erl dayExample.erl

run:
	@echo Running solution: $(solution_name)
	@make run_solution

run_solution:
	@erl -noshell -eval 'dayExample:run("../../data/test.txt")' -s init stop