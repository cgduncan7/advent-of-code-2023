solution_name = "Example solution"

build:
	@erlc ../../../lib/erl/harness.erl dayExample.erl

run:
	@echo Running solution: $(solution_name)
	@time make run_solution

run_solution:
	@# Add command to run solution, don't include building as it is timed
	@erl -noshell -eval 'dayExample:run("../../data/test.txt")' -s init stop