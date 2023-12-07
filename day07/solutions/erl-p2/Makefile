solution_name = "Day 07 solution"

all: build run

build:
	@erlc ../../../lib/erl/harness.erl day07.erl

run:
	@echo Running solution: $(solution_name)
	@erl -noshell -eval 'day07:run("../../data/$(data).txt")' -s init stop