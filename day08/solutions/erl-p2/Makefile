solution_name = "Day 08 solution"

all: build run

build:
	@erlc ../../../lib/erl/harness.erl day08.erl

run:
	@echo Running solution: $(solution_name)
	@erl -noshell -eval 'day08:run("../../data/$(data).txt")' -s init stop