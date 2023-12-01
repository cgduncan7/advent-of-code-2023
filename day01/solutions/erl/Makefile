solution_name = "Day 1 solution"

all: build run

build:
	@erlc ../../../lib/erl/harness.erl day01.erl

run:
	@echo Running solution: $(solution_name)
	@erl -noshell -eval 'day01:run("../../data/$(data).txt")' -s init stop