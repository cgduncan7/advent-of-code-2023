solution_name = "Day 10 solution"

all: build run

build:
	@erlc ../../../lib/erl/harness.erl day10.erl

run:
	@echo Running solution: $(solution_name)
	@erl -noshell -eval 'day10:run("../../data/$(data).txt")' -s init stop