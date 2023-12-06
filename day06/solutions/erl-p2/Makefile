solution_name = "Day 06 solution"

all: build run

build:
	@erlc ../../../lib/erl/harness.erl day06.erl

run:
	@echo Running solution: $(solution_name)
	@erl -noshell -eval 'day06:run("../../data/$(data).txt")' -s init stop