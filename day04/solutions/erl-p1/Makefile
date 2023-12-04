solution_name = "Day 04 solution"

all: build run

build:
	@erlc ../../../lib/erl/harness.erl day04.erl

run:
	@echo Running solution: $(solution_name)
	@erl -noshell -eval 'day04:run("../../data/$(data).txt")' -s init stop