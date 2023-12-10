solution_name = "Day 3 solution"

all: build run

build:
	@erlc ../../../lib/erl/harness.erl day03.erl

run:
	@echo $(solution_name)
	@erl -noshell -eval 'day03:run("../../data/$(data).txt")' -s init stop