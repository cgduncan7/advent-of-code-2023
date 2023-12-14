solution_name = "Day 12 solution"

all: build run

build:
	@erlc ../../../lib/erl/harness.erl day12.erl

run:
	@echo $(solution_name)
	@erl -noshell -eval 'day12:run("../../data/$(data).txt")' -s init stop