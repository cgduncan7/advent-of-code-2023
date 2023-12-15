solution_name = "Day 15 solution"

all: build run

build:
	@erlc ../../../lib/erl/harness.erl day15.erl

run:
	@echo $(solution_name)
	@erl -noshell -eval 'day15:run("../../data/$(data).txt")' -s init stop