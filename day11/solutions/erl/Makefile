solution_name = "Day 11 solution"

all: build run

build:
	@erlc ../../../lib/erl/harness.erl day11.erl

run:
	@echo $(solution_name)
	@erl -noshell -eval 'day11:run("../../data/$(data).txt")' -s init stop