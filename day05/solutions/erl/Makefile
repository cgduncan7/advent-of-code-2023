solution_name = "Day 05 solution"

all: build run

build:
	@erlc ../../../lib/erl/harness.erl day05.erl

run:
	@echo $(solution_name)
	@erl -noshell -eval 'day05:run("../../data/$(data).txt")' -s init stop