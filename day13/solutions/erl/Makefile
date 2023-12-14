solution_name = "Day 13 solution"

all: build run

build:
	@erlc ../../../lib/erl/harness.erl day13.erl

run:
	@echo $(solution_name)
	@erl -noshell -eval 'day13:run("../../data/$(data).txt")' -s init stop