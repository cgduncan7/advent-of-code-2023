solution_name = "Day 20 solution"

all: build run

build:
	@erlc ../../../lib/erl/harness.erl day20.erl

run:
	@echo $(solution_name)
	@erl -noshell -eval 'day20:run("../../data/$(data).txt")' -s init stop