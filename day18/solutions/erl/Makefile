solution_name = "Day 18 solution"

all: build run

build:
	@erlc ../../../lib/erl/harness.erl day18.erl

run:
	@echo $(solution_name)
	@erl -noshell -eval 'day18:run("../../data/$(data).txt")' -s init stop