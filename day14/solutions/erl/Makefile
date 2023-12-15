solution_name = "Day 14 solution"

all: build run

build:
	@erlc ../../../lib/erl/harness.erl day14.erl

run:
	@echo $(solution_name)
	@erl -noshell -eval 'day14:run("../../data/$(data).txt")' -s init stop