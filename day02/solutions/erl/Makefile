solution_name = "Day 2 solution"

all: build run

build:
	@erlc ../../../lib/erl/harness.erl day02.erl

run:
	@echo $(solution_name)
	@erl -noshell -eval 'day02:run("../../data/$(data).txt")' -s init stop