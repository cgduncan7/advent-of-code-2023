solution_name = "Day 16 solution"

all: build run

build:
	@erlc ../../../lib/erl/harness.erl day16.erl

run:
	@echo $(solution_name)
	@erl -noshell -eval 'day16:run("../../data/$(data).txt")' -s init stop