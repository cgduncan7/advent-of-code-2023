solution_name = "Day 09 solution"

all: build run

build:
	@erlc ../../../lib/erl/harness.erl day09.erl

run:
	@echo $(solution_name)
	@erl -noshell -eval 'day09:run("../../data/$(data).txt")' -s init stop