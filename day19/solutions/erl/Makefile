solution_name = "Day 19 solution"

all: build run

build:
	@erlc ../../../lib/erl/harness.erl day19.erl

run:
	@echo $(solution_name)
	@erl -noshell -eval 'day19:run("../../data/$(data).txt")' -s init stop