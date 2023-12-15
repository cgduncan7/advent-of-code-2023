solution_name = "Day XX solution"

all: build run

build:
	@erlc ../../../lib/erl/harness.erl dayXX.erl

run:
	@echo $(solution_name)
	@erl -noshell -eval 'dayXX:run("../../data/$(data).txt")' -s init stop