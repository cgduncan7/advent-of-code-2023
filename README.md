# Advent of Code 2023

## Structure

Each day's challenge has its own folder with the following structure

```ascii
.
└── dayX/
    ├── data/
    │   ├── test*.txt
    │   └── *.txt (-)
    └── solutions/
        └── */
            └── Makefile (!)

(!) = required
(-) = ignored
```

All files within `dayX/data/` are ignored unless they begin with test. Do not check-in actual challenge data as it can be quite large. Simplify it first for testing.

## Solutions

Each solution has its own folder within `dayX/solutions/` and is required to have a Makefile. The contents of the Makefile are described below.

### Template

```makefile
solution_name = "Example solution"

all: build run

build:
    # Build stuff

run:
	@echo $(solution_name)
    # Run stuff
```