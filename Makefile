new:
ifdef day
	@echo 'Making directory for day $(day)'
	@mkdir -p day$(day)/data
	@mkdir -p day$(day)/solutions
	@touch day$(day)/README.md
	@touch day$(day)/data/.gitkeep
	@touch day$(day)/solutions/.gitkeep
	@echo "# day$(day)\n" >> day$(day)/README.md
	@echo "[Challenge](https://adventofcode.com/2023/day/$(day))" >> day$(day)/README.md
else
	@echo 'Run again with argument day=<n>'
endif

run:
ifdef day
ifdef solution
		@make -C day$(day)/solutions/$(solution) run
else
		@echo Run again with argument solution=<name>
endif
else
	@echo 'Run again with argument day=<n>'
endif