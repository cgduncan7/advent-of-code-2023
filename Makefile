new:
ifdef day
	@echo 'Making directory for day $(day)'
	@mkdir -p day$(day)/data
	@mkdir -p day$(day)/solutions
	@touch day$(day)/README.md
	@touch day$(day)/data/.gitkeep
	@touch day$(day)/solutions/.gitkeep
	@cp -r templates/* day$(day)/solutions
	@find day$(day)/solutions -name "dayXX.*" -exec bash -c 'mv $$0 $$(echo $$0 | sed "s/dayXX/day$(day)/")' {} \;
	@grep -rl "XX" day$(day)/solutions | xargs sed -i "" "s/XX/$(day)/g"
	@echo "# day$(day)\n" >> day$(day)/README.md
	@echo "[Challenge](https://adventofcode.com/2023/day/$(day))" >> day$(day)/README.md
else
	@echo 'Run again with argument day=<n>'
endif

run:
ifdef day
ifdef solution
ifdef data
		@make -C day$(day)/solutions/$(solution) data=$(data)
else
	@echo Run again with argument data=<filename>
endif
else
	@echo Run again with argument solution=<name>
endif
else
	@echo 'Run again with argument day=<n>'
endif