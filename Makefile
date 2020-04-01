test: ## Run tests
	swipl -t "load_test_files([]), run_tests." -s utils.pl
	swipl -t "load_test_files([]), run_tests." -s game.pl

coverage: ## Run tests and display coverage
	swipl -t "use_module(library(test_cover)), load_test_files([]), show_coverage(run_tests)." -s utils.pl -s game.pl

help: ## List available commands
	@grep -E '^[a-zA-Z_-%]+:.*?## .*$$' $(MAKEFILE_LIST) | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'
