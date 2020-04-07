.PHONY : doc, run, test, coverage, help

level 	= info
file 	= log.log
players = 4
fac		= 9
port 	= 9000

run: ## Run the game
	@swipl --quiet -t "main($(level), \"$(file)\", $(players), $(fac))" -s game.pl 
    
test: ## Run tests
	@swipl -t "load_test_files([]), run_tests." -s game.pl 
 
coverage: ## Run tests and display coverage
	@swipl --quiet -t "use_module(library(test_cover)), load_test_files([]), show_coverage(run_tests)." -s utils.pl -s game.pl

install: ## Install dependencies
	sudo apt-get install swi-prolog

doc: ## Install the documentation server
	@echo Open the server at http://localhost:$(port)/azul/help
	@echo Close the below process when you read the documentation
	@swipl -g "use_module(library(http/http_path)), doc_server($(port))" -s game.pl

help: ## List available commands
	@grep -E '^[a-zA-Z_-%]+:.*?## .*$$' $(MAKEFILE_LIST) | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'
