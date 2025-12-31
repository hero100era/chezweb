# Makefile for Scheme Web Framework

SCHEME = scheme
LIBDIRS = .

.PHONY: all compile run clean example

all: compile

compile:
	@echo "Compiling libraries..."
	@mkdir -p compiled
	@$(SCHEME) --compile-imported-libraries --libdirs "$(LIBDIRS)" -q << 'EOF'
	(import (chezweb))
	(printf "All libraries compiled successfully!~n")
	EOF

run: compile example

example:
	@echo "Starting server..."
	@CHEZSCHEMELIBDIRS="$(LIBDIRS)" $(SCHEME) --script examples/app.ss

clean:
	@echo "Cleaning..."
	@rm -rf compiled
	@find . -name "*.so" -type f -delete 2>/dev/null || true
	@find . -name "*.wpo" -type f -delete 2>/dev/null || true
	@echo "Done."

repl: compile
	@CHEZSCHEMELIBDIRS="$(LIBDIRS)" $(SCHEME) -q << 'EOF'
	(import (chezweb))
	(printf "Library loaded. Type (exit) to quit.~n")
	(new-cafe)
	EOF
