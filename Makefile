.DEFAULT_GOAL := test

EMACS ?= emacs
CASK ?= cask
TEST_FILES := $(wildcard test/*-tests.el)

compile:
	$(EMACS) -Q --batch \
	-L . \
	--eval '(setq byte-compile-error-on-warn t)' \
	-f batch-byte-compile *.el

test:
	$(EMACS) -Q --batch \
	-L . -L test $(addprefix -l ,$(TEST_FILES)) \
	-f ert-run-tests-batch-and-exit

clean:
	$(RM) *.elc test/*.elc

lint:
	$(CASK) exec $(EMACS) -Q --batch -L . -l package-lint \
	--eval '(setq package-lint-main-file "fanyi.el")' \
	-f package-lint-batch-and-exit *.el

.PHONY: compile test clean lint
