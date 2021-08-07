EMACS ?= emacs
CASK ?= cask

compile:
	$(CASK) exec $(EMACS) -Q -batch \
	-L .							\
	-f batch-byte-compile *.el

lint:
	$(CASK) exec $(EMACS) -Q -batch \
	-l package-lint.el				\
	-f package-lint-batch-and-exit *.el

.PHONY: compile lint
