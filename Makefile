EMACS ?= emacs
CASK ?= cask

compile:
	$(CASK) exec $(EMACS) -Q -batch \
	-L .							\
	-f batch-byte-compile *.el

test:
	$(CASK) exec $(EMACS) -Q -batch \
	-L . -l test/fanyi-tests.el \
	-f ert-run-tests-batch-and-exit

.PHONY: compile test
