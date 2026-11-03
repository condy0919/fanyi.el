EMACS ?= emacs
CASK ?= cask

compile:
	$(CASK) exec $(EMACS) -Q -batch \
	-L .							\
	-f batch-byte-compile *.el

test:
	$(CASK) exec $(EMACS) -Q -batch \
	-L . -L test -l test/fanyi-tests.el -l test/fanyi-base-tests.el \
	-l test/fanyi-config-tests.el \
	-f ert-run-tests-batch-and-exit

.PHONY: compile test
