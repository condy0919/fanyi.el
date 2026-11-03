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
	-l test/fanyi-provider-tests.el \
	-l test/fanyi-haici-tests.el -l test/fanyi-longman-tests.el \
	-l test/fanyi-response-tests.el \
	-l test/fanyi-etymon-tests.el \
	-l test/fanyi-shr-tests.el \
	-l test/fanyi-challenge-tests.el \
	-f ert-run-tests-batch-and-exit

.PHONY: compile test
