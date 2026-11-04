.DEFAULT_GOAL := test

EMACS ?= emacs
CASK ?= cask

compile:
	$(EMACS) -Q --batch \
	-L . \
	-f batch-byte-compile *.el

test:
	$(EMACS) -Q --batch \
	-L . -L test -l test/fanyi-tests.el -l test/fanyi-base-tests.el \
	-l test/fanyi-retry-tests.el \
	-l test/fanyi-config-tests.el \
	-l test/fanyi-provider-tests.el \
	-l test/fanyi-youdao-ui-tests.el \
	-l test/fanyi-haici-tests.el -l test/fanyi-longman-tests.el \
	-l test/fanyi-response-tests.el \
	-l test/fanyi-etymon-tests.el \
	-l test/fanyi-shr-tests.el \
	-l test/fanyi-font-lock-tests.el \
	-l test/fanyi-navigation-tests.el \
	-l test/fanyi-bookmark-tests.el \
	-l test/fanyi-shr-reflow-tests.el \
	-l test/fanyi-shr-section-tests.el \
	-l test/fanyi-longman-style-tests.el \
	-l test/fanyi-longman-sun-tests.el \
	-l test/fanyi-longman-explode-tests.el \
	-l test/fanyi-longman-section-tests.el \
	-f ert-run-tests-batch-and-exit

clean:
	$(RM) *.elc test/*.elc

lint:
	$(CASK) exec $(EMACS) -Q --batch -L . -l package-lint \
	--eval '(setq package-lint-main-file "fanyi.el")' \
	-f package-lint-batch-and-exit *.el

.PHONY: compile test clean lint
