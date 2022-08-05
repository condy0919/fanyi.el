EMACS ?= emacs

compile:
	$(EMACS) -Q -batch \
	-L .							\
	-f batch-byte-compile *.el

lint:
	$(EMACS) -Q -batch													                                    \
	--eval "(require 'package)"														                          \
	--eval "(push '(\"melpa\" . \"https://melpa.org/packages/\") package-archives)"	\
	--eval "(package-initialize)"													                          \
	--eval "(package-refresh-contents)"												                      \
	--eval "(setq package-lint-batch-fail-on-warnings nil)"						              \
	-l package-lint.el																                              \
	-f package-lint-batch-and-exit fanyi.el

.PHONY: compile lint
