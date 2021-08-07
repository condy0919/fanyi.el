EMACS ?= emacs
CASK ?= cask

compile:
	$(CASK) exec $(EMACS) -Q -batch \
	-L .							\
	-f batch-byte-compile *.el

lint:
	$(CASK) exec $(EMACS) -Q -batch													\
	--eval "(require 'package)"														\
	--eval "(push '(\"melpa\" . \"https://melpa.org/packages/\") package-archives)"	\
	--eval "(package-initialize)"													\
	--eval "(package-refresh-contents)"												\
	-l package-lint.el																\
	-f package-lint-batch-and-exit *.el

.PHONY: compile lint
