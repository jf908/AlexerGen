#!/usr/bin/make

.DEFAULT_GOAL := all

all:
	stack build
	ln -sf $(shell find .stack-work/dist/ -type f -name 'alexergen' | head -n 1) ./alexergen
.PHONY: all

clean:
	stack clean;\
	$(RM) alexergen
.PHONY: clean
