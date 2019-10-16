#!/usr/bin/make

.DEFAULT_GOAL := all

all:
	stack build
.PHONY: all

clean:
	stack clean
.PHONY: clean
