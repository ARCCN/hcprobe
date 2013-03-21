.DEFAULT: all
.PHONY: all

CABAL       = cabal-dev
BUILDDIR    = ../build

ifdef BUILDDIR
	BUILDDIR_ARG    = --build $(BUILDDIR)
endif

all:
	$(CABAL) build

configure:
	$(CABAL) configure $(BUILDDIR_ARG)
