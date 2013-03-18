.DEFAULT: all
.PHONY: all

CABAL       = cabal
BUILDDIR    = ../build

ifdef BUILDDIR
	BUILDDIR_ARG    = --build $(BUILDDIR)
endif

all:
	$(CABAL) build $(BUILDDIR_ARG)

configure:
	$(CABAL) configure $(BUILDDIR_ARG)
