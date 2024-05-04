GHC = ghc
SRC = src
DIRS = $(shell find $(SRC) -type d)
TARGETS = $(wildcard $(SRC)/**/*.hs)

.PHONY: all clean

all: $(TARGETS)
	$(GHC) --make $(SRC)/Main.hs -isrc -o interpreter

clean:
	rm -f interpreter
	find $(SRC) -type f \( -name '*.hi' -o -name '*.o' \) -delete
