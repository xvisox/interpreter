GHC = ghc
SRC = src
DIRS = $(shell find $(SRC) -type d)
TARGETS = $(foreach dir,$(DIRS),$(wildcard $(dir)/*.hs))

all: $(TARGETS)
	$(GHC) --make $(SRC)/Main.hs -isrc -o interpreter

clean:
	rm -f interpreter
	find $(SRC) -type f -name '*.hi' -delete
	find $(SRC) -type f -name '*.o' -delete
