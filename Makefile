GHC = ghc
SRC = src
TARGETS = $(wildcard $(SRC)/*.hs)

all: $(TARGETS)
	$(GHC) --make $^ -o interpreter

clean:
	rm -f interpreter
	rm -f $(SRC)/*.hi $(SRC)/*.o