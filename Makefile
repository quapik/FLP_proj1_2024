GHC = ghc

FLAGS = -Wall

SRC = flp-fun.hs

OUT = flp-fun

all: $(OUT)	

$(OUT): $(SRC)
	$(GHC) $(FLAGS) -o $@ $<

clean:
	rm -f $(OUT) *.o *.hi

