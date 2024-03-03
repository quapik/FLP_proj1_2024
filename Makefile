GHCC = ghc

FLAGS = -Wall

SRC = src/Main.hs

OUT = flp-fun

all: $(OUT)	

$(OUT): $(SRC)
	$(GHCC) $(FLAGS) -o $@ $<

clean:
	rm -f $(OUT) *.o *.hi

