GHCC = ghc

FLAGS = -Wall

SRC = tree.hs

OUT = tree

all: $(OUT)	

$(OUT): $(SRC)
	$(GHCC) $(FLAGS) -o $@ $<

clean:
	rm -f $(OUT) *.o *.hi

