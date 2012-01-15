
all: clean
	ghc -c Quadtree.hs
	ghc -o spaceman Spaceman.hs

clean:
	rm -f *.hi *.o
