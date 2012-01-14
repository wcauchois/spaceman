
all: clean
	ghc -c Quadtree.hs
	ghc -o spaceman Spaceman.hs Quadtree.o

clean:
	rm -f *.hi *.o
