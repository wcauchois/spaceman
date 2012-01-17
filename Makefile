
all: clean
	ghc -c Quadtree.hs
	ghc -o spaceman Spaceman.hs

test:
	ghc QuadtreeTests.hs
	./QuadtreeTests

clean:
	rm -f *.hi *.o QuadtreeTests spaceman
