%.o: %.hs
	ghc -c -O $^

cbuddy: Cribbage.o
	ghc -o cbuddy Cribbage.o

.PHONY: clean

clean:
	rm -f cbuddy *.o
