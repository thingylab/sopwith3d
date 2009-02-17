all:
	cd src && ghc --make -o ../Sopwith Sopwith.hs

test: all
	./Sopwith

clean:
	rm ./src/*.hi
	rm ./src/*.o
	rm ./Sopwith