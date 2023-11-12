lib:
	cd Rubiks/lib && touch .depend && make depend && make

rubiks: lib
	cd Rubiks && touch .depend && make depend

run: rubiks
	cd Rubiks && make test

doc: lib rubiks
	cd Rubiks && mkdir html && make doc && cp -r html ../doc && rm -rf html

clean:
	cd Rubiks && make clean && rm .depend
	cd Rubiks/lib && make clean && rm .depend
	rm -rf doc

.PHONY: lib rubiks test clean
