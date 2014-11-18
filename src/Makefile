all: PartialInspector.js

PartialInspector.js: PartialInspector.hs
	hastec PartialInspector.hs # -main-is Railoscopy

clean:
	-rm -r main
	-rm *~
	-rm *.hi
	-rm *.o
	-rm PartialInspector.js

distclean: clean
	-rm PartialInspector
	-rm PartialInspector.js
