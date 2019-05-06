EXEC = communication
all:
	ocamlc -o $(EXEC) unix.cma phase1.ml phase2.ml

run: all
	./$(EXEC)

clean:
	rm -f `find . -name "*.o"`
	rm -f `find . -name "*.a"`
	rm -f `find . -name "*.cm*"`
	rm -f `find . -name "*~"`
	rm -f $(EXEC)