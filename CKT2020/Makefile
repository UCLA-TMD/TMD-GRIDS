COMP = gfortran
FFLAGS = -O3
RUNFLAGS = -ffixed-line-length-none -cpp -fbacktrace -fcheck=all
DEBUGFLAGS = -g -Wall -Wextra -Wsurprising -Walign-commons -ffpe-trap=zero,invalid,overflow,underflow
SOURCE = example.f CKT.f

executables:
	$(COMP) $(FFLAGS) -c $(SOURCE) 
	$(COMP) $(LIBPATH) $(LIB) *.o -o example.out

clean:
	-rm *.out
	-rm *.o
	-rm *.pdf
