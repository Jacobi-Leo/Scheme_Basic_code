NGRID = 200000
PFLAG = 0

FC = gfortran
FCFLAGS = -ggdb -c \
	-Wall -Wsurprising -Wextra -Wunderflow -pedantic \
	-fcheck=all \
	-fbacktrace \
	-std=f2003 \

FLFLAGS = -ggdb -fbacktrace

main: main.o scheme.o
	$(FC) $(FLFLAGS) -o main *.o

main.o: main.f03 scheme.mod
	$(FC) $(FCFLAGS) main.f03

scheme.o: scheme.f03 const.mod
	$(FC) $(FCFLAGS) scheme.f03

scheme.mod: scheme.f03 const.mod
	$(FC) $(FCFLAGS) scheme.f03

const.o: const.f03
	$(FC) $(FCFLAGS) const.f03

const.mod: const.f03
	$(FC) $(FCFLAGS) const.f03

run: main
	./main $(NGRID) $(PFLAG) >> data.txt

clean:
	rm *.o *.mod

cleanall:
	rm main *.o *.mod


