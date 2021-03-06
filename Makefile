NGRID = 128

PFLAG = 2

FC = gfortran
FCFLAGS = -c \
	-std=f2003 -pedantic \
# 	-ggdb \
# 	-Wall -Wsurprising -Wextra -Wunderflow \
# 	-fcheck=all \
# 	-fbacktrace \

# FLFLAGS = -ggdb -fbacktrace

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
	./main $(NGRID) $(PFLAG) >> u.txt

clean:
	rm *.o *.mod

cleanall:
	rm main *.o *.mod


