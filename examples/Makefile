.PHONY: all clean compare

all: compare


bench.prof: *.hs prof
	ghc --make -O2 -prof -auto-all -o $@ bench.hs -odir prof -hidir prof

bench.exe: *.hs exe
	ghc --make -O2 -o $@ bench.hs -odir exe -hidir exe

exe:
	mkdir -p exe

prof:
	mkdir -p prof

%.hp: bench.prof
	./$< "$(basename $(basename $@))" input.txt > /dev/null +RTS -h$(subst .,,$(suffix $(basename $@)))
	mv bench.prof.hp $@

%.ps: %.hp
	hp2ps -c $<

compare: bench.exe
	time -p ./$< "parsec"    input.txt > /dev/null
	time -p ./$< "parsimony" input.txt input.txt > /dev/null


clean:
	-rm *.hi *.o *.hp *.aux *.ps
