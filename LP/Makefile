SRC=lp
DIR=/usr/global/glpk/4.45
EXE=test
DATA=sorted.data.txt
#DATA=sorted.dataRealModified.txt

all:$(EXE)
	./$(EXE) < $(DATA)

$(EXE):$(SRC).o
	gcc -o $(EXE) -L $(DIR)/lib/ $(SRC).o -lglpk -lm

$(SRC).o:$(SRC).c
	gcc -I $(DIR)/include/ -c $(SRC).c

clean:
	rm -f *~ $(SRC).o $(EXE)
