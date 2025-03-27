all: clean round_robin

round_robin: round_robin.o data_structure.o scheduler.o read_input.o print_output.o
	g++ round_robin.o data_structure.o scheduler.o read_input.o print_output.o -o round_robin

round_robin.o: round_robin.cpp
	g++ -g -c round_robin.cpp

data_structure.o: data_structure.cpp data_structure.h
	g++ -g -c data_structure.cpp

scheduler.o: scheduler.cpp scheduler.h
	g++ -g -c scheduler.cpp

read_input.o: read_input.cpp read_input.h
	g++ -g -c read_input.cpp

print_output.o: print_output.cpp print_output.h
	g++ -g -c print_output.cpp

clean:
	-rm round_robin *.o