CC = g++
#CFALGS = -Wall
DIR =-I /Users/laijunxu/Desktop/NSPO/CADAC4/Simulations_C++/CRUISE5/include
OBJ =  execution.o class_functions.o global_functions.o utility_functions.o cruise_functions.o cruise_modules.o satellite_functions.o satellite_modules.o target_functions.o target_modules.o round3_modules.o
CPP = execution.cpp class_functions.cpp global_functions.cpp utility_functions.cpp cruise_functions.cpp cruise_modules.cpp satellite_functions.cpp satellite_modules.cpp target_functions.cpp target_modules.cpp round3_modules.cpp
go: ${OBJ}
	$(CC) ${DIR} ${OBJ} -o execution.out
execution.o: ${CPP}
	${CC} ${DIR} -c ${CPP}
clean:
	${RM} *.o
