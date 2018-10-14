CC = g++
#CFALGS = -Wall
DIR = -I /Users/laijunxu/Desktop/NSPO/CADAC4/Simulations_C++/FALCON6/include
OBJ = execution.o actuator.o aerodynamics.o class_functions.o control.o environment.o euler.o forces.o global_functions.o guidance.o kinematics.o newton.o plane_functions.o propulsion.o utility_functions.o
CPP = execution.cpp actuator.cpp aerodynamics.cpp class_functions.cpp control.cpp environment.cpp euler.cpp forces.cpp global_functions.cpp guidance.cpp kinematics.cpp newton.cpp plane_functions.cpp propulsion.cpp utility_functions.cpp
go: ${OBJ}
	${CC} ${DIR} ${OBJ} -o execution.out
execution.o: ${CPP}
	${CC} ${DIR} -c ${CPP} 
clean:
	${RM} *.o
	${RM} *.out
