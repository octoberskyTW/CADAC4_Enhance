CC = g++
#CFALGS = -Wall
DIR = -I /Users/laijunxu/Desktop/NSPO/CADAC/Simulations_C++/SRAAM6/include
OBJ = actuator.o aerodynamics.o class_functions.o control.o environment.o euler.o execution.o flat3_modules.o forces.o global_functions.o guidance.o intercept.o kinematics.o missile_functions.o newton.o propulsion.o seeker.o target_functions.o target_modules.o tvc.o utility_functions.o
CPP = actuator.cpp aerodynamics.cpp class_functions.cpp control.cpp environment.cpp euler.cpp execution.cpp flat3_modules.cpp forces.cpp global_functions.cpp guidance.cpp intercept.cpp kinematics.cpp missile_functions.cpp newton.cpp propulsion.cpp seeker.cpp target_functions.cpp target_modules.cpp tvc.cpp utility_functions.cpp

go: ${OBJ}
	${CC} ${DIR} ${OBJ} -o execution.out
actuator.o: ${CPP}
	${CC} ${DIR} -c ${CPP} 
clean:
	${RM} *.o
