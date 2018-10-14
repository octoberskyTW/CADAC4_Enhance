CC = g++
#CFALGS = -Wall
DIR = -I /Users/laijunxu/Desktop/NSPO/CADAC4/Simulations_C++/GHAME6/include
OBJ = actuator.o aerodynamics.o class_functions.o control.o datalink.o environment.o euler.o execution.o forces.o global_functions.o gps.o ground0_modules.o guidance.o hyper_functions.o ins.o intercept.o kinematics.o newton.o propulsion.o radar_functions.o radar_modules.o rcs.o round3_modules.o satellite_functions.o satellite_modules.o seeker.o startrack.o utility_functions.o
CPP = actuator.cpp aerodynamics.cpp class_functions.cpp control.cpp datalink.cpp environment.cpp euler.cpp execution.cpp forces.cpp global_functions.cpp gps.cpp ground0_modules.cpp guidance.cpp hyper_functions.cpp ins.cpp intercept.cpp kinematics.cpp newton.cpp propulsion.cpp radar_functions.cpp radar_modules.cpp rcs.cpp round3_modules.cpp satellite_functions.cpp satellite_modules.cpp seeker.cpp startrack.cpp utility_functions.cpp
go: ${OBJ}
	${CC} ${DIR} ${OBJ} -o execution.out
actuator.o: ${CPP}
	${CC} ${DIR} -c ${CPP} 
clean:
	${RM} *.o
