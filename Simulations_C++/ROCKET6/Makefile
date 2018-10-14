BIN_IMAGE = ROCKET6
CC = g++
CXXFLAGS = -Wall -g
CXXFLAGS += -I.

##### C Source #####
CPP_SOURCES = actuator.cpp \
              aerodynamics.cpp \
              class_functions.cpp \
              control.cpp \
              environment.cpp \
              euler.cpp \
              execution.cpp \
              forces.cpp \
              global_functions.cpp \
              gps.cpp \
              guidance.cpp \
              hyper_functions.cpp \
              ins.cpp \
              intercept.cpp \
              kinematics.cpp \
              newton.cpp \
              propulsion.cpp \
              rcs.cpp \
              startrack.cpp \
              tvc.cpp \
              utility_functions.cpp


##### OBJECTS #####
OBJECTS = $(patsubst %.cpp, %.o, $(CPP_SOURCES))
all: $(BIN_IMAGE) $(CPP_SOURCES)

deps := $(OBJECTS:%.o=%.o.d)

%.o: %.c
	$(CC) -c $< -o $@ $(CXXFLAGS)

$(BIN_IMAGE): $(OBJECTS)
	$(CC) -Wall -g $(CPP_SOURCES) -o $@ $(CXXFLAGS)
.PHONY : clean
clean:
	rm -f $(BIN_IMAGE)
	find . -name "*.o" -type f -delete
	find . -name "*.d" -type f -delete
