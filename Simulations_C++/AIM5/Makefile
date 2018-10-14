BIN_IMAGE = AIM5
CC = g++
CXXFLAGS = -Wall -g
CXXFLAGS += -I.

##### C Source #####
CPP_SOURCES = aim_functions.cpp \
				aim_modules.cpp \
				aircraft_functions.cpp \
				aircraft_modules.cpp \
				class_functions.cpp \
				execution.cpp \
				flat3_modules.cpp \
				global_functions.cpp \
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