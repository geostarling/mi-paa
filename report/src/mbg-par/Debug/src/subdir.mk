################################################################################
# Automatically-generated file. Do not edit!
################################################################################

# Add inputs and outputs from these tool invocations to the build variables 
CPP_SRCS += \
../src/Graph.cpp \
../src/ProcessInfo.cpp \
../src/Solver.cpp \
../src/main.cpp \
../src/utils.cpp 

OBJS += \
./src/Graph.o \
./src/ProcessInfo.o \
./src/Solver.o \
./src/main.o \
./src/utils.o 

CPP_DEPS += \
./src/Graph.d \
./src/ProcessInfo.d \
./src/Solver.d \
./src/main.d \
./src/utils.d 


# Each subdirectory must supply rules for building sources it contributes
src/%.o: ../src/%.cpp
	@echo 'Building file: $<'
	@echo 'Invoking: GCC C++ Compiler'
	mpic++ -O0 -g3 -Wall -c -fmessage-length=0 -ggdb -MMD -MP -MF"$(@:%.o=%.d)" -MT"$(@:%.o=%.d)" -o"$@" "$<"
	@echo 'Finished building: $<'
	@echo ' '


