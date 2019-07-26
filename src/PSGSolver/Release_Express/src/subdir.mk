CPP_SRCS = \
../src/PSGSolver.cpp 

OBJS = \
./src/PSGSolver.o 

src/%.o: ../src/%.cpp
	@echo 'Building file: $<'
	@echo 'Invoking: GCC C++ Compiler'
	$(CXX) -DPSG_EXPRESS $(CXXARCHFLAGS) -c $(CXXFLAGS) $(CXXPICFLAGS) $(ADD_CXXFLAGS) $(ADD_MFLAG) -o "$@" "$<"
	@echo 'Finished building: $<'
	@echo ' '


