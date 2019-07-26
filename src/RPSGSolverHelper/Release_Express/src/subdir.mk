CPP_SRCS = \
../src/RPSGSolverHelper.cpp

OBJS = \
./src/RPSGSolverHelper.o

src/%.o: ../src/%.cpp
	@echo 'Building file: $<'
	@echo 'Invoking: GCC C++ Compiler'
	$(CXX) -DPSG_EXPRESS $(CXXARCHFLAGS) -c $(ARCHFLAG) $(CXXFLAGS) $(CXXPICFLAGS) $(ADD_CXXFLAGS) $(ADD_MFLAG) -o "$@" "$<"
	@echo 'Finished building: $<'
	@echo ' '


