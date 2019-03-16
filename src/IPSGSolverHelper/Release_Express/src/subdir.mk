CPP_SRCS = \
../src/IPSGSolverHelper.cpp 

OBJS = \
./src/IPSGSolverHelper.o 
src/%.o: ../src/%.cpp
	@echo 'Building file: $<'
	@echo 'Invoking: GCC C++ Compiler'
	$(CXX) -DPSG_EXPRESS $(CXXARCHFLAGS) -c $(CXXFLAGS) $(CXXPICFLAGS) $(ADD_CXXFLAGS) -o "$@" "$<"
	@echo 'Finished building: $<'
	@echo ' '


