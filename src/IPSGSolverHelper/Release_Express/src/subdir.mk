CPP_SRCS = \
../src/IPSGSolverHelper.cpp 

OBJS = \
./src/IPSGSolverHelper.o 
src/%.o: ../src/%.cpp
#	@$(eval DETOS=$(shell uname))
#	$(info detected_OS: DETOS)
#ifeq ("$(shell uname)", "Darwin")
#	@echo "Darwin"
#	@$(eval ADD_MFLAG=)
#else
#	@echo "Not Darwin"
#	@$(eval ADD_MFLAG=-m64)
#endif
	@echo 'Building file: $<'
	@echo 'Invoking: GCC C++ Compiler'
	$(CXX) -DPSG_EXPRESS $(CXXARCHFLAGS) -c $(CXXFLAGS) $(CXXPICFLAGS) $(ADD_CXXFLAGS) $(ADD_MFLAG) -o "$@" "$<"
	@echo 'Finished building: $<'
	@echo ' '


