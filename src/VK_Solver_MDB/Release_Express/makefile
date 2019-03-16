-include src/subdir.mk

# All Target
all: libVK_Solver_MDB_Express.a

# Tool invocations
#libVK_Solver_MDB_Express.a: $(OBJS) $(USER_OBJS) #$(LIBS) # libMILP.a
libVK_Solver_MDB_Express.a: $(OBJS)
	@echo 'Building target: $@'
#	@echo 'Invoking: GCC Linker'
	$(AR) -cr  "libVK_Solver_MDB_Express.a" $(OBJS)
#	ar -cr  "libVK_Solver_MDB_Express.a" $(OBJS) $(USER_OBJS) $(LIBS)
	@echo 'Finished building target: $@'
	@echo ' '

    

# Other Targets
clean:
#	-$(RM) $(EXECUTABLES)$(OBJS)$(C_DEPS) libVK_Solver_MDB_Express.a cifort.mod funcnames.mod gurogipsg.mod intelinterf.mod inttypes.mod modcommons.mod
	-$(RM) $(OBJS) libVK_Solver_MDB_Express.a cifort.mod funcnames.mod gurogipsg.mod intelinterf.mod inttypes.mod modcommons.mod
	-@echo ' '

