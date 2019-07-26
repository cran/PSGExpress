
-include src/subdir.mk

getFlag:
ifeq ($(OS), Windows_NT)
	@$(eval DETOS="Windows_NT")
else
	@$(eval DETOS="$(shell uname)")
endif
	@echo 'detected_OS:' $(DETOS)
ifeq ($(DETOS), "Darwin")
	@$(eval ADD_MFLAG=-m64)
else
	@$(eval ADD_MFLAG=)
endif

# All Target
all: libVK_Solver_MDB_Express.a

# Tool invocations
libVK_Solver_MDB_Express.a: getFlag $(OBJS)
	@echo 'Building target: $@'
	$(AR) -cr  "libVK_Solver_MDB_Express.a" $(OBJS)
	@echo 'Finished building target: $@'
	@echo ' '


# Other Targets
clean:
#	-$(RM) $(EXECUTABLES)$(OBJS)$(C_DEPS) libVK_Solver_MDB_Express.a cifort.mod funcnames.mod gurogipsg.mod intelinterf.mod inttypes.mod modcommons.mod
	-$(RM) $(OBJS) libVK_Solver_MDB_Express.a cifort.mod funcnames.mod gurogipsg.mod intelinterf.mod inttypes.mod modcommons.mod
	-@echo ' '

