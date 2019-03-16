-include src/subdir.mk

# All Target
all: MILP_Express

# Tool invocations
MILP_Express: $(OBJS)
	@echo 'Building target: $@'
	@echo 'Invoking: ar rcs packer'
#	$(AR) rcs  "libMILP_Express.a" $(OBJS) $(USER_OBJS) $(LIBS)
	$(AR) rcs  "libMILP_Express.a" $(OBJS)
	@echo 'Finished building target: $@'
	@echo ' '

# Other Targets
clean:
#	-$(RM) $(EXECUTABLES)$(OBJS)$(C_DEPS) libMILP_Express.a intelinterf.mod inttypes.mod
	-$(RM) $(OBJS) libMILP_Express.a intelinterf.mod inttypes.mod
	-@echo ' '


