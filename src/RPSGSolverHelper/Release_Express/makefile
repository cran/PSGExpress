-include src/subdir.mk

# All Target
all: libRPSGSolverHelper_Express.a

# Tool invocations
libRPSGSolverHelper_Express.a: $(OBJS)
	@echo 'Building target: $@'
	@echo 'Invoking: GCC Archiver'
	$(AR) -cr  "libRPSGSolverHelper_Express.a" $(OBJS)
	@echo 'Finished building target: $@'
	@echo ' '

# Other Targets
clean:
	-$(RM) $(OBJS) libRPSGSolverHelper_Express.a
	-@echo ' '

