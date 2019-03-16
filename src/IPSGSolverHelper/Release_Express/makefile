-include src/subdir.mk

# All Target
all: libIPSGSolverHelper_Express.a

# Tool invocations
libIPSGSolverHelper_Express.a: $(OBJS)
	@echo 'Building target: $@'
	@echo 'Invoking: GCC Archiver'
	$(AR) -cr  "libIPSGSolverHelper_Express.a" $(OBJS)
	@echo 'Finished building target: $@'
	@echo ' '

# Other Targets
clean:
	-$(RM) $(OBJS) libIPSGSolverHelper_Express.a
	-@echo ' '

