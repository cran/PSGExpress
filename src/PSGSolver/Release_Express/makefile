-include src/subdir.mk

# All Target
all: libPSGSolver_Express.a

# Tool invocations
libPSGSolver_Express.a: $(OBJS)
	@echo 'Building target: $@'
	@echo 'Invoking: GCC Archiver'
	$(AR) -cr  "libPSGSolver_Express.a" $(OBJS)
	@echo 'Finished building target: $@'
	@echo ' '

# Other Targets
clean:
	-$(RM) $(OBJS) libPSGSolver_Express.a
	-@echo ' '

