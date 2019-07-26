

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
all: libPSGSolver_Express.a

# Tool invocations
libPSGSolver_Express.a: getFlag $(OBJS)
	@echo 'Building target: $@'
	@echo 'Invoking: GCC Archiver'
	$(AR) -cr  "libPSGSolver_Express.a" $(OBJS)
	@echo 'Finished building target: $@'
	@echo ' '

# Other Targets
clean:
	-$(RM) $(OBJS) libPSGSolver_Express.a
	-@echo ' '

