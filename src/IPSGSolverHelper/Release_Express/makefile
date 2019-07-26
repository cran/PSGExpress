
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

all: libIPSGSolverHelper_Express.a

# Tool invocations
libIPSGSolverHelper_Express.a: getFlag $(OBJS)

#	DETOS1=$(shell uname)
#	$(info detected_OS_1: $(DETOS1))

	@echo 'Building target: $@'
	@echo 'Invoking: GCC Archiver'
	$(AR) -cr  "libIPSGSolverHelper_Express.a" $(OBJS)
	@echo 'Finished building target: $@'
	@echo ' '

# Other Targets
clean:
	-$(RM) $(OBJS) libIPSGSolverHelper_Express.a
	-@echo ' '

