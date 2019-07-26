-include src/subdir.mk

$(OBJS): $(OBJS_1)

#.PHONY: getFlag

#getFlag:
#	@$(eval DETOS=$(shell uname))
#	$(info detected_OS: DETOS)
#ifeq ("$(shell uname)", "Darwin")
#	@echo "Darwin"
#	@$(eval ADD_MFLAG=-m64)
#else
#	@echo "Not Darwin"
#	@$(eval ADD_MFLAG=)
#endif

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
all: MILP_Express

# Tool invocations
MILP_Express: getFlag $(OBJS)
	@echo 'Building target: $@'
	@echo 'Invoking: ar rcs packer'
	$(AR) -cr  "libMILP_Express.a" $(OBJS)
	@echo 'Finished building target: $@'
	@echo ' '

# Other Targets
clean:
	-$(RM) $(OBJS) libMILP_Express.a intelinterf.mod inttypes.mod
	-@echo ' '


