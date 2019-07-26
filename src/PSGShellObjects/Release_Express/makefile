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
all: libPSGShellObjects_Express.a

# Tool invocations
libPSGShellObjects_Express.a: getFlag $(OBJS)
	@echo 'Building target: $@'
	@echo 'Invoking: GCC Archiver'
	$(AR) -cr  "libPSGShellObjects_Express.a" $(OBJS)
	@echo 'Finished building target: $@'
	@echo ' '

# Other Targets
clean:
	-$(RM) $(OBJS) libPSGShellObjects_Express.a
	-@echo ' '

