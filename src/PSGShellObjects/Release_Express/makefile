-include src/subdir.mk

# All Target
all: libPSGShellObjects_Express.a

# Tool invocations
libPSGShellObjects_Express.a: $(OBJS)
	@echo 'Building target: $@'
	@echo 'Invoking: GCC Archiver'
	$(AR) -cr  "libPSGShellObjects_Express.a" $(OBJS)
	@echo 'Finished building target: $@'
	@echo ' '

# Other Targets
clean:
	-$(RM) $(OBJS) libPSGShellObjects_Express.a
	-@echo ' '

