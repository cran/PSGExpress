FOR_SRCS = \
../src/IntelInterf.for \
../src/MB11.for \
../src/MB12.for \
../src/MB2.for \
../src/MB31.for \
../src/MB32.for \
../src/MB33.for \
../src/MB34.for \
../src/MB35.for \
../src/MB42A.for \
../src/MB5.for \
../src/MB5_vk.for \
../src/MB6.for \
../src/MB8.for \
../src/Mb1a.for \
../src/Mb1b.for \
../src/Mb42a_VK.for \
../src/Mb7.for \
../src/mb0a.for \
../src/mb0b.for 

OBJS = \
./src/IntelInterf.o \
./src/MB11.o \
./src/MB12.o \
./src/MB2.o \
./src/MB31.o \
./src/MB32.o \
./src/MB33.o \
./src/MB34.o \
./src/MB35.o \
./src/MB42A.o \
./src/MB5.o \
./src/MB5_vk.o \
./src/MB6.o \
./src/MB8.o \
./src/Mb1a.o \
./src/Mb1b.o \
./src/Mb42a_VK.o \
./src/Mb7.o \
./src/mb0a.o \
./src/mb0b.o 


src/%.o: ../src/%.for
	@echo 'Building file: $<'
	@echo 'Invoking: GNU Fortran Compiler'
	$(FC) -c -D___PSGExpRess $(FPICFLAGS) $(FFLAGS) $(ADD_FFLAGS) -o "$@" "$<"
	@echo 'Finished building: $<'
	@echo ' '

src/IntelInterf.o: ../src/IntelInterf.for

src/MB11.o: ../src/MB11.for src/IntelInterf.o

src/MB12.o: ../src/MB12.for src/IntelInterf.o

src/MB2.o: ../src/MB2.for

src/MB31.o: ../src/MB31.for

src/MB32.o: ../src/MB32.for

src/MB33.o: ../src/MB33.for

src/MB34.o: ../src/MB34.for

src/MB35.o: ../src/MB35.for

src/MB42A.o: ../src/MB42A.for

src/MB5.o: ../src/MB5.for

src/MB5_vk.o: ../src/MB5_vk.for

src/MB6.o: ../src/MB6.for

src/MB8.o: ../src/MB8.for

src/Mb1a.o: ../src/Mb1a.for

src/Mb1b.o: ../src/Mb1b.for

src/Mb42a_VK.o: ../src/Mb42a_VK.for

src/Mb7.o: ../src/Mb7.for src/IntelInterf.o

src/mb0a.o: ../src/mb0a.for src/IntelInterf.o

src/mb0b.o: ../src/mb0b.for src/IntelInterf.o


