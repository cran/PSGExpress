FOR_SRCS = \
../src/IntelInterf.F \
../src/MB2.F \
../src/MB31.F \
../src/MB32.F \
../src/MB33.F \
../src/MB34.F \
../src/MB35.F \
../src/MB42A.F \
../src/MB5.F \
../src/MB5_vk.F \
../src/MB6.F \
../src/MB8.F \
../src/Mb1a.F \
../src/Mb1b.F \
../src/Mb42a_VK.F \
../src/MB11.F \
../src/MB12.F \
../src/Mb7.F \
../src/mb0a.F \
../src/mb0b.F 

OBJS = \
./src/IntelInterf.o \
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
./src/MB11.o \
./src/MB12.o \
./src/Mb7.o \
./src/mb0a.o \
./src/mb0b.o 

src/%.o: ../src/%.F
	@echo 'Building file: $<'
	@echo 'Invoking: GNU Fortran Compiler'
	$(FC) -c -D___PSGExpRess $(FPICFLAGS) $(FFLAGS) $(ADD_FFLAGS) $(ADD_MFLAG) -o "$@" "$<"
	@echo 'Finished building: $<'
	@echo ' '


inttypes.mod: src/IntelInterf.o
intelinterf.mod: src/IntelInterf.o inttypes.mod

	
src/IntelInterf.o: ../src/IntelInterf.F

src/MB2.o: ../src/MB2.F intelinterf.mod inttypes.mod

src/MB31.o: ../src/MB31.F

src/MB32.o: ../src/MB32.F

src/MB33.o: ../src/MB33.F

src/MB34.o: ../src/MB34.F

src/MB35.o: ../src/MB35.F

src/MB42A.o: ../src/MB42A.F

src/MB5.o: ../src/MB5.F

src/MB5_vk.o: ../src/MB5_vk.F

src/MB6.o: ../src/MB6.F

src/MB8.o: ../src/MB8.F

src/Mb1a.o: ../src/Mb1a.F

src/Mb1b.o: ../src/Mb1b.F

src/Mb42a_VK.o: ../src/Mb42a_VK.F

src/MB11.o: ../src/MB11.F src/IntelInterf.o

src/MB12.o: ../src/MB12.F src/IntelInterf.o

src/Mb7.o: ../src/Mb7.F src/IntelInterf.o

src/mb0a.o: ../src/mb0a.F src/IntelInterf.o

src/mb0b.o: ../src/mb0b.F src/IntelInterf.o


