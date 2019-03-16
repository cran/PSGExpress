

FOR_SRCS = \
../src/IntelInterf.for \
../src/CiFort48_Interf.for \
../src/FuncNames.for \
../src/M1.for \
../src/Approx_CVaRs.for \
../src/Cardinality.for \
../src/CheckFunctions_I.for \
../src/CheckFunctions_II.for \
../src/ExtractMultiConstraints_I.for \
../src/ExtractMultiConstraints_II.for \
../src/FM_Multi_One.for \
../src/FormInitDataList_for_PSG_I.for \
../src/FormInitDataList_for_PSG_II.for \
../src/FUN_ALL_Set_I.for \
../src/FUN_ALL_Set_II.for \
../src/GRAD_ALL_Stas_I.for \
../src/GRAD_ALL_Stas_II.for \
../src/Grad_Draw.for \
../src/Grad_MaxFunc.for \
../src/Inh_Dll.for \
../src/Ishtvan_start_Box.for \
../src/L1_L2_All.for \
../src/Linearization_J.for \
../src/New_CVAR_FUN.for \
../src/New_Ordering.for \
../src/Problem_Init_Sp.for \
../src/PSHEN_Q_NON_I.for \
../src/PSHEN_Q_NON_II.for \
../src/QLD_my_M_L_I.for \
../src/QLD_my_M_L_II.for \
../src/QLD_my_M_L_III.for \
../src/Read_Nabor_Two_I.for \
../src/Read_Nabor_Two_II.for \
../src/Read_Nabor_Two_III.for \
../src/Read_Task_Set_I.for \
../src/Read_Task_Set_II.for \
../src/rvk_Inh.for \
../src/rvk_St.for \
../src/SecondStageProblem+pp_2.for \
../src/SolveCycleOfProblems_I.for \
../src/SolveCycleOfProblems_II.for \
../src/SolveOneProblem.for \
../src/Sorting.for \
../src/Subr_for_Problem_Init.for \
../src/Vk_fun_G_I.for \
../src/Vk_fun_G_II.for \
../src/VK_START_WDB.for \
../src/GPSR_Ext_vk.f90 \
../src/GPSR_vk.f90 \
../src/LinuxIntel-Functions.for


OBJS = \
./src/IntelInterf.o \
./src/CiFort48_Interf.o \
./src/FuncNames.o \
./src/M1.o \
./src/Approx_CVaRs.o \
./src/Cardinality.o \
./src/CheckFunctions_I.o \
./src/CheckFunctions_II.o \
./src/ExtractMultiConstraints_I.o \
./src/ExtractMultiConstraints_II.o \
./src/FM_Multi_One.o \
./src/FormInitDataList_for_PSG_I.o \
./src/FormInitDataList_for_PSG_II.o \
./src/FUN_ALL_Set_I.o \
./src/FUN_ALL_Set_II.o \
./src/GRAD_ALL_Stas_I.o \
./src/GRAD_ALL_Stas_II.o \
./src/Grad_Draw.o \
./src/Grad_MaxFunc.o \
./src/Inh_Dll.o \
./src/Ishtvan_start_Box.o \
./src/L1_L2_All.o \
./src/Linearization_J.o \
./src/New_CVAR_FUN.o \
./src/New_Ordering.o \
./src/Problem_Init_Sp.o \
./src/PSHEN_Q_NON_I.o \
./src/PSHEN_Q_NON_II.o \
./src/QLD_my_M_L_I.o \
./src/QLD_my_M_L_II.o \
./src/QLD_my_M_L_III.o \
./src/Read_Nabor_Two_I.o \
./src/Read_Nabor_Two_II.o \
./src/Read_Nabor_Two_III.o \
./src/Read_Task_Set_I.o \
./src/Read_Task_Set_II.o \
./src/rvk_Inh.o \
./src/rvk_St.o \
./src/SecondStageProblem+pp_2.o \
./src/SolveCycleOfProblems_I.o \
./src/SolveCycleOfProblems_II.o \
./src/SolveOneProblem.o \
./src/Sorting.o \
./src/Subr_for_Problem_Init.o \
./src/Vk_fun_G_I.o \
./src/Vk_fun_G_II.o \
./src/VK_START_WDB.o \
./src/GPSR_Ext_vk.o \
./src/GPSR_vk.o \
./src/LinuxIntel-Functions.o



src/%.o: ../src/%.for
	@echo 'Building file: $<'
	@echo 'Invoking: GNU Fortran Compiler'
#	gfortran -D___PSGExpRess \
#	-fcray-pointer -fno-underscoring -O3 -c -fPIC -Wno-tabs -fdollar-ok -fmessage-length=0 \
#	-fd-lines-as-comments -ffixed-line-length-132 -fdefault-double-8 -fdefault-real-8 -cpp \
#	-o "$@" "$<"

	$(FC) -c -D___PSGExpRess -DNO_GUROBI $(FPICFLAGS) $(FFLAGS) $(ADD_FFLAGS) -o "$@" "$<"

#	$(FC) $(FPICFLAGS) $(FFLAGS) -D___PSGExpRess \
#	-fcray-pointer -fno-underscoring -c  -Wno-tabs -fdollar-ok \
#	-fd-lines-as-comments -ffixed-line-length-132 -fdefault-double-8 -fdefault-real-8 -cpp \
#	-o "$@" "$<"
	@echo 'Finished building: $<'
	@echo ' '




src/IntelInterf.o: ../src/IntelInterf.for

/src/CiFort48_Interf.o: ../src/CiFort48_Interf.for src/IntelInterf.o

/src/FuncNames.o: ../src/FuncNames.for

/src/M1.o: ../src/M1.for src/IntelInterf.o src/CiFort48_Interf.o /src/FuncNames.o

/src/Approx_CVaRs.o: ../src/Approx_CVaRs.for /src/FuncNames.o

/src/Cardinality.o: ../src/Cardinality.for src/IntelInterf.o /src/M1.o

/src/CheckFunctions_I.o: ../src/CheckFunctions_I.for src/CiFort48_Interf.o /src/M1.o

/src/CheckFunctions_II.o: ../src/CheckFunctions_II.for src/CiFort48_Interf.o /src/M1.o

/src/ExtractMultiConstraints_I.o: ../src/ExtractMultiConstraints_I.for src/CiFort48_Interf.o /src/FuncNames.o /src/M1.o

/src/ExtractMultiConstraints_II.o: ../src/ExtractMultiConstraints_II.for src/CiFort48_Interf.o /src/FuncNames.o /src/M1.o

/src/FM_Multi_One.o: ../src/FM_Multi_One.for src/CiFort48_Interf.o /src/FuncNames.o /src/M1.o

/src/FormInitDataList_for_PSG_I.o: ../src/FormInitDataList_for_PSG_I.for src/IntelInterf.o src/CiFort48_Interf.o /src/FuncNames.o /src/M1.o

/src/FormInitDataList_for_PSG_II.o: ../src/FormInitDataList_for_PSG_II.for src/IntelInterf.o src/CiFort48_Interf.o /src/FuncNames.o /src/M1.o

/src/FUN_ALL_Set_I.o: ../src/FUN_ALL_Set_I.for src/CiFort48_Interf.o /src/FuncNames.o /src/M1.o

/src/FUN_ALL_Set_II.o: ../src/FUN_ALL_Set_II.for src/CiFort48_Interf.o /src/FuncNames.o /src/M1.o

/src/GRAD_ALL_Stas_I.o: ../src/GRAD_ALL_Stas_I.for src/CiFort48_Interf.o /src/M1.o

/src/GRAD_ALL_Stas_II.o: ../src/GRAD_ALL_Stas_II.for src/CiFort48_Interf.o /src/M1.o

/src/Grad_Draw.o: ../src/Grad_Draw.for

/src/Grad_MaxFunc.o: ../src/Grad_MaxFunc.for src/CiFort48_Interf.o /src/M1.o

/src/Inh_Dll.o: ../src/Inh_Dll.for /src/M1.o

/src/Ishtvan_start_Box.o: ../src/Ishtvan_start_Box.for src/IntelInterf.o src/CiFort48_Interf.o /src/M1.o

/src/L1_L2_All.o: ../src/L1_L2_All.for src/CiFort48_Interf.o

/src/Linearization_J.o: ../src/Linearization_J.for src/CiFort48_Interf.o /src/M1.o

/src/New_CVAR_FUN.o: ../src/New_CVAR_FUN.for

/src/New_Ordering.o: ../src/New_Ordering.for

/src/Problem_Init_Sp.o: ../src/Problem_Init_Sp.for src/CiFort48_Interf.o /src/FuncNames.o /src/M1.o

/src/PSHEN_Q_NON_I.o: ../src/PSHEN_Q_NON_I.for src/IntelInterf.o /src/M1.o

/src/PSHEN_Q_NON_II.o: ../src/PSHEN_Q_NON_II.for src/IntelInterf.o /src/M1.o

/src/QLD_my_M_L_I.o: ../src/QLD_my_M_L_I.for src/CiFort48_Interf.o

/src/QLD_my_M_L_II.o: ../src/QLD_my_M_L_II.for src/CiFort48_Interf.o

/src/QLD_my_M_L_III.o: ../src/QLD_my_M_L_III.for src/CiFort48_Interf.o

/src/Read_Nabor_Two_I.o: ../src/Read_Nabor_Two_I.for src/IntelInterf.o src/CiFort48_Interf.o /src/M1.o

/src/Read_Nabor_Two_II.o: ../src/Read_Nabor_Two_II.for src/IntelInterf.o src/CiFort48_Interf.o /src/M1.o

/src/Read_Nabor_Two_III.o: ../src/Read_Nabor_Two_III.for src/IntelInterf.o src/CiFort48_Interf.o /src/M1.o

/src/Read_Task_Set_I.o: ../src/Read_Task_Set_I.for src/IntelInterf.o src/CiFort48_Interf.o /src/FuncNames.o /src/M1.o

/src/Read_Task_Set_II.o: ../src/Read_Task_Set_II.for src/IntelInterf.o src/CiFort48_Interf.o /src/FuncNames.o /src/M1.o

/src/rvk_Inh.o: ../src/rvk_Inh.for src/IntelInterf.o /src/M1.o

/src/rvk_St.o: ../src/rvk_St.for /src/M1.o

/src/SecondStageProblem+pp_2.o: ../src/SecondStageProblem+pp_2.for src/CiFort48_Interf.o /src/M1.o

/src/SolveCycleOfProblems_I.o: ../src/SolveCycleOfProblems_I.for src/IntelInterf.o src/CiFort48_Interf.o /src/M1.o

/src/SolveCycleOfProblems_II.o: ../src/SolveCycleOfProblems_II.for src/IntelInterf.o src/CiFort48_Interf.o /src/M1.o

/src/SolveOneProblem.o: ../src/SolveOneProblem.for src/IntelInterf.o src/CiFort48_Interf.o /src/M1.o

/src/Sorting.o: ../src/Sorting.for src/IntelInterf.o

/src/Subr_for_Problem_Init.o: ../src/Subr_for_Problem_Init.for src/IntelInterf.o src/CiFort48_Interf.o /src/M1.o

/src/Vk_fun_G_I.o: ../src/Vk_fun_G_I.for src/IntelInterf.o /src/M1.o

/src/Vk_fun_G_II.o: ../src/Vk_fun_G_II.for src/IntelInterf.o /src/M1.o

/src/VK_START_WDB.o: ../src/VK_START_WDB.for src/IntelInterf.o src/CiFort48_Interf.o /src/M1.o

/src/GPSR_Ext_vk.o: ../src/GPSR_Ext_vk.f90

/src/GPSR_vk.o: ../src/GPSR_vk.f90 /src/CiFort48_Interf.o

/src/LinuxIntel-Functions.o: ../src/LinuxIntel-Functions.for src/IntelInterf.o src/CiFort48_Interf.o
