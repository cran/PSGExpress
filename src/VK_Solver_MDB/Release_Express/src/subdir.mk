

FOR_SRCS = \
../src/IntelInterf.F \
../src/FuncNames.F \
../src/M1.F \
../src/CiFort48_Interf.F \
../src/Approx_CVaRs.F \
../src/Cardinality.F \
../src/CheckFunctions_I.F \
../src/CheckFunctions_II.F \
../src/ExtractMultiConstraints_I.F \
../src/ExtractMultiConstraints_II.F \
../src/FM_Multi_One.F \
../src/FormInitDataList_for_PSG_I.F \
../src/FormInitDataList_for_PSG_II.F \
../src/FUN_ALL_Set_I.F \
../src/FUN_ALL_Set_II.F \
../src/GRAD_ALL_Stas_I.F \
../src/GRAD_ALL_Stas_II.F \
../src/Grad_Draw.F \
../src/Grad_MaxFunc.F \
../src/Inh_Dll.F \
../src/Ishtvan_start_Box.F \
../src/L1_L2_All.F \
../src/Linearization_J.F \
../src/New_CVAR_FUN.F \
../src/New_Ordering.F \
../src/Problem_Init_Sp.F \
../src/PSHEN_Q_NON_I.F \
../src/PSHEN_Q_NON_II.F \
../src/QLD_my_M_L_I.F \
../src/QLD_my_M_L_II.F \
../src/QLD_my_M_L_III.F \
../src/Read_Nabor_Two_I.F \
../src/Read_Nabor_Two_II.F \
../src/Read_Nabor_Two_III.F \
../src/Read_Task_Set_I.F \
../src/Read_Task_Set_II.F \
../src/rvk_Inh.F \
../src/rvk_St.F \
../src/SecondStageProblem+pp_2.F \
../src/SolveCycleOfProblems_I.F \
../src/SolveCycleOfProblems_II.F \
../src/SolveOneProblem.F \
../src/Sorting.F \
../src/Subr_for_Problem_Init.F \
../src/Vk_fun_G_I.F \
../src/Vk_fun_G_II.F \
../src/VK_START_WDB.F \
../src/GPSR_Ext_vk.F \
../src/GPSR_vk.F \
../src/LinuxIntel-Functions.F

OBJS = \
./src/IntelInterf.o \
./src/FuncNames.o \
./src/M1.o \
./src/CiFort48_Interf.o \
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

src/%.o: ../src/%.F
	@echo 'Building file: $<'
	@echo 'Invoking: GNU Fortran Compiler'
	$(FC) -c -D___PSGExpRess -DNO_GUROBI $(FPICFLAGS) $(FFLAGS) $(ADD_FFLAGS) $(ADD_MFLAG) -o "$@" "$<"
	@echo 'Finished building: $<'
	@echo ' '


inttypes.mod: src/IntelInterf.o
intelinterf.mod: src/IntelInterf.o
funcnames.mod: src/FuncNames.o
modcommons.mod: src/M1.o
cifort.mod: src/CiFort48_Interf.o


src/IntelInterf.o: ../src/IntelInterf.F

src/FuncNames.o: ../src/FuncNames.F

src/M1.o: ../src/M1.F src/IntelInterf.o src/FuncNames.o intelinterf.mod inttypes.mod funcnames.mod

src/CiFort48_Interf.o: ../src/CiFort48_Interf.F src/IntelInterf.o src/FuncNames.o intelinterf.mod inttypes.mod funcnames.mod

src/Approx_CVaRs.o: ../src/Approx_CVaRs.F src/FuncNames.o funcnames.mod

src/Cardinality.o: ../src/Cardinality.F src/IntelInterf.o src/M1.o intelinterf.mod inttypes.mod modcommons.mod

src/CheckFunctions_I.o: ../src/CheckFunctions_I.F src/CiFort48_Interf.o src/M1.o cifort.mod modcommons.mod

src/CheckFunctions_II.o: ../src/CheckFunctions_II.F src/CiFort48_Interf.o src/M1.o cifort.mod modcommons.mod

src/ExtractMultiConstraints_I.o: ../src/ExtractMultiConstraints_I.F src/CiFort48_Interf.o src/FuncNames.o src/M1.o cifort.mod modcommons.mod funcnames.mod

src/ExtractMultiConstraints_II.o: ../src/ExtractMultiConstraints_II.F src/CiFort48_Interf.o src/FuncNames.o src/M1.o cifort.mod modcommons.mod funcnames.mod

src/FM_Multi_One.o: ../src/FM_Multi_One.F src/CiFort48_Interf.o src/FuncNames.o src/M1.o  cifort.mod modcommons.mod funcnames.mod

src/FormInitDataList_for_PSG_I.o: ../src/FormInitDataList_for_PSG_I.F src/IntelInterf.o src/CiFort48_Interf.o src/FuncNames.o src/M1.o cifort.mod modcommons.mod funcnames.mod intelinterf.mod inttypes.mod

src/FormInitDataList_for_PSG_II.o: ../src/FormInitDataList_for_PSG_II.F src/IntelInterf.o src/CiFort48_Interf.o src/FuncNames.o src/M1.o cifort.mod modcommons.mod funcnames.mod intelinterf.mod inttypes.mod

src/FUN_ALL_Set_I.o: ../src/FUN_ALL_Set_I.F src/CiFort48_Interf.o src/FuncNames.o src/M1.o funcnames.mod cifort.mod modcommons.mod

src/FUN_ALL_Set_II.o: ../src/FUN_ALL_Set_II.F src/CiFort48_Interf.o src/FuncNames.o src/M1.o funcnames.mod cifort.mod modcommons.mod

src/GRAD_ALL_Stas_I.o: ../src/GRAD_ALL_Stas_I.F src/CiFort48_Interf.o src/M1.o cifort.mod modcommons.mod

src/GRAD_ALL_Stas_II.o: ../src/GRAD_ALL_Stas_II.F src/CiFort48_Interf.o src/M1.o cifort.mod modcommons.mod

src/Grad_Draw.o: ../src/Grad_Draw.F

src/Grad_MaxFunc.o: ../src/Grad_MaxFunc.F src/CiFort48_Interf.o src/M1.o cifort.mod modcommons.mod

src/Inh_Dll.o: ../src/Inh_Dll.F src/M1.o modcommons.mod

src/Ishtvan_start_Box.o: ../src/Ishtvan_start_Box.F src/IntelInterf.o src/CiFort48_Interf.o src/M1.o cifort.mod modcommons.mod intelinterf.mod inttypes.mod

src/L1_L2_All.o: ../src/L1_L2_All.F src/CiFort48_Interf.o cifort.mod

src/Linearization_J.o: ../src/Linearization_J.F src/CiFort48_Interf.o src/M1.o cifort.mod modcommons.mod

src/New_CVAR_FUN.o: ../src/New_CVAR_FUN.F

src/New_Ordering.o: ../src/New_Ordering.F

src/Problem_Init_Sp.o: ../src/Problem_Init_Sp.F src/CiFort48_Interf.o src/FuncNames.o src/M1.o funcnames.mod cifort.mod modcommons.mod

src/PSHEN_Q_NON_I.o: ../src/PSHEN_Q_NON_I.F src/IntelInterf.o src/M1.o intelinterf.mod inttypes.mod modcommons.mod

src/PSHEN_Q_NON_II.o: ../src/PSHEN_Q_NON_II.F src/IntelInterf.o src/M1.o intelinterf.mod inttypes.mod modcommons.mod

src/QLD_my_M_L_I.o: ../src/QLD_my_M_L_I.F src/CiFort48_Interf.o cifort.mod

src/QLD_my_M_L_II.o: ../src/QLD_my_M_L_II.F src/CiFort48_Interf.o cifort.mod

src/QLD_my_M_L_III.o: ../src/QLD_my_M_L_III.F src/CiFort48_Interf.o cifort.mod

src/Read_Nabor_Two_I.o: ../src/Read_Nabor_Two_I.F src/IntelInterf.o src/CiFort48_Interf.o src/M1.o cifort.mod modcommons.mod

src/Read_Nabor_Two_II.o: ../src/Read_Nabor_Two_II.F src/IntelInterf.o src/CiFort48_Interf.o src/M1.o cifort.mod modcommons.mod intelinterf.mod inttypes.mod

src/Read_Nabor_Two_III.o: ../src/Read_Nabor_Two_III.F src/IntelInterf.o src/CiFort48_Interf.o src/M1.o cifort.mod modcommons.mod intelinterf.mod inttypes.mod

src/Read_Task_Set_I.o: ../src/Read_Task_Set_I.F src/IntelInterf.o src/CiFort48_Interf.o src/FuncNames.o src/M1.o cifort.mod modcommons.mod funcnames.mod intelinterf.mod inttypes.mod

src/Read_Task_Set_II.o: ../src/Read_Task_Set_II.F src/IntelInterf.o src/CiFort48_Interf.o src/FuncNames.o src/M1.o cifort.mod modcommons.mod funcnames.mod intelinterf.mod inttypes.mod

src/rvk_Inh.o: ../src/rvk_Inh.F src/IntelInterf.o src/M1.o modcommons.mod intelinterf.mod inttypes.mod

src/rvk_St.o: ../src/rvk_St.F src/M1.o modcommons.mod

src/SecondStageProblem+pp_2.o: ../src/SecondStageProblem+pp_2.F src/CiFort48_Interf.o src/M1.o cifort.mod modcommons.mod

src/SolveCycleOfProblems_I.o: ../src/SolveCycleOfProblems_I.F src/IntelInterf.o src/CiFort48_Interf.o src/M1.o cifort.mod modcommons.mod intelinterf.mod inttypes.mod

src/SolveCycleOfProblems_II.o: ../src/SolveCycleOfProblems_II.F src/IntelInterf.o src/CiFort48_Interf.o src/M1.o cifort.mod modcommons.mod intelinterf.mod inttypes.mod

src/SolveOneProblem.o: ../src/SolveOneProblem.F src/IntelInterf.o src/CiFort48_Interf.o src/M1.o cifort.mod modcommons.mod intelinterf.mod inttypes.mod

src/Sorting.o: ../src/Sorting.F src/IntelInterf.o intelinterf.mod inttypes.mod

src/Subr_for_Problem_Init.o: ../src/Subr_for_Problem_Init.F src/IntelInterf.o src/CiFort48_Interf.o src/M1.o

src/Vk_fun_G_I.o: ../src/Vk_fun_G_I.F src/IntelInterf.o src/M1.o cifort.mod modcommons.mod

src/Vk_fun_G_II.o: ../src/Vk_fun_G_II.F src/IntelInterf.o src/M1.o cifort.mod modcommons.mod

src/VK_START_WDB.o: ../src/VK_START_WDB.F src/IntelInterf.o src/CiFort48_Interf.o src/M1.o cifort.mod modcommons.mod intelinterf.mod inttypes.mod 

src/GPSR_Ext_vk.o: ../src/GPSR_Ext_vk.F

src/GPSR_vk.o: ../src/GPSR_vk.F src/CiFort48_Interf.o cifort.mod

src/LinuxIntel-Functions.o: ../src/LinuxIntel-Functions.F src/IntelInterf.o src/CiFort48_Interf.o cifort.mod intelinterf.mod inttypes.mod
