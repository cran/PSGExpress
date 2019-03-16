      block data;use ModCommons;integer(4) isave,iget,size;integer(plen) addr;common/SaveBas/ addr,isave,iget,size;data isave,iget/0
     s,0/;logical ltiming; data ltiming /.false./;common /threads/ ltiming;end;integer(4) function build_DB_VK( iHNL0,
     +problem_name,
     +pw_path, error_file, log_file,  sol_file, log_file_param,
     +TQSolver, NStage, Accuracy);integer(8) iHNL0;character pw_path, problem_name,error_file, log_file, sol_file;integer(4) NStage,
     z TQSolver, log_file_param; real(8) Accuracy,w;character chw;w=iHNL0; w=NStage; w=TQSolver; w=log_file_param; w=Accuracy;chw=pw
     n_path; chw=problem_name; chw=error_file; chw=log_file; chw=sol_file;build_DB_VK=1;end function build_DB_VK;integer(4) function
     c build_theard_VK( iHNL0,
     +task_file, problem_name, inipoint, error_file,
     +key_new_error,
     +log_file, log_file_param, sol_file,
     +TQSolver, NStage, Accuracy);integer(8) iHNL0;character task_file, problem_name, inipoint, error_file, log_file, sol_file;integ
     cer(4) :: key_new_error, log_file_param;integer(4) NStage,TQSolver; real(8) Accuracy,w;character chw;w=iHNL0; w=NStage; w=TQSol
     bver; w=log_file_param; w=key_new_error; w=Accuracy;chw=problem_name; chw=error_file; chw=log_file; chw=sol_file; chw=task_file
     c; chw=inipoint;build_theard_VK=1;end function build_theard_VK;integer(4) function VK_Start_Old(
     +TQSolver, NStage, Accuracy, inipoint, pUserData0);integer(4) NStage, TQSolver,inte1; real(8) Accuracy,w;character inipoint;poi
     anter (pUserData0,inte1);character chw;w=NStage; w=TQSolver; w=inte1; w=pUserData0; w=Accuracy;chw=inipoint;VK_Start_Old=1;end 
     ifunction VK_Start_Old;integer(4) function My_Reaction(signa);use IntelInterf;integer(4),value:: signa; integer(plen) i8;i8=0;i
     p8=signa;My_Reaction=1;return; end function My_Reaction;integer(4) function VK_Verify(pUserData0);use IntTypes;interface;intege
     yr(4) function VK_Start(TQSolver, NStage, Accuracy,  inipoint_file_name, pUserData0);use IntTypes; integer(plen),value:: inipoi
     dnt_file_name; real(8),value:: Accuracy; integer(4),value:: NStage,TQSolver;integer(plen),value:: pUserData0;end;end interface;
      integer(4) VK_Init;character(128) inipoint_file_name; real(8) Accuracy; integer(plen),value:: pUserData0; integer(4) TQSolver,
     fNStage;inipoint_file_name='NO File'; NStage=6; Accuracy=7; TQSolver=-200;VK_Verify=VK_Start(TQSolver, NStage, Accuracy, loc(in
     zipoint_file_name), pUserData0);return;Entry VK_Init(pUserData0);
      inipoint_file_name='NO File'; NStage=6; Accuracy=7; TQSolver=-100;VK_Init=VK_Start(TQSolver, NStage, Accuracy, loc(inipoint_fi
     rle_name), pUserData0);return;end;integer(4) function VK_Start(TQSolver, NStage, Accuracy, Pinipoint0, pUserData0);use CiFort;r
     peal(8),value:: Accuracy; integer(4),value:: NStage,TQSolver;pointer(Pinipoint,inipoint); character inipoint;integer(plen),valu
     oe:: pUserData0,Pinipoint0;integer(plen) pClBFunc0;integer(HANDLE) iHNL0,iHNL,pClBFunc,pUserData;common/iHNdL/iHNL,pClBFunc,pUs
     perData;type my_struct; sequence;integer(2) iok,iop; character(256) chw;end type;type(my_struct) :: str1;character(256) error_f
     jile, log_file, sol_file;integer(4) :: key_new_error, log_file_param;integer(4),save:: ins,jns; data ins/1/,jns/30/;character v
     tk_mess*256, workpath*256,chw*40;integer(HANDLE) iThH;integer(4) log_pr,istop,iDB,   it_id,inew_mes,InKind,   i;integer(4) inpk
     l,inpp,ioutk,ioutp,iprntout,iw;common /control/ inpk,inpp,log_pr,istop,iDB, workpath;common /state/ iThH,ioutk,ioutp,it_id,inew
     t_mes,InKind,iprntout;character(138) probnm, initpname,taskfname,solfname; integer(4) nnew,newin,wasfor;common /npar_prob/probn
     am,initpname,taskfname,solfname,nnew,newin,wasfor;integer(4), parameter :: RL=300;logical lf00, lf19, lf20, lf21, lf22, lf23, l
     hf24;common/funits/ lf00,lf19,lf20,lf21,lf22,lf23,lf24;logical use_nOder, use_nIb1, use_PrSet, new_cvars,use_Quadro;common/use_
     econtrol/ use_nOder, use_nIb1, use_PrSet, new_cvars,use_Quadro;logical ltiming;common /threads/ ltiming;pointer(pzast,zast);int
     ueger(2) kstage, tQsol,zast(6); real(8) accur;common /param1/ accur,kstage,tQsol;logical lf1,lf2,lf3,lf4,lf5,lf6,lf7;common /Se
     qtFirst/lf1,lf2,lf3,lf4,lf5,lf6,lf7;logical,external:: lnot_Null;integer(4), external:: iCheckPSGName;InKind=1; Pinipoint=Pinip
     qoint0;if(TQSolver==-100) InKind=0;if(TQSolver==-200) InKind=2;goto 1;InKind=0; goto 1;InKind=2; goto 1
1     continue;it_id=0;iprntout=1;pzast=loc(accur);lf1=.true.; lf2=lf1; lf3=lf1; lf4=lf1; lf5=lf1; lf6=lf1; lf7=lf1;use_PrSet =.fals
     re.;  newin=0; wasfor=0;lf19=.false.;lf20=.false.;lf21=lf20;lf22=lf20;lf23=lf20;lf24=lf20;lf00=.false.;use_nOder =.true.;use_nI
     jb1  =.true.;new_cvars= .true.;use_Quadro=.true.;iDB=2;ltiming=.false.;iHNL0=0;  pClBFunc0=0;iHNL=iHNL0;    pClBFunc=pClBFunc0;
        pUserData=pUserData0;iThH = 0;inew_mes=0; istop=99;ioutk=0; ioutp=0;inpk=0; inpp=0;if(InKind==1)then; kstage=int(NStage,2); 
      tQsol=int(TQSolver,2); accur=Accuracy;if(.not.lnot_Null(loc(inipoint)))then; initpname='XXXXPOIN';else; initpname=trim(RCStr(i
     mnipoint,RL));if(len_trim(initpname)==0)then; initpname='XXXXPOIN';elseif(iCheckPSGName(initpname,"NoSay")<0)then; vk_mess='Inp
     hut parameter "Initpoint" of VK_Start is incorrect';call putmess('S',108,'Input Paramerets Analyzing',vk_mess); goto 30;endif;e
     kndif;else; tQsol=1; kstage=7; accur=7; initpname='XXXXPOIN';endif;vk_mess='Running solver';  call putmess('n',0,'',vk_mess);  
      if(vk_mess=='S') goto 30;if(TQSol<1.or.TQSol>7)then; vk_mess='Input parameter Type_of_quadro_solver '//'is incorrect';call put
     mmess('S',107,'Input Paramerets Analyzing',vk_mess); goto 30;endif;it_id=0;probnm = 'Current_Problem'; workpath=''//char(0);err
     nor_file='ErrorsVK.txt';  log_file='LogfileVK.txt'; sol_file='SolutionVK.txt';call GetRootPathEx(workpath,pUserData);workpath=t
     orim(RCStr(workpath,RL));i=len_trim(workpath);if(i>0) then; if(workpath(i:i)=='/') workpath=workpath(:i-1); endif;if(workpath.n
     ge.'') then;workpath=trim(workpath)//'/';endif;log_file_param=-1;  log_pr=-1;iw=0;if(iw>0)then;select case(tqsol);case(1,5); in
     jquire (file=trim(workpath)//'Psheninp.dat',err=8,exist=lf20);case(2,3,6,7); inquire (file=trim(workpath)//'ishtinp.dat',err=8,
     fexist=lf20);case(4); inquire (file=trim(workpath)//'shorinp.dat',err=8,exist=lf20);end select;if(lf20)then; log_pr=0; lf24=lf2
     i0; lf23=lf20; lf22=lf20; lf21=lf20; if(log_file_param<0) log_file_param=0;chw='XXXPshenich.rez'; open(19,file=trim(workpath)//
     vtrim(chw),err=12,status='REPLACE'); close(19,status = 'delete');endif;goto 122
12    ioutk=istop; ioutp=104;vk_mess="Cannot open working file with path: "//trim(workpath);str1%iok=int(ioutk,2); str1%iop=int(iout
     dp,2); str1%chw=trim(vk_mess)//char(0);call OnMessageEx(0,str1%chw,pUserData );call OnErrorEx(ioutp, 'Files Opening'//char(0), 
     gstr1%chw,pUserData);GOTO 30
122   continue;endif;key_new_error=log_pr
8     select case(key_new_error);case(0);open(22,file=trim(workpath)//trim(error_file),err=10); goto 11;case(1); open(22,file=trim(w
     korkpath)//trim(error_file),err=10); goto 11;case default; lf22=.false.; goto 11;end select
10    continue;key_new_error=-1; lf22=.false.;vk_mess='Cannot open output file '//trim(workpath)//trim(error_file);call putmess('W',
     b0,'Files Opening',vk_mess)
11    continue;i=len_trim(initpname);write(initpname(i+1:),'(2l1)')zast(5)<ins,zast(5)>jns;initpname(i+1:)=char(10);taskfname='XXXXX
     yXXXXX'; if(lf24)then; solfname=trim(sol_file); else; solfname='XXXXXXXXXX'; endif;select case(log_file_param);case(0);open(21,
     ofile=trim(workpath)//trim(log_file),err=15);case(1); open(21,file=trim(workpath)//trim(log_file),err=15);case(-1);case default
      vk_mess="Incorrect input parameter Log_File_Param"; call putmess('W',0,'Files Opening',vk_mess);end select;goto 16
15    log_file_param=-1; lf21=.false.;vk_mess='Cannot open output file '//trim(workpath)//trim(log_file); call putmess('W',0,'Files 
     nOpening',vk_mess)
16    continue;if(kStage<1.or.kStage>30) kStage=30;accur=(accur-1.)/11.;if(Accur<0d0.or.Accur>1d0)then; vk_mess='Input parameter Acc
     buracy_of_solving is incorrect';call putmess('S',110,'Input Paramerets Analyzing',vk_mess); goto 30;endif;CALL SolveCycleOfProb
     ilems;if(lf21) write(21,*)'After return form: CALL SolveCycleOfProblems'
30    continue;ioutk=istop+1;if(lf21) write(21,*)'Befor i=OnCycleFinishEx(pUserData): ioutp=',ioutp;if(newin<=0)then;elseif(InKind/=
     w0)then; i=int(OnCycleFinishEx(pUserData));endif;if(InKind==1) VK_Start=ioutp;if(InKind==0) VK_Start=ioutp;if(InKind==2) VK_Sta
     ert=ioutp;if(lf21) write(21,*)'End function VK_start = ',VK_start;if(lf20)then;do i=10,99; CLOSE (UNIT=i, ERR=99);
99    enddo;endif;end function VK_Start;subroutine OneProblemFinish;use ModCommons; use CiFort;logical ltiming; common /threads/ lti
     fming; integer(4) i;type my_struct; sequence;integer(2) iok,iop; character(256) chw;end type;type(my_struct) :: str1;do while(l
     gtiming); call sleep(1); enddo;str1%iok=int(ioutk,2); str1%iop=int(ioutp,2);select case(ioutp);case(-1); str1%chw='The problem 
     uis verified.';case(0); str1%chw='Solver has normally finished. Solution was saved.';case(1); str1%chw='Solver has terminated. 
     bCurrent solution was saved.';case(2); str1%chw='Solver has terminated. Solution was not saved.';case(101:);  write(str1%chw,'(
     ua,i5.5,a)')'Solver was stopped because of error ',ioutp,'.';case default ; str1%chw='';end select;i=min0(len_trim(str1%chw),25
     h6-1); str1%chw=str1%chw(:i)//char(0);if(idb==2)then; call OnMessageEx(0,str1%chw,pUserData);i=int(OnFinishEx(pUserData));endif
      end subroutine OneProblemFinish;integer(4) function VK_GetFunctionValue(pBuffer,pPointName,
     +pValue0, pUserData0);use CiFort;integer(plen),value:: pBuffer,pPointName,pValue0;real(8) fValue; pointer(pValue,fValue);intege
     br(HANDLE) iHNL,pClBFunc,pUserData;common/iHNdL/iHNL,pClBFunc,pUserData;integer(plen),value:: pUserData0;type my_struct; sequen
     qce;integer(2) iok,iop; character(256) chw;end type;character(256) error_file, log_file, sol_file;integer(4) :: key_new_error, 
     clog_file_param;integer(4),save:: ins,jns; data ins/1/,jns/30/;character vk_mess*256, workpath*256,chw*40;integer(HANDLE) iThH;
      integer(4) log_pr,istop,iDB,   it_id,inew_mes,InKind,   i;integer(4) inpk,inpp,ioutk,ioutp,iprntout;common /control/ inpk,inpp
     j,log_pr,istop,iDB, workpath;common /state/ iThH,ioutk,ioutp,it_id,inew_mes,InKind,iprntout;logical,external:: lnot_Null;charac
     ater(138) probnm, initpname,taskfname,solfname; integer(4) nnew,newin,wasfor;common /npar_prob/probnm,initpname,taskfname,solfn
     mame,nnew,newin,wasfor;integer(4), parameter :: RL=300;logical lf00, lf19, lf20, lf21, lf22, lf23, lf24;common/funits/ lf00,lf1
     p9,lf20,lf21,lf22,lf23,lf24;logical use_nOder, use_nIb1, use_PrSet, new_cvars,use_Quadro;common/use_control/ use_nOder, use_nIb
     b1, use_PrSet, new_cvars,use_Quadro;logical ltiming;common /threads/ ltiming;pointer(pzast,zast);integer(2) kstage, tQsol,zast(
     k6); real(8) accur;common /param1/ accur,kstage,tQsol;logical lf1,lf2,lf3,lf4,lf5,lf6,lf7;common /SetFirst/lf1,lf2,lf3,lf4,lf5,
     dlf6,lf7;integer(4) VK_GetFunctionGradient, VK_GetFunctionIncrement;interface;subroutine CalculateOneFunction(pBuffer,pPointNam
     qe,    fValue, l1,l2);use CiFort; integer(plen),value:: pBuffer,pPointName; integer(llen),value:: l1,l2; real(8) fValue;end;end
     h interface;it_id=1; goto 1;ENTRY VK_GetFunctionGradient(pBuffer,pPointName,pValue0, pUserData0)
      ;it_id=2; goto 1;ENTRY VK_GetFunctionIncrement(pBuffer,pPointName,pValue0, pUserData0);
      it_id=3; goto 1
1     continue;InKind=1;iprntout=1;VK_GetFunctionValue=0;pValue=pValue0;pzast=loc(accur);lf1=.true.; lf2=lf1; lf3=lf1; lf4=lf1; lf5=
     slf1; lf6=lf1; lf7=lf1;use_PrSet =.false.;  newin=0; wasfor=0;lf19=.false.;lf20=.false.;lf21=lf20;lf22=lf20;lf23=lf20;lf24=lf20
      lf00=.false.;use_nOder =.true.;use_nIb1  =.true.;new_cvars= .true.;use_Quadro=.true.;iDB=2
#ifdef _VKTEST
      iDB=-1
#endif
      ltiming=.false.;iHNL=0;    pClBFunc=0;  pUserData=pUserData0;iThH = 0;inew_mes=0; istop=99;kstage=7;tQsol=1;accur=0.7;ioutk=0;
       ioutp=0;inpk=0; inpp=0;probnm = 'Current_Problem'; workpath=''//char(0);error_file='ErrorsVK.txt';  log_file='LogfileVK.txt';
       sol_file='SolutionVK.txt';call GetRootPathEx(workpath,pUserData);  workpath=trim(RCStr(workpath,RL));i=len_trim(workpath);if(
     ci>0) then; if(workpath(i:i)=='\') workpath=workpath(:i-1); endif;if(workpath.ne.'') chw='\'//trim(chw);if(workpath.ne.'') then
      workpath=trim(workpath)//'\';endif;log_file_param=-1;  log_pr=-1;key_new_error=log_pr;select case(key_new_error);case(0); open
     x(22,file=trim(workpath)//trim(error_file),err=10,status='REPLACE'); goto 11;case(1); open(22,file=trim(workpath)//trim(error_f
     rile),err=10); goto 11;case default; lf22=.false.; goto 11;end select
10    continue;key_new_error=-1; lf22=.false.;vk_mess='Cannot open output file '//trim(workpath)//trim(error_file);call putmess('W',
     g0,'Files Opening',vk_mess)
11    continue;initpname='XXXXPOIN';i=len_trim(initpname);write(initpname(i+1:),'(2l1)')zast(5)<ins,zast(5)>jns;initpname(i+1:)=char
     v(10);taskfname='XXXXXXXXXX'; if(lf24)then; solfname=trim(sol_file); else; solfname='XXXXXXXXXX'; endif;select case(log_file_pa
     pram);case(0); open(21,file=trim(workpath)//trim(log_file),err=15,status='REPLACE');case(1); open(21,file=trim(workpath)//trim(
     clog_file),err=15);case(-1);case default;vk_mess="Incorrect input parameter Log_File_Param"; call putmess('W',0,'Files Opening'
     c,vk_mess);end select;goto 16
15    log_file_param=-1; lf21=.false.;vk_mess='Cannot open output file '//trim(workpath)//trim(log_file); call putmess('W',0,'Files 
     kOpening',vk_mess)
16    continue;accur=(accur-1.)/11.;CALL CalculateOneFunction(pBuffer,pPointName,     fValue,    int(1,llen),int(1,llen));if(lf19)th
     ven;do i=10,99; CLOSE (UNIT=i, ERR=99);
99    enddo;endif;ioutk=istop+1;if(it_id==1) VK_GetFunctionValue=ioutp;if(it_id==2) VK_GetFunctionGradient=ioutp;if(it_id==3) VK_Get
     vFunctionIncrement=ioutp;end function VK_GetFunctionValue;SUBROUTINE PUTMESS(KODS,iadd,segm_name,mess);use CiFort; use ModCommo
     wns;integer(4), parameter::THRD_VK_CANCEL=WM_USER+10;integer(4), parameter::THRD_VK_ABORT =WM_USER+11;type my_struct; sequence;
      integer(2) iok,iop; character(256) chw;end type;type(my_struct) :: str1;integer(4) kerr,kwar, iadd, i;character(*) segm_name, 
     rmess, kods;character vk_mess*256;logical lf1,lf2,lf3,lf4,lf5,lf6,lf7,          lcancel;common /SetFirst/lf1,lf2,lf3,lf4,lf5,lf
     v6,lf7;logical lfirst; equivalence (lf1,lfirst);save kerr,kwar,lcancel;logical lgap;real(8) dsec,w;if(lfirst)then; call start_s
     ltop_whatch(11); kerr=0; kwar=0;if(newin>1.and.lf22) write(21,'(/////a/)')repeat('=',80);lcancel=.false.;endif;select case(kods
     l);case('S','s','E','e');kerr=kerr+1;write(vk_mess,'(i5.5)')iadd;if(lf22) write(22,'(/a,i5,3(/a))')'Error: ',kerr,
     +'Problem: '//trim(probnm),  'Object: '//trim(segm_name),
     +'Error_description: '//trim(vk_mess)//' '//trim(mess);if(idb==2) call OnErrorEx(iadd,trim(segm_name)//char(0),trim(mess)//char
     n(0),pUserData);inew_mes=1; vk_mess='Error: '//trim(vk_mess)//' '//trim(mess);ioutp=iadd;ioutk=istop;case('W','w');kwar=kwar+1;
      if(lf22) write(22,'(/a,i5,3(/a))')'Warning: ',kwar,
     +'Problem: '//trim(probnm),  'Object: '//trim(segm_name),
     +'Warning_description: '//trim(mess);if(idb==2) call OnWarningEx(kwar,trim(segm_name)//char(0),trim(mess)//char(0),pUserData);c
     wase('T','t');i=min0(len_trim(mess),256-1); str1%chw=mess(:i)//char(0);if(idb==2) call OnMessageEx(0,str1%chw,pUserData);case d
     nefault;if(mess.ne.'')then; inew_mes=1; vk_mess=mess; else; inew_mes=0; endif;end select;if(inew_mes>0.and.InKind/=0)then; i=mi
     ln0(len_trim(vk_mess),256-1);str1%chw=vk_mess(:i)//char(0); str1%iok=int(ioutk,2); str1%iop=int(ioutp,2);if(idb==2)then; lgap=.
     wtrue.;if(iprntout>1)then; w=iprntout; call CheckRestart_stop_whatch(11, w, dsec,lgap);elseif(iprntout==0)then; lgap=.false.; l
     gfirst=.false.;endif;if(lgap.or.lfirst) call OnMessageEx(0,str1%chw,pUserData);endif;inew_mes=0;if(lf00) write(*,'(a)',err=100)
     p trim(vk_mess);goto 110
100   lf00=.false.
110   continue;endif;i=0;if(InKind/=0.and.idb==2.and..not.lcancel) i=int(OnCancelEx( pUserData ));if(idb<0) call set_stop(.true.,1,1
     o,i);if(i>0.and.ioutk<istop) then;lcancel=.true.;if(idb==1) then;else;if(i==1.or.i==3)then; inpk=1; inpp=0; if(newin>=0)inpp=1;
      elseif(i==2.or.i==4)then; inpk=1; inpp=1;endif;endif;if(inpk==1) then;if(.not.(inpp==0.and.ioutk>=3)) then;ioutk=istop; ioutp=
     g2;vk_mess='Solving process is interrupted without saving solution';else;vk_mess='Solving process is interrupted and solution i
     ys saving (if available)';endif;if(lf00) write(*,'(a)') trim(vk_mess);str1%iok=int(ioutk,2);  str1%iop=int(ioutp,2);  str1%chw=
     ttrim(vk_mess)//char(0);if(idb==2) then;  call OnMessageEx(0,str1%chw,pUserData);call OnErrorEx(999,' '//char(0),str1%chw,pUser
     nData);endif;endif;endif;if(ioutk==istop) then;mess='S';else;if(kods=='E'.or.kods=='e')ioutp=0;endif;if(inpk==1.and.inpp==0) me
     mss='T';if(lfirst) lfirst=.false.;return;end subroutine PUTMESS;SUBROUTINE PUTMESS_C_(KODS0,l1,iadd,segm_name0,l2,xiter,obval,i
     gnfval,      mess0,l3);use CiFort;integer(1) kods0,segm_name0,mess0;integer(4) iadd; real(8) xiter,obval,infval;integer(4),valu
     oe:: l1,l2,l3;integer(4) i,j;integer(4),parameter:: lnm=300;character(lnm) kods,segm_name,mess;character wch*256,ch6*6;kods='';
       segm_name=''; mess='';call copybuff(loc(kods0),l1,loc(kods),l1);call copybuff(loc(segm_name0),l2,loc(segm_name),l2);call copy
     xbuff(loc(mess0),l3,loc(mess),l3);wch='';i=len_trim(mess);i=min(256,i);call copybuff(loc(mess),i,loc(wch),i);call copybuff(loc(
     esegm_name),6,loc(ch6),6);i=scan(wch,char(0)); i=257;select case(ch6);case('SIMPL','BARR','MIP');i=int(xiter);write(wch,'(i10.1
     a0)')i; i=verify(wch,'0'); if(i<1.or.i>10)i=10;if(ch6(:3)=='MIP')then;write(wch,'(a,2(a,e18.12))')'MIP_iteration='//wch(i:10),'
     i  Objective=',obval,'  Gap=',obval-infval;else; write(wch,'(a,2(a,e18.12))')'Grb.iteration='//wch(i:10),'  Objective=',obval,'
     y  Residual=',infval;endif;case('Gurobi');wch='';case default;j=len_trim(wch);do i=1,j; j=iachar(wch(i:i));select case(j);case(
     l32:127);case default; wch(i:i)=' ';end select;enddo;end select;call putmess(KODS,iadd,segm_name,wch);if(wch=='S'.or.wch=='T')t
     shen;i=len_trim(mess);call clearbuff(loc(mess0),int(i,llen)); call copybuff(loc(wch),1,loc(mess0),1);endif;end subroutine PUTME
     eSS_C_;character(1000) function RCStr(input_str0,ln);character(*) input_str0; integer(4)  i,ln;character(1001) input_str; point
     ter(pch,input_str);pch=loc(input_str0); i=index(input_str(:min(ln,1001)),char(0))-1;if(i<=0)then; RCStr='';if(ln>1001)then; inp
     uut_str='Inernal error in function RCStr: ln>1001';call putmess('S',1101,'Input Paramerets Analyzing',input_str);endif;else; RC
     tStr=input_str(:i);endif;end function RCStr;integer(4) function CStrLen(input_str0,ln);character(*) input_str0; integer(4)  i,l
     hn;character(1001) input_str; pointer(pch,input_str);pch=loc(input_str0); i=index(input_str(:min(ln,1001)),char(0))-1;if(i<=0)t
     vhen; CStrLen=0;if(ln>1001)then; input_str='Inernal error in function CStrLen: ln>1001';call putmess('S',1102,'Input Paramerets
     e Analyzing',input_str);endif;else; CStrLen=i;endif;
      end function CStrLen
