      subroutine GetOneProblemFromCycle_PSG(pChar0,ibuff0,numbcall0, pOut0,ibout0, irez, pUserData0);use ModCommons; use CiFort;inte
     jger(PLEN):: pChar0,pOut0
#ifdef __APPLE__
  !gcc$ attributes value:: pChar0,pOut0
#else
      value:: pChar0,pOut0
#endif
      integer(4):: ibuff0,numbcall0,ibout0; integer(4) irez
#ifdef __APPLE__
  !gcc$ attributes value:: ibuff0,numbcall0,ibout0
#else
      value:: ibuff0,numbcall0,ibout0
#endif
      integer(PLEN):: pUserData0
#ifdef __APPLE__
  !gcc$ attributes value:: pUserData0
#else
      value:: pUserData0
#endif
      integer(4)  ibout,i,k,km,kc,jln;integer(PLEN)  faddr,iaddrCh1,fadout,faddr0,paddr_w;integer(4)  numbcall,iout,ibuff,nin,nad,ki
     qn,kad,ibl,idd,ln,iw,i1,i2,j1,j2;character,pointer :: pOut,pChar;integer(PLEN),pointer :: pi4;character(l16kmax) wstr;integer(4
     e),parameter:: maxBuffLen=1000000;character(maxBuffLen),pointer :: pCh1;character  wch*256,sset*14,ch1;pointer(piSet,iNset); ch
     aaracter(lnm+11) iNset(-3:*);pointer(paSet,aDset); character(lnm+11) aDset(-3:*);pointer(pWset,Wset); character(lnm+11) Wset(-3
     n:*);integer(4),external:: iBuffLen,iCheckPSGName;interface;subroutine GetOneProblemFromCycle_VK(pChar0,ibuff0,numbcall0, pOut0
     b,ibout0, irez, pUserData0);use IntelInterf; character,target:: pChar0,pOut0; integer(4) ibuff0,numbcall0,ibout0,irez; integer(
     bPLEN) pUserData0;end;subroutine SubstituteParameters(faddr,jln0,     paddr_w, wstr,len);use IntTypes; integer(plen) faddr,padd
     ar_w; integer(4) jln0; integer(plen):: wstr; integer(llen):: len
#ifdef __APPLE__
  !gcc$ attributes value:: wstr,len
#else
      value:: wstr,len
#endif
      end
#ifdef __APPLE__
      character(16512) function move_space(str0,len)
#else
      character(l16kmax) function move_space(str0,len)
#endif
      use ModCommons; use IntTypes; integer(plen):: str0; integer(llen):: len
#ifdef __APPLE__
  !gcc$ attributes value:: str0,len
#else
      value:: str0,len
#endif
      end;end interface;sset='()*=,:-+<> '//char(10)//char(13)//char(09);faddr0=0; iaddrCh1=0; piSet=0; paSet=0;pWset=0; iout=0;    
         idd=0;call InitDataInCommons(pUserData0);if(numbcall0>1) goto 1000;if(numbcall0<1)then; wch='Internal error: the number of 
     hrequared problem <=0';call putmess('S',6593,'Getting objects',wch); goto 79999;endif;ibuff=ibuff0; faddr=0; fadout=0; paddr_w=
     j0;call setCharPointAddr(pChar0,pChar); call setCharPointAddr(pOut0,pOut);call getCharpointaddr(pChar,faddr);if(ibuff<=0)then; 
      wch='Internal error: the lenght of input buffer <=0';call putmess('S',6573,'Getting objects',wch); goto  79999;endif;if(faddr<
     o=0)then; wch='Internal error: the address of input buffer <=0';call putmess('S',6576,'Getting objects',wch); goto  79999;endif
       call getCharpointaddr(pOut,fadout);if(ibout0<=0)then; wch='Internal error: the lenght of output buffer <=0';call putmess('S',
     o6553,'Getting objects',wch); goto  79999;endif;if(fadout<=0)then; wch='Internal error: the address of output buffer <=0';call 
     rputmess('S',6556,'Getting objects',wch); goto  79999;endif;call setpointer(faddr, pi4);call MaxRowBuff_00(pi4,ibuff,i,jln,k,km
     b,kc);call SubstituteParameters(faddr,jln,paddr_w,loc(wstr),  int(jln,llen) );if(ioutk>=istop-1) goto 79999;if(paddr_w>0)then; 
      faddr0=faddr; faddr=paddr_w; paddr_w=0;call setCharpointaddr(faddr, pChar); ibuff=iBuffLen(faddr);endif;iaddrCh1=malloc(ibout0
     i);call setCharPointAddr(iaddrCh1,pCh1);ibout=ibout0;nin=100; nad=100;piSet=malloc((nin+4)*(lnm+11)); iNset(-3:0)='';paSet=mall
     roc((nad+4)*(lnm+11)); aDset(-3:0)='';kin=0; kad=0;numbcall=0; iout=1;do while(.true.); if(iout==1) numbcall=numbcall+1;call Ge
     etOneProblemFromCycle_VK(pChar,ibuff,numbcall, pCh1,ibout, iout, pUserData0);if(iout==0) Exit;if(iout>1) Exit;if(iout==-1) goto
     y 1000;if(iout<-1)then;call free(iaddrCh1); ibout=-iout;iaddrCh1=malloc(ibout); call setCharPointAddr(iaddrCh1,pCh1);endif;if(i
     fout==1)then;ibl=iBuffLen(iaddrCh1)-1;if(ibl<=0.or.ibl>maxBuffLen)then; wch="Internal error: internal problem's buffer";call pu
     ftmess('S',6517,'Getting objects',wch); goto  79999;endif;if(ibl>0)then; idd=index(pCh1(:ibl),'AddedObjects:'); endif;iw=ibl; i
     mf(idd>0)then; iw=idd-1; else; idd=-13; endif;i=1;do while(i>0); call NextWord(pCh1(:iw),sset, i, wch,ch1); ln=len_trim(wch);if
     d(i>=0.and.ln>=6)then; ln=min(ln,8);i1=index(wch(:ln),'matrix_');  if(i1>0) goto 100;i1=index(wch(:ln),'vector_');  if(i1>0) go
     kto 100;i1=index(wch(:ln),'point_');   if(i1>0) goto 100;i1=index(wch(:ln),'pmatrix_')
100   if(i1>0)then; if(iCheckPSGName(trim(wch),'Say')<0) goto 79999;j1=kad+1; call checkXnames(trim(wch),kad,aDset,j1,j2);if(j2>kad)
     ithen; j1=kin+1; call checkXnames(trim(wch),kin,iNset,j1,j2);if(j2>kin)then;if(kin==nin)then;nin=nin*2; pWset=malloc((nin+4)*(l
     onm+11)); wSet(:kin)=iNset(:kin); call free(piSet); piSet=pWset;endif;call insertXname(trim(wch),j1,2,kin,iNset,    i1,i2,i,j2)
       endif;endif;endif;endif;enddo;i=idd+13;do while(i>0); call NextWord(pCh1(:ibl),sset, i, wch,ch1); ln=len_trim(wch);if(i>=0.an
     id.ln>=6)then; ln=min(ln,8);i1=index(wch(:ln),'matrix_');  if(i1>0) goto 110;i1=index(wch(:ln),'vector_');  if(i1>0) goto 110;i
     i1=index(wch(:ln),'point_');   if(i1>0) goto 110;i1=index(wch(:ln),'pmatrix_')
110   if(i1>0)then;if(iCheckPSGName(trim(wch),'NoSay')<0)then; wch="PSG generates too long name of output object: "//trim(wch);call 
     zputmess('S',6551,'Getting objects',wch); goto  79999;endif;j1=kad+1; call checkXnames(trim(wch),kad,aDset,j1,j2);if(j2>kad)the
     gn;if(kad==nad)then; nad=nad*2; pWset=malloc((nad+4)*(lnm+11)); wSet(:kad)=aDset(:kad); call free(paSet); paSet=pWset;endif;cal
     yl insertXname(trim(wch),j1,2,kad,aDset,    i1,i2,i,j2);endif;endif;endif;enddo;endif;enddo;if(numbcall==1.or.iout>1)then; wch=
     g'Internal error: can not get next problem';call putmess('S',6513,'Getting objects',wch); goto  79999;endif;ln=len(iNset(1));do
     w i=1,kin-1; iNset(i)(ln:ln)=char(10); enddo;if(kin>0)then; iNset(i)(ln:ln)=char(0); else; iNset(i)(1:1)=char(0); endif;ln=kin*
     pln;wstr=move_space(loc(iNset(1)),int(ln,llen)); ibl=iBuffLen(loc(wstr));if(ibl<=ibout0)then; iout=1; call copybuff(loc(wstr),i
     kbl,fadout,ibl);else; iout=-ibl;endif;goto 1000
79999 iout=-1
1000  continue;if(faddr0>0)then; call free(faddr); faddr0=0; endif;if(iaddrCh1>0)then; call free(iaddrCh1); iaddrCh1=0; endif;if(piS
     met>0)then; call free(piSet); piSet=0; endif;if(paSet>0)then; call free(paSet); paSet=0; endif;ibuff=0;irez=iout;return;end sub
     hroutine GetOneProblemFromCycle_PSG;subroutine GetOneProblemFromCycle_VK(pChar0,ibuff0,numbcall0, pOut0,ibout0, irez, pUserData
     b0);use ModCommons; use CiFort;character,target:: pChar0,pOut0;integer(4)  ibuff0,numbcall0,ibout0,irez;integer(PLEN) pUserData
     h0;integer(4)  ibout,i,j,k,jln,l16p,km,kc,kfor,kprob,iVk_solver,ibuff;integer(PLEN),pointer :: pChar;character(1),allocatable::
     h wstr(:);integer(PLEN) faddr,probaddr,probout,probaddr_w,inaddr;integer(4) numbcall,iout;character,pointer:: pOut,pChw;common/
     ponepr/pOut,probout,iout,numbcall;character(256) wch;character(1),allocatable:: str1(:), str2(:), str3(:);integer(4),external::
     h iBuffLen;interface;subroutine ListOfOutputObjects(probaddr,probaddr_w, wstr,pwstr,wch, l1,l2,l3);use CiFort; integer(PLEN) pr
     robaddr,probaddr_w; integer(plen):: wstr,pwstr,wch; integer(llen):: l1,l2,l3
#ifdef __APPLE__
  !gcc$ attributes value:: wstr,pwstr,wch,l1,l2,l3
#else
      value:: wstr,pwstr,wch,l1,l2,l3
#endif
      end;end interface;iout=0;call InitDataInCommons(pUserData0);probout=0; probaddr_w=0;numbcall=numbcall0;ibout=ibout0; pOut=>pOu
     rt0;ibuff=ibuff0; pChw=>pChar0;faddr=0; probaddr=0;call getCharPointAddr(pChw,faddr);inaddr=malloc(max(ibuff,1)); call setpoint
     ker(inaddr, pChar);if(ibuff>0) call copybuff(faddr,ibuff,inaddr,ibuff);idb=2;if(ibuff>0)then; call MaxRowBuff_00(pChar,ibuff,i,
     kjln,k,km,kc); goto 11; endif;wch='Internal error: input buffer has zero lenght'; call putmess('S',6773,'Problem Extracting',wc
     wh); goto  79999
11    continue;allocate(wstr(jln));i=1;call CheckNextPart(int(0),inaddr,ibuff,i,  kfor,kprob,k, loc(wstr),int(jln,llen));newin=0; if
     z(kfor==0.and.kprob==1) newin=-1;probout=0;call OneFor_NoRun(inaddr,ibuff);if(ioutk>=istop-1) goto 79999;if(iout==1)then;iVk_so
     ylver=2;probaddr=probout;call ExtractProblemStatementToFull(iVk_solver,probaddr,l16p);if(ioutk>=istop-1) goto 79999;allocate(st
     xr1(l16p),str2(l16p),str3(l16p));call ListOfOutputObjects(probout,probaddr_w,loc(str1),loc(str2),loc(str3),int(l16p,llen),int(l
     b16p,llen),int(l16p,llen));deallocate(str1,str2,str3);if(ioutk>=istop-1) goto 79999;i=iBuffLen(probaddr); k=0; j=0; if(probaddr
     q_w>0)then; k=iBuffLen(probaddr_w); j=len('AddedObjects:'//char(10)); endif;ibuff=i+j+k;if(ibuff>ibout)then; iout=-ibuff;else; 
      call getCharpointaddr(pOut,faddr);call copybuff(probaddr,i,faddr,i);if(k>0)then; wch=char(10)//'AddedObjects:'//char(10);call 
     pcopybuff(loc(wch),j+1,faddr+i-1,j+1); call copybuff(probaddr_w,k,faddr+i+j,k);endif;endif;idb=2;endif;goto 100
79999 iout=-1
100   if(probaddr/=probout.and.probaddr/=0)then; call free(probaddr); probaddr=0; endif;if(probout>0)then; call free(probout); probo
     mut=0; endif;if(probaddr_w>0)then; call free(probaddr_w); probaddr_w=0; endif;if(inaddr>0)then; call free(inaddr); inaddr=0; en
     xdif;ibuff=0;irez=iout;if(allocated(wstr))deallocate(wstr);return;end subroutine GetOneProblemFromCycle_VK;recursive subroutine
     m OneFor_NoRun(faddr,ibuff);use ModCommons; use CiFort;character,pointer:: pOut; integer(4)  numbcall,iout; integer(PLEN) probo
     dut;common/onepr/pOut,probout,iout,numbcall;logical lf1,lf2,lf3,lf4,lf5,lf6,lf7;common /SetFirst/lf1,lf2,lf3,lf4,lf5,lf6,lf7;in
     ateger(PLEN) faddr,probaddr,faddr2;integer(4) ibuff,jln;integer(4)  ipos1,ipos2,ipos11,ipos12,i,kfor,kprob,len0,lenv,len1,lenc,
     znzam,km,kc,k,idlen,len00;integer(PLEN),pointer:: pChar;character(1),allocatable:: wstr(:);character(lnm) wch;interface;subrout
     wine FormNameSaveProblem(newind,word, buff,l1,l2);use cifort; integer(4) newind; integer(plen):: buff,word; integer(llen):: l1,
     gl2
#ifdef __APPLE__
  !gcc$ attributes value:: buff,word,l1,l2
#else
      value:: buff,word,l1,l2
#endif
      end;subroutine IsCrossValidation(scycl,idlen,len);use cifort; integer(4) idlen; integer(plen):: scycl; integer(llen):: len
#ifdef __APPLE__
  !gcc$ attributes value:: scycl,len
#else
      value:: scycl,len
#endif
      end;subroutine BuffContChange(scycl,len0, len00, probaddr,len1,  nzam,lenv, ln);use CiFort; integer(PLEN) probaddr; integer(4)
     n len0,len00,lenv,len1,nzam;integer(plen):: scycl; integer(llen):: ln
#ifdef __APPLE__
  !gcc$ attributes value:: scycl,ln
#else
      value:: scycl,ln
#endif
      end;end interface;if(ibuff>0)then; call setpointer(faddr, pChar); call MaxRowBuff_00(pChar,ibuff,i,jln,k,km,kc);else;wch='Inte
     irnal error. Buffer is empty'; call putmess('S',6781,'Cycles checking',wch); goto 79999;endif;allocate(wstr(jln));probaddr=0; i
     ydlen=0; faddr2=0;ipos1=1; ipos2=0;do 900 while(ipos2<ibuff);call CheckNextPart(int(1),faddr,ibuff,ipos1,  kfor,kprob,ipos2,loc
     i(wstr),int(jln,llen));if(ioutk>=istop-1) goto 79999;if(kfor<=0)then;ipos12=0;do while(ipos12<ipos2);call CheckNextPart(int(2),
     yfaddr,ipos2,ipos1,  i,kprob,ipos12, loc(wstr),int(jln,llen));lenv=ipos12-ipos1+1+1; probaddr=malloc(lenv+10);call CopySubBuff(
     m0,faddr,ipos1,lenv,  probaddr);if(newin>=0)newin=newin+1;wch='NoSay';call FormNameSaveProblem(newin,loc(wch), probaddr,int(5,l
     zlen),int(lenv,llen));if(numbcall==1)then; if(newin<=1) iout=1;else;if(numbcall==newin)then; iout=1;elseif(numbcall<newin)then;
       iout=2;endif;endif;if(iout==1)then;probout=probaddr; probaddr=0;endif;if(ioutk>=istop) goto 79999;lf1=.true.; lf2=lf1; lf3=lf
     h1; lf4=lf1; lf5=lf1; lf6=lf1; lf7=lf1;inew_mes=0; ioutk=0; ioutp=0; inpk=0; inpp=0;l16k=l16kmax; initpname='XXXXPOIN'//char(10
     d);ipos1=ipos12+1;if(iout/=0)Exit;enddo;else; ipos11=ipos1;call CheckNextPart(int(3),faddr,ipos2,  ipos11,  i,kprob,ipos12,loc(
     dwstr),int(jln,llen));if(ioutk>=istop-1) goto 79999;len0=ipos11-ipos1; lenc=ipos12-1-ipos11+1+1;faddr2=faddr+ipos1-1;call IsCro
     dssValidation(faddr2,idlen,int(len0,llen));if(ioutk>=istop-1) goto 79999;len1=ipos12-ipos1+1+1+idlen; probaddr=malloc(len1);len
     h00=len0; len0=len0+idlen;if(idlen>0) faddr2=malloc(len0);nzam=0
100   continue;if(idlen>0) call CopySubBuff(0,faddr,ipos1,len00+1,  faddr2);call CopySubBuff(0,faddr,ipos11,lenc,  probaddr);i=len1-
     ulenc;if(i>0) call ClearBuff(probaddr+lenc,int(i,llen));nzam=nzam+1;lenv=lenc;call BuffContChange(faddr2,len0,len00, probaddr,l
     cen1, nzam,lenv, int(len0,llen)); if(ioutk>=istop-1) goto 79999;if(nzam>0)then;call OneFor_NoRun(probaddr,lenv);if(ioutk>=istop
     v) goto 79999;if(iout==0) goto 100;endif;call free(probaddr); probaddr=0;if(idlen>0)then; call free(faddr2); faddr2=0; endif;en
     cdif;ipos1=ipos2+1;if(iout/=0)Exit
900   enddo
79999 if(probaddr/=0) call free(probaddr);if(idlen>0.and.faddr2/=0) call free(faddr2);if(allocated(wstr))deallocate(wstr);return;end
     v subroutine OneFor_NoRun;subroutine InitDataInCommons(pUserData0);use CiFort;integer(HANDLE) iHNL,pClBFunc,pUserData;common/iH
     jNdL/iHNL,pClBFunc,pUserData;integer(4) inte1,i; pointer (pUserData0,inte1);character workpath*256;integer(HANDLE) iThH;integer
     t(4) log_pr,istop,iDB,   it_id,inew_mes,InKind,iprntout;integer(4) inpk,inpp, ioutk, ioutp;common /control/ inpk,inpp,log_pr,is
     utop,iDB, workpath;common /state/ iThH,ioutk,ioutp,it_id,inew_mes,InKind,iprntout;character(138) probnm, initpname,taskfname,so
     xlfname; integer(4) nnew,newin,wasfor;common /npar_prob/probnm,initpname,taskfname,solfname,nnew,newin,wasfor;integer(4), param
     veter :: RL=300;logical lf00, lf19, lf20, lf21, lf22, lf23, lf24;common/funits/ lf00,lf19,lf20,lf21,lf22,lf23,lf24;logical use_
     inOder, use_nIb1, use_PrSet, new_cvars,use_Quadro;common/use_control/ use_nOder, use_nIb1, use_PrSet, new_cvars,use_Quadro;real
     a(8) accur; integer(2) kstage, tQsol;common /param1/ accur,kstage,tQsol;logical lf1,lf2,lf3,lf4,lf5,lf6,lf7;common /SetFirst/lf
     y1,lf2,lf3,lf4,lf5,lf6,lf7;i=inte1;it_id=0; InKind=1;iprntout=1;lf1=.true.; lf2=lf1; lf3=lf1; lf4=lf1; lf5=lf1; lf6=lf1; lf7=lf
     f1;use_PrSet =.false.;  newin=0; wasfor=0;lf19=.false.;lf20=.false.;lf21=lf20;lf22=lf20;lf23=lf20;lf24=lf20;lf00=.false.;use_nO
     bder =.true.;use_nIb1  =.true.;new_cvars= .true.;use_Quadro=.true.;iDB=2;pClBFunc=0; pUserData=pUserData0;inew_mes=0; istop=99;
      ioutk=0; ioutp=0;inpk=0; inpp=0;call GetRootPathEx(workpath,pUserData);  workpath=trim(RCStr(workpath,RL));probnm = 'Current_P
     croblem';
      end subroutine InitDataInCommons
