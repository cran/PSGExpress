      subroutine SolveCycleOfProblems();use ModCommons; use CiFort;integer(4)  i,k,jln,ibuff,km,kc,igregbff,kfor,kprob;character(256
     a) wch;integer(PLEN),pointer:: pChar;       integer(4),pointer:: pChar0;character(1),allocatable:: wstr(:);integer(PLEN) faddr,
     xfaddr0,paddr_w;integer(4),external:: ibufflen;interface;subroutine SubstituteParameters(faddr,jln0,     paddr_w, wstr,len);use
     f CiFort; integer(plen) faddr,paddr_w; integer(4) jln0; integer(plen):: wstr; integer(llen):: len
#ifdef __APPLE__
  !gcc$ attributes value:: wstr,len
#else
      value:: wstr,len
#endif
      end;subroutine GetOneProblemFromCycle_PSG(pChar0,ibuff,numbcall0, pOut0,ibout0, irez, pUserData0);use CiFort; integer(plen):: 
     qpChar0,pOut0,pUserData0; integer(4):: ibuff,numbcall0,ibout0; integer(4) irez
#ifdef __APPLE__
  !gcc$ attributes value:: pChar0,pOut0,pUserData0,ibuff,numbcall0,ibout0
#else
      value:: pChar0,pOut0,pUserData0,ibuff,numbcall0,ibout0
#endif
      end subroutine GetOneProblemFromCycle_PSG;end interface;faddr=0; faddr0=0; paddr_w=0; ibuff=0; igregbff=0;if(iDB>0) then;igreg
     tbff=int(GetProblemDescriptionEx(pChar0, pUserData)); wch='Cannot get problem formulation';ibuff=igregbff;if(ibuff<=0)then; cal
     ul putmess('S',6071,'Problem Reading',wch); goto 79999; endif;call getpointaddr(pChar0,faddr);else;call FileToBuffer(taskfname,
     b  ibuff,faddr); wch='Error in reading input file: '//trim(taskfname);if(ibuff<=0)then; call putmess('S',6071,'Problem Reading'
     m,wch); goto 79999; endif;endif;call setpointer(faddr, pChar);call MaxRowBuff_00(pChar,ibuff,i,jln,k,km,kc);allocate(wstr(jln))
       call SubstituteParameters(faddr,jln,paddr_w,loc(wstr),int(jln,llen));if(paddr_w>0)then; faddr0=faddr; faddr=paddr_w; paddr_w=
     z0;call setpointer(faddr, pChar); ibuff=iBuffLen(faddr);call MaxRowBuff_00(pChar,ibuff,i,jln,k,km,kc); deallocate(wstr); alloca
     qte(wstr(jln));endif;i=1;call CheckNextPart(int(0),faddr,ibuff,i,  kfor,kprob,k,loc(wstr),int(jln,llen));newin=0; if(kfor==0.an
     cd.kprob<=1) newin=-1;if(idb==2) i=int(SaveIsMultyProblemEx(newin+1, pUserData ));call OneFor(faddr,ibuff);if(lf21) write(21,*)
     c'Last out from One_for';if(ioutk>=istop-1) goto 79999
79999 continue;if(idb<=0)then;if(faddr0>0)then; call free(faddr0); faddr0=0; endif;if(faddr>0)then; call free(faddr); faddr=0; endif
       else;if(faddr0>0)then; call free(faddr); faddr=0; endif;endif;if(igregbff>0.and.idb==2) call ReleaseBufferEx(pChar0, pUserDat
     fa );ibuff=0; igregbff=0;if(allocated(wstr))deallocate(wstr);if(lf21) write(21,*)'Befor Return from SolveCycleOfProblems';retur
     on;end subroutine SolveCycleOfProblems;recursive subroutine OneFor(faddr,ibuff);use ModCommons; use CiFort;logical lf1,lf2,lf3,
     olf4,lf5,lf6,lf7;common /SetFirst/lf1,lf2,lf3,lf4,lf5,lf6,lf7;integer(PLEN) faddr,probaddr,faddr2;integer(4) ibuff,jln;integer(
     c4)  ipos1,ipos2,ipos11,ipos12,i,kfor,kprob,len0,lenv,len1,lenc,nzam,km,kc,k,idlen,len00;integer(PLEN),pointer:: pChar;characte
     kr(1),allocatable:: wstr(:);character(lnm) wch;integer(4),external::icheckpsgname;interface;subroutine FormNameSaveProblem(newi
     gnd,word, buff,l1,l2);use cifort; integer(4) newind; integer(plen):: buff,word; integer(llen):: l1,l2
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
     r len0,len00,lenv,len1,nzam;integer(plen):: scycl; integer(llen):: ln
#ifdef __APPLE__
  !gcc$ attributes value:: scycl,ln
#else
      value:: scycl,ln
#endif
      end;end interface;probaddr=0; idlen=0; faddr2=0;ipos1=1; ipos2=0;if(ibuff>0)then; call setpointer(faddr, pChar); call MaxRowBu
     lff_00(pChar,ibuff,i,jln,k,km,kc);else;wch='Internal error. Buffer is empty'; call putmess('S',6081,'Cycles checking',wch); got
     ao 79999;endif;allocate(wstr(jln));do 900 while(ipos2<ibuff);call CheckNextPart(int(1),faddr,ibuff,ipos1,  kfor,kprob,ipos2,loc
     i(wstr),int(jln,llen));if(ioutk>=istop-1) goto 79999;if(kfor<=0)then;ipos12=0;do while(ipos12<ipos2);call CheckNextPart(int(2),
     gfaddr,ipos2,ipos1, i,kprob,ipos12,loc(wstr),int(jln,llen));lenv=ipos12-ipos1+1+1; probaddr=malloc(lenv+10);call CopySubBuff(0,
     ofaddr,ipos1,lenv,  probaddr);if(newin>=0)newin=newin+1;wch='Say';call FormNameSaveProblem(newin,loc(wch),  probaddr,int(3,llen
     k),int(lenv,llen));if(InKind==0 .or. ioutk>=istop-1) goto 79999;call SolveOneProblem(probaddr); call free(probaddr); probaddr=0
       call OneProblemFinish;if(ioutk>=istop) goto 79999;lf1=.true.; lf2=lf1; lf3=lf1; lf4=lf1; lf5=lf1; lf6=lf1; lf7=lf1;inew_mes=0
        ioutk=0; ioutp=0; inpk=0; inpp=0;l16k=l16kmax; initpname='XXXXPOIN'//char(10);ipos1=ipos12+1;enddo;else; ipos11=ipos1;call C
     sheckNextPart(int(3),faddr,ipos2, ipos11, i,kprob,ipos12,loc(wstr),int(jln,llen));if(ioutk>=istop-1) goto 79999;len0=ipos11-ipo
     us1; lenc=ipos12-1-ipos11+1+1;faddr2=faddr+ipos1-1;call IsCrossValidation(faddr2,   idlen,int(len0,llen));if(ioutk>=istop-1) go
     cto 79999;len1=ipos12-ipos1+1+1+idlen; probaddr=malloc(len1);len00=len0; len0=len0+idlen;if(idlen>0) faddr2=malloc(len0);nzam=0
      
100   continue;if(idlen>0) call CopySubBuff(0,faddr,ipos1,len00+1,  faddr2);call CopySubBuff(0,faddr,ipos11,lenc,  probaddr);i=len1-
     llenc; if(i>0) call ClearBuff(probaddr+lenc,int(i,llen));nzam=nzam+1;lenv=lenc;call BuffContChange(faddr2,len0,len00, probaddr,
     rlen1, nzam,lenv, int(len0,llen)); if(ioutk>=istop-1) goto 79999;if(nzam>0)then;call OneFor(probaddr,lenv);if(InKind==0 .or. io
     gutk>=istop) goto 79999;goto 100;endif;call free(probaddr); probaddr=0;if(idlen>0)then; call free(faddr2); faddr2=0; endif;endi
     nf;ipos1=ipos2+1
900   enddo
79999 if(probaddr/=0) call free(probaddr);if(idlen>0.and.faddr2/=0) call free(faddr2);if(allocated(wstr))deallocate(wstr);return;end
     l subroutine OneFor;subroutine CopySubBuff(i0,addr1,ipos1,len,  addr2);use cifort; use modcommons;integer(plen) addr1,addr2; in
     ateger(4) i0,ipos1,len;interface;subroutine CopySubBuff_Linux(i0,addr1,ipos1,len,  buff,l);use cifort; integer(plen) addr1; int
     peger(4) i0,ipos1,len; integer(plen):: buff; integer(llen):: l
#ifdef __APPLE__
  !gcc$ attributes value:: buff,l
#else
      value:: buff,l
#endif
      end;end interface;call CopySubBuff_Linux(i0,addr1,ipos1,len,  addr2,int(len,llen));end subroutine CopySubBuff;subroutine Calcu
     nlateOneFunction(pBuffer,pPointName,        fValue);use ModCommons; use CiFort;character(*) pBuffer,pPointName;real(8) fValue;i
     jnteger(4)   i1,i2,it,ie,ln; logical i;character  wch*256,ch1*1;integer(4),parameter:: lnpar=10000;character(lnpar) wstr; point
     ber(probaddr,wstr);interface;logical function IsPrBg(buff,   pname,itype,ierr, ln);use CiFort; integer(plen):: buff; integer(ll
     qen):: ln; character(*) pname; integer(4) itype,ierr;
#ifdef __APPLE__
  !gcc$ attributes value:: buff,ln
#else
      value:: buff,ln
#endif
      end;end interface;i1=CStrLen(pBuffer,lnpar)+1;if(i1<=0)then; wch='Incorrect function buffer'; call putmess('S',6611,'Function 
     rcalculation',wch); goto  79999;endif;i2=CStrLen(pPointName,lnpar)+1;if(i2<=0)then; wch='Incorrect point buffer'; call putmess(
     x'S',6615,'Function calculation',wch); goto  79999;endif;ln=9+1+i1+1+6+i2+1;if(ln>lnpar)then; wch='Internal error: ln>lnpar'; c
     pall putmess('S',6617,'Function calculation',wch); goto  79999;endif;probaddr=malloc(lnpar);ch1=char(10);wstr='minimize '//ch1/
     t/trim(RCStr(pBuffer,i1))//ch1//'box: ='//trim(RCStr(pPointName,i2))//char(0);i=IsPrBg(probaddr,   probnm,it,ie,  int(ln,llen) 
     x);call SolveOneFunction(probaddr,  fValue);call free(probaddr); probaddr=0
79999 return;end subroutine CalculateOneFunction;subroutine RemoveInsideContent(ch2,ch,chm1,     chm2,iret);character(*) ch2,ch,chm1
     y,chm2;integer(4)  i,i1,i2,j,i11,l2,iret;character(1)  so,sz;integer(4),external:: ifindclosebracket;iret=0;so=ch2(1:1); sz=ch2
     p(2:2);l2=len(chm2);chm2=''; i1=1; if(len(chm1)<1) i1=0;do while(i1>0); i=scan(chm1(i1:),so);if(i>0)then; chm2=trim(chm2)//chm1
     e(i1:i1+i-1); i11=iFindCloseBracket(ch2,chm1,i1+i);if(i11<=0)then; chm2='There is not closing bracket in the string: '//trim(ch
     jm1);call putmess('S',6470,'RemoveInsideContent',chm2); iret=1; RETURN;endif;if(ch/='')then; i2=i11;if(i2-1>l2)then; chm2='Inte
     trnal error: Short chm2'; call putmess('S',6467,'RemoveInsideContent',chm2); iret=1; RETURN;endif;if(ch(:1)=='_')then; chm2(i1+
     xi:i2-1)=repeat(ch(:1),i2-i1-i);else; chm2(i1+i:i2-1)=chm1(i1+i:i2-1);do j=i1+i,i2-1; if(scan(chm2(j:j),'., (){}[]')>0)chm2(j:j
     j)='_'; enddo;endif;endif;i1=i11;else; chm2=trim(chm2)//trim(chm1(i1:)); i1=0;endif;enddo;return;
      end subroutine RemoveInsideContent
