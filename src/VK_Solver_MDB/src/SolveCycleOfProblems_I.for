      subroutine SolveCycleOfProblems();use ModCommons; use CiFort;integer(4)  i,k,jln,ibuff,km,kc,igregbff,kfor,kprob;character(256
     l) wch;integer(PLEN),pointer:: pChar;       integer(4),pointer:: pChar0;character(1),allocatable:: wstr(:);integer(PLEN) faddr,
     lfaddr0,paddr_w;integer(4),external:: ibufflen;interface;subroutine SubstituteParameters(faddr,jln0,     paddr_w, wstr,len);use
     l CiFort; integer(plen) faddr,paddr_w; integer(4) jln0; integer(plen),value:: wstr; integer(llen),value:: len;end;subroutine Ge
     ltOneProblemFromCycle_PSG(pChar0,ibuff,numbcall0, pOut0,ibout0, irez, pUserData0);use CiFort; integer(plen),value:: pChar0,pOut
     l0,pUserData0; integer(4),value:: ibuff,numbcall0,ibout0; integer(4) irez;end subroutine GetOneProblemFromCycle_PSG;end interfa
     lce;faddr=0; faddr0=0; paddr_w=0; ibuff=0; igregbff=0;if(iDB>0) then;igregbff=int(GetProblemDescriptionEx(pChar0, pUserData)); 
      wch='Cannot get problem formulation';ibuff=igregbff;if(ibuff<=0)then; call putmess('S',6071,'Problem Reading',wch); goto 79999
     l; endif;call getpointaddr(pChar0,faddr);else;call FileToBuffer(taskfname,  ibuff,faddr); wch='Error in reading input file: '//
     ltrim(taskfname);if(ibuff<=0)then; call putmess('S',6071,'Problem Reading',wch); goto 79999; endif;endif;call setpointer(faddr,
     l pChar);call MaxRowBuff_00(pChar,ibuff,i,jln,k,km,kc);allocate(wstr(jln));call SubstituteParameters(faddr,jln,paddr_w,loc(wstr
     l),int(jln,llen));if(paddr_w>0)then; faddr0=faddr; faddr=paddr_w; paddr_w=0;call setpointer(faddr, pChar); ibuff=iBuffLen(faddr
     l);call MaxRowBuff_00(pChar,ibuff,i,jln,k,km,kc); deallocate(wstr); allocate(wstr(jln));endif;i=1;call CheckNextPart(int(0),fad
     ldr,ibuff,i,  kfor,kprob,k,loc(wstr),int(jln,llen));newin=0; if(kfor==0.and.kprob<=1) newin=-1;if(idb==2) i=int(SaveIsMultyProb
     llemEx(newin+1, pUserData ));call OneFor(faddr,ibuff);if(lf21) write(21,*)'Last out from One_for';if(ioutk>=istop-1) goto 79999
79999 continue;if(idb<=0)then;if(faddr0>0)then; call free(faddr0); faddr0=0; endif;if(faddr>0)then; call free(faddr); faddr=0; endif
      else;if(faddr0>0)then; call free(faddr); faddr=0; endif;endif;if(igregbff>0.and.idb==2) call ReleaseBufferEx(pChar0, pUserData
     l );ibuff=0; igregbff=0;if(allocated(wstr))deallocate(wstr);if(lf21) write(21,*)'Befor Return from SolveCycleOfProblems';return
      end subroutine SolveCycleOfProblems;recursive subroutine OneFor(faddr,ibuff);use ModCommons; use CiFort;logical lf1,lf2,lf3,lf
     l4,lf5,lf6,lf7;common /SetFirst/lf1,lf2,lf3,lf4,lf5,lf6,lf7;integer(PLEN) faddr,probaddr,faddr2;integer(4) ibuff,jln;integer(4)
     l  ipos1,ipos2,ipos11,ipos12,i,kfor,kprob,len0,lenv,len1,lenc,nzam,km,kc,k,idlen,len00;integer(PLEN),pointer:: pChar;character(
     l1),allocatable:: wstr(:);character(lnm) wch;integer(4),external::icheckpsgname;interface;subroutine FormNameSaveProblem(newind
     l,word, buff,l1,l2);use cifort; integer(4) newind; integer(plen),value:: buff,word; integer(llen),value:: l1,l2;end;subroutine 
     lIsCrossValidation(scycl,idlen,len);use cifort; integer(4) idlen; integer(plen),value:: scycl; integer(llen),value:: len;end;su
     lbroutine BuffContChange(scycl,len0, len00, probaddr,len1,  nzam,lenv, ln);use CiFort; integer(PLEN) probaddr; integer(4) len0,
     llen00,lenv,len1,nzam;integer(plen),value:: scycl; integer(llen),value:: ln;end;end interface;probaddr=0; idlen=0; faddr2=0;ipo
     ls1=1; ipos2=0;if(ibuff>0)then; call setpointer(faddr, pChar); call MaxRowBuff_00(pChar,ibuff,i,jln,k,km,kc);else;wch='Internal
     l error. Buffer is empty'; call putmess('S',6081,'Cycles checking',wch); goto 79999;endif;allocate(wstr(jln));do 900 while(ipos
     l2<ibuff);call CheckNextPart(int(1),faddr,ibuff,ipos1,  kfor,kprob,ipos2,loc(wstr),int(jln,llen));if(ioutk>=istop-1) goto 79999
      if(kfor<=0)then;ipos12=0;do while(ipos12<ipos2);call CheckNextPart(int(2),faddr,ipos2,ipos1, i,kprob,ipos12,loc(wstr),int(jln,
     lllen));lenv=ipos12-ipos1+1+1; probaddr=malloc(lenv+10);call CopySubBuff(0,faddr,ipos1,lenv,  probaddr);if(newin>=0)newin=newin
     l+1;wch='Say';call FormNameSaveProblem(newin,loc(wch),  probaddr,int(3,llen),int(lenv,llen));if(InKind==0 .or. ioutk>=istop-1) 
     lgoto 79999;call SolveOneProblem(probaddr); call free(probaddr); probaddr=0;call OneProblemFinish;if(ioutk>=istop) goto 79999;l
     lf1=.true.; lf2=lf1; lf3=lf1; lf4=lf1; lf5=lf1; lf6=lf1; lf7=lf1;inew_mes=0; ioutk=0; ioutp=0; inpk=0; inpp=0;l16k=l16kmax; ini
     ltpname='XXXXPOIN'//char(10);ipos1=ipos12+1;enddo;else; ipos11=ipos1;call CheckNextPart(int(3),faddr,ipos2, ipos11, i,kprob,ipo
     ls12,loc(wstr),int(jln,llen));if(ioutk>=istop-1) goto 79999;len0=ipos11-ipos1; lenc=ipos12-1-ipos11+1+1;faddr2=faddr+ipos1-1;ca
     lll IsCrossValidation(faddr2,   idlen,int(len0,llen));if(ioutk>=istop-1) goto 79999;len1=ipos12-ipos1+1+1+idlen; probaddr=mallo
     lc(len1);len00=len0; len0=len0+idlen;if(idlen>0) faddr2=malloc(len0);nzam=0
100   continue;if(idlen>0) call CopySubBuff(0,faddr,ipos1,len00+1,  faddr2);call CopySubBuff(0,faddr,ipos11,lenc,  probaddr);i=len1-
     llenc; if(i>0) call ClearBuff(probaddr+lenc,int(i,llen));nzam=nzam+1;lenv=lenc;call BuffContChange(faddr2,len0,len00, probaddr,
     llen1, nzam,lenv, int(len0,llen)); if(ioutk>=istop-1) goto 79999;if(nzam>0)then;call OneFor(probaddr,lenv);if(InKind==0 .or. io
     lutk>=istop) goto 79999;goto 100;endif;call free(probaddr); probaddr=0;if(idlen>0)then; call free(faddr2); faddr2=0; endif;endi
     lf;ipos1=ipos2+1
900   enddo
79999 if(probaddr/=0) call free(probaddr);if(idlen>0.and.faddr2/=0) call free(faddr2);if(allocated(wstr))deallocate(wstr);return;end
     l subroutine OneFor;subroutine CopySubBuff(i0,addr1,ipos1,len,  addr2);use cifort; use modcommons;integer(plen) addr1,addr2; in
     lteger(4) i0,ipos1,len;interface;subroutine CopySubBuff_Linux(i0,addr1,ipos1,len,  buff,l);use cifort; integer(plen) addr1; int
     leger(4) i0,ipos1,len; integer(plen),value:: buff; integer(llen),value:: l;end;end interface;call CopySubBuff_Linux(i0,addr1,ip
     los1,len,  addr2,int(len,llen));end subroutine CopySubBuff;subroutine CalculateOneFunction(pBuffer,pPointName,        fValue);u
     lse ModCommons; use CiFort;character(*) pBuffer,pPointName;real(8) fValue;integer(4)   i1,i2,it,ie,ln; logical i;character  wch
     l*256,ch1*1;integer(4),parameter:: lnpar=10000;character(lnpar) wstr; pointer(probaddr,wstr);interface;logical function IsPrBg(
     lbuff,   pname,itype,ierr, ln);use CiFort; integer(plen),value:: buff; integer(llen),value:: ln; character(*) pname; integer(4)
     l itype,ierr;end;end interface;i1=CStrLen(pBuffer,lnpar)+1;if(i1<=0)then; wch='Incorrect function buffer'; call putmess('S',661
     l1,'Function calculation',wch); goto  79999;endif;i2=CStrLen(pPointName,lnpar)+1;if(i2<=0)then; wch='Incorrect point buffer'; c
     lall putmess('S',6615,'Function calculation',wch); goto  79999;endif;ln=9+1+i1+1+6+i2+1;if(ln>lnpar)then; wch='Internal error: 
     lln>lnpar'; call putmess('S',6617,'Function calculation',wch); goto  79999;endif;probaddr=malloc(lnpar);ch1=char(10);wstr='mini
     lmize '//ch1//trim(RCStr(pBuffer,i1))//ch1//'box: ='//trim(RCStr(pPointName,i2))//char(0);i=IsPrBg(probaddr,   probnm,it,ie,  i
     lnt(ln,llen) );call SolveOneFunction(probaddr,  fValue);call free(probaddr); probaddr=0
79999 return;end subroutine CalculateOneFunction;subroutine RemoveInsideContent(ch2,ch,chm1,     chm2,iret);character(*) ch2,ch,chm1
     l,chm2;integer(4)  i,i1,i2,j,i11,l2,iret;character(1)  so,sz;integer(4),external:: ifindclosebracket;iret=0;so=ch2(1:1); sz=ch2
     l(2:2);l2=len(chm2);chm2=''; i1=1; if(len(chm1)<1) i1=0;do while(i1>0); i=scan(chm1(i1:),so);if(i>0)then; chm2=trim(chm2)//chm1
     l(i1:i1+i-1); i11=iFindCloseBracket(ch2,chm1,i1+i);if(i11<=0)then; chm2='There is not closing bracket in the string: '//trim(ch
     lm1);call putmess('S',6470,'RemoveInsideContent',chm2); iret=1; RETURN;endif;if(ch/='')then; i2=i11;if(i2-1>l2)then; chm2='Inte
     lrnal error: Short chm2'; call putmess('S',6467,'RemoveInsideContent',chm2); iret=1; RETURN;endif;if(ch(:1)=='_')then; chm2(i1+
     li:i2-1)=repeat(ch(:1),i2-i1-i);else; chm2(i1+i:i2-1)=chm1(i1+i:i2-1);do j=i1+i,i2-1; if(scan(chm2(j:j),'., (){}[]')>0)chm2(j:j
     l)='_'; enddo;endif;endif;i1=i11;else; chm2=trim(chm2)//trim(chm1(i1:)); i1=0;endif;enddo;return;
      end subroutine RemoveInsideContent
