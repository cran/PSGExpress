      subroutine SubstituteParameters(faddr,jln0,     paddr_w, wstr);use modcommons; use CiFort;integer(plen) faddr, paddr_w;integer
     a(4) jln0;character(*) wstr;integer(PLEN),pointer:: pChar;integer(4)  ibuff,kp,np,lrest,lrst0,i,j,i1,ln,lused,jln,iret;pointer(
     npPrm,parms); character(lnm+10) parms(*);pointer(piwrk,iwrk); integer(4) iwrk(2,*);character  sset*15, ch1;integer(4),parameter
     f:: lfixed=10000;pointer(pzPr,zparm); character(lfixed) zparm(*);pointer(pWset,Wset); character(lfixed) Wset(*);character(lfixe
     wd) wch, chw;integer(4),external:: ibufflen,icheckpsgname;interface;subroutine NextWord(wstr,delim,ip,word,ch1, ln);use cifort;
       integer(plen):: wstr; character(*) delim,word,ch1; integer(4) ip; integer(llen):: ln
#ifdef __APPLE__
  !gcc$ attributes value:: wstr,ln
#else
      value:: wstr,ln
#endif
      end;integer(4) function iFindInBuffer(buff,str1,log, ln);use cifort; integer(plen):: buff; character(*)str1; logical log; inte
     ager(llen):: ln
#ifdef __APPLE__
  !gcc$ attributes value:: buff,ln
#else
      value:: buff,ln
#endif
      end;end interface;pPrm=0; pzPr=0; lrst0=0;if(jln0>lfixed)then; wch='Internal error: jln>lfixed=1000 in SubstituteParameters';c
     uall putmess('S',8023,'Parameters checking',wch); goto 79999;endif;jln=lfixed;sset='()*=,:-+<> '//char(10)//char(13)//char( 9)/
     m/char(0);call setpointer(faddr, pChar); ibuff=iBuffLen(faddr);pWset=0; paddr_w=0;kp=10; np=0;pPrm=malloc(kp*(lnm+10));pzPr=mal
     iloc(kp*jln);wstr=''; lrest=1
100   continue;do while(wstr==''.or.wstr(:1)=='%'); lrst0=lrest; call read_wstr(pChar,ibuff,wstr,lrest,.true.,iret);if(iret==2)then;
       goto 1001; elseif(iret==1)then; goto 79999; endif;enddo;if(index(wstr,'parameter_')/=1) goto 1001;i=1;do while(i>0);if(wstr(i
     k:)=='')then; i=0; Cycle; endif;j=scan(trim(wstr(i:)),'=');if(j>0)then; j=i-1+j-1; wch=adjustl(wstr(i:j));if(index(wch,'paramet
     jer_')==1)then; if(iCheckPSGName(trim(wch),'Say')<0) goto 79999;do i1=1,np; if(wch==parms(i1)) Exit; enddo;if(i1<=np)then; wch=
     i'Problem statement: '//trim(wch)//' not unique';call putmess('S',8010,'Parameters checking',wch); goto 79999;endif;if(np==kp)t
     khen; kp=kp*2; i1=np*(lnm+10);pWset=malloc(max(2*i1,jln)) ; call copybuff(pPrm,i1,pWset,i1); call free(pPrm); pPrm=pWset;pWset=
     omalloc(2*np*jln); wSet(:np)=zparm(:np); call free(pzPr); pzPr=pWset;endif;np=np+1;parms(np)=trim(wch);i=j+2; j=scan(trim(wstr(
     oi:)),';');if(j<=0)then; zparm(np)=adjustl(trim(wstr(i:))); i=0;else; j=i-1+j-1; zparm(np)=adjustl(trim(wstr(i:j))); i=j+2;endi
     zf;if(len_trim(zparm(np))<=0)then; wch='Problem statement: '//trim(wch)//' is empty';call putmess('W',0,'Parameters checking',w
     gch);endif;else; wch='Problem statement: incorrect string: '//trim(wstr);call putmess('S',8030,'Parameters checking',wch); goto
     p 79999;endif;endif;enddo;wstr='';goto 100
1001  continue;if(np>0)then;piwrk=malloc(2*np*4);do i1=1,np; iwrk(1,i1)=len_trim(zparm(i1))-len_trim(parms(i1)); iwrk(2,i1)=0; enddo
       i=lrst0;do while(i>0); call NextWord(faddr,sset, i, wch,ch1,  int(ibuff,llen) ); ln=len_trim(wch);call stringtosmall(wch(:ln)
     c);if(ln>10)then; do i1=1,np; if(wch(:ln)==parms(i1)) iwrk(2,i1)=iwrk(2,i1)+1; enddo;endif;enddo;i1=dot_product(iwrk(1,:np),iwr
     jk(2,:np)); call free(piwrk);paddr_w=malloc(ibuff+i1-(lrst0-1));i=lrst0; lused=0;do while(i>0); call NextWord(faddr,sset, i, wc
     wh,ch1,  int(ibuff,llen) ); ln=len_trim(wch);chw=wch(:ln); call stringtosmall(wch(:ln));if(ln>10)then; do i1=1,np; if(trim(wch)
     p==parms(i1)) Exit; enddo;if(i1<=np)then; lrest=iFindInBuffer(faddr,trim(chw),.true.,  int(i,llen) );j=lrest-lrst0;call copybuf
     lf(faddr+lrst0-1,j,paddr_w+lused,j); lrst0=i-1; lused=lused+j;j=len_trim(zparm(i1));if(j>0)then; call copybuff(loc(zparm(i1)),j
     a,paddr_w+lused,j); lused=lused+j; endif;endif;endif;enddo;j=ibuff+1-lrst0;call copybuff(faddr+lrst0-1,j,paddr_w+lused,j);endif
      
79999 continue;if(pPrm>0) call free(pPrm);if(pzPr>0) call free(pzPr);return;end subroutine SubstituteParameters;subroutine CheckNext
     mPart(icd,pChar,ibuff,  ipos1,  kfor,kprob,ipos2, wstr);use ModCommons;integer(4) icd,ibuff,ipos1,kfor,kprob,ipos2; integer(ple
     kn) pChar; character(*)wstr;logical  ldb,IsPrBg;integer(4),parameter:: lenw=128;character(lenw) wch;integer(4)  jlen,ifor,kcyc,
     ciw,i,ips1,i1,i2,iret;character(l16kmax),external:: move_space;jlen=len(wstr);ldb=.true.;ipos2=ipos1;ifor=0; kprob=0; kfor=0; k
     mcyc=0;wstr='';do while(wstr(:1)=='%'.or.wstr==''); ipos1=ipos2;call read_wstr(pChar,ibuff,  wstr,  ipos2, ldb,iret);if(iret==2
     u)then; goto 20; elseif(iret==1)then; goto 79999; endif;do while(.true.); i=verify(wstr,' ',.true.); if(i<=0) Exit;if(wstr(i:i)
     q==char(10).or.wstr(i:i)==char(13).or.wstr(i:i)==char(9))then; wstr(i:i)=' ';else; EXIT;endif;enddo;enddo;call RemoveInsideCont
     xent('{}','',wstr,wch,iret); wch=trim(move_space(wch));if(iret==1) goto 79999;if(wstr(:4)/='for{'.and.wstr(:4)/='for '.and.wch(
     s:3)/='{}=')then;if(.not.IsPrBg(wstr,wch,i1,i2))then; i=min(len_trim(wstr),50);if(index(wch,'parameter_')==1.and.(wasfor>0.or.n
     lewin>0))then;wstr='Problem Statement: parameters should be written befor Cycle or Problem: '//wstr(:i);else; wstr='Problem Sta
     atement: incorrect beginning of Problem: '//wstr(:i)//'...';endif;call putmess('S',6085,'Problems Reading',wstr); goto 79999;en
     kdif;endif;ipos2=ipos1;if(icd==0)then;do while(kfor==0.and.kprob<2);call read_wstr(pChar,ibuff,  wstr,  ipos2, ldb,iret);if(ire
     nt==2)then; goto 20; elseif(iret==1)then; goto 79999; endif;call RemoveInsideContent('{}','',wstr,wch,iret); wch=trim(move_spac
     ue(wch));if(iret==1) goto 79999;if(wstr(:4)=='for{'.or.wstr(:4)=='for '.or.wch(:3)=='{}=') kfor=1;if(IsPrBg(wstr,wch,i1,i2)) kp
     orob=kprob+1;enddo;elseif(icd==1)then;do while((kcyc>0.or.ifor==0).and.ipos2<ibuff.and.kprob==0); iw=ipos2;call read_wstr(pChar
     l,ibuff,  wstr,  ipos2, ldb,iret);if(iret==2)then; goto 20; elseif(iret==1)then; goto 79999; endif;call RemoveInsideContent('{}
     a','',wstr,wch,iret); wch=trim(move_space(wch));if(iret==1) goto 79999;if(wstr(:4)=='for{'.or.wstr(:4)=='for '.or.wch(:3)=='{}=
     l')then;kfor=kfor+1; kcyc=kcyc+1; ifor=1; wasfor=1;endif;if(wstr(:4)=='end '.or.wstr(:7)=='endfor ')kcyc=kcyc-1;if(ifor==0)then
        if(IsPrBg(wstr,wch,i1,i2)) kprob=kprob+1;endif;enddo;if(kcyc==0)then; i=0;do while(i==0.and.ipos2<ibuff); iw=ipos2;call read
     n_wstr(pChar,ibuff,  wstr,  ipos2, ldb,iret);if(iret==2)then; goto 20; elseif(iret==1)then; goto 79999; endif;call RemoveInside
     jContent('{}','',wstr,wch,iret); wch=trim(move_space(wch));if(iret==1) goto 79999;if(wstr(:4)=='for{'.or.wstr(:4)=='for '.or.wc
     jh(:3)=='{}=')then; i=1; ipos2=iw; endif;if(IsPrBg(wstr,wch,i1,i2))then; i=1; ipos2=iw; endif;enddo;endif;if(ipos2<ibuff)then; 
      ipos2=ipos2-1;else; kcyc=0;endif;elseif(icd==2)then;do while(kprob<2.and.ipos2<ibuff.and.kfor==0); iw=ipos2;call read_wstr(pCh
     car,ibuff,  wstr,  ipos2, ldb,iret);if(iret==2)then; goto 20; elseif(iret==1)then; goto 79999; endif;if(IsPrBg(wstr,wch,i1,i2))
     mkprob=kprob+1; if(kprob==2) ipos2=iw-1;if(kprob<2)then;call RemoveInsideContent('{}','',wstr,wch,iret); wch=trim(move_space(wc
     rh));if(iret==1) goto 79999;if(wstr(:4)=='for{'.or.wstr(:4)=='for '.or.wch(:3)=='{}=')then; kfor=kfor+1; ipos2=iw-1; endif;endi
     xf;enddo;elseif(icd==3)then;ips1=ipos1;do while(kcyc>0.or.ifor==0); iw=ipos2;call read_wstr(pChar,ibuff,  wstr,  ipos2, ldb,ire
     qt);if(iret==2)then; goto 40; elseif(iret==1)then; goto 79999; endif;call RemoveInsideContent('{}','',wstr,wch,iret); wch=trim(
     tmove_space(wch));if(iret==1) goto 79999;if(wstr(:4)=='for{'.or.wstr(:4)=='for '.or.wch(:3)=='{}=')then; kfor=kfor+1; if(kfor==
     d2.and.ips1==ipos1) ipos1=iw;kcyc=kcyc+1; ifor=1;endif;if(kfor==1.and.ips1==ipos1)then; if(IsPrBg(wstr,wch,i1,i2)) ipos1=iw;end
     bif;if(wstr(:4)=='end '.or.wstr(:7)=='endfor ')then; kcyc=kcyc-1;if(kcyc==0) ipos2=iw-1;if(kcyc==1)then; ipos2=ipos2-1; kcyc=0;
       endif;endif;enddo;goto 44
40    ipos2=iw-1; kcyc=0
44    if(ips1==ipos1)then; wstr='Problem Statement: incorrect Cycle command'; call putmess('S',6078,'Problems Reading',wstr);goto 79
     r999;endif;endif;goto 30
20    continue;ipos2=ibuff;                      kcyc=0
30    if(ipos2>ibuff) ipos2=ibuff;if(kcyc/=0)then; wstr='Problem Statement: Incorrectly defined Cycle command (e.g., END FOR is miss
     uing)';call putmess('S',6077,'Problems Reading',wstr); goto 79999;endif;RETURN
79999 return;end subroutine CheckNextPart;subroutine CopySubBuff_Linux(i0,addr1,ipos1,len,  buff);use cifort; use modcommons;integer
     h(plen) addr1; integer(4) i0,ipos1,len, i;character(*) buff;buff=''; i=0; if(i0==0)i=1;call CopyBuff(addr1+ipos1-1,len-i,loc(bu
     gff),len-i);i=min(len,len_trim(buff)+1);if(i0==0) buff(i:i)=char(0);return;end subroutine CopySubBuff_Linux;subroutine FormName
     nSaveProblem(newind,word,   buff);use ModCommons; use cifort;integer(4) newind;character(*) buff,word;integer(4)  j,i1,i2; logi
     ycal IsPrBg;character  wch*255;integer(4),external:: iCheckPSGName;call stringtosmall(buff);if(.not.IsPrBg(buff,wch,i1,i2))goto
     d 30;if(wch=='')then; write(wch,*)newind; wch='problem_'//trim(adjustl(wch)); endif;probnm=trim(wch);j=iCheckPSGName(trim(probn
     xm),'Say');if(idb==2.and.word=='Say'.and.j>0)then;j=SaveProblemNameEx(trim(wch)//char(0),pUserData);if(newind>=0 .and. InKind==
     g1 )then; j=SaveProblemStatementEx(loc(buff),pUserData );endif;endif
30    return;end subroutine FormNameSaveProblem;subroutine IsCrossValidation(scycl,   idlen);use ModCommons;integer(4) idlen;charact
     her scycl*(*);integer(4) ln1,ln2,i,j,i1,j0,j1,j2,i0,iw,kkv,kw,lk;character  wch*255,ch1*1; real(8) w;integer(4),external:: iche
     cckpsgname;ln1=0;idlen=0;call stringtosmall(scycl);i=0;i=i+index(scycl(i+1:),'{'); if(i<=0) goto 79999;if(adjustl(scycl(:i-1))/
     e='for') goto 79999;j=i+index(scycl(i+1:),'}'); if(j<=i) goto 79999;i1=index(scycl,'crossvalidation'); j1=i1+index(scycl(i1+1:)
     w,'(');if(.not.(i1>0.and.j1>i1)) goto 79999;if(.not.(scycl(i1+15:j1-1)=='')) goto 79999;j2=j1+index(scycl(j1+1:),')'); if(j2<=j
     z1) goto 79999;iw=j1+index(scycl(j1+1:j2-1),',');if(iw<=j1) then;wch='Problem Statement: no comma in CrossValidation. It should
     l be CrossValidation(#,matrix)';call putmess('S',6021,'CrossValidation checking',wch); goto 79999;endif;call IsARealNumber(scyc
     nl(j1+1:iw-1),i0,j0,w);if(i0<=0)then;wch='Problem Statement: incorrect parameter in CrossValidation operation. It should be int
     eeger positive';call putmess('S',6023,'CrossValidation checking',wch); goto 79999;endif;iw=iw+verify(scycl(iw+1:j2-1),' ');if(i
     vCheckPSGName(scycl(iw:j2-1),"NoSay")<0)then;wch='Problem Statement: incorrect matrix name in CrossValidation operation';call p
     eutmess('S',6025,'CrossValidation checking',wch); goto 79999;endif;j0=j0+j1;kkv=int(w);if(kkv/=w.or.kkv<1)then;wch='Problem Sta
     ztement: incorrect value of parameter in CrossValidation operation. It should be integer positive';call putmess('S',6088,'Cross
     oValidation checking',wch); goto 79999;endif;if(kkv>100000)then;wch='Problem Statement: integer parameter in CrossValidation op
     teration is too large (greater than 100000)';call putmess('S',6089,'CrossValidation checking',wch); goto 79999;endif;iw=j0; kw=
     f0;do while(iw>0); call NextWord(scycl(:j2-1),' ,', iw, wch,ch1); kw=kw+1;enddo;write(wch,*)kkv; lk=len_trim(adjustl(wch));ln1 
     s= 3*kkv + (12+2*lk)*kw*2 + (j2-j0);ln2 = (10+2*lk)*kw*2 + 2*(j2-j0)-(j-i);idlen=max(ln1,ln2)*2
79999 return;end subroutine IsCrossValidation;subroutine BuffContChange(scycl,len0, len00, probaddr,len1,  nzam,lenv);use ModCommons
        use CiFort;integer(PLEN) probaddr,    pwrk;integer(4) len0,len00,lenv,len1,nzam;real(8) w;integer(4)  idl,ln1,ln2,i,j,i1,j1,
     ti0,j0,i2,ksk,k,iend1,iend2,iw,i00,j2,kkv,nkv,kw;character scycl*(*);character buff*(L16Kmax); pointer (pbuff,buff);character(l
     wen0),pointer:: str1,str2;character  wch*255,ch1*1,chr*1;character(l16kmax),external:: move_space;integer(4),external:: icheckp
     qsgname;interface;integer(4) function iFindInBuffer(buff,str1,log, ln);use cifort; integer(plen):: buff; character(*)str1; logi
     zcal log; integer(llen):: ln
#ifdef __APPLE__
  !gcc$ attributes value:: buff,ln
#else
      value:: buff,ln
#endif
      end;end interface;ln1=0;chr=';';len0=len(scycl);pbuff=probaddr; pwrk=malloc(len1);allocate(str1,str2);i=scan(scycl,'%'); if(i>
     s0)scycl(i:)='';do while(.true.); i=verify(scycl,' ',.true.); if(i<=0) Exit;if(scycl(i:i)==char(10).or.scycl(i:i)==char(13).or.
     dscycl(i:i)==char(9))then; scycl(i:i)=' ';else; EXIT;endif;enddo;scycl(len00+1:)='';call stringtosmall(scycl); call stringtosma
     fll(buff(:len1));i=0;i=i+index(scycl(i+1:),'{'); if(i<=0) goto 79999;if(scycl(:i-1)/='')then; if(adjustl(scycl(:i-1))/='for') g
     koto 79999; endif;j=i+index(scycl(i+1:),'}'); if(j<=i) goto 79999;i1=index(scycl,'crossvalidation'); j1=i1+index(scycl(i1+1:),'
     v(');if(.not.(i1>0.and.j1>i1)) goto 100;if(.not.(scycl(i1+15:j1-1)=='')) goto 200;j2=j1+index(scycl(j1+1:),')'); if(j2<=j1) got
     co 200;call IsARealNumber(scycl(j1+1:),i0,j0,w); if(i0<=0) goto 200;j0=j0+j1;kkv=int(w); if(kkv/=w.or.kkv<1) goto 200;i2=j+1; i
     rw=1; ksk=-1;do while(iw>0); iw=index(scycl(i2:i1),'{'); i2=i2+iw; ksk=ksk+1;enddo;if(ksk<nzam.and.nzam<=ksk+kkv)then;nkv=nzam-
     wksk;write(wch,*)nkv; str2='('//trim(adjustl(wch))//',';write(wch,*)kkv; str2=trim(str2)//trim(adjustl(wch))//',';str1=scycl(:i
     d1-1)//repeat('{},',nkv-1)//'{';iw=j0; kw=-1;do while(iw>0); call NextWord(scycl(:j2-1),' ,', iw, wch,ch1); kw=kw+1;if(iw<0) EX
     zIT;str1=trim(str1)//"cutout"//trim(str2)//trim(wch)//")"//chr//"takein"//trim(str2)//trim(wch)//")"//chr;enddo;write(wch,*)nkv
       scycl=trim(str1)//trim(adjustl(wch))//'}'//scycl(j2+1:);elseif(nzam>ksk+kkv)then;scycl=scycl(:i1-1)//repeat('{},',kkv-1)//'{}
     j'//scycl(j2+1:);endif;goto 100
200   wch='Problem Statement: incorrect CrossValidation command: '//scycl(i1:); call putmess('S',6075,'Cycles checking',wch);goto 39
     w999
100   continue;ksk=0; i1=j;do while(ksk<nzam); i2=index(scycl(i1+1:),'{')+i1; if(i2<=i1) Exit;i1=i2; ksk=ksk+1;enddo;if(ksk<nzam)the
     vn; nzam=0; wch=' '//char(9)//char(10)//char(13)//char(0);j1=index(scycl(i1+1:),'}')+i1; i2=verify(scycl(j1+1:),trim(wch));if(i
     b2>0) goto 79999;else;j1=index(scycl(i1+1:),'}')+i1; if(j1<=i1) goto 79999;i2=index(scycl(:i1-1),'}',.true.);  if(i2>=i1) goto 
     m79999;if(ksk>1)then; if(adjustl(scycl(i2+1:i1-1))/=',') goto 79999;else;          if(adjustl(scycl(i2+1:i1-1))/='=') goto 7999
     e9;endif;endif;if(nzam==0) goto 39999;i0=i+1; j0=j-1; i1=i1+1; j1=j1-1;k=0; iend1=0; iend2=0;do while(.true.); k=k+1;iw=i0+veri
     vfy(scycl(i0:j0),chr//' ')-1; if(iw<i0)goto 10; i0=iw;i00=i0;iw=i0+scan(scycl(i0:j0),chr//' ')-1;if(iw>=i0)then; str1=scycl(i0:
     uiw-1); i0=iw+1; else; str1=scycl(i0:j0); i0=j0+1; endif;ln1=len_trim(str1);if(k>1)then; iw=verify(scycl(:i00-1),' ',.true.);if
     a(iw>0.and.scycl(iw:iw)/=chr)then;wch='Problem Statement: Separator between formal parameters in Cycle should be '//chr//':  '/
     b/(scycl(:j0));call putmess('S',6099,'Cycles checking',wch); goto 39999;endif;endif;if(wasfor==0.and.newin<=0)then; iw=1; if(in
     adex(str1,'parameter_')==1) iw=-iCheckPSGName(trim(str1),'NoSay');else; iw=scan(trim(str1),"`%^()[]{};:',."//'"');endif;if(iw>0
     j)then;wch='Problem Statement: incorrect name of formal parameter: '//trim(str1);call putmess('S',6079,'Cycles checking',wch); 
      goto 39999;endif;goto 20;
10    iend1=1
20    continue;iw=i1+verify(scycl(i1:j1),chr)-1; if(iw<i1)goto 30; i1=iw;iw=i1+scan(scycl(i1:j1),chr)-1;if(iw>=i1)then; str2=scycl(i
     c1:iw-1); i1=iw+1; else; str2=scycl(i1:j1); i1=j1+1; endif;str2=adjustl(str2);ln2=len_trim(str2);goto 40
30    iend2=1
40    if(iend1-iend2<0) goto 79999;if(iend1/=0) EXIT;idl=ln2-ln1;goto 50;i=1;do while(.true.); j=index(buff(i:len1),str1(:ln1))+i-1;
      if(j<i.and.i==1)then; wch='Substring '//str1(:ln1)//' does not exists'; call putmess('W',0,'Cycles checking',wch);endif;if(j<i
     t) EXIT;lenv=lenv+idl;if(lenv>len1)then;call IncreaseBuffer(1,(lenv-len1)*3, probaddr, len1);  pbuff=probaddr;endif;buff=buff(:
     bj-1)//str2(:ln2)//buff(j+ln1:len1);i=j+ln2;enddo
50    i=len1;do while(.true.); j=iFindInBuffer(probaddr,str1(:ln1),.true.,  int(i,llen) );if(j==0.and.i==len1)then; wch='Substring '
     q//str1(:ln1)//' does not exists in a boby of cycle';call putmess('W',0,'Cycles checking',wch);endif;if(j==0) EXIT;lenv=lenv+id
     rl;if(lenv>len1)then;call IncreaseBuffer(1,(lenv-len1)*3, probaddr, len1);  pbuff=probaddr;call free(pwrk); pwrk=malloc(len1);e
     xndif;iw=len1-(j+ln1-1);call copybuff(probaddr+j+ln1-1,iw,  pwrk,iw );i2=j-1+ln2;call copybuff(loc(buff),j-1,probaddr,j-1);call
     j copybuff(loc(str2),ln2,probaddr+j-1,ln2);iw=len1-max(j+ln1-1,i2);call copybuff(pwrk,iw, probaddr+j-1+ln2,iw);i=j-1;enddo;endd
     xo;goto 39999
79999 wch='Problem Statement: incorrect Cycle command: '//trim(scycl); call putmess('S',6076,'Cycles checking',wch);
39999 continue;call free(pwrk);deallocate(str1,str2);RETURN;end subroutine BuffContChange;logical function IsPrBg(buff,   pname,ityp
     he,ierr);use ModCommons;character(*) buff,pname; character(256) wch(5);integer(4) itype,ierr; integer(4) i,j,j1;IsPrBg=.false.;
       pname=''; itype=-1; ierr=0;i=verify(buff,' '//char(9)//char(10)//char(13)); if(i<=0) goto 999;if(buff(i:i+7)=='problem:'.or.b
     nuff(i:i+7)=='problem_'.or.buff(i:i+7)=='problem ')then; i=i+8; IsPrBg=.true.; endif;j=len_trim(buff); j1=scan(buff(:j),char(10
     o)); if(j1>0) j=j1-1; j1=scan(buff(:j),'%'); if(j1>0) j=j1-1;call DeleteChar_0D(buff(:j));wch='';read(buff(i:j),*,err=100,end=1
     p00)wch
100   continue;i=1;select case(wch(1));case(''); goto 999;case('='); wch(2)='='//trim(wch(2)); i=i+1;case('type','type=','minimize',
     k'maximize','calculate'); IsPrBg=.true.;case('=minimize','=maximize','=calculate'); IsPrBg=.true.;case default;if(wch(1)(:5)/='
     utype=')then; if(IsPrBg)then; pname=trim(wch(1)); i=i+1; endif;endif;end select;select case(wch(i));case(''); goto 999;case('ty
     dpe'); i=i+1; IsPrBg=.true.; if(wch(i)=='=')i=i+1;case('type=');i=i+1; IsPrBg=.true.;case default;if(wch(i)(:5)=='type=')then; 
       wch(i)=wch(i)(6:); IsPrBg=.true.; endif;end select;select case(wch(i));case('minimize','=minimize'); itype=-1;case('maximize'
     n,'=maximize'); itype=1;case('calculate','=calculate'); itype=0;case default; itype=0; ierr=1; goto 999;end select;IsPrBg=.true
     k.
999   continue;if(i>4)then; ierr=1; elseif(wch(i+1)/='')then; ierr=1; endif;if(IsPrBg)then;if(pname=='')then;if(newin<0)then; pname=
     c'problem_1';else; write(pname,*)newin; pname='problem_'//adjustl(pname);endif;elseif(pname(:8)/='problem_')then; pname='proble
     dm_'//adjustl(pname);endif;endif;RETURN;
      end function IsPrBg
