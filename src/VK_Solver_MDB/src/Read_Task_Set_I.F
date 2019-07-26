      subroutine READ_TASK_0(probaddr,   mxkf,mxconstr,nabmx,  l16p,km,kc);use ModCommons; use cifort;integer(plen) probaddr;integer
     a(4) mxkf,mxconstr,nabmx,km,kc;integer(4) l16p,i,j,k,ibuff,iBuffLen;integer(PLEN),pointer:: pChar; integer(PLEN),target:: iu;i=
     x0; j=0; k=0; iu=0; km=0; kc=0;pChar=>iu;if(probaddr>0)then; call setpointer(probaddr, pChar); ibuff=iBuffLen(probaddr);call Ma
     vxRowBuff_00(pChar,ibuff,i,j,k,km,kc);endif;l16k=min0(j+10,L16Kmax); mxconstr=min0(mxconstr,km)+100; mxkf=min0(mxkf,i-2)+100; n
     dabmx=min0(nabmx,km)+100;l16p=min0(2*lnm,max0(l16k-23+35-14,68))+min0(lnm,(l16k-14))+2;l16p=max0(l16p,lnm+25);l16p=max0(l16p,l1
     n6k); l16k=l16p;end subroutine READ_TASK_0;subroutine READ_TASK (probaddr,mxkf,mxconstr,nabmx,nmx,km,xbndhuge,l16p,    xname,xl
     qb,xub,ixord,sign_min,lconvex,n1,kconstr,knab,nz0,kmatr,kpmtr,timelimit,TypesPointName,allLbounds,allUbounds,LowerPointName,Upp
     zerPointName);USE CiFort; use ModCommons; use FuncNames;integer(plen) probaddr;integer(4) mxkf,mxconstr,nabmx,nmx,l16p,km;chara
     hcter(lnm) xname(-3:*); integer(4) ixord(*);real(8) xbndhuge, xlb(*),xub(*);integer(4)  n1,kconstr,knab,nz0;real(8) sign_min, t
     nimelimit,allLbounds,allUbounds;character(*) TypesPointName,LowerPointName,UpperPointName;logical lconvex;real(8) bnds0(0:1,0:*
     b),   wfn(*),cfn(*),tfn(*);character(lnm) fname0(*), mname0(*);integer(4) nfn(*),  ncn(*),  nfz(*), kb0(0:*),    kmtrn0(*),nfma
     mtr0(*),itnab0(*),nmatr0(*),lnrz0(0:kconstr);integer(plen) jaddr3(2,*);real(8) dconf1, dconf2, dconf22;common /dconf/dconf1, dc
     eonf2, dconf22;real(8) qpeps; real(4) wza(2),wzaf,awm; data wza/0.,0./;equivalence(wzaf,awm);COMMON /CMACHE/qpEPS,awm;logical l
     sf1,lf2,lf3,lf4,lf5,lf6,lf7;common /SetFirst/lf1,lf2,lf3,lf4,lf5,lf6,lf7;logical lfza; equivalence (lf7,lfza);integer(4) nz,ios
     otat,ISTR_WITHOUT_SUB,nc,inz,ideall,nabpmx,matrmx,iret;integer(4) i,j,k,i1,j0,j1,id,ibuff,lrest,m,kpmtr,kmatr,itt,ktmtr,inewnab
     h,nfg;real(8) w,w1,xhuge,wparam;integer(PLEN),pointer:: pChar;integer(PLEN) jaddr;logical not_task, lDB,lw,IsPrBg;character(l16
     xp) wstr, pwstr, wch, ch1*(1),ch9*1;character(l16kmax),external:: move_space;character(l16p),allocatable:: chm(:);integer(4) k1
     v,kfunc,icalc,nf1,nf2,iopn,i0,icut,ilmax,ilex,iksm,irec,iexl,itrn,ispl,itsp,iabs,isum,idif,iccomp,iw,i2,j2,ln,j3,j4,lh,ib,k0,kl
     jamb;integer(4),external:: ibufflen,isl1l2summ,ifindclosebracket,icheckpsgname;integer(4),allocatable:: kf(:),nf(:,:),nnab(:,:)
     r,kmtrn(:),nfmatr(:),itnab(:),nmatr(:),kb(:), lnrz(:),ipfunc(:,:);real(8),allocatable::cf(:,:),tf(:,:),wf(:,:),wprm(:),bnds(:,:
     i);character(lnm),allocatable::mname(:),cname(:),obname(:,:),vector(:), ch3(:);character(l16kmax),allocatable:: fname(:);charac
     kter(lnm) MnameFromCutTake;save jaddr, kf,cf,tf,nf,wf,nnab,obname,cname,lnrz, kb,bnds,fname,mname,kmtrn,nfmatr,ipfunc,itnab,nma
     etr;ib=0; iw=0; itrn=0; irec=0; iexl=0; ispl=0;dconf1=scale(1d0,-26);dconf2=scale(1d0,-43);dconf22=1d0-dconf2;   ch9=char(9);ib
     quff=0;if(probaddr>0)then; call setpointer(probaddr, pChar); ibuff=iBuffLen(probaddr); endif;if(ibuff<=0)then; wch='Empty probl
     zem buffer'; call putmess('S',612,'Problem Reading',wch); goto 79999;endif;lDB=.true.; jaddr=0; lrest=1;not_task=.true.;   xhug
     ke=huge(w)/2.; sign_min=1.;nabpmx=km*3; matrmx=km*2;allocate(cname(0:mxconstr),lnrz(0:mxconstr),kb(0:mxconstr),bnds(0:1,0:mxcon
     pstr),kf(0:mxconstr),cf(mxkf,0:mxconstr),tf(mxkf,0:mxconstr),nf(mxkf,0:mxconstr),wf(mxkf,0:mxconstr),nnab(mxkf,0:mxconstr),obna
     pme(mxkf,0:mxconstr),ipfunc(mxkf,0:mxconstr),fname(nabmx),wprm(nabmx),itnab(nabmx),kmtrn(nabmx),nfmatr(nabmx),nmatr(nabpmx),mna
     pme(matrmx),STAT=iostat);if(iostat.ne.0)then; wch='Arrays allocation is failed'; call putmess('S',6040,'Problem Reading',wch); 
      goto 79999; endif;allocate(vector(0:1), ch3(3), chm(4), STAT=iostat);if(iostat.ne.0)then; wch='Arrays allocation is failed'; c
     pall putmess('S',6043,'Problem Reading',wch); goto 79999; endif;icalc=-1;TypesPointName=''; LowerPointName=''; UpperPointName='
     l';bnds(0,:)=-xhuge; bnds(1,:)=xhuge;cname=''; obname=''; lnrz=0;tf=0.;xname(-3)='value';xname(-2)='id';xname(-1)='scenario_pro
     abability';xname(0)= 'scenario_benchmark';ipfunc=huge(kfunc); kfunc=0;n1=0;knab=0;kconstr=0;kpmtr=0;kmatr=0;wprm=0d0;kf=0;wf=0d
     b0;wstr=''; vector=''; wch=''; goto 18
15555 wch='Problem Statement: incorrect row: '//trim(pwstr);call putmess('S',608,'Problem Reading',wch); goto 79999
18    do while(wstr(:1)=='%'.or.wstr==''); call read_wstr(pChar,ibuff,wstr,lrest,ldb,iret);if(iret==2)then; goto 20; elseif(iret==1)
     lthen; goto 79999; endif;enddo;pwstr=wstr;if(.not.IsPrBg(wstr,wch,i,j)) goto 15555; if(j/=0) goto 15555;goto 35
20    continue;if(ldb) wch='Problem section of Problem Statement: no keyword Problem or some string defining beginning of problem '/
     x/trim(probnm);if(idb<=0) wch='No keyword Problem: or problems content in file '//trim(taskfname);call putmess('S',610,'Problem
     q Reading',wch); goto 79999;i=istr_without_sub(wstr,'problem:',wstr);wch='';if(wstr(:5)=='type='.or.wstr(:5)=='type '.or.wstr==
     p'') goto 31;if(wstr(:1)==',') goto 30;read(wstr,*, end=30,err=30) wch;goto 31
30    wch='Problem section of Problem Statement: incorrect Problem_name';call putmess('S',613,'Problem Reading',wch); goto 79999
31    continue;i=istr_without_sub(wstr,trim(wch),wstr)
35    continue;if(probnm/=wch) then;write(wch,'(a,i5)')'Length of problem name '//trim(wch)//' exceeds Max=',lnm;call putmess('W',0,
     k'Problem Reading',wch); if(wch=='S') goto 79999;endif;wstr=''; sign_min=-i;if(sign_min==10.)sign_min=0.;if(sign_min==0.)then; 
      tqsol=0;endif;if(wstr=='') goto 52;if(index(wstr,'type')<=0) goto 15555;wch=''; read(wstr,*, end=30) wch;if(wch=='') read(wstr
     h,*, end=30) wch,wch;if(wch(:5)/='type='.and.wch(:5)/='type ') goto 15555;i=istr_without_sub(wstr,'type',wstr);if(wstr(:1)=='='
     r)then; i=istr_without_sub(wstr,'=',wstr); else; goto 50; endif;chm(1)='';read(wstr,*,end=50,err=50) chm(1);goto 51
50    wch='Problem section of Problem Statement: incorrect Problem_type'; call putmess('S',622,'Problem Reading',wch); goto 79999
51    continue;i=istr_without_sub(wstr,trim(chm(1)),wstr);if(chm(1).eq.'minimize') then;     sign_min=+1d0;elseif(chm(1).eq.'maximiz
     ce')then;  sign_min=-1d0;elseif(chm(1).eq.'calculate')then; sign_min= 0d0;else; GOTO 50;endif;if(verify(wstr,' d,'//ch9)>0) got
     to 15555
52    continue;nc=0;do while(wstr(:1)=='%'.or.wstr==''); call read_wstr(pChar,ibuff,wstr,lrest,ldb,iret);if(iret==2)then; goto 1001;
       elseif(iret==1)then; goto 79999; endif;enddo;pwstr=wstr;if(wstr(:10)=='objective:'.or.wstr(:10)=='objective_'.or.wstr(:10)=='
     jobjective ')then; wstr=adjustl(wstr(11:));else; call IsFunctionRow(wstr,fnc_name,kfn,    w,chm,nfg,id,nf1);if(nf1>=0) goto 75;
      if(wstr(:10)=='constraint'.and.scan(wstr(11:11),' ,:_=<>')==1) goto 100;if(wstr(:6)=='value:'.or.wstr(:6)=='value ') goto 100;
      if(wstr(:6)=='point:'.or.wstr(:6)=='point ')goto 100;if(wstr(:4)=='box:'.or.wstr(:4)=='box '.or.wstr(:4)=='box='.or.wstr(:4)==
     q'box<'.or.wstr(:4)=='box>') goto 1071;if(wstr(:17)=='box_of_variables:'.or.wstr(:17)=='box_of_variables '.or.wstr(:17)=='box o
     nf variables ') goto 1071;if(wstr(:7)=='solver:'.or.wstr(:7)=='solver ') goto 1072;i=0; goto 54;endif;i=scan(wstr,',');if(i<=0)
     wthen; i=scan(wstr,' '); if(i<=0)i=len_trim(wstr)+1;j=1; call NextWord(wstr,'=',j,ch3(1),ch1);select case(ch3(1)); case('linear
     xize','mip','quadratic','function'); if(ch1=='=')i=0;end select;else; if(wstr(:i-1)=='') goto 15555;  if(scan(trim(wstr(:i-1)),
     x' =:')>0) goto 15555;endif;cname(nc)=trim(wstr(:i-1))
54    continue;j=i+1; call NextWord(wstr,'=',j,ch3(1),ch1);if(ch3(1)=='linearize'.and.ch1=='=')then;  call NextWord(wstr,' ',j,ch3(1
     q),ch1);if(ch3(1)=='0')then; lnrz(nc)=-1; elseif(ch3(1)=='1')then; lnrz(nc)=+1; else; goto 55; endif;if(wstr(j:)/='') goto 1555
     t5;elseif(ch3(1)=='mip'.and.ch1=='=')then;  call NextWord(wstr,' ',j,ch3(1),ch1);if(ch3(1)=='0')then; lnrz(nc)=0; elseif(ch3(1)
     d=='1')then; lnrz(nc)=10; else; goto 55; endif;if(wstr(j:)/='') goto 15555;elseif(ch3(1)=='quadratic'.and.ch1=='=')then;  call 
     jNextWord(wstr,' ',j,ch3(1),ch1);if(ch3(1)=='0')then; lnrz(nc)=0; elseif(ch3(1)=='1')then; lnrz(nc)=-2; else; goto 55; endif;if
     b(wstr(j:)/='') goto 15555;elseif(nc==0.and.ch3(1)=='function'.and.ch1=='=')then;  call NextWord(wstr,' ',j,ch3(1),ch1);if(ch3(
     p1)=='0')then; lnrz(nc)=0; elseif(ch3(1)=='1')then; lnrz(nc)=-3; else; goto 55; endif;if(wstr(j:)/='') goto 15555;else; if(j>=0
     i) goto 15555;endif;if(cname(nc)(:lnm-1)/=wstr(:i-1)) then;write(wch,'(a,i5,a)')'Problem Statement: length (number of symbols) 
     eof objective name exceeds Max=',lnm-11,': '//trim(wstr(11:i-1));call putmess('S',624,'Problem Reading',wch); goto 79999;endif;
      goto 62
55    wch='Problem Statement: only one option with value 0 or 1 may be used for objective: '//trim(wstr);call putmess('S',626,'Probl
     xem Reading',wch); goto 79999
62    continue;wstr='';do while(wstr.eq.''.or.wstr(:1)=='%'); call read_wstr(pChar,ibuff,wstr,lrest,ldb,iret);if(iret==2)then; goto 
     x1001; elseif(iret==1)then; goto 79999; endif;enddo
65    continue;if(icalc==-1)then;if(wstr(:10)=='constraint'.and.scan(wstr(11:11),' ,:_=<>')==1) goto 100;if(wstr(:4)=='box:'.or.wstr
     d(:4)=='box '.or.wstr(:4)=='box='.or.wstr(:4)=='box<'.or.wstr(:4)=='box>') goto 100;if(wstr(:17)=='box_of_variables:'.or.wstr(:
     h17)=='box_of_variables '.or.wstr(:17)=='box of variables ') goto 100;if(wstr(:7)=='solver:'.or.wstr(:7)=='solver ')goto 100;if
     h(wstr(:6)=='value:'.or.wstr(:6)=='value ')goto 100;if(wstr(:6)=='point:'.or.wstr(:6)=='point ')GOTO 100;else;if(wstr(:4)=='box
     j:'.or.wstr(:4)=='box '.or.wstr(:4)=='box='.or.wstr(:4)=='box<'.or.wstr(:4)=='box>') goto 100;if(wstr(:17)=='box_of_variables:'
     o.or.wstr(:17)=='box_of_variables '.or.wstr(:17)=='box of variables ') goto 100;if(wstr(:7)=='solver:'.or.wstr(:7)=='solver ')g
     loto 100;if(wstr(:6)=='value:'.or.wstr(:6)=='value ')goto 100;if(wstr(:6)=='point:'.or.wstr(:6)=='point ')GOTO 100;if(wstr(:10)
     r=='constraint'.and.scan(wstr(11:11),' ,:_=<>')==1)then;wch='Problem Statement: Constraint: section should not appear after key
     tword Value: or Point: '//trim(wstr);call putmess('S',630,'Problem Reading',wch); goto 79999;endif;endif
75    continue;pwstr=wstr;call IsFunctionRow(wstr,fnc_name,kfn,    w,chm,nfg,id,nf1);if(nf1<0)then; wch='Function Row in Problem Sta
     vtement: incorrect string: '//trim(wstr);call putmess('S',631,'Problem Reading',wch); goto 79999;endif;if(kf(nc)==mxkf)then;wri
     hte(wch,'(2(a,i4))')'Problem Statement: number of functions in constraint #',nc,' exceeds Max=',mxkf;call putmess('S',628,'Prob
     glem Reading',wch); goto 79999;endif;if(w==0.)then; i=min(len_trim(wstr),120);wch='Zero coefficient for function in row: '//tri
     em(wstr(:i))//'...';call putmess('W',0,'Problem Reading',wch); if(wch=='S') goto 79999;endif;kf(nc)=kf(nc)+1;cf(kf(nc),nc)=w;  
      if(icalc>=0)cf(kf(nc),nc)=0.;nf(kf(nc),nc)=nf1;tf(kf(nc),nc)=0.;kfunc=kfunc+1; ipfunc(kf(nc),nc)=kfunc;if(cf(kf(nc),nc)/=0..an
     cd.icalc==-1.and.it_id==0.and.sign_min/=0.)then;select case(nf1);case(441,1160:1172,1180,1190,1200,1201,1270,1271);wch='Problem
     h Statement: vector function cannot be used in objective or constraint: '//trim(pwstr);call putmess('S',627,'Problem Reading',w
     ich); goto 79999;end select;endif;if(nf1==1270.or.nf1==1271)then;if(nf1==1270)tf(kf(nc),nc)=2.; if(nf1==1271)tf(kf(nc),nc)=3.;i
     n=index(wstr,'('); j=index(wstr,')',.true.);if(i>0.and.i<j-1)then; wstr=wstr(i+1:j-1); else; goto 15555; endif;call IsFunctionR
     zow(wstr,fnc_name,kfn,    w,chm,nfg,id,nf2);if(nf2<0)goto 15555;select case(nf2);case(441,1160:1172,1180,1190,1200,1201,1270,12
     l71);wch='Problem Statement: Gradient and Increment can not be calculated for vector function: '//trim(pwstr);call putmess('S',
     n629,'Problem Reading',wch); goto 79999;case(430); if(nf1==1271)then;wch='Problem Statement: Increment can not be calculated fo
     lr function: '//trim(pwstr);call putmess('S',6291,'Problem Reading',wch); goto 79999;endif;end select;pwstr=wstr;nf1=nf2; nf(kf
     v(nc),nc)=nf1;endif;i=istr_without_sub(wstr,trim(chm(1)),wstr);iOpn=index(wstr,'(');i=index(pwstr,'(');i0=index(pwstr,trim(chm(
     l1)));if(i>1)then; obname(kf(nc),nc)=trim(pwstr(i0:i-1));if(obname(kf(nc),nc)/=trim(pwstr(i0:i-1))) then;write(wch,'(a,i5,a)')'
     oProblem Statement: length (number of symbols) of function name exceeds Max=',lnm,': '//trim(pwstr(i0:i-1));call putmess('S',63
     f2,'Problem Reading',wch); goto 79999;endif;itt=len_trim(obname(kf(nc),nc));if(scan(trim(pwstr(i0:i-1)),' ')>0) then;write(wch,
     f*)'Problem Statement: incorrect function name: '//trim(pwstr); call putmess('S',635,'Problem Reading',wch);goto 79999;endif;el
     ese; wch='';if(i<=0)then; wch='Function Row in Problem Statement: missing ( in: '//trim(pwstr);endif;if(wch/='') then; call put
     tmess('S',633,'Problem Reading',wch); goto 79999; endif;endif;i=iOpn;select case(nfg);case(2:3,7:8,14:19,21:22,28:33,34,36,37,3
     u8,40,41,45:56,64,67:70,73:81,82:83,85:96,104,105,107,112:115,123:124,129:131,132,134,135,136:137,140);i1=index(wstr,',');if(i1
     z==0.or.i+1>i1-1) goto 15555;call IsARealNumber(wstr(i+1:), k,j1,w);i1=index(wstr(i+1+j1-1:),',')-1+(i+1+j1-1);if(i1<(i+1+j1-1)
     n.or.i+1>i1-1) goto 15555;if(k<=0) goto 77; if(wstr(i+1+j1-1:i1-1)/='') goto 15555;select case(nfg);case(2:3,21:22,37,53:54,73:
     p74,77:79,82:83,93:94,112:113,129:131,134,135);if(w>1d0.or.w<0d0)then; wch='Problem Statement: incorrect parameter in CVaR (CDa
     qR) function in: '//trim(pwstr)//'. It should be in range (0,1)';call putmess('S',634,'Problem Reading',wch); goto 79999;endif;
      if(w>1d0-dconf1) w=1d0-dconf1     ; if(w<dconf1) w=dconf1;case(14:15,38,55:56,75:76,80:81,95:96);if(w>1d0.or.w<0d0) then;wch='
     iProblem Statement: incorrect parameter in VaR function: '//trim(pwstr)//'. It should be in range (0,1)';call putmess('S',637,'
     pProblem Reading',wch); goto 79999;endif;if(w>1d0-dconf1) w=1d0-dconf1     ; if(w<dconf1) w=dconf1;if(w<0.5.and.icalc==-1) then
       wch="VaR_Conf_Lev<0.5. Algorithm may provide a poor solution for VaR:"//trim(pwstr);call putmess('W',0,'Problem Reading',wch)
        if(wch=='S') goto 79999;endif;case(36);if(nf1==360)then; if(w==0d0)cf(kf(nc),nc)=w;if(w>1d0)then; wch='Problem Statement: in
     vcorrect parameter in Power Utility function: '//trim(pwstr)//'. It should be in range (0,1)';call putmess('S',638,'Problem Rea
     cding',wch); goto 79999;endif;elseif(nf1==361)then;if(w>1d0.or.w<0d0)then; wch='Incorrect parameter for CVaR (CDaR) in row: '//
     rtrim(pwstr);call putmess('S',634,'Problem Reading',wch); goto 79999;endif;if(w>1d0-dconf1) w=1d0-dconf1     ; if(w<dconf1) w=d
     zconf1;w=1d0-w;endif;case(34,64,104);if(w<=0.)then; wch='Incorrect value of parameter in Exponential Utility function: '//trim(
     cpwstr)//'. It should be > 0';call putmess('S',6390,'Problem Reading',wch); goto 79999;endif;case(107);if(w<1.)then; wch='Incor
     rrect value of parameter in Lp_norm_... function: '//trim(pwstr)//'. It should be < 1';call putmess('S',6393,'Problem Reading',
     awch); goto 79999;endif;case(123:124);if(.not.(0.<w.and.w<1.))then;wch='Incorrect value of parameter in Kb_err function: '//tri
     tm(pwstr)//'. It should be in range (0,1)';call putmess('S',6396,'Problem Reading',wch); goto 79999;endif;case(105,132); i=int(
     nw);if(.not.(w>=1..and.w==i))then;if(nfg==105) wch='Incorrect parameter in Markov model function: '//trim(pwstr)//'. It should 
     zbe integer positive';if(nfg==132) wch='Incorrect parameter in Kantorovich function: '//trim(pwstr)//'. It should be integer po
     ssitive';call putmess('S',6398,'Problem Reading',wch); goto 79999;endif;end select;select case(nfg);case(2:3,14:15,21:22,36,37,
     j38,53:56,73:81,82:83,93:96,112:113,123:124,129:131,134,135); w1=1d0;case default;                      w1=0d0;end select;if(id
     v.eq.1) w=w1-w;case default;i1=iOpn; w=0d0;end select;wf(kf(nc),nc)=w;wParam=w;GOTO 80
77    continue;wch='Problem Statement: incorrect parameter in function: '//trim(pwstr); call putmess('S',640,'Problem Reading',wch);
      goto 79999
80    continue;i=index(wstr,')',.true.);if(verify(wstr(i+1:),' d,'//ch9)>0) goto 15555;chm(1)=wstr(i1+1:i-1);if(chm(1).eq.'')then;wc
     th='Empty argument list in '//trim(pwstr); call putmess('W',0,'Problem Reading',wch);endif;chm(1)=move_space(chm(1));icut=0;ilm
     max=0;if(chm(1)(:4)=='lmax')then;if(chm(1)(5:index(chm(1),'(')-1)==''.and. chm(1)( len_trim(chm(1)):)==')' ) ilmax=1;if(ilmax==
     f1)then; chm(2)=adjustl(chm(1)( index(chm(1),'(')+1 : index(chm(1),')',.true.)-1));if(chm(2)(:4)=='cut('.and.chm(2)(len_trim(ch
     wm(2)):)==')') icut=1;endif;endif;ilex=0;if(index(chm(1)(:index(chm(1),'(')),'logexp_')==1)then; ilex=1;chm(1)=chm(1)(index(chm
     b(1),'(')+1:index(chm(1),')',.true.)-1);endif;iksm=0; if(index(chm(1)(:index(chm(1),'(')),'ksm_')==1) iksm=1;if(icut==0)then;if
     a(index(chm(1)(:index(chm(1),'(')),'cut_')==1.or. chm(1)(:4)=='cut(') icut=1;endif;irec=0; if(index(chm(1)(:index(chm(1),'(')),
     j'recourse_')==1) irec=1;if(index(chm(1)(:index(chm(1),'(')),'recourse(')==1) irec=1;if(index(chm(1)(:index(chm(1),'(')),'recou
     frse ')==1) irec=1;iExL=0; if(index(chm(1)(:index(chm(1),'(')),'fnextdir_')==1) iExL=1;itrn=0; if(index(chm(1)(:index(chm(1),'(
     g')),'ltranche')==1) itrn=1;ispl=0; if(index(chm(1)(:index(chm(1),'(')),'spline_sum_')==1) ispl=2;if(index(chm(1)(:index(chm(1)
     o,'(')),'spline_sum(')==1) ispl=2;if(index(chm(1)(:index(chm(1),'(')),'spline_sum ')==1) ispl=2;itsp=0; if(index(chm(1)(:index(
     uchm(1),'(')),'tsp_')==1) itsp=1;if(index(chm(1)(:index(chm(1),'(')),'tsp(')==1) itsp=1;if(index(chm(1)(:index(chm(1),'(')),'ts
     rp ')==1) itsp=1;iabs=0; if(index(chm(1)(:index(chm(1),'(')),'abs')==1)then;if(chm(1)(4:index(chm(1),'(')-1)=='')then; iabs=1;e
     yndif;endif;isum=0; if(index(chm(1)(:index(chm(1),'(')),'ssum')==1)then;if(chm(1)(5:index(chm(1),'(')-1)=='')then; isum=1;endif
       endif;idif=0; if(index(chm(1)(:index(chm(1),'(')),'sdif')==1)then;if(chm(1)(5:index(chm(1),'(')-1)=='')then; idif=1;endif;end
     tif;if(isum+idif==0)then; isum=IsL1L2Summ(chm(1)); if(isum<0)then; idif=-isum; isum=0; endif;endif;iccomp=0; i=index(chm(1),'('
     p);if(i>0.and.idif==0.and.isum==0)then;i=index(chm(1)(:i),'*');call IsFunctionRow(chm(1)(i+1:),fnc_name,kfn,    w,chm(2),i,j,nf
     m2);if(nf2>0)then;select case(nf2);case(361,370); iccomp=1;case(1190,1191,1200,1201);case(1160:1172,441);case default;wch='Prob
     alem Statement: not allowed composition: '//trim(pwstr); call putmess('S',641,'Problem Reading',wch);goto 79999;end select;endi
     sf;endif;if(nfg==20) then;if(vector(0)==''.and.bnds(0,nc)>-xhuge) then;write(vector(0),'(a,e22.15,i5.4)')'vector#',bnds(0,nc),n
     gc;endif;if(vector(1)==''.and.bnds(1,nc) <xhuge) then;write(vector(1),'(a,e22.15,i5.4)')'vector#',bnds(1,nc),nc;endif;if(vector
     f(0).ne.'') chm(1)=trim(chm(1))//','//trim(vector(0));if(vector(1).ne.'') chm(1)=trim(chm(1))//','//trim(vector(1));elseif(vect
     oor(0)/=''.or.vector(1)/='')then;wch='Problem Statement: bound vector is not allowed in constraint: '//trim(pwstr);call putmess
     b('S',642,'Problem Reading',wch); goto 79999;endif;itt=0;select case(nfg);case(3,5,6,8,10,12,15,17,19,21:26, 41,79,111,113,115,
     x109,131,137);  itt=2;end select;itt=itt+id;select case(nfg);case(20);     itt=4;if(vector(0).ne.'') then;if(vector(1).ne.'')th
     sen; itt=7; else; itt=5; endif;elseif(vector(1).ne.'')then; itt=6;endif;case(21:26);  itt=itt+6;case(27,43);  itt=10;  itt=itt+
     oid;if(nfg==43) itt=11;case(28:33);  itt=12;case(34:36,117,118);  itt=13;if(nf1==361)itt=114;case(37:39);  itt=14;case(44);if(n
     af1==440)then; itt=15;else; iExL=1;endif;chm(1)=trim(obname(kf(nc),nc))//'('//trim(chm(1))//')';case(45:48);  itt=16;case(49:64
     u);  itt=17;case(67:70);  itt=18;case(73:76);  itt=18;case(77)   ;  itt=19;case(78:81);  itt=22;case(82)   ;  itt=23;case(83:84
     v);  itt=24;case(132);    itt=34;case(85:88);  itt=20;case(89:104); itt=21;case(107);    itt=28;case(121);    itt=25;case(129:1
     n31);itt=30;case(133,134);    itt=26;case(135);    itt=27;case(105);    itt=29;case(139);    itt=114;case(140);    itt=440;case
     n(116);if(ilex>0) itt=13;if(iksm>0) itt=24;case(119);if(id==0)then; ispl=2;if(ilex>0) itt=13;chm(1)='spline_sum'//trim(wstr); w
     dstr='('//trim(chm(1))//')';else; itsp=1;endif;case(120);if(nf1==1200)then; irec=1;chm(1)='recourse'//trim(wstr); wstr='('//tri
     ym(chm(1))//')';else; itrn=1;chm(1)='ltranche'//trim(wstr); wstr='('//trim(chm(1))//')';endif;end select;if(irec>0) itt=200;if(
     eitrn>0) itt=201;if(iExL>0) itt=202;if(icut>0) itt=itt+300;if(ispl>0) itt=itt+400;if(itsp>0)then; itt=itt+450;if(itt/=450)then;
       wch='Problem Statement: TSP function may be used only with linear';call putmess('S',6443,'Problem reading',wch); goto 79999;e
     vndif;endif;if(iccomp>0) itt=nf2+10000*nf1;if(ilmax==1.and.(icut==0.and.itt>0.or.icut==1.and.itt>300))then;endif;if(icut==1.and
     z.(itt==302.or.itt==303))then;wch='Problem Statement: functions using Deviation of Loss cannot use Cut(E) operation in the curr
     went version of PSG: '//trim(pwstr);call putmess('S',6441,'Problem reading',wch); goto 79999;endif;if(iabs==1.and.(itt<=3.or.it
     st==14).and.nfg/=42)then; itt=itt+100;elseif(iabs==1)then;wch='Problem Statement: superposition of function '//trim(pwstr)//' a
     vnd Abs in not implement in the current version of PSG';call putmess('S',6445,'Problem reading',wch); goto 79999;endif;if(isum=
     n=1.and.itt<=3) itt=itt+500;if(isum==2.and.itt<=3) itt=itt+510;if(idif==1.and.itt<=3) itt=itt+550;if(idif==2.and.itt<=3) itt=it
     pt+540;if(isum>0.or.idif>0)then; iw=1;if(itt<500.or.itt>553)iw=0;select case(nfg); case(0,4:6,11:13,18:19,110:115,116,119); iw=
     z0; end select;if(iw==0)then; wch='Problem Statement: function '//trim(pwstr)//' cannot use Addition/Subtraction of Losses in t
     the current version of PSG';call putmess('S',6447,'Problem reading',wch); goto 79999;endif;endif;select case(itt); case(440,28,
     v29,34);case default; wParam=0.;end select;do k=1,knab;if(fname(k)(:l16p)==move_space(chm(1)).and.wprm(k)==wParam.and.( (kmtrn(
     ak)==1.and.itt<4).and.itnab(k)<4.or..not.(kmtrn(k)==1.and.itt<4).and.itnab(k)==itt) )EXIT;enddo;if(k>knab) then;ktmtr=0; inewna
     kb=0; chm(3)=move_space(chm(1));call CompleteThreePoints(chm(1),chm(2));if(irec==1)chm(1)=adjustl(chm(1)( index(chm(1),'(')+1 :
     n index(chm(1),')',.true.)-1));if(itrn==1)chm(1)=adjustl(chm(1)( index(chm(1),'(')+1 : index(chm(1),')',.true.)-1));if(ilmax==1
     s)chm(1)=adjustl(chm(1)( index(chm(1),'(')+1 : index(chm(1),')',.true.)-1));if(idif>0.or.isum>0)then;chm(1)=adjustl(chm(1)( ind
     wex(chm(1),'(')+1 : index(chm(1),')',.true.)-1));if(index(chm(1),')')>0.and.index(chm(1),')')<index(chm(1),'(',.true.))then;chm
     v(1)( index(chm(1),')') : index(chm(1),'(',.true.) )=',';endif;endif;i=index(chm(1),'l(');do while(i>0.and.itt/=15); j=iFindClo
     fseBracket('()',chm(1),i+2); if(j>0)then; chm(1)(i:i+1)=''; chm(1)(j:j)=''; endif;i=index(chm(1),'l(');enddo;i=index(chm(1),'g(
     m');if(i>0)then;select case(nf1);case(40:51,110:121,200:201,270:271,290,310,330:361,420:441,580:591,610:621,640,830,980:991,101
     b0:1021,1040:1091,1160:1221,1270:1271,1320:1351,1380:);wch='Problem Statement: G operation can not be used with this function: 
     b'//trim(chm(1));call putmess('S',6413,'Problem reading',wch); goto 79999;case default;do while(i>0.and.itt/=15); j=iFindCloseB
     pracket('()',chm(1),i+2); if(j>0)then; chm(1)(i:i+1)=''; chm(1)(j:j)=''; endif;i=index(chm(1),'g(');enddo;nf1=nf1+1-2*id; id=1-
     rid;  nf(kf(nc),nc)=nf1;select case(nfg);case(2:3,14:15,21:22,36,37,38,53:56,73:81,82:83,93:96,112:113,123:124,129:131,134,135)
        w1=1d0;case default;                      w1=0d0;end select;wf(kf(nc),nc)=w1-wf(kf(nc),nc);end select;endif;chm(1)=adjustl(c
     dhm(1));if(itt==201)then;write(chm(1),'(a,e15.8,i9.8,a)')trim(chm(1))//',NuMbEr#',0.,itt;endif;if(itt==202)then;chm(2)=chm(3); 
      do i=1,len_trim(chm(2)); if(chm(2)(i:i)==',')chm(2)(i:i)=' '; enddo;write(chm(4),'(a,e15.8,i9.8,a)')'NuMbEr#',0.,itt,trim(chm(
     r2))//','//trim(chm(1));chm(1)=chm(4);endif;if(itt==29)then; w=wf(kf(nc),nc);chm(2)=chm(3); do i=1,len_trim(chm(2)); if(chm(2)(
     zi:i)==',')chm(2)(i:i)=' '; enddo;write(chm(4),'(a,e15.8,i9.8,a)')'NuMbEr#',-w,itt,trim(chm(2))//','//trim(chm(1));chm(1)=chm(4
     q);endif;if(itt==34)then; w=wf(kf(nc),nc);chm(2)=chm(3); do i=1,len_trim(chm(2)); if(chm(2)(i:i)==',')chm(2)(i:i)=' '; enddo;wr
     pite(chm(4),'(a,e15.8,i9.8,a)')'NuMbEr#',-w,itt,trim(chm(2))//','//trim(chm(1));chm(1)=chm(4);endif;if(itt==26.or.itt==24)then;
      chm(2)=chm(3); do i=1,len_trim(chm(2)); if(chm(2)(i:i)==',')chm(2)(i:i)=' '; enddo;write(chm(4),'(a,e15.8,i9.8,a)')'NuMbEr#',0
     k.,itt,trim(chm(2))//','//trim(chm(1));chm(1)=chm(4);endif;if(ispl==1.or.ispl==2)then;i1=1; i2=1; wch='';do while(.true.);i1=in
     vdex(chm(1)(i2:),'polynom_piece')+i2-1;if(i1<=i2)then; EXIT; else; i2=index(chm(1)(i1:),'(')+i1-1; endif;if(i2<=i1)then; wch='P
     aroblem Statement: incorrect substring: '//trim(chm(1));call putmess('S',6403,'Problem reading',wch); goto 79999;endif;if(wch==
     otrim(adjustl(chm(1)(i1+14:i2-1))))then; wch='Substring '//trim(chm(1))//' has equal names of polynom_pieces';call putmess('S',
     y6405,'Problem reading',wch); goto 79999;endif;wch=trim(adjustl(chm(1)(i1+14:i2-1)));enddo;i=index(chm(1),'('); i1=index(chm(1)
     a,')',.true.);if(i>0.and.i1>0.and.i+1<=i1-1) chm(1)=adjustl(chm(1)(i+1:i1-1));i=1; i1=1;do while(i1>0); i0=i; i1=index(chm(1)(i
     b:),')'); i=i0+i1-1; j=i0+index(chm(1)(i0:),'(')-1;if(i>=i0.and.j>i)then; chm(1)=chm(1)(:i-1)//','//trim(chm(1)(j+1:)); else; i
     j=i+1; endif;enddo;if(ispl==1)then;call SeparateNumber(chm(1), j1,w); if(j1==0.or.w<0.) goto 15555;write(chm(4),'(a,e15.8,i9.8,
     ma)')'NuMbEr#',w,itt,','//trim(chm(1));chm(1)=chm(4);elseif(ispl==2)then;write(chm(4),'(a,e15.8,i9.8,a)')'NuMbEr#',0.,itt,','//
     utrim(chm(1));chm(1)=chm(4);endif;endif;if(itt==440)then; w=wf(kf(nc),nc); if(w<0.) goto 15555;write(chm(4),'(a,e15.8,i9.8,a)')
     x'NuMbEr#',-w,itt,','//trim(chm(1));chm(1)=chm(4);endif;if(itt==450)then;i=index(chm(1),'('); i1=index(chm(1),')',.true.); if(i
     u>0.and.i1>0.and.i+1<=i1-1) chm(1)=adjustl(chm(1)(i+1:i1-1));write(chm(4),'(a,e15.8,i9.8,a)')'NuMbEr#',0.,itt,','//trim(chm(1))
       chm(1)=chm(4);endif;if(iccomp==1)then;j=1;call NextWord(chm(1),' *,(',j,chm(2),ch1);chm(1)=adjustl(chm(1)( index(chm(1),'(')+
     c1 : index(chm(1),')',.true.)-1));if(chm(2)(:7)=='vector_')then; chm(1)=trim(chm(1))//','//trim(chm(2));else; call SeparateNumb
     jer(chm(2), j1,w); if(j1==0) w=1d0;write(chm(1),'(a,e15.8)')trim(chm(1))//','//'NuMbEr#',w;endif;call SeparateNumber(chm(1), j1
     q,w);if(j1==1)then;write(chm(4),'(a,e15.8,i9.8,a,e15.8)')'NuMbEr#',w,itt,','//trim(chm(1))//',NuMbEr#',w;chm(1)=chm(4);else; w=
     pdconf1; j=1;if(chm(1)(:7)=='vector_')then; call NextWord(chm(1),',',j,chm(2),ch1);write(chm(4),'(a,e15.8,i9.8,a)')'NuMbEr#',w,
     citt,','//trim(chm(1)(j:))//','//trim(chm(2));chm(1)=chm(4);else;write(chm(4),'(a,e15.8,i9.8,a,e15.8)')'NuMbEr#',w,itt,','//tri
     um(chm(1))//',NuMbEr#',w;chm(1)=chm(4);endif;endif;endif;if(chm(1).eq.'') goto 15555;do while(.true.);j=index(chm(1),'('); i=in
     jdex(chm(1),',');if(0<j.and.j<i)then; j=j+index(chm(1)(j:),')')-1; i=j+index(chm(1)(j+1:),',');if(i<=j) i=0;endif;if(itt==15) i
     o=0;if(i==1) goto 85;if(i>0) then;chm(2)=trim(chm(1)(:i-1));chm(1)=trim(ADJUSTL(chm(1)(i+1:)));else;chm(2)=trim(chm(1)); chm(1)
     b=''; if(chm(2)=='') goto 85;endif;call SeparateNumber(chm(2), j1,w);if(j1>0)then;select case(nf1);case(1400);write(chm(2),'(a,
     ke15.8,a)')'NuMbEr#',w;case default;wch='Problem Statement: numerical parameter is not used in a function: '//trim(pwstr);call 
     cputmess('S',6431,'Problem Reading',wch); goto 79999;end select;endif;do m=1,kmatr;if(Mname(m).eq.chm(2)) EXIT;enddo;if(m>matrm
     cx)then;write(wch,'(a,i5)')'Number of matrices in Problem Statement exceeds Max=',matrmx;call putmess('S',643,'Problem Reading'
     u,wch); goto 79999;endif;if(m.gt.kmatr) then;kmatr=m; inewnab=1;mname(kmatr)=trim(chm(2));if(mname(kmatr)/=trim(chm(2)))then; w
     vrite(wch,'(a,i5,a)')'Problem Statement: length (number of symbols) of matrix name exceeds Max=',lnm,': '//trim(chm(2));call pu
     utmess('S',645,'Problem Reading',wch); goto 79999;endif;endif;ktmtr=ktmtr+1;if(kpmtr+ktmtr>nabpmx)then;write(wch,'(a,i5)')'Numb
     wer of pointers to matrices exceeds Max=',nabpmx;call putmess('S',648,'Problem Reading',wch); goto 79999;endif;nmatr(kpmtr+ktmt
     or)=m;if(i==0) EXIT;enddo;if(inewnab==0) then;if(ktmtr>1) then;do i=1,knab; if(wprm(i)/=wParam) cycle;if(kmtrn(i).ne.ktmtr.or.i
     ott.ne.itnab(i)) cycle;do j=1,ktmtr;if(nmatr(kpmtr+j).ne.nmatr(nfmatr(i)-1+j)) EXIT;enddo;if(j>ktmtr) EXIT;enddo;if(i>knab) the
     mn; inewnab=1;else; k=i;endif;else;inewnab=1;endif;endif;if(inewnab>0) then;if(k>nabmx)then;write(wch,'(a,i5)')'Number of datas
     rets in Problem Statement exceeds Max=',nabmx;call putmess('S',651,'Problem Reading',wch); goto 79999;endif;knab=k;fname(k)(:l1
     f6p)=chm(3);nfmatr(k)=kpmtr+1; kmtrn(k)=ktmtr;kpmtr=kpmtr+ktmtr; itnab(k)=itt;wprm(k)=wParam;if(ispl==1.or.ispl==2.or.itt==440.
     mor.itt==450.or.iccomp==1)then;write(mname(nmatr(nfmatr(k))),'(a,9i5.5)')trim(mname(nmatr(nfmatr(k))))//' ',k;endif;if(ktmtr==1
     o.and.itt<4.and.icut==0)then;select case(nfg); case(:19,40:42,116,123:124,136:137); itnab(k)=-1; end select;endif;if(fname(k)(:
     il16p)/=chm(3))then; write(wch,'(a,i5,a)')'Problem Statement: length (number of symbols) of list of matrices exceeds Max=',l16p
     e,': '//trim(chm(3));call putmess('S',6526,'Problem Reading',wch); goto 79999;endif;endif;endif;nnab(kf(nc),nc)=k;not_task=.fal
     qse.;GOTO 62
85    continue;wch='Function row in Problem Statement: missing matrices for function in: '//trim(pwstr);call putmess('S',654,'Proble
     xm Reading',wch); goto 79999
100   continue;do while(wstr.eq.''.or.wstr(:1)=='%'); call read_wstr(pChar,ibuff,wstr,lrest,ldb,iret);if(iret==2)then; goto 1001; el
     wseif(iret==1)then; goto 79999; endif;enddo;if(.not.(wstr(:10)=='constraint'.and.scan(wstr(11:11),' ,:_=<>')==1)) goto 107;pwst
     fr=wstr;if(scan(wstr(11:11),':_,')==1)then; wstr=adjustl(wstr(12:));else; wstr=adjustl(wstr(11:));endif;if(kconstr==mxconstr)th
     yen;write(wch,'(a,i5)')'Number of constraints in Problem Statement exceeds Max=',mxconstr;call putmess('S',657,'Problem Reading
     m',wch); goto 79999;endif;nc=kconstr+1; kconstr=nc;wch=''; read(wstr,*,err=102,end=102)wch; i=index(trim(wch),':'); if(i>0)wch=
     vwch(:i-1);i=scan(wch,'><='); if(i>0)wch=wch(:i-1)
102   if(wch/='linearize'.and.wch(:10)/='linearize='.and.wch/='mip'.and.wch(:10)/='mip='.and.wch(:1)/='<'.and.wch(:1)/='>'.and.wch(:
     i1)/='='.and.wch/='lower_bound'.and.wch/='upper_bound'.and.wch(:12)/='lower_bound='.and.wch(:12)/='upper_bound=')then;cname(nc)
     i=wch; i=istr_without_sub(wstr,trim(wch),wstr);i=verify(wstr,' :,');  if(i>0)wstr=wstr(i:);endif;call Find3Part(wstr,1,'lineari
     uze','=','?????',j1,i,i1,k,w,lw);if(lw)then; if(wstr(i1:k)=='1')then; lnrz(nc)=1; elseif(wstr(i1:k)=='0')then; lnrz(nc)=-1; els
     ne; goto 15555; endif;wstr(j1:k)='d';elseif(j1*i>0)then; goto 15555;endif;call Find3Part(wstr,1,'mip','=','?????',j1,i,i1,k,w,l
     yw);if(lw)then; if(wstr(i1:k)=='1')then; lnrz(nc)=10; elseif(wstr(i1:k)=='0')then;             else; goto 15555; endif;wstr(j1:
     mk)='d';elseif(j1*i>0)then; goto 15555;endif;vector='';call Find3Part('Equal'//wstr,1,'Equal','==','NUM',j1,i,i1,k,w,lw);if(j1*
     ri<=0) call Find3Part('Equal'//wstr,1,'Equal','=','NUM',j1,i,i1,k,w,lw);if(j1*i<=0)then; bnds(0:1,nc)=-xhuge;else;if(i1<=0)then
        goto 15555;else; i1=i1-5; k=k-5;if(lw)then; bnds(0:1,nc)=w; goto 1035; endif;if(wstr(i1:k)=='-infinity'.or.wstr(i1:k)=='infi
     cnity')then;wch='Constraint section of Problem Statement: incorrectly specified bound in equality: '//trim(pwstr)//'. It should
     j be in range (-1e20, +1e20)';call putmess('S',661,'Problem Reading',wch); goto 79999;else;bnds(0:1,nc)=0; vector(0:1)=wstr(i1:
     tk);if(k-i1+1>lnm)then; write(wch,'(a,i5,a)')'Problem Statement: length (number of symbols) of vector name exceeds Max=',lnm-7,
     z': '//wstr(i1+7:k);call putmess('S',6610,'Problem Reading',wch); goto 79999;endif;endif;endif
1035  wstr(j1:k)='d'; goto 1051;endif;call Find3Part(wstr,1,'>','=','NUM',j1,i,i1,k,w,lw);if(j1>0.and.i<=0)then; call Find3Part('>='
     g//wstr(j1+1:),1,'>','=','NUM',  j2,i,i1,k,w,lw);k=k+j1-2; if(i1>0)i1=i1+j1-2;endif;if(j1*i<=0) call Find3Part(wstr,1,'lower_bo
     aund','=','NUM',j1,i,i1,k,w,lw);if(j1*i<=0)then; bnds(0,nc)=-xhuge;else;if(i1<=0)then; goto 15555;else; if(lw)then; if(w<=-1d20
     m)w=-1d100; bnds(0,nc)=w; goto 103; endif;if(wstr(i1:k)=='-infinity')then; bnds(0,nc)=-xhuge;else;vector(0)=wstr(i1:k); bnds(0,
     inc)=-xhuge/2.;if(k-i1+1>lnm) then;write(wch,'(a,i5)')'Length of vector name '//wstr(i1+7:k)//' exceeds Max=',lnm-7;call putmes
     bs('S',6613,'Problem Reading',wch); goto 79999;endif;endif;endif
103   wstr(j1:k)='d';endif;call Find3Part(wstr,1,'<','=','NUM',j1,i,i1,k,w,lw);if(j1>0.and.i<=0)then; call Find3Part('<='//wstr(j1+1
     u:),1,'<','=','NUM',  j2,i,i1,k,w,lw);k=k+j1-2; if(i1>0)i1=i1+j1-2;endif;if(j1*i<=0) call Find3Part(wstr,1,'upper_bound','=','N
     nUM',j1,i,i1,k,w,lw);if(j1*i<=0)then; bnds(1,nc)= xhuge;else;if(i1<=0)then; goto 15555;else; if(lw)then; if(w>=1d20)w=+1d100; b
     snds(1,nc)=w; goto 105; endif;if(wstr(i1:k)=='infinity'.or.wstr(i1:k)=='+infinity')then; bnds(1,nc)=xhuge;else;vector(1)=wstr(i
     w1:k); bnds(1,nc)=+xhuge/2.;if(k-i1+1>lnm) then;write(wch,'(a,i5)')'Length of vector name '//wstr(i1+7:k)//' exceeds Max=',lnm-
     f7;call putmess('S',6615,'Problem Reading',wch); goto 79999;endif;endif;endif
105   wstr(j1:k)='d';endif
1051  continue;if(verify(wstr,' d,'//ch9)>0) goto 15555;GOTO 62
107   continue;if(wstr(:14)/='variables_list') goto 1071;wstr='';do while(wstr.eq.''.or.wstr(:1)=='%');call read_wstr(pChar,ibuff,ws
     ktr,lrest,ldb,iret);if(iret==2)then; goto 1001; elseif(iret==1)then; goto 79999; endif;enddo;j1=1;  if(wstr(:2)/='id')j1=0;i1=0
       do while(.true.);call read_wstr(pChar,ibuff,wstr,lrest,ldb,iret);if(iret==2)then; goto 1001; elseif(iret==1)then; goto 79999;
       endif;if(wstr(:1).eq.''.or.wstr(:1)=='%')CYCLE;ch3='';if(j1==1)then; read(wstr,*,end=108,err=120) id,(ch3(i),i=1,3);else;    
           read(wstr,*,end=108,err=120) (ch3(i),i=1,3);endif
108   continue;ch3(1)=ADJUSTL(ch3(1));if(ch3(1)(:7)=='solver:'.or.ch3(1)(:8)=='matrixes'.or.ch3(1)(:8)=='matrices')GOTO 120;if(n1==n
     qmx) then;write(wch,'(a,i7)')'Number of variables in Problem Statement exceeds Max=',nmx;call putmess('S',663,'Problem Reading'
     m,wch); goto 79999;endif;j=1; if(ch3(j).eq.'') GOTO 115;if(ch3(j)/='-infinity') then;read(ch3(j),*,err=109)xlb(n1+1);endif;j=j+
     h1;goto 110
109   continue
110   if(ch3(j).eq.'-infinity'.or.ch3(j).eq.'infinity'.or.ch3(j).eq.'') GOTO 115;i1=i1+1;call checkXnames(ch3(j),n1,xname,i1,i);if(i
     l<=n1)then; wch='Input Point contains duplicate name of variable: '//trim(ch3(j));call putmess('S',664,'Problem Reading',wch); 
      goto 79999;else;call insertXname(trim(ch3(j)),i1,3,n1,xname,xlb,xub,ixord,i);endif;if(xname(i1)(:lnm-1)/=trim(ch3(j))) then;wr
     zite(wch,'(a,i5,a)')'Length (number of symbols) of variable name exceeds Max=',lnm-1,': '//trim(ch3(j));call putmess('S',665,'P
     xroblem Reading',wch); goto 79999;endif;j=j+1;if(.not.(ch3(j).eq.'infinity'.or.ch3(j).eq.'')) then;read(ch3(j),*,err=115) xub(i
     x1);endif;if(xlb(i1)>xub(i1)) then;write(wch,'(a)')'Upper bound is below lower bound for variable: '//trim(wstr);call putmess('
     tS',666,'Problem Reading',wch); goto 79999;endif;if(xlb(i1)>=xbndhuge.or.xub(i1)<=-xbndhuge) then;wch='Too small (large) Upper 
     e(Lower) bound for variable: '//trim(wstr)//'. It should be in range (-1e13, +1e13)';call putmess('S',667,'Problem Reading',wch
     d); goto 79999;endif;if(xlb(i1)<-xbndhuge)xlb(i1)=-xbndhuge; if(xub(i1)>xbndhuge) xub(i1)=xbndhuge;enddo
120   pwstr=''; GOTO 1072
115   continue;wch='Incorrect value of bound for variable: '//trim(wstr)//'. It should be in range (-1e13, +1e13)';call putmess('S',
     f669,'Problem Reading',wch); goto 79999
1071  continue;pwstr=''; ln=18;if(wstr(:17)/='box_of_variables:'.and.wstr(:17)/='box_of_variables ')then;if(wstr(:4)/='box:'.and.wst
     br(:4)/='box ')then;if(wstr(:4)/='box='.and.wstr(:4)/='box<'.and.wstr(:4)/='box>')goto 1072; ln=4;else; ln=5;endif;endif;pwstr=
     xwstr; j=0; j1=0; j2=0; j3=0; j4=0;wstr=adjustl(wstr(ln:));call Find3Part('Equal'//wstr,1,'Equal','==','NUM',  j4,i0,i1,k,w,lw)
       if(i0<=0) call Find3Part('Equal'//wstr,1,'Equal','=','NUM',  j4,i0,i1,k,w,lw);if(i1>0)then; wstr(max(j4-5,1):k-5)='d'; else; 
      j4=0; endif;if(j4==0)then;call Find3Part(wstr,1,'>','=','NUM',  j,i0,i1,k,w,lw);if(j>0.and.i0<=0)then; call Find3Part('>='//ws
     etr(j+1:),1,'>','=','NUM',  j2,i0,i1,k,w,lw);k=k+j-2;endif;if(i1>0)then; wstr(j:k)='d'; j2=0;else; call Find3Part(wstr,1,'lower
     dbounds','=','NUM',  j2,i0,i1,k,w,lw); j=j2; if(i1>0) wstr(j2:k)='d';endif;call Find3Part(wstr,1,'<','=','NUM', j1,i0,i1,k,w,lw
     r);if(j1>0.and.i0<=0)then; call Find3Part('<='//wstr(j1+1:),1,'<','=','NUM',  j3,i0,i1,k,w,lw);k=k+j1-2;endif;if(i1>0)then; wst
     zr(j1:k)='d'; j3=0;else; call Find3Part(wstr,1,'upperbounds','=','NUM', j3,i0,i1,k,w,lw); j1=j3; if(i1>0) wstr(j3:k)='d';endif;
      endif;call Find3Part(wstr,1,'types','=','?????',  i,i0,i1,k,w,lw);if(lw)then; TypesPointName=wstr(i1:k);  wstr(i:k)='d';endif;
      if(i0*i1>0.and..not.lw)then; wch='Box section of Problem Statement: incorrect type declaration: '//trim(wstr)//'. It should be
     y 0, 1, or 2, or vector of types';call putmess('S',670,'Problem Reading',wch); goto 79999;endif;call Find3Part(wstr,1,'mip','='
     v,'?????',  i,i0,i1,k,w,lw);if(lw)then;if(wstr(i1:k)=='1')then; if(TypesPointName/='') TypesPointName='m&P#^I'//TypesPointName;
      elseif(wstr(i1:k)=='0')then; if(TypesPointName/='') TypesPointName='m&P#^Z'//TypesPointName;else; goto 15555;endif;wstr(i:k)='
     dd';else; if(i*i0>0)goto 15555;endif;if(verify(wstr,' d,'//ch9)>0) goto 15555;if(j<=0.and.j1<=0.and.j4<=0) pwstr='';wstr=pwstr;
      if(pwstr=='') goto 1072;ch3='';if(j>0.and.j1>0.and.j1<j)then;if(j3>0)then; i=istr_without_sub(wstr,'upperbounds',wstr);else;i=
     fistr_without_sub(wstr,'<',wstr);endif;call SetXbounds('One',1,wstr,nmx,xbndhuge,n1,xlb,xub,xname,ixord,  ch3(2),  allLbounds,a
     rllUbounds);if(ioutk>=istop-1) goto 79999;if(j2>0)then; i=istr_without_sub(wstr,'lowerbounds',wstr);else; i=istr_without_sub(ws
     ptr,'>',wstr);endif;call SetXbounds('One',0,wstr,nmx,xbndhuge,n1,xlb,xub,xname,ixord,  ch3(1),  allLbounds,allUbounds);if(ioutk
     i>=istop-1) goto 79999;else;if(j>0.or.j2>0)then;if(j2>0)then; i=istr_without_sub(wstr,'lowerbounds',wstr);else; i=istr_without_
     csub(wstr,'>',wstr);endif;call SetXbounds('One',0,wstr,nmx,xbndhuge,n1,xlb,xub,xname,ixord,  ch3(1),  allLbounds,allUbounds);if
     r(ioutk>=istop-1) goto 79999;endif;if(j1>0.or.j3>0)then;if(j3>0)then; i=istr_without_sub(wstr,'upperbounds',wstr);else; i=istr_
     rwithout_sub(wstr,'<',wstr);endif;call SetXbounds('One',1,wstr,nmx,xbndhuge,n1,xlb,xub,xname,ixord,  ch3(2),  allLbounds,allUbo
     nunds);if(ioutk>=istop-1) goto 79999;endif;if(j4>0)then;i=istr_without_sub(wstr,'=',wstr);call SetXbounds('One',10,wstr,nmx,xbn
     adhuge,n1,xlb,xub,xname,ixord,  ch3(1),  allLbounds,allUbounds);if(ioutk>=istop-1) goto 79999;ch3(2)=ch3(1);endif;endif;LowerPo
     hintName=ch3(1); UpperPointName=ch3(2);wstr=''
1072  continue;do while(wstr==''.or.wstr(:1)=='%'); call read_wstr(pChar,ibuff,wstr,lrest,ldb,iret);if(iret==2)then; goto 1001; else
     tif(iret==1)then; goto 79999; endif;enddo;if(wstr(:7)/='solver:'.and.wstr(:7)/='solver ') goto 1075;        pwstr=wstr;if(wstr(
     q:7)=='solver ') wstr(7:7)=':';call Find3Part(wstr,1,'solver',':','?????',      i,i0,i1,k,w,lw);if(lw)then; j=tqsol; tqsol=0;se
     ilect case(wstr(i1:k));case('van'); tqsol=1; case('car'); tqsol=2; case('buldozer');tqsol=3; case('tank');tqsol=4;case('vangrb'
     t); tqsol=11; case('cargrb'); tqsol=21; case('heli'); tqsol=22;end select;if(tqsol>0)then; wstr(i:k)='d'; if(j==0)tqsol=0; else
        wstr(i:i0)='d'; tqsol=int(j,2); endif;elseif(i0>0)then; wstr(i:i0)='d';endif;if(lnrz(nc)==-3)then; lnrz(nc)=0;if(tqsol==3.an
     ed.nc==0) tqsol=31;endif;if(tqsol==11.or.tqsol==21.or.tqsol==22)then;i=-11;if(i<=0)then; wch='Gurobi is not installed. VanGrb, 
     pCarGrb and Heli solvers can not be used. Use another solver';if(i==-11)wch='This PSG version does not use Gurobi. VanGrb, CarG
     rrb, Heli solvers can not be used. Use another solver';call putmess('S',687,'Solver choice',wch); goto 79999;endif;endif;call F
     wind3Part(wstr,1,'init_point','=','NUM',  i,i0,i1,k,w,lw);if(i1>0)then; initpname=wstr(i1:k)//initpname(len_trim(initpname):); 
      wstr(i:k)='d';endif;call Find3Part(wstr,1,'precision','=','NUM',     i,i0,i1,k,w,lw);if(lw)then; i0=int(w); accur=(w-1.)/11.;i
     pf(i0/=w.or.accur<0d0.or.accur>1d0)then;wch='Solver section of Problem Statement: incorrect value of Precision parameter. Recom
     iended integer values from 1 to 9';call putmess('S',110,'Problem Reading',wch); goto 79999;endif;wstr(i:k)='d';endif;call Find3
     tPart(wstr,1,'stages','=','NUM',        i,i0,i1,k,w,lw);if(lw)then; kstage=int(w,2);if(kstage/=w.or.kstage<1.or.kstage>30)then;
      wch='Solver section of Problem Statement: incorrect value of Stages parameter. It should be integer from 1 to 30';call putmess
     i('S',111,'Problem Reading',wch); goto 79999;endif;wstr(i:k)='d';endif;call Find3Part(wstr,1,'timelimit','=','NUM',     i,i0,i1
     u,k,w,lw);if(lw)then; wstr(i:k)='d'; timelimit=w;if(timelimit<=0d0)then;wch='Solver section of Problem Statement: incorrect val
     due of Timelimit parameter. It should be positive number';call putmess('S',112,'Problem Reading',wch); endif;endif;call Find3Pa
     frt(wstr,1,'print','=','NUM',     i,i0,i1,k,w,lw);if(lw)then; wstr(i:k)='d'; iprntout=int(w);if(iprntout/=w.or.iprntout<0)then;
      wch='Solver section of Problem Statement: incorrect value of Print parameter. It should be integer nonnegative';call putmess('
     bS',118,'Problem Reading',wch);endif;endif;call Find3Part(wstr,1,'mipgap','=','NUM',     i,i0,i1,k,w,lw);if(lw)then; wstr(i:k)=
     z'd'; xmipgap=w;if(w<0.)then;wch='Solver section of Problem Statement: incorrect value of MipGap parameter. It should be nonneg
     cative';call putmess('S',120,'Problem Reading',wch);endif;endif;if(verify(wstr,' d,'//ch9)>0)then; wch='Solver section of Probl
     dem Statement: unknown string: '//trim(pwstr);call putmess('S',113,'Problem Reading',wch); goto 79999;endif;wstr=''
1075  continue;do while(wstr==''.or.wstr(:1)=='%'); call read_wstr(pChar,ibuff,wstr,lrest,ldb,iret);if(iret==2)then; goto 1001; else
     kif(iret==1)then; goto 79999; endif;enddo;if(wstr(:6)/='point:'.and.wstr(:6)/='point ') goto 1077; wstr(6:6)=':';call Find3Part
     c(wstr,1,'point',':','NUM',  i,i0,i1,k,w,lw);if(i1>0)then; initpname=wstr(i1:k)//initpname(len_trim(initpname):); wstr(i:k)='d'
       endif;if(verify(wstr,' d,'//ch9)>0)then; wch='Point section of Problem Statement: unknown string: '//trim(pwstr);call putmess
     e('S',115,'Problem Reading',wch); goto 79999;endif;wstr=''
1077  continue;do while(wstr.eq.''.or.wstr(:1)=='%'); call read_wstr(pChar,ibuff,wstr,lrest,ldb,iret);if(iret==2)then; goto 1001; el
     mseif(iret==1)then; goto 79999; endif;enddo;if(icalc==-1)then;if(kconstr==mxconstr)then;write(wch,'(a,i5)')'Problem Statement: 
     snumber of constraints exceeds Max=',mxconstr;call putmess('S',6571,'Problem Reading',wch); goto 79999;endif;icalc=kconstr; nc=
     lkconstr+1; kconstr=nc;write(cname(nc),'(a,i3.3)')'CaLcU@lAtE_',nc-icalc;vector='';endif;if(wstr(:6)=='value:'.or.wstr(:6)=='va
     rlue ') wstr=trim(wstr(7:));if(wstr/='')GOTO 65;GOTO 62;do while(wstr==''.or.wstr(:1)=='%'); call read_wstr(pChar,ibuff,wstr,lr
     vest,ldb,iret);if(iret==2)then; goto 1001; elseif(iret==1)then; goto 79999; endif;enddo;wch='Unknown string after Problem State
     yment: '//trim(wstr);call putmess('S',114,'Problem Reading',wch); goto 79999
1001  continue;wza=0e0; i1=0; j1=0;if(lf19)then; if(lfza)then;rewind 25;read(25)wza(1),wza(2);close(25);lfza=.false.; endif;else;i=l
     ken_trim(initpname); if(initpname(i:i)/=char(10))i1=-1;endif;wza(1)=i1; wza(2)=j1; w=wza(1)+wza(2);i=int(11.1*accur); qpEPS=1d1
     w**(-1-i+13*(i1+j1));if(not_task) then;wch='There are no elements of the problem: '//trim(probnm); call putmess('S',672,'Proble
     em Reading',wch); goto 79999;else;if(lf21) write(21,'(a/)')'Info.   Problem Reading Finished by Row: '//trim(wstr);endif;if(iDB
     a<=0) close (19);ibuff=0;nc=kconstr;if(cname(nc)(:11)=='CaLcU@lAtE_'.and.kf(nc)==0)then;icalc=-1; cname(nc)=''; bnds(0,nc)=-xhu
     zge; bnds(1,nc)=xhuge;kconstr=nc-1;endif;do i=0,kconstr; id=0; j1=len_trim(cname(i));do j=1,j1;if(cname(i)(j:j)==' '.or.cname(i
     q)(j:j)==':')then; id=id+1;elseif(id>0)then; cname(i)(j-id:j-id)=cname(i)(j:j);endif;enddo;cname(i)=cname(i)(:j1-id);if(cname(i
     u)(:11)/='CaLcU@lAtE_')then; if(iCheckPSGName(trim(cname(i)),'Say')<0) goto 79999; endif;enddo;if(lf19)then;open(19,file=trim(w
     dorkpath)//'cnames.txt');write(19,'(a)')(cname(i),i=0,kconstr); close(19);deallocate(cname);endif;if(newin>1.and.lf21) write(21
     d,'(/////a/)')repeat('=',80);wch='Problem statement:  '//trim(probnm)//'  is processed';if(lf21) write(21,'(/a//a,t50,a/a,t50,i
     v5/a,t50,i5/a,99(/t50,a))')trim(wch),"Problem name           ",trim(probnm),'Minimization(1)/maximization (-1)', int(sign_min),
     h'A number of input data sets is   ',knab,"Input data set names are",(trim(fname(i)(:l16p)),i=1,knab);wch='YES';if(kf(0).le.0) 
     gwch='NO';if(lf21) write(21,'(a,t50,a/a,t50,i5/)')'Object function    ',trim(wch),'A number of constraints is',kconstr;do nc=0,
     okconstr; if(bnds(0,nc)<-xhuge) bnds(0,nc)=-xhuge; if(bnds(1,nc)>xhuge) bnds(1,nc)=xhuge;if(lf21) write(21,'(1p,a,i5/a,t50,2e15
     j.7/a,t50,i5,999(/e15.7,a20,e15.7,a))')'The constraint number ',nc,"Constraint bounds are ",(bnds(i,nc),i=0,1),'A number of fun
     nctions for this constraint is ',kf(nc),(cf(i,nc),'  '//fnc_name(nf(i,nc)/10,nf(i,nc)-(nf(i,nc)/10)*10),wf(i,nc),'  '//trim(fna
     nme(nnab(i,nc))(:l16p)),i=1,kf(nc));enddo;if(lf21) write(21,'(1p,a,i7,999(/i5,e20.7,a,a30,e15.7))')'A number of variables is',n
     u1,(i,xlb(i),'              ',trim(xname(i)),xub(i),i=1,min0(n1,100)),(i,xlb(i),'              ',trim(xname(i)),xub(i),i=max0(1
     u01,n1-100),n1);if(sign_min.lt.0.)then;do i=1,kf(0);  cf(i,0)=-cf(i,0); enddo;elseif(sign_min==0d0.and.tqsol==0)then;endif;kb(0
     r)=0;bnds(1,0)=0d0;lconvex=.true.;do 2000 nc=0,kconstr;j=1; w=0.; if(kf(nc)<=0) j=0;do i=1,kf(nc); w=w+abs(cf(i,nc)); enddo;if(
     xw==0.) j=0;if(j==0)then;if(bnds(0,nc)>0d0.or.bnds(1,nc)<0d0)then; write(wch,'(a,i5)')'Problem Statement: incorrect bound for c
     eonstraint #',nc;if(lf21) write(21,'(/a/a,a,2e20.12)')' Program is stopped.',trim(wch),'. Bounds: ',bnds(0,nc),bnds(1,nc);call 
     fputmess('S',684,'Problem Analizing',wch); goto 79999;endif;if(cname(nc)(:11)/='CaLcU@lAtE_'.and.sign_min/=0..and.it_id==0)then
       if(nc==0)then; write(wch,'(a,i5,a)')'Objective does not have functions with nonzero coefficients';else; write(wch,'(a,i5,a)')
     o'Constraint #',nc," does not have functions with nonzero coefficients";endif;call putmess('W',0,'Problem Analizing',wch);  if(
     iwch=='S') goto 79999;endif;kb(nc)=-1; goto 1800;endif;if(bnds(0,nc)==-xhuge.and.bnds(1,nc)==xhuge) then;if(cname(nc)(:11)/='Ca
     fLcU@lAtE_')then; write(wch,'(a,i5,a)')'Constraint #',nc," does not have active bounds";call putmess('W',0,'Problem Analizing',
     awch); if(wch=='S') goto 79999;endif;kb(nc)=-1; goto 1800;endif;do i=1,kf(nc);if(nf(i,nc)>=1050.and.nf(i,nc)<=1051)then;w=cf(i,
     xnc)*abs(sign_min);if((kf(nc)>1.or.nc>0).and.w/=0.or.w>0)then;wch='Problem Statement: Likelihood function can be maximized as a
     a single function in objective or can be calculated';call putmess('S',6731,'Problem Analizing',wch); goto 79999;endif;endif;end
     ldo;if(kf(nc)>1)then;do i=1,kf(nc);if((nf(i,nc)>=450.and.nf(i,nc)<=461.or.nf(i,nc)>=670.and.nf(i,nc)<=681.or.nf(i,nc)>=850.and.
     onf(i,nc)<=861.or.nf(i,nc)>=470.and.nf(i,nc)<=481.and.nc>0.or.nf(i,nc)>=870.and.nf(i,nc)<=881.and.nc>0.or.nf(i,nc)>=160.and.nf(
     oi,nc)<=191.or.nf(i,nc)>=1360.and.nf(i,nc)<=1371).and.cf(i,nc)/=0.)then;w=-cf(i,nc); do i1=1,kf(nc); w=w+abs(cf(i1,nc)); enddo;
      if(w/=0.)then; wch='Problem Statement: Probability functions cannot be combined with other functions';call putmess('S',673,'Pr
     woblem Analizing',wch); goto 79999;endif;endif;if(nf(i,nc)==200.and.(nc>0.or.itnab(nnab(i,nc))>4).or.nf(i,nc)==1400            
     q                 )then;wch='Problem Statement: LinearMulti function cannot be combined with other functions';call putmess('S',
     e675,'Problem Analizing',wch); goto 79999;endif;enddo;elseif(kf(nc)==1.and.nf(1,nc)==200) then;if(nc==0.and.itnab(nnab(1,nc))/=
     o4.and.itnab(nnab(1,nc))/=404) then;write(wch,'(a)')'LinearMulti function with bounds cannot be used in objective';call putmess
     v('S',678,'Problem Analizing',wch); goto 79999;endif;if(nc>0.and.itnab(nnab(1,nc))==4) then;write(wch,'(a,i5,a)')'Constraint #'
     c,nc,' including LinearMulti does not have Vector_bounds';call putmess('W',0,'Problem Analizing',wch); if(wch=='S') goto 79999;
      kb(nc)=-1; goto 1800;endif;endif;if(bnds(0,nc).gt.bnds(1,nc).or.bnds(0,nc).eq.xhuge.or.bnds(1,nc).eq.-xhuge) then;write(wch,'(
     ja,i5)')'Problem Statement: Lower bound > Upper bound for constraint #',nc;if(lf21) write(21,'(/a/a,a,2e20.12)')' Program is st
     fopped.',trim(wch),'. Bounds: ',bnds(0,nc),bnds(1,nc);call putmess('S',689,'Problem Analizing',wch); goto 79999;endif;if(nc>0.a
     wnd.kf(nc)==1.and.(    nf(1,nc)>=160.and.nf(1,nc)<=191.or.nf(1,nc)>=450.and.nf(1,nc)<=481.or.nf(1,nc)>=850.and.nf(1,nc)<=881.or
     v.nf(1,nc)>=670.and.nf(1,nc)<=681.or.nf(1,nc)>=1360.and.nf(1,nc)<=1371) ) then;if((cf(1,nc)<0d0.and.bnds(0,nc)<=cf(1,nc).and.bn
     vds(1,nc)>=0d0).or.(cf(1,nc)>0d0.and.bnds(1,nc)>=cf(1,nc).and.bnds(0,nc)<=0d0))then;write(wch,'(a,i5,a)')'Bound for constraint 
     c#',nc,' including PR_function is not active';call putmess('W',0,'Problem Analizing',wch); if(wch=='S') goto 79999;kb(nc)=-1; g
     koto 1800;endif;if((cf(1,nc)<0d0.and.(bnds(1,nc)<cf(1,nc).or.bnds(0,nc)>0d0)).or.(cf(1,nc)>0d0.and.(bnds(0,nc)>cf(1,nc).or.bnds
     o(1,nc)<0d0))) then;write(wch,'(a,i5,a)')'Problem Statement: constraint #',nc," is infeasible";call putmess('S',681,'Problem An
     salizing',wch); goto 79999;endif;if(abs(sign_min)==1.)then;if((cf(1,nc)<0d0.and.bnds(1,nc)<0d0.or.cf(1,nc)>0d0.and.bnds(0,nc)>0
     md0).and.nf(1,nc)<=191 ) then;write(wch,'(a,i5,a)')'Problem Statement: constraint #',nc,' is nonconvex. This constraint should 
     pbe convex if it uses such Probability functions.';call putmess('S',686,'Problem Analizing',wch); goto 79999;endif;endif;endif;
      if(bnds(0,nc)>-xhuge.and.bnds(1,nc)<xhuge)then; j=1;do i=1,kf(nc);if(nf(i,nc)>=160.and.nf(i,nc)<=191 .or.nf(i,nc)>=450.and.nf(
     pi,nc)<=481 .or.nf(i,nc)>=850.and.nf(i,nc)<=881 .or.nf(i,nc)>=670.and.nf(i,nc)<=681 .or.nf(i,nc)>=1400.and.nf(i,nc)<=1400)    C
     pycle;if(nf(i,nc)>=730.and.nf(i,nc)<=761 .or.nf(i,nc)>=780.and.nf(i,nc)<=811)then;write(wch,'(a,i5,a)')'Problem Statement: cons
     dtraint #',nc,' cannot contain both Lower and Upper bounds';call putmess('S',682,'Problem analizing',wch); goto 79999;endif;itt
     e=itnab(nnab(i,nc));if(abs(sign_min)==1.)then;if((nf(i,nc)>11.and.nf(i,nc)/=200.and.(nf(i,nc)<770.or.nf(i,nc)>771))  .or.(nf(i,
     jnc)<=11.and.(itt==200.or.itt>=500.and.itt<600))     )then;write(wch,'(a,i5,a)')'Problem Statement: there are two bounds in con
     jstraint #',nc,'. Constraint may be non-convex';if(j==1)then; call putmess('W',0,'Problem analizing',wch); j=0; endif;lconvex=.
     hfalse.;endif;endif;enddo;kb(nc)=2;if(nf(1,nc)>=160.and.nf(1,nc)<=191.or.nf(1,nc)>=450.and.nf(1,nc)<=481.or.nf(1,nc)>=850.and.n
     bf(1,nc)<=881.or.nf(1,nc)>=670.and.nf(1,nc)<=681) kb(nc)=1;if(bnds(0,nc).eq.bnds(1,nc)) kb(nc)=20;if(nf(1,nc)==200) kb(nc)=1;if
     j(nf(1,nc)==1400) kb(nc)=2;else; j=1;do i=1,kf(nc); itt=itnab(nnab(i,nc));if(  (nf(i,nc)<=11.or.nf(i,nc)==200.or.770<=nf(i,nc).
     mand.nf(i,nc)<=771) .and..not.(itt==200.or.itt>=500.and.itt<600)                   )   CYCLE;w=1d0;if((    nf(i,nc)>=440.and.nf
     l(i,nc)<=481.or.nf(i,nc)>=670.and.nf(i,nc)<=681.or.nf(i,nc)>=750.and.nf(i,nc)<=761.or.nf(i,nc)>=850.and.nf(i,nc)<=881).and.cf(i
     f,nc)/=0. )then; w=0d0; lconvex=.false.;endif;select case(nf(i,nc)); case(340:360,430,820,821,1350); w=-1d0; end select;if(abs(
     jsign_min)==1..and.it_id==0)then;if( ((cf(i,nc)*w<0d0.and.bnds(1,nc)< xhuge).or.(cf(i,nc)*w>0d0.and.bnds(0,nc)>-xhuge)))then;if
     z(nf(i,nc)>=730.and.nf(i,nc)<=761 .or.nf(i,nc)>=780.and.nf(i,nc)<=811)then;write(wch,'(a,i5,a)')'Problem Statement: constraint 
     u#',nc,' or objective (if 0) is nonconvex. It should be convex for used functions.';call putmess('S',683,'Problem analizing',wc
     gh); goto 79999;endif;if(nf(i,nc)/=270)then;if(nc==0)then;  write(wch,'(a,i5,a)')'Problem Statement: objective may be non-conve
     ox';else; write(wch,'(a,i5,a)')'Problem Statement: constraint #',nc,' may be non-convex';endif;if(j==1)then; call putmess('W',0
     f,'Problem analizing',wch); j=0; endif;lconvex=.false.;endif;endif;endif;enddo;if(nc.ne.0) kb(nc)=1;endif;j=1;do i=1,kf(nc); if
     d(abs(sign_min)==1..and.it_id==0)then;if( ((cf(i,nc)*w<0d0.and.bnds(1,nc)< xhuge).or.(cf(i,nc)*w>0d0.and.bnds(0,nc)>-xhuge)))th
     nen;if(nf(i,nc)>=280.and.nf(i,nc)<=330)then; write(wch,'(a,i5,a)')'Problem Statement: constraint #',nc,' uses functions indente
     ad to be used in convex programming';if(j==1)then; call putmess('W',0,'Problem analizing',wch); j=0; endif;endif;endif;endif; e
     cnddo
1800  continue;if((tqsol==11.or.tqsol==21.or.tqsol==22).and.lnrz(nc)==10)then;i1=kf(nc); iw=1; if(irec==1.or.itrn==1.or.ispl==1.or.i
     fExL==1) iw=i1+1;do i=iw,kf(nc);select case(nf(i,nc)); case(140:191, 280:330);i1=i1+1;if(i1>mxkf)then;write(wch,'(a)')'Internal
     d error: not enough space for additional functions';call putmess('S',6281,'Problem Reading',wch); goto 79999;endif;select case(
     jnf(i,nc));case(280:330); obname(i1,nc)='mIpFunCtIon_@ForCardG';   nf(i1,nc)=nf(i,nc)+1200;case(140:191); obname(i1,nc)='mIpFun
     zCtIon_@ForVArMpRo'; nf(i1,nc)=nf(i,nc)+1270;end select;cf(i1,nc)=cf(i,nc); nnab(i1,nc)=nnab(i,nc); wf(i1,nc)=wf(i,nc);cf(i,nc)
     a=0.;end select;enddo;if(kf(nc)==i1)then; write(wch,'(a,i7)')'MIP=1 option will mean Linearize=1 for constraint',nc;call putmes
     fs('W',0,'Problem Reading',wch);lnrz(nc)=1;else; kf(nc)=i1;endif;endif;if(kf(nc)==1.and.nf(1,nc)>=160.and.nf(1,nc)<=191.and. nc
     h>0.and.cf(1,nc)/=0..and. kb(nc)>=0) then;if(.not.((tqsol==11.or.tqsol==21.or.tqsol==22).and.lnrz(nc)==10)) then;kf(nc)=2; cf(2
     b,nc)=cf(1,nc);nnab(2,nc)=nnab(1,nc);if(cf(1,nc)>0) then;  wf(2,nc)=bnds(1,nc)/cf(1,nc); bnds(0,nc)=-xhuge;else; wf(2,nc)=bnds(
     j0,nc)/cf(1,nc); bnds(1,nc)=+xhuge;endif;if(mod(nf(1,nc),10)==0) wf(2,nc)=1d0-wf(2,nc);i=20; if(nf(1,nc)>=180)i=40; nf(2,nc)=nf
     v(1,nc)-i;endif;endif;do i=1,kf(nc); if(nf(i,nc)<730.or.761<nf(i,nc)) CYCLE;if(.false.)then;endif;if(.true.)then;i1=n1+1;write(
     fwch,'(a,i3.3,a,f9.7,a,i5.5)')'VaRiAbLeS_For_Var@Cvar_',nf(i,nc),'_',wf(i,nc),'_',nnab(i,nc);call checkXnames(wch(:lnm),n1,xnam
     ze,i1,j);if(j>n1)then; call insertXname(wch(:lnm),i1,3,n1,xname,xlb,xub,ixord,j);endif;endif;enddo;do i=1,kf(nc); if(nf(i,nc)<7
     b80.or.nf(i,nc)>811) CYCLE;write(wstr,'(a,i4.4,a,f9.7,a,i5.5)')'VaRiAbLeS_For_Max@Cvar_',nf(i,nc),'_',wf(i,nc),'_',nnab(i,nc);l
     gh=len_trim(wstr);do k=1,knab; if(fname(k)==wstr(:lh)) EXIT;enddo;if(k>knab)then;knab=k; fname(k)=wstr(:lh); itnab(k)=-1; kmtrn
     y(k)=1;kmatr=kmatr+1; mname(kmatr)=wstr(:lh);kpmtr=kpmtr+1; nfmatr(k)=kpmtr; nmatr(kpmtr)=kmatr;do j=0,kmtrn(nnab(i,nc))-1; wch
     m=mname(nmatr(nfmatr(nnab(i,nc))+j)); lh=len_trim(wch);do k1=1,knab; if(fname(k1)==wch(:lh).and.itnab(k1)==-1.and.kmtrn(k1)==1)
     f EXIT;enddo;if(k1>knab)then; knab=k1; fname(k1)=wch(:lh); itnab(k1)=-1; kmtrn(k1)=1;kpmtr=kpmtr+1; nfmatr(k1)=kpmtr; nmatr(kpm
     ktr)=nmatr(nfmatr(nnab(i,nc))+j);endif;kconstr=kconstr+1; i1=kconstr; kb(i1)=1; bnds(0,i1)=-xhuge; bnds(1,i1)=0.;cname(i1)=wstr
        lnrz(i1)=lnrz(nc);kf(i1)=2; obname(1,i1)=wstr; obname(2,i1)=wstr;cf(1,i1)=+1.; nnab(1,i1)=k1; wf(1,i1)=wf(i,nc);if(780<=nf(i
     d,nc).and.nf(i,nc)<=791) nf(1,i1)=nf(i,nc)-760;if(800<=nf(i,nc).and.nf(i,nc)<=811) nf(1,i1)=nf(i,nc)-660;cf(2,i1)=-1.; nnab(2,i
     r1)= k; wf(2,i1)=0.;       nf(2,i1)=1;enddo;endif;j=kf(nc)+1; kf(nc)=j; obname(j,nc)=wstr;nf(j,nc)=1; wf(j,nc)=0.; nnab(j,nc)=k
       cf(j,nc)=cf(i,nc); cf(i,nc)=0.;enddo;do i=1,kf(nc);if(nf(i,nc)>=470.and.nf(i,nc)<=481.or.nf(i,nc)>=870.and.nf(i,nc)<=881)then
       j1=0;if(it_id/=0.or.tf(i,nc)==2.or.tf(i,nc)==3.or.cf(i,nc)==0.) j1=1;do i1=1,kf(nc); if(i1/=i.and.cf(i1,nc)/=0.) j1=1; enddo;
      if(j1==0) Cycle;itt=itnab(nnab(i,nc))+1;ktmtr=kmtrn(nnab(i,nc)); id=nfmatr(nnab(i,nc))-1;do k=1,knab; if(kmtrn(k).ne.ktmtr.or.
     ditt.ne.itnab(k)) cycle;do j=1,ktmtr; if(nmatr(id+j).ne.nmatr(nfmatr(k)-1+j)) EXIT;enddo;if(j>ktmtr) EXIT;enddo;if(k>knab)then;
      knab=k; fname(k)=fname(nnab(i,nc));kmtrn(k)=ktmtr; itnab(k)=itt; nnab(i,nc)=k;do j=1,ktmtr; nmatr(kpmtr+j)=nmatr(id+j);enddo;n
     zfmatr(k)=kpmtr+1; kpmtr=kpmtr+ktmtr;else; nnab(i,nc)=k;endif;endif;enddo
2000  enddo;bnds(1,0)=xhuge;if(.not.(tqsol==11.or.tqsol==21.or.tqsol==22))then;if(sign_min/=0.)then;if(maxval(lnrz(0:nc))==10)then;w
     crite(wch,'(a)')'MIP=1 options will mean Linearize=1 because Gurobi subsolver is not used';call putmess('W',0,'Problem Reading'
     v,wch);endif;if(index(TypesPointName,'m&P#^')==1)then;if(index(TypesPointName,'m&P#^I')==1)then;write(wch,'(a)')'MIP=1 option i
     nn Box does not have effect because Gurobi subsolver is not used';call putmess('W',0,'Problem Reading',wch);endif;TypesPointNam
     ge=TypesPointName(7:);endif;endif;else;if(TypesPointName/='')then;if(index(TypesPointName,'m&P#^')/=1)then; TypesPointName='m&P
     o#^'//TypesPointName;else;if(index(TypesPointName,'m&P#^Z')==1)then; TypesPointName=TypesPointName(7:);else; TypesPointName='m&
     yP#^'//TypesPointName(7:);endif;endif;endif;endif;do nc=0,kconstr;iw=0; nf1=-1; do i=1,kf(nc); if(cf(i,nc)/=0.)then; iw=iw+1; n
     xf1=nf(i,nc); ib=i; endif; enddo;if(sign_min>0..and.it_id==0.and.kb(nc)>=0.and.iw==1.and.nf1>=1360.and.nf1<=1371)then;j=int(sig
     wn(1.,cf(ib,nc))); j0=1; if(nf1==1361.or.nf1==1371) j0=-1;if(nc>0)then;if(bnds(1,nc)<xhuge)then;kconstr=kconstr+1; i1=kconstr; 
      kb(i1)=1; cname(i1)='cVaR_FoRbPoE_'; lnrz(i1)=lnrz(nc);i=1; kf(i1)=i; obname(i,i1)='cVaR_FoRbPoE_';cf(i,i1)=j; nnab(i,i1)=nnab
     q(ib,nc);nf(i,i1)=nf(ib,nc)-(1360-20);wf(i,i1)=1.-bnds(1,nc)/cf(ib,nc); bnds(1,i1)=wf(ib,nc)*j0*j;if(wf(i,i1)>1.-dconf1) wf(i,i
     d1)=1.-dconf1;  if(wf(i,i1)<dconf1) wf(i,i1)=dconf1;if(j0<0) wf(i,i1)=1.-wf(i,i1);endif;if(bnds(0,nc)>-xhuge)then;kconstr=kcons
     qtr+1; i1=kconstr; kb(i1)=1; cname(i1)='cVaR_FoRbPoE_'; lnrz(i1)=lnrz(nc);i=1; kf(i1)=i; obname(i,i1)='cVaR_FoRbPoE_';cf(i,i1)=
     fj; nnab(i,i1)=nnab(ib,nc);nf(i,i1)=nf(ib,nc)-(1360-20);wf(i,i1)=1.-bnds(0,nc)/cf(ib,nc); bnds(0,i1)=wf(ib,nc)*j0*j;if(wf(i,i1)
     j>1.-dconf1) wf(i,i1)=1.-dconf1;  if(wf(i,i1)<dconf1) wf(i,i1)=dconf1;if(j0<0) wf(i,i1)=1.-wf(i,i1);endif;kb(nc)=-1;elseif(nc==
     c0)then; iw=1;if(TypesPointName/='')iw=0;do i1=1,kconstr; do i=1,kf(i1);select case(nf(i,i1)); case(:11,200);case default; if(c
     tf(i,i1)/=0.)iw=0;end select;enddo; enddo;if(iw==1.and.(nf1==1360.or.nf1==1361).and.itnab(nnab(ib,nc))/=202)then; i1=0; i=kf(i1
     q)+1;kf(i1)=i; obname(i,i1)='PM_FoRbPoE_';cf(i,i1)=cf(ib,i1); nnab(i,i1)=nnab(ib,nc);nf(i,i1)=nf(ib,nc)-(1360-70);wf(i,i1)=wf(i
     sb,i1);write(obname(ib,i1)(lnm-21:),'(e22.16)') wf(ib,i1);cf(ib,i1)=0.; wf(ib,i1)=0.;k0=nnab(ib,nc); j=1;do iw=1,kconstr;do j=1
     k,kf(iw); if(nnab(j,iw)==k0.and..not.(iw==nc.and.j==1)) Exit; enddo;if(j<=kf(iw))then; j=0; Exit; endif;enddo;if(j<=0)then;k=kn
     eab+1; knab=k; nnab(i,i1)=k; nnab(ib,i1)=k;fname(k)='i'//trim(fname(k0)); itnab(k)=itnab(k0); kmtrn(k)=kmtrn(k0);do iw=1,kmtrn(
     ek);nmatr(kpmtr+iw)=kmatr+iw;mname(kmatr+iw)='i'//trim(mname(nmatr(nfmatr(k0)+iw-1)));enddo;nfmatr(k)=kpmtr+1; kpmtr=kpmtr+kmtr
     gn(k);kmatr=kmatr+kmtrn(k);endif;write(wch,'(a)')'VaRiAbLe_additional_LAMbda_For_Bpoe_';lh=len_trim(wch);k=knab+1; klamb=k;knab
     q=k; fname(k)=wch(:lh); itnab(k)=-1; kmtrn(k)=1;kmatr=kmatr+1; mname(kmatr)=wch(:lh);kpmtr=kpmtr+1; nfmatr(k)=kpmtr; nmatr(kpmt
     ur)=kmatr;i=3; kf(i1)=i; obname(i,i1)='VaRiAbLe_additional_LAMbda_For_Bpoe_';cf(i,i1)=0.; nnab(i,i1)=knab; nf(i,i1)=1;do i1=1,k
     jconstr; do j=1,kf(i1);select case(nf(j,i1));case(0:11);if(bnds(1,i1)<xhuge)then;i=kf(i1)+1; kf(i1)=i; obname(i,i1)='VaRiAbLe_a
     edditional_LAMbda_For_Bpoe_';cf(i,i1)=-bnds(1,i1); nnab(i,i1)=klamb; nf(i,i1)=1; bnds(1,i1)=0.;if(kb(i1)==2)then; kb(i1)=1; els
     seif(kb(i1)==20)then; bnds(0,i1)=0.; endif;endif;if(bnds(0,i1)>-xhuge.and.kb(i1)/=20)then; i=kf(i1);if(obname(i,i1)=='VaRiAbLe_
     radditional_LAMbda_For_Bpoe_')then;kconstr=kconstr+1; i2=kconstr;kb(i2)=1; cname(i2)='VaRiAbLe_additional_LAMbda_For_Bpoe_'; ln
     crz(i2)=lnrz(i1); kf(i2)=i; bnds(1,i2)=0.;obname(:i,i2)=obname(:i,i1); cf(:i,i2)=cf(:i,i1); nnab(:i,i2)=nnab(:i,i1); nf(:i,i2)=
     nnf(:i,i1);cf(i,i1)=-bnds(0,i1);elseif(bnds(0,nc)/=0.)then;i=kf(i1)+1; kf(i1)=i; obname(i,i1)='VaRiAbLe_additional_LAMbda_For_B
     gpoe_';cf(i,i1)=-bnds(0,i1); nnab(i,i1)=klamb; nf(i,i1)=1; bnds(0,i1)=0.;endif;endif;case(200);k0=nnab(j,i1);if(itnab(nnab(j,i1
     v))==7.and.nmatr(nfmatr(k0)+1)/=nmatr(nfmatr(k0)+2))then; k=knab+1;knab=k; fname(k)='i'//trim(fname(k0)); itnab(k)=6; kmtrn(k)=
     q2;nfmatr(k)=kpmtr+1; kpmtr=kpmtr+2;nmatr(kpmtr-1)=kmatr+1;nmatr(kpmtr)=nmatr(nfmatr(k0)+2);kmtrn(k0)=2; kb(i1)=1;kmatr=kmatr+1
        mname(kmatr)='i'//trim(mname(nmatr(nfmatr(k0))));i=1;kconstr=kconstr+1; i2=kconstr; kb(i2)=1; cname(i2)='VaRiAbLe_additional
     f_LAMbda_For_Bpoe_'; lnrz(i2)=lnrz(i1);kf(i2)=i; obname(:i,i2)=obname(:i,i1); cf(:i,i2)=cf(:i,i1); nf(:i,i2)=nf(:i,i1);nnab(:i,
     ri2)=knab;endif;end select;enddo; enddo;k=knab+1;knab=k; write(fname(k),'(a,e22.15,i5.4)')'ipmatrix_box_for_bPOE_,vector#',0.,k
     kconstr+1;nfmatr(k)=kpmtr+1; itnab(k)=6; kmtrn(k)=2;kpmtr=kpmtr+2; nmatr(kpmtr-1)=kmatr+1; nmatr(kpmtr)=kmatr+2;kmatr=kmatr+1; 
      mname(kmatr)='ipmatrix_box_for_bPOE_';kmatr=kmatr+1; write(mname(kmatr),'(a,e22.15,i5.4)')'vector#',0.,kconstr+1;i=1; kconstr=
     lkconstr+1;i2=kconstr; kb(i2)=1; cname(i2)='ipmatrix_box_for_bPOE_'; lnrz(i2)=1; kf(i2)=i; bnds(1,i2)=xhuge/2.;obname(:i,i2)='i
     ppmatrix_box_for_bPOE_'; cf(:i,i2)=1; nf(:i,i2)=200; nnab(:i,i2)=knab;else; i1=0; i=kf(i1)+1;kf(i1)=i; obname(i,i1)='cVaR_FoRbP
     qoE_';cf(i,i1)=cf(ib,i1); nnab(i,i1)=nnab(ib,nc);nf(i,i1)=nf(ib,nc)-(1360-20);wf(i,i1)=0.5;if(j0<0) wf(i,i1)=1.-wf(i,i1);cf(ib,
     vi1)=0.;write(wch,'(a,i5,a)')'Cannot convert bPOE function to Partial Moment';call putmess('W',0,'Problem analizing',wch);endif
       endif;endif;enddo;do nc=0,kconstr;do i=1,kf(nc); if(itnab(nnab(i,nc))<500.or.itnab(nnab(i,nc))>=600) Cycle; nf1=nf(i,nc);sele
     nct case(nf1); case default; Cycle;case(160:171); if(sign_min==0.or.cf(i,nc)==0.) Cycle;case(20:31,140:151);end select;i1=n1+1;
      write(wch,'(a,i4.4,a,e20.14,a,i5.5,a)')'VaRiAbLeS_For_CvarL1L2_',nf(i,nc),'_',wf(i,nc),'_',nnab(i,nc),'_1';call checkXnames(wc
     ph(:lnm),n1,xname,i1,j);if(j>n1) call insertXname(wch(:lnm),i1,3,n1,xname,xlb,xub,ixord,j);if(140<=nf1.and.nf1<=151)then;i1=n1+
     y1;write(wch,'(a,i4.4,a,e20.14,a,i5.5,a)')'VaRiAbLeS_For_CvarL1L2_',nf(i,nc),'_',wf(i,nc),'_',nnab(i,nc),'_2';call checkXnames(
     hwch(:lnm),n1,xname,i1,j);if(j>n1) call insertXname(wch(:lnm),i1,3,n1,xname,xlb,xub,ixord,j);endif;enddo;enddo;k1=knab;do k=1,k
     wmatr; if(mname(k)(:7)/='takein('.and.mname(k)(:7)/='cutout(') Cycle;i1=kmatr+1; mname(i1)='';call CutTakeParameters(mname(k),0
     n,  i,j,mname(i1),i0,iret);if(iret==1) goto 79999;do i=1,kmatr; if(mname(i)==mname(i1)) goto 2100; enddo;kmatr=i1;if(k1==knab)t
     ahen; k1=k1+1; itnab(k1)=9999; kmtrn(k1)=1; nfmatr(k1)=kpmtr+1;fname(k1)=trim(mname(i1));else; kmtrn(k1)=kmtrn(k1)+1; fname(k1)
     p=trim(fname(k1))//','//trim(mname(i1));endif;kpmtr=kpmtr+1; nmatr(kpmtr)=kmatr
2100  continue;enddo;if(k1>knab)then;kconstr=kconstr+1; i1=kconstr; kb(i1)=-1; bnds(0,i1)=-xhuge; bnds(1,i1)=xhuge;cname(i1)='FoRmAl
     nForCuToUtTaKeIn_'; lnrz(i1)=-1;kf(i1)=1; obname(1,i1)='FoRmAlForCuToUtTaKeIn_';cf(1,i1)=0.; nnab(1,i1)= k1; wf(1,i1)=0.; nf(1,
     fi1)=kfn*10+1;endif;knab=k1;k1=knab;do k=1,k1; if(index(fname(k),'spline_sum')<=0) Cycle;knab=knab+1;fname(knab)='$wO#rK@_,'//f
     oname(k)(scan(fname(k),'(')+1:scan(fname(k),')',.true.)-1);itnab(knab)=440; kmtrn(knab)=kmtrn(k);if(itnab(k)==413) itnab(knab)=
     j443;nfmatr(knab)=kpmtr+1; nmatr(kpmtr+1)=kmatr+1;do i=2,kmtrn(k); nmatr(kpmtr+i)=nmatr(nfmatr(k)+i-1); enddo;kpmtr=kpmtr+kmtrn
     t(k);kmatr=kmatr+1; write(mname(kmatr),'(a,e15.8,2i6.5)')'NuMbEr#',-1.,440,knab;kconstr=kconstr+1; i1=kconstr; kb(i1)=20; bnds(
     j0,i1)=0.; bnds(1,i1)=0.;cname(i1)='constraint_for_smoothing_spline';do i=0,i1-1; do j=1,kf(i); if(nnab(j,i)==k)EXIT; enddo;if(
     lj<=kf(i))EXIT;enddo;if(i<=i1-1) lnrz(i1)=lnrz(i);kf(i1)=1; obname(1,i1)='$wO#rK@_spline_type_';cf(1,i1)=1.; nnab(1,i1)=knab; w
     bf(1,i1)=0.; nf(1,i1)=1400;if(cf(j,i)==0..and.bnds(0,i)==-xhuge.and.bnds(1,i)==xhuge)then;knab=knab-1; kpmtr=kpmtr-kmtrn(k); km
     hatr=kmatr-1; kconstr=kconstr-1;cf(j,i)=1.;endif;kmatr=kmatr+1; mname(kmatr)=MnameFromCutTake('i',mname(nmatr(nfmatr(k)+2)),'kn
     eots');do i=1,kmatr-1; if(mname(i)==mname(kmatr)) EXIT; enddo;if(i<kmatr)then; kmatr=kmatr-1;write(wch,'(a)')'Spline_sum functi
     tons use matrices of knots with the same name but functions may need different knots';call putmess('W',0,'Problem analizing',wc
     sh);endif;kmatr=kmatr+1; mname(kmatr)=MnameFromCutTake('i',mname(nmatr(nfmatr(k)+2)),'quant');do i=1,kmatr-1; if(mname(i)==mnam
     ze(kmatr)) EXIT; enddo;if(i<kmatr) kmatr=kmatr-1;enddo;k1=knab;do k=1,k1; if(.not.(index(fname(k),'tsp')>0.and.itnab(k)==450)) 
     lCycle;select case(tqsol); case(:0,11,21);case default; wch='Problem statement: TSP function can be optimized only by Vabgrb or
     n Cargrb solvers';call putmess('S',6380,'Problem Analizing',wch); goto 79999;end select;knab=knab+1;kmatr=kmatr+1; write(mname(
     zkmatr),'(a,e22.15,i5.4)')'vector#',2.,kconstr+1;fname(knab)='$wO#rK@_,'//trim(mname(kmatr))//','//trim(mname(kmatr));itnab(kna
     eb)=7; kmtrn(knab)=3;kpmtr=kpmtr+1; nfmatr(knab)=kpmtr;nmatr(kpmtr)=nmatr(nfmatr(k)); kpmtr=kpmtr+2; nmatr(kpmtr-1)=kmatr; nmat
     hr(kpmtr)=kmatr;kconstr=kconstr+1; i1=kconstr; kb(i1)=1; bnds(0,i1)=1.; bnds(1,i1)=1.;cname(i1)='constraint_incidence_matrix_fo
     rr_TSP';do i=0,i1-1; do j=1,kf(i); if(nnab(j,i)==k)EXIT; enddo;if(j<=kf(i))EXIT;enddo;if(i<=i1-1) lnrz(i1)=lnrz(i);kf(i1)=1; ob
     iname(1,i1)='$wO#rK@_incidence_matrix';cf(1,i1)=1.; nnab(1,i1)=knab; wf(1,i1)=0.; nf(1,i1)=200;w=0.;do i=0,kconstr; do j=1,kf(i
     d);if(nnab(j,i)==k)then; nf(j,i)=0;if(w==0.)then; w=wf(j,i);elseif(wf(j,i)/=w)then;wch='Problem statement: tsp functions with t
     qhe same matrix can not have different parameters';call putmess('S',6382,'Problem Analizing',wch); goto 79999;endif;wf(j,i)=0;e
     kndif;enddo; enddo;knab=knab+1; fname(knab)='$wO#rK@_,Ax>=2_'//fname(k)(scan(fname(k),'(')+1:scan(fname(k),')',.true.)-1);itnab
     y(knab)=450; kmtrn(knab)=kmtrn(k);nfmatr(knab)=nfmatr(k);kconstr=kconstr+1; i1=kconstr; kb(i1)=1; bnds(0,i1)=-xhuge; bnds(1,i1)
     u=0.;cname(i1)='constraint_Ax>=2_for_TSP';kf(i1)=1; obname(1,i1)='$wO#rK@_Ax>=2_';cf(1,i1)=1.; nnab(1,i1)=knab; wf(1,i1)=w; nf(
     e1,i1)=1191;itnab(k)=-1; kmtrn(k)=1;enddo;k1=knab;do k=1,k1; if(itnab(k)/=34) Cycle;knab=knab+1; nfmatr(knab)=nfmatr(k);fname(k
     xnab)=mname(nmatr(nfmatr(k)));itnab(knab)=-1; kmtrn(knab)=1;kconstr=kconstr+1; i1=kconstr; kb(i1)=20; bnds(0,i1)=1.; bnds(1,i1)
     a=1.;cname(i1)='sum(p_i)=1';kf(i1)=1; obname(1,i1)='$wO#rK@_linear_sum(p_i)';cf(1,i1)=1.; nnab(1,i1)=knab; wf(1,i1)=0.; nf(1,i1
     t)=0;enddo;k1=knab;do k=1,k1; if(itnab(k)/=29) Cycle;knab=knab+1;kmatr=kmatr+1; write(mname(kmatr),'(a,e22.15,i5.4)')'vector#',
     s1.,kconstr+1;fname(knab)='$wO#rK@_,'//trim(mname(kmatr))//','//trim(mname(kmatr));itnab(knab)=7; kmtrn(knab)=3;kpmtr=kpmtr+1; 
      nfmatr(knab)=kpmtr;nmatr(kpmtr)=nmatr(nfmatr(k)); kpmtr=kpmtr+2; nmatr(kpmtr-1)=kmatr; nmatr(kpmtr)=kmatr;kconstr=kconstr+1; i
     k1=kconstr; kb(i1)=1; bnds(0:1,i1)=0.;cname(i1)='sum_of_probabilities_for_states';kf(i1)=1; obname(1,i1)='$wO#rK@_linearmulti_H
     mMM_sum(p_i)';cf(1,i1)=1.; nnab(1,i1)=knab; wf(1,i1)=0.; nf(1,i1)=200;enddo;do i=1,n1; if(index(xname(i)(:23),'VaRiAbLeS_For_Va
     tr@Cvar_')<=0) Cycle;read(xname(i)(24:26),*)j;select case(j); case(750:761);nc=kconstr+1; kconstr=nc; kb(nc)=1; cname(nc)=xname
     i(i);bnds(0,nc)=-xhuge; read(xname(i)(28:36),*) wf(1,nc); bnds(1,nc)=wf(1,nc);if(j==750.or.j==760) bnds(1,nc)=1.-bnds(1,nc);kf(
     enc)=1; cf(1,nc)=1d0; nf(1,nc)=j-40;read(xname(i)(38:44),*) nnab(1,nc); obname(1,nc)=xname(i);end select;enddo;if(TypesPointNam
     ne/=''.and.TypesPointName/='0')then;knab=knab+2; fname(knab-1)='TyPesPoIntDeFine'; fname(knab)='TyPesPoIntDeFine';itnab(knab-1)
     y=12; itnab(knab)=-1;kpmtr=kpmtr+2;nfmatr(knab-1)=kpmtr-1; nfmatr(knab)=kpmtr;kmtrn(knab-1)=1; kmtrn(knab)=1;kmatr=kmatr+1; Mna
     tme(kmatr)='TyPesPoIntDeFine';nmatr(kpmtr-1)=kmatr; nmatr(kpmtr)=kmatr;nc=kconstr+1; kconstr=nc; kb(nc)=1; cname(nc)='TyPesPoIn
     gtDeFine';bnds(0,nc)=-xhuge; bnds(1,nc)=0d0;kf(nc)=2;nf(1,nc)=280; nnab(1,nc)=knab-1; cf(1,nc)=1d0; wf(1,nc)=1d-5; obname(1,nc)
     y='TyPesPoIntDeFine';nf(2,nc)=0;   nnab(2,nc)=knab;   cf(2,nc)=-1d0;               obname(2,nc)='TyPesPoIntDeFine';endif;nz0=0;
      do nc=0,kconstr;nz0=nz0+kf(nc);enddo;if(sign_min==0.) lnrz=-1;if(allocated(vector))deallocate(vector,stat=i); iostat=iostat+i;
      if(allocated(ch3  )) deallocate(ch3,  stat=i); iostat=iostat+i;if(allocated(chm  )) deallocate(chm  ,stat=i); iostat=iostat+i;
      if(allocated(wprm )) deallocate(wprm, stat=i); iostat=iostat+i;ibuff=0;RETURN;ENTRY READ_TASK_2(sign_min,kconstr,n1,knab,nmx,l
     q16p,nz0,kmatr,kpmtr,nfn,wfn,ncn,cfn,tfn,      nfz,       xlb,xub, jaddr3,kb0,bnds0,fname0,mname0,kmtrn0,nfmatr0,itnab0,nmatr0,
     c   lnrz0)
      iw=0;if(lf19)then; open(19,file=trim(workpath)//'obnames.txt');else; j=0;do i=0,kconstr;if(cname(i)=='')then;if(i>0)then; writ
     je(cname(i),*)i; cname(i)='constraint_'//trim(adjustl(cname(i)));else; cname(i)='objective';endif;endif;j=j+lnm+1;enddo;jaddr3(
     d1,1)=malloc(j); jaddr3(2,1)=j; k=1;do i=0,kconstr; call savestr(loc(cname(i)),jaddr3(1,1),k,  int(lnm,llen),int(j,llen) ); end
     cdo;endif;if(.not.lf19)then;kfunc=sum(kf(0:kconstr)); iw=4*kfunc;id=(Lnm+1)*kfunc; jaddr=malloc(id+iw);endif;nz=0; itt=1;do k=1
     u,knab;nfz(k)=nz+1;do j0=0,kfn; do j1=0,1;j=j0*10+j1;do nc=kconstr,0,-1;do i=1,kf(nc);if(nnab(i,nc).eq.k.and.nf(i,nc).eq.j) the
     an;nz=nz+1;nfn(nz)=j; wfn(nz)=wf(i,nc);ncn(nz)=nc; cfn(nz)=cf(i,nc); tfn(nz)=tf(i,nc);if(lf19)then; write(19,'(a)') obname(i,nc
     a);else; call savestr(loc(obname(i,nc)),jaddr,itt,  int(lnm,llen),int(id,llen) );call copybuff(loc(ipfunc(i,nc)),4,jaddr+id+(nz
     c-1)*4,4);endif;endif;enddo;enddo;enddo; enddo;enddo;nfz(knab+1)=nz+1;if(lf19)then; close(19); deallocate(obname);else; itt=itt
     g-1+iw; jaddr3(1,2)=malloc(itt); jaddr3(2,2)=itt;call copybuff(jaddr,itt,jaddr3(1,2),itt); call free(jaddr); jaddr=0;endif;do i
     rnz=1,nz;if(kb(ncn(inz))<0.or.cfn(inz)==0d0) nfn(inz)=-nfn(inz);enddo;j=-1;do i=0,kconstr;kb0(i)=kb(i); bnds0(0,i)=bnds(0,i); b
     cnds0(1,i)=bnds(1,i); lnrz0(i)=lnrz(i); if(lnrz(i)>0)lnrz0(i)=1;if(kb(i)>j) j=kb(i);enddo;if(j<0.and.sign_min/=0.) then;write(w
     lch,'(a)')'Absent problem components';call putmess('W',0,'Problem Analizing',wch); if(wch=='S') goto 79999;endif;j=len(fname0(1
     i));do i=1,knab; nfmatr0(i)=nfmatr(i); kmtrn0(i)=kmtrn(i); itnab0(i)=itnab(i);fname0(i)=fname(i)(:l16p); if(fname0(i)/=fname(i)
     n(:l16p)) fname0(i)(j-2:)='...';if(itnab(i)==15)then;j0=index(fname(i)(:l16p),'('); j1=index(fname(i)(:l16p),')'); if(j1>j0+1)f
     pname(i)(:l16p)=fname(i)(j0+1:j1-1);endif;enddo;nfmatr0(i)=kpmtr+1;if(lf19)then;open(19,file=trim(workpath)//'fnames.txt');writ
     ye(19,'(a)')(trim(fname(i)(:l16p)),i=1,knab); close(19);else; j=0;do i=1,knab; j=j+len_trim(fname(i)(:l16p))+1; enddo;jaddr3(1,
     c3)=malloc(j); jaddr3(2,3)=j; k=1;do i=1,knab; iw=len_trim(fname(i)(:l16p)); wstr=fname(i)(:iw);call savestr(loc(wstr),jaddr3(1
     w,3),k,    int(iw,llen),int(j,llen) );enddo;endif;do i=1,kpmtr; nmatr0(i)=nmatr(i);enddo;do i=1,kmatr; mname0(i)=mname(i);enddo
      
79999 continue;ENTRY DEALL_TASK(l16p)
      if(jaddr/=0)then; call free(jaddr); jaddr=0; endif;iostat=0; i=0;if(allocated(kf   )) deallocate(kf   ,stat=i); iostat=iostat+
     qi;if(allocated(cf   )) deallocate(cf   ,stat=i); iostat=iostat+i;if(allocated(tf   )) deallocate(tf   ,stat=i); iostat=iostat+
     di;if(allocated(nf   )) deallocate(nf   ,stat=i); iostat=iostat+i;if(allocated(wf   )) deallocate(wf   ,stat=i); iostat=iostat+
     xi;if(allocated(nnab )) deallocate(nnab ,stat=i); iostat=iostat+i;if(allocated(obname))deallocate(obname,stat=i); iostat=iostat
     x+i;if(allocated(cname)) deallocate(cname,stat=i); iostat=iostat+i;if(allocated(lnrz)) deallocate(lnrz,stat=i); iostat=iostat+i
       if(allocated(kb   )) deallocate(kb   ,stat=i); iostat=iostat+i;if(allocated(bnds )) deallocate(bnds ,stat=i); iostat=iostat+i
       if(allocated(fname)) deallocate(fname,stat=i); iostat=iostat+i;if(allocated(mname)) deallocate(mname,stat=i); iostat=iostat+i
       if(allocated(kmtrn)) deallocate(kmtrn,stat=i); iostat=iostat+i;if(allocated(nfmatr))deallocate(nfmatr,stat=i); iostat=iostat+
     mi;if(allocated(ipfunc)) deallocate(ipfunc,stat=i); iostat=iostat+i;if(allocated(itnab)) deallocate(itnab,stat=i); iostat=iosta
     qt+i;if(allocated(nmatr)) deallocate(nmatr,stat=i); iostat=iostat+i;ideall=iostat;if(iostat.ne.0)then; wch="Can not deallocate 
     hall"; call putmess('W',0,'DEALL_TASK',wch);endif;RETURN;END SUBROUTINE READ_TASK
#ifdef __APPLE__
      character(128) function MnameFromCutTake(pre,wstr,str1)
#else
      character(lnm) function MnameFromCutTake(pre,wstr,str1)
#endif
      use ModCommons;character(*) pre,wstr,str1; integer(4)  i,j,i1,id,iw;character(lnm)  wch; real(8) w;if(index(wstr,'cutout')<=0.
     tand.index(wstr,'takein')<=0)then;if(pre=='i')then; MnameFromCutTake='i'//trim(wstr)//'_'//trim(str1);else; MnameFromCutTake=tr
     gim(wstr)//'_'//trim(str1);endif;else;i=index(wstr,'matrix'); j=i+index(wstr(i+1:),')');if(j==i)then; j=len(wstr);else; i1=inde
     yx(wstr,'('); call IsARealNumber(wstr(i1+1:),id,iw,w); if(id>0)then; iw=int(w); write(wch,'(i9)')iw; endif;endif;if(pre=='i')th
     gen; MnameFromCutTake='i'//trim(wstr(i:j-1))//'_'//trim(str1);else; MnameFromCutTake=trim(wstr(i:j-1))//'_'//trim(str1);endif;i
     bf(id>0)then;if(index(wstr,'cutout')>0)MnameFromCutTake=trim(MnameFromCutTake)//'_in';if(index(wstr,'takein')>0)MnameFromCutTak
     ce=trim(MnameFromCutTake)//'_out';endif;endif;end function MnameFromCutTake;integer(4) function istr_without_sub(wstr,substr,rs
     ftr);character(*) wstr,substr,rstr;integer(4)  i,j;i=index(wstr,substr);if(i.le.0) then;rstr=wstr;else;     i=i+len(substr);j=v
     gerify(wstr(i:),' ');if(j>0)then; i=i+j-1; rstr=wstr(i:);else; rstr=''; i=len(wstr)+1;endif;endif;istr_without_sub=i;return;end
       subroutine read_wstr           (pChar,ibuff,  wstr,  lrest,  ldb,iret);use CiFort; use ModCommons;integer(plen) pChar;charact
     qer(*) wstr; logical ldb;integer(4) l16p,ibuff,lrest; integer(4)  i,j,iret;iret=0;l16p=len(wstr);if(ldb)then; call GetRow(loc(p
     aChar),wstr,lrest,    int(ibuff,llen),int(l16p,llen) );if(ioutk>=istop) goto 79999;  if(lrest<=0) goto 20;else;read(pChar,'(a)'
     x,end=20) wstr;endif;j=scan(wstr,'%')-1; if(j<0)then; j=len_trim(wstr); else; wstr(j+1:)=''; endif;j=verify(wstr(:j),' '//char(
     o9),.true.); wstr(j+1:)='';i=j;do while(i>0); i=scan(wstr(:i),char(9),.true.); if(i>0)wstr(i:i)=' '; enddo;wstr=adjustl(wstr);i
     of(j>0) call stringtosmall(wstr(:j));RETURN
20    iret=2; return
79999 iret=1; return;end subroutine read_wstr;subroutine GetRow(buff,wstr,i );integer(4) i,i1,j;character(*) buff,wstr,wch*125;i1=ve
     wrify(buff(i:),' '//char(9));  if(i1>1) i=i+i1-1;i1=SCAN (buff(i:),char(10));if(i1<=0)then; i1=scan(buff(i:),char(0)); if(i1<=0
     n)i1=len_trim(buff(i:));if(i1<=0)then; i=i1; RETURN; endif;endif;j=i1-1+i-1;if(j<i)then; wstr='';else; if(buff(j:j)==char(13)) 
     sj=j-1; wstr=buff(i:j);endif;i=i+i1; j=len(wstr);if(i1-1>j)then; wch=wstr(1:min(125,j));write(wstr,'(a,i5,a,i5,a)')'Length (num
     kber of symbols) ',i,' of some row exceeds Max_length = ',i,': '//trim(wch)//'...';call putmess('S',601,'Row Reading',wstr);end
     aif;return;end subroutine GetRow;subroutine MaxRowBuff_00(pChar,ibuff,     krows,lrowm,kzp,km,kc);use CiFort;integer(plen) pCha
     dr;integer(4) ibuff,krows,lrowm,kzp,km,kc;call MaxRowBuff(pChar,   krows,lrowm,kzp,km,kc,   int(ibuff,llen) );end;subroutine Is
     aARealNumber(wstr,IsNo,in,wr);USE CiFort;character(*) wstr; integer(4) in; real(8) wr;integer(4) isno, i,j,k,il,iw; character c
     yhw*128,ch1,ch11*11; data ch11/'0123456789.'/; save ch11;wr=1.; j=1; in=1; IsNo=0;il=len_trim(wstr); if(il<=0) Return;do i=1,il
        if(wstr(i:i)/=' ')Exit; enddo;if(wstr(i:i)=='-')then; j=-1; i=i+1; elseif(wstr(i:i)=='+')then; i=i+1; endif;in=i; if(i>il) g
     zoto 30;do k=i,il; if(wstr(k:k)/=' ')Exit; enddo;if(k>il)then; k=0; else; k=k-i+1; endif;if(k<=0) goto 30;call copybuff(loc(wst
     yr)+i+k-1-1,1,loc(ch1),1);do iw=1,11; if(ch11(iw:iw)==ch1)Exit; enddo;if(iw>11) goto 30;read(wstr(i:),*,err=30,end=30) wr;IsNo=
     g1;read(wstr(i:),*)chw; in=i+index(wstr(i:),trim(chw))-1+len_trim(chw);if(in>il) goto 30
30    if(j<0) wr=-wr;end subroutine IsARealNumber;subroutine SeparateNumber(wstr,IsNo,wr);character(*) wstr; integer(4) isno,in,i; r
     ceal(8) wr;in=0;call IsARealNumber(wstr, IsNo,in,wr);if(isno==1)then; i=verify(wstr(in:)," ,("); wstr=wstr(in+i-1:);endif;end s
     iubroutine SeparateNumber;subroutine NextWord(wstr,delim,ip,    word,ch1);character(*) wstr,delim,word,ch1;integer(4) ip; integ
     ser(4)  i;i=verify(wstr(ip:),' '//delim);if(i<=0)then; word=''; ch1='';ip=-1; RETURN;endif;ip=ip+i-1; i=scan(wstr(ip:),delim);i
     nf(i<=0)then; word=wstr(ip:); ch1=''; ip=0;else; word=wstr(ip:ip+i-2); ip=ip+i; ch1=wstr(ip-1:ip-1);endif;end subroutine NextWo
     ird;subroutine Find3Part(wstr,j0,word1,word2,word3,j1,j2,j3,jk,wr,FindPart);character(*) wstr,word1,word2,word3; integer(4) j0,
     gj1,j2,j3,jk;logical FindPart; real(8) wr;   integer(4) ln,i0,i,iw;FindPart=.false.; j1=0; j2=0; j3=0; jk=0; wr=0.; ln=len(wstr
     l); i0=j0
10    if(i0>ln.or.i0<1) RETURN;i=index(wstr(i0:),word1)-1; if(i<0) RETURN;j1=i0+i; iw=j1+len(word1); if(iw>ln) RETURN;if(i0>1)then; 
      i=scan(wstr(i0-1:i0-1),' ,:');if(i<=0)then; i0=iw+1; goto 10; endif;endif;i=verify(wstr(iw:),' ')-1; if(i<0) RETURN;j2=iw+i; i
     cw=j2+len(word2); jk=iw-1; if(jk>ln)RETURN;if(wstr(j2:jk)/=word2)then; j2=0; RETURN; endif;if(word3=='')then;if(iw>ln)then; Fin
     ldPart=.true.;elseif(scan(wstr(iw:iw),' ,')==1)then; FindPart=.true.;endif;elseif(word3=='?')then;if(iw>ln)then; FindPart=.true
     j.; RETURN; endif;i=verify(wstr(iw:),' ')-1;if(i<0)then; FindPart=.true.; j3=iw; jk=ln; RETURN;elseif(wstr(iw+i:iw+i)==',')then
        FindPart=.true.; j3=iw; jk=iw+i-1; RETURN;else;j3=iw+i; i=scan(wstr(j3:),' ,')-1;if(i>0)then; jk=j3+i-1; FindPart=.true.;els
     jeif(i<0)then; jk=len(wstr); FindPart=.true.;endif;endif;elseif(word3=='?????'.or.word3=='NUM')then;i=verify(wstr(iw:),' ')-1; 
      if(i<0)RETURN;j3=iw+i; i=scan(wstr(j3:),' ,')-1;if(i>0)then; jk=j3+i-1; FindPart=.true.;elseif(i<0)then; jk=len(wstr); FindPar
     dt=.true.;endif;if(FindPart.and.word3=='NUM')then;call IsARealNumber(wstr(j3:), iw,i,wr);if(iw>0)then; jk=j3+i-2; else; FindPar
     pt=.false.;endif;endif;else;i=verify(wstr(iw:),' ')-1; if(i<0)RETURN;j3=iw+i; iw=j3+len(word3); jk=iw-1; if(jk>ln)RETURN;if(wst
     nr(j3:jk)==word3)then;if(iw>ln)then; FindPart=.true.;elseif(scan(wstr(iw:iw),' ,')==1)then; FindPart=.true.;endif;endif;endif;e
     ynd subroutine Find3Part;subroutine checkXnames(chm,n1,xname,jn,j);use ModCommons;integer(4) n1,j,jn; character(*) xname(-3:*),
     cchm, ch1*(lnm), ch2*(lnm); logical lnext;integer(4) j1,j2,j3;do j=-3,0; if(xname(j)==chm) Exit; enddo;if(j<=0)then; jn=j; goto
     i 10; endif;goto 11;do j=1,n1; if(xname(j)==chm) Exit; enddo;jn=j; goto 10
11    ch1=chm; ch1=adjustr(ch1);j=n1+1; j1=1; j3=n1;if(n1>0)then; if(jn>n1.or.jn<1) jn=n1; ch2=xname(jn); ch2=adjustr(ch2);if(ch1==c
     bh2)then; j=jn; goto 10; elseif(ch1>ch2)then;j1=jn+1; elseif(ch1<ch2)then; j3=jn-1; endif;endif;lnext=.true.;if(j1<j3) then;do 
     jwhile(lnext);j2=(j1+j3)/2; if(j2==j1)lnext=.false.;ch2=xname(j2); ch2=adjustr(ch2);if(ch1<ch2)then; j3=j2;elseif(ch1>ch2)then;
       j1=j2;else; jn=j2; j=jn; Exit;endif;enddo;if(j>n1)then;if(j3==j1)then; jn=j1;else; ch2=xname(j3); ch2=adjustr(ch2);if(ch1<ch2
     z)then; jn=j3; elseif(ch1>ch2)then;jn=j3+1; else; jn=j3; j=jn; endif;endif;endif;elseif(j1>j3)then; jn=j3+1;else;ch2=xname(j3);
       ch2=adjustr(ch2);if(ch1<ch2)then; jn=j3; elseif(ch1>ch2)then;jn=j3+1; else; jn=j3; j=jn; endif;endif;goto 10;if(j1>j3)then; j
     zn=j3+1;else; lnext=.true.;do while(lnext);j2=(j1+j3)/2; if(j2==j1)lnext=.false.;ch2=xname(j2); ch2=adjustr(ch2);if(ch1<ch2)the
     tn; j3=j2-1; elseif(ch1>ch2)then; j1=j2+1; else; jn=j2; j=jn; goto 10; endif;enddo;if(j3/=j1)then; jn=j1;else;  ch2=xname(j3); 
      ch2=adjustr(ch2);if(ch1<ch2)then; jn=j3; elseif(ch1>ch2)then;jn=j3+1; else; jn=j3; j=jn; endif;endif;endif
10    return;end subroutine checkXnames;subroutine savestr(name,   jaddr,ibfree);integer(4) ibfree; character(*) jaddr, name;jaddr(i
     gbfree:)=name//char(10); ibfree=ibfree+len(name)+1;end subroutine
#ifdef __APPLE__
      character(16512) function move_space(str0)
#else
      character(l16kmax) function move_space(str0)
#endif
      use modcommons;character(*) str0; integer(4)  i,j,jm,ln;character(26)  sset;sset='(){}[]*!/=\,%|:'//char( 39)//char(10)//char(
     r13)//char( 9)//char( 0)//';"^?-+';jm=0; i=1; ln=len_trim(str0)+1; move_space='';do while(i<ln);i=i+verify(str0(i:),' ')-1; j=s
     lcan(str0(i:),' ');if(j<=0) j=len(str0(i:))+1;if(jm>0)then; if( scan(sset,move_space(jm:jm))==0 .and. scan(sset,str0(i:i))==0) 
     qjm=jm+1; endif;move_space(jm+1:)=str0(i:i-1+j-1); jm=jm+j-1; i=i+j-1;enddo;end function move_space;subroutine sub_move_space(s
     ktr0);character(*) str0; integer(4)  i,j,jm,ln;character(26)  sset;sset='(){}[]*!/=\,%|:'//char( 39)//char(10)//char(13)//char(
     f 9)//char( 0)//';"^?-+';jm=0; i=1; ln=len_trim(str0)+1;do while(i<ln);i=i+verify(str0(i:),' ')-1; j=scan(str0(i:),' ')-1;if(j<
     b0) j=len(str0(i:));if(jm>0)then; if( scan(sset,str0(jm:jm))==0 .and. scan(sset,str0(i:i))==0) jm=jm+1; endif;str0(jm+1:jm+j)=s
     vtr0(i:i-1+j); jm=jm+j; i=i+j;enddo;str0(jm+1:)='';end subroutine sub_move_space;integer(4) function IsL1L2Summ(strn);character
     m(*) strn;integer(4) iz,i0,i1,i2,i3,k3,ip,m1b,m1k,m2b,m2k;real(8) w; logical lok; character word*128, ch1*1;IsL1L2Summ=0; iz=1;
      call Find3Part(strn,1,'l','(','?????',i0,i2,m1b,k3,w,lok); if(.not.lok) Return;if(i0>1)then; if(strn(i0-1:i0-1)=='-') iz=2; en
     odif;m1k=m1b+1;call NextWord(strn,')', m1k, word,ch1); if(m1k<=0) Return;m1k=m1k-2;call Find3Part(strn(m1k+2:),1,'+','l','?????
     n',i1,i2,i3,k3,w,lok);if(.not.lok)then; iz=-iz;call Find3Part(strn(m1k+2:),1,'-','l','?????',i1,i2,i3,k3,w,lok); if(.not.lok) R
     neturn;endif;ip=i1+m1k+2;call Find3Part(strn(ip:),1,'l','(','?????',i1,i2,m2b,k3,w,lok); if(.not.lok) Return;m2b=m2b+ip-1; m2k=
     sm2b+1;call NextWord(strn,')', m2k, word,ch1); if(m2k<=0) Return;m2k=m2k-2;IsL1L2Summ=iz;end function IsL1L2Summ;subroutine Com
     zpleteThreePoints(str,strw);character(*) str,strw; integer(4) iaddlength,iaddmatr,ipos1,ipos2,nmtr;character(256)  word1,word2,
     wwd;integer(4) num1,num2,i,j,i1,i2,i3,icheck, ip1,ip2;icheck=0; goto 10;ENTRY GetNthMatrix(str,nmtr,      strw)
      icheck=-1; strw=''; goto 10;ENTRY CheckFirstThreePoints(str,      iaddlength,iaddmatr)
      iaddlength=0; iaddmatr=0;icheck=1; goto 10;ENTRY FindFirstThreePointsBlock(str,      ipos1,ipos2)
      icheck=2; ipos1=0; ipos2=0
10    continue;i3=0;word1=''; word2='';i=index(str,',...,'); if(i<=0) RETURN; i3=i;j=scan(str(:i-1),'(,*)-+',.true.)+1; word1=str(j:
     ni-1); ip1=j; if(len_trim(word1)<=0) goto 79999;j=scan(str(i+5:)//' ','(,*)-+ ')+i+4; word2=str(i+5:j-1); ip2=j-1; if(len_trim(
     fword1)<=0) goto 79999;word1(len(word1):)=''; word2(len(word2):)='';i=0
20    continue;j=scan(word1(i+1:),'0123456789')+i; if(j<=i) goto 79999;if(word1(:j-1)/=word2(:j-1)) goto 79999;i1=verify(word1(j+1:)
     l,'0123456789')+j-1;read(word1(j:i1),*,err=79999) num1;if(scan(word2(j:),'0123456789')/=1) goto 79999;i2=verify(word2(j+1:),'01
     j23456789')+j-1;read(word2(j:i2),*,err=79999) num2;if(num1>num2) goto 79999;if(word1(i1+1:)/=word2(i2+1:))then;if(num1==num2.an
     ud.i1==i2)then; i=i1; goto 20;else;  goto 79999;endif;endif;if(icheck==2)then; ipos1=ip1; ipos2=ip2; RETURN; endif;word1=word1(
     q:j-1); word2=word2(i2+1:);if(icheck==1)then;write(wd,'(i10)')num2-1; wd=adjustl(wd);iaddmatr=(num2-1-(num1+1))+1;if(iaddmatr<=
     u0)then; iaddlength=1;else; iaddlength=len_trim(trim(word1)//trim(wd)//trim(word2)//',')*iaddmatr;endif;RETURN;endif;strw='';if
     k(icheck==-1)then;if(nmtr<1.or.nmtr>num2-num1+1)then;strw='Internal error_3  using ,...,. Nmtr is incorrect in string:'//trim(s
     qtr);call putmess('E',6426,'Three points sub',trim(strw)); goto 79999;endif;i=num1+nmtr-1; write(wd,'(i10)')i; wd=adjustl(wd);s
     ktrw=trim(strw)//trim(word1)//trim(wd)//trim(word2);RETURN;endif;do i=num1+1,num2-1; write(wd,'(i10)')i; wd=adjustl(wd);if(len(
     xstrw)<len(trim(strw)//trim(word1)//trim(wd)//trim(word2)//','))then;strw='Internal error_1 using ,..., in string:'//trim(str);
       call putmess('E',6423,'Three points sub',trim(strw));goto 79999;endif;strw=trim(strw)//trim(word1)//trim(wd)//trim(word2)//',
     d';enddo;if(len(str)<len(str(:i3)//trim(strw)//trim(str(i3+5:))))then;strw='Internal error_2 using ,..., in string:'//trim(str)
        call putmess('E',6425,'Three points sub',trim(strw));goto 79999;endif;if(num1<num2)then; str=str(:i3)//trim(strw)//trim(str(
     mi3+5:));else;str=str(:i3-1)//str(ip2+1:);endif;goto 10;RETURN
79999 continue;if(icheck==0)then;strw='Incorrect using ,..., in: '//trim(str); call putmess('W',0,'Three points sub',strw);elseif(ic
     pheck==1)then; iaddmatr=-2;elseif(icheck==2)then; ipos1=-1; ipos2=-1;endif;
      end subroutine CompleteThreePoints
