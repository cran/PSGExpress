      subroutine READ_TASK_0(probaddr,   mxkf,mxconstr,nabmx,  l16p,km,kc);use ModCommons; use cifort;integer(plen) probaddr;integer
     l(4) mxkf,mxconstr,nabmx,km,kc;integer(4) l16p,i,j,k,ibuff,iBuffLen;integer(PLEN),pointer:: pChar; integer(PLEN),target:: iu;i=
     l0; j=0; k=0; iu=0; km=0; kc=0;pChar=>iu;if(probaddr>0)then; call setpointer(probaddr, pChar); ibuff=iBuffLen(probaddr);call Ma
     lxRowBuff_00(pChar,ibuff,i,j,k,km,kc);endif;l16k=min0(j+10,L16Kmax); mxconstr=min0(mxconstr,km)+100; mxkf=min0(mxkf,i-2)+100; n
     labmx=min0(nabmx,km)+100;l16p=min0(2*lnm,max0(l16k-23+35-14,68))+min0(lnm,(l16k-14))+2;l16p=max0(l16p,lnm+25);l16p=max0(l16p,l1
     l6k); l16k=l16p;end subroutine READ_TASK_0;subroutine READ_TASK (probaddr,mxkf,mxconstr,nabmx,nmx,km,xbndhuge,l16p,    xname,xl
     lb,xub,ixord,
     +sign_min,lconvex,n1,kconstr,knab,nz0,kmatr,kpmtr,timelimit,
     +TypesPointName,allLbounds,allUbounds,LowerPointName,UpperPointName);USE CiFort; use ModCommons; use FuncNames;integer(plen) pr
     lobaddr;integer(4) mxkf,mxconstr,nabmx,nmx,l16p,km;character(lnm) xname(-3:*); integer(4) ixord(*);real(8) xbndhuge, xlb(*),xub
     l(*);integer(4)  n1,kconstr,knab,nz0;real(8) sign_min, timelimit,allLbounds,allUbounds;character(*) TypesPointName,LowerPointNa
     lme,UpperPointName;logical lconvex;real(8) bnds0(0:1,0:*),   wfn(*),cfn(*),tfn(*);character(lnm) fname0(*), mname0(*);integer(4
     l) nfn(*),  ncn(*),  nfz(*), kb0(0:*),    kmtrn0(*),nfmatr0(*),itnab0(*),nmatr0(*),lnrz0(0:kconstr);integer(plen) jaddr3(2,*);r
     leal(8) dconf1, dconf2, dconf22;common /dconf/dconf1, dconf2, dconf22;real(8) qpeps; real(4) wza(2),wzaf,awm; data wza/0.,0./;e
     lquivalence(wzaf,awm);COMMON /CMACHE/qpEPS,awm;logical lf1,lf2,lf3,lf4,lf5,lf6,lf7;common /SetFirst/lf1,lf2,lf3,lf4,lf5,lf6,lf7
      logical lfza; equivalence (lf7,lfza);integer(4) nz,iostat,ISTR_WITHOUT_SUB,nc,inz,ideall,nabpmx,matrmx,iret;integer(4) i,j,k,i
     l1,j0,j1,id,ibuff,lrest,m,kpmtr,kmatr,itt,ktmtr,inewnab,nfg;real(8) w,w1,xhuge,wparam;integer(PLEN),pointer:: pChar;integer(PLE
     lN) jaddr;logical not_task, lDB,lw,IsPrBg;character(l16p) wstr, pwstr, wch, ch1*(1),ch9*1;character(l16kmax),external:: move_sp
     lace;character(l16p),allocatable:: chm(:);integer(4)  k1,kfunc,icalc,nf1,nf2,iopn,i0,icut,ilmax,ilex,iksm,irec,iexl,itrn,ispl,i
     ltsp,iabs,isum,idif,
     +iccomp,iw,i2,j2,ln,j3,j4,lh,ib,k0,klamb;integer(4),external:: ibufflen,isl1l2summ,ifindclosebracket,icheckpsgname;integer(4),a
     lllocatable:: kf(:),nf(:,:),nnab(:,:),kmtrn(:),nfmatr(:),itnab(:),nmatr(:),kb(:), lnrz(:),ipfunc(:,:);real(8),allocatable::cf(:
     l,:),tf(:,:),wf(:,:),wprm(:),bnds(:,:);character(lnm),allocatable::mname(:),cname(:),obname(:,:),vector(:), ch3(:);character(l1
     l6kmax),allocatable:: fname(:);character(lnm) MnameFromCutTake;save jaddr, kf,cf,tf,nf,wf,nnab,obname,cname,lnrz, kb,bnds,fname
     l,mname,kmtrn,nfmatr,ipfunc,
     +itnab,nmatr;ib=0; iw=0; itrn=0; irec=0; iexl=0;dconf1=scale(1d0,-26);dconf2=scale(1d0,-43);dconf22=1d0-dconf2;   ch9=char(9);i
     lbuff=0;if(probaddr>0)then; call setpointer(probaddr, pChar); ibuff=iBuffLen(probaddr); endif;if(ibuff<=0)then; wch='Empty prob
     llem buffer'; call putmess('S',612,'Problem Reading',wch); goto 79999;endif;lDB=.true.; jaddr=0; lrest=1;not_task=.true.;   xhu
     lge=huge(w)/2.; sign_min=1.;nabpmx=km*3; matrmx=km*2;allocate(
     +cname(0:mxconstr),
     +lnrz(0:mxconstr),
     +kb(0:mxconstr),
     +bnds(0:1,0:mxconstr),
     +kf(0:mxconstr),
     +cf(mxkf,0:mxconstr),
     +tf(mxkf,0:mxconstr),
     +nf(mxkf,0:mxconstr),
     +wf(mxkf,0:mxconstr),
     +nnab(mxkf,0:mxconstr),
     +obname(mxkf,0:mxconstr),
     +ipfunc(mxkf,0:mxconstr),
     +fname(nabmx),
     +wprm(nabmx),
     +itnab(nabmx),
     +kmtrn(nabmx),
     +nfmatr(nabmx),
     +nmatr(nabpmx),
     +mname(matrmx),
     +STAT=iostat);if(iostat.ne.0)then; wch='Arrays allocation is failed'; call putmess('S',6040,'Problem Reading',wch); goto 79999;
       endif;allocate(vector(0:1), ch3(3), chm(4), STAT=iostat);if(iostat.ne.0)then; wch='Arrays allocation is failed'; call putmess
     l('S',6043,'Problem Reading',wch); goto 79999; endif;icalc=-1;TypesPointName=''; LowerPointName=''; UpperPointName='';bnds(0,:)
     l=-xhuge; bnds(1,:)=xhuge;cname=''; obname=''; lnrz=0;tf=0.;xname(-3)='value';xname(-2)='id';xname(-1)='scenario_probability';x
     lname(0)= 'scenario_benchmark';ipfunc=huge(kfunc); kfunc=0;n1=0;knab=0;kconstr=0;kpmtr=0;kmatr=0;wprm=0d0;kf=0;wf=0d0;wstr=''; 
      vector=''; wch=''; goto 18
15555 wch='Problem Statement: incorrect row: '//trim(pwstr);call putmess('S',608,'Problem Reading',wch); goto 79999
18    do while(wstr(:1)=='%'.or.wstr==''); call read_wstr(pChar,ibuff,wstr,lrest,ldb,iret);if(iret==2)then; goto 20; elseif(iret==1)
     lthen; goto 79999; endif;enddo;pwstr=wstr;if(.not.IsPrBg(wstr,wch,i,j)) goto 15555; if(j/=0) goto 15555;goto 35
20    continue;if(ldb) wch='Problem section of Problem Statement: no keyword Problem or some string defining beginning of problem '
     +//trim(probnm);if(idb<=0) wch='No keyword Problem: or problems content in file '//trim(taskfname);call putmess('S',610,'Proble
     lm Reading',wch); goto 79999;i=istr_without_sub(wstr,'problem:',wstr);wch='';if(wstr(:5)=='type='.or.wstr(:5)=='type '.or.wstr=
     l='') goto 31;if(wstr(:1)==',') goto 30;read(wstr,*, end=30,err=30) wch;goto 31
30    wch='Problem section of Problem Statement: incorrect Problem_name';call putmess('S',613,'Problem Reading',wch); goto 79999
31    continue;i=istr_without_sub(wstr,trim(wch),wstr)
35    continue;if(probnm/=wch) then;write(wch,'(a,i5)')'Length of problem name '//trim(wch)//' exceeds Max=',lnm;call putmess('W',0,
     l'Problem Reading',wch); if(wch=='S') goto 79999;endif;wstr=''; sign_min=-i;if(sign_min==10.)sign_min=0.;if(sign_min==0.)then; 
      tqsol=0;endif;if(wstr=='') goto 52;if(index(wstr,'type')<=0) goto 15555;wch=''; read(wstr,*, end=30) wch;if(wch=='') read(wstr
     l,*, end=30) wch,wch;if(wch(:5)/='type='.and.wch(:5)/='type ') goto 15555;i=istr_without_sub(wstr,'type',wstr);if(wstr(:1)=='='
     l)then; i=istr_without_sub(wstr,'=',wstr); else; goto 50; endif;chm(1)='';read(wstr,*,end=50,err=50) chm(1);goto 51
50    wch='Problem section of Problem Statement: incorrect Problem_type'; call putmess('S',622,'Problem Reading',wch); goto 79999
51    continue;i=istr_without_sub(wstr,trim(chm(1)),wstr);if(chm(1).eq.'minimize') then;     sign_min=+1d0;elseif(chm(1).eq.'maximiz
     le')then;  sign_min=-1d0;elseif(chm(1).eq.'calculate')then; sign_min= 0d0;else; GOTO 50;endif;if(verify(wstr,' d,'//ch9)>0) got
     lo 15555
52    continue;nc=0;do while(wstr(:1)=='%'.or.wstr==''); call read_wstr(pChar,ibuff,wstr,lrest,ldb,iret);if(iret==2)then; goto 1001;
       elseif(iret==1)then; goto 79999; endif;enddo;pwstr=wstr;if(wstr(:10)=='objective:'.or.wstr(:10)=='objective_'.or.wstr(:10)=='
     lobjective ')then; wstr=adjustl(wstr(11:));else; call IsFunctionRow(wstr,fnc_name,kfn,    w,chm,nfg,id,nf1);if(nf1>=0) goto 75;
      if(wstr(:10)=='constraint'.and.scan(wstr(11:11),' ,:_=<>')==1) goto 100;if(wstr(:6)=='value:'.or.wstr(:6)=='value ') goto 100;
      if(wstr(:6)=='point:'.or.wstr(:6)=='point ')goto 100;if(wstr(:4)=='box:'.or.wstr(:4)=='box '.or.wstr(:4)=='box='.or.wstr(:4)==
     l'box<'.or.wstr(:4)=='box>') goto 1071;if(wstr(:17)=='box_of_variables:'.or.wstr(:17)=='box_of_variables '.or.wstr(:17)=='box o
     lf variables ') goto 1071;if(wstr(:7)=='solver:'.or.wstr(:7)=='solver ') goto 1072;i=0; goto 54;endif;i=scan(wstr,',');if(i<=0)
     lthen; i=scan(wstr,' '); if(i<=0)i=len_trim(wstr)+1;j=1; call NextWord(wstr,'=',j,ch3(1),ch1);select case(ch3(1)); case('linear
     lize','mip','quadratic','function'); if(ch1=='=')i=0;end select;else; if(wstr(:i-1)=='') goto 15555;  if(scan(trim(wstr(:i-1)),
     l' =:')>0) goto 15555;endif;cname(nc)=trim(wstr(:i-1))
54    continue;j=i+1; call NextWord(wstr,'=',j,ch3(1),ch1);if(ch3(1)=='linearize'.and.ch1=='=')then;  call NextWord(wstr,' ',j,ch3(1
     l),ch1);if(ch3(1)=='0')then; lnrz(nc)=-1; elseif(ch3(1)=='1')then; lnrz(nc)=+1; else; goto 55; endif;if(wstr(j:)/='') goto 1555
     l5;elseif(ch3(1)=='mip'.and.ch1=='=')then;  call NextWord(wstr,' ',j,ch3(1),ch1);if(ch3(1)=='0')then; lnrz(nc)=0; elseif(ch3(1)
     l=='1')then; lnrz(nc)=10; else; goto 55; endif;if(wstr(j:)/='') goto 15555;elseif(ch3(1)=='quadratic'.and.ch1=='=')then;  call 
     lNextWord(wstr,' ',j,ch3(1),ch1);if(ch3(1)=='0')then; lnrz(nc)=0; elseif(ch3(1)=='1')then; lnrz(nc)=-2; else; goto 55; endif;if
     l(wstr(j:)/='') goto 15555;elseif(nc==0.and.ch3(1)=='function'.and.ch1=='=')then;  call NextWord(wstr,' ',j,ch3(1),ch1);if(ch3(
     l1)=='0')then; lnrz(nc)=0; elseif(ch3(1)=='1')then; lnrz(nc)=-3; else; goto 55; endif;if(wstr(j:)/='') goto 15555;else; if(j>=0
     l) goto 15555;endif;if(cname(nc)(:lnm-1)/=wstr(:i-1)) then;write(wch,'(a,i5,a)')'Problem Statement: length (number of symbols) 
     lof objective name exceeds Max=',lnm-11,
     +': '//trim(wstr(11:i-1));call putmess('S',624,'Problem Reading',wch); goto 79999;endif;goto 62
55    wch='Problem Statement: only one option with value 0 or 1 may be used for objective: '//trim(wstr);call putmess('S',626,'Probl
     lem Reading',wch); goto 79999
62    continue;wstr='';do while(wstr.eq.''.or.wstr(:1)=='%'); call read_wstr(pChar,ibuff,wstr,lrest,ldb,iret);if(iret==2)then; goto 
     l1001; elseif(iret==1)then; goto 79999; endif;enddo
65    continue;if(icalc==-1)then;if(wstr(:10)=='constraint'.and.scan(wstr(11:11),' ,:_=<>')==1) goto 100;if(wstr(:4)=='box:'.or.wstr
     l(:4)=='box '.or.wstr(:4)=='box='.or.wstr(:4)=='box<'.or.wstr(:4)=='box>') goto 100;if(wstr(:17)=='box_of_variables:'.or.wstr(:
     l17)=='box_of_variables '.or.wstr(:17)=='box of variables ') goto 100;if(wstr(:7)=='solver:'.or.wstr(:7)=='solver ')goto 100;if
     l(wstr(:6)=='value:'.or.wstr(:6)=='value ')goto 100;if(wstr(:6)=='point:'.or.wstr(:6)=='point ')GOTO 100;else;if(wstr(:4)=='box
     l:'.or.wstr(:4)=='box '.or.wstr(:4)=='box='.or.wstr(:4)=='box<'.or.wstr(:4)=='box>') goto 100;if(wstr(:17)=='box_of_variables:'
     l.or.wstr(:17)=='box_of_variables '.or.wstr(:17)=='box of variables ') goto 100;if(wstr(:7)=='solver:'.or.wstr(:7)=='solver ')g
     loto 100;if(wstr(:6)=='value:'.or.wstr(:6)=='value ')goto 100;if(wstr(:6)=='point:'.or.wstr(:6)=='point ')GOTO 100;if(wstr(:10)
     l=='constraint'.and.scan(wstr(11:11),' ,:_=<>')==1)then;wch='Problem Statement: Constraint: section should not appear after key
     lword Value: or Point: '//trim(wstr);call putmess('S',630,'Problem Reading',wch); goto 79999;endif;endif
75    continue;pwstr=wstr;call IsFunctionRow(wstr,fnc_name,kfn,    w,chm,nfg,id,nf1);if(nf1<0)then; wch='Function Row in Problem Sta
     ltement: incorrect string: '//trim(wstr);call putmess('S',631,'Problem Reading',wch); goto 79999;endif;if(kf(nc)==mxkf)then;wri
     lte(wch,'(2(a,i4))')'Problem Statement: number of functions in constraint #',nc,' exceeds Max=',mxkf;call putmess('S',628,'Prob
     llem Reading',wch); goto 79999;endif;if(w==0.)then; i=min(len_trim(wstr),120);wch='Zero coefficient for function in row: '//tri
     lm(wstr(:i))//'...';call putmess('W',0,'Problem Reading',wch); if(wch=='S') goto 79999;endif;kf(nc)=kf(nc)+1;cf(kf(nc),nc)=w;  
      if(icalc>=0)cf(kf(nc),nc)=0.;nf(kf(nc),nc)=nf1;tf(kf(nc),nc)=0.;kfunc=kfunc+1; ipfunc(kf(nc),nc)=kfunc;if(cf(kf(nc),nc)/=0..an
     ld.icalc==-1.and.it_id==0.and.sign_min/=0.)then;select case(nf1);case(441,1160:1172,1180,1190,1200,1201,1270,1271);wch='Problem
     l Statement: vector function cannot be used in objective or constraint: '//trim(pwstr);call putmess('S',627,'Problem Reading',w
     lch); goto 79999;end select;endif;if(nf1==1270.or.nf1==1271)then;if(nf1==1270)tf(kf(nc),nc)=2.; if(nf1==1271)tf(kf(nc),nc)=3.;i
     l=index(wstr,'('); j=index(wstr,')',.true.);if(i>0.and.i<j-1)then; wstr=wstr(i+1:j-1); else; goto 15555; endif;call IsFunctionR
     low(wstr,fnc_name,kfn,    w,chm,nfg,id,nf2);if(nf2<0)goto 15555;select case(nf2);case(441,1160:1172,1180,1190,1200,1201,1270,12
     l71);wch='Problem Statement: Gradient and Increment can not be calculated for vector function: '//trim(pwstr);call putmess('S',
     l629,'Problem Reading',wch); goto 79999;case(430); if(nf1==1271)then;wch='Problem Statement: Increment can not be calculated fo
     lr function: '//trim(pwstr);call putmess('S',6291,'Problem Reading',wch); goto 79999;endif;end select;pwstr=wstr;nf1=nf2; nf(kf
     l(nc),nc)=nf1;endif;i=istr_without_sub(wstr,trim(chm(1)),wstr);iOpn=index(wstr,'(');i=index(pwstr,'(');i0=index(pwstr,trim(chm(
     l1)));if(i>1)then; obname(kf(nc),nc)=trim(pwstr(i0:i-1));if(obname(kf(nc),nc)/=trim(pwstr(i0:i-1))) then;write(wch,'(a,i5,a)')'
     lProblem Statement: length (number of symbols) of function name exceeds Max='
     +,lnm,': '//trim(pwstr(i0:i-1));call putmess('S',632,'Problem Reading',wch); goto 79999;endif;itt=len_trim(obname(kf(nc),nc));i
     lf(scan(trim(pwstr(i0:i-1)),' ')>0) then;write(wch,*)'Problem Statement: incorrect function name: '//trim(pwstr); call putmess(
     l'S',635,'Problem Reading',wch);goto 79999;endif;else; wch='';if(i<=0)then; wch='Function Row in Problem Statement: missing ( i
     ln: '//trim(pwstr);endif;if(wch/='') then; call putmess('S',633,'Problem Reading',wch); goto 79999; endif;endif;i=iOpn;select c
     lase(nfg);case(2:3,7:8,14:19,21:22,28:33,34,36,37,38,40,41,45:56,64,67:70,73:81,82:83,85:96,
     +104,105,107,112:115,123:124,129:131,132,134,135,136:137,140);i1=index(wstr,',');if(i1==0.or.i+1>i1-1) goto 15555;call IsARealN
     lumber(wstr(i+1:), k,j1,w);i1=index(wstr(i+1+j1-1:),',')-1+(i+1+j1-1);if(i1<(i+1+j1-1).or.i+1>i1-1) goto 15555;if(k<=0) goto 77
     l; if(wstr(i+1+j1-1:i1-1)/='') goto 15555;select case(nfg);case(2:3,21:22,37,53:54,73:74,77:79,82:83,93:94,112:113,129:131,134,
     l135);if(w>1d0.or.w<0d0)then; wch='Problem Statement: incorrect parameter in CVaR (CDaR) function in: '//trim(pwstr)//
     +'. It should be in range (0,1)';call putmess('S',634,'Problem Reading',wch); goto 79999;endif;if(w>1d0-dconf1) w=1d0-dconf1   
     l  ; if(w<dconf1) w=dconf1;case(14:15,38,55:56,75:76,80:81,95:96);if(w>1d0.or.w<0d0) then;wch='Problem Statement: incorrect par
     lameter in VaR function: '//trim(pwstr)//'. It should be in range (0,1)';call putmess('S',637,'Problem Reading',wch); goto 7999
     l9;endif;if(w>1d0-dconf1) w=1d0-dconf1     ; if(w<dconf1) w=dconf1;if(w<0.5.and.icalc==-1) then;wch="VaR_Conf_Lev<0.5. Algorith
     lm may provide a poor solution for VaR:"//trim(pwstr);call putmess('W',0,'Problem Reading',wch); if(wch=='S') goto 79999;endif;
      case(36);if(nf1==360)then; if(w==0d0)cf(kf(nc),nc)=w;if(w>1d0)then; wch='Problem Statement: incorrect parameter in Power Utili
     lty function: '//trim(pwstr)//
     +'. It should be in range (0,1)';call putmess('S',638,'Problem Reading',wch); goto 79999;endif;elseif(nf1==361)then;if(w>1d0.or
     l.w<0d0)then; wch='Incorrect parameter for CVaR (CDaR) in row: '//trim(pwstr);call putmess('S',634,'Problem Reading',wch); goto
     l 79999;endif;if(w>1d0-dconf1) w=1d0-dconf1     ; if(w<dconf1) w=dconf1;w=1d0-w;endif;case(34,64,104);if(w<=0.)then; wch='Incor
     lrect value of parameter in Exponential Utility function: '//trim(pwstr)//'. It should be > 0';call putmess('S',6390,'Problem R
     leading',wch); goto 79999;endif;case(107);if(w<1.)then; wch='Incorrect value of parameter in Lp_norm_... function: '//trim(pwst
     lr)//'. It should be < 1';call putmess('S',6393,'Problem Reading',wch); goto 79999;endif;case(123:124);if(.not.(0.<w.and.w<1.))
     lthen;wch='Incorrect value of parameter in Kb_err function: '//trim(pwstr)//'. It should be in range (0,1)';call putmess('S',63
     l96,'Problem Reading',wch); goto 79999;endif;case(105,132); i=int(w);if(.not.(w>=1..and.w==i))then;if(nfg==105) wch='Incorrect 
     lparameter in Markov model function: '//trim(pwstr)//'. It should be integer positive';if(nfg==132) wch='Incorrect parameter in
     l Kantorovich function: '//trim(pwstr)//'. It should be integer positive';call putmess('S',6398,'Problem Reading',wch); goto 79
     l999;endif;end select;select case(nfg);case(2:3,14:15,21:22,36,37,38,53:56,73:81,82:83,93:96,112:113,123:124,129:131,134,135); 
      w1=1d0;case default;                      w1=0d0;end select;if(id.eq.1) w=w1-w;case default;i1=iOpn; w=0d0;end select;wf(kf(nc
     l),nc)=w;wParam=w;GOTO 80
77    continue;wch='Problem Statement: incorrect parameter in function: '//trim(pwstr); call putmess('S',640,'Problem Reading',wch);
      goto 79999
80    continue;i=index(wstr,')',.true.);if(verify(wstr(i+1:),' d,'//ch9)>0) goto 15555;chm(1)=wstr(i1+1:i-1);if(chm(1).eq.'')then;wc
     lh='Empty argument list in '//trim(pwstr); call putmess('W',0,'Problem Reading',wch);endif;chm(1)=move_space(chm(1));icut=0;ilm
     lax=0;if(chm(1)(:4)=='lmax')then;if(chm(1)(5:index(chm(1),'(')-1)==''.and. chm(1)( len_trim(chm(1)):)==')' ) ilmax=1;if(ilmax==
     l1)then; chm(2)=adjustl(chm(1)( index(chm(1),'(')+1 : index(chm(1),')',.true.)-1));if(chm(2)(:4)=='cut('.and.chm(2)(len_trim(ch
     lm(2)):)==')') icut=1;endif;endif;ilex=0;if(index(chm(1)(:index(chm(1),'(')),'logexp_')==1)then; ilex=1;chm(1)=chm(1)(index(chm
     l(1),'(')+1:index(chm(1),')',.true.)-1);endif;iksm=0; if(index(chm(1)(:index(chm(1),'(')),'ksm_')==1) iksm=1;if(icut==0)then;if
     l(index(chm(1)(:index(chm(1),'(')),'cut_')==1
     +.or. chm(1)(:4)=='cut(') icut=1;endif;irec=0; if(index(chm(1)(:index(chm(1),'(')),'recourse_')==1) irec=1;if(index(chm(1)(:ind
     lex(chm(1),'(')),'recourse(')==1) irec=1;if(index(chm(1)(:index(chm(1),'(')),'recourse ')==1) irec=1;iExL=0; if(index(chm(1)(:i
     lndex(chm(1),'(')),'fnextdir_')==1) iExL=1;itrn=0; if(index(chm(1)(:index(chm(1),'(')),'ltranche')==1) itrn=1;ispl=0; if(index(
     lchm(1)(:index(chm(1),'(')),'spline_sum_')==1) ispl=2;if(index(chm(1)(:index(chm(1),'(')),'spline_sum(')==1) ispl=2;if(index(ch
     lm(1)(:index(chm(1),'(')),'spline_sum ')==1) ispl=2;itsp=0; if(index(chm(1)(:index(chm(1),'(')),'tsp_')==1) itsp=1;if(index(chm
     l(1)(:index(chm(1),'(')),'tsp(')==1) itsp=1;if(index(chm(1)(:index(chm(1),'(')),'tsp ')==1) itsp=1;iabs=0; if(index(chm(1)(:ind
     lex(chm(1),'(')),'abs')==1)then;if(chm(1)(4:index(chm(1),'(')-1)=='')then; iabs=1;endif;endif;isum=0; if(index(chm(1)(:index(ch
     lm(1),'(')),'ssum')==1)then;if(chm(1)(5:index(chm(1),'(')-1)=='')then; isum=1;endif;endif;idif=0; if(index(chm(1)(:index(chm(1)
     l,'(')),'sdif')==1)then;if(chm(1)(5:index(chm(1),'(')-1)=='')then; idif=1;endif;endif;if(isum+idif==0)then; isum=IsL1L2Summ(chm
     l(1)); if(isum<0)then; idif=-isum; isum=0; endif;endif;iccomp=0; i=index(chm(1),'(');if(i>0.and.idif==0.and.isum==0)then;i=inde
     lx(chm(1)(:i),'*');call IsFunctionRow(chm(1)(i+1:),fnc_name,kfn,    w,chm(2),i,j,nf2);if(nf2>0)then;select case(nf2);case(361,3
     l70); iccomp=1;case(1190,1191,1200,1201);case(1160:1172,441);case default;wch='Problem Statement: not allowed composition: '//t
     lrim(pwstr); call putmess('S',641,'Problem Reading',wch);goto 79999;end select;endif;endif;if(nfg==20) then;if(vector(0)==''.an
     ld.bnds(0,nc)>-xhuge) then;write(vector(0),'(a,e22.15,i5.4)')'vector#',bnds(0,nc),nc;endif;if(vector(1)==''.and.bnds(1,nc) <xhu
     lge) then;write(vector(1),'(a,e22.15,i5.4)')'vector#',bnds(1,nc),nc;endif;if(vector(0).ne.'') chm(1)=trim(chm(1))//','//trim(ve
     lctor(0));if(vector(1).ne.'') chm(1)=trim(chm(1))//','//trim(vector(1));elseif(vector(0)/=''.or.vector(1)/='')then;wch='Problem
     l Statement: bound vector is not allowed in constraint: '//trim(pwstr);call putmess('S',642,'Problem Reading',wch); goto 79999;
      endif;itt=0;select case(nfg);case(3,5,6,8,10,12,15,17,19,21:26, 41,79,111,113,115,109,131,137);  itt=2;end select;itt=itt+id;s
     lelect case(nfg);case(20);     itt=4;if(vector(0).ne.'') then;if(vector(1).ne.'')then; itt=7; else; itt=5; endif;elseif(vector(
     l1).ne.'')then; itt=6;endif;case(21:26);  itt=itt+6;case(27,43);  itt=10;  itt=itt+id;if(nfg==43) itt=11;case(28:33);  itt=12;c
     lase(34:36,117,118);  itt=13;if(nf1==361)itt=114;case(37:39);  itt=14;case(44);if(nf1==440)then; itt=15;else; iExL=1;endif;chm(
     l1)=trim(obname(kf(nc),nc))//'('//trim(chm(1))//')';case(45:48);  itt=16;case(49:64);  itt=17;case(67:70);  itt=18;case(73:76);
        itt=18;case(77)   ;  itt=19;case(78:81);  itt=22;case(82)   ;  itt=23;case(83:84);  itt=24;case(132);    itt=34;case(85:88);
        itt=20;case(89:104); itt=21;case(107);    itt=28;case(121);    itt=25;case(129:131);itt=30;case(133,134);    itt=26;case(135
     l);    itt=27;case(105);    itt=29;case(139);    itt=114;case(140);    itt=440;case(116);if(ilex>0) itt=13;if(iksm>0) itt=24;ca
     lse(119);if(id==0)then; ispl=2;if(ilex>0) itt=13;chm(1)='spline_sum'//trim(wstr); wstr='('//trim(chm(1))//')';else; itsp=1;endi
     lf;case(120);if(nf1==1200)then; irec=1;chm(1)='recourse'//trim(wstr); wstr='('//trim(chm(1))//')';else; itrn=1;chm(1)='ltranche
     l'//trim(wstr); wstr='('//trim(chm(1))//')';endif;end select;if(irec>0) itt=200;if(itrn>0) itt=201;if(iExL>0) itt=202;if(icut>0
     l) itt=itt+300;if(ispl>0) itt=itt+400;if(itsp>0)then; itt=itt+450;if(itt/=450)then; wch='Problem Statement: TSP function may be
     l used only with linear';call putmess('S',6443,'Problem reading',wch); goto 79999;endif;endif;if(iccomp>0) itt=nf2+10000*nf1;if
     l(ilmax==1.and.(icut==0.and.itt>0.or.icut==1.and.itt>300))then;endif;if(icut==1.and.(itt==302.or.itt==303))then;wch='Problem St
     latement: functions using Deviation of Loss cannot use Cut(E) operation in the current version of PSG: '//
     +trim(pwstr);call putmess('S',6441,'Problem reading',wch); goto 79999;endif;if(iabs==1.and.(itt<=3.or.itt==14).and.nfg/=42)then
     l; itt=itt+100;elseif(iabs==1)then;wch='Problem Statement: superposition of function '//trim(pwstr)//' and Abs in not implement
     l in the current version of PSG';call putmess('S',6445,'Problem reading',wch); goto 79999;endif;if(isum==1.and.itt<=3) itt=itt+
     l500;if(isum==2.and.itt<=3) itt=itt+510;if(idif==1.and.itt<=3) itt=itt+550;if(idif==2.and.itt<=3) itt=itt+540;if(isum>0.or.idif
     l>0)then; iw=1;if(itt<500.or.itt>553)iw=0;select case(nfg); case(0,4:6,11:13,18:19,110:115,116,119); iw=0; end select;if(iw==0)
     lthen; wch='Problem Statement: function '//trim(pwstr)//
     +' cannot use Addition/Subtraction of Losses in the current version of PSG';call putmess('S',6447,'Problem reading',wch); goto 
     l79999;endif;endif;select case(itt); case(440,28,29,34);case default; wParam=0.;end select;do k=1,knab;if(fname(k)(:l16p)==move
     l_space(chm(1)).and.wprm(k)==wParam.and.
     +( (kmtrn(k)==1.and.itt<4).and.itnab(k)<4.or.
     +.not.(kmtrn(k)==1.and.itt<4).and.itnab(k)==itt) )
     +EXIT;enddo;if(k>knab) then;ktmtr=0; inewnab=0; chm(3)=move_space(chm(1));call CompleteThreePoints(chm(1),chm(2));if(irec==1)ch
     lm(1)=adjustl(chm(1)( index(chm(1),'(')+1 : index(chm(1),')',.true.)-1));if(itrn==1)chm(1)=adjustl(chm(1)( index(chm(1),'(')+1 
     l: index(chm(1),')',.true.)-1));if(ilmax==1)chm(1)=adjustl(chm(1)( index(chm(1),'(')+1 : index(chm(1),')',.true.)-1));if(idif>0
     l.or.isum>0)then;chm(1)=adjustl(chm(1)( index(chm(1),'(')+1 : index(chm(1),')',.true.)-1));if(index(chm(1),')')>0.and.index(chm
     l(1),')')<index(chm(1),'(',.true.))then;chm(1)( index(chm(1),')') : index(chm(1),'(',.true.) )=',';endif;endif;i=index(chm(1),'
     ll(');do while(i>0.and.itt/=15); j=iFindCloseBracket('()',chm(1),i+2); if(j>0)then; chm(1)(i:i+1)=''; chm(1)(j:j)=''; endif;i=i
     lndex(chm(1),'l(');enddo;i=index(chm(1),'g(');if(i>0)then;select case(nf1);case(40:51,110:121,200:201,270:271,290,310,330:361,4
     l20:441,580:591,610:621,640,830,980:991,
     +1010:1021,1040:1091,1160:1221,1270:1271,1320:1351,1380:);wch='Problem Statement: G operation can not be used with this functio
     ln: '//trim(chm(1));call putmess('S',6413,'Problem reading',wch); goto 79999;case default;do while(i>0.and.itt/=15); j=iFindClo
     lseBracket('()',chm(1),i+2); if(j>0)then; chm(1)(i:i+1)=''; chm(1)(j:j)=''; endif;i=index(chm(1),'g(');enddo;nf1=nf1+1-2*id; id
     l=1-id;  nf(kf(nc),nc)=nf1;select case(nfg);case(2:3,14:15,21:22,36,37,38,53:56,73:81,82:83,93:96,112:113,123:124,129:131,134,1
     l35); w1=1d0;case default;                      w1=0d0;end select;wf(kf(nc),nc)=w1-wf(kf(nc),nc);end select;endif;chm(1)=adjust
     ll(chm(1));if(itt==201)then;write(chm(1),'(a,e15.8,i9.8,a)')trim(chm(1))//',NuMbEr#',0.,itt;endif;if(itt==202)then;chm(2)=chm(3
     l); do i=1,len_trim(chm(2)); if(chm(2)(i:i)==',')chm(2)(i:i)=' '; enddo;write(chm(4),'(a,e15.8,i9.8,a)')'NuMbEr#',0.,itt,trim(c
     lhm(2))//','//trim(chm(1));chm(1)=chm(4);endif;if(itt==29)then; w=wf(kf(nc),nc);chm(2)=chm(3); do i=1,len_trim(chm(2)); if(chm(
     l2)(i:i)==',')chm(2)(i:i)=' '; enddo;write(chm(4),'(a,e15.8,i9.8,a)')'NuMbEr#',-w,itt,trim(chm(2))//','//trim(chm(1));chm(1)=ch
     lm(4);endif;if(itt==34)then; w=wf(kf(nc),nc);chm(2)=chm(3); do i=1,len_trim(chm(2)); if(chm(2)(i:i)==',')chm(2)(i:i)=' '; enddo
      write(chm(4),'(a,e15.8,i9.8,a)')'NuMbEr#',-w,itt,trim(chm(2))//','//trim(chm(1));chm(1)=chm(4);endif;if(itt==26.or.itt==24)the
     ln;chm(2)=chm(3); do i=1,len_trim(chm(2)); if(chm(2)(i:i)==',')chm(2)(i:i)=' '; enddo;write(chm(4),'(a,e15.8,i9.8,a)')'NuMbEr#'
     l,0.,itt,trim(chm(2))//','//trim(chm(1));chm(1)=chm(4);endif;if(ispl==1.or.ispl==2)then;i1=1; i2=1; wch='';do while(.true.);i1=
     lindex(chm(1)(i2:),'polynom_piece')+i2-1;if(i1<=i2)then; EXIT; else; i2=index(chm(1)(i1:),'(')+i1-1; endif;if(i2<=i1)then; wch=
     l'Problem Statement: incorrect substring: '//trim(chm(1));call putmess('S',6403,'Problem reading',wch); goto 79999;endif;if(wch
     l==trim(adjustl(chm(1)(i1+14:i2-1))))then; wch='Substring '//trim(chm(1))//' has equal names of polynom_pieces';call putmess('S
     l',6405,'Problem reading',wch); goto 79999;endif;wch=trim(adjustl(chm(1)(i1+14:i2-1)));enddo;i=index(chm(1),'('); i1=index(chm(
     l1),')',.true.);if(i>0.and.i1>0.and.i+1<=i1-1) chm(1)=adjustl(chm(1)(i+1:i1-1));i=1; i1=1;do while(i1>0); i0=i; i1=index(chm(1)
     l(i:),')'); i=i0+i1-1; j=i0+index(chm(1)(i0:),'(')-1;if(i>=i0.and.j>i)then; chm(1)=chm(1)(:i-1)//','//trim(chm(1)(j+1:)); else;
       i=i+1; endif;enddo;if(ispl==1)then;call SeparateNumber(chm(1), j1,w); if(j1==0.or.w<0.) goto 15555;write(chm(4),'(a,e15.8,i9.
     l8,a)')'NuMbEr#',w,itt,','//trim(chm(1));chm(1)=chm(4);elseif(ispl==2)then;write(chm(4),'(a,e15.8,i9.8,a)')'NuMbEr#',0.,itt,','
     l//trim(chm(1));chm(1)=chm(4);endif;endif;if(itt==440)then; w=wf(kf(nc),nc); if(w<0.) goto 15555;write(chm(4),'(a,e15.8,i9.8,a)
     l')'NuMbEr#',-w,itt,','//trim(chm(1));chm(1)=chm(4);endif;if(itt==450)then;i=index(chm(1),'('); i1=index(chm(1),')',.true.); if
     l(i>0.and.i1>0.and.i+1<=i1-1) chm(1)=adjustl(chm(1)(i+1:i1-1));write(chm(4),'(a,e15.8,i9.8,a)')'NuMbEr#',0.,itt,','//trim(chm(1
     l));chm(1)=chm(4);endif;if(iccomp==1)then;j=1;call NextWord(chm(1),' *,(',j,chm(2),ch1);chm(1)=adjustl(chm(1)( index(chm(1),'('
     l)+1 : index(chm(1),')',.true.)-1));if(chm(2)(:7)=='vector_')then; chm(1)=trim(chm(1))//','//trim(chm(2));else; call SeparateNu
     lmber(chm(2), j1,w); if(j1==0) w=1d0;write(chm(1),'(a,e15.8)')trim(chm(1))//','//'NuMbEr#',w;endif;call SeparateNumber(chm(1), 
     lj1,w);if(j1==1)then;write(chm(4),'(a,e15.8,i9.8,a,e15.8)')'NuMbEr#',w,itt,','//trim(chm(1))//',NuMbEr#',w;chm(1)=chm(4);else; 
      w=dconf1; j=1;if(chm(1)(:7)=='vector_')then; call NextWord(chm(1),',',j,chm(2),ch1);write(chm(4),'(a,e15.8,i9.8,a)')'NuMbEr#',
     lw,itt,','//trim(chm(1)(j:))//','//trim(chm(2));chm(1)=chm(4);else;write(chm(4),'(a,e15.8,i9.8,a,e15.8)')'NuMbEr#',w,itt,','//t
     lrim(chm(1))//',NuMbEr#',w;chm(1)=chm(4);endif;endif;endif;if(chm(1).eq.'') goto 15555;do while(.true.);j=index(chm(1),'('); i=
     lindex(chm(1),',');if(0<j.and.j<i)then; j=j+index(chm(1)(j:),')')-1; i=j+index(chm(1)(j+1:),',');if(i<=j) i=0;endif;if(itt==15)
     l i=0;if(i==1) goto 85;if(i>0) then;chm(2)=trim(chm(1)(:i-1));chm(1)=trim(ADJUSTL(chm(1)(i+1:)));else;chm(2)=trim(chm(1)); chm(
     l1)=''; if(chm(2)=='') goto 85;endif;call SeparateNumber(chm(2), j1,w);if(j1>0)then;select case(nf1);case(1400);write(chm(2),'(
     la,e15.8,a)')'NuMbEr#',w;case default;wch='Problem Statement: numerical parameter is not used in a function: '//trim(pwstr);cal
     ll putmess('S',6431,'Problem Reading',wch); goto 79999;end select;endif;do m=1,kmatr;if(Mname(m).eq.chm(2)) EXIT;enddo;if(m>mat
     lrmx)then;write(wch,'(a,i5)')'Number of matrices in Problem Statement exceeds Max=',matrmx;call putmess('S',643,'Problem Readin
     lg',wch); goto 79999;endif;if(m.gt.kmatr) then;kmatr=m; inewnab=1;mname(kmatr)=trim(chm(2));if(mname(kmatr)/=trim(chm(2)))then;
       write(wch,'(a,i5,a)')
     +'Problem Statement: length (number of symbols) of matrix name exceeds Max=',lnm,': '//trim(chm(2));call putmess('S',645,'Probl
     lem Reading',wch); goto 79999;endif;endif;ktmtr=ktmtr+1;if(kpmtr+ktmtr>nabpmx)then;write(wch,'(a,i5)')'Number of pointers to ma
     ltrices exceeds Max=',nabpmx;call putmess('S',648,'Problem Reading',wch); goto 79999;endif;nmatr(kpmtr+ktmtr)=m;if(i==0) EXIT;e
     lnddo;if(inewnab==0) then;if(ktmtr>1) then;do i=1,knab; if(wprm(i)/=wParam) cycle;if(kmtrn(i).ne.ktmtr.or.itt.ne.itnab(i)) cycl
     le;do j=1,ktmtr;if(nmatr(kpmtr+j).ne.nmatr(nfmatr(i)-1+j)) EXIT;enddo;if(j>ktmtr) EXIT;enddo;if(i>knab) then; inewnab=1;else; k
     l=i;endif;else;inewnab=1;endif;endif;if(inewnab>0) then;if(k>nabmx)then;write(wch,'(a,i5)')'Number of datasets in Problem State
     lment exceeds Max=',nabmx;call putmess('S',651,'Problem Reading',wch); goto 79999;endif;knab=k;fname(k)(:l16p)=chm(3);nfmatr(k)
     l=kpmtr+1; kmtrn(k)=ktmtr;kpmtr=kpmtr+ktmtr; itnab(k)=itt;wprm(k)=wParam;if(ispl==1.or.ispl==2.or.itt==440.or.itt==450.or.iccom
     lp==1)then;write(mname(nmatr(nfmatr(k))),'(a,9i5.5)')trim(mname(nmatr(nfmatr(k))))//' ',k;endif;if(ktmtr==1.and.itt<4.and.icut=
     l=0)then;select case(nfg); case(:19,40:42,116,123:124,136:137); itnab(k)=-1; end select;endif;if(fname(k)(:l16p)/=chm(3))then; 
      write(wch,'(a,i5,a)')
     +'Problem Statement: length (number of symbols) of list of matrices exceeds Max=',l16p,': '//trim(chm(3));call putmess('S',6526
     l,'Problem Reading',wch); goto 79999;endif;endif;endif;nnab(kf(nc),nc)=k;not_task=.false.;GOTO 62
85    continue;wch='Function row in Problem Statement: missing matrices for function in: '//trim(pwstr);call putmess('S',654,'Proble
     lm Reading',wch); goto 79999
100   continue;do while(wstr.eq.''.or.wstr(:1)=='%'); call read_wstr(pChar,ibuff,wstr,lrest,ldb,iret);if(iret==2)then; goto 1001; el
     lseif(iret==1)then; goto 79999; endif;enddo;if(.not.(wstr(:10)=='constraint'.and.scan(wstr(11:11),' ,:_=<>')==1)) goto 107;pwst
     lr=wstr;if(scan(wstr(11:11),':_,')==1)then; wstr=adjustl(wstr(12:));else; wstr=adjustl(wstr(11:));endif;if(kconstr==mxconstr)th
     len;write(wch,'(a,i5)')'Number of constraints in Problem Statement exceeds Max=',mxconstr;call putmess('S',657,'Problem Reading
     l',wch); goto 79999;endif;nc=kconstr+1; kconstr=nc;wch=''; read(wstr,*,err=102,end=102)wch; i=index(trim(wch),':'); if(i>0)wch=
     lwch(:i-1);i=scan(wch,'><='); if(i>0)wch=wch(:i-1)
102   if(wch/='linearize'.and.wch(:10)/='linearize='.and.wch/='mip'.and.wch(:10)/='mip='.and.
     +wch(:1)/='<'.and.wch(:1)/='>'.and.wch(:1)/='='.and.
     +wch/='lower_bound'.and.wch/='upper_bound'.and.wch(:12)/='lower_bound='.and.wch(:12)/='upper_bound=')then;cname(nc)=wch; i=istr
     l_without_sub(wstr,trim(wch),wstr);i=verify(wstr,' :,');  if(i>0)wstr=wstr(i:);endif;call Find3Part(wstr,1,'linearize','=','???
     l??',j1,i,i1,k,w,lw);if(lw)then; if(wstr(i1:k)=='1')then; lnrz(nc)=1; elseif(wstr(i1:k)=='0')then; lnrz(nc)=-1; else; goto 1555
     l5; endif;wstr(j1:k)='d';elseif(j1*i>0)then; goto 15555;endif;call Find3Part(wstr,1,'mip','=','?????',j1,i,i1,k,w,lw);if(lw)the
     ln; if(wstr(i1:k)=='1')then; lnrz(nc)=10; elseif(wstr(i1:k)=='0')then;             else; goto 15555; endif;wstr(j1:k)='d';elsei
     lf(j1*i>0)then; goto 15555;endif;vector='';call Find3Part('Equal'//wstr,1,'Equal','==','NUM',j1,i,i1,k,w,lw);if(j1*i<=0) call F
     lind3Part('Equal'//wstr,1,'Equal','=','NUM',j1,i,i1,k,w,lw);if(j1*i<=0)then; bnds(0:1,nc)=-xhuge;else;if(i1<=0)then; goto 15555
      else; i1=i1-5; k=k-5;if(lw)then; bnds(0:1,nc)=w; goto 1035; endif;if(wstr(i1:k)=='-infinity'.or.wstr(i1:k)=='infinity')then;wc
     lh='Constraint section of Problem Statement: incorrectly specified bound in equality: '//trim(pwstr)//
     +'. It should be in range (-1e20, +1e20)';call putmess('S',661,'Problem Reading',wch); goto 79999;else;bnds(0:1,nc)=0; vector(0
     l:1)=wstr(i1:k);if(k-i1+1>lnm)then; write(wch,'(a,i5,a)')'Problem Statement: length (number of symbols) of vector name exceeds 
     lMax=',
     +lnm-7,': '//wstr(i1+7:k);call putmess('S',6610,'Problem Reading',wch); goto 79999;endif;endif;endif
1035  wstr(j1:k)='d'; goto 1051;endif;call Find3Part(wstr,1,'>','=','NUM',j1,i,i1,k,w,lw);if(j1>0.and.i<=0)then; call Find3Part('>='
     l//wstr(j1+1:),1,'>','=','NUM',  j2,i,i1,k,w,lw);k=k+j1-2; if(i1>0)i1=i1+j1-2;endif;if(j1*i<=0) call Find3Part(wstr,1,'lower_bo
     lund','=','NUM',j1,i,i1,k,w,lw);if(j1*i<=0)then; bnds(0,nc)=-xhuge;else;if(i1<=0)then; goto 15555;else; if(lw)then; if(w<=-1d20
     l)w=-1d100; bnds(0,nc)=w; goto 103; endif;if(wstr(i1:k)=='-infinity')then; bnds(0,nc)=-xhuge;else;vector(0)=wstr(i1:k); bnds(0,
     lnc)=-xhuge/2.;if(k-i1+1>lnm) then;write(wch,'(a,i5)')'Length of vector name '//wstr(i1+7:k)//' exceeds Max=',lnm-7;call putmes
     ls('S',6613,'Problem Reading',wch); goto 79999;endif;endif;endif
103   wstr(j1:k)='d';endif;call Find3Part(wstr,1,'<','=','NUM',j1,i,i1,k,w,lw);if(j1>0.and.i<=0)then; call Find3Part('<='//wstr(j1+1
     l:),1,'<','=','NUM',  j2,i,i1,k,w,lw);k=k+j1-2; if(i1>0)i1=i1+j1-2;endif;if(j1*i<=0) call Find3Part(wstr,1,'upper_bound','=','N
     lUM',j1,i,i1,k,w,lw);if(j1*i<=0)then; bnds(1,nc)= xhuge;else;if(i1<=0)then; goto 15555;else; if(lw)then; if(w>=1d20)w=+1d100; b
     lnds(1,nc)=w; goto 105; endif;if(wstr(i1:k)=='infinity'.or.wstr(i1:k)=='+infinity')then; bnds(1,nc)=xhuge;else;vector(1)=wstr(i
     l1:k); bnds(1,nc)=+xhuge/2.;if(k-i1+1>lnm) then;write(wch,'(a,i5)')'Length of vector name '//wstr(i1+7:k)//' exceeds Max=',lnm-
     l7;call putmess('S',6615,'Problem Reading',wch); goto 79999;endif;endif;endif
105   wstr(j1:k)='d';endif
1051  continue;if(verify(wstr,' d,'//ch9)>0) goto 15555;GOTO 62
107   continue;if(wstr(:14)/='variables_list') goto 1071;wstr='';do while(wstr.eq.''.or.wstr(:1)=='%');call read_wstr(pChar,ibuff,ws
     ltr,lrest,ldb,iret);if(iret==2)then; goto 1001; elseif(iret==1)then; goto 79999; endif;enddo;j1=1;  if(wstr(:2)/='id')j1=0;i1=0
      do while(.true.);call read_wstr(pChar,ibuff,wstr,lrest,ldb,iret);if(iret==2)then; goto 1001; elseif(iret==1)then; goto 79999; 
      endif;if(wstr(:1).eq.''.or.wstr(:1)=='%')CYCLE;ch3='';if(j1==1)then; read(wstr,*,end=108,err=120) id,(ch3(i),i=1,3);else;     
          read(wstr,*,end=108,err=120) (ch3(i),i=1,3);endif
108   continue;ch3(1)=ADJUSTL(ch3(1));if(ch3(1)(:7)=='solver:'.or.ch3(1)(:8)=='matrixes'.or.ch3(1)(:8)=='matrices')GOTO 120;if(n1==n
     lmx) then;write(wch,'(a,i7)')'Number of variables in Problem Statement exceeds Max=',nmx;call putmess('S',663,'Problem Reading'
     l,wch); goto 79999;endif;j=1; if(ch3(j).eq.'') GOTO 115;if(ch3(j)/='-infinity') then;read(ch3(j),*,err=109)xlb(n1+1);endif;j=j+
     l1;goto 110
109   continue
110   if(ch3(j).eq.'-infinity'.or.ch3(j).eq.'infinity'.or.ch3(j).eq.'') GOTO 115;i1=i1+1;call checkXnames(ch3(j),n1,xname,i1,i);if(i
     l<=n1)then; wch='Input Point contains duplicate name of variable: '//trim(ch3(j));call putmess('S',664,'Problem Reading',wch); 
      goto 79999;else;call insertXname(trim(ch3(j)),i1,3,n1,xname,xlb,xub,ixord,i);endif;if(xname(i1)(:lnm-1)/=trim(ch3(j))) then;wr
     lite(wch,'(a,i5,a)')'Length (number of symbols) of variable name exceeds Max=',lnm-1,': '//trim(ch3(j));call putmess('S',665,'P
     lroblem Reading',wch); goto 79999;endif;j=j+1;if(.not.(ch3(j).eq.'infinity'.or.ch3(j).eq.'')) then;read(ch3(j),*,err=115) xub(i
     l1);endif;if(xlb(i1)>xub(i1)) then;write(wch,'(a)')'Upper bound is below lower bound for variable: '//trim(wstr);call putmess('
     lS',666,'Problem Reading',wch); goto 79999;endif;if(xlb(i1)>=xbndhuge.or.xub(i1)<=-xbndhuge) then;wch='Too small (large) Upper 
     l(Lower) bound for variable: '//trim(wstr)//'. It should be in range (-1e13, +1e13)';call putmess('S',667,'Problem Reading',wch
     l); goto 79999;endif;if(xlb(i1)<-xbndhuge)xlb(i1)=-xbndhuge; if(xub(i1)>xbndhuge) xub(i1)=xbndhuge;enddo
120   pwstr=''; GOTO 1072
115   continue;wch='Incorrect value of bound for variable: '//trim(wstr)//'. It should be in range (-1e13, +1e13)';call putmess('S',
     l669,'Problem Reading',wch); goto 79999
1071  continue;pwstr=''; ln=18;if(wstr(:17)/='box_of_variables:'.and.wstr(:17)/='box_of_variables ')then;if(wstr(:4)/='box:'.and.wst
     lr(:4)/='box ')then;if(wstr(:4)/='box='.and.wstr(:4)/='box<'.and.wstr(:4)/='box>')goto 1072; ln=4;else; ln=5;endif;endif;pwstr=
     lwstr; j=0; j1=0; j2=0; j3=0; j4=0;wstr=adjustl(wstr(ln:));call Find3Part('Equal'//wstr,1,'Equal','==','NUM',  j4,i0,i1,k,w,lw)
     l;if(i0<=0) call Find3Part('Equal'//wstr,1,'Equal','=','NUM',  j4,i0,i1,k,w,lw);if(i1>0)then; wstr(max(j4-5,1):k-5)='d'; else; 
      j4=0; endif;if(j4==0)then;call Find3Part(wstr,1,'>','=','NUM',  j,i0,i1,k,w,lw);if(j>0.and.i0<=0)then; call Find3Part('>='//ws
     ltr(j+1:),1,'>','=','NUM',  j2,i0,i1,k,w,lw);k=k+j-2;endif;if(i1>0)then; wstr(j:k)='d'; j2=0;else; call Find3Part(wstr,1,'lower
     lbounds','=','NUM',  j2,i0,i1,k,w,lw); j=j2; if(i1>0) wstr(j2:k)='d';endif;call Find3Part(wstr,1,'<','=','NUM', j1,i0,i1,k,w,lw
     l);if(j1>0.and.i0<=0)then; call Find3Part('<='//wstr(j1+1:),1,'<','=','NUM',  j3,i0,i1,k,w,lw);k=k+j1-2;endif;if(i1>0)then; wst
     lr(j1:k)='d'; j3=0;else; call Find3Part(wstr,1,'upperbounds','=','NUM', j3,i0,i1,k,w,lw); j1=j3; if(i1>0) wstr(j3:k)='d';endif;
      endif;call Find3Part(wstr,1,'types','=','?????',  i,i0,i1,k,w,lw);if(lw)then; TypesPointName=wstr(i1:k);  wstr(i:k)='d';endif;
      if(i0*i1>0.and..not.lw)then; wch='Box section of Problem Statement: incorrect type declaration: '//trim(wstr)//
     +'. It should be 0, 1, or 2, or vector of types';call putmess('S',670,'Problem Reading',wch); goto 79999;endif;call Find3Part(w
     lstr,1,'mip','=','?????',  i,i0,i1,k,w,lw);if(lw)then;if(wstr(i1:k)=='1')then; if(TypesPointName/='') TypesPointName='m&P#^I'//
     lTypesPointName;elseif(wstr(i1:k)=='0')then; if(TypesPointName/='') TypesPointName='m&P#^Z'//TypesPointName;else; goto 15555;en
     ldif;wstr(i:k)='d';else; if(i*i0>0)goto 15555;endif;if(verify(wstr,' d,'//ch9)>0) goto 15555;if(j<=0.and.j1<=0.and.j4<=0) pwstr
     l='';wstr=pwstr;if(pwstr=='') goto 1072;ch3='';if(j>0.and.j1>0.and.j1<j)then;if(j3>0)then; i=istr_without_sub(wstr,'upperbounds
     l',wstr);else;i=istr_without_sub(wstr,'<',wstr);endif;call SetXbounds('One',1,wstr,nmx,xbndhuge,
     +n1,xlb,xub,xname,ixord,  ch3(2),  allLbounds,allUbounds);if(ioutk>=istop-1) goto 79999;if(j2>0)then; i=istr_without_sub(wstr,'
     llowerbounds',wstr);else; i=istr_without_sub(wstr,'>',wstr);endif;call SetXbounds('One',0,wstr,nmx,xbndhuge,
     +n1,xlb,xub,xname,ixord,  ch3(1),  allLbounds,allUbounds);if(ioutk>=istop-1) goto 79999;else;if(j>0.or.j2>0)then;if(j2>0)then; 
      i=istr_without_sub(wstr,'lowerbounds',wstr);else; i=istr_without_sub(wstr,'>',wstr);endif;call SetXbounds('One',0,wstr,nmx,xbn
     ldhuge,
     +n1,xlb,xub,xname,ixord,  ch3(1),  allLbounds,allUbounds);if(ioutk>=istop-1) goto 79999;endif;if(j1>0.or.j3>0)then;if(j3>0)then
     l; i=istr_without_sub(wstr,'upperbounds',wstr);else; i=istr_without_sub(wstr,'<',wstr);endif;call SetXbounds('One',1,wstr,nmx,x
     lbndhuge,
     +n1,xlb,xub,xname,ixord,  ch3(2),  allLbounds,allUbounds);if(ioutk>=istop-1) goto 79999;endif;if(j4>0)then;i=istr_without_sub(w
     lstr,'=',wstr);call SetXbounds('One',10,wstr,nmx,xbndhuge,
     +n1,xlb,xub,xname,ixord,  ch3(1),  allLbounds,allUbounds);if(ioutk>=istop-1) goto 79999;ch3(2)=ch3(1);endif;endif;LowerPointNam
     le=ch3(1); UpperPointName=ch3(2);wstr=''
1072  continue;do while(wstr==''.or.wstr(:1)=='%'); call read_wstr(pChar,ibuff,wstr,lrest,ldb,iret);if(iret==2)then; goto 1001; else
     lif(iret==1)then; goto 79999; endif;enddo;if(wstr(:7)/='solver:'.and.wstr(:7)/='solver ') goto 1075;        pwstr=wstr;if(wstr(
     l:7)=='solver ') wstr(7:7)=':';call Find3Part(wstr,1,'solver',':','?????',      i,i0,i1,k,w,lw);if(lw)then; j=tqsol; tqsol=0;se
     llect case(wstr(i1:k));case('van'); tqsol=1; case('car'); tqsol=2; case('buldozer');tqsol=3; case('tank');tqsol=4;case('vangrb'
     l); tqsol=11; case('cargrb'); tqsol=21; case('heli'); tqsol=22;end select;if(tqsol>0)then; wstr(i:k)='d'; if(j==0)tqsol=0; else
     l; wstr(i:i0)='d'; tqsol=int(j,2); endif;elseif(i0>0)then; wstr(i:i0)='d';endif;if(lnrz(nc)==-3)then; lnrz(nc)=0;if(tqsol==3.an
     ld.nc==0) tqsol=31;endif;if(tqsol==11.or.tqsol==21.or.tqsol==22)then;i=-11;if(i<=0)then; wch='Gurobi is not installed. VanGrb, 
     lCarGrb and Heli solvers can not be used. Use another solver';if(i==-11)wch='This PSG version does not use Gurobi. VanGrb, CarG
     lrb, Heli solvers can not be used. Use another solver';call putmess('S',687,'Solver choice',wch); goto 79999;endif;endif;call F
     lind3Part(wstr,1,'init_point','=','NUM',  i,i0,i1,k,w,lw);if(i1>0)then; initpname=wstr(i1:k)//initpname(len_trim(initpname):); 
      wstr(i:k)='d';endif;call Find3Part(wstr,1,'precision','=','NUM',     i,i0,i1,k,w,lw);if(lw)then; i0=int(w); accur=(w-1.)/11.;i
     lf(i0/=w.or.accur<0d0.or.accur>1d0)then;wch='Solver section of Problem Statement: incorrect value of Precision parameter. Recom
     lended integer values from 1 to 9';call putmess('S',110,'Problem Reading',wch); goto 79999;endif;wstr(i:k)='d';endif;call Find3
     lPart(wstr,1,'stages','=','NUM',        i,i0,i1,k,w,lw);if(lw)then; kstage=int(w,2);if(kstage/=w.or.kstage<1.or.kstage>30)then;
      wch='Solver section of Problem Statement: incorrect value of Stages parameter. It should be integer from 1 to 30';call putmess
     l('S',111,'Problem Reading',wch); goto 79999;endif;wstr(i:k)='d';endif;call Find3Part(wstr,1,'timelimit','=','NUM',     i,i0,i1
     l,k,w,lw);if(lw)then; wstr(i:k)='d'; timelimit=w;if(timelimit<=0d0)then;wch='Solver section of Problem Statement: incorrect val
     lue of Timelimit parameter. It should be positive number';call putmess('S',112,'Problem Reading',wch); endif;endif;call Find3Pa
     lrt(wstr,1,'print','=','NUM',     i,i0,i1,k,w,lw);if(lw)then; wstr(i:k)='d'; iprntout=int(w);if(iprntout/=w.or.iprntout<0)then;
      wch='Solver section of Problem Statement: incorrect value of Print parameter. It should be integer nonnegative';call putmess('
     lS',118,'Problem Reading',wch);endif;endif;call Find3Part(wstr,1,'mipgap','=','NUM',     i,i0,i1,k,w,lw);if(lw)then; wstr(i:k)=
     l'd'; xmipgap=w;if(w<0.)then;wch='Solver section of Problem Statement: incorrect value of MipGap parameter. It should be nonneg
     lative';call putmess('S',120,'Problem Reading',wch);endif;endif;if(verify(wstr,' d,'//ch9)>0)then; wch='Solver section of Probl
     lem Statement: unknown string: '//trim(pwstr);call putmess('S',113,'Problem Reading',wch); goto 79999;endif;wstr=''
1075  continue;do while(wstr==''.or.wstr(:1)=='%'); call read_wstr(pChar,ibuff,wstr,lrest,ldb,iret);if(iret==2)then; goto 1001; else
     lif(iret==1)then; goto 79999; endif;enddo;if(wstr(:6)/='point:'.and.wstr(:6)/='point ') goto 1077; wstr(6:6)=':';call Find3Part
     l(wstr,1,'point',':','NUM',  i,i0,i1,k,w,lw);if(i1>0)then; initpname=wstr(i1:k)//initpname(len_trim(initpname):); wstr(i:k)='d'
      endif;if(verify(wstr,' d,'//ch9)>0)then; wch='Point section of Problem Statement: unknown string: '//trim(pwstr);call putmess(
     l'S',115,'Problem Reading',wch); goto 79999;endif;wstr=''
1077  continue;do while(wstr.eq.''.or.wstr(:1)=='%'); call read_wstr(pChar,ibuff,wstr,lrest,ldb,iret);if(iret==2)then; goto 1001; el
     lseif(iret==1)then; goto 79999; endif;enddo;if(icalc==-1)then;if(kconstr==mxconstr)then;write(wch,'(a,i5)')'Problem Statement: 
     lnumber of constraints exceeds Max=',mxconstr;call putmess('S',6571,'Problem Reading',wch); goto 79999;endif;icalc=kconstr; nc=
     lkconstr+1; kconstr=nc;write(cname(nc),'(a,i3.3)')'CaLcU@lAtE_',nc-icalc;vector='';endif;if(wstr(:6)=='value:'.or.wstr(:6)=='va
     llue ') wstr=trim(wstr(7:));if(wstr/='')GOTO 65;GOTO 62;do while(wstr==''.or.wstr(:1)=='%'); call read_wstr(pChar,ibuff,wstr,lr
     lest,ldb,iret);if(iret==2)then; goto 1001; elseif(iret==1)then; goto 79999; endif;enddo;wch='Unknown string after Problem State
     lment: '//trim(wstr);call putmess('S',114,'Problem Reading',wch); goto 79999
1001  continue;wza=0e0; i1=0; j1=0;if(lf19)then; if(lfza)then;rewind 25;read(25)wza(1),wza(2);close(25);lfza=.false.; endif;else;i=l
     len_trim(initpname); if(initpname(i:i)/=char(10))i1=-1;endif;wza(1)=i1; wza(2)=j1; w=wza(1)+wza(2);i=int(11.1*accur); qpEPS=1d1
     l**(-1-i+13*(i1+j1));if(not_task) then;wch='There are no elements of the problem: '//trim(probnm); call putmess('S',672,'Proble
     lm Reading',wch); goto 79999;else;if(lf21) write(21,'(a/)')'Info.   Problem Reading Finished by Row: '//trim(wstr);endif;if(iDB
     l<=0) close (19);ibuff=0;nc=kconstr;if(cname(nc)(:11)=='CaLcU@lAtE_'.and.kf(nc)==0)then;icalc=-1; cname(nc)=''; bnds(0,nc)=-xhu
     lge; bnds(1,nc)=xhuge;kconstr=nc-1;endif;do i=0,kconstr; id=0; j1=len_trim(cname(i));do j=1,j1;if(cname(i)(j:j)==' '.or.cname(i
     l)(j:j)==':')then; id=id+1;elseif(id>0)then; cname(i)(j-id:j-id)=cname(i)(j:j);endif;enddo;cname(i)=cname(i)(:j1-id);if(cname(i
     l)(:11)/='CaLcU@lAtE_')then; if(iCheckPSGName(trim(cname(i)),'Say')<0) goto 79999; endif;enddo;if(lf19)then;open(19,file=trim(w
     lorkpath)//'cnames.txt');write(19,'(a)')(cname(i),i=0,kconstr); close(19);deallocate(cname);endif;if(newin>1.and.lf21) write(21
     l,'(/////a/)')repeat('=',80);wch='Problem statement:  '//trim(probnm)//'  is processed';if(lf21) write(21,'(/a//a,t50,a/a,t50,i
     l5/a,t50,i5/a,99(/t50,a))')
     +trim(wch),
     +"Problem name           ",trim(probnm),
     +'Minimization(1)/maximization (-1)', int(sign_min),
     +'A number of input data sets is   ',knab,
     +"Input data set names are",(trim(fname(i)(:l16p)),i=1,knab);wch='YES';if(kf(0).le.0) wch='NO';if(lf21) write(21,'(a,t50,a/a,t5
     l0,i5/)')'Object function    ',trim(wch),
     +'A number of constraints is',kconstr;do nc=0,kconstr; if(bnds(0,nc)<-xhuge) bnds(0,nc)=-xhuge; if(bnds(1,nc)>xhuge) bnds(1,nc)
     l=xhuge;if(lf21) write(21,'(1p,a,i5/a,t50,2e15.7/a,t50,i5,999(/e15.7,a20,e15.7,a))')
     +'The constraint number ',nc,"Constraint bounds are ",(bnds(i,nc),i=0,1),
     +'A number of functions for this constraint is ',kf(nc),
     +(cf(i,nc),'  '//fnc_name(nf(i,nc)/10,nf(i,nc)-(nf(i,nc)/10)*10),
     +wf(i,nc),'  '//trim(fname(nnab(i,nc))(:l16p)),i=1,kf(nc));enddo;if(lf21) write(21,'(1p,a,i7,999(/i5,e20.7,a,a30,e15.7))')
     +'A number of variables is',n1,
     +(i,xlb(i),'              ',trim(xname(i)),xub(i),i=1,min0(n1,100)),
     +(i,xlb(i),'              ',trim(xname(i)),xub(i),i=max0(101,n1-100),n1);if(sign_min.lt.0.)then;do i=1,kf(0);  cf(i,0)=-cf(i,0)
     l; enddo;elseif(sign_min==0d0.and.tqsol==0)then;endif;kb(0)=0;bnds(1,0)=0d0;lconvex=.true.;do 2000 nc=0,kconstr;j=1; w=0.; if(k
     lf(nc)<=0) j=0;do i=1,kf(nc); w=w+abs(cf(i,nc)); enddo;if(w==0.) j=0;if(j==0)then;if(bnds(0,nc)>0d0.or.bnds(1,nc)<0d0)then; wri
     lte(wch,'(a,i5)')'Problem Statement: incorrect bound for constraint #',nc;if(lf21) write(21,'(/a/a,a,2e20.12)')' Program is sto
     lpped.',trim(wch),'. Bounds: ',bnds(0,nc),bnds(1,nc);call putmess('S',684,'Problem Analizing',wch); goto 79999;endif;if(cname(n
     lc)(:11)/='CaLcU@lAtE_'.and.sign_min/=0..and.it_id==0)then;if(nc==0)then; write(wch,'(a,i5,a)')'Objective does not have functio
     lns with nonzero coefficients';else; write(wch,'(a,i5,a)')'Constraint #',nc," does not have functions with nonzero coefficients
     l";endif;call putmess('W',0,'Problem Analizing',wch);  if(wch=='S') goto 79999;endif;kb(nc)=-1; goto 1800;endif;if(bnds(0,nc)==
     l-xhuge.and.bnds(1,nc)==xhuge) then;if(cname(nc)(:11)/='CaLcU@lAtE_')then; write(wch,'(a,i5,a)')'Constraint #',nc," does not ha
     lve active bounds";call putmess('W',0,'Problem Analizing',wch); if(wch=='S') goto 79999;endif;kb(nc)=-1; goto 1800;endif;do i=1
     l,kf(nc);if(nf(i,nc)>=1050.and.nf(i,nc)<=1051)then;w=cf(i,nc)*abs(sign_min);if((kf(nc)>1.or.nc>0).and.w/=0.or.w>0)then;wch='Pro
     lblem Statement: Likelihood function can be maximized as a single function in objective or can be calculated';call putmess('S',
     l6731,'Problem Analizing',wch); goto 79999;endif;endif;enddo;if(kf(nc)>1)then;do i=1,kf(nc);if((nf(i,nc)>=450.and.nf(i,nc)<=461
     l.or.
     +nf(i,nc)>=670.and.nf(i,nc)<=681.or.
     +nf(i,nc)>=850.and.nf(i,nc)<=861.or.
     +nf(i,nc)>=470.and.nf(i,nc)<=481.and.nc>0.or.
     +nf(i,nc)>=870.and.nf(i,nc)<=881.and.nc>0.or.
     +nf(i,nc)>=160.and.nf(i,nc)<=191.or.
     +nf(i,nc)>=1360.and.nf(i,nc)<=1371)
     +.and.cf(i,nc)/=0.)then;w=-cf(i,nc); do i1=1,kf(nc); w=w+abs(cf(i1,nc)); enddo;if(w/=0.)then; wch='Problem Statement: Probabili
     lty functions cannot be combined with other functions';call putmess('S',673,'Problem Analizing',wch); goto 79999;endif;endif;if
     l(nf(i,nc)==200.and.(nc>0.or.itnab(nnab(i,nc))>4).or.
     +nf(i,nc)==1400                             )then;wch='Problem Statement: LinearMulti function cannot be combined with other fu
     lnctions';call putmess('S',675,'Problem Analizing',wch); goto 79999;endif;enddo;elseif(kf(nc)==1.and.nf(1,nc)==200) then;if(nc=
     l=0.and.itnab(nnab(1,nc))/=4.and.itnab(nnab(1,nc))/=404) then;write(wch,'(a)')'LinearMulti function with bounds cannot be used 
     lin objective';call putmess('S',678,'Problem Analizing',wch); goto 79999;endif;if(nc>0.and.itnab(nnab(1,nc))==4) then;write(wch
     l,'(a,i5,a)')'Constraint #',nc,' including LinearMulti does not have Vector_bounds';call putmess('W',0,'Problem Analizing',wch)
     l; if(wch=='S') goto 79999;kb(nc)=-1; goto 1800;endif;endif;if(bnds(0,nc).gt.bnds(1,nc).or.bnds(0,nc).eq.xhuge.or.bnds(1,nc).eq
     l.-xhuge) then;write(wch,'(a,i5)')'Problem Statement: Lower bound > Upper bound for constraint #',nc;if(lf21) write(21,'(/a/a,a
     l,2e20.12)')' Program is stopped.',trim(wch),'. Bounds: ',bnds(0,nc),bnds(1,nc);call putmess('S',689,'Problem Analizing',wch); 
      goto 79999;endif;if(nc>0.and.kf(nc)==1.and.(    nf(1,nc)>=160.and.nf(1,nc)<=191
     +.or.nf(1,nc)>=450.and.nf(1,nc)<=481
     +.or.nf(1,nc)>=850.and.nf(1,nc)<=881
     +.or.nf(1,nc)>=670.and.nf(1,nc)<=681
     +.or.nf(1,nc)>=1360.and.nf(1,nc)<=1371
     +) ) then;if((cf(1,nc)<0d0.and.bnds(0,nc)<=cf(1,nc).and.bnds(1,nc)>=0d0)
     +.or.
     +(cf(1,nc)>0d0.and.bnds(1,nc)>=cf(1,nc).and.bnds(0,nc)<=0d0))then;write(wch,'(a,i5,a)')'Bound for constraint #',nc,' including 
     lPR_function is not active';call putmess('W',0,'Problem Analizing',wch); if(wch=='S') goto 79999;kb(nc)=-1; goto 1800;endif;if(
     l(cf(1,nc)<0d0.and.(bnds(1,nc)<cf(1,nc).or.bnds(0,nc)>0d0)).or.
     +(cf(1,nc)>0d0.and.(bnds(0,nc)>cf(1,nc).or.bnds(1,nc)<0d0))) then;write(wch,'(a,i5,a)')'Problem Statement: constraint #',nc," i
     ls infeasible";call putmess('S',681,'Problem Analizing',wch); goto 79999;endif;if(abs(sign_min)==1.)then;if((cf(1,nc)<0d0.and.b
     lnds(1,nc)<0d0.or.cf(1,nc)>0d0.and.bnds(0,nc)>0d0).and.nf(1,nc)<=191 ) then;write(wch,'(a,i5,a)')'Problem Statement: constraint
     l #',nc,
     +' is nonconvex. This constraint should be convex if it uses such Probability functions.';call putmess('S',686,'Problem Analizi
     lng',wch); goto 79999;endif;endif;endif;if(bnds(0,nc)>-xhuge.and.bnds(1,nc)<xhuge)then; j=1;do i=1,kf(nc);if(nf(i,nc)>=160.and.
     lnf(i,nc)<=191 .or.
     +nf(i,nc)>=450.and.nf(i,nc)<=481 .or.
     +nf(i,nc)>=850.and.nf(i,nc)<=881 .or.
     +nf(i,nc)>=670.and.nf(i,nc)<=681 .or.
     +nf(i,nc)>=1400.and.nf(i,nc)<=1400)    Cycle;if(
     +nf(i,nc)>=730.and.nf(i,nc)<=761 .or.
     +nf(i,nc)>=780.and.nf(i,nc)<=811
     +)then;write(wch,'(a,i5,a)')'Problem Statement: constraint #',nc,' cannot contain both Lower and Upper bounds';call putmess('S'
     l,682,'Problem analizing',wch); goto 79999;endif;itt=itnab(nnab(i,nc));if(abs(sign_min)==1.)then;if((nf(i,nc)>11.and.nf(i,nc)/=
     l200.and.(nf(i,nc)<770.or.nf(i,nc)>771))  .or.
     +(nf(i,nc)<=11.and.(itt==200.or.itt>=500.and.itt<600))     )then;write(wch,'(a,i5,a)')'Problem Statement: there are two bounds 
     lin constraint #',nc,'. Constraint may be non-convex';if(j==1)then; call putmess('W',0,'Problem analizing',wch); j=0; endif;lco
     lnvex=.false.;endif;endif;enddo;kb(nc)=2;if(nf(1,nc)>=160.and.nf(1,nc)<=191.or.nf(1,nc)>=450.and.nf(1,nc)<=481.or.nf(1,nc)>=850
     l.and.nf(1,nc)<=881.or.
     +nf(1,nc)>=670.and.nf(1,nc)<=681) kb(nc)=1;if(bnds(0,nc).eq.bnds(1,nc)) kb(nc)=20;if(nf(1,nc)==200) kb(nc)=1;if(nf(1,nc)==1400)
     l kb(nc)=2;else; j=1;do i=1,kf(nc); itt=itnab(nnab(i,nc));if(  (nf(i,nc)<=11.or.nf(i,nc)==200.or.770<=nf(i,nc).and.nf(i,nc)<=77
     l1) .and.
     +.not.(itt==200.or.itt>=500.and.itt<600)                   )   CYCLE;w=1d0;if((    nf(i,nc)>=440.and.nf(i,nc)<=481
     +.or.nf(i,nc)>=670.and.nf(i,nc)<=681
     +.or.nf(i,nc)>=750.and.nf(i,nc)<=761
     +.or.nf(i,nc)>=850.and.nf(i,nc)<=881
     +).and.cf(i,nc)/=0. )then; w=0d0; lconvex=.false.;endif;select case(nf(i,nc)); case(340:360,430,820,821,1350); w=-1d0; end sele
     lct;if(abs(sign_min)==1..and.it_id==0)then;if( ((cf(i,nc)*w<0d0.and.bnds(1,nc)< xhuge).or.(cf(i,nc)*w>0d0.and.bnds(0,nc)>-xhuge
     l)))then;if(
     +nf(i,nc)>=730.and.nf(i,nc)<=761 .or.
     +nf(i,nc)>=780.and.nf(i,nc)<=811
     +)then;write(wch,'(a,i5,a)')'Problem Statement: constraint #',nc,
     +' or objective (if 0) is nonconvex. It should be convex for used functions.';call putmess('S',683,'Problem analizing',wch); go
     lto 79999;endif;if(nf(i,nc)/=270)then;if(nc==0)then;  write(wch,'(a,i5,a)')'Problem Statement: objective may be non-convex';els
     le; write(wch,'(a,i5,a)')'Problem Statement: constraint #',nc,' may be non-convex';endif;if(j==1)then; call putmess('W',0,'Prob
     llem analizing',wch); j=0; endif;lconvex=.false.;endif;endif;endif;enddo;if(nc.ne.0) kb(nc)=1;endif;j=1;do i=1,kf(nc); if(abs(s
     lign_min)==1..and.it_id==0)then;if( ((cf(i,nc)*w<0d0.and.bnds(1,nc)< xhuge).or.(cf(i,nc)*w>0d0.and.bnds(0,nc)>-xhuge)))then;if(
     +nf(i,nc)>=280.and.nf(i,nc)<=330
     +)then; write(wch,'(a,i5,a)')'Problem Statement: constraint #',nc,
     +' uses functions indented to be used in convex programming';if(j==1)then; call putmess('W',0,'Problem analizing',wch); j=0; en
     ldif;endif;endif;endif; enddo
1800  continue;if((tqsol==11.or.tqsol==21.or.tqsol==22).and.lnrz(nc)==10)then;i1=kf(nc); iw=1; if(irec==1.or.itrn==1.or.ispl==1.or.i
     lExL==1) iw=i1+1;do i=iw,kf(nc);select case(nf(i,nc)); case(140:191, 280:330);i1=i1+1;if(i1>mxkf)then;write(wch,'(a)')'Internal
     l error: not enough space for additional functions';call putmess('S',6281,'Problem Reading',wch); goto 79999;endif;select case(
     lnf(i,nc));case(280:330); obname(i1,nc)='mIpFunCtIon_@ForCardG';   nf(i1,nc)=nf(i,nc)+1200;case(140:191); obname(i1,nc)='mIpFun
     lCtIon_@ForVArMpRo'; nf(i1,nc)=nf(i,nc)+1270;end select;cf(i1,nc)=cf(i,nc); nnab(i1,nc)=nnab(i,nc); wf(i1,nc)=wf(i,nc);cf(i,nc)
     l=0.;end select;enddo;if(kf(nc)==i1)then; write(wch,'(a,i7)')'MIP=1 option will mean Linearize=1 for constraint',nc;call putmes
     ls('W',0,'Problem Reading',wch);lnrz(nc)=1;else; kf(nc)=i1;endif;endif;if(kf(nc)==1.and.nf(1,nc)>=160.and.nf(1,nc)<=191.and. nc
     l>0.and.cf(1,nc)/=0..and. kb(nc)>=0) then;if(.not.((tqsol==11.or.tqsol==21.or.tqsol==22).and.lnrz(nc)==10)) then;kf(nc)=2; cf(2
     l,nc)=cf(1,nc);nnab(2,nc)=nnab(1,nc);if(cf(1,nc)>0) then;  wf(2,nc)=bnds(1,nc)/cf(1,nc); bnds(0,nc)=-xhuge;else; wf(2,nc)=bnds(
     l0,nc)/cf(1,nc); bnds(1,nc)=+xhuge;endif;if(mod(nf(1,nc),10)==0) wf(2,nc)=1d0-wf(2,nc);i=20; if(nf(1,nc)>=180)i=40; nf(2,nc)=nf
     l(1,nc)-i;endif;endif;do i=1,kf(nc); if(nf(i,nc)<730.or.761<nf(i,nc)) CYCLE;if(.false.)then;endif;if(.true.)then;i1=n1+1;write(
     lwch,'(a,i3.3,a,f9.7,a,i5.5)')'VaRiAbLeS_For_Var@Cvar_',nf(i,nc),'_',wf(i,nc),'_',nnab(i,nc);call checkXnames(wch(:lnm),n1,xnam
     le,i1,j);if(j>n1)then; call insertXname(wch(:lnm),i1,3,n1,xname,xlb,xub,ixord,j);endif;endif;enddo;do i=1,kf(nc); if(nf(i,nc)<7
     l80.or.nf(i,nc)>811) CYCLE;write(wstr,'(a,i4.4,a,f9.7,a,i5.5)')'VaRiAbLeS_For_Max@Cvar_',nf(i,nc),'_',wf(i,nc),'_',nnab(i,nc);l
     lh=len_trim(wstr);do k=1,knab; if(fname(k)==wstr(:lh)) EXIT;enddo;if(k>knab)then;knab=k; fname(k)=wstr(:lh); itnab(k)=-1; kmtrn
     l(k)=1;kmatr=kmatr+1; mname(kmatr)=wstr(:lh);kpmtr=kpmtr+1; nfmatr(k)=kpmtr; nmatr(kpmtr)=kmatr;do j=0,kmtrn(nnab(i,nc))-1; wch
     l=mname(nmatr(nfmatr(nnab(i,nc))+j)); lh=len_trim(wch);do k1=1,knab; if(fname(k1)==wch(:lh).and.itnab(k1)==-1.and.kmtrn(k1)==1)
     l EXIT;enddo;if(k1>knab)then; knab=k1; fname(k1)=wch(:lh); itnab(k1)=-1; kmtrn(k1)=1;kpmtr=kpmtr+1; nfmatr(k1)=kpmtr; nmatr(kpm
     ltr)=nmatr(nfmatr(nnab(i,nc))+j);endif;kconstr=kconstr+1; i1=kconstr; kb(i1)=1; bnds(0,i1)=-xhuge; bnds(1,i1)=0.;cname(i1)=wstr
     l; lnrz(i1)=lnrz(nc);kf(i1)=2; obname(1,i1)=wstr; obname(2,i1)=wstr;cf(1,i1)=+1.; nnab(1,i1)=k1; wf(1,i1)=wf(i,nc);if(780<=nf(i
     l,nc).and.nf(i,nc)<=791) nf(1,i1)=nf(i,nc)-760;if(800<=nf(i,nc).and.nf(i,nc)<=811) nf(1,i1)=nf(i,nc)-660;cf(2,i1)=-1.; nnab(2,i
     l1)= k; wf(2,i1)=0.;       nf(2,i1)=1;enddo;endif;j=kf(nc)+1; kf(nc)=j; obname(j,nc)=wstr;nf(j,nc)=1; wf(j,nc)=0.; nnab(j,nc)=k
      cf(j,nc)=cf(i,nc); cf(i,nc)=0.;enddo;do i=1,kf(nc);if(nf(i,nc)>=470.and.nf(i,nc)<=481.or.nf(i,nc)>=870.and.nf(i,nc)<=881)then;
      j1=0;if(it_id/=0.or.tf(i,nc)==2.or.tf(i,nc)==3.or.cf(i,nc)==0.) j1=1;do i1=1,kf(nc); if(i1/=i.and.cf(i1,nc)/=0.) j1=1; enddo;i
     lf(j1==0) Cycle;itt=itnab(nnab(i,nc))+1;ktmtr=kmtrn(nnab(i,nc)); id=nfmatr(nnab(i,nc))-1;do k=1,knab; if(kmtrn(k).ne.ktmtr.or.i
     ltt.ne.itnab(k)) cycle;do j=1,ktmtr; if(nmatr(id+j).ne.nmatr(nfmatr(k)-1+j)) EXIT;enddo;if(j>ktmtr) EXIT;enddo;if(k>knab)then;k
     lnab=k; fname(k)=fname(nnab(i,nc));kmtrn(k)=ktmtr; itnab(k)=itt; nnab(i,nc)=k;do j=1,ktmtr; nmatr(kpmtr+j)=nmatr(id+j);enddo;nf
     lmatr(k)=kpmtr+1; kpmtr=kpmtr+ktmtr;else; nnab(i,nc)=k;endif;endif;enddo
2000  enddo;bnds(1,0)=xhuge;if(.not.(tqsol==11.or.tqsol==21.or.tqsol==22))then;if(sign_min/=0.)then;if(maxval(lnrz(0:nc))==10)then;w
     lrite(wch,'(a)')'MIP=1 options will mean Linearize=1 because Gurobi subsolver is not used';call putmess('W',0,'Problem Reading'
     l,wch);endif;if(index(TypesPointName,'m&P#^')==1)then;if(index(TypesPointName,'m&P#^I')==1)then;write(wch,'(a)')'MIP=1 option i
     ln Box does not have effect because Gurobi subsolver is not used';call putmess('W',0,'Problem Reading',wch);endif;TypesPointNam
     le=TypesPointName(7:);endif;endif;else;if(TypesPointName/='')then;if(index(TypesPointName,'m&P#^')/=1)then; TypesPointName='m&P
     l#^'//TypesPointName;else;if(index(TypesPointName,'m&P#^Z')==1)then; TypesPointName=TypesPointName(7:);else; TypesPointName='m&
     lP#^'//TypesPointName(7:);endif;endif;endif;endif;do nc=0,kconstr;iw=0; nf1=-1; do i=1,kf(nc); if(cf(i,nc)/=0.)then; iw=iw+1; n
     lf1=nf(i,nc); ib=i; endif; enddo;if(sign_min>0..and.it_id==0.and.kb(nc)>=0.and.iw==1.and.nf1>=1360.and.nf1<=1371)then;j=int(sig
     ln(1.,cf(ib,nc))); j0=1; if(nf1==1361.or.nf1==1371) j0=-1;if(nc>0)then;if(bnds(1,nc)<xhuge)then;kconstr=kconstr+1; i1=kconstr; 
      kb(i1)=1; cname(i1)='cVaR_FoRbPoE_'; lnrz(i1)=lnrz(nc);i=1; kf(i1)=i; obname(i,i1)='cVaR_FoRbPoE_';cf(i,i1)=j; nnab(i,i1)=nnab
     l(ib,nc);nf(i,i1)=nf(ib,nc)-(1360-20);wf(i,i1)=1.-bnds(1,nc)/cf(ib,nc); bnds(1,i1)=wf(ib,nc)*j0*j;if(wf(i,i1)>1.-dconf1) wf(i,i
     l1)=1.-dconf1;  if(wf(i,i1)<dconf1) wf(i,i1)=dconf1;if(j0<0) wf(i,i1)=1.-wf(i,i1);endif;if(bnds(0,nc)>-xhuge)then;kconstr=kcons
     ltr+1; i1=kconstr; kb(i1)=1; cname(i1)='cVaR_FoRbPoE_'; lnrz(i1)=lnrz(nc);i=1; kf(i1)=i; obname(i,i1)='cVaR_FoRbPoE_';cf(i,i1)=
     lj; nnab(i,i1)=nnab(ib,nc);nf(i,i1)=nf(ib,nc)-(1360-20);wf(i,i1)=1.-bnds(0,nc)/cf(ib,nc); bnds(0,i1)=wf(ib,nc)*j0*j;if(wf(i,i1)
     l>1.-dconf1) wf(i,i1)=1.-dconf1;  if(wf(i,i1)<dconf1) wf(i,i1)=dconf1;if(j0<0) wf(i,i1)=1.-wf(i,i1);endif;kb(nc)=-1;elseif(nc==
     l0)then; iw=1;if(TypesPointName/='')iw=0;do i1=1,kconstr; do i=1,kf(i1);select case(nf(i,i1)); case(:11,200);case default; if(c
     lf(i,i1)/=0.)iw=0;end select;enddo; enddo;if(iw==1.and.(nf1==1360.or.nf1==1361).and.itnab(nnab(ib,nc))/=202)then; i1=0; i=kf(i1
     l)+1;kf(i1)=i; obname(i,i1)='PM_FoRbPoE_';cf(i,i1)=cf(ib,i1); nnab(i,i1)=nnab(ib,nc);nf(i,i1)=nf(ib,nc)-(1360-70);wf(i,i1)=wf(i
     lb,i1);write(obname(ib,i1)(lnm-21:),'(e22.16)') wf(ib,i1);cf(ib,i1)=0.; wf(ib,i1)=0.;k0=nnab(ib,nc); j=1;do iw=1,kconstr;do j=1
     l,kf(iw); if(nnab(j,iw)==k0.and..not.(iw==nc.and.j==1)) Exit; enddo;if(j<=kf(iw))then; j=0; Exit; endif;enddo;if(j<=0)then;k=kn
     lab+1; knab=k; nnab(i,i1)=k; nnab(ib,i1)=k;fname(k)='i'//trim(fname(k0)); itnab(k)=itnab(k0); kmtrn(k)=kmtrn(k0);do iw=1,kmtrn(
     lk);nmatr(kpmtr+iw)=kmatr+iw;mname(kmatr+iw)='i'//trim(mname(nmatr(nfmatr(k0)+iw-1)));enddo;nfmatr(k)=kpmtr+1; kpmtr=kpmtr+kmtr
     ln(k);kmatr=kmatr+kmtrn(k);endif;write(wch,'(a)')'VaRiAbLe_additional_LAMbda_For_Bpoe_';lh=len_trim(wch);k=knab+1; klamb=k;knab
     l=k; fname(k)=wch(:lh); itnab(k)=-1; kmtrn(k)=1;kmatr=kmatr+1; mname(kmatr)=wch(:lh);kpmtr=kpmtr+1; nfmatr(k)=kpmtr; nmatr(kpmt
     lr)=kmatr;i=3; kf(i1)=i; obname(i,i1)='VaRiAbLe_additional_LAMbda_For_Bpoe_';cf(i,i1)=0.; nnab(i,i1)=knab; nf(i,i1)=1;do i1=1,k
     lconstr; do j=1,kf(i1);select case(nf(j,i1));case(0:11);if(bnds(1,i1)<xhuge)then;i=kf(i1)+1; kf(i1)=i; obname(i,i1)='VaRiAbLe_a
     ldditional_LAMbda_For_Bpoe_';cf(i,i1)=-bnds(1,i1); nnab(i,i1)=klamb; nf(i,i1)=1; bnds(1,i1)=0.;if(kb(i1)==2)then; kb(i1)=1; els
     leif(kb(i1)==20)then; bnds(0,i1)=0.; endif;endif;if(bnds(0,i1)>-xhuge.and.kb(i1)/=20)then; i=kf(i1);if(obname(i,i1)=='VaRiAbLe_
     ladditional_LAMbda_For_Bpoe_')then;kconstr=kconstr+1; i2=kconstr;kb(i2)=1; cname(i2)='VaRiAbLe_additional_LAMbda_For_Bpoe_'; ln
     lrz(i2)=lnrz(i1); kf(i2)=i; bnds(1,i2)=0.;obname(:i,i2)=obname(:i,i1); cf(:i,i2)=cf(:i,i1); nnab(:i,i2)=nnab(:i,i1); nf(:i,i2)=
     lnf(:i,i1);cf(i,i1)=-bnds(0,i1);elseif(bnds(0,nc)/=0.)then;i=kf(i1)+1; kf(i1)=i; obname(i,i1)='VaRiAbLe_additional_LAMbda_For_B
     lpoe_';cf(i,i1)=-bnds(0,i1); nnab(i,i1)=klamb; nf(i,i1)=1; bnds(0,i1)=0.;endif;endif;case(200);k0=nnab(j,i1);if(itnab(nnab(j,i1
     l))==7.and.nmatr(nfmatr(k0)+1)/=nmatr(nfmatr(k0)+2))then; k=knab+1;knab=k; fname(k)='i'//trim(fname(k0)); itnab(k)=6; kmtrn(k)=
     l2;nfmatr(k)=kpmtr+1; kpmtr=kpmtr+2;nmatr(kpmtr-1)=kmatr+1;nmatr(kpmtr)=nmatr(nfmatr(k0)+2);kmtrn(k0)=2; kb(i1)=1;kmatr=kmatr+1
     l; mname(kmatr)='i'//trim(mname(nmatr(nfmatr(k0))));i=1;kconstr=kconstr+1; i2=kconstr; kb(i2)=1; cname(i2)='VaRiAbLe_additional
     l_LAMbda_For_Bpoe_'; lnrz(i2)=lnrz(i1);kf(i2)=i; obname(:i,i2)=obname(:i,i1); cf(:i,i2)=cf(:i,i1); nf(:i,i2)=nf(:i,i1);nnab(:i,
     li2)=knab;endif;end select;enddo; enddo;k=knab+1;knab=k; write(fname(k),'(a,e22.15,i5.4)')'ipmatrix_box_for_bPOE_,vector#',0.,k
     lconstr+1;nfmatr(k)=kpmtr+1; itnab(k)=6; kmtrn(k)=2;kpmtr=kpmtr+2; nmatr(kpmtr-1)=kmatr+1; nmatr(kpmtr)=kmatr+2;kmatr=kmatr+1; 
      mname(kmatr)='ipmatrix_box_for_bPOE_';kmatr=kmatr+1; write(mname(kmatr),'(a,e22.15,i5.4)')'vector#',0.,kconstr+1;i=1; kconstr=
     lkconstr+1;i2=kconstr; kb(i2)=1; cname(i2)='ipmatrix_box_for_bPOE_'; lnrz(i2)=1; kf(i2)=i; bnds(1,i2)=xhuge/2.;obname(:i,i2)='i
     lpmatrix_box_for_bPOE_'; cf(:i,i2)=1; nf(:i,i2)=200; nnab(:i,i2)=knab;else; i1=0; i=kf(i1)+1;kf(i1)=i; obname(i,i1)='cVaR_FoRbP
     loE_';cf(i,i1)=cf(ib,i1); nnab(i,i1)=nnab(ib,nc);nf(i,i1)=nf(ib,nc)-(1360-20);wf(i,i1)=0.5;if(j0<0) wf(i,i1)=1.-wf(i,i1);cf(ib,
     li1)=0.;write(wch,'(a,i5,a)')'Cannot convert bPOE function to Partial Moment';call putmess('W',0,'Problem analizing',wch);endif
      endif;endif;enddo;do nc=0,kconstr;do i=1,kf(nc); if(itnab(nnab(i,nc))<500.or.itnab(nnab(i,nc))>=600) Cycle; nf1=nf(i,nc);selec
     lt case(nf1); case default; Cycle;case(160:171); if(sign_min==0.or.cf(i,nc)==0.) Cycle;case(20:31,140:151);end select;i1=n1+1;w
     lrite(wch,'(a,i4.4,a,e20.14,a,i5.5,a)')'VaRiAbLeS_For_CvarL1L2_',nf(i,nc),'_',wf(i,nc),'_',nnab(i,nc),'_1';call checkXnames(wch
     l(:lnm),n1,xname,i1,j);if(j>n1) call insertXname(wch(:lnm),i1,3,n1,xname,xlb,xub,ixord,j);if(140<=nf1.and.nf1<=151)then;i1=n1+1
      write(wch,'(a,i4.4,a,e20.14,a,i5.5,a)')'VaRiAbLeS_For_CvarL1L2_',nf(i,nc),'_',wf(i,nc),'_',nnab(i,nc),'_2';call checkXnames(wc
     lh(:lnm),n1,xname,i1,j);if(j>n1) call insertXname(wch(:lnm),i1,3,n1,xname,xlb,xub,ixord,j);endif;enddo;enddo;k1=knab;do k=1,kma
     ltr; if(mname(k)(:7)/='takein('.and.mname(k)(:7)/='cutout(') Cycle;i1=kmatr+1; mname(i1)='';call CutTakeParameters(mname(k),0, 
     l i,j,mname(i1),i0,iret);if(iret==1) goto 79999;do i=1,kmatr; if(mname(i)==mname(i1)) goto 2100; enddo;kmatr=i1;if(k1==knab)the
     ln; k1=k1+1; itnab(k1)=9999; kmtrn(k1)=1; nfmatr(k1)=kpmtr+1;fname(k1)=trim(mname(i1));else; kmtrn(k1)=kmtrn(k1)+1; fname(k1)=t
     lrim(fname(k1))//','//trim(mname(i1));endif;kpmtr=kpmtr+1; nmatr(kpmtr)=kmatr;
2100  enddo;if(k1>knab)then;kconstr=kconstr+1; i1=kconstr; kb(i1)=-1; bnds(0,i1)=-xhuge; bnds(1,i1)=xhuge;cname(i1)='FoRmAlForCuToUt
     lTaKeIn_'; lnrz(i1)=-1;kf(i1)=1; obname(1,i1)='FoRmAlForCuToUtTaKeIn_';cf(1,i1)=0.; nnab(1,i1)= k1; wf(1,i1)=0.; nf(1,i1)=kfn*1
     l0+1;endif;knab=k1;k1=knab;do k=1,k1; if(index(fname(k),'spline_sum')<=0) Cycle;knab=knab+1;fname(knab)='$wO#rK@_,'//fname(k)(s
     lcan(fname(k),'(')+1:scan(fname(k),')',.true.)-1);itnab(knab)=440; kmtrn(knab)=kmtrn(k);if(itnab(k)==413) itnab(knab)=443;nfmat
     lr(knab)=kpmtr+1; nmatr(kpmtr+1)=kmatr+1;do i=2,kmtrn(k); nmatr(kpmtr+i)=nmatr(nfmatr(k)+i-1); enddo;kpmtr=kpmtr+kmtrn(k);kmatr
     l=kmatr+1; write(mname(kmatr),'(a,e15.8,2i6.5)')'NuMbEr#',-1.,440,knab;kconstr=kconstr+1; i1=kconstr; kb(i1)=20; bnds(0,i1)=0.;
       bnds(1,i1)=0.;cname(i1)='constraint_for_smoothing_spline';do i=0,i1-1; do j=1,kf(i); if(nnab(j,i)==k)EXIT; enddo;if(j<=kf(i))
     lEXIT;enddo;if(i<=i1-1) lnrz(i1)=lnrz(i);kf(i1)=1; obname(1,i1)='$wO#rK@_spline_type_';cf(1,i1)=1.; nnab(1,i1)=knab; wf(1,i1)=0
     l.; nf(1,i1)=1400;if(cf(j,i)==0..and.bnds(0,i)==-xhuge.and.bnds(1,i)==xhuge)then;knab=knab-1; kpmtr=kpmtr-kmtrn(k); kmatr=kmatr
     l-1; kconstr=kconstr-1;cf(j,i)=1.;endif;kmatr=kmatr+1; mname(kmatr)=MnameFromCutTake('i',mname(nmatr(nfmatr(k)+2)),'knots');do 
     li=1,kmatr-1; if(mname(i)==mname(kmatr)) EXIT; enddo;if(i<kmatr)then; kmatr=kmatr-1;write(wch,'(a)')'Spline_sum functions use m
     latrices of knots with the same name but functions may need different knots';call putmess('W',0,'Problem analizing',wch);endif;
      kmatr=kmatr+1; mname(kmatr)=MnameFromCutTake('i',mname(nmatr(nfmatr(k)+2)),'quant');do i=1,kmatr-1; if(mname(i)==mname(kmatr))
     l EXIT; enddo;if(i<kmatr) kmatr=kmatr-1;enddo;k1=knab;do k=1,k1; if(.not.(index(fname(k),'tsp')>0.and.itnab(k)==450)) Cycle;sel
     lect case(tqsol); case(:0,11,21);case default; wch='Problem statement: TSP function can be optimized only by Vabgrb or Cargrb s
     lolvers';call putmess('S',6380,'Problem Analizing',wch); goto 79999;end select;knab=knab+1;kmatr=kmatr+1; write(mname(kmatr),'(
     la,e22.15,i5.4)')'vector#',2.,kconstr+1;fname(knab)='$wO#rK@_,'//trim(mname(kmatr))//','//trim(mname(kmatr));itnab(knab)=7; kmt
     lrn(knab)=3;kpmtr=kpmtr+1; nfmatr(knab)=kpmtr;nmatr(kpmtr)=nmatr(nfmatr(k)); kpmtr=kpmtr+2; nmatr(kpmtr-1)=kmatr; nmatr(kpmtr)=
     lkmatr;kconstr=kconstr+1; i1=kconstr; kb(i1)=1; bnds(0,i1)=1.; bnds(1,i1)=1.;cname(i1)='constraint_incidence_matrix_for_TSP';do
     l i=0,i1-1; do j=1,kf(i); if(nnab(j,i)==k)EXIT; enddo;if(j<=kf(i))EXIT;enddo;if(i<=i1-1) lnrz(i1)=lnrz(i);kf(i1)=1; obname(1,i1
     l)='$wO#rK@_incidence_matrix';cf(1,i1)=1.; nnab(1,i1)=knab; wf(1,i1)=0.; nf(1,i1)=200;w=0.;do i=0,kconstr; do j=1,kf(i);if(nnab
     l(j,i)==k)then; nf(j,i)=0;if(w==0.)then; w=wf(j,i);elseif(wf(j,i)/=w)then;wch='Problem statement: tsp functions with the same m
     latrix can not have different parameters';call putmess('S',6382,'Problem Analizing',wch); goto 79999;endif;wf(j,i)=0;endif;endd
     lo; enddo;knab=knab+1; fname(knab)='$wO#rK@_,Ax>=2_'//fname(k)(scan(fname(k),'(')+1:scan(fname(k),')',.true.)-1);itnab(knab)=45
     l0; kmtrn(knab)=kmtrn(k);nfmatr(knab)=nfmatr(k);kconstr=kconstr+1; i1=kconstr; kb(i1)=1; bnds(0,i1)=-xhuge; bnds(1,i1)=0.;cname
     l(i1)='constraint_Ax>=2_for_TSP';kf(i1)=1; obname(1,i1)='$wO#rK@_Ax>=2_';cf(1,i1)=1.; nnab(1,i1)=knab; wf(1,i1)=w; nf(1,i1)=119
     l1;itnab(k)=-1; kmtrn(k)=1;enddo;k1=knab;do k=1,k1; if(itnab(k)/=34) Cycle;knab=knab+1; nfmatr(knab)=nfmatr(k);fname(knab)=mnam
     le(nmatr(nfmatr(k)));itnab(knab)=-1; kmtrn(knab)=1;kconstr=kconstr+1; i1=kconstr; kb(i1)=20; bnds(0,i1)=1.; bnds(1,i1)=1.;cname
     l(i1)='sum(p_i)=1';kf(i1)=1; obname(1,i1)='$wO#rK@_linear_sum(p_i)';cf(1,i1)=1.; nnab(1,i1)=knab; wf(1,i1)=0.; nf(1,i1)=0;enddo
      k1=knab;do k=1,k1; if(itnab(k)/=29) Cycle;knab=knab+1;kmatr=kmatr+1; write(mname(kmatr),'(a,e22.15,i5.4)')'vector#',1.,kconstr
     l+1;fname(knab)='$wO#rK@_,'//trim(mname(kmatr))//','//trim(mname(kmatr));itnab(knab)=7; kmtrn(knab)=3;kpmtr=kpmtr+1; nfmatr(kna
     lb)=kpmtr;nmatr(kpmtr)=nmatr(nfmatr(k)); kpmtr=kpmtr+2; nmatr(kpmtr-1)=kmatr; nmatr(kpmtr)=kmatr;kconstr=kconstr+1; i1=kconstr;
       kb(i1)=1; bnds(0:1,i1)=0.;cname(i1)='sum_of_probabilities_for_states';kf(i1)=1; obname(1,i1)='$wO#rK@_linearmulti_HMM_sum(p_i
     l)';cf(1,i1)=1.; nnab(1,i1)=knab; wf(1,i1)=0.; nf(1,i1)=200;enddo;do i=1,n1; if(index(xname(i)(:23),'VaRiAbLeS_For_Var@Cvar_')<
     l=0) Cycle;read(xname(i)(24:26),*)j;select case(j); case(750:761);nc=kconstr+1; kconstr=nc; kb(nc)=1; cname(nc)=xname(i);bnds(0
     l,nc)=-xhuge; read(xname(i)(28:36),*) wf(1,nc); bnds(1,nc)=wf(1,nc);if(j==750.or.j==760) bnds(1,nc)=1.-bnds(1,nc);kf(nc)=1; cf(
     l1,nc)=1d0; nf(1,nc)=j-40;read(xname(i)(38:44),*) nnab(1,nc); obname(1,nc)=xname(i);end select;enddo;if(TypesPointName/=''.and.
     lTypesPointName/='0')then;knab=knab+2; fname(knab-1)='TyPesPoIntDeFine'; fname(knab)='TyPesPoIntDeFine';itnab(knab-1)=12; itnab
     l(knab)=-1;kpmtr=kpmtr+2;nfmatr(knab-1)=kpmtr-1; nfmatr(knab)=kpmtr;kmtrn(knab-1)=1; kmtrn(knab)=1;kmatr=kmatr+1; Mname(kmatr)=
     l'TyPesPoIntDeFine';nmatr(kpmtr-1)=kmatr; nmatr(kpmtr)=kmatr;nc=kconstr+1; kconstr=nc; kb(nc)=1; cname(nc)='TyPesPoIntDeFine';b
     lnds(0,nc)=-xhuge; bnds(1,nc)=0d0;kf(nc)=2;nf(1,nc)=280; nnab(1,nc)=knab-1; cf(1,nc)=1d0; wf(1,nc)=1d-5; obname(1,nc)='TyPesPoI
     lntDeFine';nf(2,nc)=0;   nnab(2,nc)=knab;   cf(2,nc)=-1d0;               obname(2,nc)='TyPesPoIntDeFine';endif;nz0=0;do nc=0,kc
     lonstr;nz0=nz0+kf(nc);enddo;if(sign_min==0.) lnrz=-1;if(allocated(vector))deallocate(vector,stat=i); iostat=iostat+i;if(allocat
     led(ch3  )) deallocate(ch3,  stat=i); iostat=iostat+i;if(allocated(chm  )) deallocate(chm  ,stat=i); iostat=iostat+i;if(allocat
     led(wprm )) deallocate(wprm, stat=i); iostat=iostat+i;ibuff=0;RETURN;ENTRY READ_TASK_2(sign_min,kconstr,n1,knab,nmx,l16p,nz0,km
     latr,kpmtr,
     +nfn,wfn,ncn,cfn,tfn,      nfz,       xlb,xub, jaddr3,
     +kb0,bnds0,fname0,mname0,kmtrn0,nfmatr0,itnab0,nmatr0,   lnrz0);iw=0;if(lf19)then; open(19,file=trim(workpath)//'obnames.txt');
      else; j=0;do i=0,kconstr;if(cname(i)=='')then;if(i>0)then; write(cname(i),*)i; cname(i)='constraint_'//trim(adjustl(cname(i)))
      else; cname(i)='objective';endif;endif;j=j+lnm+1;enddo;jaddr3(1,1)=malloc(j); jaddr3(2,1)=j; k=1;do i=0,kconstr; call savestr(
     lloc(cname(i)),jaddr3(1,1),k,  int(lnm,llen),int(j,llen) ); enddo;endif;if(.not.lf19)then;kfunc=sum(kf(0:kconstr)); iw=4*kfunc;
      id=(Lnm+1)*kfunc; jaddr=malloc(id+iw);endif;nz=0; itt=1;do k=1,knab;nfz(k)=nz+1;do j0=0,kfn; do j1=0,1;j=j0*10+j1;do nc=kconst
     lr,0,-1;do i=1,kf(nc);if(nnab(i,nc).eq.k.and.nf(i,nc).eq.j) then;nz=nz+1;nfn(nz)=j; wfn(nz)=wf(i,nc);ncn(nz)=nc; cfn(nz)=cf(i,n
     lc); tfn(nz)=tf(i,nc);if(lf19)then; write(19,'(a)') obname(i,nc);else; call savestr(loc(obname(i,nc)),jaddr,itt,  int(lnm,llen)
     l,int(id,llen) );call copybuff(loc(ipfunc(i,nc)),4,jaddr+id+(nz-1)*4,4);endif;endif;enddo;enddo;enddo; enddo;enddo;nfz(knab+1)=
     lnz+1;if(lf19)then; close(19); deallocate(obname);else; itt=itt-1+iw; jaddr3(1,2)=malloc(itt); jaddr3(2,2)=itt;call copybuff(ja
     lddr,itt,jaddr3(1,2),itt); call free(jaddr); jaddr=0;endif;do inz=1,nz;if(kb(ncn(inz))<0.or.cfn(inz)==0d0) nfn(inz)=-nfn(inz);e
     lnddo;j=-1;do i=0,kconstr;kb0(i)=kb(i); bnds0(0,i)=bnds(0,i); bnds0(1,i)=bnds(1,i); lnrz0(i)=lnrz(i); if(lnrz(i)>0)lnrz0(i)=1;i
     lf(kb(i)>j) j=kb(i);enddo;if(j<0.and.sign_min/=0.) then;write(wch,'(a)')'Absent problem components';call putmess('W',0,'Problem
     l Analizing',wch); if(wch=='S') goto 79999;endif;j=len(fname0(1));do i=1,knab; nfmatr0(i)=nfmatr(i); kmtrn0(i)=kmtrn(i); itnab0
     l(i)=itnab(i);fname0(i)=fname(i)(:l16p); if(fname0(i)/=fname(i)(:l16p)) fname0(i)(j-2:)='...';if(itnab(i)==15)then;j0=index(fna
     lme(i)(:l16p),'('); j1=index(fname(i)(:l16p),')'); if(j1>j0+1)fname(i)(:l16p)=fname(i)(j0+1:j1-1);endif;enddo;nfmatr0(i)=kpmtr+
     l1;if(lf19)then;open(19,file=trim(workpath)//'fnames.txt');write(19,'(a)')(trim(fname(i)(:l16p)),i=1,knab); close(19);else; j=0
      do i=1,knab; j=j+len_trim(fname(i)(:l16p))+1; enddo;jaddr3(1,3)=malloc(j); jaddr3(2,3)=j; k=1;do i=1,knab; iw=len_trim(fname(i
     l)(:l16p)); wstr=fname(i)(:iw);call savestr(loc(wstr),jaddr3(1,3),k,    int(iw,llen),int(j,llen) );enddo;endif;do i=1,kpmtr; nm
     latr0(i)=nmatr(i);enddo;do i=1,kmatr; mname0(i)=mname(i);enddo
79999 continue;ENTRY DEALL_TASK(l16p)
      if(jaddr/=0)then; call free(jaddr); jaddr=0; endif;iostat=0; i=0;if(allocated(kf   )) deallocate(kf   ,stat=i); iostat=iostat+
     li;if(allocated(cf   )) deallocate(cf   ,stat=i); iostat=iostat+i;if(allocated(tf   )) deallocate(tf   ,stat=i); iostat=iostat+
     li;if(allocated(nf   )) deallocate(nf   ,stat=i); iostat=iostat+i;if(allocated(wf   )) deallocate(wf   ,stat=i); iostat=iostat+
     li;if(allocated(nnab )) deallocate(nnab ,stat=i); iostat=iostat+i;if(allocated(obname))deallocate(obname,stat=i); iostat=iostat
     l+i;if(allocated(cname)) deallocate(cname,stat=i); iostat=iostat+i;if(allocated(lnrz)) deallocate(lnrz,stat=i); iostat=iostat+i
      if(allocated(kb   )) deallocate(kb   ,stat=i); iostat=iostat+i;if(allocated(bnds )) deallocate(bnds ,stat=i); iostat=iostat+i;
      if(allocated(fname)) deallocate(fname,stat=i); iostat=iostat+i;if(allocated(mname)) deallocate(mname,stat=i); iostat=iostat+i;
      if(allocated(kmtrn)) deallocate(kmtrn,stat=i); iostat=iostat+i;if(allocated(nfmatr))deallocate(nfmatr,stat=i); iostat=iostat+i
      if(allocated(ipfunc)) deallocate(ipfunc,stat=i); iostat=iostat+i;if(allocated(itnab)) deallocate(itnab,stat=i); iostat=iostat+
     li;if(allocated(nmatr)) deallocate(nmatr,stat=i); iostat=iostat+i;ideall=iostat;if(iostat.ne.0)then; wch="Can not deallocate al
     ll"; call putmess('W',0,'DEALL_TASK',wch);endif;RETURN;END SUBROUTINE READ_TASK;character(lnm) function MnameFromCutTake(pre,ws
     ltr,str1);use ModCommons;character(*) pre,wstr,str1; integer(4)  i,j,i1,id,iw;character(lnm)  wch; real(8) w;if(index(wstr,'cut
     lout')<=0.and.index(wstr,'takein')<=0)then;if(pre=='i')then; MnameFromCutTake='i'//trim(wstr)//'_'//trim(str1);else; MnameFromC
     lutTake=trim(wstr)//'_'//trim(str1);endif;else;i=index(wstr,'matrix'); j=i+index(wstr(i+1:),')');if(j==i)then; j=len(wstr);else
     l; i1=index(wstr,'('); call IsARealNumber(wstr(i1+1:),id,iw,w); if(id>0)then; iw=int(w); write(wch,'(i9)')iw; endif;endif;if(pr
     le=='i')then; MnameFromCutTake='i'//trim(wstr(i:j-1))//'_'//trim(str1);else; MnameFromCutTake=trim(wstr(i:j-1))//'_'//trim(str1
     l);endif;if(id>0)then;if(index(wstr,'cutout')>0)MnameFromCutTake=trim(MnameFromCutTake)//'_in';if(index(wstr,'takein')>0)MnameF
     lromCutTake=trim(MnameFromCutTake)//'_out';endif;endif;end function MnameFromCutTake;integer(4) function istr_without_sub(wstr,
     lsubstr,rstr);character(*) wstr,substr,rstr;integer(4)  i,j;i=index(wstr,substr);if(i.le.0) then;rstr=wstr;else;     i=i+len(su
     lbstr);j=verify(wstr(i:),' ');if(j>0)then; i=i+j-1; rstr=wstr(i:);else; rstr=''; i=len(wstr)+1;endif;endif;istr_without_sub=i;r
     leturn;end;subroutine read_wstr           (pChar,ibuff,  wstr,  lrest,  ldb,iret);use CiFort; use ModCommons;integer(plen) pCha
     lr;character(*) wstr; logical ldb;integer(4) l16p,ibuff,lrest; integer(4)  i,j,iret;iret=0;l16p=len(wstr);if(ldb)then; call Get
     lRow(loc(pChar),wstr,lrest,    int(ibuff,llen),int(l16p,llen) );if(ioutk>=istop) goto 79999;  if(lrest<=0) goto 20;else;read(pC
     lhar,'(a)',end=20) wstr;endif;j=scan(wstr,'%')-1; if(j<0)then; j=len_trim(wstr); else; wstr(j+1:)=''; endif;j=verify(wstr(:j),'
     l '//char(9),.true.); wstr(j+1:)='';i=j;do while(i>0); i=scan(wstr(:i),char(9),.true.); if(i>0)wstr(i:i)=' '; enddo;wstr=adjust
     ll(wstr);if(j>0) call stringtosmall(wstr(:j));RETURN
20    iret=2; return
79999 iret=1; return;end subroutine read_wstr;subroutine GetRow(buff,
     +wstr,
     +i );integer(4) i,i1,j;character(*) buff,wstr,wch*125;i1=verify(buff(i:),' '//char(9));  if(i1>1) i=i+i1-1;i1=SCAN (buff(i:),ch
     lar(10));if(i1<=0)then; i1=scan(buff(i:),char(0)); if(i1<=0)i1=len_trim(buff(i:));if(i1<=0)then; i=i1; RETURN; endif;endif;j=i1
     l-1+i-1;if(j<i)then; wstr='';else; if(buff(j:j)==char(13)) j=j-1; wstr=buff(i:j);endif;i=i+i1; j=len(wstr);if(i1-1>j)then; wch=
     lwstr(1:min(125,j));write(wstr,'(a,i5,a,i5,a)')'Length (number of symbols) ',i,' of some row exceeds Max_length = ',i,': '//tri
     lm(wch)//'...';call putmess('S',601,'Row Reading',wstr);endif;return;end subroutine GetRow;subroutine MaxRowBuff_00(pChar,ibuff
     l,     krows,lrowm,kzp,km,kc);use CiFort;integer(plen) pChar;integer(4) ibuff,krows,lrowm,kzp,km,kc;call MaxRowBuff(pChar,   kr
     lows,lrowm,kzp,km,kc,   int(ibuff,llen) );end;subroutine IsARealNumber(wstr,
     +IsNo,in,wr);USE CiFort;character(*) wstr; integer(4) in; real(8) wr;integer(4) isno, i,j,k,il,iw; character chw*128,ch1,ch11*1
     l1; data ch11/'0123456789.'/; save ch11;wr=1.; j=1; in=1; IsNo=0;il=len_trim(wstr); if(il<=0) Return;do i=1,il; if(wstr(i:i)/='
     l ')Exit; enddo;if(wstr(i:i)=='-')then; j=-1; i=i+1; elseif(wstr(i:i)=='+')then; i=i+1; endif;in=i; if(i>il) goto 30;do k=i,il;
       if(wstr(k:k)/=' ')Exit; enddo;if(k>il)then; k=0; else; k=k-i+1; endif;if(k<=0) goto 30;call copybuff(loc(wstr)+i+k-1-1,1,loc(
     lch1),1);do iw=1,11; if(ch11(iw:iw)==ch1)Exit; enddo;if(iw>11) goto 30;read(wstr(i:),*,err=30,end=30) wr;IsNo=1;read(wstr(i:),*
     l)chw; in=i+index(wstr(i:),trim(chw))-1+len_trim(chw);if(in>il) goto 30
30    if(j<0) wr=-wr;end subroutine IsARealNumber;subroutine SeparateNumber(wstr,
     +IsNo,wr);character(*) wstr; integer(4) isno,in,i; real(8) wr;in=0;call IsARealNumber(wstr, IsNo,in,wr);if(isno==1)then; i=veri
     lfy(wstr(in:)," ,("); wstr=wstr(in+i-1:);endif;end subroutine SeparateNumber;subroutine NextWord(wstr,delim,
     +ip,    word,ch1);character(*) wstr,delim,word,ch1;integer(4) ip; integer(4)  i;i=verify(wstr(ip:),' '//delim);if(i<=0)then; wo
     lrd=''; ch1='';ip=-1; RETURN;endif;ip=ip+i-1; i=scan(wstr(ip:),delim);if(i<=0)then; word=wstr(ip:); ch1=''; ip=0;else; word=wst
     lr(ip:ip+i-2); ip=ip+i; ch1=wstr(ip-1:ip-1);endif;end subroutine NextWord;subroutine Find3Part(wstr,j0,word1,word2,word3,
     +j1,j2,j3,jk,wr,FindPart);character(*) wstr,word1,word2,word3; integer(4) j0,j1,j2,j3,jk;logical FindPart; real(8) wr;   intege
     lr(4) ln,i0,i,iw;FindPart=.false.; j1=0; j2=0; j3=0; jk=0; wr=0.; ln=len(wstr); i0=j0
10    if(i0>ln.or.i0<1) RETURN;i=index(wstr(i0:),word1)-1; if(i<0) RETURN;j1=i0+i; iw=j1+len(word1); if(iw>ln) RETURN;if(i0>1)then; 
      i=scan(wstr(i0-1:i0-1),' ,:');if(i<=0)then; i0=iw+1; goto 10; endif;endif;i=verify(wstr(iw:),' ')-1; if(i<0) RETURN;j2=iw+i; i
     lw=j2+len(word2); jk=iw-1; if(jk>ln)RETURN;if(wstr(j2:jk)/=word2)then; j2=0; RETURN; endif;if(word3=='')then;if(iw>ln)then; Fin
     ldPart=.true.;elseif(scan(wstr(iw:iw),' ,')==1)then; FindPart=.true.;endif;elseif(word3=='?')then;if(iw>ln)then; FindPart=.true
     l.; RETURN; endif;i=verify(wstr(iw:),' ')-1;if(i<0)then; FindPart=.true.; j3=iw; jk=ln; RETURN;elseif(wstr(iw+i:iw+i)==',')then
     l; FindPart=.true.; j3=iw; jk=iw+i-1; RETURN;else;j3=iw+i; i=scan(wstr(j3:),' ,')-1;if(i>0)then; jk=j3+i-1; FindPart=.true.;els
     leif(i<0)then; jk=len(wstr); FindPart=.true.;endif;endif;elseif(word3=='?????'.or.word3=='NUM')then;i=verify(wstr(iw:),' ')-1; 
      if(i<0)RETURN;j3=iw+i; i=scan(wstr(j3:),' ,')-1;if(i>0)then; jk=j3+i-1; FindPart=.true.;elseif(i<0)then; jk=len(wstr); FindPar
     lt=.true.;endif;if(FindPart.and.word3=='NUM')then;call IsARealNumber(wstr(j3:), iw,i,wr);if(iw>0)then; jk=j3+i-2; else; FindPar
     lt=.false.;endif;endif;else;i=verify(wstr(iw:),' ')-1; if(i<0)RETURN;j3=iw+i; iw=j3+len(word3); jk=iw-1; if(jk>ln)RETURN;if(wst
     lr(j3:jk)==word3)then;if(iw>ln)then; FindPart=.true.;elseif(scan(wstr(iw:iw),' ,')==1)then; FindPart=.true.;endif;endif;endif;e
     lnd subroutine Find3Part;subroutine checkXnames(chm,n1,xname,
     +jn,
     +j);use ModCommons;integer(4) n1,j,jn; character(*) xname(-3:*),chm, ch1*(lnm), ch2*(lnm); logical lnext;integer(4) j1,j2,j3;do
     l j=-3,0; if(xname(j)==chm) Exit; enddo;if(j<=0)then; jn=j; goto 10; endif;goto 11;do j=1,n1; if(xname(j)==chm) Exit; enddo;jn=
     lj; goto 10
11    ch1=chm; ch1=adjustr(ch1);j=n1+1; j1=1; j3=n1;if(n1>0)then; if(jn>n1.or.jn<1) jn=n1; ch2=xname(jn); ch2=adjustr(ch2);if(ch1==c
     lh2)then; j=jn; goto 10; elseif(ch1>ch2)then;j1=jn+1; elseif(ch1<ch2)then; j3=jn-1; endif;endif;lnext=.true.;if(j1<j3) then;do 
     lwhile(lnext);j2=(j1+j3)/2; if(j2==j1)lnext=.false.;ch2=xname(j2); ch2=adjustr(ch2);if(ch1<ch2)then; j3=j2;elseif(ch1>ch2)then;
       j1=j2;else; jn=j2; j=jn; Exit;endif;enddo;if(j>n1)then;if(j3==j1)then; jn=j1;else; ch2=xname(j3); ch2=adjustr(ch2);if(ch1<ch2
     l)then; jn=j3; elseif(ch1>ch2)then;jn=j3+1; else; jn=j3; j=jn; endif;endif;endif;elseif(j1>j3)then; jn=j3+1;else;ch2=xname(j3);
       ch2=adjustr(ch2);if(ch1<ch2)then; jn=j3; elseif(ch1>ch2)then;jn=j3+1; else; jn=j3; j=jn; endif;endif;goto 10;if(j1>j3)then; j
     ln=j3+1;else; lnext=.true.;do while(lnext);j2=(j1+j3)/2; if(j2==j1)lnext=.false.;ch2=xname(j2); ch2=adjustr(ch2);if(ch1<ch2)the
     ln; j3=j2-1; elseif(ch1>ch2)then; j1=j2+1; else; jn=j2; j=jn; goto 10; endif;enddo;if(j3/=j1)then; jn=j1;else;  ch2=xname(j3); 
      ch2=adjustr(ch2);if(ch1<ch2)then; jn=j3; elseif(ch1>ch2)then;jn=j3+1; else; jn=j3; j=jn; endif;endif;endif
10    return;end subroutine checkXnames;subroutine savestr(name,   jaddr,ibfree);integer(4) ibfree; character(*) jaddr, name;jaddr(i
     lbfree:)=name//char(10); ibfree=ibfree+len(name)+1;end subroutine;character(l16kmax) function move_space(str0);use modcommons;c
     lharacter(*) str0; integer(4)  i,j,jm,ln;character(26)  sset;sset='(){}[]*!/=\,%|:'//char( 39)//char(10)//char(13)//char( 9)//c
     lhar( 0)//';"^?-+';jm=0; i=1; ln=len_trim(str0)+1; move_space='';do while(i<ln);i=i+verify(str0(i:),' ')-1; j=scan(str0(i:),' '
     l);if(j<=0) j=len(str0(i:))+1;if(jm>0)then; if( scan(sset,move_space(jm:jm))==0 .and. scan(sset,str0(i:i))==0) jm=jm+1; endif;m
     love_space(jm+1:)=str0(i:i-1+j-1); jm=jm+j-1; i=i+j-1;enddo;end function move_space;subroutine sub_move_space(str0);character(*
     l) str0; integer(4)  i,j,jm,ln;character(26)  sset;sset='(){}[]*!/=\,%|:'//char( 39)//char(10)//char(13)//char( 9)//char( 0)//'
     l;"^?-+';jm=0; i=1; ln=len_trim(str0)+1;do while(i<ln);i=i+verify(str0(i:),' ')-1; j=scan(str0(i:),' ')-1;if(j<0) j=len(str0(i:
     l));if(jm>0)then; if( scan(sset,str0(jm:jm))==0 .and. scan(sset,str0(i:i))==0) jm=jm+1; endif;str0(jm+1:jm+j)=str0(i:i-1+j); jm
     l=jm+j; i=i+j;enddo;str0(jm+1:)='';end subroutine sub_move_space;integer(4) function IsL1L2Summ(strn);character(*) strn;integer
     l(4) iz,i0,i1,i2,i3,k3,ip,m1b,m1k,m2b,m2k;real(8) w; logical lok; character word*128, ch1*1;IsL1L2Summ=0; iz=1;call Find3Part(s
     ltrn,1,'l','(','?????',i0,i2,m1b,k3,w,lok); if(.not.lok) Return;if(i0>1)then; if(strn(i0-1:i0-1)=='-') iz=2; endif;m1k=m1b+1;ca
     lll NextWord(strn,')', m1k, word,ch1); if(m1k<=0) Return;m1k=m1k-2;call Find3Part(strn(m1k+2:),1,'+','l','?????',i1,i2,i3,k3,w,
     llok);if(.not.lok)then; iz=-iz;call Find3Part(strn(m1k+2:),1,'-','l','?????',i1,i2,i3,k3,w,lok); if(.not.lok) Return;endif;ip=i
     l1+m1k+2;call Find3Part(strn(ip:),1,'l','(','?????',i1,i2,m2b,k3,w,lok); if(.not.lok) Return;m2b=m2b+ip-1; m2k=m2b+1;call NextW
     lord(strn,')', m2k, word,ch1); if(m2k<=0) Return;m2k=m2k-2;IsL1L2Summ=iz;end function IsL1L2Summ;subroutine CompleteThreePoints
     l(str,strw);character(*) str,strw; integer(4) iaddlength,iaddmatr,ipos1,ipos2,nmtr;character(256)  word1,word2,wd;integer(4) nu
     lm1,num2,i,j,i1,i2,i3,icheck, ip1,ip2;icheck=0; goto 10;ENTRY GetNthMatrix(str,nmtr,      strw)
      icheck=-1; strw=''; goto 10;ENTRY CheckFirstThreePoints(str,      iaddlength,iaddmatr)
      iaddlength=0; iaddmatr=0;icheck=1; goto 10;ENTRY FindFirstThreePointsBlock(str,      ipos1,ipos2)
      icheck=2; ipos1=0; ipos2=0
10    continue;i3=0;word1=''; word2='';i=index(str,',...,'); if(i<=0) RETURN; i3=i;j=scan(str(:i-1),'(,*)-+',.true.)+1; word1=str(j:
     li-1); ip1=j; if(len_trim(word1)<=0) goto 79999;j=scan(str(i+5:)//' ','(,*)-+ ')+i+4; word2=str(i+5:j-1); ip2=j-1; if(len_trim(
     lword1)<=0) goto 79999;word1(len(word1):)=''; word2(len(word2):)='';i=0
20    continue;j=scan(word1(i+1:),'0123456789')+i; if(j<=i) goto 79999;if(word1(:j-1)/=word2(:j-1)) goto 79999;i1=verify(word1(j+1:)
     l,'0123456789')+j-1;read(word1(j:i1),*,err=79999) num1;if(scan(word2(j:),'0123456789')/=1) goto 79999;i2=verify(word2(j+1:),'01
     l23456789')+j-1;read(word2(j:i2),*,err=79999) num2;if(num1>num2) goto 79999;if(word1(i1+1:)/=word2(i2+1:))then;if(num1==num2.an
     ld.i1==i2)then; i=i1; goto 20;else;  goto 79999;endif;endif;if(icheck==2)then; ipos1=ip1; ipos2=ip2; RETURN; endif;word1=word1(
     l:j-1); word2=word2(i2+1:);if(icheck==1)then;write(wd,'(i10)')num2-1; wd=adjustl(wd);iaddmatr=(num2-1-(num1+1))+1;if(iaddmatr<=
     l0)then; iaddlength=1;else; iaddlength=len_trim(trim(word1)//trim(wd)//trim(word2)//',')*iaddmatr;endif;RETURN;endif;strw='';if
     l(icheck==-1)then;if(nmtr<1.or.nmtr>num2-num1+1)then;strw='Internal error_3  using ,...,. Nmtr is incorrect in string:'//trim(s
     ltr);call putmess('E',6426,'Three points sub',trim(strw)); goto 79999;endif;i=num1+nmtr-1; write(wd,'(i10)')i; wd=adjustl(wd);s
     ltrw=trim(strw)//trim(word1)//trim(wd)//trim(word2);RETURN;endif;do i=num1+1,num2-1; write(wd,'(i10)')i; wd=adjustl(wd);if(len(
     lstrw)<len(trim(strw)//trim(word1)//trim(wd)//trim(word2)//','))then;strw='Internal error_1 using ,..., in string:'//trim(str);
       call putmess('E',6423,'Three points sub',trim(strw));goto 79999;endif;strw=trim(strw)//trim(word1)//trim(wd)//trim(word2)//',
     l';enddo;if(len(str)<len(str(:i3)//trim(strw)//trim(str(i3+5:))))then;strw='Internal error_2 using ,..., in string:'//trim(str)
     l; call putmess('E',6425,'Three points sub',trim(strw));goto 79999;endif;if(num1<num2)then; str=str(:i3)//trim(strw)//trim(str(
     li3+5:));else;str=str(:i3-1)//str(ip2+1:);endif;goto 10;RETURN
79999 continue;if(icheck==0)then;strw='Incorrect using ,..., in: '//trim(str); call putmess('W',0,'Three points sub',strw);elseif(ic
     lheck==1)then; iaddmatr=-2;elseif(icheck==2)then; ipos1=-1; ipos2=-1;endif;
      end subroutine CompleteThreePoints
