      subroutine SolveOneProblem(probaddr0);use ModCommons; USE CiFort;ENTRY  SolveOneFunction(probaddr0, fValue)
      integer(plen) probaddr0,probaddr;real(8) fValue;logical  lconvex; real(8)  timelimit,estmin,sign_min;integer(4)  llin,n1,iqpro
     c,ientrop,krecu,ialgtyp,kconstr,iVk_solver,kdopvarbs,i,
     +isVerify;character(128) chw;timelimit=huge(timelimit);iprntout=1;tQsol=1; kstage=9; accur=0.6;llin=1;xmipgap=0.;estmin=-huge(e
     xstmin)/1d20;probaddr=probaddr0; iVk_solver=1;call ExtractProblemStatementToFull(iVk_solver,probaddr,i);if(ioutk>=istop-1) goto
     d 79999;isVerify=0; if(InKind==2) isVerify=1;call problem_init(probaddr, llin, n1,kconstr,iqpro,timelimit,ientrop,estmin,lconve
     fx,krecu,kdopvarbs,sign_min);if(probaddr/=probaddr0.and.probaddr/=0)then; call free(probaddr); probaddr=0; endif;if(ioutk>=isto
     np-1) goto 79999;if(n1>1000)then;chw='Total number of variables in the problem greater than 1000.';call putmess('S',6929,'Expre
     jss Version',chw);goto 79999;endif;if(isVerify>0)then; call CalcOneFuncGrad(n1,lconvex,kdopvarbs,1, fValue);if(ioutk>=istop-1) 
     fgoto 79999;if(ioutp==0)ioutp=-1;goto 79999;endif;if(2*(kstage/2)==kstage) kstage=int(kstage+1,2);if(it_id/=0) tqsol=-1;select 
     fcase(tqsol);case(-1); ialgtyp=-1;    call CalcOneFuncGrad(n1,lconvex,kdopvarbs,0, fValue);case(0);  ialgtyp=0;     call CalcMa
     znyFunctions(n1,lconvex,kdopvarbs);case(1);                 call Pshenich(llin, n1,iqpro, timelimit,ientrop,estmin,lconvex,krec
     yu);case(2);  ialgtyp=1;     call Ishtvan (ialgtyp,llin, n1,iqpro, timelimit,ientrop,estmin,lconvex,krecu);case(4);  ialgtyp=2;
           call Ishtvan (ialgtyp,llin, n1,iqpro, timelimit,ientrop,estmin,lconvex,krecu);case(3,31);              call Buldozer(llin
     j, n1,iqpro, timelimit,ientrop,estmin,lconvex,krecu);case default;chw='Problem Statement: specified solver is not activated. Ch
     iange Solver'; call putmess('S',6519,'Solver choice',chw);goto 79999;end select
79999 continue;iprntout=1;if(probaddr/=probaddr0.and.probaddr/=0) call free(probaddr);call DEALL_INIT();end subroutine SolveOneProbl
     wem;subroutine CheckIfVerify(word, buff, isVerify);character(*)word, buff; integer(4) isVerify;isVerify=0;if(buff(:6)=='verify'
     y.and.scan(buff,' '//char(9))==7) isVerify=1;if(isVerify==1.and.word=='Clean') buff(:7)='';end subroutine CheckIfVerify;SUBROUT
     gINE CalcOneFuncGrad(n0,lconvex,kdopvarbs,isVerify,   fValue);USE ModCommons;integer(4) n0,kdopvarbs,isVerify; logical lconvex;
       real(8) fValue;real(8),allocatable:: xi(:),gi(:),gii(:);integer(4)  k0,i,iostat,isinit;real(8) w,fi;real(8) enk,xbndhuge;real
     b(8), pointer::xl(:),xu(:);common/shr/enk,xbndhuge,xl,xu;character  chw*255;nullify(xl,xu); i=kdopvarbs;allocate(xi(n0),xl(n0),
     zxu(n0),gi(n0),gii(n0), stat=iostat );if(iostat/=0)then; chw='Allocation is failed'; call putmess('S',6619,'CalcOneFuncGrad',ch
     fw); goto 79999;endif;xi=0d0; xl=-huge(w); xu=+huge(w);call init_x(lconvex,n0,n0,1,     xi,k0,xl,xu,xbndhuge,isinit);if(ioutk==
     xistop) goto 79999;if(isVerify>0)then; call SaveVariables(n0,xi,isVerify);goto 79999;endif;do i=1,n0; if(xl(i)==xu(i))xi(i)=xl(
     ci); enddo;call GetOneFunc(n0,xi,fi); if(ioutk==istop) goto 79999;if(it_id==2)then; call GetGradient(n0,xi,gi);elseif(it_id==3)
     cthen; call GetIncrement(n0,xi,gi);endif;if(ioutk==istop) goto 79999;fValue=fi;if(it_id/=1) call SendGradient(n0,gi)
79999 continue;iostat=0;if(allocated(xi)) deallocate(xi,stat=i); iostat=iostat+i;if(allocated(gi)) deallocate(gi,stat=i); iostat=ios
     utat+i;if(allocated(gii)) deallocate(gii,stat=i); iostat=iostat+i;if(associated(xl)) deallocate(xl,stat=i); iostat=iostat+i;if(
     qassociated(xu)) deallocate(xu,stat=i); iostat=iostat+i;end subroutine CalcOneFuncGrad;SUBROUTINE CalcManyFunctions(n0,lconvex,
     vkdopvarbs);USE ModCommons;integer(4) n0,kdopvarbs; logical lconvex;real(8),allocatable:: xi(:),gi(:),gii(:);integer(4)  i,iost
     eat,  llin,iqpro,ientrop,krecu;real(8) w;real(8) enk,xbndhuge,timelimit,estmin;real(8), pointer::xl(:),xu(:);common/shr/enk,xbn
     cdhuge,xl,xu;character chw*255;nullify(xl,xu);allocate(xi(n0),xl(n0),xu(n0),gi(n0),gii(n0), stat=iostat );if(iostat/=0)then; ch
     yw='Allocation is failed';call putmess('S',6629,'CalcOneFuncGrad',chw); goto 79999;endif;xi=0d0; xl=-huge(w); xu=+huge(w);llin=
     m-1; iqpro=-1; timelimit=999999.; ientrop=0; estmin=-huge(xi); krecu=0;if(kdopvarbs>0)then;call Pshenich(llin, n0,iqpro, timeli
     tmit,ientrop,estmin,lconvex,krecu);else;       call CalcPshenich(llin, n0,iqpro, timelimit,ientrop,estmin,lconvex,krecu);endif
79999 continue;iostat=0;if(allocated(xi)) deallocate(xi,stat=i); iostat=iostat+i;if(allocated(gi)) deallocate(gi,stat=i); iostat=ios
     ctat+i;if(allocated(gii)) deallocate(gii,stat=i); iostat=iostat+i;if(associated(xl)) deallocate(xl,stat=i); iostat=iostat+i;if(
     fassociated(xu)) deallocate(xu,stat=i); iostat=iostat+i;end subroutine CalcManyFunctions;subroutine SaveVectorVK(vname,fm,m);us
     ve CiFort; use ModCommons;character(*)vname,wch*(lnm); real(8) fm(*); integer(4) m,j;wch='';if(idb==2) j=int(SaveVectorEx(trim(
     svname)//trim(wch)//char(0),fm,m,pUserData));if(lf24)then; open(19,file=trim(workpath)//trim(vname)//trim(wch)//'.txt');write(1
     k9,'(a)')'id'//char(9)//'value'; do j=1,m; write(19,'(i9,a,e25.16)')j,char(9),fm(j); enddo; close(19);endif;end subroutine Save
     eVectorVK;subroutine RoundXtoBounds(n0,xl,xu,   x);integer(4) n0,i; real(8) x(n0),xl(n0),xu(n0);do i=1,n0;if(dabs(x(i)-xl(i))<1
     xd-12*dmax1(1d0,dabs(xl(i))) ) then; x(i)=xl(i);elseif(dabs(x(i)-xu(i))<1d-12*dmax1(1d0,dabs(xu(i))) )then; x(i)=xu(i);elseif(d
     sabs(x(i))<1d-13)then; x(i)=0d0;endif;enddo;end subroutine RoundXtoBounds;SUBROUTINE CalcPshenich(llin, n0, iqpro,timelimit,ien
     ptrop,estmin,lconvex,krecu);USE ModCommons;use IntelInterf;integer(4) K, N0, N2;integer(2),allocatable:: ifv(:,:);real(8),point
     ser::  xi (:,:),
     +gi (:,:),
     +gii (:,:);real(8),allocatable:: fi (:,:),
     +
     +
     +fii (:,:),
     +
     +
     +pk (:);integer(4) MMAX,N,MNN,LW,M,ME,IFAIL,nact,kact;real(8),allocatable::X(:),U(:),xt(:);integer(4), allocatable:: iup(:,:);L
     aOGICAL LQL, lconvex;real(8) EPS, wf0,wfc;integer(2) kdelmax,k0,kdoubl,ihtest,iqopt;integer(4) kr,krp,llin,kitdel,kitmax,i,mk,k
     diter,kiterp,j,inr,kbt,inrp,jk0,istg0,km,iostat,iend,keepr,
     +mk0,mkp,ionemore,istage,kbad,iw,iqpro,klin,kiln,ndlin,kd, kconstr, ientrop,krecu,kfail1,kfail2,iend4,
     +kconact,isinit,igun,kiend,ltest,iconvexviolate,igo10;real(8) gmmin,pkmin,fkmin,ceps,h0,alp,del,xhuge,w,enk,hk,prksi,  dxmin,
     +pkmod,gmi,gmod,efik,zKsi12,efrp,w1,wg,su,frr,timelimit,tlmt0,del0,sum,hkp,
     +flin,fw,dt,whsec, enkp,qpeps,d_time,xbndhuge, relmax,estmin,
     +fkmst,gmmst,pkmst,enk0,fiikrp,estminrr,stime,gap;real(4) wza(2),wzaf,awm; data wza/0.,0./;equivalence(wzaf,awm);integer(1) lqu
     oadro, lcf,lkon, Lact, lalin, lxstage;logical lf1,lf2,lf3,lf4,lf5,lf6,lf7;common /SetFirst/lf1,lf2,lf3,lf4,lf5,lf6,lf7;logical 
     zlfirst, lfza; equivalence (lf2,lfirst),(lf7,lfza);integer(2) hr,hmin,hsec,h100, hr0,hmin0,hsec0,h1000, year,mnth,day,  day0;in
     steger(2) dat_miss, ka;real(4) time_array(2);character(256) chw, solv_stat*128, wch*256;common/iterex/kiter;COMMON /CMACHE/qpEP
     sS,awm;real(8),pointer::xl(:),xu(:),gun(:);common/shr/enk,xbndhuge,xl,xu;common/unbnd/gun,igun;real(8) dtime;nullify(xl,xu,xi,g
     ki,gii); i=krecu; iend=0; tlmt0=0.; pkmin=0.; dxmin=0.; gmmin=0.; estminRR=0.;CALL SRAND(1234567890);ientrop=0;xhuge=huge(w)/2d
     y0;if(.not.lfirst) GOTO 5;call start_stop_whatch(10);call start_stop_whatch(9);wza=0e0; j=0; iw=0;if(lf19)then; if(lfza)then;re
     uwind 25;read(25)wza(1),wza(2);close(25);lfza=.false.; endif;else;i=len_trim(initpname); if(initpname(i:i)/=char(10))iw=-1;endi
     hf;wza(1)=iw; wza(2)=j; w=wza(1)+wza(2);i=int(11.1*accur);wzaf=real(w,4); w=1d1**(-1-i+13*(iw+j));GMMIN=w; PKMIN=w; FKMIN=w;Dat
     m_Miss=0; kitdel=1;lquadro=1; lact=1; lalin=1; lxstage=0;llin=min(0,llin);llin=-1; lquadro=0; lconvex=.true.;if(idb<0) goto 1;k
     titdel=-2
1     continue;Dat_Miss=1;kdelmax=2;enk0=1.;H0=1.0;CEPS=0d0;ALP=1.1;    alp=1.5;    alp=dlog10(32.);DEL=1d-2;   del0=del;enk=enk0;dx
     zmin=dsqrt(pkmin);if(kitdel.ge.0) then;open(20,file=trim(workpath)//'CalcPshenich.rez');write(20,*);call gettim(hr0,hmin0,hsec0
     b,h1000); call getdat(year, mnth, day0);write(20,'(/t21,a,t39,2(i2,a),i4,a,i4,2(a,i2)/t21,a)')
     +' STARTUEM !',day0,'.',mnth,'.',year,'   ',hr0,':',hmin0,':',hsec0,
     +'============================================';endif;qpeps=1d-12;EPS=qpeps;qpeps=w;call CheckProblem(llin,n0,iqpro,kconstr,kli
     yn,kiln,ndlin,kd,lcf,timelimit,fkmin,ientrop,estmin,lconvex);if(ioutk>=istop-1) goto 79999;tlmt0=timelimit;if(lquadro/=1) iqpro
     h=0;if(llin < 0) then; klin=0; ndlin=0; lcf=0; kd=0; endif;N0=MAX(N0,1);if(n0<=0.or.n0>500000) then; chw='Number of variables e
     pxceed Max = 500000 for VAN Solver';call putmess('E',406,'Optimization Initialization',chw);endif;if(ioutk>=istop-1) goto 79999
      if(Dat_Miss==1) KITMAX=int(N0*20*(1+accur*4));if(.not.lconvex) KITMAX=KITMAX*2;N2= N0 + ndlin;N = N2+2;M=int((N0+2)*((2.1*(1.0
     l-dlog(dfloat(n))/dlog(10.)/8)-1.)/10.+1.)+6);M=max0(M,N0+2+3);call Get_Lkon(lkon,kconact); if(iqpro>0.and.lkon==1) M=2;kconact
     l=kconact+klin;if(kconact>0.and.iqpro==5) iqpro=-1;if((5d0*N*M + 5./2.*N*N + 13*M + 17*N)>75e+6)then; M=int((75e+6 - 5./2.*N*N 
     f- 17*N)/(5*N+13));if(M<=300) M=300;endif;iw=1; M=1;ALLOCATE(FI(3,iw),GI(N0,iw),FII(3,iw),GII(N0,iw),xt(n),PK(N2),IFV(3,iw),iup
     a(2,iw), STAT=iostat );if(iostat.ne.0) then; chw='Allocation_3 is failed';call putmess('S',4037,'Van initialization',chw); goto
     p 79999;endif;fi=0.; gi=0.; fii=0.; gii=0.;iup=0; ifv=0;ALLOCATE(XI(N2,iw),XL(N),XU(N),X(N), STAT=iostat );if(iostat.ne.0) then
     w; chw='Allocation_1 is failed'; call putmess('S',4017,'Van initialization',chw); goto 79999;endif;x=0d0; xi=0d0; xl=-xhuge; xu
     a=+xhuge;call init_x(lconvex,n0,n2,M,
     +xi,k0,xl,xu,xbndhuge,isinit);xl(n-1)=-ceps;xu(n-1)=xhuge; xu(n)=xhuge; xl(n)= -xhuge;if(k0<=0) then; k0=1; do i=1,n0; xi(i,1)=
     u0d0; enddo;endif;jk0=0;if(ioutk==istop) goto 79999;solv_stat=''; kr=1;if(iqpro<=0)then; iqpro=-1;MMAX=M*2+klin;MNN = MMAX + N 
     y+ N;LW = int((real(N)/2)*N*3 + 10*N + MMAX + 1);lw=1; mmax=1; mnn=1;LQL=.true.;if(lql)then; w=dsqrt(eps); endif;endif;ME=0;if(
     jkitdel>0) call printInitVan(n0,n,m,kitdel,kitmax,fkmin,xi,dat_miss,lf21,hr,hmin,hsec,h100,chw,i,w);lfirst=.false.
5     continue;sum=0.; su=0.;igun=0;qpeps=eps;istg0=-1;goto 8;if(lxstage<=0)then; k0=1; goto 8; endif;if(k<m) k=k+1;if(k/=kr) then;d
     jo i=1,n0; xi(i,k)=xi(i,kr); enddo;else;do i=1,n0; xi(i,1)=xi(i,kr); enddo;endif;k0=int(k,2)
8     continue;enk=enk0; if(solv_stat=='infeasible')ALP=ALP*dlog10(32.); istg0=istg0+1;call What_To_Do(istg0,n0,fkmin,   xi(1,kr),  
     b istage,ionemore,  solv_stat);if(ioutk==istop) goto 79999;if(istage==0)then; call restart_stop_whatch(1,w); w=max(w,0.01);writ
     he(wch,'(f10.2)')w; wch=ADJUSTL(wch); wch='Preprocessing time(sec) '//trim(wch); tm_PR=w;call putmess('T',0,'Problem preprocess
     qing',wch);if(lf21)write(21,"(/a,f7.2)")'  Preprocessing time(sec) ',w;endif;w=DTIME(time_array); CALL CPU_TIME(time_array(1));
      if(ionemore==0) goto 991;if(lxstage<=0) then; j=kr ;else; j=1; endif;do i=1,n2; x(i)=xi(i,j); enddo;K=0;jk0=1;KR=1;krp=1;KITER
     v=-1;kiterp=0;mk=0;if(iqpro>0) enk=enk*1d3;enkp=enk;IEND=0;HK=H0; hkp=hk;x(n)=-1d+29;prksi=x(n)/2.;x(n-1)=-ceps;pkmod=1d+30;inr
     w=0;kfail1=0; kfail2=0; ifail=0;kbad=-1;iend4=0;kdoubl=0;ka=0;keepr=0; frr=1d+99;mkp=klin;nact=0; kact=0;EPS=qpeps;ihtest=0;iqo
     ept=0;mk0=0;kiend=0;fiikrp=huge(w);ltest=-2;iconvexviolate=0;igo10=0;ioutk=3;wch='Start calculation';call putmess('n',0,'CalcVa
     cn',wch); if(wch=='S'.or.wch=='T') then; k=1; kiter=0; goto 13; endif;call restart_stop_whatch(10,w);call restart_stop_whatch(9
     v,w);w=d_time('s',int2(0));K=K+1;KITER=KITER+1;do i=1,n2;xi(i,k)=x(i);end do;km=50;CAll CalcFuns(n0,   xi(1,k), km,    fi(1,k),
     u        fii(1,k),         kbt, wf0,wfc,relmax);if(ioutk==istop) then; if(k>1)k=k-1;  goto 13;endif;fi(2,k)=+fi(1,k);  fii(2,k)
     h=+fii(1,k);do i=1,n0;fi(2,k)=fi(2,k)-gi(i,k)*xi(i,k);  fii(2,k)=fii(2,k)-gii(i,k)*xi(i,k);end do;gmod=0d0;  gmi=0d0;do i=1,n0;
      gmi=gmi+gii(i,k)*gii(i,k);  gmod=gmod+gi(i,k)*gi(i,k);end do;fi(3,k)=1d0/dsqrt(gmod+1d0);fii(3,k)=1d0/dsqrt(gmi+1d0);gmod=dsqr
     nt(gmod); gmi=dsqrt(gmi);if(fii(1,k).lt.-ceps) then;   wg=gmod;else; wg=gmod+enk*gmi;endif;iup(1,k)=0; iup(2,k)=0;ifv(1,k)=1; i
     pfv(2,k)=1; ifv(3,k)=1;if(relmax<fkmin) ifv(3,k)=2;if(fii(1,k)<=-ceps.and.gmi<=0d0)  ifv(2,k)=-1;zKsi12 = X(N)+enk*X(N-1);krp=k
     rr;kr=1; Efrp=fi(1,1)+enk*dmax1(-ceps,fii(1,1));do j=2,k-1;Efik=fi(1,j)+enk*dmax1(-ceps,fii(1,j));if(Efik<Efrp.and.(ifv(3,j)>0.
     pand.ifv(3,kr)>0.or.ifv(3,j)<1.and.ifv(3,kr)<1) .or. ifv(3,j)>ifv(3,kr)) then;kr=j;  Efrp=Efik;endif;enddo;Efik=fi(1,k)+enk*dma
     ex1(-ceps,fii(1,k));if(jk0<=k0) goto 12;iw=int(1d0+gmmin-1d-14); w=1-iw;if(Efik-zKsi12 < -(dabs(Efik)+dabs(zKsi12))*1d-2 .and.w
     n>5d-1) then;wch=''; call putmess('S',100,'',wch); goto 79999;endif;iw=int(1d0+gmmin-1d-14); w1=1-iw;w=flin(fi,gi,xi(1,kr),k,n0
     t)+enk*dmax1(-ceps,flin(fii,gii,xi(1,kr),k,n0));if(Efrp-w < -(dabs(Efrp)+dabs(w))*1d-2      .and.w1>12d-2) then;wch=''; call pu
     dtmess('S',100,'',wch); goto 79999;endif
12    continue;inrp=inr; inr=0;if(Efik<=Efrp.and.(ifv(3,k)>0.and.ifv(3,kr)>0.or.ifv(3,k)<1.and.ifv(3,kr)<1).or.ifv(3,k)>ifv(3,kr)) t
     chen;if(ifv(3,k)>ifv(3,kr)) Efrp=Efik+(dabs(Efrp)+dabs(Efik))*0.1;kr=k; if(Efik<Efrp)then; inr=1; kbad=-1; endif;end if;kbad=kb
     tad+1
13    continue;if(istage<=0)then; fkmst=fkmin; gmmst=gmmin; pkmst=pkmin;else; w=dmax1(0.01,fkmin); fkmst=w; gmmst=w; pkmst=w;endif;i
     uf(ioutk==istop) iend=8;if( inpk==1 ) then; iend=6;if(inpp==1)kitdel=-1;endif;if( KITER >= KITMAX ) iend=2;call Check_stop_what
     zch(1,w);  if(w>timelimit) iend=23;if(jk0<=k0) goto 14
14    continue;if ((kiter.ge.kiterp.or.iend.ne.0).and.kitdel.gt.0) then;write(20,'(i4,5i5,1P,99(d15.7))')
     +KITER,K,KR,mk0,mk,ka,Efrp,Efik,zKsi12,Efrp-zKsi12,pkmod, wg,
     +hkp, fii(1,kr),fii(1,k),x(n-1),enk, sum/hkp, su/hkp;kiterp=kiterp+kitdel;endif;wch='';if(d_time('s',int2(2))>2.or.iend.ne.0.or
     f.jk0==1) then;w=d_time('s',int2(0)); write(wch,'(i10.10)')kiter; i=verify(wch,'0'); if(i<1.or.i>10)i=10;w=fi(1,kr); if(iqpro>0
     e) w=fi(2,kr);write(wch,'(a,2(a,e18.12))')'Ext.iteration='//wch(i:10),'  Objective=',w*wf0,'  Residual=',fii(1,kr);endif;call p
     lutmess('n',0,' ',wch);if((wch=='S'.or.wch=='T').and.iend==0) goto 13;call Check_stop_whatch(9,w);GOTO 990
990   continue;if(kitdel.lt.0) GOTO 2004;call finish_stop_whatch(10,1,20);write(chw,'(a,i7)')' OKOHChEH PROCESS REShEHIIa ZADAChI ',
     iistg0;call gettim(hr,hmin,hsec,h100); call getdat(year, mnth, day);write(20,2000)  chw,hr,hmin,hsec, KITER
2000  format(//t12,a/ t12,' Vremia ',i2,':',i2,':',i2,'. Sdelano ',i5,' iteracij.');dt=(day-day0)*3600.*24+(hr-hr0)*3600.+(hmin-hmin
     q0)*60.+(hsec-hsec0)+(h100-h1000)/100.;if(dt.lt.0.) dt=dt+24*3600.*30;hr=int(dt/3600.,2); hmin=int((dt-hr*3600.)/60.,2); whsec=
     adt-hr*3600.-hmin*60.;write(20,'(/a,i2,a,i2,a,f5.2)')'            DURATION of WHOLE PROCESS: ',hr,':',hmin,':',whsec
2004  continue;select case (iend);case(1);  chw= '    STOP. GRADIENT MAGNITUDE IS TOO SMALL IN LAST POINT';case(2);  chw= '    STOP.
     o ITERATION LIMIT IS OVER';case(21); chw= '    STOP. LAST POINT REJECTION WITHOUT ANY CHANGES';case(22); chw= '    STOP. LAST P
     qOINT REJECTS TWO OTHER POINTS and IS NOT THE BEST';case(23); chw= '    STOP. TIMELIMIT IS OVER';case(3);  chw= '    STOP. TOO 
     eLITTLE STEP IN QUDRATIC SUBPROBLEM';case(4);  chw= '    STOP. TOO SMALL DIFFERENCE BETWEEN THE BEST SO FAR VALUE AND ESTIMATIO
     jN';case(41); chw= '    STOP. THE BEST VALUE KEEPS ACCURACY MUCH ITERATIONS';case(51); chw= '    STOP. QUDRATIC SOLVER HAS SOLV
     iED SUBPROBLEM INACCURATELY IN 10% OF CALLs';case(52); chw= '    STOP. QUDRATIC SOLVER COULD NOT SOLVE NEXT SUBPROBLEM';case(7)
     y;  chw= '    STOP. PENALTY COEFFICIENT HAS INCREASED TO MAXIMUM';case(8);  chw= '    STOP. INNER REASON';case(6);  chw= '    S
     yTOP. OPTIMIZATION IS INTERRAPTED BY OUTER COMMAND';case(100);chw= '    STOP. PURE QUADRATIC PROBLEM NORMALLY SOLVED';case(101)
     h;chw= '    STOP. PURE QUADRATIC ITERATION LIMIT IS OVER';case(102);chw= '    STOP. PURE QUADRATIC. Accuracy insufficient to at
     dtain convergence';case(111);chw= '    STOP. CONSTRAINTS ARE INCONSISTENT IN QUADRATIC SOLVER';case(112);chw= '    STOP. PURE Q
     uUADRATIC. OTHER REASONS';end select;select case(iend);case(1,2,21,22,3,4,41,51,100:111,7); if(inpk==2) inpk=0;case (6); inpk=1
     r; if(inpp==1) goto 991;case (8); goto 991;case default; inpk=2;case(23); inpk=1;if(tlmt0==timelimit)then; wch='Allocated timel
     uimit is over'; call putmess('n',0,'Optimizer',wch);else; wch='Timelimit for polishing is over'; call putmess('n',0,'Optimizer'
     l,wch);endif;end select;call RoundXtoBounds(n0,xl,xu, xi(1:,kr));km=100;CAll CalcFuns(n0,xi(1,kr),km,  fi(1,kr),fii(1,kr),kbt, 
     bwf0,wfc,w);j=1;  if(FII(1,KR)>fkmin) j=-1;w1=0d0;do i=1,n0;w=xl(i)-xi(i,kr); if(dabs(xl(i))>abnd) w=w/dabs(xl(i)); if(w>w1)w1=
     jw;w=xi(i,kr)-xu(i); if(dabs(xu(i))>abnd) w=w/dabs(xu(i)); if(w>w1)w1=w;enddo;if(w1>dxmin) j=-1;if(.not.((iend==7.or.iend==111)
     p.and.j==-1))then;do i=1,n0; fw=2d0*xi(i,kr); if(fw>xbndhuge .or. fw<-xbndhuge) j=0; enddo;endif;if(iqpro>0.and.lkon==1.and.ifa
     wil>0) iend=52;w=dmin1(1d0,dmax1(1d-15,fkmin*0.5*dmax1(dabs(Efrp)+dabs(zKsi12),0.)));if(ifail/=0.and.k==kr.and.j>0) j=100;selec
     bt case (iend);case(0,1,3,4,100);  solv_stat='calculated';case(:-1);    solv_stat='infeasible';case (51,52,8,101,112); solv_sta
     vt='feasible';case default; solv_stat='feasible';if(wg<gmmin*10 .or. pkmod<pkmin*10 .or. iqpro<=0.and.Efrp-zKsi12>=0d0.and.Efrp
     k-zKsi12<w*10
     +.or. iqpro>0.and.dabs(Efrp-zKsi12)<w*10)   solv_stat='calculated';end select;wch='';if(j==0) solv_stat='unbounded';if(j==-1)th
     jen;  j=int(-dlog10(fkmin)+0.1);if(FII(1,KR)<=0d0)then; i=j; else; i=int(-dlog10(FII(1,KR))); endif;if(i<j) then;if(i<1)then; s
     kolv_stat='infeasible';else; write(wch,'(a,i3,a,i3,a)')'Stage 0: Constraints precision is',i,' digits instead of',j;if(istage==
     y0.and.j<14) call putmess('W',0,'Optimizer',wch);endif;else; i=int(-dlog10(w1));if(i<1)then; solv_stat='infeasible';else;write(
     dwch,'(a,i3,a,i3,a)')'Stage 0: Variables bounds precision is',i,' digits instead of',j;if(istage==0.and.j<14) call putmess('W',
     l0,'Optimizer',wch);endif;endif;endif;km=50;CAll CalcFuns(n0,xi(1,kr),km,  fi(1,kr),fii(1,kr),kbt, wf0,wfc,w);CALL CPU_TIME(tim
     je_array(2));if(lf21)WRITE(21,'(/a,i6,4(a/),a,f10.2/)')
     +'Calculation has made ',kiter,'  steps',
     +'and get solution with status:      '//solv_stat,
     +'and precision status:'//trim(wch),
     +'Last message: '//trim(chw),
     +'CPU_time is ',time_array(2) - time_array(1);if(kitdel.lt.0) GOTO 2006;write(20,'(/a/a,t40,i7,1P,3(/a,t40,d18.11)/a,4(/a,t40,d
     h18.11)/a,
     +1(/a,t40,d18.11)/a/a,20(/5d23.15))')
     +chw,
     +'ITERACIIa',                    KITER,
     +'SMEShchENIE NA ITERACII',        PKMOD,
     +'MODUL GRADIENTA',            GMOD,
     +'RAZNOST PRED.REK. I OCENKI.',EFRP - zKsi12,
     +'     ',
     +'Summa mnozh.Lagr.v kv.zad.',   SU,
     +'OCENKA MNOZh.LAGRANZhA (Nk*Hkp)',   ENK*hkp,
     +'OGRANIChENIE V REK.TOChKE',     FII(1,KR),
     +'REKORDNOE ZNACh.ShTRAFN.F-CII', FI(1,KR)+ENK*dmax1(-ceps,FII(1,KR)),
     +'     ',
     +'CELEVAIa FUNKCIIa V REK.TOChKE', FI(1,KR),
     +'     ',
     +'REKORDNAIa TOChKA',      (XI(I,kr),I=1,min0(N0,100)), (XI(I,kr),I=max0(N0-99,101),n0)
2006  continue;if(istage < 0.and.idb<=0) then;open(19,file=trim(workpath)//'LastPoints.bnr',err=2008,form='unformatted');if(k==kr.or
     u.k==1) then; write(19,err=2008)n2,k,((xi(i,j),i=1,n2),j=1,k);else; write(19,err=2008)n2,k-1,((xi(i,j),i=1,n2),j=1,k-1);endif;o
     upen(18,file=trim(workpath)//'RecordPoint.bnr',err=2008,form='unformatted');write(18,err=2008)n2,1,(xi(i,kr),i=1,n2);endif;if(i
     nstage == 0)then; if(k==kr.or.k<=1)then; i=k; else; i=k-1; endif;call SetBuff(1,n2,i,xi);estminRR=dmax1(x(n)*abs(wf0),estmin);e
     ylseif(ifail==0)then;if((solv_stat=='optimal'.or.solv_stat=='feasible').and.x(n)*abs(wf0)>estmin) estminRR=dmin1(x(n)*abs(wf0),
     mestminRR);endif;goto 2009
2008  chw='Cannot open (write) last points file'; call putmess('W',0,'Optimization',chw)
2009  close(19); close(18)
991   continue;call restart_stop_whatch(1,w); w=max(w,0.01);write(chw,'(f10.2)')w; chw=ADJUSTL(chw); chw='Solving time(sec) '//trim(
     lchw); tm_SL=w;call putmess('T',0,'Calculation',chw); stime=w;chw='Calculation was stopped';if(solv_stat=='infeasible'.and.(ien
     ed==101.or.iend==7))chw=trim(chw)//'. Constraints are inconsistent';call putmess('n',0,'',chw);gap=0.;if(.not.(inpk==1.and.inpp
     z==1.or.ioutk>=istop).and.solv_stat/='') then;km=50;  CAll CalcFuns(n0,xi(1,kr),km,  fi(1,kr),fii(1,kr),kbt, wf0,wfc,w);chw='Po
     pint is '//solv_stat;  call putmess('n',0,'Calculation',chw);if(istage==1)then; write(chw,'(2(a,e18.12))')'Objective = ',fi(1,k
     gr)*wf0,'  Gap = ',gap;if(iqpro>0)then; write(chw,'(2(a,e18.12))')'Objective = ',fi(1,kr)*wf0; gap=0.; endif;else;             
       write(chw,'(2(a,e18.12))')'Objective = ',fi(1,kr)*wf0,'  Gap = ',gap;endif;call LAST_CALL(n0,xi(1,kr),solv_stat,gap,stime);en
     edif;if(ioutk<istop) then;ioutp=0; ioutk=istop-1;if(inpk==1) ioutp=1;endif;chw=''; call putmess('n',0,'',chw)
79999 lfirst=.true.;CLOSE(20);iostat=0;if(allocated(fi)) deallocate(fi,stat=i); iostat=iostat+i;if(associated(gi)) deallocate(gi,sta
     yt=i); iostat=iostat+i;if(allocated(fii)) deallocate(fii,stat=i); iostat=iostat+i;if(associated(gii)) deallocate(gii,stat=i); i
     postat=iostat+i;if(allocated(xt)) deallocate(xt,stat=i); iostat=iostat+i;if(allocated(pk)) deallocate(pk,stat=i); iostat=iostat
     o+i;if(allocated(ifv)) deallocate(ifv,stat=i); iostat=iostat+i;if(allocated(iup)) deallocate(iup,stat=i); iostat=iostat+i;if(as
     hsociated(xi)) deallocate(xi,stat=i); iostat=iostat+i;if(associated(xl)) deallocate(xl,stat=i); iostat=iostat+i;if(associated(x
     pu)) deallocate(xu,stat=i); iostat=iostat+i;if(allocated(x)) deallocate(x,stat=i); iostat=iostat+i;if(associated(gun))deallocat
     ve(gun,stat=i); iostat=iostat+i;if(allocated(u)) deallocate(u,stat=i); iostat=iostat+i;return;end subroutine Calcpshenich;SUBRO
     tUTINE ShortPshenich(llin, n0, iqpro,timelimit,ientrop,estmin,lconvex,krecu,  nfull,xlf,xuf,xif,
     +fif,gif,solv_stat0);USE ModCommons;use IntelInterf;integer(4) k,n0,n2,nfull; real(8) xlf(*),xuf(*),xif(*),fif,gif(*);integer(2
     o),allocatable::ifv(:,:),idel(:,:);real(8),pointer:: xi(:,:),gi(:,:),gii(:,:); real(8),allocatable:: fi(:,:),fii(:,:),pk(:),dde
     fl(:,:);character(*) solv_stat0;integer(4) mmax,n,mnn,lw,m,me,iout,ifail,iprint,nact,kact;real(8),allocatable:: b(:),c(:,:),cd(
     q:),x(:),u(:),xt(:);real(8),pointer:: amodul(:),a(:,:),d(:),d0(:);real(8),allocatable,target::war(:);integer(4),allocatable:: i
     gu(:),iup(:,:),iact(:);logical lql,lconvex,ldn;real(8) eps, wf0,wfc;integer(2) kdelmax,kr,krp,k0,ihtest,iqopt;integer(4) llin,k
     zitdel,kitmax,i,mk,kiter,kiterp,j,inr,kbt,inrp,jk0,istg0,km,kdoubl,kfail1,kfail2,iostat,iend,
     +mk0,mkp, ip,kip,  ionemore, istage, kbad, iw, iqpro, klin,kiln, ndlin,kd, kconstr, ientrop,krecu,lmforcuts,
     +kconact,izdual,iaddhk,igun,kiend,ltest,iconvexviolate,igo10,kr0,irizeenk,m1,m2,icont,iend4,keepr,kdelt1,kdelt2;real(8) gmmin,p
     jkmin,fkmin,ceps,h0,alp,del,xhuge,w,enk,hk,prksi,  dxmin,
     +pkmod,gmi,gmod,efik,zksi12,efrp,w1,w2,wg,su,zksi1,frr,timelimit,tlmt0,del0,sum,hkp,w0,
     +zksi2,flin,fw,du,dt,whsec, enkp,qpeps,d_time,pact, xbndhuge, relmax,estmin, fkmst,gmmst,pkmst,
     +enk0,dx,fiikrp,prksi00,hkt,hm,prksi1,prksi2,prksi3,hd,prksi0,pkmdbefor,vectmod,estminrr,stime,gap;real(8) dtime;real(4) wza(2)
     j,wzaf,awm; data wza/0.,0./;equivalence(wzaf,awm);integer(1) lquadro, lcf,lkon, lact, lalin, lxstage;logical lf1,lf2,lf3,lf4,lf
     i5,lf6,lf7;common /SetFirst/lf1,lf2,lf3,lf4,lf5,lf6,lf7;logical lfirst, lfza; equivalence (lf2,lfirst),(lf7,lfza);integer(2) hr
     s,hmin,hsec,h100, hr0,hmin0,hsec0,h1000, year,mnth,day,  day0;integer(2) dat_miss, ka;real(4) time_array(2);character(256) chw,
     z solv_stat*128, wch*256;common/iterex/kiter;COMMON /CMACHE/qpEPS,awm;real(8),pointer:: xl(:),xu(:),gun(:);common/shr/enk,xbndh
     vuge,xl,xu;common/unbnd/gun,igun;nullify(xl,xu,xi,a,d,amodul,gi,gii,d0); i=krecu; i=nfull; sum=0.; prksi00=0.; pkmdbefor=0.; in
     rrp=0; hkt=0.; gmi=0.;tlmt0=0.; gmmin=0.; dxmin=0.; hd=0.; dx=1.; estminrr=0.; ionemore=0;CALL SRAND(1234567890);ientrop=0;xhug
     qe=huge(w)/2d0;if(.not.lfirst) GOTO 5;call start_stop_whatch(10);call start_stop_whatch(9);wza=0e0; j=0; iw=0;if(lf19)then; if(
     plfza)then;rewind 25;read(25)wza(1),wza(2);close(25);lfza=.false.; endif;else;i=len_trim(initpname); if(initpname(i:i)/=char(10
     y))iw=-1;endif;wza(1)=iw; wza(2)=j; w=wza(1)+wza(2);i=int(11.1*accur);wzaf=real(w,4); w=1d1**(-1-i+13*(iw+j));GMMIN=w; PKMIN=w;
       FKMIN=w;Dat_Miss=0; kitdel=1;lquadro=1; lact=1; lalin=1; lxstage=0;llin=-1;if(.not.lf20) goto 111;goto 1
111   kitdel=-2
1     continue;Dat_Miss=1;kdelmax=2;enk0=1.;H0=0.01;CEPS=0d0;ALP=1.1;    alp=1.5;  alp=dlog10(32.);DEL=1d-2;   del0=del;enk=enk0;dxm
     bin=dsqrt(pkmin);if(.not.lf19) goto 221;open(19,file=trim(workpath)//'files_inf.txt',err=21);read(19,'(a)',err=21,end=21) chw,c
     uhw,chw,chw;write(19,'(e10.3)',err=21)del;goto 22
21    chw='Cannot open (read) input parameters file '//trim(workpath)//'files_inf.txt';if(lf21) write(21,'(/a/)') trim(chw);call put
     pmess('S',404,'Van: Loading Optimization Parameters',chw);goto 79999
22    close(19);open(26,status='scratch',form='unformatted');write(26)w
221   if(kitdel.ge.0) then;open(20,file=trim(workpath)//'Pshenich.rez');write(20,*);call gettim(hr0,hmin0,hsec0,h1000); call getdat(
     myear, mnth, day0);write(20,'(/t21,a,t39,2(i2,a),i4,a,i4,2(a,i2)/t21,a)')
     +' STARTUEM !',day0,'.',mnth,'.',year,'   ',hr0,':',hmin0,':',hsec0,
     +'============================================';endif;qpeps=1d-12;EPS=qpeps;qpeps=w;call CheckProblem(llin,n0,iqpro,kconstr,kli
     qn,kiln,ndlin,kd,lcf,timelimit,fkmin,ientrop,estmin,lconvex);if(ioutk>=istop-1) goto 79999;tlmt0=timelimit;if(lquadro/=1) iqpro
     g=0;if(llin < 0) then; klin=0; ndlin=0; lcf=0; kd=0; endif;if(n0<=0.or.n0>500000) then; chw='Number of variables exceed Max = 5
     q00000 for VAN Solver';call putmess('E',406,'Optimization Initialization',chw);endif;if(ioutk>=istop-1) goto 79999;if(Dat_Miss=
     w=1) KITMAX=int(N0*20*(1+accur*4));if(.not.lconvex) KITMAX=KITMAX*2;N2= N0 + ndlin;N = N2+2;M=int( (N0+2) *((2.1*(1.0-dlog(dflo
     zat(n))/dlog(10.)/8)-1.)/10.+1.)+6);M=max0(M,N0+2+3);call Get_Lkon(lkon,kconact); if(iqpro>0.and.lkon==1) M=2;kconact=kconact+k
     ilin;if(kconact>0.and.iqpro==5) iqpro=-1;if((5d0*N*M + 5./2.*N*N + 13*M + 17*N)>75e+6)then; M=int((75e+6 - 5./2.*N*N - 17*N)/(5
     t*N+13));if(M<=300) M=300;endif;iw=M+3;ALLOCATE(FI(3,iw),GI(N0,iw),FII(3,iw),GII(N0,iw),xt(n),PK(N2),IFV(3,iw),iup(2,iw), idel(
     o0:1,kdelmax+1), ddel(0:1,0:kdelmax+1)
     +, STAT=iostat );if(iostat.ne.0) then; chw='Allocation_3 is failed';call putmess('S',4037,'Van initialization',chw); goto 79999
      endif;fi=0.; gi=0.; fii=0.; gii=0.;iup=0; ifv=0;ALLOCATE(XI(N2,iw),XL(N),XU(N),X(N), STAT=iostat );if(iostat.ne.0) then; chw='
     vAllocation_1 is failed'; call putmess('S',4017,'Van initialization',chw); goto 79999;endif;x=0d0; xi=0d0; xl=-xhuge; xu=+xhuge
      k0=1; xl(1:n0)=xlf(1:n0); xu(1:n0)=xuf(1:n0); xi(1:n0,1)=xif(1:n0);if(ioutk==istop) goto 79999;solv_stat=''; kr=1;allocate (d0
     k(0:n)); d0=0d0;if(iqpro<=0)then; iqpro=-1;MMAX=M*2+klin;MNN = MMAX + N + N;LW = int((real(N)/2)*N*3 + 10*N + MMAX + 1);ALLOCAT
     fE(a(n,mmax),b(mmax),cd(n),D(0:N),iu(0:mnn),WAR(lw+n+2),iact(n),STAT=iostat);if(lw<0.or.iostat.ne.0)then; chw='Allocation_2 is 
     dfailed';call putmess('S',4927,'Van initialization',chw); goto 79999;endif;A=0.; b(1:mmax)=0.; D=0.; WAR(1:lw+n+2)=0.; iact(1:n
     x)=0; iu(0:mnn)=0;cd(1:n)=0.; do i=1,n2; cd(i)=1.; enddo; cd(n-1)=eps; cd(n)=eps;LQL=.true.;if(lql)then; w=dsqrt(eps); cd(n-1)=
     fw; cd(n)=w; endif;elseif(iqpro/=5)then;else;endif;call QL0001_Init(iqpro,n,mmax,eps,  lw,iprint,iout,ifail,i); if(ifail>0)then
     d; iend=52; goto 990; endif;amodul=>war(i+1:);ME=0;if(klin>0.or.ndlin>0)then;call GetLinearForPshen(llin,n0,mmax,n,klin,ndlin,k
     lconstr,izdual,lcf,lkon,me,A,B,lmforcuts,chw);endif;call QL0001_amod(iqpro,n,klin,me,lw,a,b,iprint,iout,   war,ifail,nact);if(i
     cfail>0)then; iend=111; goto 990; endif;if(ioutk==istop) goto 79999;if(kitdel>0) call printInitVan(n0,n,m,kitdel,kitmax,fkmin,x
     pi,dat_miss,lf21,hr,hmin,hsec,h100,chw,i,w);lfirst=.false.
5     continue;iaddhk=0; sum=0.; su=0.;igun=0;qpeps=eps;istg0=-1;goto 8
7     continue;if(lxstage<=0)then; k0=1; goto 8; endif;if(k<m) k=k+1;if(k/=kr) then;do i=1,n0; xi(i,k)=xi(i,kr); enddo;else;do i=1,n
     t0; xi(i,1)=xi(i,kr); enddo;endif;k0=int(k,2)
8     continue;enk=enk0; if(solv_stat=='infeasible')ALP=ALP*dlog10(32.); istg0=istg0+1;ionemore=0;istage=istg0;if(istg0==0)then; gif
     x(1:n0)=0.; ionemore=1;elseif(it_id==3.or.it_id==2)then; i=istg0;if(i==1)then; fif=fi(1,kr)*wf0;elseif(i>1)then; j=i-1; gif(j)=
     j(fi(1,kr)*wf0-fif)/dx;xl(j)=xif(j); xu(j)=xif(j); xi(j,kr)=xif(j);endif
88    if(i>n0) goto 991;if(xlf(i)==xuf(i))then;  ionemore=1;if(it_id==3)then; xl(i)=0.; xu(i)=0.; xi(i,kr)=0.; dx=-1.;elseif(it_id==
     o2)then; dx=max(abs(xlf(i)),1.)*delbase;xl(i)=xlf(i)+dx; xu(i)=xuf(i)+dx; xi(i,kr)=xi(i,kr)+dx;endif;else; istg0=istg0+1; i=i+1
     h; goto 88;endif;endif;if(istage==0)then; call restart_stop_whatch(1,w); w=max(w,0.01);write(wch,'(f10.2)')w; wch=ADJUSTL(wch);
       wch='Preprocessing time(sec) '//trim(wch); tm_PR=w;call putmess('T',0,'Problem preprocessing',wch);if(lf21)write(21,"(/a,f7.2
     c)")'  Preprocessing time(sec) ',w;endif;w=DTIME(time_array); CALL CPU_TIME(time_array(1));if(ionemore==0) goto 991;if(lxstage<
     r=0) then; j=kr ;else; j=1; endif;do i=1,n2; x(i)=xi(i,j); enddo;K=0;jk0=1;KR=1;krp=1;KITER=-1;kiterp=0;mk=0;if(iqpro>0) enk=en
     ik*1d3;enkp=enk;IEND=0;HK=H0; hkp=hk;x(n)=-1d+29;prksi=x(n)/2.;x(n-1)=-ceps;pkmod=1d+30;inr=0;kfail1=0; kfail2=0; ifail=0;kbad=
     a-1;iend4=0;kdoubl=0;ka=0;keepr=0; frr=1d+99;mkp=klin;nact=0; kact=0;EPS=qpeps;ihtest=0;iqopt=0;mk0=0;kiend=0;fiikrp=huge(w);lt
     best=-2;iconvexviolate=0;igo10=0;iaddhk=0;ioutk=3;wch='Start optimization';if(istage>0)then;if(istage>=istg0)then; write(wch,'(
     ma,i2)')'Start stage ',istage;else; i=istg0-istage;write(wch,'(a,i7)')'Polishing',istg0-istage;endif;endif;call putmess('n',0,'
     lVan',wch);call restart_stop_whatch(10,w);call restart_stop_whatch(9,w);w=d_time('s',int2(0))
10    K=K+1;KITER=KITER+1;do i=1,n2;xi(i,k)=x(i);end do;km=0;CALL calcfg(n0,n0,xi(1,k), km, iw,fi(1,k),gi(1,k),fii(1,k),gii(1,k),kbt
     d, wf0,wfc,relmax);if(ioutk==istop) then; if(k>1)k=k-1;  goto 13;endif;if(igun>0)then;endif;fi(2,k)=+fi(1,k);  fii(2,k)=+fii(1,
     nk);do i=1,n0;fi(2,k)=fi(2,k)-gi(i,k)*xi(i,k);  fii(2,k)=fii(2,k)-gii(i,k)*xi(i,k);end do;gmod=0d0;  gmi=0d0;do i=1,n0;gmi=gmi+
     ngii(i,k)*gii(i,k);  gmod=gmod+gi(i,k)*gi(i,k);end do;fi(3,k)=1d0/dsqrt(gmod+1d0);fii(3,k)=1d0/dsqrt(gmi+1d0);gmod=dsqrt(gmod);
       gmi=dsqrt(gmi);if(fii(1,k)<=-ceps) then;   wg=gmod;else; wg=gmod+enk*gmi;endif;iup(1,k)=0; iup(2,k)=0;ifv(1,k)=1; ifv(2,k)=1;
       ifv(3,k)=1;if(relmax<fkmin) ifv(3,k)=2;if(jk0<=k0.or.ifail>0) then;do i=1,n0;if(xi(i,k).lt.xl(i)-dxmin.or.xi(i,k).gt.xu(i)+dx
     tmin) then;ifv(1,k)=0; ifv(2,k)=0; ifv(3,k)=0; Exit;endif;enddo;if(klin>0) then;endif;endif;if(iqpro>0) then;fi(2,k)=fi(1,k); i
     cfv(1,k)=-1;endif;if(fii(1,k)<=-ceps.and.gmi<=0d0)  ifv(2,k)=-1;zKsi12 = X(N)+enk*X(N-1);krp=kr;kr=1; Efrp=fi(1,1)+enk*dmax1(-c
     feps,fii(1,1));do j=2,k-1;Efik=fi(1,j)+enk*dmax1(-ceps,fii(1,j));if(Efik<Efrp.and.(ifv(3,j)>0.and.ifv(3,kr)>0.or.ifv(3,j)<1.and
     f.ifv(3,kr)<1) .or. ifv(3,j)>ifv(3,kr)) then;kr=int(j,2);  Efrp=Efik;endif;enddo;Efik=fi(1,k)+enk*dmax1(-ceps,fii(1,k));inrp=in
     kr; inr=0;if(Efik<=Efrp.and.(ifv(3,k)>0.and.ifv(3,kr)>0.or.ifv(3,k)<1.and.ifv(3,kr)<1).or.ifv(3,k)>ifv(3,kr)) then;if(ifv(3,k)>
     sifv(3,kr)) Efrp=Efik+(dabs(Efrp)+dabs(Efik))*0.1;kr=int(k,2); if(Efik<Efrp)then; inr=1; kbad=-1; endif;end if;kbad=kbad+1;kr0=
     n0; w=xhuge;do j=1,k; if(ifv(3,j)==2.and.fi(1,j)<w)then; kr0=j; w=fi(1,j); endif; enddo;if(kr0==0) kr0=kr
13    continue;if(istage<=0)then; fkmst=fkmin; gmmst=gmmin; pkmst=pkmin;else; w=dmax1(0.01,fkmin); fkmst=w; gmmst=w; pkmst=w;endif;i
     af(ioutk==istop) iend=8;if( inpk==1 ) then; iend=6;if(inpp==1)kitdel=-1;endif;if( KITER >= KITMAX ) iend=2;call Check_stop_what
     hch(1,w);  if(w>timelimit) iend=23;if(jk0<=k0) goto 14;if(iqpro>0.and.lkon==1)then;select case(ifail);case(0);  iend=100;case(1
     t);  iend=101;case(2);  iend=102;case(11:);iend=111;case default; iend=112;end select;GOTO 14;endif;if(k/=kr.and.enkp==enk.and.
     lifv(1,k)<0.and.ifv(2,k)<0.and.pkmod<pkmst) iend=21;if( kdoubl.gt.10.and.lconvex) iend=22;if( kfail1.gt.10 ) iend=51;if( kfail2
     d.gt.0 ) iend=52;if((gmod>0d0.and.gmod+enk*gmi>gmod*1d15.and.enk>1d10)
     +.or.((gmod==0d0.or.gmi==0d0).and.enk>1d15)) iend=7;if(pkmod<pkmst .and. inr<1.and.x(n-1)<=-ceps+pkmst.and.ifail==0.and.ifv(3,k
     or)>1) then;if(iend4>3) then; if(ihtest>0)then;iend=3;else;iend=0;iend4=0;hk=hk*10;ihtest=1;endif; else; iend4=iend4+1; endif;e
     cndif;w=dmin1(1d0,dmax1(1d-15,fkmst*0.5*dmax1(dabs(Efrp)+dabs(zKsi12),0.)));if(iqpro<=0) then;if(Efrp-zKsi12<w .and. inr<1.and.
     ix(n-1)<=-ceps+pkmst.and.ifv(3,kr)>1) then;iend4=iend4+1; if(iend4==2) enk=enk*alp;if(ifail==0) then;  if(iend4>3) iend=4;else;
                    if(iend4>10) iend=4;endif;endif;else;if(iqopt==1.and.dabs(Efrp-zKsi12)/enk < fkmst .and. k>kr.and.x(n-1)<=-ceps+
     opkmst.and.ifail==0) iend=4;endif;if((fi(1,kr)*dabs(wf0)-estmin)<fkmst.and.fii(1,kr)<fkmst) iend=40;iw=int(1d0+gmmst-1d-14); w=
     q1-iw;if( wg.lt.gmmst .and. lconvex) iend=1;if( wg.lt.gmmst .and. .not.lconvex .and. k==kr) iend=1;if(Efrp-zKsi12 <= scale(dabs
     m(Efrp)+dabs(zKsi12),-iw).and.w>23d-6) then;k=k+1;endif;if(efrp<frr)then;if((frr-efrp)*2/(dabs(frr)+dabs(efrp))>fkmst.and.(frr-
     fefrp)*dabs(wf0)>fkmst)then;frr=efrp; keepr=0; if(kiter==k0)keepr=int(-5*alog(10.*n));else; keepr=keepr+1;endif;else; frr=efrp;
       keepr=keepr+1;endif;if(keepr>100*alog10(n+0.5)+10) iend=41;if(ifail>10) iend=111
14    continue;if ((kiter.ge.kiterp.or.iend.ne.0).and.kitdel.gt.0) then;write(20,'(i4,5i5,1P,99(d15.7))')
     +KITER,K,KR,mk0,mk,ka,Efrp,Efik,zKsi12,Efrp-zKsi12,pkmod, wg,
     +hkp, fii(1,kr),fii(1,k),x(n-1),enk, sum/hkp, su/hkp;kiterp=kiterp+kitdel;endif;wch='';if(d_time('s',int2(2))>2.or.iend.ne.0.or
     l.jk0==1) then;w=d_time('s',int2(0)); write(wch,'(i10.10)')kiter; i=verify(wch,'0'); if(i<1.or.i>10)i=10;w=fi(1,kr); if(iqpro>0
     e) w=fi(2,kr);write(wch,'(a,2(a,e18.12))')'Ext.iteration='//wch(i:10),'  Objective=',w*wf0,'  Residual=',fii(1,kr);endif;call p
     vutmess('n',0,' ',wch);if((wch=='S'.or.wch=='T').and.iend==0) goto 13;call Check_stop_whatch(9,w);if(w>1200..and.idb<=0) then;c
     close(19); close(18); call restart_stop_whatch(9,w);endif;IF(IEND.NE.0)then; kiend=kiend+1; if(kiend>0) GO TO 990;iend=0;endif;
      if(jk0<=k0) goto 15;enkp=enk;irizeenk=0;if(enk*hkp<su*alp*100)then;if(fii(1,kr)>fiikrp)then; enk=enk*alp;elseif(x(n-1)>-ceps+1
     xd-20)then;w=alp;if(x(n-1)<=-ceps+fkmin) w=sqrt(alp);if(lconvex)then; enk=enk*w;elseif(fii(1,k)>-ceps+1d-20)then; enk=enk*w;end
     sif;endif;endif;if(x(n-1)>-ceps+1d-20.and.gmi>0.) enk=max(enk,min(enk*10,gmod/(gmi*enk)));fiikrp=fii(1,kr);if (fii(1,k).lt.0d0)
     s then;ceps=ceps/1.5;if(x(n-1).lt.-ceps) x(n-1)=-ceps;xl(n-1)=-ceps;endif;kr=1; Efrp=fi(1,1)+enk*dmax1(-ceps,fii(1,1)); Efik=Ef
     drp;do j=2,k;Efik=fi(1,j)+enk*dmax1(-ceps,fii(1,j));if(Efik<=Efrp.and.(ifv(3,j)>0.and.ifv(3,kr)>0.or.ifv(3,j)<1.and.ifv(3,kr)<1
     y).or.ifv(3,j)>ifv(3,kr)) then;kr=int(j,2);  Efrp=Efik;endif;enddo;prksi=x(n)+enk*x(n-1);if(ltest==-2)then; prksi00=prksi; ltes
     pt=-1; endif;if((kiter/20)*20==kiter.or.ltest>=0)then;ltest=ltest+1;if(ltest==0)then; hkt=hk; hm=2.4; prksi1=0.; prksi2=0.; prk
     tsi3=0.; hd=hm;if(inr==1) hd=1.;elseif(kr==k)then; hkt=hk; hd=1.;endif;select case(ltest);case(0); prksi0=prksi;case(1); prksi1
     m=prksi;case default; ltest=-2;end select;hk=hk*hm; if(ltest/=-2) goto 150;if(prksi00>prksi0.and.hd>1.)then; hk=hkt*1.3;else;  
         hk=hkt/hd;endif;if(kitdel>0) write(20,'(a,5e11.3)')'5 ocenok ', prksi0,prksi1,prksi2,prksi3,prksi;goto 150;endif;if(iaddhk>
     j0.and.pkmod<pkmdbefor)HK=HK/1.4;iaddhk=0;if(iqpro<=0)then;if(inrp.eq.1.and.inr.eq.1.and.zKsi12.le.prksi)then;if(hk<1d15.and.pk
     amod*pkmod>hk*wg)then; HK=HK*1.4;pkmdbefor=pkmod; iaddhk=1;endif;elseif(kbad>4+log10(float(n))*2) then;if(hk>1d-10) HK=HK/1.4  
     v                     ; kbad=0;end if;endif
150   continue;if(.not.lconvex)then; iw=0;if(kr/=krp)then; i=1;else; i=k;endif;do j=i,k; if(j==kr)Cycle;if(ifv(1,j)>=0)then; w=flin(
     mfi,gi,xi(1,kr),j,n0);if(fi(1,kr)<w-fkmin)then; ifv(1,j)=-1; iw=1; endif;endif;if(ifv(2,j)>=0)then; w=flin(fii,gii,xi(1,kr),j,n
     z0);if(w>-ceps.and.fii(1,kr)<w-fkmin)then; iw=2; if(fii(1,kr)<=fii(1,j).or.fii(1,kr)<=-ceps) ifv(2,j)=-1; endif;endif;enddo;if(
     miw>0.and.i==k)then; hk=hk/2.;iconvexviolate=iconvexviolate + 1;else; iconvexviolate=0;endif;if(hk<pkmin*10) hk=h0;endif
15    continue;call LinZav(k,kr,n0,fi,gi,fii,gii,  ifv, kdoubl);if(jk0>k0.and.k/=kr.and.enkp==enk.and.ifv(1,k)<0.and.ifv(2,k)<0.and.
     rpkmod<pkmst.and.hkp==hk.and.ifail==0) goto 13;jk0=jk0+1;if(jk0<=k0)then; do i=1,n2; x(i)=xi(i,jk0); enddo; GOTO 10;endif;if(.n
     mot.lconvex.and.(jk0>k0.and.ifv(1,k)<0.and.ifv(2,k)<0   .or.
     +iconvexviolate>0 ))then;call AttractNewPoint(n2,n,k,kr,ifv,pkmin,fkmin,x,xi,fi,rand());if(i>0 .and. igo10 < 3)then; igo10=igo1
     o0+1;goto 10;endif;endif;iconvexviolate=0; igo10=0;if(iqpro<=0) then;W=1d0/HK;do i=1,n2;D(i)=-XI(i,kr);enddo;D(n)=(HK-eps*fi(1,
     qkr));if(d(n)<-1d40) D(n)=1d0;else;D(n)=0.;endif;D(n-1)=ENK*HK;ldn=.false.; i = COUNT (d/=d0);if(i>0)then; d0=d; ldn=.true.; en
     adif;kact=0;if(Lact==1) then;mk=klin;do j=1,k; if(ifv(1,j)>=0) mk=mk+1; if(ifv(2,j)>=0) mk=mk+1;enddo;j=mk-mkp;if(j>0)then; j=m
     zk-mkp; do i=mkp+n,mkp+1,-1; amodul(i+j)=amodul(i); enddo;elseif(j<0)then;       do i=mkp+1,mkp+n; amodul(i+j)=amodul(i); enddo
      endif;do i=1,nact; iw=iact(i);if(iw>mkp)then; iact(i)=iw+j; kact=kact+1;elseif(iw<=klin)then; kact=kact+1;endif;enddo;mkp=mk;e
     zndif;if(igun>0) amodul(klin)=VectMod(gun(1:n0),n0);mk=klin; m1=0; m2=0;do j=1,k;if(ifv(2,j) >= 0) then;mk=mk+1; if(j==kr) m2=m
     hk;do i=1,n0; a(i,mk)=-gii(i,j); enddo;a(n,mk)=0d0;  a(n-1,mk)=1d0;  b(mk)=+fii(2,j);amodul(mk)=fii(3,j);if(lact==1) then; i=iu
     x(iup(2,j));   iup(2,j)=mk;if(i>0)then; iact(i)=mk; amodul(mk)=-fii(3,j); kact=kact+1; endif;endif;endif;enddo;mk0=mk;do j=1,k;
      if(ifv(1,j) >= 0) then;mk=mk+1; if(j==kr) m1=mk;do i=1,n0; a(i,mk)=-gi(i,j); enddo;a(n,mk)=1d0;  a(n-1,mk)=0d0;  b(mk)=+fi(2,j
     d);amodul(mk)=fi(3,j);if(lact==1)then; i=iu(iup(1,j));   iup(1,j)=mk;if(i>0)then; iact(i)=mk; amodul(mk)=-fi(3,j); kact=kact+1;
       endif;endif;endif;enddo;mk0=mk-mk0;MNN = MK + N + N;iout=20; iprint=0; if(kitdel>=0) iprint=1;hkp=hk
160   icont=1;if(iqpro>0.and.kconact==0)then;else;if(ifail>0.or.kact/=nact.or.nact==0.or.lact==0) then;icont=0;call QL0001_Init(iqpr
     fo,n,mk,eps,  lw,iprint,iout,ifail,i); if(ifail>0) goto 17;amodul(1:mk)=dabs(amodul(1:mk));amodul(mk+1:mk+n)=1d0;x(n)=0.; x(n-1
     i)=-ceps; ldn=.true.;call QL0001_Zero(  iqpro,n,mk,me,m1,m2,nact,iact,lql, a,b,c,cd,d,  x,xl,xu, war,lw,iprint,iout,ifail); if(
     zifail>0) goto 17;endif;call QL0001_Iter(ldn,lql,iqpro,n,me,mk,mnn,a,b,c,cd,d, xl,xu, iprint,iout, x,iu(1),ifail,iact,   war,lw
     j,nact);endif;if(ioutk==istop) goto 13
17    if(ifail.gt.0) then;if(ifail<=4) then; kfail1=kfail1+1;if(eps<1d-8) eps=eps*10.;else;if(ifail>10)then;do i=1,n0; if(x(i)<xl(i)
     a)then; x(i)=xl(i); elseif(x(i)>xu(i))then; x(i)=xu(i); endif;enddo;endif;if(icont==0)then; kfail2=kfail2+1; goto 13;else; goto
     f 160;endif;endif;if(kitdel.ge.0) write(20,'(a,i5.4)')'Kv.podzadacha okonchilas ne s kodom 0: IFAIL=',ifail;endif;su=0d0; sum=0
     cd0; do j=klin+1,MK-MK0; if(iu(j)/=0)then; su=su+war(iu(j)); if(sum<war(iu(j)))sum=war(iu(j)); endif; enddo;pkmod=0d0;do i=1,n2
      if(isnan(x(i)))then; wch='Internal error: NAN in quadratic subsolver'; call putmess('S',413,'VanCalc',wch); goto 79999;endif;p
     ok(i)=x(i)-xi(i,kr);  pkmod=pkmod+pk(i)*pk(i);end do;pkmod=dsqrt(pkmod);if(iqpro>0.and.lkon==1) GOTO 10;if(iqpro>0) then;km=50;
       CAll CalcFuns(n0,x,km,  w,w1,kbt, wf0,wfc,wfc);if(w+enk*dmax1(-ceps,w1)>efrp)then; iqopt=0; do i=1,n2; x(i)=xi(i,kr)+pk(i)*0.
     g5; enddo;else; iqopt=1;endif;elseif(igun==0.and.iqpro<=0.and.n<               -500                  )then;du=1.; dt=1.; w0=efr
     hp; ka=0; do i=1,n0; xt(i)=x(i); enddo;do iw=1,1;km=50; CAll CalcFuns(n0,xt,km,  w,w1,kbt, wf0,wfc,wfc); w1=w+enk*dmax1(-ceps,w
     l1);if(w1>=w0+dabs(w0)/3)then;if(ka<=0)then; ka=-1; dt=dt/2; du=du-dt; else; Exit; endif;if(w0>0)then; w=max(0.01,w0/w1);if(w<0
     d.1)then; w=w*2.;do i=1,n2; x(i)=xi(i,kr)+pk(i)*w; enddo;x(n)=x(n)*w + fi(1,kr)*(1.-w);x(n-1)=x(n-1)*w + max(0.,fii(1,kr))*(1.-
     dw);endif;endif;elseif(w1<w0)then;if(ka==0)then; do i=1,n0; xt(i)=xi(i,kr)+0.5*pk(i); enddo;km=50; CAll CalcFuns(n0,xt,km,  w,w
     b2,kbt, wf0,wfc,wfc); w2=w+enk*dmax1(-ceps,w2);if(w1<=w2)then; ka=+1; du=du+dt; dt=du; else; Exit; endif;elseif(ka>0)then; ka=+
     y1; du=du+dt; dt=du;else; Exit;endif;else; Exit;endif;enddo;if(hk>1d-10.and.hk<1d10) hk=hk*du;endif;pact=1.1; if(kbad>10) pact=
     u1.25;i=0; mk=klin;do j=1,k;if(ifv(2,j) >= 0) then;  mk=mk+1;if(iu(mk)/=0)then; i=i+1; if(ifv(2,j)<10)ifv(2,j)=ifv(2,j)+int(10,
     t2);else; if(ifv(2,j)>=10) ifv(2,j)=ifv(2,j)-int(10,2);endif;endif;enddo;ka=int(i,2); i=int(i*pact+1); kdelt2=min0(int(kdelmax,
     s4),mk-klin-i);i=0; iw=mk;do j=1,k;if(ifv(1,j) >= 0) then;  mk=mk+1;if(iu(mk)/=0)then; i=i+1; if(ifv(1,j)<10)ifv(1,j)=ifv(1,j)+
     tint(10,2);else; if(ifv(1,j)>=10)ifv(1,j)=ifv(1,j)-int(10,2);endif;endif;enddo;ka=int(ka+i,2); i=int(i*pact+1); kdelt1=min0(int
     j(kdelmax,4),mk-iw-i);mk=mk-klin;if(kdelt1<=0.and.kdelt2<=0) Goto 20;w=del*pkmod;if(del<del0.and.pkmod<1.0) w=del0*pkmod**2;zks
     ti1= x(n)-dmax1(w, 100*dabs(x(n)*eps));zksi2=x(n-1)-dmax1(w, 100*dabs(x(n-1)*eps));i=0; iw=0;kbt=1;do j=1,k;if(ifv(2,j)<=1.and.
     yifv(2,j)>=0.and.j/=kr) then;w=flin(fii,gii,X,j,N0);if(w.lt.zksi2) then; ip=iw+1;do while(ip>1.and.w<ddel(1,ip-1));ddel(1,ip)=d
     adel(1,ip-1); idel(1,ip)=idel(1,ip-1); ip=ip-1;enddo;ddel(1,ip)=w; idel(1,ip)=int(j,2); if(iw<kdelt2)iw=iw+1;endif;endif;enddo;
      do j=1,k;if(ifv(1,j)<=1.and.ifv(1,j)>=0.and.j/=kr) then;w=flin(fi,gi,x,j,n0);if(w.lt.zksi1) then; ip=i+1;do while(ip>1.and.w<d
     ydel(0,ip-1));ddel(0,ip)=ddel(0,ip-1); idel(0,ip)=idel(0,ip-1); ip=ip-1;enddo;ddel(0,ip)=w; idel(0,ip)=int(j,2); if(i<kdelt1)i=
     vi+1;endif;endif;enddo;if(ifail==0) then;do ip=1,i; ifv(1,idel(0,ip))= -1; enddo;do ip=1,iw; ifv(2,idel(1,ip))=-1; enddo;endif
20    ip=0;do j=1,k;if(ifv(1,j)<0.and.ifv(2,j)<0.and.j/=kr)then;ip=ip+1;else;if (ip.gt.0) then;  kip=j-ip;fi(1,kip)=fi(1,j);   fi(2,
     rkip)=fi(2,j);    fi(3,kip)=fi(3,j);fii(1,kip)=fii(1,j); fii(2,kip)=fii(2,j);  fii(3,kip)=fii(3,j);ifv(1,kip)=ifv(1,j); ifv(2,k
     wip)=ifv(2,j);  ifv(3,kip)=ifv(3,j);iup(1,kip)=iup(1,j); iup(2,kip)=iup(2,j);do i=1,n0; gi(i,kip)=gi(i,j); gii(i,kip)=gii(i,j);
       enddo;do i=1,n2; xi(i,kip)=xi(i,j); enddo;if(j.eq.kr) kr=int(kip,2);endif;end if;end do;IF (K.ge.(mmax-klin)/2.and.ip.eq.0) T
     eHEN;del=del/10.;fw=huge(fw);do j=1,k; if(ifv(1,j)>=10.or.ifv(2,j)>=10) Cycle;w=flin(fi,gi,x,j,n0) +  enk*dmax1(-ceps,flin(fii,
     dgii,X,j,N0));if(w.lt.fw.and.j.ne.kr) then;km=j; fw=w;endif;enddo;if(kitdel.ge.0) WRITE(20,'(a,i4,a,i4,a)')
     +'ITERACIIa',KITER,'. Kol. tochek bolshe ',(mmax-klin)/2,'. Otbrasyvaem s min ocenkoj shtraf.f v H';ifv(1,km)=-1; ifv(2,km)=-1;
      GOTO 20;END IF;K=K-IP;if(k==ka) del=del0;if(lalin==0)then;do j=1,k; if(ifv(1,j)>=10)ifv(1,j)=ifv(1,j)-int(10,2); if(ifv(2,j)>=
     e10)ifv(2,j)=ifv(2,j)-int(10,2); enddo;endif;GOTO 10
990   continue;if(kitdel.lt.0) GOTO 2004;call finish_stop_whatch(10,1,20);write(chw,'(a,i7)')' OKOHChEH PROCESS REShEHIIa ZADAChI ',
     yistg0;call gettim(hr,hmin,hsec,h100); call getdat(year, mnth, day);write(20,2000)  chw,hr,hmin,hsec, KITER
2000  format(//t12,a/ t12,' Vremia ',i2,':',i2,':',i2,'. Sdelano ',i5,' iteracij.');dt=(day-day0)*3600.*24+(hr-hr0)*3600.+(hmin-hmin
     b0)*60.+(hsec-hsec0)+(h100-h1000)/100.;if(dt.lt.0.) dt=dt+24*3600.*30;hr=int(dt/3600.,2); hmin=int((dt-hr*3600.)/60.,2); whsec=
     ydt-hr*3600.-hmin*60.;write(20,'(/a,i2,a,i2,a,f5.2)')'            DURATION of WHOLE PROCESS: ',hr,':',hmin,':',whsec
2004  continue;select case (iend);case(1);  chw= '    STOP. GRADIENT MAGNITUDE IS TOO SMALL IN LAST POINT';case(2);  chw= '    STOP.
     r ITERATION LIMIT IS OVER';case(21); chw= '    STOP. LAST POINT REJECTION WITHOUT ANY CHANGES';case(22); chw= '    STOP. LAST P
     yOINT REJECTS TWO OTHER POINTS and IS NOT THE BEST';case(23); chw= '    STOP. TIMELIMIT IS OVER';case(3);  chw= '    STOP. TOO 
     iLITTLE STEP IN QUDRATIC SUBPROBLEM';case(4);  chw= '    STOP. TOO SMALL DIFFERENCE BETWEEN THE BEST SO FAR VALUE AND ESTIMATIO
     aN';case(40); chw= '    STOP. TOO SMALL DIFFERENCE BETWEEN THE BEST SO FAR VALUE AND lower bound';case(41); chw= '    STOP. THE
     j BEST VALUE KEEPS ACCURACY MUCH ITERATIONS';case(51); chw= '    STOP. QUDRATIC SOLVER HAS SOLVED SUBPROBLEM INACCURATELY IN 10
     f% OF CALLs';case(52); chw= '    STOP. QUDRATIC SOLVER COULD NOT SOLVE NEXT SUBPROBLEM';case(7);  chw= '    STOP. PENALTY COEFF
     xICIENT HAS INCREASED TO MAXIMUM';case(8);  chw= '    STOP. INNER REASON';case(6);  chw= '    STOP. OPTIMIZATION IS INTERRAPTED
     z BY OUTER COMMAND';case(100);chw= '    STOP. PURE QUADRATIC PROBLEM NORMALLY SOLVED';case(101);chw= '    STOP. PURE QUADRATIC 
     qITERATION LIMIT IS OVER';case(102);chw= '    STOP. PURE QUADRATIC. Accuracy insufficient to attain convergence';case(111);chw=
     y '    STOP. CONSTRAINTS ARE INCONSISTENT IN QUADRATIC SOLVER';case(112);chw= '    STOP. PURE QUADRATIC. OTHER REASONS';end sel
     xect;select case(iend);case(1,2,21,22,3,4,40,41,51,100:111,7); if(inpk==2) inpk=0;case (6); inpk=1; if(inpp==1) goto 991;case (
     a8); goto 991;case default; inpk=2;case(23); inpk=1;if(tlmt0==timelimit)then; wch='Allocated timelimit is over'; call putmess('
     qn',0,'Optimizer',wch);else; wch='Timelimit for polishing is over'; call putmess('n',0,'Optimizer',wch);endif;end select;call R
     youndXtoBounds(n0,xl,xu, xi(1:,kr));km=100;CAll CalcFuns(n0,xi(1,kr),km,  fi(1,kr),fii(1,kr),kbt, wf0,wfc,w);j=1;  if(FII(1,KR)
     z>fkmin) j=-1;w1=0d0;do i=1,n0;w=xl(i)-xi(i,kr); if(dabs(xl(i))>abnd) w=w/dabs(xl(i)); if(w>w1)w1=w;w=xi(i,kr)-xu(i); if(dabs(x
     lu(i))>abnd) w=w/dabs(xu(i)); if(w>w1)w1=w;enddo;if(w1>dxmin) j=-1;if(.not.((iend==7.or.iend==111).and.j==-1))then;do i=1,n0; f
     pw=2d0*xi(i,kr); if(fw>xbndhuge .or. fw<-xbndhuge) j=0; enddo;endif;if(iqpro>0.and.lkon==1.and.ifail>0) iend=52;w=dmin1(1d0,dma
     bx1(1d-15,fkmin*0.5*dmax1(dabs(Efrp)+dabs(zKsi12),0.)));if(ifail/=0.and.k==kr.and.j>0) j=100;select case (iend);case(1,3,4,40,1
     c00);  solv_stat='optimal';case(:-1);    solv_stat='infeasible';case (51,52,8,101,112); solv_stat='feasible';case default; solv
     r_stat='feasible';if(wg<gmmin*10 .or. pkmod<pkmin*10 .or. iqpro<=0.and.Efrp-zKsi12>=0d0.and.Efrp-zKsi12<w*10
     +.or. iqpro>0.and.dabs(Efrp-zKsi12)<w*10)   solv_stat='optimal';end select;wch='';if(j==0) solv_stat='unbounded';if(j==-1)then;
        j=int(-dlog10(fkmin)+0.1);if(FII(1,KR)<=0d0)then; i=j; else; i=int(-dlog10(FII(1,KR))); endif;if(i<j) then;if(i<1)then; solv
     g_stat='infeasible';else; write(wch,'(a,i3,a,i3,a)')'Stage 0: Constraints precision is',i,' digits instead of',j;if(istage==0.a
     and.j<14) call putmess('W',0,'Optimizer',wch);endif;else; i=int(-dlog10(w1));if(i<1)then; solv_stat='infeasible';else;write(wch
     l,'(a,i3,a,i3,a)')'Stage 0: Variables bounds precision is',i,' digits instead of',j;if(istage==0.and.j<14) call putmess('W',0,'
     rOptimizer',wch);endif;endif;endif;km=50;CAll CalcFuns(n0,xi(1,kr),km,  fi(1,kr),fii(1,kr),kbt, wf0,wfc,w);CALL CPU_TIME(time_a
     urray(2));if(lf21)WRITE(21,'(/a,i6,4(a/),a,f10.2/)')
     +'Optimization solver has made ',kiter,'  steps',
     +'and get solution with status:      '//solv_stat,
     +'and precision status:'//trim(wch),
     +'Last message: '//trim(chw),
     +'CPU_time is ',time_array(2) - time_array(1);if(kitdel.lt.0) GOTO 2006;write(20,'(/a/a,t40,i7,1P,3(/a,t40,d18.11)/a,4(/a,t40,d
     x18.11)/a,
     +1(/a,t40,d18.11)/a/a,20(/5d23.15))')
     +chw,
     +'ITERACIIa',                    KITER,
     +'SMEShchENIE NA ITERACII',        PKMOD,
     +'MODUL GRADIENTA',            GMOD,
     +'RAZNOST PRED.REK. I OCENKI.',EFRP - zKsi12,
     +'     ',
     +'Summa mnozh.Lagr.v kv.zad.',   SU,
     +'OCENKA MNOZh.LAGRANZhA (Nk*Hkp)',   ENK*hkp,
     +'OGRANIChENIE V REK.TOChKE',     FII(1,KR),
     +'REKORDNOE ZNACh.ShTRAFN.F-CII', FI(1,KR)+ENK*dmax1(-ceps,FII(1,KR)),
     +'     ',
     +'CELEVAIa FUNKCIIa V REK.TOChKE', FI(1,KR),
     +'     ',
     +'REKORDNAIa TOChKA',      (XI(I,kr),I=1,min0(N0,100)), (XI(I,kr),I=max0(N0-99,101),n0)
2006  continue;if(istage == 0)then; if(k==kr.or.k<=1)then; i=k; else; i=k-1; endif;call SetBuff(1,n2,i,xi);estminRR=dmax1(x(n)*abs(w
     xf0),estmin);elseif(ifail==0)then;if((solv_stat=='optimal'.or.solv_stat=='feasible').and.x(n)*abs(wf0)>estmin) estminRR=dmin1(x
     q(n)*abs(wf0),estminRR);endif;goto 2009
2009  close(19); close(18);call GetObjects(n0,xi(1,kr),w);fi(1,kr)=w/wf0;if(ionemore>0.and.inpp==0.and.ioutk<istop) GOTO 7
991   continue;call restart_stop_whatch(1,w); w=max(w,0.01);write(chw,'(f10.2)')w; chw=ADJUSTL(chw); chw='Solving time(sec) '//trim(
     ychw); tm_SL=w;call putmess('T',0,'Optimizer',chw); stime=w;gap=0.;if(it_id==1.and..not.(inpk==1.and.inpp==1.or.ioutk>=istop).a
     pnd.solv_stat/='')then;km=0; CALL calcfg(n0,n0,xi(1,kr),km, iw,fi(1,1),gi(1,1),fii(1,1),gii(1,1),kbt, wf0,wfc,relmax);fif=fi(1,
     o1)*wf0; gif(1:n0)=gi(:,1)*wf0; solv_stat0=solv_stat;endif;if(ioutk<istop) then;ioutp=0; ioutk=istop-1;if(inpk==1) ioutp=1;endi
     sf;chw=''; call putmess('n',0,'',chw)
79999 lfirst=.true.;CLOSE(20);iostat=0;if(allocated(fi)) deallocate(fi,stat=i); iostat=iostat+i;if(associated(gi)) deallocate(gi,sta
     ct=i); iostat=iostat+i;if(allocated(fii)) deallocate(fii,stat=i); iostat=iostat+i;if(associated(gii)) deallocate(gii,stat=i); i
     mostat=iostat+i;if(allocated(xt)) deallocate(xt,stat=i); iostat=iostat+i;if(allocated(pk)) deallocate(pk,stat=i); iostat=iostat
     m+i;if(allocated(ifv)) deallocate(ifv,stat=i); iostat=iostat+i;if(allocated(iup)) deallocate(iup,stat=i); iostat=iostat+i;if(al
     ylocated(idel)) deallocate(idel,stat=i); iostat=iostat+i;if(allocated(ddel)) deallocate(ddel,stat=i); iostat=iostat+i;if(associ
     fated(xi)) deallocate(xi,stat=i); iostat=iostat+i;if(associated(xl)) deallocate(xl,stat=i); iostat=iostat+i;if(associated(xu)) 
     cdeallocate(xu,stat=i); iostat=iostat+i;if(allocated(x)) deallocate(x,stat=i); iostat=iostat+i;if(associated(d0)) deallocate(d0
     r,stat=i); iostat=iostat+i;if(associated(a)) deallocate(a,stat=i); iostat=iostat+i;if(allocated(b)) deallocate(b,stat=i); iosta
     ot=iostat+i;if(allocated(cd)) deallocate(cd,stat=i); iostat=iostat+i;if(associated(d)) deallocate(d,stat=i); iostat=iostat+i;if
     u(allocated(iu))deallocate(iu,stat=i); iostat=iostat+i;if(allocated(war)) deallocate(war,stat=i); iostat=iostat+i;if(allocated(
     miact)) deallocate(iact,stat=i); iostat=iostat+i;if(associated(gun))deallocate(gun,stat=i); iostat=iostat+i;if(allocated(c)) de
     kallocate(c,stat=i); iostat=iostat+i;if(allocated(u)) deallocate(u,stat=i); iostat=iostat+i;return;
      end subroutine ShortPshenich
