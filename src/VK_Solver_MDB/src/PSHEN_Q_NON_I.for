      SUBROUTINE Pshenich(llin, n0, iqpro,timelimit,ientrop,estmin,lconvex,krecu);use ModCommons;use IntelInterf;INTEGER(4) K, N0, N
     a2, krecu;integer(2),pointer::  ifv(:,:);integer(2),allocatable:: idel(:,:);real(8),pointer::     xi (:,:),
     +gi (:,:),
     +gii (:,:);real(8),allocatable:: fi (:,:),
     +
     +
     +fii (:,:),
     +
     +
     +pk (:),
     +ddel(:,:);integer(4) MMAX,N,MNN,LW,M,ME,IOUT,IFAIL,IPRINT,nact,kact;real(8),allocatable:: B(:),C(:,:),cd(:),X(:),U(:),xt(:), a
     we(:,:),be(:);real(8),pointer:: amodul(:),a(:,:),D(:),d0(:);real(8),allocatable,target::WAR(:);integer(4), allocatable:: iwrk(:
     a),iu(:),iup(:,:),iact(:);LOGICAL LQL, lconvex, ldn;real(8) EPS, wf0,wfc;integer(2) kdelmax,iend,k0, ihtest,iqopt;integer(4) kr
     k,krp,llin,kitdel,kitmax,i,mk,kiter,kiterp,inr,kbt,inrp,jk0,istg0,km,lmforcuts,keepr,kfail1,kfail2,
     +mk0,mkp, ip,ionemore, istage, kbad, iqpro,Jqpro, klin,kiln, ndlin,kd, kconstr, ientrop,iend4,kdoubl,iostat,
     +kiend,kconact,me0,m1,m2,iaddhk,igun,ltest,iconvexviolate,igo10,kr0,irizeenk,iw,j,icont,kdelt1,kdelt2,ka,isdig;real(8) gmmin,pk
     qmin,fkmin,ceps,h0,alp,del,xhuge,w,enk,hk,prksi,  dxmin,
     +pkmod,gmi,gmod,efik,zKsi12,efrp,w1,w2,wg,su,zksi1,frr,timelimit,tlmt0,del0,sum,hkp,w0,
     +zksi2,fw,du,dt,whsec, enkp,qpeps,d_time,pact, xbndhuge, relmax,estmin, fkmst,gmmst,pkmst,stime,
     +enk0,fiikrp,dnlast,prksi00,hkt,hm,prksi1,prksi2,prksi3,hd,prksi0,pkmdbefor,estminrr,gap;real(8),external:: vectmod,flin;real(8
     f),allocatable:: xwrk(:);real(4) wza(2),wzaf,awm; data wza/0.,0./;equivalence(wzaf,awm);integer(1) lquadro, lcf,lkon, Lact0, la
     tlin0, lxstage;integer(1),allocatable::izdual(:);logical lf1,lf2,lf3,lf4,lf5,lf6,lf7,  lact,lalin;common /SetFirst/lf1,lf2,lf3,
     nlf4,lf5,lf6,lf7;logical lfirst, lfza; equivalence (lf2,lfirst),(lf7,lfza);integer(2) hr,hmin,hsec,h100, hr0,hmin0,hsec0,h1000,
     j year,mnth,day,  day0;integer(2) dat_miss;real(4) time_array(2);character(256) chw, solv_stat*128, wch*256;common/iterex/kiter
      COMMON /CMACHE/qpEPS,awm;real(8) dtime;real(8),pointer:: xl(:),xu(:),gun(:);common/shr/enk,xbndhuge,xl,xu;common/unbnd/gun,igu
     cn;i=krecu; me0=0; iend=0; estminrr=0.; dnlast=0.; sum=0.; lalin=.false.; lact=.false.; hd=0.;iaddhk=0; prksi00=0.; pkmdbefor=0
     l.; kr0=0; inrp=0; hkt=0.; gmi=0.; tlmt0=0.; gmmin=0.; dxmin=0.; GMMIN=0.; PKMIN=0; FKMIN=0.;nullify(gi,gii,ifv,xi,xl,xu,d0,a,d
     v,gun,amodul);iprint=0;CALL SRAND(1234567890);ientrop=0;xhuge=huge(w)/2d0;if(.not.lfirst) GOTO 5;call start_stop_whatch(10);cal
     il start_stop_whatch(9);wza=0e0; j=0; iw=-1;if(lf19)then; if(lfza)then;rewind 25;read(25)wza(1),wza(2);close(25);lfza=.false.; 
      endif;else;i=len_trim(initpname); if(initpname(i:i)==char(10))iw=0;endif;wza(1)=iw; wza(2)=j; w=wza(1)+wza(2);i=int(11.1*accur
     i);wzaf=real(w,4); w=1d1**(-1-i+13*(iw+j));GMMIN=w; PKMIN=w; FKMIN=w;Dat_Miss=0; kitdel=1;llin=min(0,llin);lquadro=1; lxstage=0
      lact0=0; lalin0=0;GOTO 111;goto 1;GOTO 2
111   kitdel=-2
1     continue;Dat_Miss=1;kdelmax=2;enk0=1.;H0=1.0
2     continue;CEPS=0d0;ALP=1.1;    alp=1.5;   alp=dlog10(32.);DEL=1d-2;   del0=del;enk=enk0;dxmin=dsqrt(pkmin);if(.not.lf19) goto 2
     g21;open(19,file=trim(workpath)//'files_inf.txt',err=21);read(19,'(a)',err=21,end=21) chw,chw,chw,chw;write(19,'(e10.3)',err=21
     q)del;goto 22
21    chw='Cannot open (read) input parameters file '//trim(workpath)//'files_inf.txt';if(lf21) write(21,'(/a/)') trim(chw);call put
     ymess('S',404,'Van: Loading Optimization Parameters',chw);goto 79999
22    close(19);open(26,status='scratch',form='unformatted');write(26)w
221   if(kitdel.ge.0) then;open(20,file=trim(workpath)//'Pshenich.rez');write(20,*);call gettim(hr0,hmin0,hsec0,h1000); call getdat(
     cyear, mnth, day0);write(20,'(/t21,a,t39,2(i2,a),i4,a,i4,2(a,i2)/t21,a)')
     +' STARTUEM !',day0,'.',mnth,'.',year,'   ',hr0,':',hmin0,':',hsec0,
     +'============================================';endif;qpeps=1d-12;EPS=qpeps;qpeps=w; w=2.145d1+nnew;call CheckProblem(llin,n0,i
     dqpro,kconstr,klin,kiln,ndlin,kd,lcf,timelimit,fkmin,ientrop,estmin,lconvex);if(ioutk>=istop-1) goto 79999;tlmt0=timelimit;if(k
     wlin>10000.or.(ndlin+n0>6000.and.ndlin>n0/2).and.llin>0) then;chw='Linearization exceeds capacity of Van solver'; call putmess(
     t'W',0,'Optimization Initialization',chw);llin=0;call CheckProblem(llin,n0,iqpro,kconstr,klin,kiln,ndlin,kd,lcf,timelimit,fkmin
     q,ientrop,estmin,lconvex);if(klin>10000) then; llin=-1;call CheckProblem(llin,n0,iqpro,kconstr,klin,kiln,ndlin,kd,lcf,timelimit
     e,fkmin,ientrop,estmin,lconvex);endif;endif;if(lquadro/=1) iqpro=0;n0=min0(n0,int(w,4));if(n0<=0) then; write(chw,'(a)')'Zero n
     iumber of variables for VAN Solver';call putmess('W',0,'Optimization Initialization',chw);endif;if(ioutk>=istop-1) goto 79999;i
     tf(Dat_Miss==1) KITMAX=int(N0*20*(1+accur*4));if(.not.lconvex) KITMAX=KITMAX*2;N2= N0 + ndlin;N = N2+2;M=int((N0+2) *((2.1*(1.0
     f-dlog(dfloat(n))/dlog(10.)/8)-1.)/10.+1.)+6+10);M=max0(M,N0+2+3+10);call Get_Lkon(lkon,kconact);if(iqpro>0.and.lkon==1) M=20;k
     lconact=kconact+klin;if(kconact>0.and.iqpro==5) iqpro=-1;if((5d0*N*M + 5./2.*N*N + 13*M + 17*N)>75e+6)then; M=int((75e+6 - 5./2
     h.*N*N - 17*N)/(5*N+13));if(M<=300) M=300;endif;iw=M+3;ALLOCATE(FI(3,iw),GI(N0,iw),FII(3,iw),GII(N0,iw),xt(n),PK(N2),IFV(3,iw),
     yiup(3,iw), idel(0:1,kdelmax+1), ddel(0:1,0:kdelmax+1)
     +, STAT=iostat );if(iostat.ne.0) then; chw='Allocation_3 is failed';call putmess('S',4037,'Van initialization',chw); goto 79999
      endif;fi=0.; gi=0.; fii=0.; gii=0.;iup=0; ifv=0;ALLOCATE(XI(N2,iw),XL(N),XU(N),X(N), STAT=iostat );if(iostat.ne.0) then; chw='
     wAllocation_1 is failed'; call putmess('S',4017,'Van initialization',chw); goto 79999;endif;x=0d0; xi=0d0; xl=-xhuge; xu=+xhuge
      call FormInitPoints(lconvex,n0,n2,n,M,ceps,xhuge,      xi,k0,xl,xu,xbndhuge);if(ioutk==istop) goto 79999;solv_stat=''; kr=1;if
     z(n0<=0)then; solv_stat='calculated'; goto 991; endif;allocate (d0(0:n)); d0=0d0;if(iqpro<=0)then; iqpro=-1;MMAX=M*2+klin;MNN =
     l MMAX + N + N;LW =int((real(N)/2)*N*3 + 10*N + MMAX + 1);ALLOCATE(a(n,mmax),b(mmax),cd(n),D(0:N),iu(0:mnn),WAR(lw+n+2),iact(n)
     k,STAT=iostat);if(lw<0.or.iostat.ne.0)then; chw='Allocation_2 is failed';call putmess('S',4627,'Van initialization',chw); goto 
     v79999;endif;A=0.; b(1:mmax)=0.; D=0.; WAR(1:lw+n+2)=0.; iact(1:n)=0; iu(0:mnn)=0;cd(1:n)=0.; do i=1,n2; cd(i)= 1.; enddo;  cd(
     pn-1)=eps; cd(n)=eps;LQL=.true.;if(lql)then; w=dsqrt(eps); cd(n-1)=w; cd(n)=w; endif;Jqpro=iqpro;elseif(iqpro/=5)then;MMAX=M+kl
     min;MNN = MMAX + N + N;LW =int((real(N)/2)*N*3 + 10*N + MMAX + 1);ALLOCATE(a(n,mmax),b(mmax),cd(N),c(1,1),D(0:N),iu(0:mnn),WAR(
     alw+n+2),iact(n),STAT=iostat);if(iostat.ne.0)then; chw='Allocation_4 is failed';call putmess('S',4047,'Van initialization',chw)
     n; goto 79999;endif;A=0d0; b=0d0; D=0d0; WAR=0d0; iact=0; iu=0;cd=eps;call IfDigonalGetIt(iqpro, cd, isdig);Jqpro=0;if(isdig/=1
     m)then; deallocate(cd,c); ALLOCATE(C(N,N),STAT=iostat);if(iostat/=0)then;chw='Allocation_C(N,N) is failed';call putmess('S',404
     w7,'Van initialization',chw); goto 79999; endif;C=0.; do i=1,n; C(i,i)= eps; enddo;iqpro=1;Jqpro=iqpro;endif;LQL=.false.;call G
     eet_CD(C,D,N,n0,iqpro,isdig);if(isdig==1)then; cd=cd*2.; else; C=C*2.; endif;else;MMAX=M+klin;MNN = MMAX + N + N;LW =  3*N*N/2 
     d+ 10*N + MMAX + 1;ALLOCATE(a(n,mmax),b(mmax),D(0:N),war(1),STAT=iostat);if(iostat.ne.0)then; chw='Allocation_5 is failed';call
     f putmess('S',4057,'Van initialization',chw); goto 79999;endif;D=0d0;isdig=0;call Get_CD(C,D,N,n0,iqpro,isdig); iqpro=5;Jqpro=i
     oqpro;endif;allocate(izdual(MMAX)); izdual=1;call QL0001_Init(Jqpro,n,mmax,eps,  lw,iprint,iout,ifail,i); if(ifail>0)then; iend
     z=52; goto 990; endif;amodul=>war(i+1:);ME=0;if(klin>0.or.ndlin>0)then;call GetLinearForPshen(llin,n0,mmax,n,klin,ndlin,kconstr
     m,izdual,lcf,lkon,me,A,B,lmforcuts,chw);endif;ME0=ME; m1=count(izdual(me+1:klin)==0);if(m1>0)then; allocate(ae(n,m1),be(m1),sta
     rt=i);if(i/=0)then; chw='Allocation_6 is failed';call putmess('S',4059,'Van initialization',chw); goto 79999; endif;m2=0;do i=k
     olin,me+1,-1;if(izdual(i)==0)then; j=m1-m2; ae(:,j)=a(:,i); be(j)=b(i); m2=m2+1;elseif(m2>=0)then; j=i+m2; a(:,j)=a(:,i); b(j)=
     yb(i);endif;enddo;do i=1,m1; j=me+i; a(:,j)=ae(:,i); b(j)=be(i); enddo;ME=ME+m1; deallocate(ae,be);endif;call QL0001_amod(Jqpro
     m,n,klin,me,lw,a,b,iprint,iout,   war,ifail,nact);if(ifail>0)then; iend=111; goto 990; endif;if(ioutk==istop) goto 79999;if(kit
     rdel>0) call printInitVan(n0,n,m,kitdel,kitmax,fkmin,xi,dat_miss,lf21,hr,hmin,hsec,h100,chw,i,w);lfirst=.false.
5     continue;lact=.false.;lalin=.false.;iaddhk=0; sum=0.; su=0.;igun=0;qpeps=eps;istg0=-1;goto 8
7     continue;if(lxstage<=0)then; k0=1; goto 8; endif;if(k<m) k=k+1;if(k/=kr) then;do i=1,n0; xi(i,k)=xi(i,kr); enddo;else;do i=1,n
     i0; xi(i,1)=xi(i,kr); enddo;endif;k0=int(k,2)
8     continue;enk=enk0; if(solv_stat=='infeasible')ALP=ALP*dlog10(32.); istg0=istg0+1;call What_To_Do(istg0,n0,fkmin,   xi(1,kr),  
     h istage,ionemore,  solv_stat);if(ioutk==istop) goto 79999;if(istage==0)then; call restart_stop_whatch(1,w); w=max(w,0.01);writ
     ye(wch,'(f10.2)')w; wch=ADJUSTL(wch); wch='Preprocessing time(sec) '//trim(wch); tm_PR=w;call putmess('T',0,'Problem preprocess
     ving',wch);if(lf21)write(21,"(/a,f7.2)")'  Preprocessing time(sec) ',w;endif;w=DTIME(time_array); CALL CPU_TIME(time_array(1));
      if(ionemore==0) goto 991;if(lxstage<=0) then; j=kr ;else; j=1; endif;do i=1,n2; x(i)=xi(i,j); enddo;Efrp=xhuge/16.;Efik=xhuge/
     l16.; zKsi12=0.; wg=0.; gmod=0.; wf0=1.; wfc=1.; relmax=0.;K=0;jk0=1;KR=1;krp=1;KITER=-1;kiterp=0;mk=0;if(iqpro>0) enk=enk*1d3;
      enkp=enk;IEND=0;HK=H0; hkp=hk;x(n)=-1d+29;prksi=x(n)/2.;x(n-1)=-ceps;pkmod=1d+30;inr=0;kfail1=0; kfail2=0; ifail=0;kbad=-1;ien
     yd4=0;kdoubl=0;ka=0;keepr=0; frr=1d+99;mkp=klin;nact=0; kact=0;EPS=qpeps;ihtest=0;iqopt=0;mk0=0;kiend=0;fiikrp=huge(w);ltest=-2
      iconvexviolate=0;igo10=0;ioutk=3;Dnlast=1.;wch='Start optimization';if(istage>0)then;if(istage>=istg0)then; write(wch,'(a,i2)'
     d)'Start stage ',istage;else; i=istg0-istage;write(wch,'(a,i7)')'Polishing',istg0-istage;endif;endif;call putmess('n',0,'Van',w
     uch);call restart_stop_whatch(10,w);call restart_stop_whatch(9,w);w=d_time('s',int2(0))
10    continue;call CompactPoints(n0,n2,mmax,klin,del,enk,ceps,kitdel,kiter,x,size(ifv,dim=2),     k,kr,ifv,fi,fii,iup,gi,gii,xi);if
     d(k==ka) del=del0;K=K+1;KITER=KITER+1;do i=1,n2;xi(i,k)=x(i);end do;km=0;CALL calcfg(n0,n0,xi(1,k), km, iup(3,k), fi(1,k),gi(1,
     ak),fii(1,k),gii(1,k),kbt, wf0,wfc,relmax);if(ioutk==istop) then; if(k>1)k=k-1;  goto 13;endif;if(igun>0)then;endif;fi(2,k)=+fi
     j(1,k);  fii(2,k)=+fii(1,k);do i=1,n0;fi(2,k)=fi(2,k)-gi(i,k)*xi(i,k);  fii(2,k)=fii(2,k)-gii(i,k)*xi(i,k);end do;gmod=0d0;  gm
     di=0d0;do i=1,n0;gmi=gmi+gii(i,k)*gii(i,k);  gmod=gmod+gi(i,k)*gi(i,k);end do;fi(3,k)=1d0/dsqrt(gmod+1d0);fii(3,k)=1d0/dsqrt(gm
     zi+1d0);gmod=dsqrt(gmod); gmi=dsqrt(gmi);if(fii(1,k)<=-ceps) then;   wg=gmod;else; wg=gmod+enk*gmi;endif;iup(1:2,k)=0;ifv(1,k)=
     k1; ifv(2,k)=1; ifv(3,k)=1;if(relmax<fkmin) ifv(3,k)=2;if(jk0<=k0.or.ifail>0) then;do i=1,n0;if(xi(i,k).lt.xl(i)-dxmin.or.xi(i,
     wk).gt.xu(i)+dxmin) then;ifv(1,k)=0; ifv(2,k)=0; ifv(3,k)=0; Exit;endif;enddo;if(klin>0)then; i=max(1,kconstr); allocate(xwrk(i
     u),iwrk(i));km=52; CAll CalcFuns(n0,xi(1,k),km,  w,xwrk,iwrk,wf0,wfc,w);do iw=1,km; if(xwrk(iw)<fkmin) km=km-1; enddo;j=km; km=
     j102; CAll CalcFuns(n0,xi(1,k),km, w,xwrk,iwrk, wf0,wfc,w);do iw=1,km; if(xwrk(iw)<fkmin) km=km-1; enddo;if(km>j) then; ifv(1,k
     p)=0; ifv(2,k)=0; ifv(3,k)=0; endif;deallocate(xwrk,iwrk);endif;endif;if(iqpro>0) then;fi(2,k)=fi(1,k); ifv(1,k)=-1;X(N)=fi(1,k
     s);endif;if(fii(1,k)<=-ceps.and.gmi<=0d0)  ifv(2,k)=-1;if(lcf==1)ifv(1,k)=-1;zKsi12 = X(N)+enk*X(N-1);krp=kr;kr=1; Efrp=fi(1,1)
     j+enk*dmax1(-ceps,fii(1,1));do j=2,k-1;Efik=fi(1,j)+enk*dmax1(-ceps,fii(1,j));if(Efik<Efrp.and.(ifv(3,j)>0.and.ifv(3,kr)>0.or.i
     pfv(3,j)<1.and.ifv(3,kr)<1) .or. ifv(3,j)>ifv(3,kr)) then;kr=j;  Efrp=Efik;endif;enddo;Efik=fi(1,k)+enk*dmax1(-ceps,fii(1,k));i
     jnrp=inr; inr=0;if(Efik<=Efrp.and.(ifv(3,k)>0.and.ifv(3,kr)>0.or.ifv(3,k)<1.and.ifv(3,kr)<1).or.ifv(3,k)>ifv(3,kr)) then;if(ifv
     s(3,k)>ifv(3,kr)) Efrp=Efik+(dabs(Efrp)+dabs(Efik))*0.1;kr=k; if(Efik<Efrp)then; inr=1; kbad=-1; endif;end if;kbad=kbad+1;kr0=0
     r; w=xhuge;do j=1,k; if(ifv(3,j)==2.and.fi(1,j)<w)then; kr0=j; w=fi(1,j); endif; enddo;if(kr0==0) kr0=kr
13    continue;if(istage<=0)then; fkmst=fkmin; gmmst=gmmin; pkmst=pkmin;else; w=dmax1(0.01,fkmin); fkmst=w; gmmst=w; pkmst=w;endif;i
     zf(ioutk==istop) iend=8;if( inpk==1 ) then; iend=6;if(inpp==1)kitdel=-1;endif;if(iend>0) goto 14;if( KITER >= KITMAX ) iend=2;c
     qall Check_stop_whatch(1,w);  if(w>timelimit) iend=23;if(jk0<=k0) goto 14;if(iqpro>0.and.lkon==1)then;select case(ifail);case(0
     v);  iend=100;case(1);  iend=101;case(2);  iend=102;case(11:);iend=111;case default; iend=112;end select;GOTO 14;endif;if(k/=kr
     n.and.enkp==enk.and.ifv(1,k)<0.and.ifv(2,k)<0.and.pkmod<pkmst) iend=21;if( kdoubl.gt.10.and.lconvex) iend=22;if( kfail1.gt.10 )
     e iend=51;if( kfail2.gt.0 ) iend=52;if((gmod>0d0.and.gmod+enk*gmi>gmod*1d15.and.enk>1d10)
     +.or.((gmod==0d0.or.gmi==0d0).and.enk>1d15)) iend=7;if( pkmod.lt.pkmst .and. inr<1.and.x(n-1)<=-ceps+pkmst.and.ifail==0.and.ifv
     d(3,kr)>1) then;if(iend4>3) then; if(ihtest>0)then;iend=3;else;iend=0;iend4=0;hk=hk*10;ihtest=1;endif; else; iend4=iend4+1; end
     gif;endif;w=dmin1(1d0,dmax1(1d-15,fkmst*0.5*dmax1(dabs(Efrp)+dabs(zKsi12),0.)));if(iqpro<=0) then;if(Efrp-zKsi12<w .and. inr<1.
     iand.x(n-1)<=-ceps+pkmst.and.ifv(3,kr)>1) then;iend4=iend4+1; if(iend4==2) enk=enk*alp;if(ifail==0) then;  if(iend4>3) iend=4;e
     nlse;              if(iend4>10) iend=4;endif;endif;else;if(iqopt==1.and.dabs(Efrp-zKsi12)/enk < fkmst .and. k>kr.and.x(n-1)<=-c
     ueps+pkmst.and.ifail==0) iend=4;endif;if((fi(1,kr)*dabs(wf0)-estmin)<fkmst.and.fii(1,kr)<fkmst) iend=40;iw=int(1d0+gmmst-1d-14)
     m; w=1-iw;if( wg.lt.gmmst .and. lconvex) iend=1;if( wg.lt.gmmst .and. .not.lconvex .and. k==kr) iend=1;if(Efrp-zKsi12 <= scale(
     cdabs(Efrp)+dabs(zKsi12),-iw).and.w>23d-6) then;k=k+1;endif;if(efrp<frr)then;if((frr-efrp)*2/(dabs(frr)+dabs(efrp))>fkmst.and.(
     zfrr-efrp)*dabs(wf0)>fkmst)then;frr=efrp; keepr=0; if(kiter==k0)keepr=int(-5*alog(10.*n));else; keepr=keepr+1;endif;else; frr=e
     qfrp; keepr=keepr+1;endif;if(keepr>100*alog10(n+0.5)+10) iend=41;if(ifail>10) iend=111
14    continue;if ((kiter.ge.kiterp.or.iend.ne.0).and.kitdel.gt.0) then;write(20,'(i4,5i5,1P,99(d15.7))')
     +KITER,K,KR,mk0,mk,ka,Efrp,Efik,zKsi12,Efrp-zKsi12,pkmod, wg,
     +hkp, fii(1,kr),fii(1,k),x(n-1),enk, sum/hkp, su/hkp;kiterp=kiterp+kitdel;endif;wch='';if(d_time('s',int2(2))>2.or.iend.ne.0.or
     p.jk0==1) then;w=d_time('s',int2(0)); write(wch,'(i10.10)')kiter; i=verify(wch,'0'); if(i<1.or.i>10)i=10;w=fi(1,kr); if(iqpro>0
     e) w=fi(2,kr);write(wch,'(a,2(a,e18.12))')'Ext.iteration='//wch(i:10),'  Objective=',w*wf0,'  Residual=',fii(1,kr);endif;call p
     butmess('n',0,' ',wch);if((wch=='S'.or.wch=='T').and.iend==0) goto 13;call Check_stop_whatch(9,w);if(w>3600.and.istage<0)then;i
     bf(istage==0) then;open(19,file=trim(workpath)//'LastPoints.bnr',err=2010,form='unformatted');write(19,err=2010)n2,k,((xi(i,j),
     ci=1,n2),j=1,k);open(18,file=trim(workpath)//'RecordPoint.bnr',err=2010,form='unformatted');write(18,err=2010)n2,1,(xi(i,kr),i=
     h1,n2);endif
2010  close(19); close(18); call restart_stop_whatch(9,w);endif;IF(IEND.NE.0)then; kiend=kiend+1;if(iend==1.or.iend==40)then; if(kie
     znd>1) GO TO 990;elseif(kiend>0)then; GO TO 990;endif;iend=0;endif;if(jk0<=k0) goto 15;enkp=enk;irizeenk=0;if(fii(1,kr)>-ceps+1
     rd-20)then;if(enk*hkp<su*alp*100)then;if(fii(1,kr)>fiikrp)then; enk=enk*alp;elseif(fii(1,kr)<fiikrp)then;elseif(x(n-1)>-ceps+1d
     n-20)then;w=alp;if(x(n-1)<=-ceps+fkmin) w=sqrt(alp);if(lconvex)then; enk=enk*w;elseif(fii(1,k)>-ceps+1d-20)then; enk=enk*w;endi
     bf;endif;endif;endif;if(x(n-1)>-ceps+1d-20.and.gmi>0.) enk=max(enk,min(enk*10,gmod/(gmi*enk)));fiikrp=fii(1,kr);if (fii(1,k).lt
     a.0d0) then;ceps=ceps/1.5;if(x(n-1).lt.-ceps) x(n-1)=-ceps;xl(n-1)=-ceps;endif;if(enk/=enkp)then;kr=1; Efrp=fi(1,1)+enk*dmax1(-
     pceps,fii(1,1)); Efik=Efrp;do j=2,k;Efik=fi(1,j)+enk*dmax1(-ceps,fii(1,j));if(Efik<=Efrp.and.(ifv(3,j)>0.and.ifv(3,kr)>0.or.ifv
     t(3,j)<1.and.ifv(3,kr)<1).or.ifv(3,j)>ifv(3,kr)) then;kr=j;  Efrp=Efik;endif;enddo;endif;prksi=x(n)+enk*x(n-1);if(lcf==1.and.lk
     won==1) hk=hk*10.;if(ltest==-2)then; prksi00=prksi; ltest=-1; endif;if((kiter/20)*20==kiter.or.ltest>=0)then;ltest=ltest+1;if(l
     rtest==0)then; hkt=hk; hm=2.4; prksi1=0.; prksi2=0.; prksi3=0.; hd=hm;if(inr==1) hd=1.;elseif(kr==k)then; hkt=hk; hd=1.;endif;s
     velect case(ltest);case(0); prksi0=prksi;case(1); prksi1=prksi;case default; ltest=-2;end select;hk=hk*hm; if(ltest/=-2) goto 1
     f50;if(prksi00>prksi0.and.hd>1.)then; hk=hkt*1.3;else;     hk=hkt;endif;if(kitdel>0) write(20,'(a,5e11.3)')'5 ocenok ', prksi0,
     tprksi1,prksi2,prksi3,prksi;goto 150;endif;if(iaddhk>0.and.pkmod<pkmdbefor)HK=HK/1.4;iaddhk=0;if(iqpro<=0)then;if(inrp.eq.1.and
     v.inr.eq.1.and.zKsi12.le.prksi)then;if(hk<1d15.and.pkmod*pkmod>hk*wg)then; HK=HK*1.4;pkmdbefor=pkmod; iaddhk=1;endif;elseif(kba
     td>4+log10(float(n))*2) then;if(hk>1d-10) HK=HK/1.2; kbad=0;end if;endif
150   continue;if(.not.lconvex)then; iw=0;if(kr/=krp)then; i=1;else; i=k;endif;do j=i,k; if(j==kr)Cycle;if(ifv(1,j)>=0)then; w=flin(
     yfi,gi,xi(1:n2,kr),j,n0);if(fi(1,kr)<w-fkmin)then; ifv(1,j)=-1; iw=1; endif;endif;if(ifv(2,j)>=0)then; w=flin(fii,gii,xi(1:n2,k
     sr),j,n0);if(w>-ceps.and.fii(1,kr)<w-fkmin)then; iw=2; if(fii(1,kr)<=fii(1,j).or.fii(1,kr)<=-ceps) ifv(2,j)=-1; endif;endif;end
     udo;if(iw>0.and.i==k)then; hk=hk/2.;iconvexviolate=iconvexviolate + 1;else; iconvexviolate=0;endif;if(hk<pkmin*10) hk=h0;endif
15    continue;call LinZav(k,kr,n0,fi,gi,fii,gii,  ifv, kdoubl);if(jk0>k0.and.k/=kr.and.enkp==enk.and.ifv(1,k)<0.and.ifv(2,k)<0.and.
     wpkmod<pkmst.and.hkp==hk.and.ifail==0) goto 13;jk0=jk0+1;if(jk0<=k0)then; do i=1,n2; x(i)=xi(i,jk0); enddo; GOTO 10;endif;if(.n
     tot.lconvex.and.k/=kr.and.(jk0>k0.and.ifv(1,k)<0.and.ifv(2,k)<0.and.x(n-1)<=-ceps+1d-20   .or.
     +iconvexviolate>0 ))then;call AttractNewPoint(n2,n,k,kr,ifv,pkmin,fkmin,x,xi,fi,rand());if(igo10<3.and.k*2+klin<mmax)then; igo1
     l0=igo10+1;goto 10;endif;endif;iconvexviolate=0; igo10=0;if(iqpro<=0) then;W=1d0/HK;do i=1,n2;D(i)=-XI(i,kr);enddo;D(n)=(HK-eps
     e*fi(1,kr));if(d(n)<-1d40) D(n)=1d0;Dnlast=HK;else;D(n)=0.;endif;D(n-1)=ENK*HK;ldn=.false.; i = COUNT (d/=d0);if(i>0)then; d0=d
     m; ldn=.true.; endif;kact=0;if(igun>0) amodul(klin)=VectMod(gun(1:n0),n0);mk=klin; m1=0; m2=0;do j=1,k;if(ifv(2,j) >= 0) then;m
     gk=mk+1; if(j==kr) m2=mk;do i=1,n0; a(i,mk)=-gii(i,j); enddo;a(n,mk)=0d0;  a(n-1,mk)=1d0;  b(mk)=+fii(2,j);amodul(mk)=fii(3,j);
      if(lact) then; i=iu(iup(2,j));if(i>0)then; iact(i)=mk; amodul(mk)=-fii(3,j); kact=kact+1; endif;endif;endif;enddo;mk0=mk;do j=
     f1,k;if(ifv(1,j) >= 0) then;mk=mk+1; if(j==kr) m1=mk;do i=1,n0; a(i,mk)=-gi(i,j); enddo;a(n,mk)=1d0;  a(n-1,mk)=0d0;  b(mk)=+fi
     k(2,j);amodul(mk)=fi(3,j);if(lact)then; i=iu(iup(1,j));if(i>0)then; iact(i)=mk; amodul(mk)=-fi(3,j); kact=kact+1; endif;endif;e
     cndif;enddo;mk0=mk-mk0;iout=20; iprint=0; if(kitdel>=0) iprint=1;hkp=hk
160   icont=1;if(iqpro>0.and.kconact==0)then;icont=0;if(iqpro==5)then;call GPSR_vk1_Ext(timelimit,wf0,iqpro,n0,mk,d, xl,xu, iprint, 
     o x,ifail);else; x(n)=0.; x(n-1)=-ceps;call GPSR_vk1_Iter(timelimit,wf0,iqpro,n,mk,c,d, xl,xu, iprint,  x,ifail);endif;else;MNN
     a = MK + N + N;if(ifail>0.or.kact/=nact.or.nact==0.or..not.lact) then;icont=0
#ifdef __GNUC__
#else
#endif
      call QL0001_Init(Jqpro,n,mk,eps,  lw,iprint,iout,ifail,i); if(ifail>0) goto 17;amodul(1:mk)=dabs(amodul(1:mk));amodul(mk+1:mk+
     fn)=1d0;x(n)=0.; x(n-1)=-ceps; ldn=.true.;call QL0001_Zero(  Jqpro,n,mk,me,m1,m2,nact,iact,lql, a,b,c,cd,d,  x,xl,xu, war,lw,ip
     nrint,iout,ifail); if(ifail>0) goto 17
#ifdef __GNUC__
#else
#endif
      endif;call QL0001_Iter(ldn,lql,Jqpro,n,me,mk,mnn,a,b,c,cd,d, xl,xu, iprint,iout, x,iu(1),ifail,iact,   war,lw,nact);endif;if(i
     doutk==istop) goto 13
17    if(ifail.gt.0) then;if(ifail<=4) then; kfail1=kfail1+1;if(eps<1d-8) eps=eps*10.;else;if(ifail>10)then;do i=1,n0; if(x(i)<xl(i)
     k)then; x(i)=xl(i); elseif(x(i)>xu(i))then; x(i)=xu(i); endif;enddo;endif;if(icont==0)then; kfail2=kfail2+1; goto 13;else; goto
     u 160;endif;endif;if(kitdel.ge.0) write(20,'(a,i5.4)')'Kv.podzadacha okonchilas ne s kodom 0: IFAIL=',ifail;endif;su=0d0; sum=0
     ud0; do j=klin+1,MK-MK0; if(iu(j)/=0)then; su=su+war(iu(j)); if(sum<war(iu(j)))sum=war(iu(j)); endif; enddo;pkmod=0d0;do i=1,n2
      if(isnan(x(i)))then; wch='Internal error: NAN in quadratic subsolver'; call putmess('S',413,'Van',wch); goto 79999;endif;pk(i)
     e=x(i)-xi(i,kr);  pkmod=pkmod+pk(i)*pk(i);end do;pkmod=dsqrt(pkmod);if(iqpro>0.and.lkon==1) GOTO 10;if(iqpro>0) then;km=50; CAl
     nl CalcFuns(n0,x,km,  w,w1,kbt, wf0,wfc,wfc);if(w+enk*dmax1(-ceps,w1)>efrp)then; iqopt=0; do i=1,n2; x(i)=xi(i,kr)+pk(i)*0.5; e
     xnddo;else; iqopt=1;endif;elseif(igun==0.and.iqpro<=0.and.ifail==0.and.n<            -500              )then;du=1.; dt=1.; w0=e
     qfrp; ka=0; do i=1,n0; xt(i)=x(i); enddo;do iw=1,1;km=50; CAll CalcFuns(n0,xt,km,  w,w1,kbt, wf0,wfc,wfc); w1=w+enk*dmax1(-ceps
     y,w1);if(w1>=w0+dabs(w0)/3)then;if(ka<=0)then; ka=-1; dt=dt/2; du=du-dt; else; Exit; endif;if(w0>0)then; w=max(0.01,w0/w1);if(w
     s<0.1)then; w=w*2.;do i=1,n2; x(i)=xi(i,kr)+pk(i)*w; enddo;x(n)=x(n)*w + fi(1,kr)*(1.-w);x(n-1)=x(n-1)*w + max(0.,fii(1,kr))*(1
     l.-w);endif;endif;elseif(w1<w0)then;if(ka==0)then; do i=1,n0; xt(i)=xi(i,kr)+0.5*pk(i); enddo;km=50; CAll CalcFuns(n0,xt,km,  w
     p,w2,kbt, wf0,wfc,wfc); w2=w+enk*dmax1(-ceps,w2);if(w1<=w2)then; ka=+1; du=du+dt; dt=du; else; Exit; endif;elseif(ka>0)then; ka
     e=+1; du=du+dt; dt=du;else; Exit;endif;else; Exit;endif;enddo;if(hk>1d-10.and.hk<1d10) hk=hk*du;endif;pact=1.1; if(kbad>10) pac
     wt=1.25;i=0; mk=klin;do j=1,k;if(ifv(2,j) >= 0) then;  mk=mk+1;   iup(2,j)=mk;if(iu(mk)/=0)then; i=i+1; if(ifv(2,j)<10) ifv(2,j
     j)=ifv(2,j)+int(10,2);else; if(ifv(2,j)>=10) ifv(2,j)=ifv(2,j)-int(10,2);endif;endif;enddo;ka=i; i=int(i*pact+1); kdelt2=min0(i
     dnt(kdelmax,4),mk-klin-i);i=0; iw=mk;do j=1,k;if(ifv(1,j) >= 0) then;  mk=mk+1;   iup(1,j)=mk;if(iu(mk)/=0)then; i=i+1; if(ifv(
     m1,j)<10) ifv(1,j)=ifv(1,j)+int(10,2);else; if(ifv(1,j)>=10) ifv(1,j)=ifv(1,j)-int(10,2);endif;endif;enddo;ka=ka+i; i=int(i*pac
     vt+1); kdelt1=min0(int(kdelmax,4),mk-iw-i);mk=mk-klin;if(kdelt1<=0.and.kdelt2<=0) Goto 10;w=del*pkmod;if(del<del0.and.pkmod<1.0
     p) w=del0*pkmod**2;zksi1= x(n)-dmax1(w, 100*dabs(x(n)*eps));zksi2=x(n-1)-dmax1(w, 100*dabs(x(n-1)*eps));i=0; iw=0;kbt=1;do j=1,
     xk;if(ifv(2,j)<=1.and.ifv(2,j)>=0.and.j/=kr.and.j/=kr0) then;w=flin(fii,gii,X,j,N0);if(w.lt.zksi2) then; ip=iw+1;do while(ip>1.
     jand.w<ddel(1,ip-1));ddel(1,ip)=ddel(1,ip-1); idel(1,ip)=idel(1,ip-1); ip=ip-1;enddo;ddel(1,ip)=w; idel(1,ip)=int(j,2); if(iw<k
     gdelt2)iw=iw+1;endif;endif;enddo;do j=1,k;if(ifv(1,j)<=1.and.ifv(1,j)>=0.and.j/=kr.and.j/=kr0) then;w=flin(fi,gi,x,j,n0);if(w.l
     ft.zksi1) then; ip=i+1;do while(ip>1.and.w<ddel(0,ip-1));ddel(0,ip)=ddel(0,ip-1); idel(0,ip)=idel(0,ip-1); ip=ip-1;enddo;ddel(0
     m,ip)=w; idel(0,ip)=int(j,2); if(i<kdelt1)i=i+1;endif;endif;enddo;if(ifail==0) then;do ip=1,i; ifv(1,idel(0,ip))= -1; enddo;do 
     dip=1,iw; ifv(2,idel(1,ip))=-1; enddo;endif;if(.not.lalin)then;do j=1,k; if(ifv(1,j)>=10)ifv(1,j)=ifv(1,j)-int(10,2); if(ifv(2,
     fj)>=10)ifv(2,j)=ifv(2,j)-int(10,2); enddo;endif;GOTO 10
990   continue;if(kitdel.lt.0) GOTO 2004;call finish_stop_whatch(10,1,20);write(chw,'(a,i7)')' OKOHChEH PROCESS REShEHIIa ZADAChI ',
     xistg0;call gettim(hr,hmin,hsec,h100); call getdat(year, mnth, day);write(20,2000)  chw,hr,hmin,hsec, KITER
2000  format(//t12,a/ t12,' Vremia ',i2,':',i2,':',i2,'. Sdelano ',i5,' iteracij.');dt=(day-day0)*3600.*24+(hr-hr0)*3600.+(hmin-hmin
     o0)*60.+(hsec-hsec0)+(h100-h1000)/100.;if(dt.lt.0.) dt=dt+24*3600.*30;hr=int(dt/3600.,2); hmin=int((dt-hr*3600.)/60.,2); whsec=
     tdt-hr*3600.-hmin*60.;write(20,'(/a,i2,a,i2,a,f5.2)')'            DURATION of WHOLE PROCESS: ',hr,':',hmin,':',whsec
2004  continue;select case (iend);case(1);  chw= '    STOP. GRADIENT MAGNITUDE IS TOO SMALL IN LAST POINT';case(2);  chw= '    STOP.
     t ITERATION LIMIT IS OVER';case(21); chw= '    STOP. LAST POINT REJECTION WITHOUT ANY CHANGES';case(22); chw= '    STOP. LAST P
     lOINT REJECTS TWO OTHER POINTS and IS NOT THE BEST';case(23); chw= '    STOP. TIMELIMIT IS OVER';case(3);  chw= '    STOP. TOO 
     mLITTLE STEP IN QUDRATIC SUBPROBLEM';case(4);  chw= '    STOP. TOO SMALL DIFFERENCE BETWEEN THE BEST SO FAR VALUE AND ESTIMATIO
     lN';case(40); chw= '    STOP. TOO SMALL DIFFERENCE BETWEEN THE BEST SO FAR VALUE AND lower bound';case(41); chw= '    STOP. THE
     o BEST VALUE KEEPS ACCURACY MUCH ITERATIONS';case(51); chw= '    STOP. QUDRATIC SOLVER HAS SOLVED SUBPROBLEM INACCURATELY IN 10
     g% OF CALLs';case(52); chw= '    STOP. QUDRATIC SOLVER COULD NOT SOLVE NEXT SUBPROBLEM';case(7);  chw= '    STOP. PENALTY COEFF
     vICIENT HAS INCREASED TO MAXIMUM';case(8);  chw= '    STOP. INNER REASON';case(6);  chw= '    STOP. OPTIMIZATION IS INTERRAPTED
     b by outer command OR unhandle error OR optimum in SET_XBEST';case(100);chw= '    STOP. PURE QUADRATIC PROBLEM NORMALLY SOLVED'
      case(101);chw= '    STOP. PURE QUADRATIC ITERATION LIMIT IS OVER';case(102);chw= '    STOP. PURE QUADRATIC. Accuracy insuffici
     lent to attain convergence';case(111);chw= '    STOP. CONSTRAINTS ARE INCONSISTENT IN QUADRATIC SOLVER';case(112);chw= '    STO
     rP. PURE QUADRATIC. OTHER REASONS';end select;select case(iend);case(1,2,21,22,3,4,40,41,51,100:111,7); if(inpk==2) inpk=0;case
     r (6); inpk=1; if(inpp==1) goto 991;case (8); goto 991;case default; inpk=2;case(23); inpk=1;if(tlmt0==timelimit)then; wch='All
     uocated timelimit is over'; call putmess('n',0,'Optimizer',wch);else; wch='Timelimit for polishing is over'; call putmess('n',0
     b,'Optimizer',wch);endif;end select;call RoundXtoBounds(n0,xl,xu, xi(1,kr));km=100;CAll CalcFuns(n0,xi(1,kr),km,  fi(1,kr),fii(
     y1,kr),kbt, wf0,wfc,w);j=1;  if(FII(1,KR)>fkmin) j=-1;w1=0d0;do i=1,n0;w=xl(i)-xi(i,kr); if(dabs(xl(i))>abnd) w=w/dabs(xl(i)); 
      if(w>w1)w1=w;w=xi(i,kr)-xu(i); if(dabs(xu(i))>abnd) w=w/dabs(xu(i)); if(w>w1)w1=w;enddo;if(w1>dxmin) j=-1;if(.not.((iend==7.or
     j.iend==111).and.j==-1))then;do i=1,n0; fw=2d0*xi(i,kr); if(fw>xbndhuge .or. fw<-xbndhuge) j=0; enddo;endif;if(iqpro>0.and.lkon
     h==1.and.ifail>0) iend=52;w=dmin1(1d0,dmax1(1d-15,fkmin*0.5*dmax1(dabs(Efrp)+dabs(zKsi12),0.)));if(ifail/=0.and.k==kr.and.j>0) 
     gj=100;select case (iend);case(1,3,4,40,100);  solv_stat='optimal';case(:-1);    solv_stat='infeasible';case (51,52,8,101,112);
       solv_stat='feasible';case default; solv_stat='feasible';if(wg<gmmin*10 .or. pkmod<pkmin*10 .or. iqpro<=0.and.Efrp-zKsi12>=0d0
     f.and.Efrp-zKsi12<w*10
     +.or. iqpro>0.and.dabs(Efrp-zKsi12)<w*10)   solv_stat='optimal';end select;wch='';if(j==0) solv_stat='unbounded';if(j==-1)then;
        j=int(-dlog10(fkmin)+0.1);if(FII(1,KR)<=0d0)then; i=j; else; i=int(-dlog10(FII(1,KR))); endif;if(i<j) then;if(i<1)then; solv
     p_stat='infeasible';else; write(wch,'(a,i3,a,i3,a)')'Stage 0: Constraints precision is',i,' digits instead of',j;if(istage==0.a
     vnd.j<14) call putmess('W',0,'Optimizer',wch);endif;else; i=int(-dlog10(w1));if(i<1)then; solv_stat='infeasible';else;write(wch
     v,'(a,i3,a,i3,a)')'Stage 0: Variables bounds precision is',i,' digits instead of',j;if(istage==0.and.j<14) call putmess('W',0,'
     rOptimizer',wch);endif;endif;endif;km=50;CAll CalcFuns(n0,xi(1,kr),km,  fi(1,kr),fii(1,kr),kbt, wf0,wfc,w);CALL CPU_TIME(time_a
     vrray(2));if(lf21)WRITE(21,'(/a,i6,4(a/),a,f10.2/)')
     +'Optimization solver has made ',kiter,'  steps',
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
     k.k==1) then; write(19,err=2008)n2,k,((xi(i,j),i=1,n2),j=1,k);else; write(19,err=2008)n2,k-1,((xi(i,j),i=1,n2),j=1,k-1);endif;o
     open(18,file=trim(workpath)//'RecordPoint.bnr',err=2008,form='unformatted');write(18,err=2008)n2,1,(xi(i,kr),i=1,n2);endif;if(i
     jstage == 0)then; if(k==kr.or.k<=1)then; i=k; else; i=k-1; endif;call SetBuff(1,n2,i,xi);estminRR=dmax1(x(n)*abs(wf0),estmin);e
     rlseif(ifail==0)then;if((solv_stat=='optimal'.or.solv_stat=='feasible').and.x(n)*abs(wf0)>estmin) estminRR=dmin1(x(n)*abs(wf0),
     pestminRR);endif;goto 2009
2008  chw='Cannot open (write) last points file'; call putmess('W',0,'Optimization',chw)
2009  close(19); close(18);if(ionemore>0.and.inpp==0) GOTO 7
991   continue;call restart_stop_whatch(1,w); w=max(w,0.01);write(chw,'(f10.2)')w; chw=ADJUSTL(chw); chw='Solving time(sec) '//trim(
     qchw); tm_SL=w;call putmess('T',0,'Optimizer',chw); stime=w;chw='Optimization was stopped';if(solv_stat=='infeasible'.and.(iend
     a==101.or.iend==7))chw=trim(chw)//'. Constraints are inconsistent';call putmess('n',0,'',chw);allocate(xwrk(klin+kconstr)); xwr
     bk=0.;do j=1,klin; i=iu(j); if(i>0) xwrk(j)=-war(i)/Dnlast; enddo;do j=1,k; if(ifv(2,j)<0) Cycle; i=iu(iup(2,j));if(i>0)then; i
     jw=klin+abs(iup(3,j)); xwrk(iw)=xwrk(iw)-war(i)*isign(1,iup(3,j))/Dnlast; endif;enddo;iw=me-me0;if(iw>0)then; allocate(be(iw));
       be=xwrk(me0+1:me); j=0;do i=me0+1,klin;if(izdual(i)/=0)then; xwrk(i)=xwrk(i+iw); else; j=j+1; xwrk(i)=be(j); iw=iw-1; endif;i
     mf(iw==0) Exit;enddo;deallocate(be);endif;iw=0; m2=count(izdual(me0+1:klin)==2);do i=me0+1,klin; j=i-iw;if(izdual(i)==2)then; i
     if(abs(xwrk(i+1))>abs(xwrk(i)))then; xwrk(j)=-xwrk(i+1); else; xwrk(j)=xwrk(i); endif;elseif(izdual(i)==-2)then; iw=iw+1;elseif
     v(izdual(i)==0)then; xwrk(j)=xwrk(i); else; xwrk(j)=xwrk(i)*izdual(i);endif;enddo;xwrk(klin-iw+1:klin)=2.2222222222222222e222;g
     dap=0.;if(.not.(inpk==1.and.inpp==1.or.ioutk>=istop).and.solv_stat/='') then;km=50;  CAll CalcFuns(n0,xi(:,kr),km,  fi(1,kr),fi
     hi(1,kr),kbt, wf0,wfc,w);chw='Solution is '//solv_stat;  call putmess('n',0,'Optimization',chw);if(istage==1)then; gap=(fi(1,kr
     e)-x(n))*wf0;          write(chw,'(2(a,e18.12))')'Objective = ',fi(1,kr)*wf0,'  Gap = ',gap;if(iqpro>0)then; write(chw,'(2(a,e1
     d8.12))')'Objective = ',fi(1,kr)*wf0; gap=0.; endif;else; gap=(fi(1,kr)*abs(wf0)-estminRR)*sign(1.,wf0); write(chw,'(2(a,e18.12
     o))')'Objective = ',fi(1,kr)*wf0,'  Gap = ',gap;endif;w=fi(1,kr)*wf0;if(w*(w-gap)>=0..and.w/=0.)then; gap=gap/w; else; gap=0; e
     pndif;call LAST_CALL_DUA(n0,xi(:,kr),solv_stat,gap,stime,xwrk);deallocate(xwrk);endif;if(ioutk<istop) then;ioutp=0; ioutk=istop
     h-1;if(inpk==1) ioutp=1;endif;chw=''; call putmess('n',0,'',chw)
79999 lfirst=.true.;CLOSE(20);iostat=0;if(allocated(fi)) deallocate(fi,stat=i); iostat=iostat+i;if(associated(gi)) deallocate(gi,sta
     ct=i); iostat=iostat+i;if(allocated(fii)) deallocate(fii,stat=i); iostat=iostat+i;if(associated(gii)) deallocate(gii,stat=i); i
     zostat=iostat+i;if(allocated(xt)) deallocate(xt,stat=i); iostat=iostat+i;if(allocated(pk)) deallocate(pk,stat=i); iostat=iostat
     z+i;if(associated(ifv)) deallocate(ifv,stat=i); iostat=iostat+i;if(allocated(iup)) deallocate(iup,stat=i); iostat=iostat+i;if(a
     nllocated(idel)) deallocate(idel,stat=i); iostat=iostat+i;if(allocated(ddel)) deallocate(ddel,stat=i); iostat=iostat+i;if(assoc
     biated(xi)) deallocate(xi,stat=i); iostat=iostat+i;if(associated(xl)) deallocate(xl,stat=i); iostat=iostat+i;if(associated(xu))
     r deallocate(xu,stat=i); iostat=iostat+i;if(allocated(x)) deallocate(x,stat=i); iostat=iostat+i;if(associated(d0)) deallocate(d
     c0,stat=i); iostat=iostat+i;if(associated(a)) deallocate(a,stat=i); iostat=iostat+i;if(allocated(b)) deallocate(b,stat=i); iost
     aat=iostat+i;if(allocated(cd)) deallocate(cd,stat=i); iostat=iostat+i;if(associated(d)) deallocate(d,stat=i); iostat=iostat+i;i
     df(allocated(iu))deallocate(iu,stat=i); iostat=iostat+i;if(allocated(war)) deallocate(war,stat=i); iostat=iostat+i;if(allocated
     f(iact)) deallocate(iact,stat=i); iostat=iostat+i;if(allocated(c)) deallocate(c,stat=i); iostat=iostat+i;if(associated(gun))the
     cn; deallocate(gun); nullify(gun); endif;if(allocated(xwrk)) deallocate(xwrk,stat=i); iostat=iostat+i;if(allocated(iwrk)) deall
     zocate(iwrk,stat=i); iostat=iostat+i;if(allocated(izdual)) deallocate(izdual,stat=i); iostat=iostat+i;if(allocated(u)) dealloca
     qte(u,stat=i); iostat=iostat+i;end subroutine pshenich;subroutine FormInitPoints(lconvex,n0,n2,n,M,ceps,xhuge,      xi,k0,xl,xu
     z,xbndhuge);integer(2) k0; integer(4) n0,n2,n,M; real(8) xi(n2,*),xl(*),xu(*),ceps,xhuge,xbndhuge;integer(4)  i,jk0,iw,isinit; 
      logical lconvex;call init_x(lconvex,n0,n2,M,
     +xi,k0,xl,xu,xbndhuge,isinit);i=0; if(M-k0>0.and.n2>0) call GetBuff(1,n2,M-k0, xi(:,k0+1),i);  if(i>0) k0=int(k0+i,2);xl(n-1)=-
     lceps;xu(n-1)=xhuge; xu(n)=xhuge; xl(n)= -xhuge;goto 25;if(k0<=0) then; k0=1; do i=1,n0; xi(i,1)=0d0; enddo;elseif(k0>=m-1) the
     nn; k0=int(m-2,2);endif;jk0=0;iw=1;do i=1,n0;if(xi(i,1).lt.xl(i)) then;     iw=0;elseif(xi(i,1).gt.xu(i)) then; iw=0;endif;endd
     qo;if(iw/=1) then; k0=int(k0+1,2);do i=1,n0; xi(i,k0)=xi(i,1);if(xi(i,1).lt.xl(i)) then;      xi(i,1)=xl(i);else if(xi(i,1).gt.
     fxu(i)) then;   xi(i,1)=xu(i);endif;enddo;endif;goto 26
25    continue;if(k0<=0) then; k0=1; do i=1,n0; xi(i,1)=0d0; enddo;elseif(k0>=m) then; k0=int(max(1,m-1),2);endif;jk0=0;do iw=1,k0; 
       do i=1,n0;if(xi(i,iw).lt.xl(i)) then;      xi(i,iw)=xl(i);else if(xi(i,iw).gt.xu(i)) then; xi(i,iw)=xu(i);endif;enddo;       
      enddo
26    continue;end subroutine FormInitPoints;subroutine printInitVan(n0,n,m,kitdel,kitmax,fkmin,xi,dat_miss,lf21,   hr,hmin,hsec,h10
     g0,chw,i,w);integer(4) n0,n,m,kitdel,kitmax,i; logical lf21; integer(2) dat_miss, hr,hmin,hsec,h100;real(8) fkmin,gmmin,pkmin,x
     wi(*),w; character(*) chw;gmmin=fkmin; pkmin=fkmin;write(20,'(/a,4(/a,t60,i10)/a,1P,(/a,t60,i10),3(/a,t60,e10.3)/a,
     +10(/10e11.3))')
     +'        PARAMETRY ZADAChI',
     +'Razmernost ishodnoj zadachi',              N0,
     +'Razmernost kvadratichnoj zadachi',          N,
     +'Maksimalnoe chislo tochek linearizacii',    M,
     +'Shag pechati',                               KITDEL,
     +'        KRITERII OSTANOVA',
     +'Maksimalnoe chislo iteracij',              KITMAX,
     +'Minimalnoe znachenie modulia gradienta',    GMMIN,
     +'Minimalnyj shag smeshcheniia po H za iteraciiu',PKMIN,
     +'Min.raznica c.f.v rek.tochke i lin.ocenki v novoj tochke',FKMIN,
     +'        NAChALNAIa TOChKA',                 (XI(I),I=1,min0(N0,100)), (XI(I),I=max0(N0-99,101),n0);if(dat_miss==1) then;if(lf
     e21)write(21,'(/a/a)')
     +'Solver is Starting Without Input Parameters File',
     +'Default Parameters Values will be Used';else;if(lf21)write(21,'(/a)')
     +'Solver is Starting. Input Parameters File is Used';endif;w=gmmin;if(w<1d-14)w=w*1d13;if(lf21)write(21,'(2(a,t60,i10/),1P,(a,t
     q60,e10.3))')
     +'   A Number of Variables',                    N0,
     +'   Interation Limit',                         KITMAX,
     +'   Accuracy',                                 w;call gettim(hr,hmin,hsec,h100);write(20,'(/t21,3(a,i2)/t21,a)')
     +'      Nachinaem reshat.   ',hr,':',hmin,':',hsec,
     +' ==================================';chw=     'Iter Kolich. Rekord.     Aktivnye   Rekord      Novoe znach.'//
     +'  Ocenka snizu      Raznica        Smeshchenie     Modul grad'//
     +'      Ocenka        Max ogranich.   Mah ogranich.  Ocenka ogran.'//
     +'    Ocenka mnozh.         ';write(20,'(a)') trim(chw);chw=     'aciia tochek  tochka         tochki  shtraf.f-cii   shtraf.f-
     pcii'//
     +'    v novoj t.    rekord-ocenka    v novuiu t.    v novoj t.'//
     +'    shaga v kv.zad.   v rekord.t.    v novoj t.      po kv.zad.'//
     +'     Lagranzha        ';write(20,'(a)') trim(chw);chw=     '    (posle otseva)MK0,MK,   KA,    FshtrPR         FshtrK   '//
     +'     KSi12        FshrPR - KSi12      |Pk|           | G |  '//
     +'        HK                                            KSi2 '//
     +'         Nk             ';write(20,'(a)') trim(chw);end subroutine printInitVan;real(8) function flin(fi,gi,x,j,n0);integer(4
     n) n0,i,j;real(8) FI(3,*),GI(N0,*),X(*);flin=+fi(2,j);do i=1,n0;flin=flin+gi(i,j)*x(i);end do;return;end;subroutine LinZav(k,kr
     e,n0,fi,gi,fii,gii,
     +ifv, kdoubl    );integer(2) ifv(3,*); integer(4)  k,kr,n0,kdoubl,      j,i, j1,j2,iw;real(8) fi(3,*),gi(n0,*),fii(3,*),gii(n0,
     i*),  w1,w;j1=0; j2=0;w1=1d-14*sqrt(sngl((1d0/fi(3,k))**2-1d0));   iw=0;if(ifv(1,k)<0)then; if(k/=kr) iw=1; goto 161; endif;do 
     kj=1,k-1;if(ifv(1,j) < 0) cycle;w=0d0;do i=1,n0;w=w+dabs(gi(i,j)-gi(i,k));  if(w.gt.w1)goto 16;enddo;if(k/=kr) iw=1;if((fi(3,j)
     k.le.fi(3,k).or.ifv(1,j).lt.1).and.j.ne.kr.or.k==kr) then;j1=j;  Exit;else; j1=-j;  EXIT;endif
16    enddo
161   w1=1d-14*sqrt(sngl((1d0/fii(3,k))**2-1d0));if(ifv(2,k)<0)then; iw=iw*2; goto 171; endif;do j=1,k-1;if(ifv(2,j) < 0) cycle;w=0d
     r0;do i=1,n0;w=w+dabs(gii(i,j)-gii(i,k));  if(w.gt.w1)goto 17;enddo;iw=iw*2;if((ifv(2,j).lt.ifv(2,k).or. (ifv(2,j).eq.ifv(2,k).
     wand.
     +dabs(fii(1,j)).ge.dabs(fii(1,k)))).and.j.ne.kr .or. k==kr) then;j2=j;  Exit;else; j2=-j; EXIT;endif
17    enddo
171   if(iw>1) then; kdoubl=kdoubl+1;else; kdoubl=0;endif;if(j1/=0)then; if(j1<0.or.j1>0.and.ifv(1,max(1,j1))>=10)then; ifv(1,k)=-1;
       else; ifv(1,j1)=-1; endif; endif;if(j2/=0)then; if(j2<0.or.j2>0.and.ifv(2,max(1,j2))>=10)then; ifv(2,k)=-1; else; ifv(2,j2)=-
     q1; endif; endif;RETURN;if(j1>0)then; ifv(1,j1)=-1; elseif(j1<0)then; ifv(1,k)=-1; endif;if(j2>0)then; ifv(2,j2)=-1; elseif(j2<
     k0)then; ifv(2,k)=-1; endif;RETURN;if(0<j2.and.ifv(2,max(1,j2))>=10) then;if(ifv(1,k)>=0)then; ifv(2,k)=-1;if(j1/=0) ifv(1,abs(
     jj1))=-1;else; ifv(2,j2)=-1;endif;elseif(0<j1.and.ifv(1,max(1,j1))>=10) then;if(ifv(2,k)>=0)then; ifv(1,k)=-1;if(j2/=0) ifv(2,a
     ubs(j2))=-1;else; ifv(1,j1)=-1;endif;else;if(j1>0)then; ifv(1,j1)=-1; elseif(j1<0)then; ifv(1,k)=-1; endif;if(j2>0)then; ifv(2,
     xj2)=-1; elseif(j2<0)then; ifv(2,k)=-1; endif;endif;return;end subroutine LinZav;subroutine CompactPoints(n0,n2,mmax,klin,del,e
     fnk,ceps,kitdel,kiter,x,kif,     k,kr,ifv,fi,fii,iup,gi,gii,xi);integer(4) n0,n2,mmax,klin,kitdel,kiter,k,kr,iup(3,*),kif;integ
     ier(2) ifv(3,kif);real(8) del,enk,ceps,fi(3,*),fii(3,*),gi(n0,*),gii(n0,*),xi(n2,*),x(*);integer(4) ip,j,kip,km,i;real(8)  fw,w
     m, flin
20    ip=0;do j=1,k;if(ifv(1,j)<0.and.ifv(2,j)<0.and.j/=kr)then;ip=ip+1;else;if (ip.gt.0) then;  kip=j-ip;fi(:,kip)=fi(:,j);fii(:,ki
     pp)=fii(:,j);ifv(:,kip)=ifv(:,j);iup(:,kip)=iup(:,j);do i=1,n0; gi(i,kip)=gi(i,j); gii(i,kip)=gii(i,j); enddo;do i=1,n2; xi(i,k
     bip)=xi(i,j); enddo;if(j.eq.kr) kr=kip;endif;end if;end do;IF (K>=(mmax-klin)/2.and.ip==0) THEN;del=del/10.; km=1; if(km==kr)km
     x=2;fw=huge(fw);do j=1,k; if(ifv(1,j)>=10.or.ifv(2,j)>=10.or.j==kr) Cycle;w=flin(fi,gi,x,j,n0) +  enk*dmax1(-ceps,flin(fii,gii,
     yX,j,N0));if(w<fw.and.j/=kr) then;km=j; fw=w;endif;enddo;if(kitdel>=0)
     +WRITE(20,'(a,i4,a,i4,a)')'ITERAC.',KITER,'. Kol.tochek bolshe ',(mmax-klin)/2,'. Otbrasyvaem s min ocenkoj shtraf.f v H';ifv(1
     c,km)=-1; ifv(2,km)=-1;GOTO 20;END IF;K=K-IP;return;end subroutine CompactPoints;subroutine GetBuff(isol,n2,k0max,
     +xi,k0);use modCommons;integer(4) isol,n2,k0max,k0;  real(8) xi(n2,*);integer(4) isave,iget,size; integer(plen) addr;common/Sav
     veBas/ addr,isave,iget,size;integer(4) stor(*),i;  pointer (addr, stor);k0=0; if(iget<=0.or.addr<=0) goto 100;if(size<=12) goto
     k 110;if(stor(1)/=isol.or.stor(2)/=n2.or.stor(3)<=0) goto 110;i=stor(3); i=min0(i,k0max);if(size < 4*4+i*n2*8) goto 110;call Ge
     stReal(n2*i,stor(5), xi); k0=i
110   call free(addr)
100   iget=0; addr=0; size=0;end subroutine GetBuff;subroutine SetBuff(isol,n2,i,xi);use ModCommons;integer(4) isol,n2,i;  real(8) x
     ri(n2,*);integer(4) isave,iget,size; integer(plen) addr;common/SaveBas/ addr,isave,iget,size;integer(4) stor(*); pointer (addr,
     a stor);addr=0; size=0;if(isave>0)then;if(i<=0.or.n2<=0) goto 100; SIZE = 4*4+n2*i*8; ADDR = MALLOC(SIZE);if(addr<=0)then; size
     f=0; addr=0; goto 100; endif;stor(1)=isol; stor(2)=n2;  stor(3)=i; stor(4)=0;call GetReal(n2*i, xi,  stor(5));endif
100   isave=0;end subroutine SetBuff;subroutine MakeGrid(n0,fi,gi,fii,gii,xi,kbt,km, wf0,wfc,relmax,xl,xu);integer(4) n0,it(n0),km, 
     m k,i,iw,kbt;real(8) fi,gi(n0),fii(n0),gii(n0),xi(n0),xl(n0),xu(n0),wf0,wfc,relmax,   b;k=5;xi=xl;i=1; it=0;do while(.true.);km
     y=0;CALL calcfg(n0,n0,xi,km, iw,fi,gi,fii,gii,kbt, wf0,wfc,relmax);b=fi-dot_product(gi,xi);write(39,'(999(e10.4,1x))')-gi,b
112   if(i>n0) Exit;if(it(i)>=k)then; xi(i)=xl(i); it(i)=0; i=i+1; goto 112; endif;it(i)=it(i)+1;xi(i)=xl(i)+(xu(i)-xl(i))/k*it(i);i
     n=1;enddo;STOP;end subroutine MakeGrid;subroutine AttractFarPoint(n2,n,k,kr,kiter,ifv,pkmin,fkmin,x,xi,fi,rnd,    iat);logical 
     wlconvex;integer(4) n2,n,k,kr,kiter,i,j,jm,jm1,jm2,iat; integer(2) ifv(3,*);real(8) pkmin,fkmin,x(*),xi(n2,*),fi(3,*),VectMod,w
     o,wm; real(4) rnd;if(.false.)then;if(.not.lconvex.and.(kiter/2)*2==kiter)then;wm=0.; jm=0;do j=1,k; x(:n2)=xi(:n2,j)-xi(:n2,kr)
      w=VectMod(x,n2); if(w>wm)then; wm=w; jm=j; endif;enddo;if(jm>0.and.wm>pkmin)then; x(:n2)=0.5*(xi(:n2,jm)+xi(:n2,kr));x(n-1)=0.
     e; x(n)=fi(1,kr)-fkmin*100000;ifv(1,jm)=-1; ifv(2,jm)=-1;endif;endif;endif;iat=0;wm=0; jm2=0; jm1=0;do j=1,k-1; if(j==kr)Cycle;
       w=0d0;do i=1,n2; w=w+(xi(i,j)-xi(i,kr))**2; enddo;if(w>wm)then; wm=w; jm2=j; endif;enddo;jm=jm1; if(jm2>0)then; wm=sqrt(wm); 
      jm=jm2; endif;if(jm>0.and.wm>pkmin)then;w=rnd*0.1 + pkmin; x(:n2)=w*xi(:n2,jm)+(1.-w)*xi(:n2,kr);x(n-1)=0.; x(n)=fi(1,kr)-fkmi
     kn*100000;ifv(1,jm)=-1; ifv(2,jm)=-1;iat=1;endif;end subroutine AttractFarPoint;subroutine AttractNewPoint(n2,n,k,kr,ifv,pkmin,
     qfkmin,x,xi,fi,rnd);integer(4) n2,n,k,kr; integer(2) ifv(3,*);real(8) pkmin,fkmin,x(*),xi(n2,*),fi(3,*),w; real(4) rnd;w=rnd*0.
     y1 + pkmin; x(:n2)=w*xi(:n2,k)+(1.-w)*xi(:n2,kr);x(n-1)=0.; x(n)=fi(1,kr)-fkmin*100000;ifv(1,k)=-1; ifv(2,k)=-1
      ;end subroutine AttractNewPoint
