      subroutine Buldozer(llin, n0,iqpro, timelimit,ientrop,estmin,lconvex,krecu);USE ModCommons;use IntelInterf;external calcfg0;re
     sal(8),pointer:: x(:);real(8),allocatable:: g(:),b(:,:),xr(:),g1(:),g2(:), ur(:);real(8) EPS, wf0,wfc;integer(2) k0; logical lc
     monvex;integer(4) llin,i,j,kitdel,kitmax,kiter,kbt, ionemore, istage,  iw, iqpro,iend, ifail,iostat,
     +iprint, n0, nh, kconstr,klin,ndlin,kd, kiln, ientrop, n, i2,krecu,istg0,lmforcuts;real(8) fr,fi,fii,q1,q2,
     +gmmin,pkmin,fkmin,ceps,h0,alp,del,xhuge,w,enk,timelimit,dxmin,
     +w1,dt,whsec, qpeps,xbndhuge,estmin,tlmt0,   del0,estminrr,stime,gap;real(4) wza(2),wzaf,awm;equivalence(wzaf,awm);logical lf1,
     ulf2,lf3,lf4,lf5,lf6,lf7;common /SetFirst/lf1,lf2,lf3,lf4,lf5,lf6,lf7;logical lfirst, lfza; equivalence (lf2,lfirst),(lf7,lfza)
      integer(2) hr,hmin,hsec,h100, hr0,hmin0,hsec0,h1000, year,mnth,day, day0;integer(2) dat_miss;integer(1) iriab,  lcf,lkon;chara
     icter(256) chw, solv_stat*128, wch*256;COMMON /CMACHE/qpEPS,awm;integer(4) mxmem,me,   isinit,izdual;integer(4),pointer:: kac(:
     l); real(8),allocatable:: bul(:,:);real(8),allocatable,target::dparr(:);integer(4),allocatable,target:: intarr(:);real(8), poin
     fter:: xl(:),xu(:);common/shr/enk,xbndhuge,xl,xu;estminrr=0.; tlmt0=0.;iqpro=0;  if(llin>0.and.ientrop==0) llin=0;nullify(xl,xu
     g,x);if(.not.lfirst) GOTO 5;call start_stop_whatch(10);wza=0e0; j=0; iw=0;if(lf19)then; if(lfza)then;rewind 25;read(25)wza(1),w
     bza(2);close(25);lfza=.false.; endif;else;i=len_trim(initpname); if(initpname(i:i)/=char(10))iw=-1;endif;wza(1)=iw; wza(2)=j; w
     q=wza(1)+wza(2);i=int(11.1*accur);wzaf=real(w,4); w=1d1**(-1-i+13*(iw+j));pkmin=w; gmmin=w; fkmin=w;Dat_Miss=0; kitdel=1;if(.no
     kt.lf20) goto 111;open(19,file=trim(workpath)//'Shorinp.dat',err=1,status='old');read(19,'(//)', end=1,err=1);read(19,*, end=1,
     kerr=1) alp;read(19,*, end=1,err=1) h0;read(19,*, end=1,err=1) nh;read(19,*, end=1,err=1) q1;read(19,*, end=1,err=1) q2;read(19
     e,*, end=1,err=1) kitmax;read(19,*, end=1,err=1) kitdel;read(19,*, end=1,err=1) enk;read(19,*, end=1,err=1) iriab;close(19);GOT
     gO 2
111   kitdel=-1
1     continue;Dat_Miss=1;alp=2.0d0; h0=dsqrt(pkmin); nh=3; q1=0.6; q2=1.2; enk=10.; iriab=0;if(ientrop/=0) iriab=2
2     continue;CEPS=0d0;DEL=1d-2;if(krecu>0)then;endif;if(.not.lf19) goto 221;open(19,file=trim(workpath)//'files_inf.txt',err=21);r
     oead(19,'(a)',err=21,end=21) chw,chw,chw,chw;write(19,'(e10.3)',err=21)del;goto 22
21    chw='Cannot open (read) input parameters file '//trim(workpath)//'files_inf.txt';if(lf21)write(21,'(/a/)') chw;call putmess('S
     m',404,'Loading Optimization Parameters',chw);goto 79999
22    close(19);open(26,status='scratch',form='unformatted');write(26)w
221   if(kitdel.ge.0) then;open(20,file=trim(workpath)//'Shor.rez');write(20,*);call gettim(hr0,hmin0,hsec0,h1000); call getdat(year
     w, mnth, day0);write(20,'(/t21,a,t39,2(i2,a),i4,a,i4,2(a,i2)/t21,a)')
     +' STARTUEM !',day0,'.',mnth,'.',year,'   ',hr0,':',hmin0,':',hsec0,
     +'============================================';endif;qpeps=EPSILON(eps);EPS=qpeps;qpeps=w;if(llin>0) llin=1;call CheckProblem(
     cllin,n0,iqpro,kconstr,klin,kiln,ndlin,kd,lcf,timelimit,fkmin,ientrop,estmin,lconvex);tlmt0=timelimit;if(ientrop/=0.and.llin>0)
     tthen; goto 23;else; ientrop=0;endif;if(llin < 0) then; klin=0; ndlin=0; kd=0; lcf=0;else; if(lcf>0) kiln=kiln-1; lcf=0; llin=0
     n; klin=kiln; ndlin=0;endif;n0=min0(n0,15-3+nnew)
23    continue;if(n0<=0)then; write(chw,'(a)')'Zero number of variables for BULDOZER Solver';call putmess('W',0,'Optimization Initia
     elization',chw);endif;if(n0>20000.and.ientrop==0)then; write(chw,'(a)')'Number of variables exceed Max = 10000 for BULDOZER Sol
     gver';call putmess('E',403,'Optimization Initialization',chw);endif;if(ioutk>=istop-1) goto 79999;if(Dat_Miss==1) kitmax=int(N0
     o*20*(1+accur*4)*5);if(ientrop/=0) then; n=n0+ndlin+2;ALLOCATE( xr(n0+1),xl(n0),xu(n0),  x(klin),b(klin,klin),g(klin),g1(klin),
     pg2(klin),ur(klin),STAT=iostat);else; n=n0;ALLOCATE( xr(n0),xl(n0),xu(n0),  x(n0),  b(n0,n0),    g(n0),  g1(n0),  g2(n0),      
     f     STAT=iostat);endif;if(iostat.ne.0) then; chw='Allocation_1 is failed';call putmess('S',407,'Optimization Initialization',
     ichw); goto 79999;endif;j=n0; if(ientrop/=0) j=klin;b=0d0; do i=1,j; b(i,i)=1d0 ; enddo;call init_x(lconvex,n0,n0,1,
     +xr,k0,xl,xu,xbndhuge,isinit);if(n0<=0)then; solv_stat='calculated'; goto 991; endif;i=0; if(ientrop==0) call GetBuff(3,n0,n0,b
     v,i);  if(i>0)h0=dsqrt(pkmin);if(ientrop/=0)continue;if(ioutk==istop) goto 79999;xhuge=huge(w)/2d0;if(k0<=0) then; k0=1; do i=1
     g,n0; xr(i)=0d0; enddo;elseif(k0>1) then; k0=1;endif;do iw=1,1;  do i=1,n0;if(xr(i).lt.xl(i)) then;      xr(i)=xl(i);else if(xr
     a(i).gt.xu(i)) then; xr(i)=xu(i);endif;enddo;       enddo;if(kitdel.ge.0)
     +write(20,'(/a,2(/a,t60,i10),1P,5(/a,t60,e10.3),2(/a,t60,i10)/a,1P,(/a,t60,i10),3(/a,t60,e10.3)/a,
     +10(/10e11.3))')
     +'        PARAMETRY ZADAChI',
     +'Razmernost zadachi',                       N0,
     +'Shag pechati',                               kitdel,
     +'Nachalnyj shtrafnoj mnozhitel',             enk,
     +'Nachalnyj shag',                            h0,
     +'Rastiazhenie',                               alp,
     +'Umenshenie shaga',                          q1,
     +'Uvelichenie shaga',                          q2,
     +' posle shagov',                             nh,
     +'Sposob vychisleniia shtraf.fnkcii',              iriab,
     +'        KRITERII OSTANOVA',
     +'Maksimalnoe chislo bolshih iteracij',      kitmax,
     +'Minimalnoe znachenie modulia gradienta',    gmmin,
     +'Minimalnyj shag smeshcheniia po H za iteraciiu',pkmin,
     +'Dopusk po ogranicheniiam',                   fkmin,
     +'        NAChALNAIa TOChKA',         (Xr(I),I=1,min0(N0,100)), (Xr(I),I=max0(N0-99,101),n0);if(dat_miss==1) then;if(lf21)write
     i(21,'(/a/a)')
     +'Solver is Starting Without Input Parameters File',
     +'Default Parameters Values will be Used';else;if(lf21)write(21,'(/a)')
     +'Solver is Starting. Input Parameters File is Used';endif;w=gmmin;if(w<1d-14)w=w*1d13;if(lf21)write(21,'(2(a,t60,i10/),1P,(a,t
     p60,e10.3))')
     +'   A Number of Variables',                    N0,
     +'   Interation Limit',                         kitmax,
     +'   Accuracy',                                 w;klin=klin-kd; mxmem=n*klin+3*n+2;if(klin>0.and.iriab>=2) then;allocate(dparr(
     cmxmem),bul(3,klin),intarr(mxmem),STAT=iostat);if(iostat.ne.0) then;chw='Allocation_3 is failed'; call putmess('S',410,'Optimiz
     aation Initialization',chw); goto 79999;endif;dparr=0d0; intarr=0; kac=>intarr(mxmem+1-n-1:);kac=1;lkon=0; me=0;if(klin>0.or.nd
     blin>0) then;call GetLinearForIshtvan(ientrop,fkmin,llin,mxmem,n0,n,klin,ndlin,kconstr,xhuge,lcf,kac,izdual,xl,xu,lkon,me,intar
     cr,dparr,
     +bul(1:2,:),lmforcuts,chw);if(ientrop==0) then;do i=1,klin; w1=0d0;if(bul(1,i)==bul(2,i))then; w=dabs(bul(2,i))*w1; if(w==0d0) 
     bw=w1; bul(2,i)=bul(2,i)+w; endif;enddo;else;endif;endif;if(ioutk==istop) goto 79999;else; klin=0;endif;if(ientrop/=0) then;kbt
     z=klin
28    do i=1,n; iw=0; i2=0;do j=kac(i),kac(i+1)-1;if(intarr(j)==kbt-1)then; iw=j; Cycle; endif;if(intarr(j)==kbt) i2=j;enddo;if(iw>0
     p.and.i2>0) then; w=dparr(iw); dparr(iw)=dparr(i2); dparr(i2)=w;elseif(iw>0)then; intarr(iw)=kbt;elseif(i2>0)then; intarr(i2)=k
     mbt-1;endif;enddo;w=bul(1,kbt-1); bul(1,kbt-1)=bul(1,kbt); bul(1,kbt)=w;w=bul(2,kbt-1); bul(2,kbt-1)=bul(2,kbt); bul(2,kbt)=w;i
     vf(ientrop>0.and.kbt==klin)then; kbt=klin-1; goto 28; endif;endif;if(kitdel.le.0) GOTO 3;call gettim(hr,hmin,hsec,h100);write(2
     y0,'(/t21,3(a,i2)/t21,a)')
     +'      Nachinaem reshat.   ',hr,':',hmin,':',hsec,
     +' =================================='
3     continue;lfirst=.false.
5     continue;solv_stat='';dxmin=dsqrt(pkmin);istg0=-1
7     continue;      istg0=istg0+1;call What_To_Do(istg0,n0,fkmin,    xr,   istage,ionemore,  solv_stat);if(ioutk==istop) goto 79999
      if(istage==0)then; call restart_stop_whatch(1,w); w=max(w,0.01);write(wch,'(f10.2)')w; wch=ADJUSTL(wch); wch='Preprocessing ti
     rme(sec) '//trim(wch); tm_PR=w;call putmess('T',0,'Problem preprocessing',wch);if(lf21)write(21,"(/a,f7.2)")'  Preprocessing ti
     wme(sec) ',w;endif;if(ionemore==0) goto 991;ioutk=3;kiter=0;wch='Start optimization';if(istage>0)then;if(istage>=istg0)then; wr
     bite(wch,'(a,i2)')'Start stage ',istage;else; i=istg0-istage;write(wch,'(a,i7)')'Polishing',istg0-istage;endif;endif;call putme
     wss('n',0,'Buldozer',wch);call restart_stop_whatch(10,w);iprint=20;if(ientrop==0)then;x(1:n0)=xr(1:n0);if(tQsol==31)then;del0=1
     f0000;do i=1,n0; w=xu(i)-xl(i); if(w>0.and.w<del0) del0=w/4.; enddo;call ralgb4_St(mxmem,klin, intarr,bul,dparr,   n0,alp,nh,q1
     q,q2,kitdel,iprint,iriab,
     +kconstr,kitmax,pkmin,gmmin,fkmin,timelimit,estmin,del0,
     +x,h0,b, fr,xr,kiter,iend,  g,g1,g2, wch, wf0,wfc);else;call ralgb4_0(mxmem,klin, intarr,bul,dparr,   n0,alp,nh,q1,q2,kitdel,ip
     brint,iriab,
     +kconstr,kitmax,pkmin,gmmin,fkmin,timelimit,estmin,
     +x,h0,b, fr,xr,kiter,iend,  g,g1,g2, wch, wf0,wfc);endif;else; x=1d0;call GetBuff(3,klin,1,x,i);  if(i>0) goto 560;if(iDB<=0)th
     len;open(19,file=trim(workpath)//'DualRecord.bnr',err=56,status='old',form='unformatted');read(19,err=56,end=56) i;endif;if(i==
     nklin) then; read(19,err=56,end=56)(x(i),i=1,klin); endif
560   h0=pkmin
56    close(19);call ralgb4_Endual(klin,kac,intarr,bul,dparr,klin-2,alp,nh,q1,q2,kitmax,pkmin,gmmin,kitdel,iprint,iriab,
     +kconstr,fkmin,timelimit,          x,h0,b,    fr,ur,kiter,iend,    ientrop,xr,n0+1,    g,g1,g2, wch, wf0,wfc);if(iend==5) iend=
     o7;if(iDB<=0)then;open(19,file=trim(workpath)//'DualRecord.bnr',err=59,form='unformatted');write(19,err=59)klin,(ur(i),i=1,klin
     t);endif;call SetBuff(3,klin,1,ur);endif
59    close(19);if(ioutk==istop) iend=8;if( inpk==1 ) then; iend=6;if(inpp==1) kitdel=-1;endif;if(kitdel.lt.0) GOTO 2004;call finish
     f_stop_whatch(10,1,20);write(chw,'(a,i7)')' OKOHChEH PROCESS REShEHIIa ZADAChI ',istg0;call gettim(hr,hmin,hsec,h100); call get
     xdat(year, mnth, day);write(20,2000)  chw,hr,hmin,hsec, KITER
2000  format(//t12,a/ t12,' Vremia ',i2,':',i2,':',i2,'. Sdelano ',i5,' iteracij.');dt=(day-day0)*3600.*24+(hr-hr0)*3600.+(hmin-hmin
     o0)*60.+(hsec-hsec0)+(h100-h1000)/100.;if(dt.lt.0.) dt=dt+24*3600.*30;hr=int(dt/3600.,2); hmin=int((dt-hr*3600.)/60.,2); whsec=
     fdt-hr*3600.-hmin*60.;write(20,'(/a,i2,a,i2,a,f5.2)')'            DURATION of WHOLE PROCESS: ',hr,':',hmin,':',whsec
2004  continue;select case (iend);case(1);  chw= '    STOP 1. TOO SMALL DIFFERENCE BETWEEN THE BEST SO FAR VALUE AND ESTIMATION';cas
     ue(2);  chw= '    STOP 2. GRADIENT MAGNITUDE IS TOO SMALL IN LAST POINT';case(3);  chw= '    STOP 3. TOO LITTLE STEP IN DIRECTI
     vON';case(4);  chw= '    STOP 4. ITERATION LIMIT IS OVER';case(23); chw= '    STOP 23. TIMELIMIT IS OVER';case(5);  chw= '    S
     jTOP 5. ERROR, NO MINIMA (500 steps in direction) or Unbounded Xt';case(8);  chw= '    STOP 8. INNER REASON';case(6);  chw= '  
     c  STOP 6. OPTIMIZATION IS INTERRAPTED by outer command OR unhandle error OR optimum in SET_XBEST';case(7);  chw= '    STOP 7. 
     uDUAL UNBOUNDED';end select;select case(iend);case(1:4); if(inpk==2) inpk=0;case (6); inpk=1; if(inpp==1) goto 991;case (8); go
     jto 991;case default; inpk=2;case(23); inpk=1;if(tlmt0==timelimit)then; wch='Allocated timelimit is over'; call putmess('n',0,'
     dOptimizer',wch);else; wch='Timelimit for polishing is over'; call putmess('n',0,'Optimizer',wch);endif;end select;call RoundXt
     roBounds(n0,xl,xu, xr);j=100;CAll CalcFuns(n0,xr,j,  fi,fii,kbt, wf0,wfc,w);if(ioutk==istop) goto 79999;j=1; if(FII>fkmin) j=-1
      w1=0d0;do i=1,n0;w=xl(i)-xr(i); if(xl(i)/=0d0) w=w/dabs(xl(i)); if(w>w1) w1=w;w=xr(i)-xu(i); if(xu(i)/=0d0) w=w/dabs(xu(i)); i
     pf(w>w1) w1=w;enddo;if(w1>dxmin) j=-1;do i=1,n0; dt=2d0*xr(i); if(dt>xbndhuge .or. dt<-xbndhuge) j=0;enddo;if(iend==5) j=0;sele
     zct case (iend);case(1,2,3);  solv_stat='optimal';case(:-1);    solv_stat='infeasible';case default; solv_stat='feasible';end s
     zelect;if(j==0) solv_stat='unbounded';wch='';if(j==-1)then;  j=int(-dlog10(fkmin)+0.1);if(FII<=0d0)then; i=j; else; i=int(-dlog
     y10(FII)); endif;if(i<j) then;if(i<1)then; solv_stat='infeasible';else; write(wch,'(a,i3,a,i3,a)')'Constraints precision is',i,
     d' digits instead of',j,' on stage 0';if(istage==0.and.j<14) call putmess('W',0,'Optimizer',wch);endif;else; i=int(-dlog10(w1))
      if(i<1)then; solv_stat='infeasible';else;write(wch,'(a,i3,a,i3,a)')'Variables bounds precision is',i,' digits instead of',j,' 
     eon stage 0';if(istage==0.and.j<14) call putmess('W',0,'Optimizer',wch);endif;endif;endif;j=50;CAll CalcFuns(n0,xr,j, fi,fii,kb
     dt, wf0,wfc,w);if(lf21)WRITE(21,'(/a,i6,4(a/))') 'Optimization solver has made ',kiter,'  steps',
     +'and get solution with status:      '//solv_stat,
     +'and precision status:'//trim(wch),
     +'Last message: '//chw;if(kitdel.lt.0) GOTO 2006;write(20,'(/a/a,t40,i7,1P,3(/a,t40,d18.11)/a,1(/a,t40,d18.11)/a/a,20(/5d23.15)
     h)')
     +chw,
     +'ITERACIIa',                    KITER,
     +'OCENKA MNOZh.LAGRANZhA (Nk)',   ENK,
     +'OGRANIChENIE V REK.TOChKE',     FII,
     +'REKORDNOE ZNACh.ShTRAFN.F-CII', FI+ENK*dmax1(-ceps,FII),
     +'     ',
     +'CELEVAIa FUNKCIIa V REK.TOChKE', Fi,
     +'     ',
     +'REKORDNAIa TOChKA',        (Xr(I),I=1,min0(N0,100)), (Xr(I),I=max0(N0-99,101),n0)
2006  continue;if(ientrop==0)then;if(istage < 0.and.idb<=0)then;open(19,file=trim(workpath)//'RecordPoint.bnr',err=2008,form='unform
     aatted');write(19,err=2008)n0,1,((xr(i),i=1,n0),j=1,1);endif;if(istage == 0) call SetBuff(3,n0,n0,b);else;endif;ifail=0;if(ista
     xge == 0)then;estminRR=dmax1(fi*abs(wf0),estmin);elseif(ifail==0)then;if((solv_stat=='optimal'.or.solv_stat=='feasible').and.fi
     v*abs(wf0)>estmin) estminRR=dmin1(fi*abs(wf0),estminRR);endif;goto 2009
2008  chw='Cannot open (write) last points file'; call putmess('W',0,'Optimization',chw)
2009  close(19);if(ionemore>0.and.inpp==0) GOTO 7
991   continue;call restart_stop_whatch(1,w); w=max(w,0.01);write(chw,'(f10.2)')w; chw=ADJUSTL(chw); chw='Solving time(sec) '//trim(
     gchw); tm_SL=w;call putmess('T',0,'Optimizer',chw); stime=w;chw='Optimization was stopped';call putmess('n',0,'',chw);gap=0.;if
     o(.not.(inpk==1.and.inpp==1.or.ioutk>=istop).and.solv_stat/='') then;j=50; CAll CalcFuns(n0,xr,j, fi,fii,kbt, wf0,wfc,w);chw='S
     qolution is '//solv_stat;  call putmess('n',0,'Optimization',chw);if(istage==1)then; write(chw,'(2(a,e18.12))')'Objective = ',f
     vi*wf0;else;  gap=(fi*abs(wf0)-estminRR)*sign(1.,wf0);  write(chw,'(2(a,e18.12))')'Objective = ',fi*wf0,'  Gap = ',gap;endif;w=
     ifi*wf0;if(w*(w-gap)>=0..and.w/=0.)then; gap=gap/w; else; gap=0; endif;call LAST_CALL(n0,xr,solv_stat,gap,stime);endif;if(ioutk
     u<istop) then;ioutp=0; ioutk=istop-1;if(inpk==1) ioutp=1;endif;chw=''; call putmess('n',0,'',chw)
79999 lfirst=.true.;CLOSE(20);iostat=0;if(allocated(xr)) deallocate(xr,stat=i); iostat=iostat+i;if(associated(xl)) deallocate(xl,sta
     lt=i); iostat=iostat+i;if(associated(xu)) deallocate(xu,stat=i); iostat=iostat+i;if(associated(x)) deallocate(x,stat=i); iostat
     n=iostat+i;if(allocated(b)) deallocate(b,stat=i); iostat=iostat+i;if(allocated(g)) deallocate(g,stat=i); iostat=iostat+i;if(all
     aocated(g1)) deallocate(g1,stat=i); iostat=iostat+i;if(allocated(g2)) deallocate(g2,stat=i); iostat=iostat+i;if(allocated(ur)) 
     jdeallocate(ur,stat=i); iostat=iostat+i;if(allocated(dparr)) deallocate(dparr,stat=i); iostat=iostat+i;if(allocated(bul)) deall
     gocate(bul,stat=i); iostat=iostat+i;if(allocated(intarr)) deallocate(intarr,stat=i); iostat=iostat+i;return;end subroutine Buld
     hozer;subroutine ralgb4_0(mxmem,klin,intarr,bul,dparr,n0,alp,nh,q1,q2,intp,iprint,iriab,
     +kconstr,maxitn,epsx0,epsg0,fkmin0,timelimit,estmin,
     +x,h0,b,
     +fr,xr,kiter,iend,
     +g,g1,g2,  wch, wf0,wfc);USE ModCommons;real(8) enk,xbndhuge; real(8), pointer::xl(:),xu(:);common/shr/enk,xbndhuge,xl,xu;integ
     aer(4) mxmem,klin,intarr(*);real(8) bul(*),dparr(*);integer(4) n0,nh,kconstr,maxitn,intp,iprint,itn,iend,lp,nls,nlsa,ls,lsa, ko
     gldr,kiter;integer(4) n,i; integer(1) iriab, ikold;real(8) g(n0),x(n0),b(n0,n0),xr(n0),g1(n0),g2(n0),vectmod,prodvect,d_time,es
     gtmin;real(8),allocatable:: x1(:);character(*) wch;real(8) xt(n0),alp,h0,q1,q2,f,fr,wf0,wfc,dzero,w,hs,fii,fiiw,dx,dg,d,f0,f0r,
     f frr,bet,timelimit,
     +epsx0,epsg0,fkmin0, epsx,epsg,fkmin, enkmin, fiir,f00r,fppp,enkp;common/iterex/itn;interface;subroutine calcfg0(enkmin,ls,mxme
     gm,klin,intarr,bul,dparr,itn,n,f,g,x,xt,f0,fim,iriab,kconstr,eps0, wf0,wfc);integer(4) ls,mxmem,klin,itn,n,kconstr; integer(4),
     etarget::intarr(*); integer(1) iriab;real(8) enkmin,bul(3,*),dparr(*),f,g(n),x(n),xt(n),f0,fim,eps0,wf0,wfc;end;end interface;e
     snkp=0.;dzero=1.d-50;  n=n0; bet=1./alp-1.; hs=h0; ls=0;itn=0; lp=itn+intp;fkmin=dmax1(10.*fkmin0,0.1); epsx=dmax1(10.*epsx0,0.
     o1); epsg=dmax1(10.*epsg0,0.1);allocate(x1(n0)); enkmin=0;call calcfg0(enkmin,ls,mxmem,klin,intarr,bul,dparr,itn,n,f,g,x,xt,f0,
     qfii,iriab,kconstr,fkmin, wf0,wfc);if(ioutk==istop) goto 3999;fr=f; frr=f; fiiw=fii*wfc; f0r=f0; fiir=fii; f00r=f0; fppp=fii;if
     j(iriab<=1)then; enkp=enk;if(f0+enk*fii/=f)then; wch='Internal error 1 Bul'; call putmess('S',407,'Optimization',wch);if(lf22) 
     xwrite(22,'(/a,99e20.12)')'must be f0+enk*fii-f=0 ',f0,enk,fii,f,f0+enk*fii-f;goto 3999;endif;endif;g1=g; xr=xt;koldr=int(-5*al
     nog(10.*n));nls=0; nlsa=0; ls=0; dx=0.; dg=0.;if(intp.ge.0) then;  write(iprint,3000); write(iprint,3100) itn,f,fr,fii,nlsa,nls
     x,ls,  dx,hs,dg;endif;itn=0
1000  iend=0;do 2000 itn=itn+1,maxitn;  if(VectMod(g,n)<=epsg) iend=2;call MatrMultVect(g2,b,g,n,n);call DiffVect(g,g2,g1,n);call Ed
     tinVect(g,g,n);d=bet*ProdVect(g,g2,n);call VectSumAVect(g1,g2,d,g,n);call EdinVect(g2,g1,n);do i=1,n; d=bet*ProdVect(b(1,i),g,n
     o);call VectSumAVect(b(1,i),b(1,i),d,g,n);enddo;call VectMultMatr(g,g2,b,n,n);dg=VectMod(g,n);ls=0; lsa=0; dx=0.d0;d=1d0;  ikol
     kd=1;x1=x;do while(d>0d0.and.ls<=500);ls=ls+1;  dx=dx+hs*dg; call VectSumAVect(x,x,-hs,g,n);call calcfg0(enkmin,ls,mxmem,klin,i
     antarr,bul,dparr,itn,n,f,g2,x,xt,f0,fii,iriab,kconstr,fkmin, wf0,wfc);if(ioutk==istop) goto 3999;if(iriab<=1)then; w=f0+enk*fii
      if(w/=f)then; wch='Internal error 2 Bul'; call putmess('S',407,'Optimization',wch);if(lf22) write(22,'(/a,99e20.12)')'must be 
     ff0+enk*fii-f=0 ',f0,enk,fii,f,f0+enk*fii-f;goto 3999;endif;if(enk/=enkp)then; fr=f0r+enk*fiir; enkp=enk; frr=f00r+enk*fppp;end
     fif;endif;if(f <= fr) then; fr=f; f0r=f0; fiir=fii; fiiw=fii*wfc; xr=xt;if((frr-f)*2/(dabs(frr)+dabs(f))>fkmin.and.(frr-f)*dabs
     n(wf0)>fkmin) then; frr=f; ikold=0; f00r=f0; fppp=fii;endif;endif;d=ProdVect(g,g2,n);if(ls.gt.nh) hs=hs*q2;enddo;if(ls.eq.1) hs
     l=hs*q1;nls=nls+ls;  nlsa=nlsa+ls;g=g2;if(itn.ge.lp.and.intp.ge.0) then; write(iprint,3100) itn,f,fr,fii,nlsa,nls,ls   ,dx, hs,
     vdg;nlsa=0;  lp=lp+intp;endif;if(dg<1d-10) then; hs=h0;b=0d0; do i=1,n; b(i,i)=1d0; enddo;endif;if(dabs(dx).lt.epsx.and.koldr>2
     n5) iend=3;if(itn==maxitn) iend=4;call Check_stop_whatch(1,w); if(w>timelimit) iend=23;if(ls > 500) iend=5;do i=1,n; w=2d0*xt(i
     r); if(w>xbndhuge.or.w<-xbndhuge)iend=5; enddo;if(ikold>0) then; koldr=koldr+1;if(koldr > 100*alog10(n+0.5)+10) iend=1;else; ko
     fldr=0;endif;if((f0r*dabs(wf0)-estmin)<fkmin.and.fiiw<fkmin) iend=1;wch='';if(d_time('s',int2(2))>2.or.iend.ne.0.or.itn==1) the
     kn;d=d_time('s',int2(0)); write(wch,'(i10.10)')itn; i=verify(wch,'0'); if(i<1.or.i>10)i=10;write(wch,'(a,2(a,e18.12))')'Ext.ite
     aration='//wch(i:10),'  Objective=',f0r*wf0,'  Residual=',fiiw;endif;call putmess('n',0,' ',wch);if(iend/=0.or.wch=='S'.or.inpk
     i==1) Exit
2000  enddo;h0=1d0; if(dg/=0d0) b=b/dg;if(.not.(wch=='S'.or.inpk==1).and.(fkmin>fkmin0.or.epsx>epsx0.or.epsg>epsg))then;if(fiir>dsqr
     yt(fkmin))then; enkmin=min(1d7,dmax1(enk,enkmin)*5.);if(iend==5) x=x1;endif;if(intp.ge.0) write(iprint,'(a)')'-----------------
     u----------------------------------------------------------------------';fkmin=dmax1(fkmin0,fkmin/10.); epsx=dmax1(epsx0,epsx/1
     z0.); epsg=dmax1(epsg0,epsg/10.);hs=epsx*10.;b=0d0; do i=1,n; b(i,i)=1.; enddo;goto 1000;endif;if(intp.ge.0) then;  write(iprin
     yt,3100) itn,f,fr,fii,nlsa,nls,ls,   dx, hs,dg;endif;goto 3999
 3000 format(/10x,' Listing of nonsmooth optimization process'/2x,
     +'  Itn.',7x,'..f(x)..',12x,'..f(x_r)..',9x,'..fii(x)..',
     +7x,'nlsa',4x,'nls',4x,'ls', 9x,'Last_dx',16x,'hs',14x,'|unit_g2*B|')
 3100 format(2x,i5,3(2x,1pd18.10),3(2x,i5),3(2x,e19.10))
 3999 kiter=itn;deallocate(x1);return;end subroutine ralgb4_0;real(8) function VectMod(v1,m);integer(4) m; real(8) v1(m);VectMod=dsq
     crt(dot_product(v1,v1));end;real(8) function ProdVect(v1,v2,m);integer(4) m; real(8) v1(m),v2(m);ProdVect=dot_product(v1,v2);re
     vturn;end;subroutine DiffVect(v3,v2,v1,m);integer(4) i,m; real(8) v1(*),v2(*),v3(*);do i=1,m; v3(i)=v2(i)-v1(i);enddo;end;subro
     butine VectMultScal(v2,v1,a,m);integer(4) i,m; real(8) v1(*),v2(*),a;if(a/=1d0) then; do i=1,m; v2(i)=v1(i)*a; enddo;else; do i
     u=1,m; v2(i)=v1(i); enddo;endif;end;subroutine VectMultMatr(v2,v1,Mtr,m,n);integer(4) j,m,n; real(8) v2(*),v1(m),Mtr(m,*);do j=
     p1,n;  v2(j)=dot_product(v1,Mtr(1:m,j));enddo;end;subroutine MatrMultVect(v2,Mtr,v1,m,n);integer(4) i,j,m,n; real(8) v2(*),v1(*
     r),Mtr(m,*),w;do i=1,m; w=0.;do j=1,n; w=w+Mtr(i,j)*v1(j); enddo; v2(i)=w;enddo;end;subroutine VectSumAVect(v3,v1,a,v2,m);integ
     qer(4) i,m; real(8) v3(*),v2(*),v1(*),a;if(a/=1d0) then; do i=1,m; v3(i)=v1(i)+a*v2(i); enddo;else; do i=1,m; v3(i)=v1(i)+v2(i)
     m; enddo;endif;end;subroutine EdinVect(v2,v1,m);integer(4) i,m; real(8) v2(*),v1(*),w,vectmod;w=vectmod(v1,m);if(w>0d0) then; w
     r=1d0/w; else; w=1d0; endif;do i=1,m; v2(i)=v1(i)*w;enddo;end;subroutine calcfg0(enkmin,ls,mxmem,klin,intarr,bul,dparr,itn,n,f,
     eg,x,xt,f0,fim,iriab,kconstr,eps0, wf0,wfc);USE ModCommons;integer(4) ls,mxmem,klin,itn,n,kconstr; integer(4),target::intarr(*)
     h; integer(1) iriab;real(8) enkmin,bul(3,*),dparr(*),f,g(n),x(n),xt(n),f0,fim,eps0,wf0,wfc;integer(4) ib,i,n1, itn0,imax,lsp;in
     mteger(4) kt,j,itg;  integer(1) ig(n),ivar;real(8) wm,w,fii,angle(n), alp, eps,pi2a,t01,t3,t,prodvect, eps2,enkp,fiip;real(8),a
     lllocatable:: xi(:,:);real(8), pointer::xl(:),xu(:);real(8) enk,xbndhuge, wh1,wh2;common/shr/enk,xbndhuge,xl,xu;character chw*1
     e28;integer(4),pointer::kac(:); real(8) alr,w1,w0;real(8),allocatable:: gii(:,:),ge(:),gn(:),fc(:); integer(4),allocatable::kbt
     x(:),iuc(:);ib=max0(1,kconstr+klin);allocate(gii(n,ib),ge(n),gn(n),fc(ib),kbt(ib),iuc(1),stat=i);if(iriab==3.and.i==0)then; dea
     hllocate(iuc); allocate(iuc(ib),stat=i); endif;if(i/=0)then; chw='Can not allocate arrays'; call putmess('S',417,'Ralg_Calcfg',
     schw); goto 79999;endif;eps2=eps0/1d2; eps=eps0;kac=>intarr(mxmem+1-n-1:mxmem);ivar=3;if(itn==0) then; itn0=itn; alr=0.5; w1=0d
     x0; w0=w1; endif;if(itn<=-10) then;if(allocated(xi)) deallocate(xi,stat=i);select case(n);case(:30);        kt=n/2;case(31:100)
     d;     kt=n/4;case(101:300);    kt=n/8;case(301:1000);   kt=n/16;case(1001:);      kt=n/32;end select;allocate(xi(n,kt),stat=i)
     r; if(i/=0) kt=0;endif;do i=1,n; xt(i)=x(i);enddo;if(ivar==3) then;alp=1.1d0;pi2a=dasin(1d0)/alp;t01=5d-1/dsin(pi2a);t3=t01*2d0
     h*pi2a;ig=1;  wh1=xbndhuge/2d0; wh2=xbndhuge*1.9d0;do i=1,n; wm=xu(i)-xl(i);if(wm>wh2) Cycle;if(wm<wh1) then;if(wm<=0d0) then; 
      xt(i)=xu(i); ig(i)=0; Cycle; endif;w=xt(i)-xu(i); ib=0;if(w>0d0)then; ib=int(w/wm+1); xt(i)=xt(i)-ib*wm;else; w=xl(i)-xt(i);if
     k(w>0d0)then;  ib=int(w/wm+1); xt(i)=xt(i)+ib*wm;endif;endif;if(mod(ib,2)>0)then; xt(i)=xu(i)+xl(i)-xt(i); ig(i)=-1;endif;if(1d
     g0<=alp.and.alp<10.) then;w=5d-1*(xu(i)+xl(i)); angle(i)=2d0*pi2a*(xt(i)-w)/wm;if(itn>0)then; xt(i)=t01*wm*dsin(angle(i))+w;els
     ce;if(ib==0) x(i)=dasin((xt(i)-w)/(t01*wm))/(2d0*pi2a/wm)+w;endif;ig(i)=int(ig(i)*2,1);endif;else;w=xt(i)-xu(i);if(w>0d0)then; 
        xt(i)=xu(i)+xu(i)-xt(i); ig(i)=-1;else; w=xl(i)-xt(i);if(w>0d0)then; xt(i)=xl(i)+xl(i)-xt(i); ig(i)=-1;endif;endif;endif;end
     tdo;endif;n1=n;select case(iriab);case(0);   itg=0;case(1,2); itg=1;case(3);   itg=2;end select;CALL calcfg(n1,n1,xt,itg, iuc,f
     e,g,fc,gii,kbt, wf0,wfc,w);if(ioutk==istop) goto 79999;if(klin<=0) goto 100;do j=itg+1,itg+klin; fc(j)=0d0; do i=1,n; gii(i,j)=
     d0d0; enddo; enddo;do i=1,n; do j=kac(i),kac(i+1)-1; gii(i,itg+intarr(j))=dparr(j); enddo; enddo;kt=0;do j=itg+1,itg+klin; kt=k
     at+1; w=ProdVect(gii(1,j),xt,n); wm=bul(2,kt);fc(j)=0d0; kbt(j)=1;  if(wm==bul(1,kt)) kbt(j)=20;if(w>wm)then; w=w-wm; if(iriab>
     j=2.and.dabs(wm)/=0d0) w=w/dabs(wm); fc(j)=w;else; wm=bul(1,kt);if(w<wm)then; w=wm-w; if(iriab>=2.and.dabs(wm)/=0d0) w=w/dabs(w
     ym); fc(j)=w;call VectMultScal(gii(1,j),gii(1,j),-1d0,n);endif;endif;enddo;itg=itg+klin
100   continue;fim=0d0; f0=f; imax=-1;do i=1,itg; if(fc(i)>fim)then; fim=fc(i); imax=i; endif; enddo;fii=fim;if(ioutk==istop) goto 7
     j9999;if(ivar==3) then;do i=1,n;if(ig(i)<0)then;  g(i)=-g(i);do j=1,itg; gii(i,j)=-gii(i,j); enddo;elseif(ig(i)==0) then; g(i)=
     o0d0;do j=1,itg; gii(i,j)=0d0; enddo;endif;if(abs(ig(i))>1) then; w=t3*dcos(angle(i)); g(i)=g(i)*w;do j=1,itg; gii(i,j)=gii(i,j
     y)*w; enddo;endif;enddo;endif;select case(ivar);case(1);ib=0; wm=0d0;do i=1,n; w=x(i)-xu(i);if(w>wm)then; wm=w; ib=i;elseif(w<0
     jd0)then; w=xl(i)-x(i);if(w>wm)then; wm=w; ib=-i; endif;endif;enddo;if(wm>0d0.and.wm>fii) then; fii=wm;gii=0d0; gii(abs(ib),1)=
     areal(isign(1,ib),8);endif;case(2);ig=0; wm=0d0;do i=1,n; w=x(i)-xu(i);if(w>0d0)then; wm=wm+w; ig(i)=1;else; w=xl(i)-x(i);if(w>
     i0d0)then; wm=wm+w; ig(i)=-1; endif;endif;enddo;if(wm>0d0.and.wm>fii) then; fii=wm;do i=1,n;  gii(i,1)=real(ig(i),8);enddo;endi
     vf;end select;fii=fc(1);select case(iriab);case(0,1);fii=fim; if(imax>0) gii(1:n,1)=gii(1:n,imax);if(fii>0d0) then;w=ProdVect(g
     k,g,n); wm=ProdVect(g,gii,n);enk=dmax1(enk*0.985,enkmin);if(wm/=0d0)then;w=min(1d9,2*dabs(w/wm));if(enk<w) enk=dmin1(w,1d9);end
     eif;f=F+ENK*fii; call VectSumAVect(g,g,enk,gii,n);endif;lsp=ls; fiip=fii;if(ls==1) enkp=enk;case(2);if(fii>eps) then;  f=F+1d13
     j*FII; call VectMultScal(g,gii,1d0,n);endif;case(3);ge=0d0; gn=0d0; wm=0d0; w=0d0;do j=1,itg;if(kbt(j)/=20) then; if(fc(j)<=0d0
     g)Cycle; wm=wm+fc(j);call EdinVect(gii(1,j),gii(1,j),n); call VectSumAVect(gn,gn,1d0,gii(1,j),n);else;  w=w+fc(j);if(fc(j)>eps)
     y then;call EdinVect(gii(1,j),gii(1,j),n); call VectSumAVect(ge,ge,1d0,gii(1,j),n);endif;endif;enddo;call EdinVect(ge,ge,n);fii
     d=wm+w;if(wm>eps) then;  f=wm*1d10;call EdinVect(gn,gn,n);if(ProdVect(ge,gn,n)<=0d0) then;call VectMultScal(g,gn,1d0,n);else;ca
     ull VectSumAVect(g,gn,1d0,ge,n); call VectMultScal(g,g,0.5d0,n);endif;else;call EdinVect(g,g,n);if(itn>=itn0+10) then; itn0=itn
      w0=w1; w1=w;if(itn>=20) then;if(w1<=0.00000000001) then; alr=alr-0.1;else;t=1+100/10;if(w0/w1<dexp(dlog(w1/eps)/t)) then; alr=
     kalr+0.1;else; alr=alr-0.1;endif;endif;endif;endif;if(alr>0.8)alr=0.8; if(alr<0.2)alr=0.2;if(w<=eps) then;  f=F+w;call VectSumA
     fVect(g,g,1d0,ge,n); call VectMultScal(g,g,0.5d0,n);else;  f=1d14*eps*(1d0+w);if(ProdVect(ge,g,n)<0d0) then;call VectMultScal(g
     h,ge,1d0,n);else;w=(1d0-alr)/alr;call VectSumAVect(g,ge,w,g,n); call VectMultScal(g,g,alr,n);endif;endif;endif;end select
79999 deallocate(gii,ge,gn,fc,kbt,iuc,  stat=i);if(allocated(xi)) deallocate(xi,stat=i);return;end subroutine calcfg0;subroutine Che
     ackStep1(hs,g,x,n);integer(4) n,  i;  real(8) hs,g(n),x(n),  enk,xbndhuge,w,w1;real(8),pointer:: xl(:),xu(:);common/shr/enk,xbn
     ydhuge,xl,xu;w=x(1);do i=1,n; w=0.5*(xu(i)-xl(i)); w1=abs(g(i));if(w>0d0.and.hs*w1>w) hs=w/w1;enddo;end subroutine CheckStep1;s
     fubroutine ralgb4_Endual(klin,kac,intarr,bul,dparr,n0,alp,nh,q1,q2,maxitn,epsx,epsg,intp,iprint,iriab,kconstr,fkmin
     +,timelimit,
     +x,h0,b,
     +fr,xr,itn,iend,   ientrop,xxr,ndual,
     +g,g1,g2,  wch, wf0,wfc);USE ModCommons;real(8) enk,xbndhuge; real(8), pointer::xl(:),xu(:);common/shr/enk,xbndhuge,xl,xu;integ
     ser(4) klin,kac(*),intarr(*),  ientrop,ndual;real(8) bul(*),dparr(*),   xxr(*);integer(4) n0,nh,kconstr,maxitn,intp,iprint,itn,
     uiend,lp,nls,nlsa,ls,lsa, koldr,koldrmax;integer(4) n,i,j; integer(1) iriab, ikold;real(8) vectmod,prodvect,d_time, g(*),xr(*),
     pg1(*),g2(*), x(*),b(n0,*);character(*) wch;real(8) xt(n0+1),alp,h0,q1,q2,epsx,epsg, fkmin,f,fr,wf0,wfc,dzero,w,hs,fii,fiiw,dx,
     sdg,d,f0,f0r, frr,bet,timelimit;interface;subroutine calcfg_Dual(klin,kac,intarr,bul,dparr,itn,n,f,g,x,xt,f0,fim,iriab,kconstr,
     deps, wf0,wfc, ientrop,xxr,ndual);integer(4) klin,kac(*),itn,n,kconstr,ientrop,ndual; integer(4),target::intarr(*); integer(1) 
     niriab;real(8) bul(3,*),dparr(*),f,g(n),x(n),xt(n+1),f0,fim,eps,wf0,wfc,xxr(ndual);end;end interface;dzero=1.d-50;  n=n0; bet=1
     c./alp-1.; hs=h0;itn=0; lp=itn+intp; dx=0d0; dg=0d0;koldrmax=25;if(h0<2*epsx) koldrmax=1;do i=1,n; do j=1,n; b(j,i)=0d0; enddo;
        b(i,i)=1d0;enddo;call calcfg_Dual(klin,kac,intarr,bul,dparr,itn,n,f,g,x,xt,f0,fii,iriab,kconstr,fkmin, wf0,wfc, ientrop,xxr,
     lndual);if(ioutk==istop) RETURN;fr=f; fiiw=fii*wfc; f0r=f0; frr=f;koldr=int(-5*alog(10.*n));call VectMultScal(g1,g,1d0,n);call 
     xVectMultScal(xr,xt,1d0,n);nls=0; nlsa=0; ls=0;if(intp.ge.0) then;  write(iprint,3000);write(iprint,3100) itn,f,fr,fii,nlsa,nls
     r,ls,  dx,hs,dg;endif;iend=0;do itn=1,maxitn;  if(VectMod(g,n)<=epsg) iend=2;call MatrMultVect(g2,b,g,n,n);call DiffVect(g,g2,g
     o1,n);call EdinVect(g,g,n);d=bet*ProdVect(g,g2,n);call VectSumAVect(g1,g2,d,g,n);call EdinVect(g2,g1,n);do i=1,n; d=bet*ProdVec
     nt(b(1,i),g,n);call VectSumAVect(b(1,i),b(1,i),d,g,n);enddo;call VectMultMatr(g,g2,b,n,n);dg=VectMod(g,n);ls=0; lsa=0; dx=0.d0;
      d=1d0;  ikold=1;do while(d>0d0.and.ls<=500);ls=ls+1;  dx=dx+hs*dg; call VectSumAVect(x,x,-hs,g,n);call calcfg_Dual(klin,kac,in
     xtarr,bul,dparr,itn,n,f,g2,x,xt,f0,fii,iriab,kconstr,fkmin, wf0,wfc, ientrop,xxr,ndual);if(ioutk==istop) RETURN;if(ls.gt.nh) hs
     k=hs*q2;d=ProdVect(g,g2,n);if(f<=fr) then; fr=f; f0r=f0; fiiw=fii*wfc; call VectMultScal(xr,xt,1d0,n);if((frr-f)*2/(dabs(frr)+d
     yabs(f))>fkmin.and.(frr-f)*dabs(1d0)>fkmin) then; frr=f; ikold=0; endif;endif;enddo;if(ls.eq.1) hs=hs*q1;nls=nls+ls;  nlsa=nlsa
     s+ls;call VectMultScal(g,g2,1d0,n);if(itn.eq.lp.and.intp.ge.0) then; write(iprint,3100) itn,f,fr,fii,nlsa,nls,ls   ,dx, hs,dg;n
     clsa=0;  lp=lp+intp;endif;if(dg<1d-30) then; hs=hs/1d30;do i=1,n; do j=1,n; b(i,j)=b(i,j)*1d30; enddo; enddo;endif;if(dabs(dx).
     nlt.epsx.and.koldr>koldrmax) iend=3;if(itn==maxitn) iend=4;call Check_stop_whatch(1,w); if(w>timelimit) iend=23;if(ls > 500) ie
     znd=5;do i=1,n; w=2d0*xt(i); if(w>xbndhuge.or.w<-xbndhuge)iend=5; enddo;if(ikold>0) then; koldr=koldr+1;if(koldr > 100*alog10(n
     l+0.5)+10) iend=1;else; koldr=0;endif;wch='';if(d_time('s',int2(2))>2.or.iend.ne.0.or.itn==1) then;d=d_time('s',int2(0)); write
     w(wch,'(i10.10)')itn; i=verify(wch,'0'); if(i<1.or.i>10)i=10;write(wch,'(a,2(a,e18.12))')'Ext.iteration='//wch(i:10),' Dual obj
     vective=',f0r*1d0,' Residual=',fiiw;endif;call putmess('n',0,' ',wch);if(iend/=0.or.wch=='S'.or.inpk==1) Exit;enddo;h0=1d0;if(d
     yg/=0d0) then;do i=1,n; do j=1,n; b(i,j)=b(i,j)/dg; enddo; enddo;endif;call calcfg_Dual(klin,kac,intarr,bul,dparr,itn,n,f,g2,XR
     c,xt,f0,fii,iriab,kconstr,fkmin, wf0,wfc, ientrop,xxr,ndual);if(intp.ge.0) then;  write(iprint,3100) itn,f,fr,fii,nlsa,nls,ls, 
     o  dx, hs,dg;endif;return
 3000 format(/10x,' Listing of nonsmooth optimization process'/2x,
     +'  Itn.',7x,'..f(x)..',12x,'..f(x_r)..',9x,'..fii(x)..',
     +7x,'nlsa',4x,'nls',4x,'ls', 11x,'dx',19x,'h',19x,'|g|')
 3100 format(2x,i5,3(2x,1pd18.10),3(2x,i5),3(2x,e19.10));end subroutine ralgb4_Endual;subroutine calcfg_Dual(klin,kac,intarr,bul,dpa
     irr,itn,n,f,g,x,xt,f0,fim,iriab,kconstr,eps, wf0,wfc, ientrop,xxr,ndual);USE ModCommons;integer(4) klin,kac(*),itn,n,kconstr,ie
     sntrop,ndual; integer(4),target::intarr(*); integer(1) iriab;real(8) bul(3,*),dparr(*),f,g(n),x(n),xt(n+1),f0,fim,eps,wf0,wfc,x
     ixr(ndual);integer(4) ib,i,n1,itn0;integer(4) j,itg; integer(1) ig(n),ivar;real(8) wm,w,fii,angle(n), alp, pi2a,t01,t3,t,prodve
     nct,ro;real(8),allocatable:: wi(:),tet(:);real(8), pointer::pxl(:),pxu(:);real(8) enk,xbndhuge, wh1,wh2;common/shr/enk,xbndhuge
     t,pxl,pxu;real(8) alr,w1,w0;character chw*128;real(8),allocatable::gii(:,:),ge(:),gn(:),fc(:),xl(:),xu(:); integer(4),allocatab
     ble:: kbt(:);w=wf0; w=wfc;ib=max0(1,kconstr+klin);allocate(gii(n,ib),ge(n),gn(n+2),fc(ib), xl(klin),xu(klin), kbt(ib),stat=i);i
     af(i/=0)then; chw='Can not allocate arrays'; call putmess('S',417,'Calcfg_Dual',chw); goto 79999;endif;xu=xbndhuge; xl=-xbndhug
     te;do i=1,klin-1; if(bul(1,i) <=-xbndhuge) xl(i)=0d0;  if(bul(2,i) >=+xbndhuge) xu(i)=0d0;enddo;ivar=3;if(itn==0) then; itn0=it
     qn; alr=0.5; w1=0d0; w0=w1; endif;allocate(wi(ndual),tet(ndual)     );do i=1,n; xt(i)=x(i); enddo;if(ivar==3) then;alp=1.1d0;pi
     w2a=dasin(1d0)/alp;t01=5d-1/dsin(pi2a);t3=t01*2d0*pi2a;ig=1;  wh1=xbndhuge/2d0; wh2=xbndhuge*1.9d0;do i=1,n; wm=xu(i)-xl(i);if(
     xwm>wh2) Cycle;if(wm<wh1) then;if(wm<=0d0) then; xt(i)=xu(i); ig(i)=0; Cycle; endif;w=xt(i)-xu(i); ib=0;if(w>0d0)then; ib=int(w
     x/wm+1); xt(i)=xt(i)-ib*wm;else; w=xl(i)-xt(i);if(w>0d0)then;  ib=int(w/wm+1); xt(i)=xt(i)+ib*wm;endif;endif;if(mod(ib,2)>0)the
     fn; xt(i)=xu(i)+xl(i)-xt(i); ig(i)=-1;endif;if(1d0<=alp.and.alp<10.) then;w=5d-1*(xu(i)+xl(i));angle(i)=2d0*pi2a*(xt(i)-w)/wm;x
     yt(i)=t01*wm*dsin(angle(i))+w;  ig(i)=int(ig(i)*2,1);endif;else;w=xt(i)-xu(i);if(w>0d0)then;   xt(i)=xu(i)+xu(i)-xt(i); ig(i)=-
     r1;else; w=xl(i)-xt(i);if(w>0d0)then; xt(i)=xl(i)+xl(i)-xt(i); ig(i)=-1;endif;endif;endif;enddo;endif;n1=n;select case(iriab);c
     qase(0);   itg=0;case(1,2); itg=1;case(3);   itg=2;end select;itg=0;if(ientrop<0)then; endif;if(ientrop>0)then;endif;if(n/=klin
     o-2)then; chw='Internal error: Dual entropy: n/=klin-2'; call putmess('S',416,'Calcfg_Dual',chw);goto 79999;endif;do i=1,klin-1
      if(bul(2,i) < xbndhuge)then;if(bul(1,i)>-xbndhuge.and.xt(i)<0d0)then; bul(3,i)=bul(1,i);else; bul(3,i)=bul(2,i);endif;else; if
     j(bul(1,i) > -xbndhuge)then; bul(3,i)=bul(1,i); else; bul(3,i)=0d0; endif;endif;enddo;xt(n+1)=1d0;do i=1,ndual; gn=0d0;do j=kac
     o(i),kac(i+1)-1; gn(intarr(j))=dparr(j); enddo;wi(i)=0d0; do j=1,n+1; wi(i)=wi(i)+gn(j)*xt(j); enddo; tet(i)=gn(klin);enddo;ro=
     twi(ndual); f=0d0;select case(iabs(ientrop));case(271);if(ro<0d0)then;write(chw,'(a,d10.3)')'Dual entropy error: ro<0 ',ro; cal
     wl putmess('W',0,'Calcfg_Dual',chw);endif;if(.false.)then; w=0d0;if(dabs(ro)>1d-200)then;do i=1,ndual-1; wm=-wi(i)/ro-1d0; xxr(
     ri)=tet(i)*dexp(wm); j=0;if(xxr(i)<pxl(i))then; xxr(i)=pxl(i); j=1; elseif(xxr(i)>pxu(i))then; xxr(i)=pxu(i); j=1; endif;if(j==
     l1) wm=dlog(xxr(i)/tet(i));w=w+wm*xxr(i);enddo;xxr(ndual)=w;f=ProdVect(wi,xxr,ndual);else;     f=-1d300; ro=1d0;do i=1,ndual-1;
       wi(i)=0d0; xxr(i)=0d0;if(xxr(i)<pxl(i))then;xxr(i)=pxl(i);elseif(xxr(i)>pxu(i))then;xxr(i)=pxu(i);endif;enddo;endif;elseif(.t
     true.)then; f=0d0;if(ro>0d0)then;do i=1,ndual-1; w=wi(i); wm=w/ro; j=0;if(dabs(wm)<700)then; wi(i)=-wm-1d0; xxr(i)=tet(i)*dexp(
     rwi(i));if(xxr(i)<pxl(i))then;xxr(i)=pxl(i); j=1; elseif(xxr(i)>pxu(i))then;xxr(i)=pxu(i); j=1; endif;else; j=1; if(wm>=0d0)the
     jn; xxr(i)=dmax1(1d-20,pxl(i)); else; xxr(i)=pxu(i); endif;endif;if(j==0)then; f=f-xxr(i); else; w=dlog(xxr(i)/tet(i)); f=f+(wm
     i+w)*xxr(i); wi(i)=w; endif;enddo;f=f*ro;else;do i=1,ndual-1; w=wi(i);if(w>=0d0)then; xxr(i)=dmax1(1d-20,pxl(i)); else; xxr(i)=
     dpxu(i); endif;f=f+w*xxr(i); wi(i)=dlog(xxr(i)/tet(i));enddo;endif;else;ro=wi(ndual); f=0d0;if(dabs(ro)>1d-200)then;do i=1,ndua
     fl-1; wi(i)=-wi(i)/ro-1d0; xxr(i)=tet(i)*dexp(wi(i)); j=0;if(xxr(i)<pxl(i))then;xxr(i)=pxl(i); j=1; elseif(xxr(i)>pxu(i))then;x
     qxr(i)=pxu(i); j=1; endif;if(j==0)then; f=f-xxr(i); else; w=dlog(xxr(i)/tet(i)); f=f+(-wi(i)-1d0+w)*xxr(i); wi(i)=w; endif;endd
     bo;else;     f=-1d300; ro=1d0;do i=1,ndual-1; wi(i)=0d0; xxr(i)=0d0;if(xxr(i)<pxl(i))then;xxr(i)=pxl(i);elseif(xxr(i)>pxu(i))th
     een;xxr(i)=pxu(i);endif;enddo;endif;f=f*ro;endif;do j=1,n; w=-bul(3,j); f=f+xt(j)*w; g(j)=w;enddo;if(.true.)then;xxr(ndual)=Pro
     idVect(wi,xxr,ndual-1);do i=1,ndual; do j=kac(i),kac(i+1)-1; if(intarr(j)>n) Cycle; g(intarr(j))=g(intarr(j))+dparr(j)*xxr(i);e
     lnddo; enddo;else;do i=1,ndual-1; do j=kac(i),kac(i+1)-1; if(intarr(j)>n) Cycle; g(intarr(j))=g(intarr(j))+dparr(j)*xxr(i);endd
     ro; enddo;if(ientrop>0) then; do j=kac(ndual),kac(ndual+1)-1; if(intarr(j)==n) w=dparr(j); enddo;g(n)=g(n)+w*ProdVect(wi,xxr,nd
     vual-1);endif;endif;case(430); w=0d0;if(ro>0d0)then;write(chw,'(a,d10.3)')'Dual Log_Sum error: ro>0 ',ro; call putmess('W',0,'C
     walcfg_Dual',chw);endif;if(.false.)then;do i=1,ndual-1; wm=ro*tet(i);if(wi(i)==0d0)then; if(wm<0d0)then; xxr(i)=pxu(i); else; x
     hxr(i)=pxl(i); endif;else;  xxr(i)=wm/(-wi(i));if(xxr(i)<pxl(i))then;xxr(i)=pxl(i); elseif(xxr(i)>pxu(i))then;xxr(i)=pxu(i); en
     adif;endif;w=w+tet(i)*dlog(xxr(i));enddo;xxr(ndual)=w;f=ProdVect(wi,xxr,ndual);else;do i=1,ndual-1; wm=ro*tet(i); j=0;if(wi(i)<
     o=0d0)then; j=1; xxr(i)=pxu(i);else;if(ro>=0d0)then; j=1; xxr(i)=pxl(i);else; xxr(i)=wm/(-wi(i));if(xxr(i)<pxl(i))then;xxr(i)=p
     mxl(i); j=1; elseif(xxr(i)>pxu(i))then;xxr(i)=pxu(i); j=1; endif;endif;endif;t=tet(i)*dlog(xxr(i));  w=w+t;f=f+t*ro; if(j==0)th
     ben; f=f-wm; else; f=f+wi(i)*xxr(i); endif;enddo;xxr(ndual)=w;endif;do j=1,n; w=-bul(3,j); f=f+xt(j)*w; g(j)=w; enddo;do i=1,nd
     aual; do j=kac(i),kac(i+1)-1; if(intarr(j)>n) Cycle; g(intarr(j))=g(intarr(j))+dparr(j)*xxr(i);enddo; enddo;endselect;f0=f; fim
     v=0d0;f=-f; do j=1,n; g(j)=-g(j); enddo;if(ivar==3) then;do i=1,n;if(ig(i)<0)then;  g(i)=-g(i);do j=1,itg; gii(i,j)=-gii(i,j); 
      enddo;elseif(ig(i)==0) then; g(i)=0d0;do j=1,itg; gii(i,j)=0d0; enddo;endif;if(abs(ig(i))>1) then; w=t3*dcos(angle(i)); g(i)=g
     y(i)*w;do j=1,itg; gii(i,j)=gii(i,j)*w; enddo;endif;enddo;endif;fii=0.;select case(iriab);case(0,1);if(fii>0d0) then;  f=F+ENK*
     ifii; call VectSumAVect(g,g,enk,gii,n);endif;case(2);if(fii>eps) then;  f=F+1d13*FII; call VectMultScal(g,gii,1d0,n);endif;case
     j(3);ge=0d0; gn=0d0; wm=0d0; w=0d0;do j=1,itg;if(kbt(j)/=20) then; if(fc(j)<0d0)Cycle; wm=wm+fc(j);call EdinVect(gii(1,j),gii(1
     g,j),n); call VectSumAVect(gn,gn,1d0,gii(1,j),n);else;  w=w+fc(j);if(fc(j)>eps) then;call EdinVect(gii(1,j),gii(1,j),n); call V
     cectSumAVect(ge,ge,1d0,gii(1,j),n);endif;endif;enddo;call EdinVect(ge,ge,n);fii=wm+w;if(wm>eps) then;  f=1d12+wm;call EdinVect(
     zgn,gn,n);if(ProdVect(ge,gn,n)<=0d0) then;call VectMultScal(g,gn,1d0,n);else;call VectSumAVect(g,gn,1d0,ge,n); call VectMultSca
     bl(g,g,0.5d0,n);endif;else;call EdinVect(g,g,n);if(itn>=itn0+10) then; itn0=itn;w0=w1; w1=w;if(itn>=20) then;if(w1<=0.000000000
     s01) then; alr=alr-0.1;else;t=1+100/10;if(w0/w1<dexp(dlog(w1/eps)/t)) then; alr=alr+0.1;else; alr=alr-0.1;endif;endif;endif;end
     xif;if(alr>0.8)alr=0.8; if(alr<0.2)alr=0.2;if(w<=eps) then;  f=F+w;call VectSumAVect(g,g,1d0,ge,n); call VectMultScal(g,g,0.5d0
     b,n);else;   f=1d12*(1d0+w);if(ProdVect(ge,g,n)<0d0) then;call VectMultScal(g,ge,1d0,n);else;w=(1d0-alr)/alr;call VectSumAVect(
     fg,ge,w,g,n); call VectMultScal(g,g,alr,n);endif;endif;endif;end select
79999 deallocate(gii,ge,gn,fc,xl,xu,kbt,  stat=i);if(allocated(wi))deallocate(wi,tet, stat=i);return;
      end subroutine calcfg_Dual
