      SUBROUTINE Ishtvan(ialgtyp,llin, n0,iqpro, timelimit,ientrop,estmin,lconvex,krecu);USE ModCommons; USE CiFort;integer(4) K, N0
     r, n2;integer(2),allocatable:: ifv (:,:);integer(4),allocatable:: idel(:,:),iup(:,:);real(8),allocatable::  xr(:),
     +fi(:,:),
     +
     +
     +gi(:),
     +fii(:,:),
     +
     +
     +gii(:),
     +pk(:),
     +ddel(:,:),
     +bb(:,:);integer(plen),allocatable:: ixaddr(:);integer(4) N,M,IFAIL,NMAX, MMAX, ME;real(8),allocatable::X(:),U(:), xlt(:),xut(:
     c), x2(:);integer(4) llin,mx, mxmem;integer(4),pointer:: relcode(:),mark(:), kac(:);real(8),pointer :: xs(:),rhs(:),rng(:),dual
     j(:);real(8),allocatable,target::dparr(:);integer(4),allocatable,target:: intarr(:);integer mapsize;parameter (mapsize=30);inte
     pger mapi(mapsize);integer mapdp(mapsize);integer(1) iun,lcf,lkon; logical lconvex;real(8) EPS, wf0,wfc;integer(1),allocatable:
     s:izdual(:);integer(2) kdelmax,iend,k0,kdoubl, Move_Bounds,moveYe,ihvar;integer(4) kr,krp,mk1,mk,mk0, kip,kd,keepr,iqpro,kw,jk0
     q,istg0,km,lmforcuts,ialg0,iend4,moveNb,irep,kfail1,kfail2;integer(4) kitdel,kitmax,i,kiter,kiterp,j,inr,kbt,inrp, ibs,ialgtyp,
     dibst,kdelt1,kdelt2,kmkm,kdeld,iostat,
     +ip, ionemore, istage, kbad, iw, ipmark,ipfree,klin,kiln,kconstr,ndlin,ientrop,krecu,maxkdop,
     +isinit,igun,kkk,kiend,nxdop,kxdop,kxdoppp,krep,lllt,kr0,jm,iconvexviolate,igo10,lpfree,ktodel,ka,kdcon,kdobj;real(8) gmmin,pkm
     win,fkmin,ceps,h0,alp,del,del0,xhuge,w,enk,enk0,hkp,ht,prksi,  dxmin,
     +pkmod,gmi,gmod,efik,zKsi12,efrp,w1,wg,su,dksi1,frr,fr_bex,timelimit,dbound,
     +dksi2,fw,dt,whsec, enkp,qpeps,d_time,pact, xbndhuge, prodvect,vectmod, relmax,estmin,
     +tlmt0,dbound0,ht3,wg3,dt3,gam2,sinfp,sinf,xinf,wr,gam1,estminrr,stime,gap,dhk;real(8),external:: flin_ish,flin_i;integer(4),al
     ulocatable:: iwrk(:); real(8),allocatable:: xwrk(:);real(4) wza(2),wzaf,awm;equivalence(wzaf,awm);logical lf1,lf2,lf3,lf4,lf5,l
     ff6,lf7;common /SetFirst/lf1,lf2,lf3,lf4,lf5,lf6,lf7;logical lfirst, lfza,lp_feas,lscale; equivalence (lf2,lfirst),(lf7,lfza);l
     rogical, external:: lnot_null;integer(2) hr,hmin,hsec,h100, hr0,hmin0,hsec0,h1000, year,mnth,day, day0;integer(2) dat_miss;char
     zacter(256) chw,wch, solv_stat*128,ch9*1;common/iterex/kiter;COMMON /CMACHE/qpEPS,awm;real(8),pointer,save:: xdp(:,:),gdp(:,:,:
     h);real(8), pointer::xl(:),xu(:),gun(:);common/shr/enk,xbndhuge,xl,xu;common/unbnd/gun,igun;i=krecu; estminrr=0.; kr0=0; inrp=0
     h; igo10=0; ipmark=0; ipfree=0; krp=0;iconvexviolate=0; hkp=0.; gmi=0.; dbound=0; dbound0=0.; tlmt0=0.;ch9=char(9);ientrop=0;nu
     fllify(dual,xl,xu,gun,xdp,gdp);CALL SRAND(1234567890); dhk=1d-2;xhuge=1d35;if(.not.lfirst) GOTO 5;call start_stop_whatch(10);wz
     pa=0e0; j=0; iw=-1;i=len_trim(initpname); if(initpname(i:i)==char(10))iw=0;wza(1)=iw; wza(2)=j; w=wza(1)+wza(2);i=int(11.1*accu
     vr);wzaf=real(w,4); w=1d1**(-1-i+13*(iw+j));GMMIN=w; PKMIN=w; FKMIN=w;Dat_Miss=0;      kitdel=1;     ihvar=10;if(.not.lf20) got
     to 111;open(19,file=trim(workpath)//'Ishtinp.dat',err=1,status='old');read(19,'(//)', end=1,err=1);read(19,*, end=1,err=1) llin
      read(19,*, end=1,err=1) KITDEL;read(19,*, end=1,err=1) KITMAX;read(19,*, end=1,err=1) kdelmax;read(19,*, end=1,err=1) enk0;rea
     xd(19,*, end=1,err=1);read(19,*, end=1,err=1) ihvar;close(19);GOTO 2
111   kitdel=-2
1     continue;Dat_Miss=1;kdelmax=2;enk0=7.
2     continue;maxkdop=4;CEPS=0.0d0;H0=1.0;ALP=1.1;    alp=1.5;   alp=dlog10(32.);DEL=1d-2;   del0=del;enk=enk0;if(.not.lf19) goto 2
     i21
221   if(lf20)then; iun=20; else; iun=-2; endif;if(kitdel>0) call Iprint0(iun,workpath,  hr0,hmin0,hsec0,h1000,day0);qpeps=EPSILON(e
     gps);EPS=qpeps;qpeps=w; w=1.185e1+nnew;call CheckProblem(llin,n0,iqpro,kconstr,klin,kiln,ndlin,kd,lcf,timelimit,fkmin,ientrop,e
     pstmin,lconvex);if(ioutk>=istop-1) goto 79999;tlmt0=timelimit;if(klin>256000.or.ndlin+n0>512000.and.llin>0) then;if(ialgtyp==1)
     q chw='Linearization exceeds capacity of Car solver';if(ialgtyp==2) chw='Linearization exceeds capacity of Tank solver';call pu
     ctmess('W',0,'Cutting plain initialization',chw);llin=0;call CheckProblem(llin,n0,iqpro,kconstr,klin,kiln,ndlin,kd,lcf,timelimi
     ut,fkmin,ientrop,estmin,lconvex);if(klin>256000) then; llin=-1;call CheckProblem(llin,n0,iqpro,kconstr,klin,kiln,ndlin,kd,lcf,t
     kimelimit,fkmin,ientrop,estmin,lconvex);endif;endif;n0=min0(n0,int(w,4));if(n0<=0)then; chw='Zero number of variables for CAR (
     aCarGrb or HELI) Solver';call putmess('W',0,'CAR (CarGrb or HELI) initialization',chw);endif;if(ioutk>=istop-1) goto 79999;if(D
     cat_Miss==1) KITMAX=int(N0*100*(1+accur*4));N2= N0 + ndlin;N = N2+2;M=int((N0+2)*((2.1*(1.0-dlog(dfloat(n))/dlog(10.)/8)-1.)/10
     u.+1.));M=max0(M,N0+2+3)+(klin-kd)+maxkdop;ALLOCATE(xr(n),FI(3,M),gi(n),FII(3,M),gii(n),XL(N),XU(N),X(N),PK(N),IFV(3,M),ixaddr(
     iM),iup(3,M),
     +idel(0:1,kdelmax+1+maxkdop), ddel(0:1,0:kdelmax+1+maxkdop),xlt(n),xut(n), STAT=iostat );if(iostat.ne.0) then; chw='Allocation_
     n1 is failed'; call putmess('S',407,'Car initialization',chw); goto 79999;endif;ixaddr=0; iup(1:3,1:M)=0;x=0d0; xr=0d0; xl=-xhu
     jge; xu=+xhuge; xlt=-xhuge; xut=xhuge;gi(1:n-1)=0d0; gii(1:n)=0d0; gi(n)=-1d0; gii(n-1)=-1d0;call init_x(lconvex,n0,n0,1,
     +xr,k0,xl,xu,xbndhuge,isinit);if(ioutk==istop) goto 79999;if(n0<=0)then; kr=1; solv_stat='calculated'; goto 991; endif;call Get
     cBuff(1,n2,1, x,i);w1=0d0; j=0;xl(n-1)=-ceps; xlt(n-1)=xl(n-1);do i=1,n0;if(xl(i)>-xbndhuge.and.xu(i)<xbndhuge)then; w1=w1+xu(i
     s)-xl(i); j=j+1;else; w=dmin1(dabs(xl(i)),dabs(xu(i)));if(w>0d0.and.w<xbndhuge)then; w1=w1+w; j=j+1; endif;endif;enddo;if(j>0) 
     vw1=w1/j;if(w1<pkmin) w1=dmax1(1.,100*pkmin);w1=dmin1(1.,w1);dbound0=w1;if(k0<=0) then; k0=1; do i=1,n0; xr(i)=0d0; enddo;elsei
     bf(k0>=1) then; k0=1;endif;jk0=0;do iw=1,k0;  do i=1,n0;if(xr(i).lt.xl(i)) then;      xr(i)=xl(i);else if(xr(i).gt.xu(i)) then;
       xr(i)=xu(i);endif;enddo;       enddo;do i=1,n;if(xl(i)<=-xbndhuge)xl(i)=-xhuge; if(xu(i)>=xbndhuge)xu(i)=xhuge;enddo;if(kitde
     dl>0) call Iprint1(n0,n,m,kitdel,kitmax,gmmin,pkmin,fkmin,xr,dat_miss,lf21);MK=M+1;MMAX = MK+1;NMAX = N;w=10.; if(nmax>1000.and
     v.mmax>1000) w=min(nmax,mmax)/20;do j=61,1,-5;w1=dmax1( 4000000.,(mmax*(nmax*j/w) + nmax*(nmax*j/w) + mmax*(nmax*j/w) + mmax*(n
     vmax/5*j/w)));mxmem=int(dmin1(2e8,w1));if(allocated(dparr)) deallocate(dparr,stat=iostat); if(allocated(intarr)) deallocate(int
     barr,stat=iostat);allocate(dparr(mxmem),intarr(mxmem),STAT=iostat);if(iostat==0)then; allocate(bb(2,2*klin),STAT=iostat); Exit;
       endif;enddo;if(iostat.ne.0)then; chw='Allocation_3 is failed'; call putmess('S',410,'Car initialization',chw); goto 79999;end
     nif;dparr=0d0; intarr=0; kac=>intarr(mxmem+1-n-1:);kac=1; mk0=0; mk1=0; ipfree=1;lkon=0; ME=0;k=0;if(klin>0.or.ndlin>0)then; kl
     cin=klin-kd;allocate(izdual(klin+kd)); izdual=1;call GetLinearForIshtvan(0,fkmin,llin,mxmem,n0,n,klin,ndlin,kconstr,xhuge,
     +lcf,kac,izdual,xl,xu,lkon,me,intarr,dparr,bb,lmforcuts,chw);do k=1,klin; fi(2,k)=-bb(1,k); fii(2,k)=-bb(2,k);fi(1,k)=1d15;  fi
     t(3,k)=0d0; ifv(1,k)=-1;fii(1,k)=1d15; fii(3,k)=0d0; ifv(2,k)=1; ifv(3,k)=2;enddo;ipfree=kac(n+1); k=klin; mk=k; mk0=0; mk1=mk;
      elseif(kconstr<=0)then; lkon=1;endif;if(ioutk==istop) goto 79999;if(kitdel>0) call Iprint2();lfirst=.false.
5     continue;igun=0;solv_stat=''; kr=1;ialg0=ialgtyp;dxmin=dsqrt(pkmin);istg0=-1
7     continue;enk=enk0; if(solv_stat=='infeasible'.and.alp<3.) ALP=ALP*dlog10(32.);istg0=istg0+1;call What_To_Do_Isht(istg0,n0,fkmi
     kn,intarr,kac,   xr,    dparr,bb,istage,ionemore,   solv_stat);if(ioutk==istop) goto 79999;do i=1,klin; fi(2,i)=-bb(1,i); fii(2
     f,i)=-bb(2,i);enddo;do i=n0+1,n-2; xlt(i)=xl(i); enddo;if(lcf==1.and.lkon==1) then;do i=1,n0; xlt(i)=xl(i); xut(i)=xu(i);enddo;
      else;do i=1,n0; if(xlt(i)<xl(i))xlt(i)=xl(i); if(xut(i)>xu(i))xut(i)=xu(i);if(xlt(i)>=xu(i))then; w=0d0; if(xu(i)>xl(i)) w=min
     r(1d1,xu(i)-xl(i)/2); xlt(i)=xu(i)-w; endif;if(xut(i)<=xl(i))then; w=0d0; if(xu(i)>xl(i)) w=min(1d1,xu(i)-xl(i)/2); xut(i)=xl(i
     n)+w; endif;enddo;dbound=dbound0;xr(1:n0)=max(min(xr(1:n0),xu(1:n0)),xl(1:n0));do i=1,n0; xlt(i)=dmax1(xl(i),xr(i)-dbound0); xu
     bt(i)=dmin1(xu(i),xr(i)+dbound0);enddo;endif;if(istage==0)then; call restart_stop_whatch(1,w); w=max(w,0.01);write(wch,'(f10.2)
     b')w; wch=ADJUSTL(wch); wch='Preprocessing time(sec) '//trim(wch); tm_PR=w;call putmess('T',0,'Problem preprocessing',wch); if(
     klf21)write(21,"(/a,f7.2)")'  Preprocessing time(sec) ',w;endif;if(ionemore==0) goto 991;if(istage==0)then;ipmark=0; ibs=0; ifa
     hil=1;else;if(lcf==1.and.lkon==1.and.istage>0) GOTO 9;call ChangeMark(n,klin,xlt,xut,rng,x, mark);endif
9     continue;call delete_cuts(n,klin,intarr,dparr,kac, i);do i=1,n2;x(i)=xr(i);end do;K=klin;kkk=k;KR=K+1;Efrp=xhuge/16.;Efik=xhug
     ze/16.; zKsi12=0.; wg=0.; gmod=0.; wf0=1.; wfc=1.; relmax=0.;ht=1.;ht3=1d0; wg3=0d0; dt3=1d0;x(n)=xl(n);prksi=x(n)/2.;x(n-1)=xl
     b(n-1);pkmod=1d+99;inr=0;kfail1=0; kfail2=0;ibst=1;irep=0;moveNb=0;moveYe=1;kbad=-1;iend4=0;kdoubl=0;su=999999.;kdeld=0; kmkm=0
      fr_bex=1d+99;keepr=0; frr=1d+99;KITER=-1;kiterp=0;jk0=1;IEND=0;enkp=enk;w=d_time('s',int2(0));lscale=.true.;kiend=0;gam2=1.;si
     vnfp=0d0;nxdop=-1; kxdop=-1;kxdoppp=0;lp_feas=.false.;krep=4;if(.not.associated(xdp))allocate(xdp(1:n0,0:maxkdop),stat=i);if(.n
     fot.associated(gdp))allocate(gdp(-2:n,0:1,0:maxkdop));gdp=0.; gdp(n,0,:)=-1d0; gdp(n-1,1,:)=-1d0;lllt=1;ioutk=3;wch='Start opti
     xmization';if(istage>0)then;if(istage>=istg0)then; write(wch,'(a,i2)')'Start stage ',istage;else; i=istg0-istage;write(wch,'(a,
     qi7)')'Polishing',istg0-istage;endif;endif;call putmess('n',0,'Car/Tank',wch);call restart_stop_whatch(10,w)
10    K=K+1;KITER=KITER+1;if(igun>0)then;endif;if(nxdop<kxdop)then; nxdop=nxdop+1;x(1:n0)=xdp(1:n0,nxdop);if(gdp(-1,0,nxdop)==1.)the
     sn; call getGiGii(n,n0,iup(3,k),fi(1,k),gi,fii(1,k),gii,relmax,  gdp(-2:n,0:1,nxdop));else; km=0;CALL calcfg(n0,n0,x,km, iup(3,
     wk),fi(1,k),gi,fii(1,k),gii,kbt, wf0,wfc,relmax);call saveGiGii(n,n0,iup(3,k),fi(1,k),gi,fii(1,k),gii,relmax,  gdp(-2:n,0:1,nxd
     cop));endif;else; km=0;CALL calcfg(n0,n0,x,km, iup(3,k),fi(1,k),gi,fii(1,k),gii,kbt, wf0,wfc,relmax);endif;if(ioutk==istop) the
     vn; if(k>1)k=k-1; goto 13; endif;fi(2,k)=+fi(1,k)-ProdVect(gi,x,n0);  fii(2,k)=+fii(1,k)-ProdVect(gii,x,n0);gmod=VectMod(gi,n0)
     i; gmi=VectMod(gii,n0);wg=gmod; if(fii(1,k) > -ceps) wg=gmod+enk*gmi;fi(3,k)=gmod;  fii(3,k)=gmi;if(lllt==1)then;ifv(1,k)=1; if
     wv(2,k)=1; ifv(3,k)=1;if(relmax<fkmin) ifv(3,k)=2;else; ifv(1,k)=0; ifv(2,k)=0; ifv(3,k)=0;endif;iup(2,k)=0;if(jk0<=k0.or.ifail
     g>0) then;do i=1,n0;if(x(i).lt.xl(i)-dxmin.or.x(i).gt.xu(i)+dxmin) then;ifv(1,k)=0; ifv(2,k)=0; ifv(3,k)=0; Exit;endif;enddo;if
     o(klin>0)then;km=200; CAll CalcFuns(n0,x,km,  w,w1,kbt, wf0,wfc,su);if(w1>fkmin)then; ifv(1,k)=0; ifv(2,k)=0; ifv(3,k)=0; endif
      endif;endif;if(fii(1,k)<=-ceps.and.gmi<=0d0)  ifv(2,k)=-1;if(lcf==1)then; ifv(1,k)=-1;endif;zKsi12 = X(N)+enk*X(N-1);krp=kr;kr
     d=klin+1; Efrp=fi(1,kr)+enk*dmax1(-ceps,fii(1,kr));do j=klin+2,k-1;Efik=fi(1,j)+enk*dmax1(-ceps,fii(1,j));if(Efik<Efrp.and.(ifv
     u(3,j)>0.and.ifv(3,kr)>0.or.ifv(3,j)<1.and.ifv(3,kr)<1) .or. ifv(3,j)>ifv(3,kr)) then;kr=j;  Efrp=Efik;endif;enddo;if(kr>=k) th
     ren; call saveXR(krp,xr,kr,fii,ceps,n2, ixaddr);do i=1,n2; xr(i)=x(i); enddo;endif;Efik=fi(1,k)+enk*dmax1(-ceps,fii(1,k));inrp=
     jinr; inr=0;if(Efik<=Efrp.and.(ifv(3,k)>0.and.ifv(3,kr)>0.or.ifv(3,k)<1.and.ifv(3,kr)<1).or.ifv(3,k)>ifv(3,kr)) then;if(ifv(3,k
     a)>ifv(3,kr)) Efrp=Efik+(dabs(Efrp)+dabs(Efik))*0.1;call saveXR(kr,xr,k,fii,ceps,n2, ixaddr);do i=1,n2; xr(i)=x(i); enddo;  kr=
     rk;if(Efik<Efrp)then; inr=1; kbad=-1; endif;end if;kbad=kbad+1;kr0=0; w=xhuge;do j=1,k; if(ifv(3,j)==2.and.fi(1,j)<w)then; kr0=
     gj; w=fi(1,j); endif; enddo;if(kr0==0) kr0=kr
13    continue;if(ioutk==istop) iend=8;if( inpk==1 ) then; iend=6;if(inpp==1)kitdel=-1; goto 14;endif;if(iend>0) goto 14;if( KITER >
     l= KITMAX ) iend=2;call Check_stop_whatch(1,w); if(w>timelimit) iend=23;if(jk0<=k0.or.nxdop<kxdop) goto 14;if(lcf==1.and.lkon==
     s1) then;select case(ifail);case(0);  iend=100;case(1);  iend=101;case(11); iend=111;case(23); iend=23;case(12:22,24:);  iend=1
     h12;case default; iend=102;end select;GOTO 14;endif;if(enkp==enk.and.ifv(1,k)<0.and.ifv(2,k)<0.and.k/=kr) iend=21;if( kdoubl.gt
     w.6 ) iend=22;if( kfail1.gt.10 ) iend=51;if( kfail2.gt.0 ) then; iend=52; if(ifail==23) iend=23; endif;if((gmod>0d0.and.gmod+en
     tk*gmi>gmod*1d15.and.enk>1d10)
     +.or.((gmod==0d0.or.gmi==0d0).and.enk>1d15)) iend=7;if( pkmod.lt.pkmin.and.inr<1.and.x(n-1)<=-ceps.and.ifail==0.and.ifv(3,kr)>1
     t) then;if(iend4>3) then; iend=3; else; iend4=iend4+1; endif;endif;w=dmin1(1d0,dmax1(1d-10,fkmin*0.5*dmax1(dabs(Efrp)+dabs(zKsi
     x12),0.)));fw=w;if(Efrp-zKsi12<w.and.inr<1.and.x(n-1)<=-ceps.and.ifail==0.and.ifv(3,kr)>1) then;iend4=iend4+1; if(iend4==2) enk
     i=enk*alp; if(iend4>3) iend=4;endif;if((fi(1,kr)*dabs(wf0)-estmin)<fkmin.and.fii(1,kr)<fkmin) iend=40;iw=int(1d0+gmmin-1d-14); 
      w=1-iw;if( wg.lt.gmmin .and. lconvex) iend=1;if( wg.lt.gmmin .and. .not.lconvex .and. k==kr) iend=1;if(Efrp-zKsi12 <= scale(da
     dbs(Efrp)+dabs(zKsi12),-iw).and.w>23d-6) then;k=k+1;endif;if(efrp<frr)then;if((frr-efrp)*2/(dabs(frr)+dabs(efrp))>fkmin.and.(fr
     dr-efrp)*dabs(wf0)>fkmin)then;frr=efrp; keepr=0; kkk=k; if(kiter==k0)keepr=int(-5*alog(10.*n));else; keepr=keepr+1;endif;else; 
      frr=efrp; keepr=keepr+1;endif;if(keepr>100*alog10(n+0.5)+10)then;if(k<=kkk)then; iend=41;else; kkk=k; keepr=keepr-10;endif;end
     xif;if(ifail==11) iend=1011;if(ifail>11.and.ifail/=23)  iend=1012;select case(iend); case(21,22,3,4,1);if(moveYe==0) moveYe=Mov
     ie_Bounds(xhuge,1d0,ifail,n0,x,xl,xu,xlt,xut,dxmin);if(moveYe>0) then; iend=0; iend4=0;elseif(moveNb<3) then;  moveNb=moveNb+1;
       iend=0;endif;if(ht>0d0.and.ht<1d0)then; ht=-2d0; if(iend==22)then; kdoubl=kdoubl-int(3,2); iend=0; endif;endif;case (0); if(h
     zt==-2d0) ht=1d0;end select
14    continue;if ((kiter.ge.kiterp.or.iend.ne.0).and.kitdel.gt.0) then;write(20,'(i6,4i5,1P,99(d15.7))')
     +KITER,K,KR,mk0,mk,Efrp,Efik,zKsi12,Efrp-zKsi12,pkmod, wg,
     +ht3, fii(1,kr),fii(1,k),x(n-1),enk;kiterp=kiterp+kitdel;endif;wch='';if(d_time('s',int2(2))>2.or.iend.ne.0) then;w=d_time('s',
     vint2(0)); write(wch,'(i10.10)')kiter; i=verify(wch,'0'); if(i<1.or.i>10)i=10;write(wch,'(a,2(a,e18.12))')'Ext.iteration='//wch
     s(i:10),'  Objective=',fi(1,kr)*wf0,'  Residual=',fii(1,kr);endif;call putmess('n',0,' ',wch);if((wch=='S'.or.wch=='T').and.ien
     ad==0) goto 13;IF(IEND.NE.0)then; kiend=kiend+1;if(iend==1.or.iend==40)then; if(kiend>1) GO TO 990;elseif(kiend>0)then; GO TO 9
     j90;endif;iend=0;endif;if(jk0<=k0.or.nxdop<kxdop) goto 15;enkp=enk;if (x(n-1).gt.-ceps+1d-20)then;enk=enk*alp;endif;if (fii(1,k
     r).lt.0d0) then;ceps=ceps/1.5;if(x(n-1).lt.-ceps) x(n-1)=-ceps;xl(n-1)=-ceps;  xlt(n-1)=-ceps;endif;iw=kr;kr=klin+1;        Efr
     ap=fi(1,kr)+enk*dmax1(-ceps,fii(1,kr));do j=klin+2,k;    Efik=fi(1,j)+enk*dmax1(-ceps,fii(1,j));if(Efik<=Efrp.and.(ifv(3,j)>0.a
     ind.ifv(3,kr)>0.or.ifv(3,j)<1.and.ifv(3,kr)<1).or.ifv(3,j)>ifv(3,kr)) then;kr=j;  Efrp=Efik;endif;enddo;if(kr/=iw)then; call re
     sstoreXR(kr,xr,n2, ixaddr);endif;prksi=x(n)+enk*x(n-1);hkp=dbound;if(inrp.eq.1.and.inr.eq.1.and.zKsi12.le.prksi)then;elseif(kba
     wd>4+log10(float(n))*2.and.x(n-1).lt.-ceps) then;if(dbound>10*pkmin/sqrt(float(n))) dbound=dbound/1.2;do i=1,n0; xlt(i)=dmax1(x
     gl(i),xr(i)-dbound*(0.1+rand())); xut(i)=dmin1(xu(i),xr(i)+dbound*(0.1+rand()));enddo;endif;if(.not.lconvex)then; iw=0;if(kr/=k
     arp)then; i=klin+1;else; i=k; goto 147;endif;jm=klin;do j=klin+1,k-1;if(ifv(2,j)>=0)then; jm=jm+1; if(j<i.or.j==kr)Cycle;w=flin
     j_Ish(n0,fii,j,xr,jm,intarr,dparr,kac);if(w>-ceps.and.fii(1,kr)<w-fkmin)then; iw=1;if(fii(1,kr)<=fii(1,j).or.fii(1,kr)<=-ceps)t
     ehen;fii(2,j)=fii(2,j)-max((w-fii(1,kr))*1000.,1e2);endif;endif;endif;enddo;do j=klin+1,k-1;if(ifv(1,j)>=0)then; jm=jm+1; if(j<
     wi.or.j==kr)Cycle;w=flin_Ish(n0,fi,j,xr,jm,intarr,dparr,kac);if(fi(1,kr)<w-dxmin)then; iw=1;fi(2,j)=fi(2,j)-max((w-fi(1,kr))*10
     a00.,1e2);endif;endif;enddo
147   continue;if(k/=kr)then;if(ifv(1,k)>=0)then; w=flin_I(fi,gi,xr,k,n0);if(fi(1,kr)<w-dxmin)then; iw=1;fi(2,k)=fi(2,k)-(w-fi(1,kr)
     w);ifv(1,k)=-1;endif;endif;if(ifv(2,k)>=0)then; w=flin_I(fii,gii,xr,k,n0);if(w>-ceps.and.fii(1,kr)<w-fkmin)then; iw=1;if(fii(1,
     ykr)<=fii(1,k).or.fii(1,kr)<=-ceps)then;fii(2,k)=fii(2,k)-(w-fii(1,kr));ifv(2,k)=-1;endif;endif;endif;endif;iconvexviolate=iw;e
     vndif
15    continue;call LinZav_MILP_1(k,kr,n0,fi,gi,fii,gii, mk1,intarr,dparr,kac,klin,  ifv,kdoubl);if((jk0>k0.or.nxdop<kxdop).and.enkp
     k==enk.and.hkp==dbound.and.ifv(1,k)<0.and.ifv(2,k)<0) then;if(ht>0d0.and.ht<1d0)then; ht=-1d0; else; if(moveYe<0) goto 13; endi
     wf;elseif(ht==-1d0)then; ht=1d0;endif;if(jk0<=k0)then; jk0=jk0+1; if(jk0<=k0)then; x(1:n2)=xr(1:n2); GOTO 10; endif; endif;if(n
     vxdop<kxdop) goto 10;if(.not.lconvex.and.k/=kr.and.(jk0>k0.and.ifv(1,k)<0.and.ifv(2,k)<0.and.x(n-1)<=-ceps+1d-20   .or.
     +iconvexviolate>0 ))then;w=rand()*0.1 + pkmin; x(:n2)=w*x(:n2)+(1.-w)*xr(:n2);x(n-1)=0.; x(n)=fi(1,kr)-fkmin*100000;if(igo10<3.
     eand.k<m/2)then; igo10=igo10+1; goto 10;endif;endif;iconvexviolate=0; igo10=0;if(ifail<=0.and.inr==1.and.moveYe==0) then;moveYe
     n=Move_Bounds(xhuge,1d0,ifail,n0,x,xl,xu,xlt,xut,dxmin);endif
17    mk0=0; mk1=0;do j=1,k; if(ifv(1,j)>=0) mk0=mk0+1;  if(ifv(2,j)>=0) mk1=mk1+1;enddo;mk=mk0+mk1;mx=mk+1;call ssx10(mx,n, mxmem, 
     x workpath,iun,
     +mapi, mapdp );lpfree=min0(mapi(13),mapdp(7))-1;if(ipfree+2*n>lpfree) then;write(wch,'(a,i2)')'It is not enough memory for MILP
     f'; call putmess('S',4014,'Car/Tank',wch); goto 79999;endif;xs=>dparr(mapdp(2):mapdp(2)+n+mx-1);rhs=>dparr(mapdp(14):mapdp(14)+
     lmx-1);rng=>dparr(mapdp(15):mapdp(15)+mx-1);dual=>dparr(mapdp(17):mapdp(17)+mx-2);relcode=>intarr(mapi(12):mapi(12)+mx-1);mark=
     w>intarr(mapi(9):mapi(9)+n+mx-1);rng=xhuge; relcode=1;do j=1,klin;if(ifv(2,j)>=0) then; rng(j)=-fii(2,j)+fi(2,j);if(rng(j)==0d0
     r) relcode(j)=0;endif;enddo;if(irep==0)then;if(igun>0)then; kip=0; do j=1,klin; if(ifv(2,j)>=0) kip=kip+1; enddo;call insert_co
     fnstr(n,gun(1:n), kip,intarr,dparr,kac);elseif(kxdop<=0)then;if(ifv(2,k)>=0) call insert_constr(n,gii,mk1,intarr,dparr,kac);if(
     nifv(1,k)>=0) call insert_constr(n,gi, mk, intarr,dparr,kac);else; j=0; do i=kxdop,0,-1; if(ifv(2,k-kxdop+i)>=0)j=j+1; enddo;do
     x i=kxdop,0,-1;if(ifv(2,k-kxdop+i)>=0)then; call insert_constr(n,gdp(1:,1,i),mk1-j+1,intarr,dparr,kac); endif;enddo;j=0; do i=k
     vxdop,0,-1; if(ifv(1,k-kxdop+i)>=0)j=j+1; enddo;do i=kxdop,0,-1;if(ifv(1,k-kxdop+i)>=0)then; call insert_constr(n,gdp(1:,0,i),m
     mk-j+1, intarr,dparr,kac); endif;enddo;endif;endif;mk=0;do j=1,k; if(ifv(2,j) >= 0) then; mk=mk+1; rhs(mk)=-fii(2,j); endif;end
     ido;mk1=mk;do j=1,k; if(ifv(1,j) >= 0) then; mk=mk+1; rhs(mk)=-fi(2,j);  endif;enddo;if(irep==0) then;pk(1:n-2)=0d0; pk(n-1)=en
     vk; pk(n)=1d0; if(k==klin) pk(n)=0d0;call delete_point(n,0,k, ip,intarr,dparr,kac,ifv,mk,mk0,mk1);call insert_constr(n,pk, mx,i
     bntarr,dparr,kac);endif;rhs(mk+1)=0d0; relcode(mk+1)=3;ipfree=kac(n+1);if(irep>0) goto 18;if(ipmark>0) then;if(mapi(9)<ipmark) 
     hthen; iw=ipmark-1;do i=1,n+klin; mark(i)=intarr(iw+i); enddo;elseif(mapi(9)>ipmark) then; iw=ipmark-1;do i=n+klin,1,-1; mark(i
     n)=intarr(iw+i); enddo;endif;else;mark=1; do i=1,n; mark(i)=4; enddo;call ChangeMark(n,klin,xlt,xut,rng,x, mark);endif;ipmark=m
     sapi(9);i=n+klin;do j=klin+1,k;if(ifv(2,j)>=0) then; i=i+1; mark(i)=6; if(ifv(2,j)>=10) mark(i)=2; endif;enddo;do j=klin+1,k;if
     m(ifv(1,j)>=0) then; i=i+1; mark(i)=6; if(ifv(1,j)>=10) mark(i)=2; endif;enddo;mark(n+mx)=7
18    continue;if(lcf==1.and.lkon==1.and.istage==0.and.irep==0) ibst=1;if(lcf==1.and.lkon==1.and.istage>0.and.ifail==0) ibst=1;if(ki
     fter>0.and.ibst==3) ibst=1;select case(irep);case(1); ibst=1;case(2); ibst=1; ialgtyp=3-ialgtyp;case(3); ibst=2;case(4); ibst=2
     m; ialgtyp=3-ialgtyp;end select;moveYe=0;call ssx11(int(0),mx,n,lpfree-ipfree+1, xlt,xut, mapi,mapdp, ialgtyp,lscale,
     +ibs,ibst,timelimit,
     +dparr,intarr, dparr(ipfree),intarr(ipfree),
     +ifail,sinf );ifail=ifail - 1;iw=ifail; w=0d0;do i=1,n; if(xlt(i)>-xhuge)then; x(i)=xs(i)+xlt(i); w=dmin1(w,xs(i)); else; x(i)=
     bxs(i); endif;w=dmin1(w,xut(i)-x(i));enddo;goto 185
185   continue;if(ioutk>=istop-1) goto 13;if(-w>dxmin)then;if(iw<=0.and.-w<dsqrt(dxmin))then;write(wch,'(a,i3,a,i6,a,1p,e10.3)')'Sta
     dge',istage,'. Iteration',kiter,'. Variables bounds violation is',-w;if(lf21)write(21,'(a)') wch;else; ifail=11;endif;endif;if(
     qifail>0) then;if(kitdel.ge.0) write(20,'(a,i4.3,a,4i3)')'Ishtvan IFAIL=',ifail,'   ialg,ibs,ibst,irep',ialgtyp,ibs,ibst,irep;i
     ef(ifail==11.and..not.lp_feas) then;xinf=1.; if(sinfp>sinf.and.sinf>0) xinf=dmax1(xinf,sinf/(sinfp-sinf));xinf=dmin1(xinf**2,2d
     u1);sinfp=sinf;if(moveYe==0) moveYe=Move_Bounds(xhuge,xinf,ifail,n0,x,xl,xu,xlt,xut,dxmin);if(moveYe>0)then; irep=1; ibs=0;if(k
     titdel.ge.0) write(20,'(a)')'   Bounds were moved';goto 17;endif;endif;if(kitdel.ge.0) write(20,'(a)')' ';iw=0; if(.not.(lcf==1
     f.and.lkon==1))iw=1;if(iw==1)then;if(ifail<=11.and.(ibs>0.or.irep<krep)) then; irep=irep+1; if(ibs==0.and.irep==1)irep=2;if(ire
     yp==krep)then; ibst=1; endif;ibs=0; goto 17;elseif(ifail>11.and.ibs>0)then; irep=irep+1; ibs=0; goto 17;endif;endif;else; lp_fe
     aas=.true.;if(irep/=0.and.kitdel>=0) write(20,'(a,i4.3,a,4i3)')'Ishtvan IFAIL=',ifail,'   ialg,ibs,ibst,irep',ialgtyp,ibs,ibst,
     hirep;ibs=1;endif;irep=0;ialgtyp=ialg0;if(ifail>0)then;  if(ifail==11.and.lp_feas) ifail=2;if(ifail<4) then; kfail1=kfail1+1; e
     wlse; kfail2=kfail2+1; goto 13; endif;endif;if(lcf==1.and.lkon==1) GOTO 10;pkmod=0d0;do i=1,n0; pk(i)=x(i)-xr(i);  pkmod=pkmod+
     wpk(i)*pk(i);  enddo;pkmod=dsqrt(pkmod);if(igun>0) GOTO 10;nxdop=-1;kxdop=-1; gdp(-1,0,:)=0.;goto 190;if((kiter/3)*3==kiter) go
     cto 190;if(ihvar<0) goto 190;if(ht>0d0.and.kiter>0.and.ifv(3,kr)>0.and.igun==0) then;ht=1d0;kxdop=0; xdp(1:n0,0)=x(1:n0);km=0; 
       CALL calcfg(n0,n0,xdp(1,kxdop),km, iw, w,gi,w1,gii,kbt, wf0,wfc,relmax);if(ioutk==istop) goto 13; call saveGiGii(n,n0,iw,w,gi
     n,w1,gii,relmax,  gdp(-2:n,0:1,0));w=w+enk*dmax1(-ceps,w1);if(w>efrp+abs(efrp)*0.0)then; w1=(w-efrp)/(dabs(w)+dabs(efrp));if(w*
     xefrp<=0.)then; w=0.5; if(ihvar==0) w=1.;else; w=w1;endif;wr=rand();gam1=0.;gam2=1.   ; if(kbad>3)gam2=0.9;select case(ihvar);c
     dase(0); ht= (0.3+0.4*wr)*(1.0-(1.0-dhk*2.)*w);dhk=dhk/13.;  if(dhk<0.0001) dhk=0.4;case(1); ht= 0.1*(0.9+0.2*wr);case(2); ht= 
     v0.2*(0.9+0.2*wr);case(3); ht= 0.3*(0.9+0.2*wr);case(4); ht= 0.4*(0.9+0.2*wr);case(5); ht= 0.5*(0.9+0.2*wr);case(10); if(kbad>3
     w)wr=wr*wr;ht= (gam1+gam2*wr)*(1.-w);case(11); ht= 0.1+0.05*(2*wr-1);case(12); ht= 0.2+0.05*(2*wr-1);case(13); ht= 0.3+0.05*(2*
     fwr-1);case(14); ht= 0.4+0.05*(2*wr-1);case(15); ht= 0.5+0.05*(2*wr-1);end select;if(ht<0.1.and.(kiter/3)*3-kiter==-2) ht=0.5;i
     cf(kxdop+1>=M-K) goto 190;kxdop=kxdop+1; do i=1,n0; xdp(i,kxdop)=xr(i)+pk(i)*ht; enddo;endif;endif
190   continue;goto 192;if(kxdop>0.or.igun>0) goto 192;if((kiter/5)*5/=kiter) goto 192;if(ht>0d0.and.kiter>0.and.ifv(3,kr)>0) then;j
     w=0; ht=1d0; wg=0d0; dt=1d0;kxdop=0; xdp(1:n0,0)=x(1:n0)
191   km=0;  CALL calcfg(n0,n0,xdp(1,kxdop),km, iw, w,gi,w1,gii,kbt, wf0,wfc,relmax);if(ioutk==istop) goto 13; call saveGiGii(n,n0,i
     aw,w,gi,w1,gii,relmax,  gdp(-2:n,0:1,kxdop));if(w1<=0d0)then; w1=0d0; do i=1,n0; w1=w1+pk(i)*gi(i); enddo;else; w=w+enk*w1; w1=
     u0d0; do i=1,n0; w1=w1+pk(i)*(gi(i)+enk*gii(i)); enddo;endif;if(w1<=0d0) wg=ht;dt=dt/(4.+rand()); ht=wg+dt;if(klin>0.and.ht>1.)
     kthen; ht=ht-dt; goto 192; endif;if(kxdop+1>=M-K) goto 192;kxdop=kxdop+1; do i=1,n0; xdp(i,kxdop)=xr(i)+pk(i)*ht; enddo;j=j+1; 
      if(j>=5.and.(w1>0d0.or.ht>1d0).or.j>=maxkdop) goto 192;goto 191;endif
192   continue;lllt=1;if(kxdop>0.or.igun>0) goto 194;if(ht>0d0.and.kiter>0.and.ifv(3,kr)>0) then;if((kiter/6)*6==kiter)then; ht3=1d0
     w; wg3=0d0; dt3=1d0;endif;kxdop=kxdop+1; do i=1,n0; xdp(i,kxdop)=xr(i)+pk(i)*ht3; enddo;km=0;  CALL calcfg(n0,n0,xdp(1,kxdop),k
     vm, iw, w,gi,w1,gii,kbt, wf0,wfc,relmax);if(ioutk==istop) goto 13; call saveGiGii(n,n0,iw,w,gi,w1,gii,relmax,  gdp(-2:n,0:1,kxd
     cop));if(ht3>1..or..not.lp_feas)lllt=0;if(w1<=0d0)then; w1=0d0; do i=1,n0; w1=w1+pk(i)*gi(i); enddo;else; w=w+enk*w1; w1=0d0; d
     ao i=1,n0; w1=w1+pk(i)*(gi(i)+enk*gii(i)); enddo;endif;if(w1<=0)then;  ht3=ht3*(1.+rand());else;           ht3=ht3*(1.-rand());
      endif;endif
194   continue;ktodel=kdelmax;kxdoppp=kxdop;pact=1.1; if(kbad>10) pact=1.25;mk=0;  kbt=1;kip=0; iw=0;do j=1,k; if(ifv(2,j)<0) Cycle;
      mk=mk+1;if(mark(n+mk)<=3)then; kip=kip+1; if(ifv(2,j)<10)ifv(2,j)=ifv(2,j)+int(10,2); iup(2,j)=mk;else; if(ifv(2,j)>=10)ifv(2,
     lj)=ifv(2,j)-int(10,2); iup(2,j)=0;if(j/=kr.and.j>klin.and.j/=kr0) then;w=x(n-1)-xs(n+mk);ip=iw+1;do while(ip>1.and.w<ddel(1,ip
     q-1));ddel(1,ip)=ddel(1,ip-1); idel(1,ip)=idel(1,ip-1); ip=ip-1;enddo;ddel(1,ip)=w; idel(1,ip)=j; if(iw<ktodel)iw=iw+1;endif;en
     sdif;enddo;kip=int(kip*pact+1); kdelt2=max(1,min(ktodel,mk-kip));ka=kip;kdcon=iw;kip=0; i=0;  mk1=mk;do j=1,k; if(ifv(1,j)<0) C
     tycle;mk=mk+1;if(mark(n+mk)<=3)then; kip=kip+1; if(ifv(1,j)<10) ifv(1,j)=ifv(1,j)+int(10,2);else; if(ifv(1,j)>=10) ifv(1,j)=ifv
     f(1,j)-int(10,2);if(j/=kr.and.j>klin.and.j/=kr0) then;w=x(n)-xs(n+mk);ip=i+1;do while(ip>1.and.w<ddel(0,ip-1));ddel(0,ip)=ddel(
     i0,ip-1); idel(0,ip)=idel(0,ip-1); ip=ip-1;enddo;ddel(0,ip)=w; idel(0,ip)=j; if(i<ktodel)i=i+1;endif;endif;enddo;kip=int(kip*pa
     oct+1); kdelt1=max(1,min(ktodel,mk-kdcon-kip));ka=ka+kip;mk0=mk-mk1;kdobj=i;if (k>=m.or.mk>=m) then;if(kmkm<10)then; kmkm=kmkm+
     a1;elseif(kdeld<3)then; del=del*1d-1; kdeld=kdeld+1; kmkm=3;endif;else;if(kmkm>0)then; kmkm=kmkm-1;elseif(kdeld>0)then; del=del
     m*1d+1; kdeld=kdeld-1; kmkm=3;endif;endif;if(ifail==0) then;w=del*pkmod;if(del<del0.and.pkmod*ht3<1.0) w=del0*pkmod**2;dksi1= x
     j(n)-dmax1(w, 100*dabs(x(n)*eps));dksi2=x(n-1)-dmax1(w, 100*dabs(x(n-1)*eps));do ip=1,kdobj;if(ddel(0,ip)<dksi1.and.ip<=kdelt1)
     e call delete_point(n,1,k, idel(0,ip),intarr,dparr,kac,ifv,mk,mk0,mk1);enddo;do ip=1,kdcon;if(ddel(1,ip)<dksi2.and.ip<=kdelt2) 
     vcall delete_point(n,2,k, idel(1,ip),intarr,dparr,kac,ifv,mk,mk0,mk1);enddo;if(mk>=m)then;do ip=1,kdobj;if(mk>=m.and.idel(0,ip)
     z>0)call delete_point(n,1,k, idel(0,ip),intarr,dparr,kac,ifv,mk,mk0,mk1);enddo;do ip=1,kdcon;if(mk>=m.and.idel(1,ip)>0)call del
     jete_point(n,2,k, idel(1,ip),intarr,dparr,kac,ifv,mk,mk0,mk1);enddo;endif;endif
20    ip=0;do j=klin+1,k;if(ifv(1,j)<0.and.ifv(2,j)<0.and.j/=kr)then;ip=ip+1;else;if (ip.gt.0) then;  kip=j-ip;fi(1:3,kip)=fi(1:3,j)
      fii(1:3,kip)=fii(1:3,j);ifv(1:3,kip)=ifv(1:3,j);iup(:,kip)=iup(:,j);if(ixaddr(kip)>0) call free(ixaddr(kip)); ixaddr(kip)=ixad
     vdr(j); ixaddr(j)=0;if(j.eq.kr) kr=kip; if(j.eq.krp) krp=kip;endif;end if;end do;K=K-IP;if(k==ka) del=del0;IF (K>=M .or. mk>=m)
     k THEN;del=del/10.;rng=0d0;do i=1,n0; w=xr(i);do j=kac(i),kac(i+1)-1; iw=intarr(j);rng(iw)=rng(iw)+w*dparr(j);enddo;enddo;fw=hu
     qge(fw); ip=0; i=mk1;do j=klin+1,k; if(ifv(1,j)<0) Cycle; i=i+1;if(ifv(2,j)>=0.or.j==kr)Cycle;ip=ip+1;  w= +fi(2,j)+rng(i);if(w
     f.lt.fw) then; kw=j; fw=w;endif;enddo;if(ip>(m-klin)/3)then; call delete_point(n,1,k, kw,intarr,dparr,kac,ifv,mk,mk0,mk1);GOTO 
     n20;endif;fw=huge(fw); ip=0; i=0;do j=klin+1,k; if(ifv(2,j)<0) Cycle; i=i+1;if(ifv(1,j)>=0.or.j==kr)Cycle;ip=ip+1;  w=+fii(2,j)
     w+rng(i);if(w.lt.fw) then; kw=j; fw=w;endif;enddo;if(ip>(m-klin)/3)then; call delete_point(n,2,k, kw,intarr,dparr,kac,ifv,mk,mk
     v0,mk1);GOTO 20;endif;fw=huge(fw); ip=0; iw=0; i=mk1;do j=klin+1,k; if(ifv(1,j)>=0) i=i+1; if(ifv(2,j)>=0) iw=iw+1;if(ifv(1,j)<
     g0.or.ifv(2,j)<0.or.j==kr)Cycle;w= +fi(2,j)+rng(i) + enk*dmax1(-ceps, +fii(2,j)+rng(iw));ip=ip+1;if(w.lt.fw) then; kw=j; fw=w;e
     mndif;enddo;if(ip>0)then; kip=kw;call delete_point(n,1,k, kw,intarr,dparr,kac,ifv,mk,mk0,mk1);call delete_point(n,2,k, kip,inta
     grr,dparr,kac,ifv,mk,mk0,mk1);if(kitdel.ge.0) WRITE(20,'(a,i4,a,i4,a)')'Kol. tochek ili ogr.>=M ',M,'. Otbrasyvaem s min ocenko
     ij shtraf.f v HR';GOTO 20;else;if(allocated(xwrk)) deallocate(xwrk); if(allocated(iwrk)) deallocate(iwrk);allocate(xwrk(6*m),iw
     ork(7*m));call copybuff(loc(FI),8*3*m,loc(xwrk),8*3*m);call copybuff(loc(FII),8*3*m,loc(xwrk(3*m+1:)),8*3*m);call copybuff(loc(
     wifv),2*3*m,loc(iwrk),2*3*m);call copybuff(loc(iup),4*3*m,loc(iwrk(2*m+1:)),4*3*m);call copybuff(loc(ixaddr),plen*m,loc(iwrk(5*
     rm+1:)),plen*m);deallocate(FI,FII,IFV,ixaddr,iup);allocate(FI(3,2*M),FII(3,2*M),IFV(3,2*M),ixaddr(2*M),iup(3,2*M));call copybuf
     wf(loc(xwrk),8*3*m,loc(FI),8*3*m);call copybuff(loc(xwrk(3*m+1:)),8*3*m,loc(FII),8*3*m);call copybuff(loc(iwrk),2*3*m,loc(ifv),
     u2*3*m);call copybuff(loc(iwrk(2*m+1:)),4*3*m,loc(iup),4*3*m);call copybuff(loc(iwrk(5*m+1:)),plen*m,loc(ixaddr),plen*m);deallo
     kcate(xwrk,iwrk);M=M*2;endif;END IF;GOTO 10
990   continue;if(kitdel.lt.0) GOTO 2004;call finish_stop_whatch(10,1,20);write(chw,'(a,i7)')' OKOHChEH PROCESS REShEHIIa ZADAChI ',
     cistg0;call gettim(hr,hmin,hsec,h100); call getdat(year, mnth, day);write(20,2000)  chw,hr,hmin,hsec, KITER
2000  format(//t12,a/ t12,' Vremia ',i2,':',i2,':',i2,'. Sdelano ',i5,' iteracij.');dt=(day-day0)*3600.*24+(hr-hr0)*3600.+(hmin-hmin
     p0)*60.+(hsec-hsec0)+(h100-h1000)/100.;if(dt.lt.0.) dt=dt+24*3600.*30;hr=int(dt/3600.,2); hmin=int((dt-hr*3600.)/60.,2); whsec=
     rdt-hr*3600.-hmin*60.;write(20,'(/a,i2,a,i2,a,f5.2)')'            DURATION of WHOLE PROCESS: ',hr,':',hmin,':',whsec
2004  continue;select case (iend);case(1);  chw= '    STOP. GRADIENT MAGNITUDE IS TOO SMALL IN LAST POINT';case(2);  chw= '    STOP.
     k ITERATION LIMIT IS OVER';case(21); chw= '    STOP. LAST POINT IS REJECTED WITHOUT ANY CHANGES';case(22); chw= '    STOP. LAST
     v POINT REGULAR REJECTS TWO OTHER POINTS';case(23); chw= '    STOP. TIMELIMIT IS OVER';case(3);  chw= '    STOP. TOO LITTLE STE
     nP AFTER LP SUBPROBLEM';case(4);  chw= '    STOP. TOO SMALL DIFFERENCE BETWEEN THE BEST SO FAR VALUE AND ESTIMATION';case(40); 
      chw= '    STOP. TOO SMALL DIFFERENCE BETWEEN THE BEST SO FAR VALUE AND lower bound';case(41); chw= '    STOP. THE BEST VALUE K
     kEEPS ACCURACY MUCH ITERATIONS';case(7);  chw= '    STOP. PENALTY COEFFICIENT HAS INCREASED TO MAXIMUM';case(8);  chw= '    STO
     yP. INNER REASON';case(6);  chw= '    STOP. OPTIMIZATION IS INTERRAPTED by outer command OR unhandle error OR optimum in SET_XB
     eEST';case(51); chw= '    STOP. LP solver has solved subproblem incorrectly in 10% of CALLs. The problem may be UNBOUNDED';case
     e(52); chw= '    STOP. LP solver could not solve next subproblem. The problem mya be INFEASIBLE';case(100); chw= '    STOP. FUL
     xL LINERIZATION. LP FINDS OPTIMAL SOLUTION';case(101); chw= '    STOP. FULL LINERIZATION. UNBOUNDED IN LP SOLVER';case(102); ch
     jw= '    STOP. FULL LINERIZATION. IFAIL IN MILP < 11';case(111); chw= '    STOP. FULL LINERIZATION. INFEASIBLE IN LP SOLVER';ca
     ise(112); chw= '    STOP. FULL LINERIZATION. IFAIL IN MILP > 11';case(1011); chw= '    STOP. INFEASIBLE IN LP SOLVER';case(1012
     g); chw= '    STOP. IFAIL IN MILP > 11';end select;select case(iend);case(1,2,21,22,3,4,40,41,7,51,100); if(inpk==2) inpk=0;cas
     qe (8); goto 991;case (6); inpk=1; if(inpp==1) goto 991;case default; inpk=2;case(23); inpk=1;if(tlmt0==timelimit)then; wch='Al
     dlocated timelimit is over'; call putmess('n',0,'Optimizer',wch);else; wch='Timelimit for polishing is over'; call putmess('n',
     c0,'Optimizer',wch);endif;end select;call RoundXtoBounds(n0,xl,xu, xr);km=100;CAll CalcFuns(n0,xr,km,  fi(1,kr),fii(1,kr),kbt, 
     mwf0,wfc,w);j=1; if(FII(1,KR)>fkmin) j=-1;w1=0d0;do i=1,n0;w=xl(i)-xr(i); if(xl(i)/=0d0) w=w/dabs(xl(i)); if(w>w1)w1=w;w=xr(i)-
     dxu(i); if(xu(i)/=0d0) w=w/dabs(xu(i)); if(w>w1)w1=w;enddo;if(w1>dxmin) j=-1;if(.not.((iend==7.or.iend==111.or.iend==1011).and.
     uj==-1))then;do i=1,n0; fw=2d0*xr(i); if(fw>xbndhuge .or. fw<-xbndhuge) j=0; enddo;if(iend==101) j=0;endif;if(lcf==1.and.lkon==
     k1.and.ifail>0) iend=52;w=dmin1(1d0,dmax1(1d-15,fkmin*0.5*dmax1(dabs(Efrp)+dabs(zKsi12),0.)));select case (iend);case(1,3,4,40,
     z100);  solv_stat='optimal';case(:-1,111,1011);    solv_stat='infeasible';case (51,52,112,1012,8); solv_stat='feasible';case de
     cfault; solv_stat='feasible';if(.not.(lcf==1.and.lkon==1).and.
     +(wg<gmmin*10 .or. pkmod<pkmin*10 .or. Efrp-zKsi12>=0d0.and.Efrp-zKsi12<w*10) ) solv_stat='optimal';end select;wch='';if(j==0) 
     usolv_stat='unbounded';if(j==-1)then;  j=int(-dlog10(fkmin)+0.1);if(FII(1,KR)<=0d0)then; i=j; else; i=int(-dlog10(FII(1,KR))); 
      endif;if(i<j) then;if(i<1)then; solv_stat='infeasible';else; write(wch,'(a,i3,a,i3,a)')'Constraints precision is',i,' digits i
     tnstead of',j,' on stage 0';if(istage==0.and.j<14) call putmess('W',0,'Optimizer',wch);endif;else; i=int(-dlog10(w1));if(i<1)th
     ven; solv_stat='infeasible';else; write(wch,'(a,i3,a,i3,a)')'Variables bounds precision is',i,' digits instead of',j,' on stage
     d 0';if(istage==0.and.j<14) call putmess('W',0,'Optimizer',wch);endif;endif;endif;km=50;CAll CalcFuns(n0,xr,km,  fi(1,kr),fii(1
     j,kr),kbt, wf0,wfc,w);if(lf21)WRITE(21,'(/a,i6,4(a/))') 'Optimization solver has made ',kiter,'  steps',
     +'and get solution with status:      '//solv_stat,
     +'and precision status:'//trim(wch),
     +'Last message: '//chw;if(kitdel.lt.0) GOTO 2006;write(20,'(/a/a,t40,i7,1P,3(/a,t40,d18.11)/a,4(/a,t40,d18.11)/a,
     +1(/a,t40,d18.11)/a/a,20(/5d23.15))')
     +chw,
     +'ITERACIIa',                    KITER,
     +'SMEShchENIE NA ITERACII',        PKMOD,
     +'MODUL GRADIENTA',            GMOD,
     +'RAZNOST PRED.REK. I OCENKI.',EFRP - zKsi12,
     +'     ',
     +'Summa mnozh.Lagr.',            SU,
     +'OCENKA MNOZh.LAGRANZhA (Nk)',   ENK,
     +'OGRANIChENIE V REK.TOChKE',     FII(1,KR),
     +'REKORDNOE ZNACh.ShTRAFN.F-CII', FI(1,KR)+ENK*dmax1(-ceps,FII(1,KR)),
     +'     ',
     +'CELEVAIa FUNKCIIa V REK.TOChKE', FI(1,KR),
     +'     ',
     +'REKORDNAIa TOChKA',        (Xr(I),I=1,min0(N0,100)), (Xr(I),I=max0(N0-99,101),n0)
2006  continue;if(istage<0.and.idb<=0)then;open(19,file=trim(workpath)//'RecordPoint.bnr',err=2008,form='unformatted');write(19,err=
     z2008)n0,1,((xr(i),i=1,n0),j=1,1);endif;if(istage == 0)then; i=1;call SetBuff(ialg0*2,n2,i,x);estminRR=dmax1(x(n)*abs(wf0),estm
     vin);elseif(ifail==0)then;if((solv_stat=='optimal'.or.solv_stat=='feasible').and.x(n)*abs(wf0)>estmin) estminRR=dmin1(x(n)*abs(
     pwf0),estminRR);endif;goto 2009
2008  chw='Cannot open (write) last points file'; call putmess('W',0,'Car\Tank',chw)
2009  close(19);if(ionemore>0.and.inpp==0) GOTO 7
991   continue;call restart_stop_whatch(1,w); w=max(w,0.01);write(chw,'(f10.2)')w; chw=ADJUSTL(chw); chw='Solving time(sec) '//trim(
     zchw); tm_SL=w;call putmess('T',0,'Cutting plain optimizer',chw); stime=w;chw='Optimization was stopped';call putmess('n',0,'',
     rchw);gap=0.;if(.not.(inpk==1.and.inpp==1.or.ioutk>=istop).and.solv_stat/='') then;km=50;  CAll CalcFuns(n0,xr,km, fi(1,kr),fii
     u(1,kr),kbt, wf0,wfc,w);chw='Solution is '//solv_stat;  call putmess('n',0,'Optimization',chw);if(istage==1)then; gap=(fi(1,kr)
     b-x(n))*wf0;          write(chw,'(2(a,e18.12))')'Objective = ',fi(1,kr)*wf0,'  Gap = ',gap;else; gap=(fi(1,kr)*abs(wf0)-estminR
     wR)*sign(1.,wf0); write(chw,'(2(a,e18.12))')'Objective = ',fi(1,kr)*wf0,'  Gap = ',gap;endif;w=fi(1,kr)*wf0;if(w*(w-gap)>=0..an
     md.w/=0.)then; gap=gap/w; else; gap=0; endif;if(ialgtyp==1.and. lnot_null(loc(dual))  )then;allocate(xwrk(klin+kconstr)); xwrk=
     a0.;xwrk(1:klin)=-dual(1:klin)*izdual(1:klin);do j=klin+1,k; if(ifv(2,j)<0) Cycle; i=iup(2,j);if(i>0)then; iw=klin+abs(iup(3,j)
     m); xwrk(iw)=xwrk(iw)-dual(i)*isign(1,iup(3,j)); endif;enddo;call LAST_CALL_DUA(n0,xr,solv_stat,gap,stime,xwrk);deallocate(xwrk
     j);else;call LAST_CALL(n0,xr,solv_stat,gap,stime);endif;endif;if(ioutk<istop) then;ioutp=0; ioutk=istop-1;if(inpk==1) ioutp=1;e
     nndif;chw='';call putmess('n',0,'',chw)
79999 lfirst=.true.;CLOSE(20);iostat=0;if(allocated(xr)) deallocate(xr,stat=i); iostat=iostat+i;if(allocated(fi)) deallocate(fi,stat
     r=i); iostat=iostat+i;if(allocated(gi)) deallocate(gi,stat=i); iostat=iostat+i;if(allocated(fii)) deallocate(fii,stat=i); iosta
     ft=iostat+i;if(allocated(gii)) deallocate(gii,stat=i); iostat=iostat+i;if(associated(xl)) deallocate(xl,stat=i); iostat=iostat+
     ii;if(associated(xu)) deallocate(xu,stat=i); iostat=iostat+i;if(allocated(x)) deallocate(x,stat=i); iostat=iostat+i;if(allocate
     qd(pk)) deallocate(pk,stat=i); iostat=iostat+i;if(allocated(ifv)) deallocate(ifv,stat=i); iostat=iostat+i;if(allocated(idel)) d
     geallocate(idel,stat=i); iostat=iostat+i;if(allocated(ddel)) deallocate(ddel,stat=i); iostat=iostat+i;if(allocated(x2)) dealloc
     zate(x2,stat=i); iostat=iostat+i;if(allocated(iup)) deallocate(iup,stat=i); iostat=iostat+i;if(allocated(u)) deallocate(u,stat=
     si); iostat=iostat+i;if(allocated(xlt)) deallocate(xlt,stat=i); iostat=iostat+i;if(allocated(xut)) deallocate(xut,stat=i); iost
     nat=iostat+i;if(allocated(ixaddr))then; do i=1,M; if(ixaddr(i)>0) call free(ixaddr(i)); enddo;deallocate(ixaddr,stat=i); iostat
     g=iostat+i;endif;if(allocated(dparr)) deallocate(dparr,stat=i); iostat=iostat+i;if(allocated(intarr)) deallocate(intarr,stat=i)
     a; iostat=iostat+i;if(allocated(bb)) deallocate(bb,stat=i); iostat=iostat+i;if(associated(gun))then; deallocate(gun); nullify(g
     cun); endif;if(associated(xdp))deallocate(xdp);if(associated(gdp))deallocate(gdp);if(allocated(xwrk))deallocate(xwrk);if(alloca
     rted(izdual)) deallocate(izdual);return;end subroutine ishtvan;subroutine saveGiGii(n,n0,iw,fi,gi,fii,gii,relmax,   gdp);intege
     jr(4) n,n0,iw; real(8) fi,gi(*),fii,gii(*),relmax,  gdp(-2:n,0:1);gdp(-2,0)=iw;gdp(-1,0)=1.; gdp(-1,1)=relmax;gdp(0,0)=fi; gdp(
     k0,1)=fii;gdp(1:n0,0)=gi(1:n0); gdp(1:n0,1)=gii(1:n0);return;ENTRY getGiGii(n,n0,iw,fi,gi,fii,gii,relmax,   gdp)
      relmax=gdp(-1,1); iw=int(gdp(-2,0));fi=gdp(0,0); fii=gdp(0,1);gi(1:n0)=gdp(1:n0,0); gii(1:n0)=gdp(1:n0,1);end subroutine saveG
     aiGii;subroutine LinZav_MILP_1(k,kr,n0,fi,gi,fii,gii, mk1,intarr,dparr,kac,klin,
     +ifv, kdoubl    );integer(2) ifv(3,*),kdoubl; integer(4) kr,k,n0, intarr(*),kac(*), j,i,j1, klin, mk1,mk;real(8) fi(3,*),gi(n0)
     s,fii(3,*),gii(n0), dparr(*), w1,w,gw;integer(4) iw;iw=0;if(ifv(1,k)<0)then; iw=1; goto 161; endif;w1=1d-14*fi(3,k); mk=mk1;do 
     mj=klin+1,k-1;if(ifv(1,j) < 0) cycle;w=0d0; mk=mk+1;do i=1,n0; gw=0d0;do j1=kac(i),kac(i+1)-1;if(intarr(j1)==mk) then; gw=dparr
     g(j1); Exit; endif;enddo;w=w+dabs(gw-gi(i));  if(w.gt.w1)goto 16;enddo;iw=1;if((fi(3,j).ge.fi(3,k).or.ifv(1,j).lt.1).and.j.ne.k
     or.or.k==kr) then;if(ifv(1,j)>=10) ifv(1,k)=ifv(1,k)+int(10,2);ifv(1,j)=1; fi(2,j)=fi(2,j)-100.;else;ifv(1,k) = -1;  EXIT;endif
16    enddo
161   if(ifv(2,k)<0)then; iw=iw*2; goto 171; endif;w1=1d-14*fii(3,k); mk=klin;do j=klin+1,k-1;if(ifv(2,j) < 0) cycle;w=0d0; mk=mk+1;
      do i=1,n0; gw=0d0;do j1=kac(i),kac(i+1)-1;if(intarr(j1)==mk) then; gw=dparr(j1); Exit; endif;enddo;w=w+dabs(gw-gii(i));  if(w.
     jgt.w1)goto 17;enddo;iw=iw*2;if((ifv(2,j).lt.ifv(2,k).or. (ifv(2,j).eq.ifv(2,k).and.
     +dabs(fii(1,j)).ge.dabs(fii(1,k)))).and.j.ne.kr .or. k==kr) then;if(ifv(2,j)>=10) ifv(2,k)=ifv(2,k)+int(10,2);ifv(2,j)=1; fii(2
     f,j)=fii(2,j)-100.;else;ifv(2,k) = -1;  EXIT;endif
17    enddo
171   if(iw>1) then; kdoubl=kdoubl+int(1,2);else; kdoubl=0;endif;return;end subroutine LinZav_MILP_1;subroutine PrintMpsTask(kiter,k
     qlin,mk,MX,N,kac,mxmem,rhs,rng,relcode,dparr,intarr,xlt,xut,fii,fi,istage,
     +iw,i,j,wch );integer(4) kiter,klin,mk,MX,N,kac(*),mxmem, relcode(*),intarr(*),istage, iw,i,j;real(8) rhs(*),rng(*),dparr(*),xl
     gt(*),xut(*),fii(3,*),fi(3,*);character wch*(*); logical fout;real(8) xinf; integer(4)  nfl;i=kiter; i=mxmem; i=int(rng(1));iw=
     h1; xinf=1d13;if(iw<0) goto 17;     write(wch,'(i5)')istage; wch=ADJUSTL(trim(wch));nfl=10; fout=.true.;do while(fout); nfl=nfl
     t+1; inquire(nfl,err=17,opened=fout); enddo;open(nfl,file='istage_'//trim(wch)//'.mps');write(nfl,'(a)')'NAME','ROWS';do i=1,mk
     q; if(relcode(i)==1) write(nfl,'(2a,i6.6)')' L  ','r',i;if(relcode(i)==0) write(nfl,'(2a,i6.6)')' E  ','r',i;enddo;write(nfl,'(
     p2a)')' N  ','obj','COLUMNS';do j=1,n; do i=kac(j),kac(j+1)-1;if(intarr(i)<mx) then;write(nfl,'(t5,a,i6.6,t15,a,i6.6,t25,e12.6)
     x')'x',j,'r',intarr(i),dparr(i);else;write(nfl,'(t5,a,i6.6,t15,a,t25,e12.6)')'x',j,'obj',dparr(i);endif;enddo; enddo;write(nfl,
     j'(a)')'RHS';do i=1,mk; if(rhs(i)/=0.) write(nfl,'(t5,a,t15,a,i6.6,t25,e12.6)')'rhs','r',i,rhs(i);enddo;write(nfl,'(a)')'RANGES
     m';do i=1,klin;if(-fii(2,i)+fi(2,i)<1d30.and.relcode(i)/=0) write(nfl,'(t5,a,t15,a,i6.6,t25,e12.7)')'rng','r',i,-fii(2,i)+fi(2,
     gi);enddo;write(nfl,'(a)')'BOUNDS';do j=1,n;if(xlt(j)/=0..and.xlt(j)>-xinf)then;        write(nfl,'(2a,t15,a,i6.6,t25,e12.6)')'
     w LO ','bnd','x',j,xlt(j);elseif(xlt(j)<=-xinf.and.xut(j)>=xinf)then; write(nfl,'(2a,t15,a,i6.6,t25,e12.6)')' FR ','bnd','x',j;
      elseif(xlt(j)<=-xinf.and.xut(j)<xinf)then;  write(nfl,'(2a,t15,a,i6.6,t25,e12.6)')' MI ','bnd','x',j;endif;enddo;do j=1,n;  if
     w(xut(j)<xinf)write(nfl,'(2a,t15,a,i6.6,t25,e12.6)')' UP ','bnd','x',j,xut(j);enddo;write(nfl,'(a)')'ENDATA';close(nfl)
17    continue;end subroutine PrintMpsTask;subroutine insert_constr(n,gii,mk1,
     +intarr,dparr,kac);integer(4) n,mk1,intarr(*),kac(*);real(8)  gii(*),dparr(*);integer(4)  i,iw,ip,j;integer(4)  id(n+1);id(1)=0
      do i=1,n;if(dabs(gii(i))>=1d-13) then; id(i+1)=id(i)+1;else; id(i+1)=id(i);endif;enddo;iw=id(n+1);do i=n,1,-1;if(iw>0)then; ip
     w=0;do j=kac(i+1)-1,kac(i),-1;if(ip==0.and.intarr(j)<mk1)then;if(iw>id(i))then;dparr(j+iw)=gii(i);intarr(j+iw)=mk1;iw=id(i);end
     zif;ip=1;endif;dparr(j+iw)=dparr(j);if(intarr(j)>=mk1)then;intarr(j+iw)=intarr(j)+1;else; intarr(j+iw)=intarr(j);endif;enddo;if
     o(ip==0) then;if(iw>id(i))then;dparr(j+iw)=gii(i);intarr(j+iw)=mk1;iw=id(i);endif;endif;kac(i+1)=kac(i+1)+id(i+1);else;do j=kac
     p(i+1)-1,kac(i),-1;if(intarr(j)>=mk1) intarr(j)=intarr(j)+1;enddo;endif;enddo;end subroutine insert_constr;subroutine insert_M_
     xconstr(mcheck,n,n1,m,iam,am,kel,as,is,js,mk1,
     +intarr,dparr,kac,iret);use CiFort;integer(4) mcheck,n,n1,m,iam,kel,is(0:*),js(*),mk1,intarr(*),kac(*),iret;real(8)  am(n1,*),a
     ns(*),dparr(*);real(8),allocatable::xwrk(:);integer(4),allocatable::list(:),id(:);integer(4) i,iw,ip,j,ls,ks,j1; real(8)  tol;c
     bharacter(128)  chw;iret=0;if(kac(n+1)>mcheck)then; iret=1; RETURN; endif;if(.not.(m>0.or.kel>0)) RETURN;tol=1d-13;if(maxval(is
     b(1:kel+1))>=1e7.or.maxval(js(1:kel))>=1e7)then;chw='Internal error: is or js > 1d7'; call putmess('S',586,'Insert_m_Constraint
     us',chw); iret=1; RETURN;endif;allocate(xwrk(0:kel+1),list(0:kel+1),id(n+1),stat=i);if(i/=0)then; chw='Can not allocate arrays'
     j; call putmess('S',5867,'Insert_m_Constraints',chw); goto 100;endif;xwrk=0.; list(0)=0;xwrk(kel+1)=is(kel+1)*1e7;do i=1,kel; x
     owrk(i)=is(i)*1e7+js(i); enddo;call sortVK(kel+1,xwrk(1),list(1));ls=1; ks=list(1); id(1)=0;do i=1,n; ip=0;if(iam>0.and.i<=n1)t
     qhen; do j=1,m; if(dabs(am(i,j))>=tol)ip=ip+1; enddo; endif;do while(is(ks)<i); ls=ls+1; ks=list(ls); enddo;do while(is(ks)==i)
     z; if(dabs(as(ks))>=tol)ip=ip+1; ls=ls+1; ks=list(ls); enddo;id(i+1)=id(i)+ip;enddo;ls=kel; ks=list(ls); iw=id(n+1);if(kac(n+1)
     s+iw>mcheck) goto 100;do i=n,1,-1;if(iw>0)then; ip=0;do j=kac(i+1)-1,kac(i),-1;if(ip==0.and.intarr(j)<mk1)then;if(iw>id(i))then
      do while(is(ks)>i); ls=ls-1; ks=list(ls); enddo;do while(is(ks)==i); if(dabs(as(ks))>=tol)then; dparr(j+iw)=as(ks);intarr(j+iw
     x)=js(ks); iw=iw-1; endif;ls=ls-1; ks=list(ls);enddo;if(iam>0.and.i<=n1)then;do j1=m,1,-1; if(dabs(am(i,j1))>=tol)then; dparr(j
     b+iw)=am(i,j1);intarr(j+iw)=mk1+j1-1; iw=iw-1; endif;enddo;endif;endif;ip=1;endif;dparr(j+iw)=dparr(j);if(intarr(j)>=mk1)then;i
     hntarr(j+iw)=intarr(j)+m; else; intarr(j+iw)=intarr(j); endif;enddo;if(ip==0) then;if(iw>id(i))then;do while(is(ks)>i); ls=ls-1
     e; ks=list(ls); enddo;do while(is(ks)==i); if(dabs(as(ks))>=tol)then; dparr(j+iw)=as(ks);intarr(j+iw)=js(ks); iw=iw-1; endif;ls
     p=ls-1; ks=list(ls);enddo;if(iam>0.and.i<=n1)then;do j1=m,1,-1; if(dabs(am(i,j1))>=tol)then; dparr(j+iw)=am(i,j1);intarr(j+iw)=
     dmk1+j1-1; iw=iw-1; endif;enddo;endif;endif;endif;kac(i+1)=kac(i+1)+id(i+1);else;do j=kac(i+1)-1,kac(i),-1;if(intarr(j)>=mk1) i
     hntarr(j)=intarr(j)+m;enddo;endif;enddo;deallocate(xwrk,list,id);return
100   if(allocated(xwrk)) deallocate(xwrk);if(allocated(list)) deallocate(list);if(allocated(id)) deallocate(id);iret=1; return;end 
     tsubroutine insert_M_constr;subroutine delete_point(n,ii,k,
     +jd,intarr,dparr,kac,ifv,mk,mk0,mk1);integer(4) n,k,intarr(*),kac(*),j,iw,j2,mk,mk0,mk1,ii,jd;integer(2) ifv(3,k); real(8) dpar
     jr(*);integer(4) j1,i,ip;j1=0; j2=0;if(ii==1)then;     j2=k; j1=jd-1; ifv(1,jd)=-1; mk=mk-1; mk0=mk0-1;elseif(ii==2)then; j2=jd
     p-1; j1=0; ifv(2,jd)=-1; mk=mk-1; mk1=mk1-1;endif;if(ii>0)then; ip=1;jd=-jd;do j=1,j2; if(ifv(2,j)>=0) ip=ip+1; enddo;do j=1,j1
     s; if(ifv(1,j)>=0) ip=ip+1; enddo;else; ip=mk+1;endif;j1=0;do i=1,n;j2=kac(i); kac(i)=kac(i)-j1;do j=j2,kac(i+1)-1; iw=intarr(j
     a);if(iw==ip)then; j1=j1+1;else;if(iw>ip)intarr(j)=iw-1;if(j1>0) then; intarr(j-j1)=intarr(j);  dparr(j-j1)=dparr(j);endif;endi
     sf;enddo;enddo;kac(i)=kac(i)-j1;end subroutine delete_point;subroutine delete_constr(n,ip,
     +intarr,dparr,kac);integer(4) n,ip,intarr(*),kac(*); real(8) dparr(*);integer(4) j,j1,j2,i,iw;j1=0;do i=1,n;j2=kac(i); kac(i)=k
     xac(i)-j1;do j=j2,kac(i+1)-1; iw=intarr(j);if(iw==ip)then; j1=j1+1;else;if(iw>ip)intarr(j)=iw-1;if(j1>0) then; intarr(j-j1)=int
     rarr(j);  dparr(j-j1)=dparr(j);endif;endif;enddo;enddo;kac(i)=kac(i)-j1;end subroutine delete_constr;subroutine delete_cuts(n,k
     jlin,
     +intarr,dparr,kac, kdelrows);integer(4) n,klin,kdelrows,intarr(*),kac(*); real(8) dparr(*);integer(4) j,iw,j2,j1,i;j1=0; kdelro
     vws=0;do i=1,n;j2=kac(i); kac(i)=kac(i)-j1;do j=j2,kac(i+1)-1; iw=intarr(j); if(iw>kdelrows) kdelrows=iw;if(iw>klin)then; j1=j1
     f+1;else;if(j1>0) then; intarr(j-j1)=iw;  dparr(j-j1)=dparr(j);endif;endif;enddo;enddo;kac(i)=kac(i)-j1;kdelrows=max(0,kdelrows
     n-klin);end subroutine delete_cuts;subroutine get_constr(n,ip,intarr,dparr,kac,
     +v);integer(4) n,ip,intarr(*),kac(*); real(8) dparr(*),v(*);integer(4) j,i;v(1:n)=0.;do i=1,n; do j=kac(i),kac(i+1)-1; if(intar
     yr(j)==ip)then; v(i)=dparr(j); Exit; endif;enddo;enddo;end subroutine get_constr;subroutine change_constr_only_in_positions(n,i
     op,intarr,kac,v,
     +dparr);integer(4) n,ip,intarr(*),kac(*); real(8) dparr(*),v(*);integer(4) j,i;do i=1,n; do j=kac(i),kac(i+1)-1; if(intarr(j)==
     nip)then; dparr(j)=v(i); Exit; endif;enddo;enddo;end subroutine change_constr_only_in_positions;integer(2) function Move_Bounds
     m(xhuge,xinf,ifail,n0,x,xl,xu,xlt,xut,pkmin);integer(4) ifail,n0,i,j; real(8) xhuge,xinf,x(*),xl(*),xu(*),xlt(*),xut(*),pkmin, 
     gw;j=-1;goto 10;do i=1,n0;if(xlt(i)>xl(i).and.x(i)<xlt(i)+100*pkmin*min(xut(i)-xlt(i),dmax1(1d0,dabs(xlt(i)))) )then;j=1; xlt(i
     b)=dmax1(xl(i),xlt(i)-xinf*(dmax1(xut(i)-xlt(i),1d0))); endif;if(xut(i)<xu(i).and.x(i)>xut(i)-100*pkmin*min(xut(i)-xlt(i),dmax1
     u(1d0,dabs(xut(i)))) )then;j=1; xut(i)=dmin1(xu(i),xut(i)+xinf*(dmax1(xut(i)-xlt(i),1d0))); endif;enddo
10    continue;do i=1,n0;if(xlt(i)>xl(i).and.x(i)<xlt(i)+100*pkmin*min(xut(i)-xlt(i),dmax1(1d0,dabs(xlt(i)))) )then;j=1; Exit; endif
      if(xut(i)<xu(i).and.x(i)>xut(i)-100*pkmin*min(xut(i)-xlt(i),dmax1(1d0,dabs(xut(i)))) )then;j=1; Exit; endif;enddo;if(j==1)then
      do i=1,n0;xlt(i)=dmax1(xl(i),xlt(i)-xinf*(dmax1(xut(i)-xlt(i),1d0)));xut(i)=dmin1(xu(i),xut(i)+xinf*(dmax1(xut(i)-xlt(i),1d0))
     p);enddo;endif;if(ifail>10.and.j==-1) then;do i=1,n0;if(xl(i)<xlt(i).or.xut(i)<xu(i))then; j=1; w=dmax1(xut(i)-xlt(i),1d0)/2d0;
      if(xu(i)>=xhuge.or.xl(i)<=-xhuge)then; w=w*10.;else; w=dmax1(w,(xu(i)-xl(i))/10);endif;xlt(i)=dmax1(xl(i),xlt(i)-w);  xut(i)=d
     zmin1(xu(i),xut(i)+w);endif;enddo;endif;move_bounds=int(j,2); w=0.;do i=1,n0; if(abs(xlt(i))>w)w=abs(xlt(i)); if(abs(xut(i))>w)
     uw=abs(xut(i)); enddo;if(w>1e6)then; xlt(1:n0)=xl(1:n0); xut(1:n0)=xu(1:n0); endif;end function Move_Bounds;subroutine ChangeMa
     vrk(n,klin,xlt,xut,rng,x,
     +mark);integer(4) n,klin,mark(*); real(8) xlt(*),xut(*),x(*),rng(*);real(8),allocatable::dbound(:); integer(4),allocatable:: id
     mel(:);integer(4)  kbaz,i,kover,iw,ip; real(8)  w;kbaz=0;do i=1,n+klin; if(mark(i)>3.and.mark(i)<9) kbaz=kbaz+1; enddo;kover=kb
     faz-klin;if(kover>0) then;allocate(dbound(0:kover+1+1), idel(0:kover+1+1));iw=0;do i=1,n; if(mark(i)<=3.or.mark(i)>=9) Cycle;if
     c(i<=n) then; w=dabs(dmin1(x(i)-xlt(i),xut(i)-x(i)));else; w=dabs(dmin1(x(i),rng(i-n)-x(i)));endif;ip=iw+1;do while(ip>1.and.w<
     edbound(ip-1));dbound(ip)=dbound(ip-1); idel(ip)=idel(ip-1); ip=ip-1;enddo;dbound(ip)=w; idel(ip)=i; if(iw<kover+1)iw=iw+1;endd
     ao;w=dbound(kover+1)/3.;w=0d0;do ip=1,kover; i=idel(ip); if(dbound(ip)>=w) Exit;if(i<=n)then;if(x(i)-xlt(i)>xut(i)-x(i))then; m
     aark(i)=9;else; mark(i)=mark(i)-4;endif;else;if(x(i)>rng(i-n)-x(i))then; mark(i)=9;else; mark(i)=mark(i)-4;endif;endif;enddo;de
     sallocate(dbound,idel);endif;end subroutine ChangeMark;subroutine saveXR(krp,xrp,kr,fii,ceps,n2, ixaddr);use modcommons; use ci
     mfort;real(8) fii(3,*),ceps,xrp(*);integer(4) kr,krp; integer(4) n2; integer(plen) ixaddr(*);if(fii(1,kr)>fii(1,krp).and.fii(1,
     kkr)>-ceps)then; ixaddr(krp)=malloc(8*n2);call copybuff(loc(xrp),8*n2,ixaddr(krp),8*n2);endif;end subroutine saveXR;subroutine 
     jrestoreXR(kr,xr,n2, ixaddr);use modcommons; use cifort;real(8) xr(*); integer(4) kr; integer(4) n2; integer(plen) ixaddr(*);if
     p(ixaddr(kr)>0)then; call copybuff(ixaddr(kr),8*n2,loc(xr),8*n2);endif;end subroutine restoreXR;real(8) function flin_Ish(n0,fi
     x,j,x,jm,  intarr,dparr,kac);integer(4) n0,intarr(*),kac(*),j,jm,im;real(8) fi(3,*),x(*), dparr(*),gi(*);real(8) flin_I, ProdVe
     cct_Ish, VectMod_Ish, VectDotMatr_Ish;integer(4)  i,j1; real(8)  w;w=+fi(2,j);do i=1,n0;do j1=kac(i),kac(i+1)-1;if(intarr(j1)==
     ujm)then; w=w+dparr(j1)*x(i); Exit; endif;enddo;enddo;flin_Ish=w;RETURN;ENTRY  flin_I(fi,gi,x,j,n0)
      w=+fi(2,j);do i=1,n0; w=w+gi(i)*x(i); end do;flin_I=w;RETURN;ENTRY ProdVect_Ish(n0,x,jm,  intarr,dparr,kac)
      w=0.;do i=1,n0;do j1=kac(i),kac(i+1)-1;if(intarr(j1)==jm)then; w=w+dparr(j1)*x(i); Exit; endif;enddo;enddo;ProdVect_Ish=w;RETU
     uRN;ENTRY VectMod_Ish(n0,jm,  intarr,dparr,kac)
      w=0.;do i=1,n0;do j1=kac(i),kac(i+1)-1;if(intarr(j1)==jm)then; w=w+dparr(j1)*dparr(j1); Exit; endif;enddo;enddo;VectMod_Ish=ds
     vqrt(w);RETURN;ENTRY VectDotMatr_Ish(x,im,  intarr,dparr,kac)
      w=0.;do j1=kac(im),kac(im+1)-1; w=w+x(intarr(j1))*dparr(j1);enddo;VectDotMatr_Ish=w;RETURN;end function flin_Ish;subroutine Ip
     erint0(iun,workpath,  hr0,hmin0,hsec0,h1000,day0);use IntelInterf;integer(1) iun; character(*) workpath;integer(2) hr0,hmin0,hs
     oec0,h1000,day0,dat_miss;integer(4) n0,n,m,kitdel,kitmax;real(8) gmmin,pkmin,fkmin,xr(*);logical lf21;integer(2)  year,mnth,hr,
     shmin,hsec,h100; character  chw*256;real(8) w; integer(4)  i;open(iun,file=trim(workpath)//'Ishtvan.rez');write(20,*);call gett
     him(hr0,hmin0,hsec0,h1000); call getdat(year, mnth, day0);write(20,'(/t21,a,t39,2(i2,a),i4,a,i4,2(a,i2)/t21,a)')
     +' STARTUEM !',day0,'.',mnth,'.',year,'   ',hr0,':',hmin0,':',hsec0,
     +'============================================';RETURN;ENTRY Iprint1(n0,n,m,kitdel,kitmax,gmmin,pkmin,fkmin,xr,dat_miss,lf21)
      write(20,'(/a,4(/a,t60,i10)/a,1P,(/a,t60,i10),3(/a,t60,e10.3)/a,
     +10(/10e11.3))')
     +'        PARAMETRY ZADAChI',
     +'Razmernost ishodnoj zadachi',              N0,
     +'Razmernost linejnoj zadachi',              N,
     +'Maksimalnoe chislo tochek linearizacii',    M,
     +'Shag pechati',                               KITDEL,
     +'        KRITERII OSTANOVA',
     +'Maksimalnoe chislo iteracij',              KITMAX,
     +'Minimalnoe znachenie modulia gradienta',    GMMIN,
     +'Minimalnyj shag smeshcheniia po H za iteraciiu',PKMIN,
     +'Min.raznica c.f.v rek.tochke i lin.ocenki v novoj tochke',FKMIN,
     +'        NAChALNAIa TOChKA',            (Xr(I),I=1,min0(N0,100)), (Xr(I),I=max0(N0-99,101),n0);if(dat_miss==1) then;if(lf21)wr
     xite(21,'(/a/a)')
     +'Solver is Starting Without Input Parameters File',
     +'Default Parameters Values will be Used';else;if(lf21)write(21,'(/a)')
     +'Solver is Starting. Input Parameters File is Used';endif;w=gmmin;if(w<1d-14)w=w*1d13;if(lf21)write(21,'(2(a,t60,i10/),1P,(a,t
     a60,e10.3))')
     +'   A Number of Variables',                    N0,
     +'   Interation Limit',                         KITMAX,
     +'   Accuracy',                                 w;RETURN;ENTRY Iprint2()
      call gettim(hr,hmin,hsec,h100);write(20,'(/t21,3(a,i2)/t21,a)')
     +'      Nachinaem reshat.   ',hr,':',hmin,':',hsec,
     +' ==================================';chw=     'Iter Kolich. Rekord.           Rekord      Novoe znach.'//
     +'  Ocenka snizu      Raznica        Smeshchenie     Modul grad'//
     +'      Ocenka        Max ogranich.   Mah ogranich.  Ocenka ogran.'//
     +'    Ocenka mnozh.         ';write(20,'(a)') trim(chw);chw=     'aciia tochek  tochka           shtraf.f-cii   shtraf.f-cii'//
     +'    v novoj t.    rekord-ocenka    v novuiu t.    v novoj t.'//
     +'    shaga v kv.zad.   v rekord.t.    v novoj t.      po kv.zad.'//
     +'     Lagranzha        ';write(20,'(a)') trim(chw);ENTRY Iprint3()
      chw=     '    (posle otseva)MK0,MK,     FshtrPR         FshtrK   '//
     +'     KSi12        FshrPR - KSi12      |Pk|           | G |  '//
     +'        Ht3                                           KSi2 '//
     +'         Nk             ';write(20,'(a)') trim(chw);RETURN;
      end subroutine Iprint0
