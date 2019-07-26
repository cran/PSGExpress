      SUBROUTINE QL0002_J(N,M,MEQ, LQL,a,B,GRAD,G,XL,XU,X,NACT,IACT,MAXIT,VSMALL,INFO,DIAG,W,LW, iqpro,chrc);use CiFort;interface;su
     mbroutine ColPermute(direction,lmake,m,n,iDP,nu,lez,llz,   iCP,pr,pz,lzd,kunit,iact,plm);integer(4) m,n,iDP,nu,iCP,kunit,iact(*
     j),lez(2,0:*); real(8),target:: pr(*),pz(*); real(8) plm(*);character(*) direction; logical lmake, lzd,llz;end;subroutine Rebui
     rldRZ(iqpro,m,n,a,gd,lzd,llz,lmake,nact0,  iact,plm,pac,  kunit,pr,pz,lez);integer(4) iqpro,m,n,nact0,iact(*),lez(2,0:*),kunit;
          real(8) a(n,*),gd(*),plm(nact0),pz(*),pac(*);logical lzd,llz,lmake;                    real(8),target::pr(*);end;end inter
     tface;character(*) chrc;integer n,m,m1,m2,meq,lw,info,iqpro,nact,iact(*),maxit;real(8),target:: w(*), a(n,*);real(8) b(*),g(n,*
     q),gd(*),grad(n),x(n),xl(*),xu(*),vsmall,diag;logical lql,ldn;integer(4) lez(2,0:n+1);INTEGER,save:: IWZ,IWR,IWW,IWD,IWX,IWA,IF
     zINC,KFINC,     kunit;integer      JFINC,IFLAG,IWS,K1,IW,KK,JFLAG,KDROP,NU,KNEXT,  MN,K,I,IA,IR,IRA,IRB,J,NM,IZ,IZA,ITERC,ITREF
     e,kunit0,igoto700,     nact0;real(8),save:: ZERO,ONE,TWO,ONHA,VFACT,xmag,XMAGR,DIAGR;real(8) CVMAX,FDIFF,FDIFFA,GA,GB,PARINC,PA
     aRNEW,RATIO,RES,STEP,SUM,SUMX,SUMY,SUMA,SUMB,SUMC,TEMP,TEMPA;LOGICAL,save:: LOWER,  lwrite29, lmake,lzd,llz,lswr;integer(4) k23
     n0, k250, k450,k_conv_test, k340, k530,k590;real(8) ww,wtime,wpd,wd,step0,wlb,wdx,cvmax0,ws;real(8),pointer:: plm0(:),xopt0(:),
     sxbf(:);real(8),pointer:: pwx(:),pws(:),pt(:),anext(:),rnew(:);real(8),pointer:: pax(:),pda(:),plm(:),pac(:),pz(:),pr(:),pww(:)
       character(100) chw,ch2*2,chT*1;integer(4),external:: iofj;ENTRY QL0002_Init(chrc,n,  lw)
      nullify(plm0,xopt0);lwrite29= .false.;lswr=     .false.;lwrite29= .false.;lswr=     .false.;lmake=.true.;IWZ=n;IWR=IWZ+n*n;IWW
     h=IWR+(n*(n+3))/2;IWD=IWW+n;IWX=IWD+n;IWA=IWX+n;ZERO=0.D+0;ONE=1.D+0;TWO=2.D+0;ONHA=1.5D+0;VFACT=1.D+0;xmag=zero;XMAGR=1.0D-2;I
     rFINC=3;KFINC=MAX0(10,N);lw=iwa;if(chrc=='init') return;goto 10;ENTRY QL0002_amod(chrc,n,m,meq,lw,a,b,     w,info)
      nullify(plm0,xopt0);do k=1,m;SUM=ZERO;do i=1,n;sum=sum+a(i,k)**2;enddo;IF (SUM.GT.ZERO) GOTO 20;IF (B(K).EQ.ZERO) GOTO 30;INFO
     c=-K;IF (K.LE.MEQ) GOTO 79999;if(b(k)<=0.)then; goto 30; else; goto 79999; endif
   20 SUM=ONE/DSQRT(SUM)
   30 IA=IWA+K;w(ia)=sum;enddo;do k=1,n;IA=IWA+M+K;w(ia)=one;enddo;info=0;if(chrc=='amod') return
10    continue;ENTRY QL0002_Zero(chrc,iqpro,n,m,meq,m1,m2,nact,iact,lql, a,b,g,gd,grad, x,xl,xu, vsmall,diag,w,  LW, lez)
      nullify(plm0,xopt0);pda=>w(iwd+1:iwd+n);pr =>w(iwr+1:iwr+(n*(n+3))/2);pz =>w(iwz+1:iwz+n*n);plm=>w(1:n);pac=>w(iwa+1:iwa+m+n);
      pax=>w(iwa+m+1:iwa+m+n);pww=>w(iww+1:iww+n);if(lwrite29.or.lswr) call restart_stop_whatch(8,wtime);if(iqpro>0) lmake=.false.;D
     pIAGR=TWO;DIAG=ZERO;if (lql) goto 165;if(iqpro<=0)then; do i=1,n; pda(i)=gd(i);enddo;else;             do i=1,n; pda(i)=g(i,i);
      enddo;endif;if(iqpro<0) goto 90;do i=1,n; wpd=pda(i);diag=dmax1(diag,vsmall-pda(i));if(iqpro>0) then;do j=i+1,n; wd=g(j,j); ww
     a=g(i,j);ga=-dmin1(wpd,wd);gb=dabs(wpd-wd)+dabs(ww);if(gb>0.) ga=ga+ww*ww/gb;diag=dmax1(diag,ga);enddo;else;do j=i+1,n; wd=gd(j
     m);ga=-dmin1(wpd,wd);diag=dmax1(diag,ga);enddo;endif;enddo;IF (DIAG.LE.ZERO) GOTO 90
   70 DIAG=DIAGR*DIAG;if(iqpro<=0)then; do i=1,n; gd(i)= diag+pda(i);enddo;else;             do i=1,n; g(i,i)=diag+pda(i);enddo;endi
     cf
   90 continue;if(iqpro<=0)then;pr=0d0; ir=0;do j=1,n; ir=ir+j; temp=gd(j);if (temp<vsmall) goto 140;pr(ir)=dsqrt(temp);enddo;else;i
     mr=0;do j=1,n;ira=0;IRB=IR+1;do i=1,j;TEMP=G(I,J);if(i.ne.1) then;do k=irb,ir;IRA=IRA+1;temp=temp-pr(k)*pr(ira);enddo;endif;ir=
     kir+1;IRA=IRA+1;if(i<j) pr(ir)=temp/pr(ira);enddo;IF (TEMP.LT.VSMALL) GOTO 140;pr(ir)=dsqrt(temp);enddo;endif;if(iqpro==2)then;
        ir=0;do j=1,n; do i=1,j; ir=ir+1; g(i,j)=pr(ir);enddo;    enddo;lql=.true.;endif;GOTO 170
  140 W(J)=ONE;SUMX=ONE;K=J
  150 SUM=ZERO;IRA=IR-1;do i=k,j;sum=sum-pr(ira)*w(i);ira=ira+i;enddo;IR=IR-K;K=K-1;w(k)=sum/pr(ir);SUMX=SUMX+W(K)**2;IF (K.GE.2) GO
     nTO 150;DIAG=DIAG+VSMALL-TEMP/SUMX;GOTO 70
  165 ir=0;if(iqpro<=0)then; pr=0d0;do i=1,n; ir=ir+i; pr(ir)=gd(i);enddo;else;do i=1,n; do j=1,i;ir=ir+1;pr(ir)=g(j,i);enddo; enddo
       endif
  170 continue;if(n<-10) then;write(29,'(/a)')'R0_transp(i,j)';k=0;do i=1,n; write(29,'(99e12.3)')(pr(j),j=k+1,k+i); k=k+i;enddo;end
     zif;lzd=.false.;llz=.true.;pz=0d0;do i=0,n; lez(1,i)=i+1; lez(2,i+1)=i; enddo;if(iqpro<=0) then;ir=0; iz=1;do i=1,n; ir=ir+i;pz
     t(iz)=1./pr(ir);if(.not.lzd)then; iz=iz+n+1;else;pt=>pz(iz+1:); call copybuff(loc(i),4,loc(pt),4);iz=iz+n;endif;enddo;else;NM=N
     p-1;do i=1,n;iz=i+(i-1)*n;ir=(i+i*i)/2;pz(iz)=1./pr(ir);if (i==n) cycle;IZA=IZ;do j=i,nm;IR=IR+I;SUM=ZERO;do k=iza,iz,n;sum=sum
     x+pz(k)*pr(ir);IR=IR+1;enddo;IZ=IZ+N;pz(iz)=-sum/pr(ir);enddo;enddo;endif;if(n<-10)then; write(29,'(/a)')'Z_na vhode';do i=1,n;
       write(29,'(99e12.3)')(pz(i+j*n),j=0,n-1); enddo;endif;plm=0.;mn=m+n;NACT=0; nact0=0;kunit=0;if(.false.)then;do i=1,meq; iact(
     oi)=i; enddo; nu=meq;if(m1>0)then; nu=nu+1; iact(nu)=m1; endif;if(m2>0)then; nu=nu+1; iact(nu)=m2; endif;do nu=1,nu; knext=iact
     n(nu); anext=>a(1:n,knext);iws=(nact+nact*nact)/2; rnew=>pr(iws+1:iws+n);call V_Multiply_Z('0',knext,m,n,anext,pz,lzd,kunit,lez
     t, rnew);i=N;call Make_Last_Rnew_Zero(n,nact,kunit,lez,  i,rnew,pz,lzd);if(abs(rnew(nact+1))<vsmall) Cycle;nact=nact+1; iact(na
     fct)=knext; ia=knext; pac(ia)=-pac(ia);enddo;endif;m1=0;if(m1>0)then; anext=>a(1:n,m1); else; anext=>pww; anext=0d0; endif
175   nact0=nact;if(nact>0)then; call SolveKKT(n,m,mn,b,xl,xu,grad,lzd,lez,nact,iact,kunit,pr,pz,      x,plm);if(lwrite29) write(29,
     h'(a,2i6,i10,20i5)')'ZERO: SolvKKT: kuint,nact,iact',kunit,nact,(iact(i),i=1,min(20,nact));endif;if(.true.)then;do i=1,n; if (p
     sax(i) <= 0d0) Cycle;if(x(i)<=xl(i)+vsmall*dmax1(1.,dabs(xl(i))).and.anext(i)>=0d0)then; knext=i+m;elseif(x(i)>=xu(i)-vsmall*dm
     cax1(1.,dabs(xu(i))).and.anext(i)<=0d0)then; knext=i+mn;else; cycle;endif;iws=(nact+nact*nact)/2; rnew=>pr(iws+1:);call V_Multi
     hply_Z('1',knext,m,n,anext,pz,lzd,kunit,lez, rnew);nu=n;call Make_Last_Rnew_Zero(n,nact,kunit,lez,  nu,rnew,pz,lzd);nact=nact+1
        iact(nact)=knext; ia=knext; if(knext>mn)ia=ia-n; pac(ia)=-pac(ia);nu=nact;if(iqpro<0.and.knext>m) call ColPermute('per',lmak
     me,m,n,1,nact,lez,llz,   nu,pr,pz,lzd,kunit,iact,plm);enddo;endif;if(nact>nact0) goto 175;if(m1>0)then; knext=m1;iws=(nact+nact
     j*nact)/2; rnew=>pr(iws+1:iws+n);call V_Multiply_Z('0',knext,m,n,anext,pz,lzd,kunit,lez, rnew);nu=n;call Make_Last_Rnew_Zero(n,
     tnact,kunit,lez,  nu,rnew,pz,lzd);if(abs(rnew(nact+1))>vsmall)then;nact=nact+1; iact(nact)=knext; ia=knext; pac(ia)=-pac(ia);en
     pdif;else;endif;if(lwrite29.or.lswr)then; call Check_stop_whatch(8,wtime); write(29,'(a,f10.2,i9)')'ZERO sec: ',wtime;endif;if(
     lchrc=='zero') RETURN;Entry QL0002_Iter(chrc,iqpro,n,m,meq,ldn,lql,a,b,grad,g,gd, xl,xu,x,nact,iact,maxit,vsmall,info,w,lw, lez
     e)
      cvmax0=0.; res=0; allocate(plm0(1),xopt0(1),xbf(1));pda=>w(iwd+1:iwd+n);plm=>w(1:n);pac=>w(iwa+1:iwa+m+n); pax=>w(iwa+m+1:iwa+
     vm+n);pwx=>w(iwx+1:iwx+n);pz =>w(iwz+1:iwz+n*n);pww=>w(iww+1:iww+n);pr =>w(iwr+1:iwr+(n*(n+3))/2);if(lwrite29.or.lswr) call res
     ltart_stop_whatch(8,wtime);mn=m+n; ITERC=1; ITREF=0; JFINC=-KFINC;k230=0; k250=0; k450=0; k_conv_test=0; k340=0; k530=0; k590=0
       step0=1.;  goto 250
  230 IFLAG=1; XMAG=ZERO; VFACT=1.D+0;  k230=k230+1;      if(lwrite29) write(29,'(a,i5)')'230: k230 ',k230
  250 IFLAG=2; INFO=0;                   k250=k250+1;if(lwrite29) write(29,'(//a,3i6,i10,20i5)')'250: k250,nact,kuint,iact',k250,nac
     tt,kunit,(iact(i),i=1,min(20,nact));if(.not.ldn)then; ldn=.true.; goto 450; endif;if(lwrite29)then; write(29,'(a,2i7)')'340: Be
     tfor Solve KKT plm<0,x<0 ',count(plm<0d0),count(x<0d0);deallocate(plm0,xbf); allocate(plm0(n),xbf(n));plm0=plm; xbf=x;endif;cal
     el SolveKKT(n,m,mn,b,xl,xu,grad,lzd,lez,nact,iact,kunit,pr,pz,      x,plm);k340=k340+1;if(lwrite29)then; write(29,'(a,1p,10e10.
     a2)')'340: Solved KKT X = ',(x(i),i=1,min(10,n));write(29,'(a,1p,10e10.2)')'380: Solved KKT LM= ',(plm(i),i=1,min(10,n));ww=max
     jval(abs(plm(1:nact)-plm0(1:nact))); temp=maxval(abs(x-xbf));if(ww>vsmall)then; write(30,'(/a,2i7)')'m,iterc',m,iterc; chT=char
     l(9);do i=1,nact;if(abs(plm(i)-plm0(i))>vsmall)then; ch2='A '; j=iact(i);if(j>m)then; ch2='XL'; j=j-m;endif;if(j>n)then; ch2='X
     eU'; j=j-n; endif;write(30,'(a,i5,1p,3(a,e10.3))')ch2,j,chT,plm(i),chT,plm0(i),chT,plm(i)-plm0(i);endif;enddo;endif;write(29,*)
     w'plm-plm0,x-xbf',ww,temp;endif;jflag=1; goto 910
420   continue;        if(lwrite29) write(29,'(a,e15.3,2i5)')'420: XMAG,IFLAG,ITREF',XMAG,IFLAG,ITREF;if(itref==1) goto 435;k=nact;d
     zo i=nact,1,-1; kdrop=i; ia=iact(kdrop); if(ia<=meq) Cycle;if(plm(kdrop)<0d0)then; NU=NACT; if(ia>mn)ia=ia-n; pac(ia)=-pac(ia);
      call ColPermute('del',lmake,m,n,nact,nu,lez,llz,   kdrop,pr,pz,lzd,kunit,iact,plm);nact=nact-1;endif;enddo;if(k>nact)then; if(
     clwrite29) write(29,'(a,i5,e15.3)')'420: Gamuzom udalili ',k-nact; goto 250;endif;goto 445
435   kdrop=nact+1;do while(kdrop>1);kdrop=kdrop-1;ia=iact(kdrop);if(ia<=meq) Cycle;if(plm(kdrop)<0d0)then;NU=NACT;if(lwrite29) writ
     ce(29,'(a,i5,e15.3)')'420: Udaliaem po odnomu KDROP,Lambd(kdrop)',KDROP,w(kdrop);if(ia>mn) ia=ia-n;pac(ia)=-pac(ia);call ColPer
     rmute('del',lmake,m,n,nact,nu,lez,llz,   kdrop,pr,pz,lzd,kunit,iact,plm);nact=nact-1;goto 250;endif;enddo
445   continue
  450 CVMAX=ZERO;                k450=k450+1;do k=1,meq; if (pac(k).le.zero) cycle;sum=-b(k)+dot_product(x,a(:,k));sumx=dabs(-sum*pa
     gc(k));if (sumx > cvmax) then;temp=dabs(b(k));do i=1,n; temp=temp+dabs(x(i)*a(i,k));enddo;tempa=temp+dabs(sum);     if (tempa.l
     se.temp) cycle;temp=temp+onha*dabs(sum); if (temp.le.tempa) cycle;cvmax=sumx;res=sum;knext=k;endif;enddo;if(cvmax>100*vsmall) g
     zoto 480;do k=1,n;LOWER=.TRUE.;if (pax(k) <= 0d0) Cycle;SUM=XL(K)-X(K);if(sum < 0d0) then;sum=x(k)-xu(k);LOWER=.FALSE.;endif;if
     v(sum > cvmax) then;CVMAX=SUM;RES=-SUM;if(lower)then; knext=k+m;else;          knext=k+mn;endif;endif;enddo;if(cvmax>100*vsmall
     i) goto 480;do k=MEQ+1,m;if (pac(k).le.zero) Cycle;SUM=-B(K);do i=1,n;sum=sum+x(i)*a(i,k);enddo;sumx=-sum*pac(k);IF (K.LE.MEQ) 
     ySUMX=DABS(SUMX);if (sumx > cvmax) then;TEMP=DABS(B(K));do i=1,n;temp=temp+dabs(x(i)*a(i,k));enddo;TEMPA=TEMP+DABS(SUM);if (tem
     wpa.le.temp) Cycle;TEMP=TEMP+ONHA*DABS(SUM);if (temp.le.tempa) Cycle;CVMAX=SUMX;RES=SUM;KNEXT=K;endif;enddo
  480 CONTINUE;if(.false.)then;wlb=0.;do i=1,nact; k=iact(i);if(k<=m)then;      wlb=wlb+plm(i)*b(k);elseif(k<=mn)then; wlb=wlb+plm(i
     k)*xl(k-m);else;              wlb=wlb+plm(i)*(-xu(k-mn));endif;enddo;wdx=dot_product(grad,x); ww=0.5*dot_product(x(1:n-2),x(1:n
     g-2));ww=ww+0.5*dot_product(x(n-1:n),x(n-1:n))*gd(n)*gd(n);if(lwrite29) write(29,'(a,1p,3e10.2)')'wdx,wlb,wdx-wlb',wdx+ww,wlb-w
     rw,wdx-wlb+2.*ww;endif;INFO=0;                k_conv_test=k_conv_test+1;if(lwrite29)then; write(29,'(a,i5,a,2i6,2e15.3,2i6)')'4
     o80: Test for convergence ',k_conv_test,'. M,KNEXT,ResNorm,Vsmall,itref,jfinc',m,knext,CVMAX,VSMALL,itref,jfinc;do i=1,nact; if
     o(plm(i)<0d0)then; write(29,*)'480: i,plm(i)<0',i,plm(i); Exit; endif; enddo;endif;IF (CVMAX.LE.VSMALL) GOTO 700;if(itref==1)th
     jen;if(cvmax<=100*vsmall) goto 700;if(cvmax0<=vsmall)then;if(cvmax>=dmin1(1d4*vsmall,1d-7))then;x=xopt0; info=3; goto 700;else;
       cvmax0=cvmax;  xopt0=x;endif;else;if(cvmax>=cvmax0)then; x=xopt0; info=4; goto 700;else; cvmax0=cvmax;  xopt0=x;endif;endif;e
     bndif;JFINC=JFINC+1;IF (JFINC.EQ.0) GOTO 510;IF (JFINC.NE.IFINC) GOTO 530;FDIFF=ZERO;FDIFFA=ZERO;if(iqpro<0) then;do i=1,n-2;su
     am=two*grad(i); sumx=dabs(sum);temp=pwx(i)+x(i); sum=sum+temp; sumx=sumx+dabs(temp);ww=x(i)-pwx(i);fdiff=fdiff+sum*ww; fdiffa=f
     sdiffa+sumx*dabs(ww);enddo;do i=n-1,n;sum=two*grad(i); sumx=dabs(sum);temp=gd(i)*(pwx(i)+x(i)); if(lql) temp=temp*gd(i);sum=sum
     t+temp; sumx=sumx+dabs(temp);ww=x(i)-pwx(i);fdiff=fdiff+sum*ww; fdiffa=fdiffa+sumx*dabs(ww);enddo;elseif(iqpro==0) then;do i=1,
     wn;SUM=TWO*GRAD(I);SUMX=DABS(SUM);temp=gd(i)*(pwx(i)+x(i));if(lql) temp=temp*gd(i);sum=sum+temp;sumx=sumx+dabs(temp);ww=x(i)-pw
     xx(i);fdiff=fdiff+sum*ww;fdiffa=fdiffa+sumx*dabs(ww);enddo;else;do i=1,n;SUM=TWO*GRAD(I);SUMX=DABS(SUM);if (lql) then;pda(i)=0.
       do j=i,n;pda(i)=pda(i)+g(i,j)*(pwx(j)+x(j));enddo;do j=1,i;temp=g(j,i)*pda(j);SUM=SUM+TEMP;sumx=sumx+dabs(temp);enddo;else;do
     k j=1,n;temp=g(i,j)*(pwx(j)+x(j));SUM=SUM+TEMP;sumx=sumx+dabs(temp);enddo;endif;ww=x(i)-pwx(i);fdiff=fdiff+sum*ww;fdiffa=fdiffa
     d+sumx*dabs(ww);enddo;endif;INFO=2;         igoto700=0;SUM=FDIFFA+FDIFF;if(sum<=fdiffa) igoto700=1;TEMP=FDIFFA+ONHA*FDIFF;if(te
     nmp<=sum)   igoto700=1;if(igoto700==1)then;goto 700;endif;JFINC=0;INFO=0
  510 continue;pwx=x
  530 iterc=iterc+1; k530=k530+1;if(iterc>maxit)then; info=1; goto 710;endif;goto 535;nu=0;do k=1,n; if (pax(k) <= 0d0) Cycle; if(na
     tct+nu==n) Exit;sum=xl(k)-x(k); lower=.true.;if(sum<0.)then; sum=x(k)-xu(k); LOWER=.FALSE.; endif;if(sum>vsmall)then; if(lower)
     lthen; knext=k+m; else; knext=k+mn;endif;nu=nu+1; iact(nact+nu)=knext;endif;enddo;if(nu<=(nact-kunit)*2) goto 535;do j=nact+1,n
     aact+nu; knext=iact(j);if(lswr)then; call Check_stop_whatch(8,wtime); chw='a(1,k),k';i=knext;if(i>m)then; chw=' XLi G,i'; i=i-m
        endif;if(i>n)then; chw=' XUi G,i'; i=i-n; endif;write(29,'(a,3i7,f10.2/a)')'680: ADDED '//trim(chw)//',Nact,Kunit,Wtime',i,j
     z,kunit+j-nact,wtime,' ';endif;enddo;nact=nact+nu;call RebuildRZ(iqpro,m,n,a,gd,lzd,llz,lmake,nact,   iact,plm,pac,   kunit,pr,
     hpz,lez);goto 533;nu=0;do k=1,n; if (pax(k) <= 0d0) Cycle; if(nact+nu==n) Exit;sum=xl(k)-x(k); lower=.true.;if(sum<0.)then; sum
     j=x(k)-xu(k); LOWER=.FALSE.; endif;if(sum>vsmall)then; if(lower)then; knext=k+m; else; knext=k+mn;endif;nu=nu+1; iact(nact+nu)=
     wknext;endif;enddo;if(nu<=1) goto 535;nact0=nact;do nu=1,nu; knext=iact(nact0+nu);IWS=IWR+(NACT+NACT*NACT)/2; rnew=>w(iws+1:iws
     v+n);if (knext<=m) then;anext=>a(1:n,knext);call V_Multiply_Z('0',knext,m,n,anext,pz,lzd,kunit,lez,  rnew);else;kunit0=kunit; i
     zf(lzd) kunit=n;anext=>pww; anext=0.; K1=KNEXT-M;if (k1 <= n) then; anext(k1)=1.;do j=1,kunit; if(iofj(n,pz,j)==k1)then; rnew(j
     c)=pz(n*(j-1)+1); else; rnew(j)=0.; endif;enddo;iz=k1+n*kunit;do j=kunit+1,n; rnew(j)=pz(iz); iz=iz+n;enddo;else; k1=knext-mn; 
      anext(k1)=-1.;do j=1,kunit; if(iofj(n,pz,j)==k1)then; rnew(j)=-pz(n*(j-1)+1); else; rnew(j)=0.; endif;enddo;iz=k1+n*kunit;do j
     u=kunit+1,n; rnew(j)=-pz(iz); iz=iz+n;enddo;endif;kunit=kunit0;endif;i=N;call Make_Last_Rnew_Zero(n,nact,  i,rnew,pz,lzd,kunit)
       if(abs(rnew(nact+1))<vsmall) goto 533;nact=nact+1; iact(nact)=knext; ia=knext;if (knext>mn) ia=ia-n;pac(ia)=-pac(ia);i=nact;i
     sf(knext>m.and.iqpro<0) call ColPermute('per',lmake,m,n,1,nact,lez,llz,   i,pr,pz,lzd,kunit,iact,plm);enddo;nu=nu-1
533   continue;if(lwrite29) write(29,'(a,i5,e15.3)')'Gamuzom vstavili ',nu-1;goto 250
535   continue;iws=(nact+nact*nact)/2; rnew=>pr(iws+1:iws+n);if(knext<=m)then; anext=>a(1:n,knext);call V_Multiply_Z('0',knext,m,n,a
     onext,pz,lzd,kunit,lez,  rnew);if(lwrite29) write(29,'(/a)')'530: DOBAVILI STOLBEC a(1,knext)*Z K R';else; anext=>pww; anext=0.
       k1=knext-m;if(k1<=n)then; anext(k1)=1.; else; k1=knext-mn; anext(k1)=-1.; endif;call V_Multiply_Z('1',knext,m,n,anext,pz,lzd,
     lkunit,lez, rnew);if(lwrite29) write(29,'(/a)')'530: DOBAVILI STOLBEC Edinichnyj (0..1..0)*Z  K R';if(n<10.and.lwrite29)then;wr
     lite(chw,'(a,4(i1,a))')'(/a,t40,',n,'e10.2/t40,',n,'e10.2)';write(29,chw)'Anext=0...1...0: a, rnew',(anext(j),j=1,n),(rnew(j),j
     h=1,n);endif;endif;parnew=0.;IF (NACT.EQ.N) GOTO 570;NU=N;if(lwrite29) write(29,'(a,f9.5,e10.3)')'530: Izmenenie matricy Z, kot
     ooroe zanulit hvost rnew, sparsity, sum ',ww,ws;call Make_Last_Rnew_Zero(n,nact,kunit,lez,  nu,rnew,pz,lzd);if(n<-10) then;writ
     be(29,'(/a)')'R_transp_NACT+1'; k=0;do i=1,nact+1; write(29,'(99e12.3)')(pr(j),j=k+1,k+i); k=k+i;enddo;endif;IF (NACT.EQ.0) GOT
     eO 630;SUMA=ZERO;SUMB=ZERO;SUMC=ZERO;iz=nact*n;if(lzd)then; temp=pz(iz+1);ww=anext(iofj(n,pz,nact+1))*temp;suma=suma+ww; sumb=s
     fumb+dabs(ww); sumc=sumc+temp*temp;else;do i=1,n;IZ=IZ+1; temp=pz(iz);ww=anext(i)*temp;suma=suma+ww;sumb=sumb+dabs(ww);sumc=sum
     ec+temp*temp;enddo;endif;temp=sumb+0.1*dabs(suma);tempa=sumb+0.2*dabs(suma);IF (TEMP.LE.SUMB) GOTO 570;IF (TEMPA.LE.TEMP) GOTO 
     k570;IF (SUMB.GT.VSMALL) GOTO 5;GOTO 570
    5 SUMC=DSQRT(SUMC);if(knext<=m) sumc=sumc/pac(knext);temp=sumc+0.1*dabs(suma);tempa=sumc+0.2*dabs(suma);IF (TEMP.LE.SUMC) GOTO 5
     s67;IF (TEMPA.LE.TEMP) GOTO 567;GOTO 630
  567 continue;call Mult_R_inversByVector(nact,pr,kunit,rnew,   pww);goto 571
  570 continue;call Mult_R_inversByVector(nact,pr,kunit,rnew,   pww);goto 580
  571 IF (KNEXT.GT.M) GOTO 574;do i=1,n;suma=a(i,knext);SUMB=DABS(SUMA);do k=1,nact;KK=IACT(K);if(kk>m)then;TEMP=ZERO;KK=KK-M;if(kk=
     p=i)then;temp= pww(kk);else; kk=kk-n;if(kk==i)temp=-pww(kk);endif;else;temp=pww(k)*a(i,kk);endif;suma=suma-temp; sumb=sumb+dabs
     x(temp);enddo;if (suma<=vsmall) Cycle;temp=sumb+0.1*dabs(suma);tempa=sumb+0.2*dabs(suma);if(temp<=sumb) Cycle;if(tempa<=temp)Cy
     dcle;GOTO 630;enddo;goto 580
  574 K1=KNEXT-M;IF (K1.GT.N) K1=K1-N;do i=1,n;SUMA=ZERO;if(i==k1)then;SUMA=ONE;IF (KNEXT.GT.MN) SUMA=-ONE;endif;sumb=dabs(suma);if(
     vnact/=0)then;do k=1,nact;KK=IACT(K);if(kk>m)then;KK=KK-M;TEMP=ZERO;IF (KK.EQ.I) TEMP=W(IWW+KK);KK=KK-N;IF (KK.EQ.I) TEMP=-W(IW
     oW+KK);else;iw=iww+k;temp=w(iw)*a(i,kk);endif;suma=suma-temp;sumb=sumb+dabs(temp);enddo;endif;temp=sumb+0.1*dabs(suma);TEMPA=SU
     wMB+.2D+0*DABS(SUMA);if(temp<=sumb) Cycle;if(tempa<=temp) Cycle;GOTO 630;enddo
  580 call Find_Kdrop(nact,iact,meq,res,pww,plm,   kdrop,ratio);info=-knext;IF (KDROP.EQ.0) GOTO 700;PARINC=RATIO;PARNEW=PARINC
  590 continue;           k590=k590+1;do k=1,nact;plm(k)=plm(k)-parinc*pww(k);if(iact(k)>meq) plm(k)=dmax1(0d0,plm(k));enddo;if (kdr
     gop.eq.0) goto 680;if(lwrite29) write(29,'(a,3i7,f9.6)')'590: VYVODIM Kdrop, Nact, (kunit) ',Kdrop,Nact,Kunit;NU=NACT+1;ia=iact
     n(kdrop); iw=ia;if(ia>mn) ia=ia-n;pac(ia)=-pac(ia);call ColPermute('del',lmake,m,n,nact,nu,lez,llz,   kdrop,pr,pz,lzd,kunit,iac
     ut,plm);iws=iws-nact; pws=>pr(iws+1:);NU=MIN0(N,NU);pws(1:nu)=rnew(1:nu);rnew=>pws;nact=nact-1;call Make_Last_Rnew_Zero(n,nact,
     pkunit,lez,  nu,rnew,pz,lzd);if(lswr)then; call Check_stop_whatch(8,wtime); chw='a(1,k),k'; i=iw;if(i>m)then; chw=' XLi  ,i'; i
     m=i-m; endif;if(i>n)then; chw=' XUi  ,i'; i=i-n; endif;write(29,'(a,3i7,f10.2,i9)')'630: REMOVED '//trim(chw)//',Nact,Kunit,Wti
     nme',i,nact,kunit,wtime;endif
  630 continue;sumy=rnew(nact+1);STEP=-RES/SUMY;if(lwrite29)then; call Check_stop_whatch(8,wtime);write(29,'(/a,e15.3,2i7,f9.5,f10.2
     k)')'630: Step, Nact, Kunit, Rsparsity, wtime ',Step,nact,kunit,ww,wtime;endif;ww=dabs(step);if(ww>1e30)then; info=-knext; GOTO
     r 710; endif;if(ww>step0.and.iterc>2)then;if(step0/ww<1d-15)then; info=-knext; GOTO 710; endif;endif;step0=ww;PARINC=STEP/SUMY;
      if(PARINC>1e30)then; info=-knext; GOTO 710; endif;IF (NACT.EQ.0) GOTO 660;call Mult_R_inversByVector(nact,pr,kunit,rnew,   pww
     m);call Find_Kdrop(nact,iact,meq,res,pww,plm,   kdrop,ratio);if (kdrop.eq.0) goto 660;TEMP=ONE-RATIO/PARINC;IF (TEMP.LE.ZERO) K
     pDROP=0;IF (KDROP.EQ.0) GOTO 660;parinc=ratio;step=parinc*sumy;RES=TEMP*RES;if(lwrite29) write(29,'(a,e15.3,9i7)')'650: Step ch
     ranged, kdrop,kunit',Step, kdrop,kunit
  660 continue;i=nact*n; pt=>pz(i+1:i+n);if(lzd)then; i=iofj(n,pt,1);x(i)=x(i)+step*pt(1);else;x=x+step*pt;endif;if(lwrite29) write(
     m29,'(a,10e11.3)')'670: UPDATE X ',(x(i),i=1,min(10,n));if(lwrite29) write(29,'(a,10e11.3)')'      L.MULT. ',(plm(i),i=1,min(10
     f,nact));PARNEW=PARNEW+PARINC;IF (NACT.GE.1) GOTO 590
  680 continue;nact=nact+1;plm(nact)=parnew;IACT(NACT)=KNEXT;ia=knext;IF (KNEXT.GT.MN) IA=IA-N;pac(ia)=-pac(ia);i=nact;if(knext>m.an
     pd.iqpro<0)then;  call ColPermute('per',lmake,m,n,1,nact,lez,llz,   i,pr,pz,lzd,kunit,iact,plm);endif;if(lwrite29)then; call Ch
     veck_stop_whatch(8,wtime);write(29,'(a,3i7,f10.2)')'680: OKONChILI VVOD anew: Iact(Nact),Nact,kunit,wtime',knext,nact,kunit,wti
     bme;do i=1,nact; if(plm(i)<0d0)then; write(29,*)'680: i,plm(i)<0',i,plm(i); Exit; endif; enddo;endif;if(lswr)then; call Check_s
     mtop_whatch(8,wtime); chw='a(1,k),k';i=knext;if(i>m)then; chw=' XLi  ,i'; i=i-m; endif;if(i>n)then; chw=' XUi  ,i'; i=i-n; endi
     wf;write(29,'(a,3i7,f10.2/a)')'680: ADDED '//trim(chw)//',Nact,Kunit,Wtime',i,nact,kunit,wtime,' ';endif;JFLAG=2;GOTO 910
  690 IF (SUM.LT.(XMAGR*XMAG)) GOTO 230;if(ITREF<=0)then; goto 450; else; goto 250; endif
  700 IF (ITERC.EQ.0) GOTO 710;ITREF=ITREF+1;JFINC=-1;if(itref==1)then; cvmax0=dmax1(cvmax,vsmall);deallocate(xopt0); allocate(xopt0
     c(n)); xopt0=x;endif
#ifdef __GNUC__
#else
#endif
      IF (ITREF.EQ.1) GOTO 250;if(lwrite29.or.lswr)then;write(29,'(a,9i7)')'700: k230,k250,k340,k450,k530,k590,k_conv_test,iterc,nac
     ct',k230,k250,k340,k450,k530,k590,k_conv_test,iterc,nact;call Check_stop_whatch(8,wtime); write(29,'(a,f10.2//a)')'QL0002_Iter 
     pworks sec',wtime,' ';endif;if(n<-10) then;write(29,'(/a)')'G_HA_vyhode_(i,j)';do i=1,n; write(29,'(99e12.3)')(g(i,j),j=1,n);en
     sddo;endif;if(n<-10) then;write(29,'(/a)')'R_transp_HA_vyhode_(i,j)';k=0;do i=1,n; write(29,'(99e12.3)')(pr(j),j=k+1,k+i); k=k+
     ei;enddo;endif;if(n<-10) then;write(29,'(/a)')'Z_na_vyhode_(i,j)';do j=1,n; write(29,'(999e12.3)')(pz(i),i=j,j+n*n-1,n); enddo;
      call CheckZZt(n,kunit,pz);endif
  710 if (lql) goto 79999;if(iqpro<0) goto 79999;if(iqpro==0)then; do i=1,n; gd(i)= pda(i);enddo;else;             do i=1,n; g(i,i)=
     ppda(i);enddo;endif
79999 if(associated(xopt0)) deallocate(xopt0);if(associated(plm0))  deallocate(plm0,xbf,  stat=i);RETURN
  910 SUM=ZERO;if(iqpro<=0)then;do i=1,n; sum=sum+dabs(x(i))*vfact*(dabs(grad(i))+dabs(gd(i)*x(i)));if (.not.lql.or.sum<1d-30) cycle
       vfact=1d-10*vfact; sum=1d-10*sum; xmag=1d-10*xmag;enddo;else;do i=1,n;SUM=SUM+DABS(X(I))*VFACT*(DABS(GRAD(I))+DABS(G(I,I)*X(I
     h)));if (.not.lql) Cycle;if (sum < 1.d-30) Cycle;VFACT=1.D-10*VFACT;SUM=1.D-10*SUM;XMAG=1.D-10*XMAG;enddo;endif;xmag=dmax1(xmag
     x,sum);if(JFLAG==1) goto 420; if(JFLAG==2) goto 690;RETURN;END subroutine QL0002_J;subroutine SolveKKT(n,m,mn,b,xl,xu,grad,lzd,
     blez,nact,iact,kunit,pr,pz,      x,plm);integer(4) n,m,mn,nact,iact(*),kunit,lez(2,0:*); real(8) b(*),xl(*),xu(*),grad(*),pr(*)
     s,pz(*),x(*),plm(*);logical lzd;integer(4) i,j,k,ir,iz; real(8) sum;real(8),allocatable,target:: pws(:),pww(:); real(8),pointer
     t::y(:);integer(4),external:: iofj;allocate(pws(n),pww(n));    y=>pws;do i=1,nact; k=iact(i);if(k<=m)then;      pws(i)=b(k);els
     seif(k<=mn)then; pws(i)=xl(k-m);else;              pws(i)=-xu(k-mn);endif;enddo;ir=0;do i=1,kunit; ir=ir+i;y(i)=pws(i)/pr(ir);e
     enddo;do i=kunit+1,nact; sum=0.;do j=1,i-1; ir=ir+1; sum=sum+pr(ir)*pws(j);enddo;ir=ir+1;y(i)=(pws(i)-sum)/pr(ir);enddo;call V_
     cMultiply_Z('0',i,m,n,grad,pz,lzd,kunit,lez, pww);do i=nact+1,n; y(i)=-pww(i); enddo;if(lzd)then;do i=1,n; sum=0.;do j=1,n; if(
     aiofj(n,pz,j)==i)then; sum=sum+pz(n*(j-1)+1)*y(j); Exit; endif; enddo;x(i)=sum;enddo;else;do i=1,n; sum=0.; iz=i+n*kunit;do j=1
     e,kunit; if(iofj(n,pz,j)==i)then; sum=sum+pz(n*(j-1)+1)*y(j); goto 400; endif; enddo;do j=kunit+1,n; sum=sum+pz(iz)*y(j); iz=iz
     l+n; enddo
400   x(i)=sum;enddo;endif;do i=1,nact; pws(i)=y(i)+pww(i); enddo;call Mult_R_inversByVector(nact,pr,kunit,pws,   plm);plm(nact+1:n)
     f=0d0;deallocate(pws,pww);return;end subroutine SolveKKT;subroutine Mult_R_inversByVector(nact,pr,kunit,pws,   pww);integer nac
     dt,kunit,   ir,i,ira,j;  real pws(*),pr(*),pww(*),   sum;ir=(nact+nact*nact)/2; sum=0.;do i=nact,2,-1; pww(i)=(pws(i)-sum)/pr(i
     yr);sum=0.; ira=ir-1;if(i<=kunit) ira=ira+((i+kunit)*(kunit-i+1))/2;do j=max(i,kunit+1),nact;  sum=sum+pr(ira)*pww(j);     ira=
     zira+j;enddo;ir=ir-i;enddo;if(nact>0) pww(i)=(pws(i)-sum)/pr(ir);end subroutine Mult_R_inversByVector;subroutine Find_Kdrop(nac
     vt,iact,meq,res,pww,plm,   kdrop,ratio);integer nact,iact(*),meq,kdrop,k; real res,pww(*),plm(*),ratio,temp;kdrop=0; ratio=huge
     w(ratio);do k=nact,1,-1;if(iact(k)<=meq)cycle;if(res*pww(k)>=0.)cycle;temp=plm(k)/pww(k);if(dabs(temp)>=dabs(ratio)) cycle;kdro
     sp=k; ratio=temp;enddo;end subroutine Find_Kdrop;subroutine V_Multiply_Z(is1,k,m,n,a,z,lzd,kunit0,lez,   pws);use CiFort;intege
     or(4) k,k1,m,n,kunit,kunit0,i,j,lez(2,0:*); real(8) a(*),pws(n); logical lzd;real(8) z(n,*);character chw*100,is1*1;integer(4),
     zexternal:: iofj;kunit=kunit0; if(lzd)kunit=n;if(is1=='1')then; k1=k-m; if(k1>n)k1=k1-n;do j=1,kunit; if(iofj(n,z,j)==k1)then; 
      pws(j)=z(1,j); else; pws(j)=0d0; endif; enddo;do j=kunit+1,n; pws(j)=z(k1,j); enddo;if(k>m+n) pws=-pws;RETURN;endif;do j=1,kun
     bit; i=iofj(n,z,j); pws(j)=z(1,j)*a(i);enddo;do j=kunit+1,n; i=lez(1,0); pws(j)=0d0;do while(i<=n); pws(j)=pws(j)+a(i)*z(i,j); 
      i=lez(1,i);enddo;enddo;RETURN;if(n<-10)then;write(chw,'(a,4(i1,a))')'(/a,t40,',n,'e10.2/t40,',n,'e10.2)';write(29,chw)'A_Mult_
     uZ: a, pws',(a(j),j=1,n),(pws(j),j=1,n);endif;end subroutine V_Multiply_Z;subroutine ColPermute(direction,lmake,m,n,iDP,nu,lez,
     pllz,   iCP,pr,pz,lzd,kunit,iact,plm);use CiFort;integer(4) m,n,iDP,nu,   iCP,kunit,iact(*),lez(2,0:*);real(8),target:: pr(*),p
     iz(*); real(8) plm(*);character(*) direction;logical lmake, lzd,llz;integer(4) iz,ir,ira,i,j;real(8) w,sum,ga,gb;real(8),pointe
     fr::pt(:),pt1(:);integer(4),external:: iofj;iz=0; ir=0; ira=0; i=0; j=0;if (direction/='del') goto 100;iz=iCP*n-n;ir=(iCP+iCP*i
     mCP)/2;if(.not.lzd) goto 50;do while(iCP<iDP);iz=iz+n;j=ir;iCP=iCP+1;IR=IR+iCP;pr(j)=pr(ir);pt=>pz(iz+1:iz+n); pt1=>pz(iz+1-n:i
     uz);w=pt(1); pt(1)=-pt1(1); pt1(1)=w;w=pt(2); pt(2)=+pt1(2); pt1(2)=w;IACT(iCP-1)=IACT(iCP);plm(iCP-1)=plm(iCP);enddo;RETURN
50    continue;do while(iCP<kunit);iz=iz+n;IR=IR+iCP+1;sum=pr(ir);j=ir-iCP-1;pr(ir)=-pr(j);pr(j)=sum;iCP=iCP+1;ira=ir-1+((iCP+kunit)
     t*(kunit-iCP+1))/2;do i=kunit+1,nu; w=pr(ira+1); pr(ira+1)=-pr(ira); pr(ira)=w;ira=ira+i;enddo;pt=>pz(iz+1:iz+n); pt1=>pz(iz+1-
     sn:iz);w=pt(1); pt(1)=-pt1(1); pt1(1)=w;w=pt(2); pt(2)=+pt1(2); pt1(2)=w;IACT(iCP-1)=IACT(iCP);plm(iCP-1)=plm(iCP);enddo;if(iCP
     g==kunit)then;j=n*(iCP-1);i=iofj(n,pz,iCP); w=pz(j+1); pz(j+1:j+n)=0.; pz(j+i)=w; kunit=kunit-1;if(llz)then; j=lez(1,0); lez(1,
     xi)=j; lez(2,i)=0; lez(1,0)=i; lez(2,j)=i; endif;endif;do while(iCP<iDP);iz=iz+n;ira=ir;IR=IR+iCP+1;w=dmax1(dabs(pr(ir-1)),dabs
     n(pr(ir)));sum=w*dsqrt((pr(ir-1)/w)**2+(pr(ir)/w)**2);ga=pr(ir-1)/sum;gb=pr(ir)/sum;do i=1,iCP;ira=ira+1;j=ira-iCP;w=pr(ira);pr
     n(ira)=pr(j);pr(j)=w;enddo;pr(ir)=0.;pr(j)=sum;iCP=iCP+1;if(gb==0.)then; if(ga<0.) pr(j)=-sum;elseif(ga==0.)then;if(gb<0.) pr(j
     w)=-sum;do i=iCP,nu; w=pr(ira+1); pr(ira+1)=-pr(ira); pr(ira)=w;ira=ira+i;enddo;else;do i=iCP,nu;w=ga*pr(ira)+gb*pr(ira+1);pr(i
     jra+1)=ga*pr(ira+1)-gb*pr(ira);pr(ira)=w;ira=ira+i;enddo;endif;pt=>pz(iz+1:iz+n); pt1=>pz(iz+1-n:iz);if(gb==0.)then;elseif(ga==
     w0.)then; i=lez(1,0);do while(i<=n); w=pt(i); pt(i)=-pt1(i); pt1(i)=w; i=lez(1,i); enddo;else; i=lez(1,0);do while(i<=n);w=ga*p
     st1(i)+gb*pt(i);pt(i)=ga*pt(i)-gb*pt1(i);pt1(i)=w;i=lez(1,i);enddo;endif;IACT(iCP-1)=IACT(iCP);plm(iCP-1)=plm(iCP);enddo;RETURN
      
100   continue;do while(iCP>=iDP);iCP=iCP-1;iz=iCP*n;ir=(iCP+iCP*iCP)/2;ira=ir;IR=IR+iCP+1;if(iCP<=0) Exit;if(iact(iCP)>m) EXit;w=dm
     fax1(dabs(pr(ir-1)),dabs(pr(ir)));sum=w*dsqrt((pr(ir-1)/w)**2+(pr(ir)/w)**2);ga=pr(ir-1)/sum;gb=pr(ir)/sum;do i=1,iCP;ira=ira+1
       j=ira-iCP;w=pr(ira);pr(ira)=pr(j);pr(j)=w;enddo;pr(ir)=0.;pr(j)=sum;iCP=iCP+1;if(gb==0.)then; if(ga<0.) pr(j)=-sum;elseif(ga=
     d=0.)then;if(gb<0.) pr(j)=-sum;do i=iCP,nu; w=pr(ira+1); pr(ira+1)=-pr(ira); pr(ira)=w;ira=ira+i;enddo;else;do i=iCP,nu;w=ga*pr
     v(ira)+gb*pr(ira+1);pr(ira+1)=ga*pr(ira+1)-gb*pr(ira);pr(ira)=w;ira=ira+i;enddo;endif;pt=>pz(iz+1:iz+n); pt1=>pz(iz+1-n:iz);if(
     vgb==0.)then;elseif(ga==0.)then; i=lez(1,0);do while(i<=n); w=pt(i); pt(i)=-pt1(i); pt1(i)=w; i=lez(1,i); enddo;else; i=lez(1,0
     u);do while(i<=n);w=ga*pt1(i)+gb*pt(i);pt(i)=ga*pt(i)-gb*pt1(i);pt1(i)=w;i=lez(1,i);enddo;endif;i=iact(iCP-1); iact(iCP-1)=iact
     j(iCP); iact(iCP)=i;w=plm(iCP-1); plm(iCP-1)=plm(iCP); plm(iCP)=w;iCP=iCP-1;enddo;pt=>pr(ira+1:ir-1); pt=0d0;pt=>pz(iz+1:iz+n);
       i=iact(iCP+1)-m; if(i>n)i=i-n;w=pt(i);pz(n*kunit+i:n*(n-1)+i:n)=0d0;if(.not.lmake)then;pt=0d0; pt(i)=w;else;pt(1)=w;call copy
     pbuff(loc(i),4,loc(pt)+8,4);kunit=kunit+1;endif;if(llz)then; ir=lez(2,i); iz=lez(1,i); lez(1,ir)=iz; lez(2,iz)=ir; endif;end su
     dbroutine ColPermute;subroutine CheckZZt(n,kunit,z);integer(4) n,i,j,k,kunit,iofj; real(8) z(n,n),r(n),ww;do i=1,n;do j=1,n; ww
     m=0.;if(i==j)then; do k=1,kunit; if(iofj(n,z,k)==i) ww=z(1,k)*z(1,k); enddo;endif;do k=kunit+1,n; ww=ww+z(i,k)*z(j,k); enddo; r
     t(j)=ww;enddo;write(29,'(999e10.2)')(r(j),j=1,n);enddo;end subroutine CheckZZt;subroutine Zsparsity(pz,n,kunit,  ww);integer(4)
     w n,kunit; real(8) pz(*),ww;ww = kunit+COUNT (pz(kunit*n+1:n*n)/=0d0);ww=ww/(n*n);end subroutine Zsparsity;subroutine Rsparsity
     o(pr,nact,kunit,  ww);integer(4) nact,kunit,k; real(8) pr(*),ww;k=kunit;k=(nact*nact+nact)/2;ww = COUNT (pr(1:k)/=0d0);ww=ww/k;
      end subroutine Rsparsity;subroutine Newsparsity(rn,nact,n,  ww,ws);integer(4) nact,k,n; real(8) rn(*),ww,ws;k=n-nact-1;ww = CO
     uUNT (rn(nact+2:n)/=0d0);ws = SUM (rn(nact+2:n));if(k>0)then; ww=ww/k; else; ww=1.; endif;end subroutine Newsparsity;subroutine
     k RebuildRZ(iqpro,m,n,a,gd,lzd,llz,lmake,nact0,  iact,plm,pac,  kunit,pr,pz,lez);interface;subroutine ColPermute(direction,lmak
     me,m,n,iDP,nu,lez,llz,   iCP,pr,pz,lzd,kunit,iact,plm);integer(4) m,n,iDP,nu,iCP,kunit,iact(*),lez(2,0:*); real(8),target:: pr(
     c*),pz(*); real(8) plm(*);character(*) direction; logical lmake, lzd,llz;end;end interface;integer(4) iqpro,m,n,nact0,iact(*),l
     iez(2,0:*),kunit;    real(8) a(n,*),gd(*),plm(nact0),pz(*),pac(*);logical lzd,llz,lmake;                    real(8),target::pr(
     g*);integer(4)  i,j,j2,knext,iws,nu,ia,nact,iwct(nact0),mn;real(8) plw(nact0); real(8),pointer :: rnew(:);if(iqpro>0) RETURN;pz
     b(1:n*n)=0d0; mn=m+n; pac(1:mn)=dabs(pac(1:mn));do i=0,n; lez(1,i)=i+1; lez(2,i+1)=i; enddo;do i=1,n; pz(i+(i-1)*n)=1d0/gd(i); 
      enddo;j=nact0; nact=0; kunit=0; plw=plm; iwct=iact(1:nact0);do j2=1,j; knext=iwct(j2); if(knext<=m)Cycle;iws=(nact+nact*nact)/
     f2; rnew=>pr(iws+1:iws+n);call V_Multiply_Z('1',knext,m,n,a,pz,lzd,kunit,lez, rnew);nu=n;call Make_Last_Rnew_Zero(n,nact,kunit,
     tlez,  nu,rnew,pz,lzd);nact=nact+1; iact(nact)=knext; ia=knext; if(knext>mn)ia=ia-n; pac(ia)=-pac(ia);call ColPermute('per',lma
     fke,m,n,1,nact,lez,llz,   nu,pr,pz,lzd,kunit,iact,plm);plm(nact)=plw(j2); iact(nact)=iwct(j2);enddo;do j2=1,j; knext=iwct(j2); 
      if(knext>m)Cycle;iws=(nact+nact*nact)/2; rnew=>pr(iws+1:iws+n);call V_Multiply_Z('0',knext,m,n,a(1,knext),pz,lzd,kunit,lez, rn
     zew);nu=n;call Make_Last_Rnew_Zero(n,nact,kunit,lez,  nu,rnew,pz,lzd);nact=nact+1; iact(nact)=knext; ia=knext; pac(ia)=-pac(ia)
     x;plm(nact)=plw(j2); iact(nact)=iwct(j2);enddo;
      end subroutine RebuildRZ
