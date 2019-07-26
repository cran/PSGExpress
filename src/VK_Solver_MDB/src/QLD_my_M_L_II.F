      subroutine Make_Last_Rnew_Zero(n,nact,kunit,lez,  j,rnew,z,lzd);integer(4) n,nact,kunit,i,j,jd,j1,it,lez(2,0:*); real(8) rnew(
     bn),z(n,n),w,w1,gb,ga; logical lzd;character chw*100;integer(4),external:: iofj;do while(j>nact+1); j1=j-1; if(rnew(j)==0.) got
     mo 200
100   if(rnew(j1)/=0.)then;ga=rnew(j1);  gb=rnew(j);w=dmax1(dabs(ga),dabs(gb));w1=w*dsqrt((ga/w)**2+(gb/w)**2);ga=ga/w1; gb=gb/w1;rn
     few(j1)=w1;if(lzd)then;do i=kunit+1,n; w=z(1,i); it=iofj(n,z,i); z(1,i)=0.; z(2,i)=0.; z(it,i)=w;enddo;lzd=.false.;endif;i=lez(
     q1,0);do while(i<=n); w1=z(i,j1); w=z(i,j);z(i,j1)=ga*w1+gb*w; z(i,j)=-gb*w1+ga*w;i=lez(1,i);enddo;jd=0;else;do while(j1>nact+1
     a.and.rnew(j1)==0.); j1=j1-1;enddo;if(rnew(j1)/=0.) goto 100;rnew(j1)=rnew(j);if(lzd)then;w=z(1,j); z(1,j)=-z(1,j1); z(1,j1)=w;
      w=z(2,j); z(2,j)=+z(2,j1); z(2,j1)=w;else;i=lez(1,0);do while(i<=n); w=z(i,j); z(i,j)=z(i,j1); z(i,j1)=w; i=lez(1,i);enddo;end
     mif;endif
200   j=j1;enddo;if(n<-10)then;write(chw,'(a,4(i1,a))')'(/a,i1,t40,',n,'e10.2/',n,'(',n,'e10.2/),t40,',n,'e10.2)';write(29,chw)'Make
     o_Pws_Zero: Nact=',nact,(rnew(j),j=1,n),((z(i,j),j=1,n),i=1,n),(rnew(j),j=1,n);endif;end subroutine Make_Last_Rnew_Zero;SUBROUT
     rINE QL0001_J(M, ME,N,MNN,lql,C,D,a,B,XL,XU,X,IOUT,IFAIL,IPRINT,WAR,LW,iact, eps, iqpro);INTEGER N,MNN,LW,  iqpro;DOUBLE PRECIS
     cION C,cd,D,A,B,X,XL,XU,WAR,DIAG, EPS, wf0,timelimit;INTEGER M,ME,IOUT,IFAIL,IPRINT,iact,J,I, IDIAG,INFO,NACT,MAXIT, iwa,iu,m1,
     xm2;LOGICAL LQL,ldn;DIMENSION c(n,*),cd(*),d(0:n),a(n,*),b(*), X(N),XL(N),XU(N),WAR(*),iu(mnn),iact(n);real(8),save:: ZERO,QPEP
     vS;interface;subroutine QL0002_amod(chrc,n,m,meq,lw,a,b,     w,info);character(*) chrc; integer n,m,meq,lw,info; real(8),target
     r:: w(*),a(n,*); real(8) b(*);end;subroutine QL0002_Zero(chrc,iqpro,n,m,meq,m1,m2,nact,iact,lql, a,b,g,gd,grad, x,xl,xu, vsmall
     f,diag,w,  LW, war);character(*) chrc;integer n,m,m1,m2,meq,lw,iqpro,nact,iact(*); real(8),target:: w(*),a(n,*);real(8) b(*),g(
     qn,*),gd(*),grad(n),x(n),xl(*),xu(*),vsmall,diag,    war;logical lql;end;subroutine QL0002_Iter(chrc,iqpro,n,m,meq,ldn,lql,a,b,
     ygrad,g,gd, xl,xu,x,nact,iact,maxit,vsmall,info,w,lw,  war);character(*) chrc;integer n,m,meq,lw,iqpro,nact,iact(*),maxit,info;
       real(8),target:: w(*),a(n,*);real(8) b(*),g(n,*),gd(*),grad(n),x(n),xl(*),xu(*),vsmall,    war;logical lql,ldn;end;end interf
     cace;ENTRY QL0001_Init(iqpro,n,m,eps,lw,iprint,iout,          ifail,iwa)
      iwa=0; if(iqpro==5) Return;ifail=0;zero=0.0d+0; info=0;qpeps=eps;i=3*n*n/2 + 10*n + m;if ((1+i).gt.lw) goto 80;call QL0002_Ini
     it('init',n,  iwa);RETURN;ENTRY QL0001_amod(iqpro,n,m,me,lw,a,b,iprint,iout,    war,ifail,nact)
      if(iqpro==5) Return;ifail=0; nact=0;call QL0002_amod('amod',n,m,me,lw,a,b,  WAR,info);if (info.lt.0) goto  70;RETURN;ENTRY QL0
     c001_Zero(       iqpro,n,m,me,m1,m2,nact,iact,lql, a,b,c,cd,d,    x,xl,xu,             WAR,lw,iprint,iout,ifail)
      ifail=0;call  QL0002_Zero('zero',iqpro,n,m,me,m1,m2,nact,iact,lql, a,b,c,cd,d(1), x,xl,xu, qpeps, diag,war,LW,   war(lw+1));ID
     rIAG=0;IF ((DIAG.GT.ZERO).AND.(DIAG.LT.1000.0)) IDIAG=int(DIAG);IF ((IPRINT.GT.0).AND.(IDIAG.GT.0)) WRITE(IOUT,1000) IDIAG;RETU
     jRN;ENTRY GPSR_vk1_Iter(timelimit,wf0,iqpro,n,m,c,d,xl,xu, iprint, x,ifail)
      ifail=0;maxit=40*(m+n);if(iqpro>0.and.m==0)then;call GPSR_vk2_Iter(iprint,timelimit,wf0,n, d,c,xl,xu,x,maxit,qpeps,ifail);else
       ifail=10;endif;RETURN;ENTRY GPSR_vk1_Ext(timelimit,wf0,iqpro,n,m, d,xl,xu, iprint, x,ifail)
      ifail=0;maxit=40*(m+n);if(iqpro>0.and.m==0)then;call GPSR_vk2_Ext(iprint,timelimit,wf0,n, d, xl,xu,x,maxit,qpeps,ifail);else;i
     mfail=10;endif;RETURN;ENTRY QL0001_Iter(ldn,lql,iqpro,n,me,m,mnn,a,b,c,cd,d, xl,xu,    iprint,iout,  x,iu,ifail,iact,   war,lw,
     onact)
      ifail=0;maxit=40*(m+n);call QL0002_Iter('iter',iqpro,n,m,me,ldn,lql,a,b,d(1),c,cd,xl,xu,x,nact,iact,maxit,qpeps,info, war,lw,w
     kar(lw+1));IF (INFO.EQ.1) GOTO 40;IF (INFO.EQ.2) GOTO 90;IF (INFO.LT.0) GOTO 70;if (info==3) goto 100;if (info==4) goto 110;iu=
     l0; do i=1,nact; j=iact(i); iu(j)=i; enddo;RETURN
   70 IFAIL=-INFO+10;IF ((IPRINT.GT.0).AND.(NACT.GT.0))WRITE(IOUT,1100) -INFO,(iact(I),I=1,NACT);RETURN
   80 IFAIL=5;IF (IPRINT.GT.0) WRITE(IOUT,1200);RETURN;IF (IPRINT.GT.0) WRITE(IOUT,1210);RETURN;IF (IPRINT.GT.0) WRITE(IOUT,1220);RE
     pTURN
   40 IFAIL=1;IF (IPRINT.GT.0) WRITE(IOUT,1300) MAXIT;RETURN
   90 IFAIL=2;IF (IPRINT.GT.0) WRITE(IOUT,1400);RETURN
  100 ifail=3; if (iprint.gt.0) write(iout,'(/a)')'***ql: Rost neviazki > 10000 na ochistke';return
  110 ifail=4; if (iprint.gt.0) write(iout,'(/a)')'***ql: Ushli s ochistki, ne dostignut tochnosti';return
 1000 FORMAT(/8X,28H***QL: MATRIX G WAS ENLARGED,I3,20H-TIMES BY UNITMATRIX)
 1100 FORMAT(/8X,18H***QL: CONSTRAINT ,I5,19H NOT CONSISTENT TO ,/,(100I5))
 1200 format(/8x,19H***ql: lw too small)
 1210 FORMAT(/8X,22H***QL: LIWAR TOO SMALL)
 1220 FORMAT(/8X,20H***QL: MNN TOO SMALL)
 1300 FORMAT(/8X,37H***QL: TOO MANY ITERATIONS (MORE THAN,I6,1H))
 1400 FORMAT(/8X,50H***QL: ACCURACY INSUFFICIENT TO ATTAIN CONVERGENCE);
      END
