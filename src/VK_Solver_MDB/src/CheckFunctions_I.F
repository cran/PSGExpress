      subroutine CheckFunctions(mname,xname,fname,kb,ientrop,nfz,knab,ncn,nfn,cfn,wfn,itnab,nmatr,nfmatr,kmtrn,prmn,adyi,nnb, bnds,i
     jx,nfix,kzp,nfp,njp,nfget,l16p,jaddr3,     mnb,p,iqpro,             lconvex,jp,jpb,mget,estmin,chw );use ModCommons; use cifort
      interface;subroutine SplineSumCheck(kcl,ilast,ypar,mpar,yfct,ykn,imatr,mdata,i2,nk, n,nv,ix1,ix2, mv,mknm, itt, fcnm,cname,nus
     ved,mused,myi,yi,p);use IntelInterf; integer(4) mpar,mdata; character(*) fcnm,cname;integer(4) kcl,ilast,itt,nused,mused,imatr,
     fi2,nk,n,nv,mv,mknm, myi,ix1(0:*),ix2(0:*);integer(plen):: yi,ypar,ykn; real(8)  p(*)
#ifdef __APPLE__
  !gcc$ attributes value:: yi,ypar,ykn
#else
      value:: yi,ypar,ykn
#endif
      integer(plen),target:: yfct
#ifdef __APPLE__
  !gcc$ attributes value:: yfct
#else
      value:: yfct
#endif
      end;subroutine FM_200_Check(i,m,ad,n,m1,ad1,n1,m2,ad2,n2,it,j1,j2,j3,iw,chw);use IntelInterf; integer(2) i; integer(4) m,n,m1,
     xn1,m2,n2,it,j1,j2,j3,iw;integer(plen):: ad,ad1,ad2; character(*) chw
#ifdef __APPLE__
  !gcc$ attributes value:: ad,ad1,ad2
#else
      value:: ad,ad1,ad2
#endif
      end;subroutine MultiQuadrPrep(yi,vi,m0,n,ix,   mget,chw);use IntelInterf; integer(4) m0,n,ix(0:*),mget(*); integer(plen):: yi,
     bvi; character(*) chw
#ifdef __APPLE__
  !gcc$ attributes value:: yi,vi
#else
      value:: yi,vi
#endif
      end;subroutine Ro_err_Check(np,mp,ixp, yp, chw);use IntelInterf; use CiFort; integer(4) np,mp, ixp(0:*); integer(plen):: yp; c
     mharacter(*) chw
#ifdef __APPLE__
  !gcc$ attributes value:: yp
#else
      value:: yp
#endif
      end;subroutine SetLableExtLoss(n,m,  yi);use IntelInterf; integer(4) n,m; integer(plen):: yi
#ifdef __APPLE__
  !gcc$ attributes value:: yi
#else
      value:: yi
#endif
      end;subroutine Polynom_Abs_Check(tqsol,m,yi,n,ix,nc,cf,iqpro,klin,ndlin,lconvex,chw);use IntelInterf; integer(2) tqsol; charac
     xter(*) chw; logical lconvex; integer(4) m,n,klin,ndlin,ix(0:n),nc,iqpro;integer(plen):: yi; real(8) cf
#ifdef __APPLE__
  !gcc$ attributes value:: yi
#else
      value:: yi
#endif
      end;subroutine Eut_Fun_Check(itt,m,yi,n,ix,nz,nf,p,nc,cf,  estmin, chw);use IntelInterf; integer(4) itt,m,n,nz, ix(0:n),nf(nz)
     i,nc(nz); integer(plen):: yi
#ifdef __APPLE__
  !gcc$ attributes value:: yi
#else
      value:: yi
#endif
      real(8) cf(nz),p(*),estmin; character(*) chw;end;subroutine Card_Check(mname,m,yi,n,ix,nz,nf,wfm,chw);use IntelInterf; integer
     y(4) m,n,nz,ix(0:*),nf(*); character(*) mname,chw; real(8) wfm(*); integer(plen):: yi
#ifdef __APPLE__
  !gcc$ attributes value:: yi
#else
      value:: yi
#endif
      end;subroutine Entropy_LogSum_Check(yi,n,ix,nz,nf,ientrop,chw);use IntelInterf; integer(4) n,nz, ix(0:n),nf(nz),ientrop; integ
     cer(plen):: yi; character(*) chw
#ifdef __APPLE__
  !gcc$ attributes value:: yi
#else
      value:: yi
#endif
      end;subroutine pCvarPrep(n,yi,ix,   jp,jpb);use IntelInterf; integer(4) n,jp(0:*),jpb(0:*),ix(0:*); integer(plen):: yi
#ifdef __APPLE__
  !gcc$ attributes value:: yi
#else
      value:: yi
#endif
      end;subroutine All_ni_nd_Check(itnab,nm,km,yis,n,ns,m,ms,ixm,ixs,nz,nc,bnds,cf,chw,iret);use IntelInterf; integer(4) itnab,km,
     sn,ns,m,ms,nz,ixm(0:n),ixs(0:ns),nc(*),nm,iret;integer(plen):: yis; real(8) bnds(0:1,0:*),cf(*); character(*) chw
#ifdef __APPLE__
  !gcc$ attributes value:: yis
#else
      value:: yis
#endif
      end;subroutine KSM_Check(yi,vy,vq,n,ix,mv,nz,nf,wf,yi0,myi,p, chw,iret);use IntelInterf; use CiFort; integer(4) n,nz,myi,mv, i
     rx(0:*),nf(*),iret;integer(plen):: yi0,yi,vy,vq; real(8) p(*),wf(*); character(*) chw
#ifdef __APPLE__
  !gcc$ attributes value:: yi0,yi,vy,vq
#else
      value:: yi0,yi,vy,vq
#endif
      end;subroutine KSM_fun_ni_Check(vy,vq,vw,vp,mv,nz,nf,yi,ys,n,ix,wf,yi0,myi,p,chw,iret);use IntelInterf; use CiFort; integer(4)
     o n,nz,myi,mv, ix(0:*),nf(*),iret; character(*) chw;integer(plen):: yi0,yi,ys,vy,vq,vw,vp; real(8) p(*),wf(*)
#ifdef __APPLE__
  !gcc$ attributes value:: yi0,yi,ys,vy,vq,vw,vp
#else
      value:: yi0,yi,ys,vy,vq,vw,vp
#endif
      end;subroutine Kantor_Check(n,mv,vy,vq,   chw,iret);use IntelInterf; integer(4) n,mv,iret;  character(*) chw; integer(plen):: 
     fvy,vq
#ifdef __APPLE__
  !gcc$ attributes value:: vy,vq
#else
      value:: vy,vq
#endif
      end;subroutine super_comp_check(itt,n,m,m4,ix2,p1,yi,y4,kmt,     prmn,ix1,p2,jp,jpb);use IntelInterf; integer(4) itt,n,m,m4,km
     gt,ix2(0:*),ix1(0:*),jp(*),jpb(*); real(8) prmn,p2(*),p1(*);integer(plen):: yi,y4
#ifdef __APPLE__
  !gcc$ attributes value:: yi,y4
#else
      value:: yi,y4
#endif
      end;end interface;character(lnm), external :: getconname;character(*) mname(*),xname(-3:*),fname(*);real(8) cfn(*),wfn(*),  bn
     cds(0:1,0:*),p(*),prmn(*),estmin; integer(plen) jaddr3(2,*);integer(4) kb(0:*),kzp,ientrop,nfz(*),knab,ncn(*),nfn(kzp),itnab(*)
     v,nmatr(*),nfmatr(*),nfget(*),kmtrn(*),mnb(*),nnb(*),ix(*),nfix(*),nfp(*),njp(*),jp(*),jpb(*),mget(*),l16p,iqpro;integer(plen) 
     eadyi(*),i8,i81; logical lconvex;character(*) chw; integer(4) j1,j2,j3,nz,iw,nm,i,k,mt,j,i1;character  wstr*(l16p),wch*256;inte
     uger(4) msec,mfirst,ilast,mthr,knotsmatrix,kcl,jw,mknts,i2,j4,iret; real(8) w;do k=1,knab;select case(itnab(k)); case(-1); mt=n
     qmatr(nfmatr(k));do i=nfz(k),nfz(k+1)-1; if(fname(k)(1:3)=='tsp'.and.index(fname(k),'(')>0) Exit;if(abs(nfn(i))<=1.and.mnb(mt)>
     x2)then; chw='Linear function should use matrix with one row';call putmess('S',5155,'Check linear function',chw); goto 79999;en
     cdif;enddo;end select;select case(itnab(k)); case(5:7,305:307);mt=nmatr(nfmatr(k)); j=nmatr(nfmatr(k)-1+kmtrn(k)-1); j1=nmatr(n
     xfmatr(k)-1+kmtrn(k));call FM_200_Check(int2(0),mnb(mt),adyi(mt),nnb(mt),mnb(j),adyi(j),nnb(j),mnb(j1),adyi(j1),nnb(j1),itnab(k
     y),  j1,j2,j3,i, chw);end select;select case(itnab(k)); case(304:307);mt=nmatr(nfmatr(k)); j=nmatr(nfmatr(k)+1);call MultiQuadr
     aPrep(adyi(mt),adyi(j),mnb(mt),nnb(mt),ix(nfix(mt)),mget(nfget(k)),chw);kmtrn(k)=kmtrn(k)-1; do j=1,kmtrn(k)-1; nmatr(nfmatr(k)
     r+j)=nmatr(nfmatr(k)+j+1); enddo;end select;select case(itnab(k)); case(500:599);mt=nmatr(nfmatr(k)); msec=nmatr(nfmatr(k)+1); 
      i1=njp(k)+mnb(mt)+2;do j=0,mnb(msec); jp(i1+j)=j+1; jpb(i1+j+1)=j; enddo;end select;select case(itnab(k)); case(25);msec=nmatr
     y(nfmatr(k)+1);call Ro_err_Check(nnb(msec),mnb(msec),ix(nfix(msec)),adyi(msec), chw);end select;select case(itnab(k)); case(202
     e);mt=nmatr(nfmatr(k)); msec=nmatr(nfmatr(k)+1);w=1./mnb(mt); do j=0,mnb(mt)-1; p(nfp(mt)+j)=w; enddo;do i=0,nnb(mt); ix(nfix(m
     kt)+i)=ix(nfix(msec)+i); enddo;call SetLableExtLoss(nnb(mt),mnb(mt),  adyi(mt));end select;enddo;do k=1,knab; if(itnab(k)<400.o
     rr.itnab(k)>443) Cycle;if(.not.(index(fname(k),'spline_sum')==1.or.index(fname(k),'$wO#rK@_')==1)) Cycle;wstr=getconname(ncn(nf
     oz(k))+1,jaddr3(:,1));mfirst=nmatr(nfmatr(k)); iw=0; ilast=0;msec=nmatr(nfmatr(k)+1); mthr=nmatr(nfmatr(k)+2);knotsmatrix=0;if(
     okmtrn(k)>3)then; j=nmatr(nfmatr(k)+3);if(kmtrn(k)/=4)then;chw='Problem Statement: number of input matrices is incorrect in Spl
     vine_sum function. It should be 2 or 3';call putmess('S',5955,'Spline checking',chw); goto 79999;endif;do i=nfix(j),nfix(j)+nnb
     w(j)-1; j1=ix(i);do kcl=1,nnb(msec); if(ix(nfix(msec)+kcl-1)==j1)Exit; enddo;if(kcl>nnb(msec))then;chw='Matrix of knots (third 
     jinput matrix in Spline_sum) contains unknown column name in header row';call putmess('S',5953,'Spline checking',chw); goto 799
     w99;endif;enddo;knotsmatrix=1;goto 100;endif;if(kmtrn(k)>nnb(msec)+3)then;chw='Problem Statement: number of input matrices is i
     qncorrect in Spline_sum function. It should be 2 or 3';call putmess('S',5956,'Spline checking',chw); goto 79999;endif;do i1=3,k
     cmtrn(k)-1; iw=0;if(index(mname(nmatr(nfmatr(k)+i1)),'vector_knots_')==1) iw=1;if(index(mname(nmatr(nfmatr(k)+i1)),'ivector_kno
     mts_')==1) iw=2;if(iw==0)Exit;do kcl=1,nnb(msec); chw=xname(ix(nfix(msec)+kcl-1));if(index(           mname(nmatr(nfmatr(k)+i1)
     x)(13+iw:)             ,trim(chw)                  )==1)Exit;enddo;if(kcl>nnb(msec))Exit;enddo;if(i1<kmtrn(k))then; chw='A name
     y of vector_knots_... is incorrect'; call putmess('S',5954,'Spline checking',chw);goto 79999;endif
100   continue;nm=0; iw=0; jw=0;do kcl=0,nnb(msec)-1; if(kcl==nnb(msec)-1)ilast=1;chw=xname(ix(nfix(msec)+kcl)); j1=ix(nfix(msec)+kc
     xl);if(knotsmatrix==1)then; j3=nmatr(nfmatr(k)+3); mknts=mnb(j3);do i2=nfix(j3),nfix(j3)+nnb(j)-1; if(ix(i2)==j1)Exit; enddo;i2
     j=i2-nfix(j3);if(i2==nnb(j))then; mknts=0;wch='There are no knots for factor '//trim(chw)//' in matrix with knots';call putmess
     y('W',0,'SplineSum checking',wch);endif;else; i2=0;do i1=3,kmtrn(k)-1;i=index(mname(nmatr(nfmatr(k)+i1)),'_'); i=i+index(mname(
     mnmatr(nfmatr(k)+i1))(i+1:),'_');if(index(mname(nmatr(nfmatr(k)+i1))(i+1:),trim(chw))==1)Exit;enddo;if(i1>=kmtrn(k))then; j3=ms
     pec; mknts=0; else; j3=nmatr(nfmatr(k)+i1); mknts=mnb(j3); endif;endif;call SplineSumCheck(kcl,ilast,   adyi(msec) ,mnb(msec), 
     e  adyi(mthr) ,   adyi(j3) ,knotsmatrix,mthr,i2,nnb(j3),nnb(mfirst),nnb(msec),ix(nfix(msec)),ix(nfix(mthr)),mnb(mthr),mknts,itn
     xab(k),chw,trim(wstr),      iw,jw,mnb(mfirst),    adyi(mfirst),p(nfp(mfirst)));if(ioutk>=istop-1) goto 79999;enddo;if(mnb(mfirs
     tt)<=0)then; nfn(nfz(k))=-abs(nfn(nfz(k))); kb(ncn(nfz(k)))=-kb(ncn(nfz(k))); endif;enddo;do k=1,knab; if(itnab(k)/=10) Cycle; 
      mt=nmatr(nfmatr(k));do i=nfz(k),nfz(k+1)-1; if(nfn(i).ne.270) Cycle;call Polynom_Abs_Check(int2(1),mnb(mt),adyi(mt),nnb(mt),ix
     j(nfix(mt)),ncn(i),cfn(i),iqpro,j,i1,lconvex, chw);if(ioutk>=istop-1) goto 79999;enddo;enddo;do k=1,knab; if(itnab(k)/=13.and.i
     htnab(k)/=413) Cycle; mt=nmatr(nfmatr(k));i=nfz(k); nz=nfz(k+1)-i;call Eut_Fun_Check(itnab(k),mnb(mt),adyi(mt),nnb(mt),ix(nfix(
     hmt)),nz,nfn(i),p(nfp(mt)),ncn(i),cfn(i),estmin,chw);enddo;do k=1,knab; if(itnab(k)/=12) Cycle; mt=nmatr(nfmatr(k));i=nfz(k); n
     jz=nfz(k+1)-i;call Card_Check(mname(mt),mnb(mt),adyi(mt), nnb(mt),ix(nfix(mt)),nz,nfn(i),wfn(i),chw);enddo;do k=1,knab; if(itna
     nb(k)/=11) Cycle; mt=nmatr(nfmatr(k));i=nfz(k); nz=nfz(k+1)-i;call Entropy_LogSum_Check(adyi(mt),nnb(mt),ix(nfix(mt)),nz,nfn(i)
     r,ientrop,chw);enddo;do k=1,knab; if(itnab(k)/=23) Cycle; mt=nmatr(nfmatr(k)); j=nmatr(nfmatr(k)+1); j1=nmatr(nfmatr(k)+2);call
     y pCvarPrep(nnb(mt),adyi(mt),ix(nfix(mt)),      jp(njp(k)),jpb(njp(k))  );enddo;do k=1,knab; selectcase(itnab(k)); case(16:18, 
     f20:21,26,27); case default; Cycle; endselect; mt=nmatr(nfmatr(k));i=nfz(k); nz=nfz(k+1)-i; iw=kmtrn(k); if(itnab(k)==26) iw=2;
      do nm=2,iw; j=nmatr(nfmatr(k)-1+nm);if(itnab(k)==26)then; mt=j; j=nmatr(nfmatr(k)+2); endif;call All_ni_nd_Check(itnab(k),nm,i
     ww,adyi(j),nnb(mt),nnb(j),mnb(mt),mnb(j),ix(nfix(mt)),ix(nfix(j)),nz,ncn(i),bnds,cfn(i),chw,iret);if(iret==1) goto 79999;enddo;
      enddo;do k=1,knab; if(itnab(k)/=24) Cycle; i1=nfmatr(k)+1; mt=nmatr(i1); j=nmatr(i1+1); j1=nmatr(i1+2); i1=nmatr(i1-1);i=nfz(k
     s); nz=nfz(k+1)-i;call KSM_Check(adyi(mt),adyi(j),adyi(j1),nnb(mt),ix(nfix(mt)),mnb(j1),nz,nfn(i),wfn(i),adyi(i1),mnb(i1),p(nfp
     e(i1)),chw,iret);if(iret==1) goto 79999;enddo;do k=1,knab; if(itnab(k)/=26) Cycle; i1=nfmatr(k)+1; mt=nmatr(i1); j=nmatr(i1+1);
       j1=nmatr(i1+2); j2=nmatr(i1+3);i=nfz(k); nz=nfz(k+1)-i; i8=0; i81=0;if(kmtrn(k)>=6)then; j3=nmatr(i1+4); i8=adyi(j3); endif;i
     if(kmtrn(k)==7)then; j4=nmatr(i1+5); i81=adyi(j4); endif;i1=nmatr(i1-1);call KSM_fun_ni_Check(adyi(j1),adyi(j2),i8,i81,mnb(j1),
     cnz,nfn(i),adyi(mt),adyi(j),nnb(mt),ix(nfix(mt)),wfn(i),adyi(i1),mnb(i1),p(nfp(i1)),chw, iret);if(iret==1) goto 79999;enddo;do 
     dk=1,knab; if(itnab(k)/=34) Cycle; mt=nmatr(nfmatr(k)); j=nmatr(nfmatr(k)+1); j1=nmatr(nfmatr(k)+2);call Kantor_Check(nnb(mt),m
     enb(j1),adyi(j),adyi(j1),     chw, iret);if(iret==1) goto 79999;enddo;do k=1,knab; if(itnab(k)<10000)Cycle; mt=nmatr(nfmatr(k))
        j1=nmatr(nfmatr(k)+1);j3=nmatr(nfmatr(k)+3);call super_comp_check(itnab(k),nnb(mt),mnb(mt),mnb(j3),ix(nfix(j1)),p(nfp(mt)),a
     zdyi(mt),adyi(j3),kmtrn(k), prmn(k),ix(nfix(mt)),p(nfp(j1)),jp(njp(k)),jpb(njp(k)));enddo
79999 end subroutine CheckFunctions
