      subroutine CheckFunctions(mname,xname,fname,kb,ientrop,nfz,knab,ncn,nfn,cfn,wfn,itnab,nmatr,nfmatr,
     +kmtrn,prmn,adyi,nnb, bnds,ix,nfix,kzp,nfp,njp,nfget,l16p,
     +jaddr3,     mnb,p,
     +iqpro,             lconvex,jp,jpb,mget,estmin,
     +chw );use ModCommons; use cifort;interface;subroutine SplineSumCheck(kcl,ilast,ypar,mpar,yfct,ykn,imatr,mdata,i2,nk, n,nv,ix1,
     lix2, mv,mknm, itt, fcnm,cname,
     +nused,mused,myi,yi,p);use IntelInterf; integer(4) mpar,mdata; character(*) fcnm,cname;integer(4) kcl,ilast,itt,nused,mused,ima
     ltr,i2,nk,n,nv,mv,mknm, myi,ix1(0:*),ix2(0:*);integer(plen),value:: yi,ypar,ykn; real(8)  p(*);integer(plen),target,value:: yfc
     lt;end;subroutine FM_200_Check(i,m,ad,n,m1,ad1,n1,m2,ad2,n2,it,j1,j2,j3,iw,chw);use IntelInterf; integer(2) i; integer(4) m,n,m
     l1,n1,m2,n2,it,j1,j2,j3,iw;integer(plen),value:: ad,ad1,ad2; character(*) chw;end;subroutine MultiQuadrPrep(yi,vi,m0,n,ix,   mg
     let,chw);use IntelInterf; integer(4) m0,n,ix(0:*),mget(*); integer(plen),value:: yi,vi; character(*) chw;end;subroutine Ro_err_
     lCheck(np,mp,ixp, yp, chw);use IntelInterf; use CiFort; integer(4) np,mp, ixp(0:*); integer(plen),value:: yp; character(*) chw;
      end;subroutine SetLableExtLoss(n,m,  yi);use IntelInterf; integer(4) n,m; integer(plen),value:: yi;end;subroutine Polynom_Abs_
     lCheck(tqsol,m,yi,n,ix,nc,cf,iqpro,klin,ndlin,lconvex,chw);use IntelInterf; integer(2) tqsol; character(*) chw; logical lconvex
     l; integer(4) m,n,klin,ndlin,ix(0:n),nc,iqpro;integer(plen),value:: yi; real(8) cf;end;subroutine Eut_Fun_Check(itt,m,yi,n,ix,n
     lz,nf,p,nc,cf,  estmin, chw);use IntelInterf; integer(4) itt,m,n,nz, ix(0:n),nf(nz),nc(nz); integer(plen),value:: yi;real(8) cf
     l(nz),p(*),estmin; character(*) chw;end;subroutine Card_Check(mname,m,yi,n,ix,nz,nf,wfm,chw);use IntelInterf; integer(4) m,n,nz
     l,ix(0:*),nf(*); character(*) mname,chw; real(8) wfm(*); integer(plen),value:: yi;end;subroutine Entropy_LogSum_Check(yi,n,ix,n
     lz,nf,ientrop,chw);use IntelInterf; integer(4) n,nz, ix(0:n),nf(nz),ientrop; integer(plen),value:: yi; character(*) chw;end;sub
     lroutine pCvarPrep(n,yi,ix,   jp,jpb);use IntelInterf; integer(4) n,jp(0:*),jpb(0:*),ix(0:*); integer(plen),value:: yi;end;subr
     loutine All_ni_nd_Check(itnab,nm,km,yis,n,ns,m,ms,ixm,ixs,nz,nc,bnds,cf,chw,iret);use IntelInterf; integer(4) itnab,km,n,ns,m,m
     ls,nz,ixm(0:n),ixs(0:ns),nc(*),nm,iret;integer(plen),value:: yis; real(8) bnds(0:1,0:*),cf(*); character(*) chw;end;subroutine 
     lKSM_Check(yi,vy,vq,n,ix,mv,nz,nf,wf,yi0,myi,p, chw,iret);use IntelInterf; use CiFort; integer(4) n,nz,myi,mv, ix(0:*),nf(*),ir
     let;integer(plen),value:: yi0,yi,vy,vq; real(8) p(*),wf(*); character(*) chw;end;subroutine KSM_fun_ni_Check(vy,vq,vw,vp,mv,nz,
     lnf,yi,ys,n,ix,wf,yi0,myi,p,chw,iret);use IntelInterf; use CiFort; integer(4) n,nz,myi,mv, ix(0:*),nf(*),iret; character(*) chw
      integer(plen),value:: yi0,yi,ys,vy,vq,vw,vp; real(8) p(*),wf(*);end;subroutine Kantor_Check(n,mv,vy,vq,   chw,iret);use IntelI
     lnterf; integer(4) n,mv,iret;  character(*) chw; integer(plen),value:: vy,vq;end;subroutine super_comp_check(itt,n,m,m4,ix2,p1,
     lyi,y4,kmt,     prmn,ix1,p2,jp,jpb);use IntelInterf; integer(4) itt,n,m,m4,kmt,ix2(0:*),ix1(0:*),jp(*),jpb(*); real(8) prmn,p2(
     l*),p1(*);integer(plen),value:: yi,y4;end;end interface;character(lnm), external :: getconname;character(*) mname(*),xname(-3:*
     l),fname(*);real(8) cfn(*),wfn(*),  bnds(0:1,0:*),p(*),prmn(*),estmin; integer(plen) jaddr3(2,*);integer(4) kb(0:*),kzp,ientrop
     l,nfz(*),knab,ncn(*),nfn(kzp),itnab(*),nmatr(*),nfmatr(*),nfget(*),
     +kmtrn(*),mnb(*),nnb(*),ix(*),nfix(*),nfp(*),njp(*),jp(*),jpb(*),mget(*),l16p,iqpro;integer(plen) adyi(*),i8,i81; logical lconv
     lex;character(*) chw; integer(4) j1,j2,j3,nz,iw,nm,i,k,mt,j,i1;character  wstr*(l16p),wch*256;integer(4) msec,mfirst,ilast,mthr
     l,knotsmatrix,kcl,jw,mknts,i2,j4,iret; real(8) w;do k=1,knab;select case(itnab(k)); case(-1); mt=nmatr(nfmatr(k));do i=nfz(k),n
     lfz(k+1)-1; if(fname(k)(1:3)=='tsp'.and.index(fname(k),'(')>0) Exit;if(abs(nfn(i))<=1.and.mnb(mt)>2)then; chw='Linear function 
     lshould use matrix with one row';call putmess('S',5155,'Check linear function',chw); goto 79999;endif;enddo;end select;select c
     lase(itnab(k)); case(5:7,305:307);mt=nmatr(nfmatr(k)); j=nmatr(nfmatr(k)-1+kmtrn(k)-1); j1=nmatr(nfmatr(k)-1+kmtrn(k));call FM_
     l200_Check(int2(0),mnb(mt),adyi(mt),nnb(mt),mnb(j),adyi(j),nnb(j),mnb(j1),adyi(j1),nnb(j1),
     +itnab(k),  j1,j2,j3,i, chw);end select;select case(itnab(k)); case(304:307);mt=nmatr(nfmatr(k)); j=nmatr(nfmatr(k)+1);call Mul
     ltiQuadrPrep(adyi(mt),adyi(j),mnb(mt),nnb(mt),ix(nfix(mt)),mget(nfget(k)),chw);kmtrn(k)=kmtrn(k)-1; do j=1,kmtrn(k)-1; nmatr(nf
     lmatr(k)+j)=nmatr(nfmatr(k)+j+1); enddo;end select;select case(itnab(k)); case(500:599);mt=nmatr(nfmatr(k)); msec=nmatr(nfmatr(
     lk)+1); i1=njp(k)+mnb(mt)+2;do j=0,mnb(msec); jp(i1+j)=j+1; jpb(i1+j+1)=j; enddo;end select;select case(itnab(k)); case(25);mse
     lc=nmatr(nfmatr(k)+1);call Ro_err_Check(nnb(msec),mnb(msec),ix(nfix(msec)),adyi(msec), chw);end select;select case(itnab(k)); c
     lase(202);mt=nmatr(nfmatr(k)); msec=nmatr(nfmatr(k)+1);w=1./mnb(mt); do j=0,mnb(mt)-1; p(nfp(mt)+j)=w; enddo;do i=0,nnb(mt); ix
     l(nfix(mt)+i)=ix(nfix(msec)+i); enddo;call SetLableExtLoss(nnb(mt),mnb(mt),  adyi(mt));end select;enddo;do k=1,knab; if(itnab(k
     l)<400.or.itnab(k)>443) Cycle;if(.not.(index(fname(k),'spline_sum')==1.or.index(fname(k),'$wO#rK@_')==1)) Cycle;wstr=getconname
     l(ncn(nfz(k))+1,jaddr3(:,1));mfirst=nmatr(nfmatr(k)); iw=0; ilast=0;msec=nmatr(nfmatr(k)+1); mthr=nmatr(nfmatr(k)+2);knotsmatri
     lx=0;if(kmtrn(k)>3)then; j=nmatr(nfmatr(k)+3);if(kmtrn(k)/=4)then;chw='Problem Statement: number of input matrices is incorrect
     l in Spline_sum function. It should be 2 or 3';call putmess('S',5955,'Spline checking',chw); goto 79999;endif;do i=nfix(j),nfix
     l(j)+nnb(j)-1; j1=ix(i);do kcl=1,nnb(msec); if(ix(nfix(msec)+kcl-1)==j1)Exit; enddo;if(kcl>nnb(msec))then;chw='Matrix of knots 
     l(third input matrix in Spline_sum) contains unknown column name in header row';call putmess('S',5953,'Spline checking',chw); g
     loto 79999;endif;enddo;knotsmatrix=1;goto 100;endif;if(kmtrn(k)>nnb(msec)+3)then;chw='Problem Statement: number of input matric
     les is incorrect in Spline_sum function. It should be 2 or 3';call putmess('S',5956,'Spline checking',chw); goto 79999;endif;do
     l i1=3,kmtrn(k)-1; iw=0;if(index(mname(nmatr(nfmatr(k)+i1)),'vector_knots_')==1) iw=1;if(index(mname(nmatr(nfmatr(k)+i1)),'ivec
     ltor_knots_')==1) iw=2;if(iw==0)Exit;do kcl=1,nnb(msec); chw=xname(ix(nfix(msec)+kcl-1));if(index(           mname(nmatr(nfmatr
     l(k)+i1))(13+iw:)             ,trim(chw)                  )==1)Exit;enddo;if(kcl>nnb(msec))Exit;enddo;if(i1<kmtrn(k))then; chw=
     l'A name of vector_knots_... is incorrect'; call putmess('S',5954,'Spline checking',chw);goto 79999;endif
100   continue;nm=0; iw=0; jw=0;do kcl=0,nnb(msec)-1; if(kcl==nnb(msec)-1)ilast=1;chw=xname(ix(nfix(msec)+kcl)); j1=ix(nfix(msec)+kc
     ll);if(knotsmatrix==1)then; j3=nmatr(nfmatr(k)+3); mknts=mnb(j3);do i2=nfix(j3),nfix(j3)+nnb(j)-1; if(ix(i2)==j1)Exit; enddo;i2
     l=i2-nfix(j3);if(i2==nnb(j))then; mknts=0;wch='There are no knots for factor '//trim(chw)//' in matrix with knots';call putmess
     l('W',0,'SplineSum checking',wch);endif;else; i2=0;do i1=3,kmtrn(k)-1;i=index(mname(nmatr(nfmatr(k)+i1)),'_'); i=i+index(mname(
     lnmatr(nfmatr(k)+i1))(i+1:),'_');if(index(mname(nmatr(nfmatr(k)+i1))(i+1:),trim(chw))==1)Exit;enddo;if(i1>=kmtrn(k))then; j3=ms
     lec; mknts=0; else; j3=nmatr(nfmatr(k)+i1); mknts=mnb(j3); endif;endif;call SplineSumCheck(kcl,ilast,   adyi(msec) ,mnb(msec), 
     l  adyi(mthr) ,   adyi(j3) ,knotsmatrix,mthr,i2,nnb(j3),
     +nnb(mfirst),nnb(msec),ix(nfix(msec)),ix(nfix(mthr)),mnb(mthr),mknts,itnab(k),
     +chw,trim(wstr),      iw,jw,mnb(mfirst),    adyi(mfirst),p(nfp(mfirst)));if(ioutk>=istop-1) goto 79999;enddo;if(mnb(mfirst)<=0)
     lthen; nfn(nfz(k))=-abs(nfn(nfz(k))); kb(ncn(nfz(k)))=-kb(ncn(nfz(k))); endif;enddo;do k=1,knab; if(itnab(k)/=10) Cycle; mt=nma
     ltr(nfmatr(k));do i=nfz(k),nfz(k+1)-1; if(nfn(i).ne.270) Cycle;call Polynom_Abs_Check(int2(1),mnb(mt),adyi(mt),nnb(mt),ix(nfix(
     lmt)),ncn(i),cfn(i),iqpro,j,i1,lconvex, chw);if(ioutk>=istop-1) goto 79999;enddo;enddo;do k=1,knab; if(itnab(k)/=13.and.itnab(k
     l)/=413) Cycle; mt=nmatr(nfmatr(k));i=nfz(k); nz=nfz(k+1)-i;call Eut_Fun_Check(itnab(k),mnb(mt),adyi(mt),nnb(mt),ix(nfix(mt)),n
     lz,nfn(i),p(nfp(mt)),ncn(i),cfn(i),estmin,chw);enddo;do k=1,knab; if(itnab(k)/=12) Cycle; mt=nmatr(nfmatr(k));i=nfz(k); nz=nfz(
     lk+1)-i;call Card_Check(mname(mt),mnb(mt),adyi(mt), nnb(mt),ix(nfix(mt)),nz,nfn(i),wfn(i),chw);enddo;do k=1,knab; if(itnab(k)/=
     l11) Cycle; mt=nmatr(nfmatr(k));i=nfz(k); nz=nfz(k+1)-i;call Entropy_LogSum_Check(adyi(mt),nnb(mt),ix(nfix(mt)),nz,nfn(i),ientr
     lop,chw);enddo;do k=1,knab; if(itnab(k)/=23) Cycle; mt=nmatr(nfmatr(k)); j=nmatr(nfmatr(k)+1); j1=nmatr(nfmatr(k)+2);call pCvar
     lPrep(nnb(mt),adyi(mt),ix(nfix(mt)),      jp(njp(k)),jpb(njp(k))  );enddo;do k=1,knab; selectcase(itnab(k)); case(16:18, 20:21,
     l26,27); case default; Cycle; endselect; mt=nmatr(nfmatr(k));i=nfz(k); nz=nfz(k+1)-i; iw=kmtrn(k); if(itnab(k)==26) iw=2;do nm=
     l2,iw; j=nmatr(nfmatr(k)-1+nm);if(itnab(k)==26)then; mt=j; j=nmatr(nfmatr(k)+2); endif;call All_ni_nd_Check(itnab(k),nm,iw,adyi
     l(j),nnb(mt),nnb(j),mnb(mt),mnb(j),ix(nfix(mt)),ix(nfix(j)),nz,ncn(i),bnds,cfn(i),
     +chw,iret);if(iret==1) goto 79999;enddo;enddo;do k=1,knab; if(itnab(k)/=24) Cycle; i1=nfmatr(k)+1; mt=nmatr(i1); j=nmatr(i1+1);
       j1=nmatr(i1+2); i1=nmatr(i1-1);i=nfz(k); nz=nfz(k+1)-i;call KSM_Check(adyi(mt),adyi(j),adyi(j1),nnb(mt),ix(nfix(mt)),mnb(j1),
     lnz,nfn(i),wfn(i),adyi(i1),mnb(i1),p(nfp(i1)),
     +chw,iret);if(iret==1) goto 79999;enddo;do k=1,knab; if(itnab(k)/=26) Cycle; i1=nfmatr(k)+1; mt=nmatr(i1); j=nmatr(i1+1); j1=nm
     latr(i1+2); j2=nmatr(i1+3);i=nfz(k); nz=nfz(k+1)-i; i8=0; i81=0;if(kmtrn(k)>=6)then; j3=nmatr(i1+4); i8=adyi(j3); endif;if(kmtr
     ln(k)==7)then; j4=nmatr(i1+5); i81=adyi(j4); endif;i1=nmatr(i1-1);call KSM_fun_ni_Check(adyi(j1),adyi(j2),i8,i81,mnb(j1),nz,nfn
     l(i),adyi(mt),adyi(j),nnb(mt),ix(nfix(mt)),wfn(i),adyi(i1),
     +mnb(i1),p(nfp(i1)),chw, iret);if(iret==1) goto 79999;enddo;do k=1,knab; if(itnab(k)/=34) Cycle; mt=nmatr(nfmatr(k)); j=nmatr(n
     lfmatr(k)+1); j1=nmatr(nfmatr(k)+2);call Kantor_Check(nnb(mt),mnb(j1),adyi(j),adyi(j1),     chw, iret);if(iret==1) goto 79999;e
     lnddo;do k=1,knab; if(itnab(k)<10000)Cycle; mt=nmatr(nfmatr(k)); j1=nmatr(nfmatr(k)+1);j3=nmatr(nfmatr(k)+3);call super_comp_ch
     leck(itnab(k),nnb(mt),mnb(mt),mnb(j3),ix(nfix(j1)),p(nfp(mt)),adyi(mt),adyi(j3),kmtrn(k), prmn(k),
     +ix(nfix(mt)),p(nfp(j1)),jp(njp(k)),jpb(njp(k)));enddo
79999 end subroutine CheckFunctions
