      subroutine GetMultiCutsGrads(list,nbz,nbj, itt,m,n,n1,n2,x,ix,yi,cf1,fw,nc,mget,maxmem,   ls,klin,intarr,dparr,kac,    nci);in
     jteger(4) ls,list(*),nbz(*),nbj(*),m,n,n1,n2,itt, ix(0:*),mget(*),maxmem, klin,intarr(*),kac(*),nc,nci(*);real(8) x(0:*),yi(0:n
     a,0:*),cf1,fw,dparr(*);real(8) w,ww; logical sp_out; integer(4) j,i,km,k,kel,iam,kk,el,m1,iret;integer(4),allocatable:: is(:),j
     js(:),    iw(:);real(8),allocatable::am(:,:),as(:), wm(:),xw(:);character(128) chw;km=0; j=ls; k=nbz(list(ls));do while(nbz(lis
     vt(j))==k); km=km+1; j=j+1; enddo;call SpMatrixAddrs(yi,yi,m,n, sp_out,i);if(sp_out)then; call SpMatrixKelm(kel); else; kel=(n+
     d1)*km; endif;iam=0; kel=kel+km;allocate(am(1,1),as(kel+2),js(kel+2),is(0:kel+2),  wm(0:n),iw(0:n), stat=i);if(i/=0) goto 90;am
     p=0.; is=n2+1; is(0)=0; kel=0;if(itt>=305.and.itt<=307)then; allocate(xw(0:n));do i=0,n; j=ix(i); if(j==0)then; xw(i)=x(j); els
     ee; xw(i)=-x(j); endif;enddo;call SpMatrixKcut(m,m1);endif;ww=cf1/fw; i=klin;do while(nbz(list(ls))==k); j=nbj(list(ls)); i=i+1
        nci(i-klin)=isign(nc,j);if(j<0)then; j=-j; w=-ww; else; w=ww; endif;if(itt<305.or.itt>307)then;if(sp_out)then; call SpM_GetS
     npRow(j,0,n, wm,iw,kk);do el=0,kk-1; kel=kel+1; js(kel)=i; is(kel)=ix(iw(el)); as(kel)=-wm(el)*w; enddo;else;do el=0,n; if(yi(e
     bl,j)==0.)Cycle; kel=kel+1; js(kel)=i; is(kel)=ix(el); as(kel)=-yi(el,j)*w; enddo;endif;else; wm=0.;call MultiQuadrGradSmall(xw
     u,sp_out,j,m1,n,mget,w,yi,   wm);do el=0,n; if(wm(el)==0.)Cycle; kel=kel+1; js(kel)=i; is(kel)=ix(el); as(kel)=wm(el); enddo;en
     edif;kel=kel+1; js(kel)=i; is(kel)=n2-1; as(kel)=-1.;ls=ls+1;enddo;call insert_M_constr(maxmem,n2,n1,m,iam,am,kel,as,is,js,klin
     s+1,intarr,dparr,kac,iret);if(iret==1) goto 90;klin=klin+km;goto 79999
90    chw='Internal error: Cannot allocate memory'; call putmess('S',5910,'GetMultiCutsGrads',chw); goto 79999
79999 deallocate(am,as,js,is,  wm,iw, stat=i);if(allocated(xw))deallocate(xw);return;end subroutine GetMultiCutsGrads;subroutine  Mu
     xltiQuadrGrad(ix,x,sp_out,nq,m0,n,mget,w1,yi,   grad);integer(4) ix(0:*),nq,m0,n,mget(*); logical sp_out;real(8) x(0:*),w1,yi(0
     k:n,0:*),grad(0:*);integer(4) i,j,m; real(8) xw(0:n),ww;do i=0,n; j=ix(i); if(j==0)then; xw(i)=x(j); else; xw(i)=-x(j); endif;e
     jnddo;call SpMatrixKcut(m0,m);if(.not.sp_out)then;do j=(nq-1)*m+1,nq*m; i=ix(mget(j));grad(i)=grad(i)+w1*dot_product(yi(:,j),xw
     n);ww=w1*xw(mget(j));do i=0,n; grad(ix(i))=grad(ix(i))+ww*yi(i,j); enddo;enddo;else;do j=(nq-1)*m+1,nq*m; i=ix(mget(j));call Sp
     vM_RowVect(m,n,j,xw, ww);grad(i)=grad(i)+w1*ww;call SpM_GradAddRow(j,w1*xw(mget(j)),ix,  grad);enddo;endif;end subroutine Multi
     cQuadrGrad;subroutine MultiQuadrGradSmall(xw,sp_out,nq,m,n,mget,w1,yi,   g);integer(4) nq,m,n,mget(*); logical sp_out;real(8) x
     fw(0:*),w1,yi(0:n,0:*),g(0:*);integer(4) i,j; real(8) ww;if(.not.sp_out)then;do j=(nq-1)*m+1,nq*m; i=mget(j);g(i)=g(i)+w1*dot_p
     product(yi(:,j),xw(0:n));ww=w1*xw(mget(j));do i=0,n; g(i)=g(i)+ww*yi(i,j); enddo;enddo;else;do j=(nq-1)*m+1,nq*m; i=mget(j);cal
     ll SpM_RowVect(m,n,j,xw, ww);g(i)=g(i)+w1*ww;call SpM_AddRow(j,w1*xw(mget(j)),  g);enddo;endif;end subroutine MultiQuadrGradSma
     ill;subroutine calc_grad_i(itg0,knab,nfz,itnab,kmtrn,nmatr,amatr,nfmatr,mnb,yi,adyi,n1,nnb,ix,nfix,jp,njp,jpb,nfn,wfn,ncn,cfn,i
     psg,kzp,polka,klast,iVarDop,mget,nfget,jmax,jmin,fw,lnrz,x,p,nfp,gst,ngst,avg,st0,nfn0,g2,fm,pf,chw,knab1,nnab,nfz1,kf0,nfn1,al
     pp,ncn1,cfn1,ifp,ib1,nfib);use ModCommons;integer(2) itg0;    integer(plen) adyi(*);integer(4) knab,n1,knab1,  nfz(*),itnab(*),
     pkmtrn(*),nmatr(*),nfmatr(*),mnb(*),nnb(*),kzp,ix(*),nfix(*),jp(*),jpb(*),njp(*),nfn(*),ncn(*),isg(0:*),klast(*),iVarDop(*),mge
     at(*),nfget(*),jmin(*),jmax(*),lnrz(*),nfp(*),ngst(*),nnab(*),nfz1(*),kf0(*),nfn1(*),ncn1(*),ifp(*),ib1(*),nfib(*),nfn0(*);real
     a(8) yi(*),wfn(*),cfn(*),polka(*),fw(*),x(*),p(*),gst(*),fm(*),pf(*),avg(*),st0(*),alp(*),cfn1(*),g2(*),amatr(*);character(*) c
     nhw;integer(4)  i,j,j1,k,nz,mt,mfirst,kf12,itt,kmt,msec,nscen,iw,i1,nz1,mcut,icut,mt2,nm,mt1,mt3,m;integer(4),allocatable:: ifC
     f(:),ibC(:);   real(8),allocatable:: alC(:),pw(:); real(8) w;integer(1),allocatable:: iscen(:);logical,external:: isAvgMulti,is
     aCVaRMulti,isPm2Max;interface;subroutine Prmulti_pen_ni_Grad(m,n,n1,x, yim,yis,ixm,ixs, nz,nf,wf,nc,cf, isg,fw, gst,g,chw);use 
     mcifort; integer(plen):: yim,yis; integer(4) m,n,n1,nz,ixm(0:n),ixs(0:n),nf(*),nc(*),isg(0:*)
#ifdef __APPLE__
  !gcc$ attributes value:: yim,yis
#else
      value:: yim,yis
#endif
      real(8) x(0:n1),wf(*),cf(*),fw(*),g(0:n1,0:*); character(*) chw; real(8),target:: gst(*);end;subroutine AllFunc_nid_Grad(m,n,n
     k1,x, yim,ix, nz,nf,wf,nc,cf, isg,fw, st0,gst0,p,g,chw);use cifort; integer(plen):: yim; integer(4) m,n,n1,nz,ix(0:n),nf(*),nc(
     b*),isg(0:*)
#ifdef __APPLE__
  !gcc$ attributes value:: yim
#else
      value:: yim
#endif
      real(8) x(0:n1),wf(*),cf(*),p(*),st0,fw(*),g(0:n1,0:*); character(*) chw; real(8),target:: gst0(0:*);end;subroutine wCvar_ni_G
     erad(n,n1,x,yim,yis,ix, nz,nf,wf,nc,cf, isg,fw,gst,polka,g,chw);use cifort; integer(plen):: yim,yis; integer(4) n,n1,nz,ix(0:n)
     c,nf(*),nc(*),isg(0:*)
#ifdef __APPLE__
  !gcc$ attributes value:: yim,yis
#else
      value:: yim,yis
#endif
      real(8) x(0:*),wf(*),cf(*),polka(*),fw(*),g(0:n1,0:*); character(*) chw; real(8),target:: gst(*);end;subroutine AvgFunc_ni_Gra
     xd(m,n,n1,x, yim,yis,p,ixm,ixs, nz,nf,wf,nc,cf,iVarDop, isg,fw, gst,g,chw);use cifort; integer(plen):: yim,yis; integer(4) m,n,
     qn1,nz,ixm(0:n),ixs(0:n),nf(*),nc(*),iVarDop(*),isg(0:*)
#ifdef __APPLE__
  !gcc$ attributes value:: yim,yis
#else
      value:: yim,yis
#endif
      real(8) x(0:n1),wf(*),cf(*),fw(*),g(0:n1,0:*),p(*); character(*) chw; real(8),target:: gst(*);end;subroutine Pr_ND_Grad(nm,m,n
     b,n1, yim,ix, nz,nf,wf,nc, isg,fw, gst,g,chw);use cifort; integer(plen):: yim; integer(4) n,n1,nz,ix(0:n),nf(*),nc(*),nm,m,isg(
     s0:*)
#ifdef __APPLE__
  !gcc$ attributes value:: yim
#else
      value:: yim
#endif
      real(8) wf(*), fw(*),g(0:n1,0:*); character(*) chw; real(8),target:: gst(*);end;subroutine Ro_err_Grad(m,n,n1, yi,ix, nz,nf,nc
     m,cf, isg,fw, gst,klast,list,p,g,chw);use cifort; integer(plen):: yi; integer(4) m,n,n1,nz,ix(0:*),nf(*),nc(*),list(*),klast(*)
     k,isg(0:*)
#ifdef __APPLE__
  !gcc$ attributes value:: yi
#else
      value:: yi
#endif
      real(8) cf(*),p(*),fw(*),g(0:n1,0:*); character(*) chw; real(8),target:: gst(*);end;subroutine cvar2_Grads(m,n,n1, yi,p,ix, nz
     n,nf,wf,nc,cf, isg,fw, gst,polka,kzp,klast,list,g,chw);use cifort; integer(plen):: yi; integer(4) m,n,n1,nz,kzp,ix(0:*),nf(*),n
     uc(*),list(*),klast(*),isg(0:*)
#ifdef __APPLE__
  !gcc$ attributes value:: yi
#else
      value:: yi
#endif
      real(8) wf(*),cf(*),fw(*),g(0:n1,0:*),p(*); character(*) chw; real(8),target:: gst(*),polka(kzp,*);end;subroutine CalcScenGrad
     ls(chyi,chw,m,n,ix,x,jp,jpb,nz,nf,nc,isg,klast,jmax,jmin,p,avg,pf,iscen0,nscen, gst0,yi);use CiFort; integer(plen):: chyi,yi; c
     lharacter(*) chw
#ifdef __APPLE__
  !gcc$ attributes value:: chyi,yi
#else
      value:: chyi,yi
#endif
      integer(4) m,n,jmax,jmin,nz,ix(0:*),jp(0:*),jpb(0:*),nf(*),nc(*),klast(*),isg(0:*),nscen;real(8) avg,x(0:*),p(*),gst0(0:*),pf(
     p*); integer(1) iscen0(m);end;integer(4) function MCutGet(m,n,yi); use CiFort; integer(plen):: yi; integer(4) m,n
#ifdef __APPLE__
  !gcc$ attributes value:: yi
#else
      value:: yi
#endif
      end;subroutine Grad_Draw_Mult(m,mfull,yi,n1,n,ix,jp,jpb,nz,nf,wf,nc,cf,g,isg,polka0,klast0,mget,nmatr,kmatr,jmax,jmin,fw);use 
     ncifort; integer(plen):: yi; integer(4) m,mfull,n1,n,nz,mget(*),nmatr,kmatr,jmax,jmin,klast0(nz),isg(0:*)
#ifdef __APPLE__
  !gcc$ attributes value:: yi
#else
      value:: yi
#endif
      integer(4) ix(0:n),jp(0:*),jpb(0:*),nf(nz),nc(nz); real(8) g(0:n1,0:*),cf(nz),wf(nz),fw(*),polka0(nz);end;subroutine GradFuncs
     eL1L2(itt,iVarDop,n12, m1,m2,n1,n2,yi1,yi2,ix1,ix2, nz,nf,wf,nc,cf,jpb1,chw,fw,gst,polka,mget,isg,g);use CiFort; integer(plen):
     b: yi1,yi2; integer(4),target:: jpb1(0:*)
#ifdef __APPLE__
  !gcc$ attributes value:: yi1,yi2
#else
      value:: yi1,yi2
#endif
      integer(4) itt,n12,iVarDop(*),m1,m2,n1,n2,ix1(0:*),ix2(0:*),nz,nf(*),nc(*),isg(0:*),mget(*);real(8) wf(*),cf(*),polka(*),g(0:n
     x12,0:*),fw(*); real(8),target::gst(*); character(*) chw;end;subroutine Card_GradCalc(lnrz,m,yi,n1,n,x,ix,nz,nf,wfm,nc,cf,chw,g
     s,isg,fw);use CiFort; integer(plen):: yi; integer(4) lnrz(0:*),m,n1,n,nz, ix(0:*),nf(*),nc(*); character(*) chw
#ifdef __APPLE__
  !gcc$ attributes value:: yi
#else
      value:: yi
#endif
      real(8) x(0:*),wfm(*),cf(*),g(0:n1,0:*),fw(*); integer(4) isg(0:*);end;subroutine Eut_Grad_(m,yi,n1,n,x,ix,nz,nf,wf,nc,cf,p, i
     wsg,fw,gst,chw,g,fm,pf);use CiFort; integer(plen):: yi; integer(4) m,n1,n,ix(0:n),nz,nf(nz),nc(nz),isg(0:*); character(*) chw
#ifdef __APPLE__
  !gcc$ attributes value:: yi
#else
      value:: yi
#endif
      real(8) p(m),wf(nz),cf(nz), x(0:n1),fw(*),g(0:n1,0:*), fm(*), pf(*), gst(0:*);end;subroutine ExtFunc_Grad_(chyi,n1,n,x,ix,nz,n
     xf,nc,cf, isg,fw,chw,g, ln);use CiFort; integer(plen):: chyi; integer(4):: ln; integer(4) n1,n,ix(0:n),nz,nf(*),nc(nz),isg(0:*)
      
#ifdef __APPLE__
  !gcc$ attributes value:: chyi,ln
#else
      value:: chyi,ln
#endif
      real(8) cf(nz),x(0:n1),fw(*),g(0:n1,0:*); character(*) chw;end;subroutine HMM_Gradient(n1,mname,mv,vy,nvars,x,ix,nz,nc,cf,wf,w
     pDegFmax,isg,fw,g);use CiFort; integer(plen):: vy; integer(4) nvars,mv,ix(0:*),nz,nc(*),n1,isg(0:*)
#ifdef __APPLE__
  !gcc$ attributes value:: vy
#else
      value:: vy
#endif
      real(8) x(0:*),cf(*),wf,wDegFmax,g(0:n1,0:*),fw(*); character(*) mname;end;subroutine Grad_CVaRs_Mult(lnrz,m,yi,p,n1,n,ix,jp,j
     rpb,g,isg,kj1,nz,nfn1,alp,ncn1,cfn1,ifp,ib1,mget,nmatr0,fw);use CiFort; integer(plen):: yi; integer(4) m,n1,n,nz,kj1,ib1(m),mge
     et(m),nmatr0
#ifdef __APPLE__
  !gcc$ attributes value:: yi
#else
      value:: yi
#endif
      integer(4) ix(0:n),jp(0:m+1),jpb(0:m+1),nfn1(nz),ncn1(nz),ifp(nz),lnrz(0:*),isg(0:*);real(8) p(m),g(0:n1,0:*),alp(nz),cfn1(nz)
     t,fw(*);end;subroutine Grad_Avg_Max(chw,mget,nmatr,m,yi,p,n1,n,ix, nz,nf,nc,cf,g,  isg, fw);use CiFort; integer(plen):: yi; int
     meger(4) m,n1,n,ix(0:*),nz, nf(*),nc(*),mget(*),nmatr,isg(0:*)
#ifdef __APPLE__
  !gcc$ attributes value:: yi
#else
      value:: yi
#endif
      real(8) p(*),cf(*),g(0:n1,0:*),fw(*); character(*) chw;end;subroutine GRAD_10_100_MULTI(chw,m0,yi,p,n1,n,ix,jp,jpb,mget,nmatr,
     hnz,nf,wf,nc,cf,g,isg,kzp,polka,klast,jmax,jmin,gst0,fw);use CiFort; integer(plen):: yi; character(*) chw
#ifdef __APPLE__
  !gcc$ attributes value:: yi
#else
      value:: yi
#endif
      integer(4) m0,n1,n,jmax,jmin,nz,nmatr,ix(0:*),jp(0:*),jpb(0:*),nf(*),nc(*),klast(*),isg(0:*),mget(*),kzp;real(8) p(*),wf(*),cf
     l(*),g(0:n1,0:*),polka(nz),fw(*); real(8),target:: gst0(0:*);end;subroutine GradCvarsL1L2forVarPr(itt,iVarDop,n12,m1,m2,n1,n2,y
     ni1,yi2,ix1,ix2,nz,nf,nc,cf,jpb1,chw,fw,st0,nfn0,gst,polka,mget,isg,g);use CiFort; integer(plen):: yi1,yi2; character(*) chw
#ifdef __APPLE__
  !gcc$ attributes value:: yi1,yi2
#else
      value:: yi1,yi2
#endif
      integer(4) itt,iVarDop(*),n12,m1,m2,n1,n2,ix1(0:*),ix2(0:*),nz,nf(*),nc(*),isg(0:*),mget(*),nfn0(*);integer(4),target:: jpb1(0
     n:*); real(8) cf(*),polka(*),g(0:n12,0:*),fw(*),st0; real(8),target::gst(*);end;subroutine Grad_CVaRs(itt,lnrz,m,yi,p,n1,n,ix,j
     vp,jpb,g,isg, kj1,nz,nfn1,alp,ncn1,cfn1,ifp,ib1,fw);use CiFort; integer(plen):: yi; integer(4) itt,m,n1,n,nz; real(8) p(m),g(0:
     wn1,0:*),alp(nz),cfn1(nz),fw(*)
#ifdef __APPLE__
  !gcc$ attributes value:: yi
#else
      value:: yi
#endif
      integer(4) ix(0:n),jp(0:m+1),jpb(0:m+1),nfn1(nz),ncn1(nz),ifp(nz),lnrz(0:*),kj1,ib1(m),isg(0:*);end;subroutine GRAD_ALL_abs(ch
     sw,itt,m,yi,p,n1,n,ix,x,jp,jpb,  mget,  amatr,nz,nf,wf,nc,cf,g,  isg,polka,klast,iVarDop,jmax,jmin,avg,st0,gst0, fw);use CiFort
        integer(plen):: yi; real(8),target:: gst0(0:*); character(*) chw
#ifdef __APPLE__
  !gcc$ attributes value:: yi
#else
      value:: yi
#endif
      integer(4) itt,m,n1,n,jmax,jmin,nz,ix(0:*),jp(0:*),jpb(0:*),nf(*),nc(*),klast(*),iVarDop(*),isg(0:*),mget(*);real(8) p(*),wf(*
     w),cf(*),g(0:n1,0:*),polka(nz),avg,st0,fw(*),x(0:*),amatr;end;subroutine GRAD_ALL(chw,itt,m,yi,p,n1,n,ix,x,jp,jpb,mget,nz,nf,wf
     m,nc,cf,g,  isg,kzp,polka,klast,iVarDop,jmax,jmin,avg,st0,gst0, fw);use cifort; integer(plen):: yi;
#ifdef __APPLE__
  !gcc$ attributes value:: yi
#else
      value:: yi
#endif
      integer(4) itt,m,n1,n,jmax,jmin,nz,ix(0:*),jp(0:*),jpb(0:*),nf(*),nc(*),klast(*),iVarDop(*),isg(0:*),mget(*),kzp;real(8) p(*),
     jwf(*),cf(*),g(0:n1,0:*),polka(*),avg,st0,fw(*),x(0:*); real(8),target:: gst0(0:*); character(*) chw;end;end interface;w=yi(1);
      chw=''; if(itg0==10) chw='itg0==10'; if(itg0==-111)chw='cardzero';do k=1,knab; itt=itnab(k);if(chw=='itg0==10'.and.itt>=0) Cyc
     ple;mt=nmatr(nfmatr(k)); kmt=kmtrn(k);i=nfz(k); nz=nfz(k+1)-i; mfirst=mt;if(itt==202)then; msec=nmatr(nfmatr(k)+1);allocate(isc
     nen(mnb(mt))); iscen=0; nscen=0;do j=1,knab1; if(nnab(j)/=k)Cycle;iw=nfz1(j+1)-1;do i1=nfz1(j),iw; if(nfn1(i1)>=0)then; if(ncn1
     i(i1)==0.or.isg(ncn1(i1))/=0) Exit; endif; enddo;if(i1>iw) Cycle;i1=nfz1(j); nz1=nfz1(j+1)-i1;call WhichScenCvars(lnrz, mnb(mt)
     v, p(nfp(mt)),jp(njp(k)),jpb(njp(k)),isg,kf0(j),   nz1,nfn1(i1),alp(i1),ncn1(i1),ifp(i1),ib1(nfib(j)),    iscen,nscen);enddo;ca
     zll CalcScenGrads(adyi(msec),chw,mnb(mt),nnb(msec),ix(nfix(msec)),x,jp(njp(k)),jpb(njp(k)),nz,nfn(i),ncn(i),isg,klast(i),jmax(k
     l),jmin(k),p(nfp(mt)),avg(k),pf,iscen,nscen,    gst(ngst(k)),adyi(mt));deallocate(iscen);endif;iw=nfz(k+1)-1;do i1=nfz(k),iw; i
     of(nfn(i1)>=0)then; if(ncn(i1)==0.or.isg(ncn(i1))/=0) Exit; endif; enddo;if(i1>iw) Cycle;select case(itt);case(8:9, 308:309); m
     tcut=MCutGet(mnb(mt),nnb(mt),adyi(mt));icut=0; if(mcut<mnb(mt))then; icut=1; kmt=mnb(mt)/mcut; endif;do j=1,kmt; if(icut==0)the
     tn; mt=nmatr(nfmatr(k)-1+j); mcut=mnb(mt); endif;call Grad_Draw_Mult(mcut,mnb(mt),adyi(mt),n1,nnb(mt),ix(nfix(mt)),jp(njp(k)),j
     spb(njp(k)),nz,nfn(i),wfn(i),ncn(i),cfn(i),g2, isg, polka(i),klast(i),mget(nfget(k)), j, kmt, jmax(k),jmin(k), fw  );enddo;case
     a(450);call GradTSPCuts(n1,nnb(mt),ix(nfix(mt)), nz,ncn(i),cfn(i), isg,fw, gst(ngst(k)),chw,   g2);case(34);call GradKantor (n1
     l,nnb(mt),ix(nfix(mt)), nz,ncn(i),cfn(i), isg,fw, gst(ngst(k)),chw,   g2);case(500:599); mt2=nmatr(nfmatr(k)+1);call GradFuncsL
     u1L2(itt,iVarDop(i),n1, mnb(mt),mnb(mt2),nnb(mt),nnb(mt2),adyi(mt),adyi(mt2),ix(nfix(mt)),ix(nfix(mt2)), nz,nfn(i),wfn(i),ncn(i
     z),cfn(i),jpb(njp(k)),chw,fw,gst(ngst(k)),polka(i),mget(nfget(k)),isg,         g2);case(12);call Card_GradCalc(lnrz,mnb(mt),ady
     ti(mt),n1,nnb(mt),x,ix(nfix(mt)), nz,nfn(i),wfn(i),ncn(i),cfn(i),chw,g2,   isg, fw );case(13);call Eut_Grad_(mnb(mt),adyi(mt),n
     w1,nnb(mt),x,ix(nfix(mt)), nz,nfn(i),wfn(i),ncn(i),cfn(i),p(nfp(mt)),   isg,fw, gst(ngst(k)),chw,   g2, fm, pf );case(15);j=8*(
     q1+mnb(mt))*(1+nnb(mt));call ExtFunc_Grad_(adyi(mt), n1,nnb(mt),x,ix(nfix(mt)),nz,nfn(i),ncn(i),cfn(i),isg,fw,chw,  g2  ,j);cas
     fe(16);j=nmatr(nfmatr(k)+1);call Prmulti_pen_NI_Grad(mnb(mt),nnb(mt),n1,x,adyi(mt),adyi(j),ix(nfix(mt)),ix(nfix(j)),nz,nfn(i),w
     tfn(i),ncn(i),cfn(i),  isg,fw,gst(ngst(k)),  g2, chw);case(17,21);call AllFunc_nid_Grad(mnb(mt),nnb(mt),n1,x,adyi(mt),ix(nfix(m
     it)),nz,nfn(i),wfn(i),ncn(i),cfn(i),  isg,fw,st0(k),gst(ngst(k)),  p(nfp(mt)), g2, chw);case(27); j=nmatr(nfmatr(k)+1);call wCv
     kar_ni_Grad(nnb(mt),n1,x,adyi(mt),adyi(j),ix(nfix(mt)),nz,nfn(i),wfn(i),ncn(i),cfn(i),  isg,fw,gst(ngst(k)),polka(i),  g2, chw)
       case(18);if(kmt>1)then; j=nmatr(nfmatr(k)+1);call AvgFunc_ni_Grad(mnb(mt),nnb(mt),n1,x,adyi(mt),adyi(j),p(nfp(mt)),ix(nfix(mt
     s)),ix(nfix(j)),nz,nfn(i),wfn(i),ncn(i),cfn(i),iVarDop(i),  isg,fw,gst(ngst(k)),  g2, chw);endif;case(20);do nm=2,kmt;call Pr_N
     cD_Grad(nm,mnb(mt),nnb(mt),n1,adyi(mt),ix(nfix(mt)),nz,nfn(i),wfn(i),ncn(i),isg,fw,gst(ngst(k)),  g2,chw);enddo;case(25);call R
     ro_err_Grad(mnb(mt),nnb(mt),n1, adyi(mt),ix(nfix(mt)),nz, nfn(i),ncn(i),cfn(i),  isg,fw,gst(ngst(k)),  klast(i),jp(njp(k)),p(nf
     kp(mt)),   g2,  chw);case(29);mt2=nmatr(nfmatr(k)+1);CALL HMM_Gradient(n1,'',mnb(mt2),adyi(mt2),nnb(mt),x,ix(nfix(mt)),nz,ncn(i
     v),cfn(i),wfn(i),st0(k),isg,fw,   g2);case(30);call cvar2_Grads(mnb(mt),nnb(mt),n1, adyi(mt),p(nfp(mt)),ix(nfix(mt)),nz, nfn(i)
     g,wfn(i),ncn(i),cfn(i),  isg,fw,gst(ngst(k)),polka(i),kzp,klast(i),jp(njp(k)),      g2, chw);case(200,201,202); j=nfmatr(k)-1; 
      mt1=nmatr(j+1); mt3=nmatr(j+3); mt2=mt1;if(itt==201)then; mt2=mt3; mt3=mt1; endif;if(itt==202)then; mt3=mt1; endif;CALL GRAD_A
     aLL(chw,itt,mnb(mt3),adyi(mt2),p(nfp(mt3)),n1,nnb(mt1),ix(nfix(mt1)),x,jp(njp(k)),jpb(njp(k)),mget(nfget(k)),nz,nfn(i),wfn(i),n
     tcn(i),cfn(i),g2, isg,kzp,polka(i),klast(i),iVarDop(i),jmax(k),jmin(k),avg(k),st0(k),gst(ngst(k)),fw );case(19); mt1=nmatr(nfma
     ttr(k)+1);CALL GRAD_ALL(chw,itt,mnb(mt1),adyi(mt1),p(nfp(mt1)),n1,nnb(mt1),ix(nfix(mt1)),x,jp(njp(k)),jpb(njp(k)),mget(nfget(k)
     z),nz,nfn(i),wfn(i),ncn(i),cfn(i),g2, isg,kzp,polka(i),klast(i),iVarDop(i),jmax(k),jmin(k),avg(k),st0(k),gst(ngst(k)),fw );case
     z(100:103); mt1=nfmatr(k);nm=mnb(mt);CALL GRAD_ALL_abs(chw,itt, nm,  adyi(mt),p(nfp(mt)),n1,nnb(mt),ix(nfix(mt)),x,jp(njp(k)),j
     jpb(njp(k)),mget(nfget(k)),amatr(mt1),nz,nfn(i),wfn(i),ncn(i),cfn(i),g2, isg,polka(i),klast(i),iVarDop(i),jmax(k),jmin(k),avg(k
     j),st0(k),gst(ngst(k)),fw );case default;if(itt<=3.and.kmt>1 .or. 300<=itt.and.itt<=303)then;if(isAvgMulti(nz,ncn(i),nfn(i),isg
     w))then;do j=1,kmt; mt=nmatr(nfmatr(k)-1+j);CALL Grad_Avg_Max(chw,mget(nfget(k)),j,mnb(mt),adyi(mt),p(nfp(mfirst)),n1,nnb(mt),i
     lx(nfix(mt)),nz,nfn(i),ncn(i),cfn(i),g2, isg, fw );enddo;Cycle;endif;if(isCVaRMulti(nz,ncn(i),nfn(i),isg))then;allocate(alC(nz)
     i,ifC(nz),ibC(mnb(mt)));call FormInputforCVaRMulti(nz,nfn(i),wfn(i),mnb(mt),  kf12,alC,ifC,ibC);do j=1,kmt; mt=nmatr(nfmatr(k)-
     n1+j);call Grad_CVaRs_Mult(lnrz,mnb(mt),adyi(mt),p(nfp(mfirst)),n1,nnb(mt),ix(nfix(mt)),jp(njp(k)),jpb(njp(k)),g2, isg,  kf12,n
     yz,nfn(i),  alC,   ncn(i),cfn(i),   ifC,   ibC,mget(nfget(k)), j, fw );enddo;deallocate(alC,ifC,ibC);Cycle;endif;if(isPm2Max(nz
     x,ncn(i),nfn(i),isg))then;call grad_pm2_max(chw,mnb(mt),p(nfp(mfirst)),n1,ix,jp(njp(k)),jpb(njp(k)),kmt,nfmatr(k),mget(nfget(k)
     u), nmatr, mnb,nnb,adyi,nfix,nz,nfn(i),wfn(i),ncn(i),cfn(i),g2, isg,polka(i),klast(i),gst(ngst(k)),fw);Cycle;endif;if(itt<=3.or
     v.300<=itt.and.itt<=303)then; mt1=nfmatr(k);do j=1,kmt; mt=nmatr(nfmatr(k)-1+j);CALL GRAD_10_100_MULTI(chw,mnb(mt),adyi(mt),p(n
     bfp(mfirst)),n1,nnb(mt),ix(nfix(mt)),jp(njp(k)),jpb(njp(k)),mget(nfget(k)), j,nz,nfn(i),wfn(i),ncn(i),cfn(i),g2, isg,kzp,polka(
     ai),klast(i),jmax(k),jmin(k),gst(ngst(k)),fw );enddo;endif;else;nm=mnb(mt); if(itt==14.or.itt==114) nm=nnb(mt);CALL GRAD_ALL(ch
     kw,itt,  nm,  adyi(mt),p(nfp(mt)),n1,nnb(mt),ix(nfix(mt)),x,jp(njp(k)),jpb(njp(k)),mget(nfget(k)),nz,nfn(i),wfn(i),ncn(i),cfn(i
     g),g2, isg,kzp,polka(i),klast(i),iVarDop(i),jmax(k),jmin(k),avg(k),st0(k),gst(ngst(k)),fw );endif;end select;enddo;do j=1,knab1
        k=nnab(j);iw=nfz1(j+1)-1;do i=nfz1(j),iw; if(nfn1(i)>=0)then; if(ncn1(i)==0.or.isg(ncn1(i))/=0) Exit; endif; enddo;if(i>iw) 
     vCycle;itt=itnab(k); kmt=kmtrn(k);i=nfz1(j); nz=nfz1(j+1)-i;mt=nmatr(nfmatr(k));m=mnb(mt); if(itt==14)m=nnb(mt);allocate(pw(m))
       if(itt==14)then; w=1d0/dfloat(m); pw=w; else; pw=p(nfp(mt):nfp(mt)+m-1); endif;select case(itt);case(500:599); mt2=nmatr(nfma
     utr(k)+1);call GradCvarsL1L2forVarPr(itt,iVarDop(i),n1, mnb(mt),mnb(mt2),nnb(mt),nnb(mt2),adyi(mt),adyi(mt2),ix(nfix(mt)),ix(nf
     dix(mt2)), nz,nfn1(i),      ncn1(i),cfn1(i),jpb(njp(k)),chw,fw,st0(k),nfn0(i),gst(ngst(k)),polka(i),mget(nfget(k)),isg,        
     d g2);case default;if(kmt>1.and.itt/=200.and.itt/=201.and.itt/=202) then; mfirst=nmatr(nfmatr(k));do j1=kmt,1,-1; mt=nmatr(nfma
     ptr(k)-1+j1);call Grad_CVaRs_Mult(lnrz,mnb(mt),adyi(mt),p(nfp(mfirst)),n1,nnb(mt),ix(nfix(mt)),jp(njp(k)),jpb(njp(k)),g2, isg, 
     f kf0(j),nz,nfn1(i),alp(i),ncn1(i),cfn1(i),ifp(i),ib1(nfib(j)),mget(nfget(k)), j1, fw );enddo;else;call Grad_CVaRs(itt,lnrz, m 
     z    ,adyi(mt), pw,n1,nnb(mt),ix(nfix(mt)),jp(njp(k)),jpb(njp(k)),g2,  isg,  kf0(j),nz,nfn1(i),alp(i),ncn1(i),cfn1(i),ifp(i),ib
     k1(nfib(j)), fw  );endif;end select;deallocate(pw);enddo;chw='';
      end subroutine calc_grad_i
