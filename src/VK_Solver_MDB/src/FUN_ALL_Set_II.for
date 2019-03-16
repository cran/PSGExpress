      subroutine calc_fi(fi,   k,nfz,kmtrn,itnab,prmn,nmatr,amatr,nfmatr,mnb,
     +yi,p,n1,nnb,x,ix,jmax,jmin,avg,mget,fm,pf,kzp,polka,klast,iVarDop,
     +nfn,wfn,ncn,cfn,bnds,jp,jpb,if2_10,if11_13,st0,gst,
     +adyi,nfp,nfix,nfget,njp,ngst,mname,chw,xwrk0);use ModCommons;integer(4)   k,n1,klast(*),iVarDop(*),nfn(*),ncn(*),kzp,
     +ix(*),nfz(*),kmtrn(*),itnab(*),nmatr(*),nfmatr(*),if11_13(*),
     +mnb(*),nnb(*),jmax(*),jmin(*),mget(*),jp(*),jpb(*),if2_10(*),
     +nfp(*),nfix(*),nfget(*),njp(*),ngst(*);    integer(plen) adyi(*),i8;real(8) fi(*),p(*),x(*),fm(*),pf(*),wfn(*),cfn(*),prmn(*),
     lamatr(*),
     +yi(*),avg(*),polka(*),bnds(*),st0(*),gst(*),xwrk0(*);character(*) mname, chw; logical,external:: lnot_Null;integer(4) i,j,nz,m
     l,mt,itt,kmt,mcut,mt1,mt4,mt2,mt3,nm;  real(8) w;real(8),allocatable::xwrk(:);interface;subroutine FM_Multi(m,yi,p,n1,n,x,ix,jm
     lax,jmin,avg,nmatr,kmatr,itt,mget,fm,pf,amatr); use cifort;integer(4) m,n1,n,nmatr,kmatr,itt,ix(0:n),mget(m),jmax,jmin;integer(
     lplen),value:: yi; real(8) p(m),x(0:*),avg,amatr(*),fm(*),pf(*);end;subroutine FM_abs(m,yi,p,n1,n,x,ix, jmax,jmin,avg,itt, mget
     l, fm,pf,amatr); use cifort;integer(plen),value:: yi; integer(4) m,n1,n,itt,ix(0:*),mget(m),jmax,jmin; real(8) p(*),x(0:*),avg,
     lfm(*),pf(*),amatr;end;subroutine FM_One_row_abs(yi,n,x,ix,jmax,jmin,avg,fm,pf,mget); use cifort;integer(plen),value:: yi; inte
     lger(4) n,ix(0:*),jmax,jmin,mget(*); real(8) x(0:*),fm(*),pf(*),avg;end;subroutine FM_Multi_Cuts(m,yi,p,n1,n,x,ix,itt,jmax,jmin
     l,avg,mget,fm,pf,mcut); use cifort;integer(plen),value:: yi; integer(4) m,n1,n,itt,ix(0:*),mget(*),jmax,jmin, mcut; real(8) fm(
     l*),pf(*),p(*),x(0:*),avg;end;subroutine FM_200(m0,yi,mname,n,x,ix,p, jmax,jmin, nmatr, itt, fm,pf, nz,ncn,cfn,bnds,mget,  m);u
     lse CiFort; character(*) mname; integer(4) m0,n,ix(0:*),m, jmax,jmin,nmatr, itt, nz,ncn(*),mget(*);integer(plen),value:: yi; re
     lal(8) x(0:*), fm(*),pf(*), cfn(*),bnds(0:1,0:*),p(*);end;subroutine FM_DRAW(m,mfull,yi,n1,n,x,ix,kmatr,nmatr,itt, jmax,jmin,fm
     l,pf, avg, mget);use CiFort; integer(4) m,mfull,n1,n,nmatr, itt,ix(0:*),kmatr,jmax,jmin,mget(*);integer(plen),value:: yi; real(
     l8) x(0:*),avg,fm(*),pf(*);end;integer(4) function MCutGet(m,n,yi); use CiFort; integer(plen),value:: yi; integer(4) m,n;end;su
     lbroutine FM_One_row(yi,n,x,ix, jmax,jmin,avg,fm,pf);use CiFort; integer(plen),value:: yi; integer(4) n,ix(0:*),jmax,jmin; real
     l(8) x(0:*),fm(*),pf(*),avg;end;subroutine FM_for_pCvar(yi,n1,n,x,ix, jmax,jmin,avg,fm,pf,p);use CiFort; integer(plen),value:: 
     lyi; integer(4) n,n1,ix(0:*),jmax,jmin; real(8) x(0:*),p(*),fm(*),pf(*),avg;end;subroutine FM_One(m,yi,p,n,x,ix, jmax,jmin,avg,
     lfm,pf);use CiFort; integer(plen),value:: yi; integer(4) m,n,ix(0:*),jmax,jmin; real(8) p(*),x(0:*),avg,fm(*),pf(*);end;subrout
     line FM_ksm_fun_ni(m,yi,p,n1,n,x,ix,   jmax,jmin,avg,fm,pf);use CiFort; integer(plen),value:: yi; integer(4) m,n1,n,ix(0:*),jma
     lx,jmin; real(8) p(*),x(0:*),avg,fm(*),pf(*);end;subroutine FM_Recourse(mname,x,ix1,nnb1,m4,nnb4,yi4,p,     yi1,jmax,jmin,avg,f
     lm,pf); use CiFort;integer(plen),value:: yi1,yi4; real(8) p(*),avg,fm(*),pf(*),x(0:*); character(*) mname(*);integer(4) nnb1,nn
     lb4,ix1(0:*),m4,jmax,jmin;end;subroutine FM_Ltranche(m,n,x,ix,yi,mv,nv,v,p, jmax,jmin, avg,fm,pf, yi3); use CiFort;integer(plen
     l),value:: yi,yi3,v; integer(4) m,n,ix(0:*),mv,nv,jmax,jmin; real(8) x(0:*),avg,fm(*),pf(*),p(*);end;subroutine FM_ExtLoss(chyi
     l,n,x,ix,m,p,jmax,jmin,avg,fm,pf); use CiFort;integer(plen),value:: chyi; integer(4) n,ix(0:*),m,jmax,jmin; real(8) x(0:*),p(*)
     l,pf(*),avg,fm(m);end;subroutine FM_setof_CvarCompPos(itt,x,ix,n,m,m2,m3,m4,p,yi2,v3,v4,conf_lev,jp,jpb,yi,jmax,jmin,avg,fm,pf)
      use CiFort; integer(plen),value:: yi2,v3,v4,yi; integer(4) itt,ix(0:*),n,m,m2,m3,m4,jp(*),jpb(*),jmax,jmin;real(8) x(0:*),avg,
     lpf(*),fm(*),p(*),conf_lev;end;subroutine GetOneLinearFunc(n,yi,nf,nc,cf,x,ix,  fi);use CiFort; integer(plen),value:: yi; integ
     ler(4) n,nf,nc,ix(0:*); real(8) cf,x(0:*),fi(0:*);end;subroutine TSPCuts(x,m,n,yi,ix, nz,nc,cf, fi,chw, gst); use CiFort;intege
     lr(plen),value:: yi; integer(4) m,n,ix(0:*),nz,nc(*); real(8) x(0:*),cf(*),fi(0:*),gst(0:*); character(*) chw;end;subroutine Fu
     lncsL1L2(mname,itt,x,iVarDop,m1,m2,n1,n2,yi1,yi2,ix1,ix2,p1,p2,nz,nf,wf,nc,cf,jp1,jpb1,fi,chw,gst,fm1,pf1,
     +polka,mget,avg);use CiFort; integer(plen),value:: yi1,yi2; integer(4) itt,iVarDop(*),m1,m2,n1,n2,ix1(0:*),ix2(0:*),nz,nf(*),nc
     l(*),mget(*);integer(4),target::jp1(0:*),jpb1(0:*); real(8) x(0:*),wf(*),cf(*),fi(0:*),polka(*),p2(*),avg;real(8),target::p1(*)
     l,gst(*),fm1(*),pf1(*); character(*) chw, mname;end;subroutine Polinom_Abs_Fun(m,yi,n1,n,x,ix,nz,nf,nc,cf, fi,gst, chw);use CiF
     lort; integer(plen),value:: yi; integer(4) m,n1,n,nz, ix(0:n),nf(nz),nc(nz);real(8) x(0:n1),cf(nz),fi(0:*),gst(0:n); character(
     l*) chw;end;subroutine Entropy_Fun(m,yi,n1,n,x,ix,nz,nf,nc,cf,fi,gst,chw);use CiFort; integer(plen),value:: yi; integer(4) m,n1
     l,n,nz,ix(0:n),nf(nz),nc(nz);real(8) x(0:n1),cf(nz),fi(0:*),gst(0:n); character(*) chw;end;subroutine Card_FunCalc(mname,m,yi,n
     l1,n,x,ix,nz,nf,wfm,nc,cf,chw,fi,xwrk);use CiFort; integer(plen),value:: yi; integer(4) m,n1,n,nz,ix(0:*),nf(*),nc(*);real(8) x
     l(0:*),wfm(*),cf(*),fi(0:*),xwrk(0:*); character(*) chw,mname;end;subroutine Eut_FunCalc(mname,m,yi, n1,n,x,ix, nz,nf, wf,nc,cf
     l,p,fi,gst,chw,fm,pf);use CiFort; integer(plen),value:: yi; integer(4) m,n1,n,nz, ix(0:n),nf(nz),nc(nz);real(8) x(0:n1),cf(nz),
     lfi(0:*),wf(nz),fm(*),p(*),gst(0:*),pf(*); character(*) chw, mname;end;subroutine ExternalFuncs(chyi, n,x,ix, nz,nf, nc,cf,fi,c
     lhw, ln);use CiFort; integer(plen),value:: chyi; integer(4),value:: ln;integer(4) n,nz,nf(*),ix(0:n),nc(nz); real(8) x(0:*),cf(
     lnz),fi(0:*); character(*) chw;end;subroutine Prmulti_ni_Calc(mname, m,n,n1,x, yim,yis,ixm,ixs, nz,nf,wf,nc,cf,fi,ST0,gst,chw);
      use CiFort; integer(plen),value:: yim,yis; integer(4) m,n,n1,nz,ixm(0:n),ixs(0:n),nf(*),nc(*);real(8) x(0:n1),wf(*),cf(*),fi(0
     l:*),ST0; character(*) chw,mname; real(8),target:: gst(*);end;subroutine Pr_ND_Calc(mname,nm,kmtrn, m,n,n1,x, yim,yis,ix, nz,nf
     l,wf,nc,cf,fi,st0,gst,chw);use CiFort; integer(plen),value:: yim,yis; integer(4) m,n,n1,nz,ix(0:n),nf(*),nc(*),nm,kmtrn;real(8)
     l x(0:n1),wf(*),cf(*),fi(0:*),ST0; character(*) chw,mname; real(8),target:: gst(*);end;subroutine AllFunc_nid_Calc(mname,kzp,m,
     ln,n1,x, yim,yis,ix, nz,nf,wf,nc,cf,fi,st0,gst,polka,chw);use CiFort; integer(plen),value:: yim,yis; integer(4) kzp,m,n,n1,nz,i
     lx(0:n),nf(*),nc(*);real(8) x(0:n1),wf(*),cf(*),fi(0:*),ST0,polka(kzp,*); real(8),target:: gst(0:*); character(*) chw,mname;end
      subroutine AvgFunc_ni_Calc(mname, m,n,n1,x, yim,yis,p,ixm,ixs, nz,nf,wf,nc,cf,iVarDop,fi,ST0,gst,chw);use CiFort; integer(plen
     l),value:: yim,yis; integer(4) m,n,n1,nz,ixm(0:n),ixs(0:n),nf(*),nc(*),iVarDop(*);real(8) x(0:n1),p(*),wf(*),cf(*),fi(0:*),ST0;
       character(*) chw,mname; real(8),target:: gst(*);end;subroutine CVaR_Col_Calc(n,n1,x, yi,ix, kf,nf,wf,nc,cf,fi,klast); use CiF
     lort;integer(plen),value:: yi; integer(4) n,n1,kf,ix(0:*),nf(*),nc(*),klast(*); real(8) x(0:n1),wf(*),cf(*),fi(0:*);end;subrout
     line wCvar_ni_Calc(n,x,yim,yis,ixm,nz,nf,wf,nc,cf,polka,fi,gst,chw);use CiFort; integer(plen),value:: yim,yis; integer(4) n,nz,
     lixm(0:n),nf(*),nc(*);real(8) x(0:*),wf(*),cf(*),fi(0:*),polka(*); real(8),target:: gst(*); character(*) chw;end;subroutine RoK
     lb_err_OutputCalc(mname,m,n,n1,x,yi,p,ix,np0,yp,nz,nf,nc,cf,kzp,fi,list,klast,polka,gst,fm); use CiFort;character(*) mname; int
     leger(4) m,n,n1,ix(0:*),np0,nz,nc(*),nf(*),kzp,list(*),klast; real(8),target:: gst(*);integer(plen),value:: yi,yp; target yp; r
     leal(8) x(0:n1),p(*),cf(*),fi(0:*),fm(*),polka(kzp,*);end;subroutine Lp_Norms_Fun(mname,m,yi,n,x,ix,nz,nf,nc,cf,wf,fm,p,jmax,jm
     lin,kzp,fi,gst, st0,polka,chw);use CiFort; integer(plen),value:: yi; character(*) mname,chw; integer(4) m,n,nz, ix(0:n),nf(nz),
     lnc(nz),jmax,jmin,kzp;real(8) x(0:*),cf(*),wf,fm(*),p(*),fi(0:*),gst(0:*),st0,polka(kzp,*);end;subroutine HMM_Calc(mname,mv,vy,
     lnvars,x,ix,nz,nc,cf,wf,wDegFmax,fi);use CiFort; integer(plen),value:: vy; integer(4) nvars,mv,ix(0:*),nz,nc(*);real(8) x(0:*),
     lcf(*),fi(0:*),wf,wDegFmax; character(*) mname;end;subroutine cvar2_FuncsCalc(mname,m,n,yi,p,ix,nz,nf,nc,wf,cf,kzp, fi,list0, f
     lm,pf,avg, polka,klast0,gst); use CiFort;integer(plen),value:: yi; integer(4) m,n,ix(0:*),nz,nf(*),nc(*),kzp; integer(4),target
     l:: klast0(*),list0(*);real(8) avg,p(*),wf(*),cf(*),fi(0:*),fm(*),pf(*); real(8),target:: gst(*),polka(kzp,*); character(*) mna
     lme;end;subroutine KantorCalc(x,n2,ix,mv,vy,vq,nz,nc,cf,fi,chw,gst); use CiFort;integer(plen),value:: vy,vq; integer(4) n2,mv,i
     lx(0:*),nz,nc(*); real(8) x(0:*),cf(*),fi(0:*),gst(0:*); character(*) chw;end;subroutine Fun_ALL(mname,ch, m,mf,yi,p,n1,n,x,ix,
     ljp,jpb,if210,if113, nz,nf,wf,nc,cf,fi,
     +kzp,polka,klast,iVarDop,jmax,jmin,avg0,st0,gst0,  kmatr,itnab,   fm,pf);use cifort; character(*) mname,ch; real(8) avg0,st0,p(
     l*),x(0:*),wf(*),cf(*),fi(0:*),polka(*),fm(*),pf(*);integer(4) m,mf,n1,n,if210,if113,nz,kmatr,itnab,kzp,ix(0:*),nf(*),nc(*),jma
     lx,jmin,klast(*),iVarDop(*),jp(0:*),jpb(0:*);integer(plen),value:: yi; real(8),target:: gst0(0:*);end;end interface;w=yi(1);i=n
     lfz(k); nz=nfz(k+1)-i; itt=itnab(k); kmt=kmtrn(k);if(chw=='itg0==10'.and.itt>=0) RETURN;mt=nmatr(nfmatr(k));mcut=mnb(mt);if( km
     lt>1 .or. itt>=0 )then;select case(itt);case(:3); mt1=nfmatr(k);do j=kmt,1,-1; mt=nmatr(mt1-1+j);call FM_Multi(mnb(mt),adyi(mt)
     l,p(nfp(mt)),
     +n1,nnb(mt),x,ix(nfix(mt)),  jmax(k),jmin(k),avg(k),  j, kmt, itt, mget(nfget(k)), fm, pf,amatr(mt1));enddo;case(100:103); mt1=
     lnfmatr(k);call FM_abs(mnb(mt),adyi(mt),p(nfp(mt)),
     +n1,nnb(mt),x,ix(nfix(mt)),  jmax(k),jmin(k),avg(k),  itt, mget(nfget(k)), fm, pf,amatr(mt1));case(114);call FM_One_row_abs(ady
     li(mt),nnb(mt),x,ix(nfix(mt)),   jmax(k),jmin(k),avg(k),fm,pf,mget(nfget(k)));case(300:303);call FM_Multi_Cuts(mnb(mt),adyi(mt)
     l,p(nfp(mt)),n1,nnb(mt),x,ix(nfix(mt)),itt,
     +jmax(k),jmin(k),avg(k),mget(nfget(k)),fm,pf,mcut);case(4:7,304:307,440);if(nfn(i)<0.or.mnb(mt)<=0) goto 100;if(itt==440)then; 
      kmt=1; endif;do j=1,kmt; mt=nmatr(nfmatr(k)-1+j);call FM_200(mnb(mt),adyi(mt),mname, nnb(mt),x,ix(nfix(mt)),p(nfp(mt)),   jmax
     l(k),jmin(k),
     +j, itt, fm,pf,  nz,ncn(i),cfn(i),bnds,mget(nfget(k)), mcut );enddo;if(itt==440)itt=4;case(8:9);do j=1,kmt; mt=nmatr(nfmatr(k)-
     l1+j);call FM_DRAW(mcut,mnb(mt),adyi(mt),n1,nnb(mt),x,ix(nfix(mt)), kmt,j,itt, jmax(k),jmin(k),fm,pf,avg(k),mget(nfget(k)));end
     ldo;case(308:309);mcut=MCutGet(mnb(mt),nnb(mt),adyi(mt)); kmt=mnb(mt)/mcut;do j=1,kmt; mt=nmatr(nfmatr(k));call FM_DRAW(mcut,mn
     lb(mt),adyi(mt),n1,nnb(mt),x,ix(nfix(mt)), kmt,j,itt, jmax(k),jmin(k),fm,pf,avg(k),mget(nfget(k)));enddo;case(14);call FM_One_r
     low(adyi(mt),nnb(mt),x,ix(nfix(mt)),   jmax(k),jmin(k),avg(k),fm,pf);case(23);call FM_for_pCvar(adyi(mt),n1,nnb(mt),x,ix(nfix(m
     lt)),   jmax(k),jmin(k),avg(k),fm,pf,p(nfp(mt)));case(24,28,30);call FM_One(mnb(mt), adyi(mt),p(nfp(mt)),nnb(mt),x,ix(nfix(mt))
     l, jmax(k),jmin(k),avg(k),fm,pf);case(26);call FM_ksm_fun_ni(mnb(mt), adyi(mt),p(nfp(mt)),n1,nnb(mt),x,ix(nfix(mt)), jmax(k),jm
     lin(k),avg(k),fm,pf);case(200);j=nfmatr(k)-1; mt1=nmatr(j+1); mt4=nmatr(j+3);call FM_Recourse(mname,x,ix(nfix(mt1)),nnb(mt1),mn
     lb(mt4),nnb(mt4),adyi(mt4),p(nfp(mt4)),
     +adyi(mt1),jmax(k),jmin(k),avg(k),fm,pf);case(201);j=nfmatr(k)-1; mt1=nmatr(j+1); mt2=nmatr(j+2); mt3=nmatr(j+3);call FM_Ltranc
     lhe(mnb(mt1),nnb(mt1),x,ix(nfix(mt1)),adyi(mt1),mnb(mt2),nnb(mt2),adyi(mt2), p(nfp(mt1)),
     +jmax(k),jmin(k),avg(k),fm,pf, adyi(mt3));case(202);j=nfmatr(k)-1; mt1=nmatr(j+1); mt2=nmatr(j+2);call FM_ExtLoss(adyi(mt2),nnb
     l(mt2),x,ix(nfix(mt2)),mnb(mt1),p(nfp(mt1)),    jmax(k),jmin(k),avg(k),fm,pf);case(10000:);mt1=nmatr(nfmatr(k)); mt2=nmatr(nfma
     ltr(k)+1); mt3=nmatr(nfmatr(k)+2); mt4=nmatr(nfmatr(k)+3);call FM_setof_CvarCompPos(itt,x,ix(nfix(mt2)),nnb(mt2),mnb(mt1),mnb(m
     lt2),mnb(mt3),mnb(mt4),p(nfp(mt1)),
     +adyi(mt2),adyi(mt3),adyi(mt4),prmn(k), jp(njp(k)),jpb(njp(k)), adyi(mt1),jmax(k),jmin(k),avg(k),fm,pf);end select;endif;if(iou
     ltk>=istop-1) goto 100;mt=nmatr(nfmatr(k));select case(itt);case(8:9,14,114,308:309);m=mnb(mt);if(itt<10)then; m=mnb(mt)*kmt; e
     llseif(itt<200)then; m=nnb(mt); endif;allocate(xwrk(m),STAT=j);if(j/=0)then; chw='Variables allocation_CALC_FI is failed'; call
     l putmess('S',510,'Func_Grad calculation',chw); goto 100;endif;w=1d0/dfloat(m); xwrk=w;CALL Fun_ALL(mname,chw, m, m  ,     adyi
     l(mt) ,  xwrk,
     +n1,nnb(mt),x,ix(nfix(mt)),jp(njp(k)),jpb(njp(k)),if2_10(k),
     +if11_13(k),nz,nfn(i),wfn(i),ncn(i),cfn(i),fi,
     +kzp,polka(i),klast(i),iVarDop(i),jmax(k),jmin(k),avg(k),st0(k),gst(ngst(k)),
     +kmt,itt, fm, pf);deallocate(xwrk);case(:7,24,26,100:103,300:307,10000:); i8=adyi(mt);if(nfn(i)==1)then;Call VariablesAddToCon(
     lmname,nz,ncn(i),cfn(i), x(ix(nfix(mt))+1),fi);elseif(nfn(i)<=11.and.nz==1)then;Call GetOneLinearFunc(nnb(mt),adyi(mt),nfn(i),n
     lcn(i),cfn(i),x,ix(nfix(mt)),  fi);else;CALL Fun_ALL(mname,chw, mcut,mcut,       i8 ,   p(nfp(mt)),
     +n1,nnb(mt),x,ix(nfix(mt)),jp(njp(k)),jpb(njp(k)),if2_10(k),
     +if11_13(k),nz,nfn(i),wfn(i),ncn(i),cfn(i),fi,
     +kzp,polka(i),klast(i),iVarDop(i),jmax(k),jmin(k),avg(k),st0(k),gst(ngst(k)),
     +kmt,itt, fm, pf);endif;case(200,201,202); j=nfmatr(k)-1; mt1=nmatr(j+1); mt3=nmatr(j+3); mt2=mt1;if(itt==201)then; mt2=mt3; mt
     l3=mt1; endif;if(itt==202) mt3=mt1;CALL Fun_ALL(mname,chw, mnb(mt3),mnb(mt3),     adyi(mt2) ,p(nfp(mt3)),
     +n1,nnb(mt1),x,ix(nfix(mt1)),jp(njp(k)),jpb(njp(k)),if2_10(k),
     +if11_13(k),nz,nfn(i),wfn(i),ncn(i),cfn(i),fi,
     +kzp,polka(i),klast(i),iVarDop(i),jmax(k),jmin(k),avg(k),st0(k),gst(ngst(k)),
     +kmt,itt, fm, pf);case(450);call TSPCuts(x,mnb(mt),nnb(mt),adyi(mt), ix(nfix(mt)),nz,ncn(i),cfn(i),   fi,chw,    gst(ngst(k)) )
      case(500:599); mt2=nmatr(nfmatr(k)+1);call FuncsL1L2(mname,itt,x,iVarDop(i), mnb(mt),mnb(mt2),nnb(mt),nnb(mt2),adyi(mt),adyi(m
     lt2),
     +ix(nfix(mt)),ix(nfix(mt2)),p(nfp(mt)),p(nfp(mt2)), nz,nfn(i),wfn(i),ncn(i),cfn(i),   jp(njp(k)),jpb(njp(k)),fi,chw,
     +gst(ngst(k)),fm,pf,polka(i),mget(nfget(k)),avg(k));case(10);CALL Polinom_Abs_Fun(mnb(mt),adyi(mt), n1,nnb(mt),x,ix(nfix(mt)),
     +nz,nfn(i),ncn(i),cfn(i),  fi, gst(ngst(k)), chw );case(11);CALL Entropy_Fun(mnb(mt),adyi(mt), n1,nnb(mt),x,ix(nfix(mt)),
     +nz,nfn(i),ncn(i),cfn(i),          fi, gst(ngst(k)), chw );case(12);CALL Card_FunCalc(mname, mnb(mt),adyi(mt), n1,nnb(mt),x,ix(
     lnfix(mt)),
     +nz,nfn(i),wfn(i),ncn(i),cfn(i), chw,         fi,xwrk0 );case(13);CALL Eut_FunCalc(mname,mnb(mt),adyi(mt), n1,nnb(mt),x,ix(nfix
     l(mt)),
     +nz,nfn(i),wfn(i),ncn(i),cfn(i),p(nfp(mt)),          fi, gst(ngst(k)), chw,fm,pf );case(15); j=8*(1+mnb(mt))*(1+nnb(mt));CALL E
     lxternalFuncs(adyi(mt), nnb(mt),x,ix(nfix(mt)),
     +nz,nfn(i),ncn(i),cfn(i),          fi,chw,  j );case(16); j=nmatr(nfmatr(k)+1);Call Prmulti_ni_Calc(mname,mnb(mt),nnb(mt),n1,x,
     l adyi(mt),adyi(j), ix(nfix(mt)),ix(nfix(j)),
     +nz,nfn(i),wfn(i),ncn(i),cfn(i),                   fi,  st0(k),gst(ngst(k)),   chw);case(20);do nm=2,kmt; j=nmatr(nfmatr(k)-1+n
     lm);Call Pr_ND_Calc(mname,nm,kmt,mnb(mt),nnb(mt),n1,x, adyi(mt),adyi(j),ix(nfix(mt)),
     +nz,nfn(i),wfn(i),ncn(i),cfn(i),                   fi,  st0(k),gst(ngst(k)),   chw);enddo;case(17,21); j=nmatr(nfmatr(k)+1);Cal
     ll AllFunc_nid_Calc(mname,kzp, mnb(j),nnb(mt),n1,x, adyi(mt),adyi(j),ix(nfix(mt)),
     +nz,nfn(i),wfn(i),ncn(i),cfn(i),                   fi,  st0(k),gst(ngst(k)),polka(i),   chw);case(18); j=nmatr(nfmatr(k)+1);Cal
     ll AvgFunc_ni_Calc(mname,mnb(mt),nnb(mt),n1,x, adyi(mt),adyi(j),p(nfp(mt)),ix(nfix(mt)),ix(nfix(j)),
     +nz,nfn(i),wfn(i),ncn(i),cfn(i),iVarDop(i),              fi,  st0(k),gst(ngst(k)),   chw);case(19); j=nmatr(nfmatr(k)+1);Call C
     lVaR_Col_Calc(nnb(mt),n1,x, adyi(j),ix(nfix(j)),
     +nz,nfn(i),wfn(i),ncn(i),cfn(i),              fi,klast(i));case(23); m=nnb(mt);CALL pCvarCalc(m,p(nfp(mt)),fm,pf,
     +jp(njp(k)),jpb(njp(k)),nz,nfn(i),wfn(i),ncn(i),cfn(i),   fi, klast(i));case(27); j=nmatr(nfmatr(k)+1);Call wCvar_ni_Calc(nnb(m
     lt),x, adyi(mt),adyi(j),ix(nfix(mt)),
     +nz,nfn(i),wfn(i),ncn(i),cfn(i), polka(i),            fi,  gst(ngst(k)), chw);case(22);case(25);j=mt; nm=0;if(kmt==2)then; j=nm
     latr(nfmatr(k)+1); nm=nnb(j); endif;Call RoKb_err_OutputCalc(mname,mnb(mt),nnb(mt),n1,x, adyi(mt),p(nfp(mt)),ix(nfix(mt)),
     +nm,adyi(j),    nz,nfn(i),ncn(i),cfn(i),kzp,    fi,  jp(njp(k)),klast(i),polka(i),gst(ngst(k)),   fm);case(28);CALL Lp_Norms_Fu
     ln(mname,mnb(mt),adyi(mt),nnb(mt),x,ix(nfix(mt)),nz,nfn(i),ncn(i),cfn(i),wfn(i),fm,p(nfp(mt)),
     +jmax(k),jmin(k),kzp,       fi, gst(ngst(k)),st0(k),polka(i),  chw );case(29);mt2=nmatr(nfmatr(k)+1);CALL HMM_calc(mname,mnb(mt
     l2),adyi(mt2),nnb(mt),x,ix(nfix(mt)),nz,ncn(i),cfn(i),wfn(i),st0(k),     fi);case(30);Call cvar2_FuncsCalc(mname,mnb(mt),nnb(mt
     l), adyi(mt),p(nfp(mt)),ix(nfix(mt)),
     +nz,nfn(i),ncn(i),wfn(i),cfn(i),kzp,   fi,jp(njp(k)),  fm,pf,avg(k),  polka(i),klast(i),gst(ngst(k)));case(34); j=nfmatr(k); mt
     l1=nmatr(j+1); mt2=nmatr(j+2);call KantorCalc(x,nnb(mt),ix(nfix(mt)),mnb(mt1),adyi(mt1),adyi(mt2),
     +nz,ncn(i),cfn(i),   fi,chw,    gst(ngst(k)) );case default; write(chw,'(i7)')itt; chw='Internal error. No Subroutine for Data 
     lType '//trim(chw);if(itt/=9999) call putmess('S',5100,'Functions calculation',chw);goto 100;end select
100   return;
      end subroutine calc_fi
