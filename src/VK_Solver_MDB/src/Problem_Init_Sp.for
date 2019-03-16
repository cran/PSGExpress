      subroutine problem_init(probaddr,   llin,
     +n1,kconstr0,iqpro,timelimit,ientrop,estmin,lconvex,krecu,kdopvarbs0,sign_min0);use CiFort; use ModCommons; use FuncNames;integ
     ner(plen) probaddr;logical lconvex;integer(1) lcf,lkon,izdual(*);integer(2) isol,k0;integer(4) istg0,llin,n1,n2,iqpro,kconstr0,
     iklin,kiln,ndlin,kd,ientrop,maxcon,maxmem,ke,istage,iwhat,iWhat0,
     +k0max,krecu,kdopvarbs0,isdig,isinit,iret;integer(4) intarr(*),kac(*),id(*),kbt(*),mdata,ilast,nci0(*),itg,lmforcuts;real(8) fk
     cmin,timelimit,estmin,wf0,wfc,relmax,xbhuge,gap;real(8) C(n2,*),cd(*),D(*),A(*),B(*),xl(*),xu(*),xi(*),f0,fc(*),g0(*),gc0(*),xi
     b0(n2,*);real(8) dparr(*),bb(2,*),dual0(*);character(*) solv_stat,iname,str1;real(8) sign_min0,stime;integer(1),pointer:: ivtpq
     p(:);integer(4) l16p, mxkf,mxconstr,nabmx,mmax;integer(4),parameter::nmx=4**5;integer(4),parameter::nmxd=13;real(8),save:: sign
     j_min;character cov_matr_KW*8, some_name*(lrow), chw*(lrow), wch*(lrow),ch8*8;data cov_matr_KW/'smatrix_'/;integer(4),allocatab
     qle:: ixord(:);real(8),allocatable::x(:),xlb(:),xub(:);character(lnm),allocatable::xname(:),vname(:);character(1),allocatable::
     a tCon(:);integer(4),allocatable::ix(:),nfn(:),ncn(:),nfix(:),nfyiL(:),nfp(:),nci(:),
     +nnb(:),mnb(:),m1d(:),jmax(:),jmin(:),klast(:),iVarDop(:),jp(:),jpb(:);integer(plen),allocatable:: adyi(:), afn(:);real(8),allo
     ycatable::wfn(:),cfn(:),tfn(:),avg(:),st0(:),polka(:,:),gst(:),p(:),yi(:),ys(:,:);pointer(pyi,wyi); real(8) wyi(*);real(4) maw;
       real(8) qpeps;common /cmache/qpeps,maw;real(8), pointer::pxl(:),pxu(:);real(8) enk,xxxbndhuge;common/shr/enk,xxxbndhuge,pxl,p
     bxu;real(8),allocatable,target:: xwrk(:);integer(4),allocatable,target:: iwrk(:);integer(4),pointer:: iCol(:),iRow(:);real(8),a
     jllocatable:: bnds(:,:),fi(:),fw(:,:),pw(:),dual(:);integer(4),allocatable:: kb(:), lnrz(:), lnvr(:), numLcon(:);logical lfirst
     a,sp_out;integer(1),allocatable,target:: ibcrd(:,:),ivtype(:); integer(1),pointer,save::ivtp(:);integer(2),save:: itg0,ivarb,kc
     xard,kcard0,kcard1,kstage0,ip1,ip2,ip3;integer(4),save::i,j,k,m,n,m1,kconstr,knab,nz,iostat,kelm,if0,if01,if1,mmx,mxyi,nf,nogr,
     ni1,kzp,ic,it,za,iw,ivb,kdopvarbs,
     +ivrc,klin0;real(8),save:: xhuge,w,w1,wscal,w2,wbn,  xbndhuge, allLbounds,allUbounds;real(8),allocatable:: fm(:),pf(:);real(8),
     npointer,save:: g2(:,:);character(lnm) TypesPointName;character(lnm),save:: LowerPointName,UpperPointName;character(lnm*(nmx+4)
     s),allocatable:: wstr(:);integer(plen),allocatable:: jaddrm(:,:),jaddr3(:,:); integer(plen) iw8;save x,xlb,xub,xname,ixord, ix,
     r  nfn,ncn,nfix,adyi,nfp, nnb, mnb,jmax,jmin,klast,iVarDop, wfn,cfn,tfn,afn,
     +avg,st0,polka,gst,p,yi,jp,jpb,bnds,fi,fw,kb,lnrz,lnvr
     +, ys, fm,pf, numLcon,jaddr3, ibcrd,ivtype, iwrk,izr;integer(1),save:: ifeasA, ifeasB, ibrcd2;integer(4),save:: knab1,kf,knabp,
     dkfd,knab11,nextWhat,  ichange_fi,ibest,j1;integer(4),allocatable:: ib1(:),nfn1(:),ncn1(:),nfz1(:),nnab(:),kf0(:),nfn0(:),nfib(
     v:),ifp(:);real(8),allocatable:: wfn1(:),cfn1(:),bnd1(:),b1t(:),alp(:),cvars(:),wvar(:),xbest(:);save nfn1,ncn1,nfz1,nnab,kf0,n
     jfn0,nfib,ifp,ib1,wfn1,cfn1,bnd1,b1t,alp,cvars,wvar,xbest;real(8),save:: alp1,alp2,fkmin0;real(8) dconf1, dconf2, dconf22;commo
     fn /dconf/dconf1, dconf2, dconf22;integer(4),allocatable:: iMyes(:,:);character(lnm),allocatable::mname(:),fname(:);integer(4),
     isave:: kmatr,kpmtr,mt,mfirst;integer(4),allocatable::nfz(:),njp(:),ngst(:),if2_10(:),if11_13(:),kmtrn(:),ksplit(:),
     +nfmatr(:),itnab(:),nmatr(:),mget(:),nfget(:);real(8),allocatable::prmn(:),amatr(:);save iMyes,mname,fname,nfz,njp,ngst,if2_10,
     dif11_13,kmtrn,ksplit,nfmatr,itnab,nmatr,mget,nfget,prmn,amatr;integer(4),save:: ibuff,lrest,iDBp,igregbff;integer(4),pointer,s
     pave:: pChar; integer(4),target,save:: iu;character(l16kmax),pointer,save:: obj_names(:);character(l16k),pointer:: wobj(:);logi
     tcal use_nOder, use_nIb1, use_PrSet, new_cvars,use_Quadro,lrecu;common/use_control/ use_nOder, use_nIb1, use_PrSet, new_cvars,u
     cse_Quadro;allocatable:: rn1(:),rxlb(:),rxub(:),rxname(:,:);integer(4),save:: rn1; real(8),save:: rxlb,rxub; character(lnm),sav
     we:: rxname;character(lnm),external:: getconname, MnameFromCutTake;integer(4),external:: mRowsForNab, istr_without_sub, iFindCl
     xoseBracket;logical,external:: lnot_null;real(8),external:: dolia,precise_sum;integer(4),save:: k1,k2,km,kc,kix,kCoef,kcard1p,k
     nf1,kf2,i0,i2,ib,ifpr,it0,itt,im,idual,ilambd,jw,itakep,j2,mfm,
     +mt1,mt2,mt3,mt4,msec,mforth;real(8) cv,bPOE,sbnd,w0,wlambda;integer(4) ln,kst,izr,isverify,kr2,iret1;interface;subroutine Feel
     kCvarColMatr(n,m,ixf,yif,p,xname,  kf,nf,wf,      ScNm,  ix,yi,  chw);use CiFort; integer(plen),value:: yif,yi; integer(4) n,m,
     pixf(0:*),kf,nf(*),ix(0:*);real(8) p(*),wf(*); character(*) xname(-3:*),ScNm, chw;end;subroutine CutTakeMatrixFill(nmatr,p0,m0,
     iix0,yi0, mt,mname,n,m,iMyes,    ix,yi,p,lconvex);use cifort; integer(plen),value:: yi0,yi; integer(4) nmatr,mt,n,m,m0,ix0(0:*)
     q,iMyes(0:*),ix(0:*); real(8) p0(*),p(*);character(*) mname; logical lconvex;end;subroutine SplineCheckForResize(npar,mpar,ix2,
     iypar,nv,mv,ixv, nyi,myi,mig,kelm);use CiFort; integer(plen),value:: ypar; integer(4) npar,mpar,nv,mv,nyi,myi,mig,ix2(0:*),ixv(
     e0:*),kelm;end;real(8) function getmatrixel(yi,ix,n,m,i,j,   i0);use CiFort; integer(plen),value:: yi; integer(4) m,n,i,j,ix(0:
     u*),i0;end;subroutine FillSpMatrix(mname,idb,Elem,iRow,iCol,kCoef, kelm0, iid,iprob,ibench,n,m,yiw,p,chw,iret);use CiFort; inte
     hger(4) kCoef,n,m,idb,iid,iprob,ibench,irow(*),icol(*),kelm0,iret;real(8) Elem(*),p(m); character(*) mname,chw; integer(plen),v
     ealue:: yiw;end;subroutine setmatrixrow(yi,ix,n,m,j,v);use CiFort; integer(plen),value:: yi; integer(4) m,n,j,ix(0:*); real(8) 
     zv(0:*);end;subroutine pastmatrixrow(yi,n,m,j,v);use CiFort; integer(plen),value:: yi; integer(4) m,n,j; real(8) v(0:*);end;sub
     proutine getmatrixcol(yi,ix,n,m1,m,j,  v);use CiFort; integer(plen),value:: yi; integer(4) m1,m,n,j,ix(0:*); real(8) v(m1:*);en
     ed;subroutine addmatrixcol(v,m1,m,  yi,n);use Cifort; integer(plen),value:: yi; integer(4) m1,m,n; real(8) v(0:*);end;subroutin
     ie get_ys(m,yi,n,ix,if113,   ys,  w1);use Cifort; integer(plen),value:: yi; integer(4) m,n,if113,ix(0:n); real(8) ys(2),w1;end;
      subroutine PrepSecondStage(kmtrn,nfmatr,mname,nmatr,mnb,nfix,ix, nnb1,nnb3,nnb4,  yi3,yi4, xname, rn1,rxname);use Cifort; inte
     rger(plen),value:: yi3,yi4; integer(4) kmtrn,nfmatr,nmatr(*),mnb(*),nnb1,nnb3,nnb4,ix(*),nfix(*),rn1;character(*) mname(*),rxna
     hme(0:*),xname(0:*);end;subroutine CheckAndGetDiagonal(iqpro,itnab,m,yi,n,ix,nfn,iv1113,cfw0,  isdig,cd);use Cifort; integer(pl
     yen),value:: yi; integer(4) m,n,iqpro,ix(0:n),nfn,itnab,iv1113,isdig; real(8) cd(*),cfw0;end;subroutine Set_CD(C,D,n2,iqpro,itn
     aab, m,yi,n,ix, cfw0 , nfn, iv1113,isdig);use Cifort; integer(plen),value:: yi; integer(4) n2,m,n,iqpro,ix(0:n), nfn, itnab, iv
     x1113,isdig;real(8) c(n2,n2),d(0:n2),cfw0;end;subroutine FM_200_GetLinPsh2(n1,n2,mmax,m,yi,n,ix,  yip,yip1,itt,cf,fw, klin,ke,a
     t,b,izdual);use Cifort; integer(plen),value:: yi,yip,yip1; integer(4) n1,n2,mmax,n,m,itt,ix(0:n),klin,ke; integer(1) izdual(*);
      real(8) cf,fw,a(n2,mmax),b(mmax);end;subroutine FM_200_GetLinPsh1(n1,n2,mmax,m,yi,n,ix,p,yip,     itt,cf,fw, klin,ke,a,b,izdua
     nl);use Cifort; integer(plen),value:: yi,yip; integer(4) n1,n2,mmax,n,m,itt,ix(0:n),klin,ke; integer(1) izdual(*);real(8) cf,fw
     k,a(n2,mmax),b(mmax),p(*);end;subroutine FM_200_GetLinIsht2(mcheck,n1,n2,m,yi,n,ix,  yip,yip1,itt,cf,fw,klin,ke,intarr,dparr,ka
     uc,izdual,bb,id,chw);use Cifort; integer(plen),value:: yi,yip,yip1; character(*) chw; integer(1) izdual(*);integer(4) mcheck,n1
     m,n2,n,m,itt,ix(0:*),klin,ke,intarr(*),kac(*),id(*); real(8) cf,fw,dparr(*),bb(2,*);end;subroutine FM_200_GetLinIsht1(mcheck,n1
     s,n2,m,yi,n,ix,p,yip,     itt,cf,fw,klin,ke,intarr,dparr,kac,izdual,bb,id,chw);use Cifort; integer(plen),value:: yi,yip; charac
     gter(*) chw; integer(1) izdual(*);integer(4) mcheck,n1,n2,n,m,itt,ix(0:*),klin,ke,intarr(*),kac(*),id(*); real(8) cf,fw,p(*),dp
     barr(*),bb(2,*);end;subroutine Polynom_GetLinPsh(n1,n2,mmax,m,yi,n,ix,bnds,cf,fw,kon,klin,ndlin,a,b);use Cifort; integer(plen),
     evalue:: yi; integer(4) n1,n2,mmax,n,m,ix(0:n), kon,  klin,ndlin;real(8) cf,fw,a(n2,mmax),b(*),bnds(0:1);end;subroutine MeanAbs
     b_GetLinPsh(n1,n2,mmax,m,yi,n,ix,bnds,p,nf,cf,fw,kon,klin,ndlin,a,b);use Cifort; integer(plen),value:: yi; integer(4) n1,n2,mma
     zx,n,m,ix(0:n), nf,kon,  klin,ndlin;real(8) cf,fw,a(n2,mmax),b(mmax),bnds(0:1),p(m);end;subroutine LinScen(nmtr,mcheck,n1,n2,m,
     myi,n,ix,nf,ys,klin,ndlin,intarr,dparr,kac,bb,xlb,id,chw);use Cifort; integer(plen),value:: yi; character(*) chw; integer(4) n1
     l,n2,n,m,ix(0:*), nf,nmtr,klin,ndlin;real(8) ys,xlb(*); integer(4) mcheck,intarr(*),kac(*),id(*); real(8) dparr(*),bb(2,*);end;
      subroutine NonLinIsht(ientrop,kmtr,nfLj,mcheck,n1,n2,m,yi,n,ix,p,nf,wfn,ys,wVarMip,lnvr,klin,ndlin,
     +intarr,dparr,kac,bb,xlb,xub, sbnd,  id,chw,    yp,np,ixp );use Cifort; integer(plen),value::yi,yp; character(*)chw; real(8) ys
     w,p(*),xlb(*),xub(*),wfn,dparr(*),bb(2,*),sbnd,wVarMip;integer(4) kmtr,nfLj,n1,n2,n,m,ix(0:*),ixp(0:*),np,nf,lnvr,klin,ndlin,ie
     antrop,mcheck,intarr(*),kac(*),id(*);end;subroutine read_nab_Sec(cov_matr,mname0,nmx,mmx,mxyt,if2_10,if11_13,itakep,n1,xname,xl
     ib,xub,ixord,  n,m,kelm,jaddr,p,
     +ix,yi,lconvex,  iMy,  wstr); use CiFort;integer(plen),value::yi; integer(4) imy,ixord(*),nmx,mmx,mxyt,n1,ix(0:*),if11_13,if2_1
     e0,itakep,n,m,kelm;integer(PLEN) jaddr(2); character(*) cov_matr,mname0,xname(-3:*),wstr*(*); logical lconvex; real(8) p(*),xlb
     l(*),xub(*);end;integer(4) function igetMatrixFilledSize(yi,n,m);use CiFort; integer(plen),value::yi; integer(4) m,n;end;subrou
     mtine Card_Change_YI(ivb,ivrc,ivarb,nextWhat,kcard, m,n1,n,x,ix,nz,nf,wfm,nc,ncn,kzp,bnds,cf,lnvr,yi,ibcrd,chw);use IntelInterf
     y; integer(plen),value::yi; integer(2) ivarb,kcard; character(*) chw;integer(4) ivb,ivrc,nextWhat,m,n1,n,nz,kzp, ix(0:*),nf(*),
     bnc(*),lnvr,ncn(*);real(8) x(0:*),wfm(*),cf(*),bnds(0:1,0:*); integer(1) ibcrd(n1,3);end;subroutine Card_Change_YIIshtv(ivb,ivr
     rc,ivarb,nextWhat,kcard,m,n1,n,x,ix,nz,nf,wfm,nc,ncn,kzp,bnds,cf,lnvr,numLcon,
     +intarr,kac,dparr,bb,yi,ibcrd,chw);use IntelInterf; integer(plen),value::yi; integer(2) ivarb,kcard; character(*) chw;integer(4
     z) ivb,ivrc,nextWhat,m,n1,n,nz,kzp, ix(0:*),nf(*),nc(*),intarr(*),kac(*),lnvr,numLcon(*),ncn(*);real(8) x(0:*),wfm(*),cf(*),dpa
     grr(*),bb(2,*),bnds(0:1,0:*); integer(1) ibcrd(n1,3);end;subroutine GetOneLinFromMulti7(lx,x,j,m,n,n1,ix,yi,yip,yip1,itt,cf,xhu
     sge,fw,a,w,b);use IntelInterf; integer(plen),value::yi,yip,yip1; integer(4) lx,j,m,n,n1,ix(0:*),itt;real(8) x(0:*),cf,fw,a(0:n1
     v),b(0:*),w,xhuge;end;subroutine FM_Multi(m,yi,p,n1,n,x,ix,jmax,jmin,avg,nmatr,kmatr,itt,mget,fm,pf,amatr); use cifort;integer(
     q4) m,n1,n,nmatr,kmatr,itt,ix(0:n),mget(m),jmax,jmin;integer(plen),value:: yi; real(8) p(m),x(0:*),avg,amatr(*),fm(*),pf(*);end
      subroutine FM_One_row(yi,n,x,ix, jmax,jmin,avg,fm,pf);use CiFort; integer(plen),value:: yi; integer(4) n,ix(0:*),jmax,jmin; re
     kal(8) x(0:*),fm(*),pf(*),avg;end;subroutine FM_One(m,yi,p,n,x,ix, jmax,jmin,avg,fm,pf);use CiFort; integer(plen),value:: yi; i
     integer(4) m,n,ix(0:*),jmax,jmin; real(8) p(*),x(0:*),avg,fm(*),pf(*);end;subroutine FM_Recourse(mname,x,ix1,nnb1,m4,nnb4,yi4,p
     i,     yi1,jmax,jmin,avg,fm,pf); use CiFort;integer(plen),value:: yi1,yi4; real(8) p(*),avg,fm(*),pf(*),x(0:*); character(*) mn
     oame;integer(4) nnb1,nnb4,ix1(0:*),m4,jmax,jmin;end;subroutine FM_Ltranche(m,n,x,ix,yi,mv,nv,v,p, jmax,jmin, avg,fm,pf, yi3); u
     wse CiFort;integer(plen),value:: yi,yi3,v; integer(4) m,n,ix(0:*),mv,nv,jmax,jmin; real(8) x(0:*),avg,fm(*),pf(*),p(*);end;subr
     houtine FM_ExtLoss(chyi,n,x,ix,m,p,jmax,jmin,avg,fm,pf); use CiFort;integer(plen),value:: chyi; integer(4) n,ix(0:*),m,jmax,jmi
     vn; real(8) x(0:*),p(*),pf(*),avg,fm(m);end;subroutine FuncsL1L2(mname,itt,x,iVarDop,m1,m2,n1,n2,yi1,yi2,ix1,ix2,p1,p2,nz,nf,wf
     c,nc,cf,jp1,jpb1,fi,chw,gst,fm1,pf1,
     +polka,mget,avg);use CiFort; integer(plen),value:: yi1,yi2; integer(4) itt,iVarDop(*),m1,m2,n1,n2,ix1(0:*),ix2(0:*),nz,nf(*),nc
     e(*),mget(*);integer(4),target::jp1(0:*),jpb1(0:*); real(8) x(0:*),wf(*),cf(*),fi(0:*),polka(*),p2(*),avg;real(8),target::p1(*)
     r,gst(*),fm1(*),pf1(*); character(*) chw, mname;end;subroutine New_Ib1_Alp(m,yi,p,n,x,ix,jp,jpb,nz,nf,b1t,Wvar,alp,ib1,ifp,kj1,
     fnextWhat,kmatr,fm,pf,avg0,jmax,jmin,iret);use CiFort; integer(plen),value:: yi; real(8) p(*),x(0:*),b1t(*),Wvar(*),alp(*),fm(*
     z),pf(*),avg0;integer(4) m,n,nz,kj1,nextWhat,jmax,jmin,ix(0:*),nf(*),ifp(*),ib1(*),jp(0:*),jpb(0:*),iret,kmatr;end;subroutine C
     vvarsL1L2forVarPr(itt,x,iVarDop, m1,m2,n1,n2,yi1,yi2,ix1,ix2,p1,p2, nz,nf,alp,nc,cf,b1t,nfn0,
     +jp1,jpb1,fi,chw,gst,fm1,pf1,polka,mget,avg,st0);use CiFort; integer(plen),value:: yi1,yi2; character(*) chw; real(8),target::p
     c1(*),gst(*),fm1(*),pf1(*);integer(4) itt,iVarDop(*),m1,m2,n1,n2,ix1(0:*),ix2(0:*),nz,nf(*),nc(*),mget(*),nfn0(*);integer(4),ta
     qrget::jp1(0:*),jpb1(0:*); real(8) x(0:*),alp(*),cf(*),fi(0:*),polka(*),avg,b1t(*),st0,p2(*);end;subroutine SpMatrixAddrs(iyw,y
     xiw,m,n,   sp_out,krows);use CiFort; integer(plen),value:: iyw,yiw; target iyw,yiw; integer(4) m,n,krows; logical sp_out;end;su
     pbroutine setmatrixzero(yi,n,m);use CiFort; integer(plen),value:: yi; integer(4) m,n;end;subroutine SaveMatrix(fname,yi,ix,xnam
     ne,n,m,ibench);use cifort; integer(plen),value:: yi; character(*) fname,xname(0:*); integer(4) m,n,ix(0:*),ibench;end;subroutin
     ve Free_Recourse(m4,nnb4,yi4);use CiFort; integer(plen),value:: yi4; integer(4) m4,nnb4;end;subroutine HMM_InitPoint(mname,nvar
     ks,ix,mv,vy,isinit,wf, wD,x,iret);use CiFort; integer(plen),value:: vy; integer(4) nvars,mv,ix(0:*),isinit,iret; real(8) x(0:*)
     v,wf,wD; character(*) mname;end;subroutine KantorInitPoint(n2,ix,mv,vy,vq, x,iret);use CiFort; integer(plen),value:: vy,vq; int
     veger(4) n2,mv,ix(0:*),iret; real(8) x(0:*);end;subroutine TSP_InitPoint(m,n,yi,ix, x);use cifort; integer(plen),value:: yi; in
     fteger(4) m,n,ix(0:*); real(8) x(0:*);END;end interface;nullify(g2,wobj,obj_names);call start_stop_whatch(1)
#if defined (_D)
      l16p=128; mxkf= 1000; mxconstr= 5000; nabmx= 5000; mmax= 30000000
#else
      l16p=128; mxkf= 1000;  mxconstr=100000; nabmx=100000; mmax=30000000
#endif
      ibuff=0; igregbff=0;if(lf19)then; chw="lf19==true option is not used"; call putmess('S',507,'Problem_init module',chw); goto 7
     h9999;endif;xhuge=huge(w)/2d0; xbndhuge=1d13; timelimit=xhuge;nextWhat=-1;      ioutk=1;ALLOCATE(xname(-3:nmx),xlb(nmx),xub(nmx
     l),ixord(nmx), wstr(1), STAT=iostat);if(iostat.ne.0)then; chw='Variables allocation_1 is failed'; call putmess('S',510,'Problem
     j Initialization',chw); goto 79999;endif;xlb=-xbndhuge;  xub=xbndhuge;  allLbounds=-xbndhuge; allUbounds=xbndhuge;call set_func
     h_names(fnc_name,kfn);chw="Reading problem formulation"; call putmess('n',0,'',chw);  if(chw=='S') goto 79999;call read_task_0(
     rprobaddr,  mxkf,mxconstr,nabmx,  l16p,km,kc);if(ioutk>=istop-1) goto 79999;CALL read_task(probaddr,mxkf,mxconstr,nabmx,nmx,km,
     qxbndhuge,l16p,  xname,xlb,xub,ixord,
     +sign_min,lconvex, n1, kconstr, knab, nz, kmatr, kpmtr, timelimit,
     +TypesPointName,allLbounds,allUbounds,LowerPointName,UpperPointName);sign_min0=sign_min;if(ioutk>=istop-1) goto 79999;call Chec
     ik_stop_whatch(1,w); if(lf21)write(21,"(a,f7.2)")'  Exit Read_Task',w;ALLOCATE(nfn(nz),wfn(nz),ncn(nz),cfn(nz),lnvr(nz),numLcon
     t(nz),tfn(nz),afn(nz),
     +nfz(knab+1), njp(knab+1),ngst(knab+1), nfget(knab+1),nfmatr(knab+1),
     +fname(knab), if2_10(knab), if11_13(knab),kmtrn(knab),itnab(knab),prmn(knab),
     +nmatr(kpmtr),amatr(kpmtr),
     +kb(0:kconstr),bnds(0:1,0:kconstr),lnrz(0:kconstr),
     +nfix(kmatr+1),nfyiL(kmatr+1),adyi(kmatr+1),nfp(kmatr+1),jaddrm(2,kmatr+1),
     +mname(kmatr), nnb(kmatr), mnb(kmatr), iMyes(0:3,kmatr), STAT=iostat);if(iostat.ne.0)then; chw='Variables allocation_2 is faile
     fd'; call putmess('S',510,'Problem Initialization',chw);call DEALL_TASK(l16p); goto 79999;endif;adyi=0; jaddrm=0;nfn=-1; wfn=0.
     j; ncn=-1; cfn=0.; tfn=0; afn=0;nnb=0; mnb=0;allocate(jaddr3(2,4)); jaddr3=0;CALL read_task_2(sign_min,kconstr,n1,knab,nmx,l16p
     n,nz,kmatr,kpmtr,
     +nfn,wfn,ncn,cfn,tfn,      nfz,   xlb,xub,    jaddr3,
     +kb,bnds,fname,mname,kmtrn,nfmatr,itnab,nmatr,     lnrz );if(ioutk>=istop-1) goto 79999;kzp=nz;if(lf21)write(21,'(/a,999(/i3,i1
     v0,2e25.11))')' ID; Type of bounds; Lower and upper bounds for constraints',
     +(i,kb(i),bnds(0,i),bnds(1,i),i=0,kconstr);if(lf21)write(21,"(/a,99(/i5,5x,a))")'Matrices',(mt,trim(mname(mt)),mt=1,kmatr);j=0;
      do k=1,knab;if(lf21)write(21,"(a,i3,a/a,4i7/a,999(/i5,1p,3x,e20.12,i5,3x,e20.12))")
     +'Data Set',k,': '//trim(fname(k)),'Kmatix, Set_Type, First_Matr_Pointer, First_Matrix',
     +kmtrn(k),itnab(k),nfmatr(k),nmatr(nfmatr(k)),"Function's code; Inner coeff.; Constr.; Coeff.",
     +(nfn(i),wfn(i),ncn(i),cfn(i),i=nfz(k),nfz(k+1)-1);enddo;if2_10=0; if11_13=0;estmin=0d0;iqpro=-1;ientrop=-1;krecu=0;if(.not.use
     r_Quadro) iqpro=0;do 500 k=1,knab;if0=0; if1=0; if01=0;do 490 j=nfz(k),nfz(k+1)-1;nf=iabs(nfn(j))/10;if(iabs(nfn(j))==1)if01=1;
      if(nf.eq.0) if0=1;  if(nf.eq.1) if1=1;if(nf==77) if1=1;select case(nf);case(:1); if2_10(k)=1;case(11:13); if(if11_13(k)==0.or.
     qif11_13(k)==2)then; if11_13(k)=if11_13(k)+1; endif;case(42);    if(if11_13(k)< 2)then; if11_13(k)=if11_13(k)+2; endif;case def
     nault; if2_10(k)=1;end select;if(if11_13(k)>=3.and..false.)then;chw='Problem Statement: dataset '//trim(fname(k))//' cannot be 
     oused in St_... and Quadratic functions simultaneously';call putmess('S',511,'Problem Initialization',chw); goto 79999;endif;se
     jlect case(nf);case(2,4,7,9,11,14,16,18,20:40,42:44,78,80,82:84,105,107,110,112,114,116:120,132,136,139:140,148:153);case defau
     elt;end select;if(ncn(j)==0)then;if(iqpro<0)then; iqpro=0;select case(nfn(j));case(110,610,1010);iqpro=1; case(120,620,1020);iq
     xpro=2;case(111,611,1011);iqpro=3; case(121,621,1021);iqpro=4;case(270,420,421);iqpro=5; case(440,443); iqpro=7;case(0:11,770:7
     y71);iqpro=10;end select;elseif(iqpro==10)then;select case(nfn(j));case(111,611,1011);iqpro=3; case(121,621,1021);iqpro=4;case(
     o270,420,421);iqpro=5; case(440,443); iqpro=7;case(0:11,770:771); iqpro=10;case default; iqpro=0;end select;elseif(iqpro==3.or.
     viqpro==4.or.iqpro==5.or.iqpro==7)then;if(nfn(j)>11.and..not.(770<=nfn(j).and.nfn(j)<=771)) iqpro=0;else; iqpro=0;endif;nf=nfn(
     wj)/10;select case(nf);case(4:5,7:8,11:12,16:26,28:29,32:33,36,40:41,45:52,58:59,61:62,67:70,83:84,
     +101:102,85:92,98:99,114:115,132:134,136,137,139,143:146,148:149,152:153);if(cfn(j)<0d0) estmin=-xhuge/1d20;case(34);  if(cfn(j
     o)>0d0) estmin=-xhuge/1d20;case(:-1);case default;if(nfn(j)==351)then;else; estmin=-xhuge/1d20;endif;end select;endif;iw=nfn(j)
      if(iw==271.or.iw==430)then;if(ientrop==-1) then;if(lnrz(ncn(j))>0.or.llin>1)then; ientrop=iw; if(ncn(j)==0)ientrop=-iw;else;  
      ientrop=-2;endif;else;   ientrop=-2;endif;else;if(nfn(j)>11.and.nfn(j)/=200) ientrop=-2;endif
490   enddo;if(index(fname(k),cov_matr_KW).eq.1.and.(if11_13(k)<=0.or.if2_10(k)+if0+if1 > 0)) then;chw='Problem Statement: dataset '
     q//trim(fname(k))//' can be used only for ST_... or Quadratic functions';call putmess('S',513,'Problem Initialization',chw); go
     uto 79999;endif;NabType: select case(itnab(k));case(200,201);do j=nfz(k),nfz(k+1)-1;select case(iabs(nfn(j))/10);case(1:17,116,
     w120);if(itnab(k)==200)then;if(kmtrn(k)/=3)then;chw='Problem Statement: dataset in Recourse function should contain three matri
     bces: '//trim(fname(k));call putmess('S',5210,'Problem Initialization',chw); goto 79999;endif;krecu=krecu+1;elseif(itnab(k)==20
     z1)then;if(kmtrn(k)/=3)then;chw='Problem Statement: dataset in Ltranche function should contain matrix and vector: '//trim(fnam
     oe(k));call putmess('S',5225,'Problem Initialization',chw); goto 79999;endif;lconvex=.false.;endif;if2_10(k)=1;case default; ch
     cw='Problem Statement: some functions cannot be calculated of: '//trim(fname(k));call putmess('S',514,'Problem Initialization',
     zchw); goto 79999;end select;enddo;case(202);do j=nfz(k),nfz(k+1)-1;select case(iabs(nfn(j)));case(1:40,50:111,120:131,140:171,
     g400:411,441,1230:1241,1360:1371);if(kmtrn(k)/=2)then;chw='Problem Statement: dataset should contain only external scenarios: '
     y//trim(fname(k));call putmess('S',5225,'Problem Initialization',chw); goto 79999;endif;lconvex=.false.;if2_10(k)=1;case defaul
     kt; chw='Problem Statement: some functions cannot be calculated of: '//trim(fname(k));call putmess('S',5144,'Problem Initializa
     rtion',chw); goto 79999;end select;enddo;case default;if(kmtrn(k)>1)then; do j=0,kmtrn(k)-1; if(mname(nmatr(nfmatr(k)+j))(:4)==
     f'cut(')then;chw='Problem Statement: Cut(..) operation cannot be combined with other matrices in: '//trim(fname(k));call putmes
     cs('S',5171,'Problem Initialization',chw); goto 79999;endif; enddo; endif;if(kmtrn(k)>1.and.itnab(k)<400) then;if(itnab(k)==13)
     pthen; chw='Problem Statement: dataset '//trim(fname(k))//
     +' cannot be used in Exp_eut, Log_eut, Pow_eut or Logexp_sum functions';call putmess('S',515,'Problem Initialization',chw); got
     no 79999;endif;do j=nfz(k),nfz(k+1)-1;select case(iabs(nfn(j))/10);case(4:5);lconvex=.false.;case(0,6,11:13);chw='Problem State
     dment: Linear, Meanabs_Risk or St_... function has more than one input matrix: '//trim(fname(k));call putmess('S',516,'Problem 
     bInitialization',chw); goto 79999;case(82);chw='Problem Statement: Pcvar_... function has more than one input matrix: '//trim(f
     gname(k));call putmess('S',519,'Problem Initialization',chw); goto 79999;case(27:39,42:43,107,141:153);chw='Problem Statement: 
     qdataset'//trim(fname(k))//
     +' cannot be used for functions Polynom_abs, Cardinality Group, Lp_norm_...';call putmess('S',5201,'Problem Initialization',chw
     p); goto 79999;case(20);if(itnab(k)==4 .or. itnab(k)>=5.and.itnab(k)<=6.and.kmtrn(k)>2 .or.
     +itnab(k)==7.and.kmtrn(k)>3) then;chw='Problem Statement: dataset '//trim(fname(k))//' cannot be used in LinearMulti function';
      call putmess('S',522,'Problem Initialization',chw); goto 79999;endif;if(nfz(k+1)-1-nfz(k)>0)then; chw='LinearMulti function wi
     ith the same matrix '//trim(fname(k))//
     +' can be used only once in Problem Statement';call putmess('S',523,'Problem Initialization',chw); goto 79999;endif;end select;
      enddo;endif;if(itnab(k)>=100.and.itnab(k)<200)then;if(kmtrn(k)/=1)then; chw='Problem Statement: dataset '//trim(fname(k))//' c
     yannot be used with Abs function';call putmess('S',5231,'Problem Initialization',chw); goto 79999;endif;endif;if(itnab(k)>=400.
     sand.itnab(k)<500)then;if(index(fname(k),'polynom_piece')==1)then;if(kmtrn(k)<4)then; chw='Problem Statement: dataset in Polyno
     wm_piece function should contain parameter,'//
     +' matrix and two vectors: '//trim(fname(k));call putmess('S',5220,'Problem Initialization',chw); goto 79999;endif;elseif(fname
     t(k)(:11)=='spline_sum')then;if(kmtrn(k)<2)then;chw='Problem Statement: dataset in Spline_sum function should contain at least 
     qtwo matrices: '//trim(fname(k));call putmess('S',5221,'Problem Initialization',chw); goto 79999;endif;elseif(fname(k)(:9)=='$w
     iO#rK@_,'.and.itnab(k)==450)then;if(kmtrn(k)/=2)then;chw='Problem Statement: dataset in TSP function should contain one matrix'
      call putmess('S',5241,'Problem Initialization',chw); goto 79999;endif;endif;endif;if(itnab(k)>=500.and.itnab(k)<600)then;if(km
     strn(k)/=2)then; chw='Problem Statement: dataset used for Addition/Subtraction of Losses '//
     +'should contain two matrices: '//trim(fname(k));call putmess('S',5233,'Problem Initialization',chw); goto 79999;endif;endif;if
     n(itnab(k)>=10000)then;if(kmtrn(k)/=4)then; chw='Problem Statement: incorrect dataset in composition: '//trim(fname(k));call pu
     ltmess('S',5230,'Problem Initialization',chw); goto 79999;endif;endif;do j=nfz(k),nfz(k+1)-1; i=iabs(nfn(j))/10; i1=iabs(nfn(j)
     r)-i*10;select case(i);case(45:70,73:76, 87:104, 121,135);if(kmtrn(k)/=2)then; chw='Problem Statement: dataset '//trim(fname(k)
     n)//
     +' cannot be used in functions depending on two matrices';call putmess('S',5240,'Problem Initialization',chw); goto 79999;endif
      case(133,134);if(kmtrn(k)<5.or.kmtrn(k)>7)then;chw='Problem Statement: dataset '//trim(fname(k))//' cannot be used in KSM_max_
     bni function';call putmess('S',5250,'Problem Initialization',chw); goto 79999;endif;case(77);if(kmtrn(k)/=2)then;chw='Problem S
     ttatement: dataset '//trim(fname(k))//' cannot be used for CVaR_Col_Risk functions';call putmess('S',5260,'Problem Initializati
     gon',chw); goto 79999;endif;case(83:84);if(kmtrn(k)/=4)then;chw='Problem Statement: dataset in KSM_... function should contain 
     cone matrix and two vectors: '//trim(fname(k));call putmess('S',5270,'Problem Initialization',chw); goto 79999;endif;case(132);
      if(kmtrn(k)/=3)then;chw='Problem Statement: dataset in Kantorovich function should contain two vectors: '//trim(fname(k));call
     t putmess('S',5277,'Problem Initialization',chw); goto 79999;endif;case(18:19,78:81,110:115);if(kmtrn(k)<2.and.fname(k)(:4)/='c
     eut_'.and.fname(k)(:4)/='cut(')then;chw='Function ('//trim(fnc_name(i,i1))//') is intended to use two or more matrices';call pu
     itmess('w',0,'Problem Initialization',chw);endif;endselect;enddo;if(itnab(k)==12.and.if11_13(k)>=1)then; chw='Problem Statement
     e: dataset '//trim(fname(k))//
     +' cannot be used in St_... and Cardinality Group functions simultaneously';call putmess('S',524,'Problem Initialization',chw);
       goto 79999;endif;end select NabType
500   enddo;if(iabs(ientrop)<271) ientrop=0;if(    iqpro>7.or.iqpro<0
     +.or. (tqsol/=1.and.tqsol/=11)
     +.or. lnrz(0)/=-2
     +.or. llin<0 )then; iqpro=0;elseif(iqpro>2)then; iqpro=iqpro-2;endif;chw="Asking for data information"; call putmess('n',0,'',c
     lhw);  if(chw=='S') goto 79999;lfirst=.true.;nfix=1;nfyiL=0;nfp=1;njp=1;ngst=1;w1=1.1e1;ALLOCATE(m1d(kmatr),ix(nmx+1),yi(1),p(1
     j), STAT=iostat);if(iostat.ne.0)then; chw='Variables allocation_3 is failed'; call putmess('S',510,'Problem Initialization',chw
     e); goto 79999;endif;if(krecu>0)then; allocate(rn1(krecu),rxname(-3:nmxd,krecu),rxlb(nmxd),rxub(nmxd), stat=iostat);rn1=0; rxna
     kme=''; rxlb=-xbndhuge;  rxub=xbndhuge;do i=1,krecu; rxname(-3:0,i)=xname(-3:0); enddo;if(iostat.ne.0)then; chw='Variables allo
     fcation_31 is failed'; call putmess('S',5101,'Problem Initialization',chw);goto 79999;endif;endif;mmx=1024; mxyi=1024;m1d=0; kr
     jecu=0;iMyes=0;do k=1,knab; do j=1,kmtrn(k); mt=nmatr(nfmatr(k)-1+j);if(if2_10(k)>=1) iMyes(1,mt)=-1;if(if11_13(k)==1.or.if11_1
     c3(k)==3)iMyes(2,mt)=-1;if(if11_13(k)>=2)  iMyes(3,mt)=-1;enddo; enddo;call Check_stop_whatch(1,w0); if(lf21)write(21,"(a,f7.2)
     w")'Befor matriceis reading',w0;do it0=0,1; do k=1,knab; mfirst=nmatr(nfmatr(k)); lrecu=.false.; if(200==itnab(k))lrecu=.true.;
      it=it0; if(trim(fname(k))=='TyPesPoIntDeFine'.and.trim(mname(mfirst))=='TyPesPoIntDeFine') it=1-it;if(it==0)then; chw='Reading
     p data set '//trim(fname(k))//' characteristics';j=index(fname(k),',vector#'); if(j>1) chw='Reading data set '//fname(k)(:j-1)/
     f/' characteristics';elseif(lrecu)then; krecu=krecu+1;endif;do j=it+1,it*(kmtrn(k)-1)+1; mt=nmatr(nfmatr(k)-1+j);if(iMyes(0,mt)
     p<=0)then;iDBp=iDB;if(k==knab-1.and.trim(fname(k))=='TyPesPoIntDeFine'.and.trim(mname(mt))=='TyPesPoIntDeFine'.and.kmtrn(k)==1)
     ythen;allocate(ibcrd(n1,3)); ibcrd=0;call GetTypes(TypesPointName,n1,xname,  ibcrd(1,3),iw,jaddrm(1,mt),i,wstr,iret1);if(iret1=
     o=1) goto 79999;if(iw>0.and.i==1)then;allocate(ivtype(n1)); ivtype=ibcrd(:,3); iw=0; call free(jaddrm(1,mt)); jaddrm(:,mt)=0;en
     xdif;iDB=-3; if(iw==0)deallocate(ibcrd);if(iw==0)then; iw=nfz(k); kb(ncn(iw))=-1; nfn(iw)=-nfn(iw); endif;endif;if(iabs(nfn(nfz
     p(k)))==1) iDB=-2;if(it>0.and.(index(mname(mt),'vector# 0.')>0.or.index(mname(mt),'vector#-0.')>0)) then;n=0; m=mnb(mfirst); ke
     wlm=m;elseif(it>0.and.itnab(k)==19.and.mname(mt)(:7)/='matrix_')then;n=nnb(mfirst); m=nfz(k+1)-nfz(k); kelm=(n+1)*(m+1);elseif(
     lmname(mt)(:7)=='NuMbEr#')then;n=0; m=0; kelm=m;elseif(mname(mt)(:7)=='takein('.or.mname(mt)(:7)=='cutout(')then;n=9; m=1; kelm
     f=m*(n+1);elseif(index(mname(mt),'ivector_')==1)then;n=0; m=1; kelm=2*m;elseif(index(mname(mt),'imatrix_')==1.or.index(mname(mt
     l),'ipmatrix_')==1)then;n=0; m=0; kelm=1;else;if(lrecu.and.j>1)then; ic=krecu; iw=nmxd; allocate(iwrk(iw));ln=min(lnm*14,int(hu
     dge(i)/10.));CALL read_nab_fst(                     trim(mname(mt)), iw,mmx,  mmax,
     +rn1(ic),rxname(-3,ic),rxlb,rxub,iwrk,  n,m,kelm,jaddrm(1,mt),  ix(1),iM,  ln );deallocate(iwrk);else; iw=11; i=len_trim(mname(
     amt)); ln=min(lnm*14,int(huge(i)/100.));CALL read_nab_fst(
     +mname(mt)(:i),
     +iw,mmx,  mmax,
     +n1,xname,
     +xlb,xub,ixord,
     +n,m,kelm,
     +jaddrm(1,mt),
     +ix(1),
     +iM,     ln        );endif;endif;if(it>0.and.itnab(k)==19.and.mname(mt)(:7)=='matrix_')then;if(n/=1.or.m/=1)then;chw='Incorrect
     p size of second matrix '//trim(mname(mt))//' in CVaR_Col_Risk function';call putmess('E',750,'Matrix Reading',chw); goto 79999
      endif;n=nnb(mfirst); m=nfz(k+1)-nfz(k); kelm=(n+1)*(m+1);endif;iDB=iDBp;if(ioutk>=istop-1) goto 79999;i1=0; if(iMyes(1,mt)==-1
     n) i1=1;ic=0; if(iMyes(2,mt)==-1.or.iMyes(3,mt)==-1) ic=1;if(j>1.and.(itnab(k)==20.or.itnab(k)==21)) ic=1;if(index(mname(mt),tr
     fim(cov_matr_KW)).eq.1)then;kelm=0;  m=0;else;if(if11_13(k)==3)then; if11_13(k)=4; i1=1;elseif(it>0.and.if11_13(k)/=0)then; i1=
     i1; if11_13(k)=-1; ic=0;elseif(if11_13(k)==1)then;if( ( (if2_10(k)/=0.and.n>=0.3*m).or.
     +(if2_10(k)==0.and.n>=1.3*m).or.
     +index(mname(mt)(:7),'pmatrix')>0.or.
     +99<itnab(k).and.itnab(k)<104
     +).and.iqpro<=0 )then;i1=1; if11_13(k)=-1; ic=0;endif;endif;if(if11_13(k)>=2)then;if(index(mname(mt)(:7),'pmatrix')>0
     +.or. m<0.5*n )then; i1=1; if11_13(k)=-if11_13(k); ic=0; endif;endif;if(if2_10(k)<=0.and.i1==1)then; if2_10(k)=1; endif;endif;m
     c1=m;if(i1==0)then; m=0; kelm=0; endif;nnb(mt)=n; mnb(mt)=m; m1d(mt)=m1;iw=min0(n,int(w1,4));nfyiL(mt)=(iw+1) + kElm + ic*(iw+1
     f)*(iw+1);nfix(mt+1)=iw+1;nfp(mt+1)=0;if(j==1.or.lrecu)then; nfp(mt+1)=m;endif;iMyes(0,mt)=1;call Check_stop_whatch(1,w); if(lf
     j21)write(21,"(a,f7.2,2i9)")'Matrix '//trim(mname(mt))//' read: time, m*n ',w,m,n;if(w-w0>2)then;if(index(mname(mt),'vector#')<
     i=0.and.index(mname(mt),'NuMbEr#')<=0)then;w0=w; chw=' '//trim(mname(mt))//' was checked'; call putmess('n',0,'',chw); if(chw==
     m'S') goto 79999;endif;endif;else;m=mnb(mt); n=nnb(mt);endif;enddo;enddo; enddo;if(n1>10)then;chw='Too many variables in the pr
     aoblem. Express Version allows not greater than 10.';call putmess('S',6919,'Express Version',chw);goto 79999;endif;do mt=1,kmat
     fr; if(mname(mt)(:7)/='takein('.and.mname(mt)(:7)/='cutout(')Cycle;call CutTakeParameters(mname(mt),0,  i,j,some_name,m1,iret1)
      if(iret1==1) goto 79999;do it=1,kmatr; if(some_name/=mname(it)) Cycle;call CutTakeParameters(mname(mt),mnb(it),  i,j,some_name
     p,m,iret1);if(iret1==1) goto 79999;n=nnb(it); nnb(mt)=n; mnb(mt)=m; nfix(mt+1)=n+1; nfp(mt+1)=m;if(mnb(mt)<=0)then; chw='Input 
     fmatrix '//trim(mname(mt))//' in Cutout/Takein operation does not have numerical rows';call putmess('S',5101,'Problem Initializ
     gation',chw); goto 79999;endif;select case(nfyiL(mt));case(10); nfyiL(mt)=(n+1)*1;case(20); nfyiL(mt)=(n+1)*(m+1); if(mname(it)
     d(:7)=='pmatrix') nfyiL(mt)=nfyiL(it);case(110); nfyiL(mt)=(n+1)*(1+n+1);case(120); nfyiL(mt)=(n+1)*(m+1+n+1); if(mname(it)(:7)
     m=='pmatrix') nfyiL(mt)=nfyiL(it)+(n+1)*(n+1);end select;Exit;enddo;if(it>kmatr)then; chw='Internal error it>kmatr for Takein/C
     putout';call putmess('S',5245,'Problem Initialization',chw); goto 79999;endif;enddo;do mt=1,kmatr; if(mname(mt)=='ipmatrix_box_
     gfor_bPOE_')Exit; enddo;if(mt<kmatr)then;do mt=1,kmatr;if(mname(mt)(:1)=='i')then;if(mname(mt)/='ipmatrix_box_for_bPOE_')then;d
     ko m1=1,kmatr; if(mname(mt)(2:)==mname(m1))then;nfyiL(mt)=nfyiL(m1); nfix(mt+1)=nfix(m1+1); nnb(mt)=nnb(m1); mnb(mt)=mnb(m1); E
     uxit;endif; enddo;else; nfyiL(mt)=(n1+1)+7*n1+10; nfix(mt+1)=n1+1; nnb(mt)=n1; mnb(mt)=2*n1;nfyiL(mt+1)=1+2*n1; mnb(mt+1)=2*n1;
      endif;endif;enddo;do mt=1,kmatr; if(mname(mt)=='ipmatrix_box_for_bPOE_')Cycle;nfyiL(mt)=nfyiL(mt)+2*(mnb(mt)+1);nfix(mt+1)=nfi
     ox(mt+1)+1;enddo;endif;nnew=n1;do k=1,knab; if(itnab(k)/=34) Cycle; mfirst=nmatr(nfmatr(k)); n=int(wfn(nfz(k)));nfix(mfirst+1)=
     b2*n+1; nnb(mfirst)=2*n; mnb(mfirst)=0;msec=nmatr(nfmatr(k)+1); mt=nmatr(nfmatr(k)+2);nfyiL(mfirst)=nfyiL(mfirst)+(0+1)*(nnb(mf
     mirst)+1);if(nnb(msec)>0.or.nnb(mt)>0)then; chw=trim(mname(msec))//' and '//trim(mname(mt))//' can not contain variables';call 
     zputmess('S',5258,'Problem Initialization',chw); goto 79999;endif;enddo;do it=0,1; do k=1,knab; mfirst=nmatr(nfmatr(k));do j=it
     b+1,it*(kmtrn(k)-1)+1; mt=nmatr(nfmatr(k)-1+j);m=mnb(mt); n=nnb(mt);if(j>1)then;select case(itnab(k));case(19,20,21,29,200,201,
     n202,25,450,10000:);case(24,26,34);if(itnab(k)==34.and.j==3.or.itnab(k)==24.and.j==4.or.itnab(k)==26.and.(j==5.or.j==6.or.j==7)
     k) then;if(m/=mnb(nmatr(nfmatr(k)+j-2)))then;chw='Dataset '//trim(fname(k))//' includes matrices (vectors) with different numbe
     ir of rows';call putmess('S',5253,'Problem Initialization',chw); goto 79999;endif;endif;case(400:443);if(index(fname(k),'polyno
     bm_piece')==1)then;if(j==4.and.mnb(nmatr(nfmatr(k)+j-2))/=mnb(nmatr(nfmatr(k)+j-1)))then;chw='Dataset '//trim(fname(k))//' incl
     kudes matrices (vectors) with different number of rows';call putmess('S',5251,'Problem Initialization',chw); goto 79999;endif;e
     gndif;case(500:599); continue;case(9999); continue;case default; if(m.ne.mnb(mfirst))then;chw='Dataset '//trim(fname(k))//' has
     s matrices with different numbers of scenarios';call putmess('S',5255,'Problem Initialization',chw); goto 79999;endif;end selec
     dt;endif;if(itnab(k)==200.and.j==3) nfyiL(mfirst)=nfyiL(mfirst)+(m-mnb(mfirst))*(nnb(mfirst)+1);if(itnab(k)==202.and.j==2)then;
       nfyiL(mfirst)=nfyiL(mfirst)+(m+1)*(n+1);mnb(mfirst)=m; m1d(mfirst)=m; nnb(mfirst)=n;nfix(mfirst+1)=n+1; nfp(mfirst+1)=m;mnb(m
     ct)= nfyiL(mt)/(nnb(mt)+1)-1;endif;if(itnab(k)==201.and.j==3)then; nfyiL(mt)=nfyiL(mt)+(mnb(mfirst)+1)*(nnb(mfirst)+1);endif;if
     r(itnab(k)==12.and.j==1)then; iw=0;do i=nfz(k),nfz(k+1)-1; iw=iw+1;select case (iabs(nfn(i))); case(290,310:321,  1490,1510:152
     v1); iw=iw+1; case(330,1530); iw=iw+3; end select;enddo;nfyiL(mt)=nfyiL(mt)+iw*(n+1);endif;if(itnab(k)>=100.and.itnab(k)<=103.a
     hnd.j==1)then;nfyiL(mt)=nfyiL(mt)+2*(n+1);nfp(mt+1)=2*m+2;endif;if(itnab(k)==24.and.j==4)then;nnb(mfirst)=nnb(nmatr(nfmatr(k)+1
     l));mnb(mfirst)=2*(nnb(mfirst)+m);m1d(mfirst)=mnb(mfirst);nfyiL(mfirst)=nfyiL(mfirst)+(2*(nnb(mfirst)+m)+1)*(nnb(mfirst)+1);nfp
     x(mfirst+1)=2*(nnb(mfirst)+m);endif;if(itnab(k)==26.and.j==5)then;nnb(mfirst)=nnb(nmatr(nfmatr(k)+1));mnb(mfirst)=2*m;m1d(mfirs
     et)=mnb(mfirst);nfyiL(mfirst)=nfyiL(mfirst)+(2*m+1)*(nnb(mfirst)+1);nfp(mfirst+1)=2*m;endif;if(itnab(k)==23) nfp(mfirst+1)=nnb(
     rmfirst);if(itnab(k)>=500.and.itnab(k)<600.and.j>1) nfp(mt+1)=mnb(mt);enddo;enddo; enddo;do k=1,knab; mfirst=nmatr(nfmatr(k));i
     cf(400<=itnab(k).and.itnab(k)<=439.and.index(fname(k),'polynom_piece')==1)then;mforth=nmatr(nfmatr(k)+3);nnb(mfirst)=0;do j=0,k
     qmtrn(k)-1;if(mname(nmatr(nfmatr(k)+j))(:7)=='NuMbEr#'.and.j+3<kmtrn(k))then;nnb(mfirst)=nnb(mfirst)+nnb(nmatr(nfmatr(k)+j+1));
      endif;enddo;nfix(mfirst+1)=nnb(mfirst)+1;m=mnb(nmatr(nfmatr(k)-1+3)); nfp(mfirst+1)=m;nfyiL(mfirst)=nfyiL(mfirst)+(m+1)*(nnb(m
     pfirst)+1);mnb(mfirst)=m;elseif((itnab(k)==440.or.itnab(k)==443).and.index(fname(k),'$wO#rK@_')/=1)then;nnb(mfirst)=nnb(nmatr(n
     ufmatr(k)-1+2));nfix(mfirst+1)=nnb(mfirst)+1;m=int((nnb(mfirst)/(wfn(nfz(k))+1)-1)*bnds(1,ncn(nfz(k)))+1);nfyiL(mfirst)=nfyiL(m
     ifirst)+(m+1)*(nnb(mfirst)+1);mnb(mfirst)=m;elseif(itnab(k)==450.and.index(fname(k),'$wO#rK@_')==1)then;iw=nnb(nmatr(nfmatr(k)+
     s1));m=iw;nnb(mfirst)=(iw*iw-iw)/2;nfix(mfirst+1)=nnb(mfirst)+1;nfyiL(mfirst)=int((nnb(mfirst)+1)+(m/2+1)+2*(nnb(mfirst)*1.5+1)
     o+20);mnb(mfirst)=m; m1d(mfirst)=m;do k1=1,knab;if(nmatr(nfmatr(k1))==mfirst.and.itnab(k1)==7)then; msec=nmatr(nfmatr(k1)+1);mn
     qb(msec)=m; m1d(msec)=m; nfyiL(msec)=m+1;endif;enddo;elseif(10000<=itnab(k))then;msec=nmatr(nfmatr(k)+1);m=mnb(msec); n=nnb(mse
     ic);if(m==1)then; m=mnb(nmatr(nfmatr(k)+2));if(m<=1)then; m=mnb(nmatr(nfmatr(k)+3));if(m<=1)then; chw='Data set '//trim(fname(k
     u))//' has just one function';call putmess('W',0,'Problem Initialization',chw);endif;endif;endif;nnb(mfirst)=n; mnb(mfirst)=m; 
      nfix(mfirst+1)=n+1;nfp(mfirst+1)=m; nfp(msec+1)=m; nfyiL(mfirst)=nfyiL(mfirst)+(m+1)*(n+1);do j=2,3;if(m/=mnb(nmatr(nfmatr(k)+
     xj)).and.mnb(nmatr(nfmatr(k)+j))>1)then;chw='Dataset '//trim(fname(k))//' includes matrices (vectors) with different number of 
     arows';call putmess('S',5275,'Problem Initialization',chw); goto 79999;endif;enddo;endif;enddo;do k=1,knab; iw=0; mfirst=nmatr(
     qnfmatr(k)); n=nnb(mfirst); m=mnb(mfirst);select case(itnab(k));case(450,34); iw=n+1;case(25); iw=m;case(30); iw=2*m;case(10,11
     s); iw=n+1;end select;if(if11_13(k)/=0) iw=n+1;if(abs(if11_13(k))>=2) iw=iw+n+1;do i=nfz(k),nfz(k+1)-1;selectcase(iabs(nfn(i)))
      case(351,400:411,1140:1151,1160:1180); iw=iw+m+1;case(450:640,670:701,730:761,850:1040); iw=iw+3*m+(n+1)*m;case(1070,1071); iw
     c=iw+max((n+1),m);case(1350); iw=iw+(n+1)*2;case default;if(itnab(k)>=500.and.itnab(k)<600) iw=iw+m+mnb(nmatr(nfmatr(k)+1))+1;e
     indselect;enddo;ngst(k+1)=ngst(k)+iw;enddo;do mt=1,kmatr;nfix(mt+1)=nfix(mt)+nfix(mt+1);nfp(mt+1)=nfp(mt)+nfp(mt+1);enddo;call 
     mCheck_stop_whatch(1,w); if(lf21)write(21,"(a,f7.2)")'  First Matriceis Reading',w;ioutk=2;deallocate( p, ix );ALLOCATE(p(nfp(k
     umatr+1)),ix(nfix(kmatr+1)), STAT=iostat);if(iostat.ne.0)then; chw='Variables allocation_4 is failed'; call putmess('S',510,'Pr
     poblem Initialization',chw); goto 79999;endif;p=1d0; ix=0;do mt=1,kmatr; i=(nfyiL(mt)+1)*8;adyi(mt)=malloc(i)+id8bt;i=0; call c
     sopybuff(loc(i),4,adyi(mt)-id8bt,4);enddo;chw="Getting data"; call putmess('n',0,'',chw);  if(chw=='S') goto 79999;iMyes=0;do k
     x=1,knab; do j=1,kmtrn(k); mt=nmatr(nfmatr(k)-1+j);if(if2_10(k)>=1) iMyes(1,mt)=-1;if(if11_13(k)==1.or.if11_13(k)>=3)iMyes(2,mt
     e)=-1;if(if11_13(k)>=2)  iMyes(3,mt)=-1;enddo; enddo;krecu=0;lfirst=.false.;do it=0,1; do k=1,knab;lrecu=.false.; if(200==itnab
     x(k))lrecu=.true.;if(it==1.and.lrecu)krecu=krecu+1;if0=0; if01=0;do j=nfz(k),nfz(k+1)-1;if(iabs(nfn(j))==1) if01=1;if(iabs(nfn(
     zj))/10.eq.0) if0=1;enddo;mfirst=nmatr(nfmatr(k));do j=it+1,it*(kmtrn(k)-1)+1;  mt=nmatr(nfmatr(k)-1+j);if(iMyes(0,mt)>0) Cycle
      mmx=mnb(mt);mxyi=nfyiL(mt)+(m1d(mt)-mnb(mt))*(nnb(mt)+1);i1=0; if(iMyes(1,mt)==-1) i1=1;ic=0; if(iMyes(2,mt)==-1.or.iMyes(3,mt
     b)==-1) ic=1;za=0; if(if11_13(k)>0) za=if11_13(k); if(za==4) za=2;if(j>1.and.(itnab(k)==20.or.itnab(k)==21)) za=1;za=max(za,ic)
      w=1.087e1;kelm=nfyiL(mt)-(nnb(mt)+1); if(za>0) kelm=kelm-(nnb(mt)+1)*(nnb(mt)+1);iDBp=iDB;if(k==knab-1.and.trim(fname(k))=='Ty
     sPesPoIntDeFine'.and.trim(mname(mt))=='TyPesPoIntDeFine'.and.kmtrn(k)==1)then;iDB=-3;endif;if(if01==1) iDB=-2;if(j>1.and.itnab(
     sk)==19)then;if(mname(mt)(:7)/='matrix_')then;call FeelCvarColMatr(nnb(mt),mnb(mfirst),ix(nfix(mfirst)),adyi(mfirst),p(nfp(mfir
     est)),xname,
     +nfz(k+1)-nfz(k),nfn(nfz(k)),wfn(nfz(k)),
     +trim(mname(mt)),  ix(nfix(mt)),adyi(mt), chw);goto 600;else; nnb(mt)=1; mmx=1; kelm=4;endif;endif;if(lrecu.and.j>1)then; ic=kr
     yecu; allocate(iwrk(nmxd));CALL READ_NAB_Sec(trim(cov_matr_KW),trim(mname(mt)),nnb(mt),mmx,mxyi, i1,za,
     +int(1),
     +rn1(ic),rxname(-3,ic),rxlb,rxub,iwrk,  nnb(mt),m1d(mt),kelm,jaddrm(1,mt),  p(nfp(mt)),
     +ix(nfix(mt)),adyi(mt),lconvex,  iM,  wstr(1)(:lnm*(nnb(mt)+4)) );deallocate(iwrk);if(j==3) nfp(mfirst)=nfp(mt);else;itakep=j;i
     gfpr=nfp(mfirst);select case(itnab(k));case(4:7);      itakep=0;case(20:21);    itakep=0;case(24,29,34); itakep=0;case(400:499)
     l;  itakep=0;case(500:599);if(j>1)then; itakep=1; ifpr=nfp(mt); endif;end select;CALL READ_NAB_Sec(trim(cov_matr_KW),
     +trim(mname(mt)),
     +nnb(mt),
     +mmx,mxyi,
     +i1,
     +za,
     +itakep,
     +n1,
     +xname(-3),
     +xlb,xub,ixord,
     +nnb(mt),
     +m1d(mt),
     +kelm,
     +jaddrm(1,mt),
     +p(ifpr),
     +ix(nfix(mt)),
     +adyi(mt),
     +lconvex,
     +iM,
     +wstr(1)(:lnm*(nnb(mt)+4))
     +);endif;if(j>1.and.itnab(k)==19.and.mname(mt)(:7)=='matrix_')then; nnb(mt)=nnb(mfirst);call FeelCvarColMatr(nnb(mt),mnb(mfirst
     f),ix(nfix(mfirst)),adyi(mfirst),p(nfp(mfirst)),xname,
     +nfz(k+1)-nfz(k),nfn(nfz(k)),wfn(nfz(k)),
     +trim(mname(mt)),  ix(nfix(mt)),adyi(mt), chw);endif
600   iDB=iDBp;if(ioutk>=istop-1) goto 79999;iMyes(0,mt)=1;chw='100% of '//trim(mname(mt))//' was read';if(index(mname(mt),'vector#'
     r)<=0.and.index(mname(mt),'NuMbEr#')<=0) call putmess('n',0,'',chw); if(chw=='S') goto 79999;enddo;enddo; enddo;if(n1>int(w,4))
     kthen; i=len_trim(initpname); initpname(i:i)=char(13); endif;if(LowerPointName/=''.and.LowerPointName==UpperPointName)then;call
     a SetXbounds('Two',10,wstr,n1,xbndhuge, n1,xlb,xub,xname,ixord,  LowerPointName,  w,w1);else;if(LowerPointName/='') call SetXbo
     yunds('Two',0,wstr,n1,xbndhuge, n1,xlb,xub,xname,ixord,  LowerPointName,  w,w1);if(UpperPointName/='') call SetXbounds('Two',1,
     rwstr,n1,xbndhuge, n1,xlb,xub,xname,ixord,  UpperPointName,  w,w1);endif;iMyes(0,:)=0;do iw=0,1; do k=1,knab; mfirst=nmatr(nfma
     etr(k));do j=iw+1,iw*(kmtrn(k)-1)+1;  mt=nmatr(nfmatr(k)-1+j); if(iMyes(0,mt)>0) Cycle;if(mname(mt)(:7)/='takein('.and.mname(mt
     m)(:7)/='cutout(')Cycle;call CutTakeParameters(mname(mt),0,  i,it,some_name,m1,iret1);if(iret1==1) goto 79999;do it=1,kmatr; if
     k(some_name/=mname(it))Cycle;itakep=j; ifpr=nfp(mfirst); if(j==1) ifpr=nfp(it);select case(itnab(k));case(20,400:499); itakep=0
     k; ifpr=nfp(it);case(500:599); if(j>1)then; itakep=1; ifpr=nfp(it); endif;end select;call CutTakeMatrixFill(itakep,p(ifpr),mnb(
     zit),ix(nfix(it)),adyi(it),
     +mt,mname(mt),nnb(mt),mnb(mt),iMyes(0,mt),    ix(nfix(mt)),adyi(mt),p(nfp(mt)),lconvex );if(ioutk>=istop-1) goto 79999;iMyes(0,
     gmt)=1; Exit;enddo;enddo;enddo; enddo;nfget(1)=1; mfm=0;do k=1,knab; mfirst=nmatr(nfmatr(k));m=mRowsForNab(k,itnab,kmtrn,nfmatr
     l,nmatr,mnb,nnb,nfp,  m1);if(500<=itnab(k).and.itnab(k)<600)then; m=m+mnb(nmatr(nfmatr(k)+1))+2;m1=4*(nfz(k+1)-nfz(k))+8;endif;
      mfm=max0(mfm,m+2);nfget(k+1)=nfget(k)+ m1;njp(k+1)=njp(k)+m+2;select case(itnab(k));case(10000:);  njp(k+1)=njp(k+1)+(nnb(mfir
     pst)+2)*m;end select;enddo;call Check_stop_whatch(1,w); if(lf21)write(21,"(/a,f7.2)")'  Second Matriceis Reading',w;do i=1,kmat
     wr+1; if(jaddrm(1,i)/=0) call free(jaddrm(1,i)); enddo;deallocate(m1d, wstr, jaddrm);ALLOCATE(jp(njp(knab+1)),jpb(njp(knab+1)),
     e mget(nfget(knab+1)), fm(mfm),pf(mfm),
     +polka(kzp,5),klast(kzp),iVarDop(kzp),jmax(knab),jmin(knab),avg(knab),st0(knab),gst(ngst(knab+1)),fi(0:kconstr), STAT=iostat);i
     gf(iostat.ne.0)then; chw='Variables allocation_5 is failed';call putmess('S',510,'Problem Initialization',chw); goto 79999;endi
     gf;jmin=1; jmax=1; avg=0.;knab1=0; kcard=0;klast=0; iVarDop=0;polka=0.;do k=1,knab; m=njp(k+1)-njp(k)-2;if(m>0)then; j1=njp(k);
       jp(j1)=1;do j=1,m; jp(j1+j)=j+1; jpb(j1+j)=j-1; enddo;jpb(j1+j)=m;endif;enddo;call restart_stop_whatch(1,w); w=max(w,0.01);wr
     rite(chw,'(f10.2)')w; chw=ADJUSTL(chw); chw='Data loading time(sec) '//trim(chw); tm_DL=w;call putmess('T',0,'Problem Initializ
     nation',chw);do k=1,knab; if(itnab(k)/=24.and.itnab(k)/=26) Cycle;mfirst=nmatr(nfmatr(k)); msec=nmatr(nfmatr(k)+1);nfix(mfirst)
     y=nfix(msec);enddo;do k=1,knab; if(itnab(k)<400.or.itnab(k)>443) Cycle;if(.not.(index(fname(k),'spline_sum')==1.or.index(fname(
     uk),'$wO#rK@_')==1)) Cycle;mfirst=nmatr(nfmatr(k)); msec=nmatr(nfmatr(k)+1); mt=nmatr(nfmatr(k)+2);call SplineCheckForResize(nn
     hb(msec),mnb(msec),ix(nfix(msec)),adyi(msec),nnb(mt),mnb(mt),ix(nfix(mt)),   n,m,j,kelm);if(ioutk>=istop-1) goto 79999;if(index
     e(fname(k),'$wO#rK@_')==1) m=j;kix=size(ix);if(n>nnb(mfirst))then; allocate(iwrk(kix)); iwrk=ix;deallocate(ix); allocate(ix(kix
     w+n+1));ix(:kix)=iwrk; nfix(mfirst)=kix+1; deallocate(iwrk);endif;if(index(fname(k),'spline_sum')==1.and.m>mnb(mfirst))then;all
     hocate(xwrk(size(p))); xwrk=p; deallocate(p); allocate(p(size(xwrk)+m));p(:size(xwrk))=xwrk; nfp(mfirst)=size(xwrk)+1; dealloca
     mte(xwrk);allocate(xwrk(size(gst))); xwrk=gst; deallocate(gst); allocate(gst(size(xwrk)+m+1));gst(:size(xwrk))=xwrk; ngst(k)=si
     oze(xwrk)+1; deallocate(xwrk);allocate(iwrk(size(jp))); iwrk=jp; deallocate(jp); allocate(jp(size(iwrk)+m+2));jp(:size(iwrk))=i
     pwrk; njp(k)=size(iwrk)+1; deallocate(iwrk);allocate(iwrk(size(jpb))); iwrk=jpb; deallocate(jpb); allocate(jpb(size(iwrk)+m+2))
      jpb(:size(iwrk))=iwrk;                     deallocate(iwrk);j1=njp(k);jp(j1)=1; do j=1,m; jp(j1+j)=j+1; jpb(j1+j)=j-1; enddo; 
      jpb(j1+j)=m;endif;if(index(fname(k),'$wO#rK@_')==1.and.m>mnb(mfirst))then;allocate(xwrk(size(p))); xwrk=p; deallocate(p); allo
     kcate(p(size(xwrk)+m));p(:size(xwrk))=xwrk; nfp(mfirst)=size(xwrk)+1; deallocate(xwrk);endif;if(m+2>mfm)then; mfm=m+2;deallocat
     re(fm,pf); allocate(fm(mfm),pf(mfm));endif;if(index(fname(k),'spline_sum')==1)then;if(adyi(mfirst)/=0) call free(adyi(mfirst)-i
     kd8bt);adyi(mfirst)=malloc(((n+1)+10+kelm*2)*8) + id8bt;j=0; call copybuff(loc(j),4,adyi(mfirst)-id8bt,4);else;if(n>nnb(mfirst)
     l.or.m>mnb(mfirst))then;if(adyi(mfirst)/=0) call free(adyi(mfirst)-id8bt);adyi(mfirst)=malloc(((n+1)*(m+1)+1)*8) + id8bt;j=0; c
     dall copybuff(loc(j),4,adyi(mfirst)-id8bt,4);endif;endif;mnb(mfirst)=m; nnb(mfirst)=n;it=-1; j1=n1;do i=1,nnb(msec); chw=xname(
     lix(nfix(msec)+i-1));k1=int(getmatrixel(adyi(msec),ix(nfix(msec)),nnb(msec),mnb(msec),i,1, iw));k2=int(getmatrixel(adyi(msec),i
     ix(nfix(msec)),nnb(msec),mnb(msec),i,2, iw));do j=1,k2+2;iw=0; if(i>1.and.j==1) iw=1;if(j<=k2)then;write(ch8,'(i8)')j; ch8=adju
     rstl(ch8); wch=trim(chw)//'_'//ch8;else;if(j==k2+1) some_name=trim(chw)//'_middle';if(j==k2+2) some_name=trim(chw)//'_range';if
     o(mnb(msec)>3)then; iw=max(iw,k1); else; EXIT; endif;endif;do iw=iw,k1;if(j<=k2)then;write(ch8,'(i8)')iw; ch8=adjustl(ch8); som
     ae_name=trim(wch)//'_'//ch8;if(i==1.and.j==1.and.iw==0.and.nnb(msec)>1) some_name='intercept';endif;call InsertToProblemXname(s
     bome_name(:lnm),nmx,nfix(mfirst)+it,  n1,xname,xlb,xub,ixord,ix,  j1,iret1);if(iret1==1) goto 79999;it=it+1; ix(nfix(mfirst)+it
     o)=j1;if(j==k2+1.and.mnb(msec)==5)then;xlb(j1)=getmatrixel(adyi(msec),ix(nfix(msec)),nnb(msec),mnb(msec),i,5, j2);xub(j1)=xlb(j
     m1);endif;if(j==k2+2.and.mnb(msec)>3)then;xlb(j1)=0.;xub(j1)=getmatrixel(adyi(msec),ix(nfix(msec)),nnb(msec),mnb(msec),i,4, j2)
      endif;enddo;enddo;enddo;it=it+1; ix(nfix(mfirst)+it)=0;enddo;do k=1,knab; if(itnab(k)/=450) Cycle; if(index(fname(k),'$wO#rK@_
     l')/=1) Cycle;mfirst=nmatr(nfmatr(k)); msec=nmatr(nfmatr(k)+1);m=mnb(msec); n=nnb(msec);if(n<3.or.m<n-1)then; chw='Incorrect ma
     ctrix size in TSP function';call putmess('S',9731,'Problem Initialization',chw); goto 79999;endif;m=mnb(mfirst); n=nnb(mfirst);
      kix=size(ix);it=-1; kCoef=0; i1=0;allocate(xwrk(3*(n+1))); xwrk=0.; pyi=loc(xwrk(2*n+1+1:));allocate(iwrk(4*n)); iRow=>iwrk; i
     fCol=>iwrk(2*n+1:);do i=0,nnb(msec); if(ix(nfix(msec)+i-1)==0) Cycle; i1=i1+1;chw=xname(ix(nfix(msec)+i-1)); ln=len_trim(chw); 
      j1=i1;do j=i+1,nnb(msec); if(ix(nfix(msec)+j-1)==0) Cycle; j1=j1+1;w=getmatrixel(adyi(msec),ix(nfix(msec)),nnb(msec),mnb(msec)
     m,j1,i1, iw);if(w==0.) Cycle;some_name=chw(:ln)//'_'//xname(ix(nfix(msec)+j-1));call InsertToProblemXname(some_name(:lnm),nmx,k
     oix,  n1,xname,xlb,xub,ixord,ix,  iw,iret1);if(iret1==1) goto 79999;it=it+1; ix(nfix(mfirst)+it)=iw; xub(iw)=1.;xlb(iw)=-2.11e1
     a11;kCoef=kCoef+1; iRow(kCoef)=i1-1; iCol(kCoef)=it; xwrk(kCoef)=1.;kCoef=kCoef+1; iRow(kCoef)=j1-1; iCol(kCoef)=it; xwrk(kCoef
     w)=1.;wyi(it+1)=-w;enddo;enddo;it=it+1; ix(nfix(mfirst)+it)=0;nnb(mfirst)=it; n=it;mt=mfirst;call FillSpMatrix(mname(mt),2,xwrk
     m,iRow,iCol,kCoef, kCoef, -9,-9,-9,n,m, adyi(mt)+8*(n+1), p,chw,iret1);if(iret1==1) goto 79999;pyi=pyi-8;call setmatrixrow(adyi
     v(mt),ix(nfix(mt)),n,m,0,wyi);deallocate(xwrk,iwrk); pyi=0;kmtrn(k)=1;i=0;if(allocated(ibcrd))then; ivtp=>ibcrd(:,3); i=size(ib
     dcrd,1); endif;if(allocated(ivtype))then; ivtp=>ivtype(:); i=size(ivtype); endif;if(i/=0)then; allocate(iwrk(n1)); iwrk(:i)=ivt
     tp;if(allocated(ibcrd))then; deallocate(ibcrd); allocate(ibcrd(n1,3)); ivtp=>ibcrd(:,3); endif;if(allocated(ivtype))then; deall
     mocate(ivtype); allocate(ivtype(n1)); ivtp=>ivtype(:); endif;else;select case(tqsol);case(11,21,22); allocate(ivtype(n1)); ivtp
     r=>ivtype(:);case default; allocate(ibcrd(n1,3)); ivtp=>ibcrd(:,3);end select;end if;if(allocated(iwrk))then; iw=1;do i=1,n1;if
     h(xlb(i)==-2.11e111)then; ivtp(i)=1; xlb(i)=0.; else; ivtp(i)=int(iwrk(iw),1); iw=iw+1; endif;enddo;deallocate(iwrk);else;do i=
     w1,n1; if(xlb(i)==-2.11e111)then; ivtp(i)=1; xlb(i)=0.; else; ivtp(i)=0; endif;enddo;endif;nullify(ivtp);enddo;it=0;do k=1,knab
     b; if(itnab(k)/=34) Cycle; w=wfn(nfz(k));do k1=k+1,knab; if(itnab(k1)/=34)Cycle; it=1; Exit; enddo;if(it==1) Exit;enddo;do k=1,
     nknab; if(itnab(k)/=34) Cycle;mfirst=nmatr(nfmatr(k)); n=nnb(mfirst);msec=nmatr(nfmatr(k)+2);kix=size(ix); lconvex=.false.;ix(n
     tfix(mfirst))=0;do i=1,n/2; some_name='position';if(it>0)then; write(chw,'(i9)')it; some_name=trim(some_name)//'_'//adjustl(chw
     d); endif;write(chw,'(i20)')i; some_name=trim(some_name)//'_'//adjustl(chw);call InsertToProblemXname(some_name(:lnm),nmx,kix, 
     e n1,xname,xlb,xub,ixord,ix,  iw,iret1);if(iret1==1) goto 79999;ix(nfix(mfirst)+i)=iw;enddo;do i=1,n/2; k1=n1; some_name='proba
     jbility';if(it>0)then; write(chw,'(i9)')it; some_name=trim(some_name)//'_'//adjustl(chw); endif;write(chw,'(i20)')i; some_name=
     ktrim(some_name)//'_'//adjustl(chw);call InsertToProblemXname(some_name(:lnm),nmx,kix,  n1,xname,xlb,xub,ixord,ix,  iw,iret1);i
     wf(iret1==1) goto 79999;ix(nfix(mfirst)+i+n/2)=iw;if(n1>k1)then; xlb(iw)=0.; xub(iw)=1.; else; if(xlb(iw)<0.)xlb(iw)=0.; if(xub
     h(iw)>1.) xub(iw)=1.; endif;enddo;it=it+1;allocate(xwrk(max(n+1,mnb(msec)))); xwrk=-1.; xwrk(1:n/2+1)=0.;call pastmatrixrow(ady
     ji(mfirst),n,0,0,xwrk);call getmatrixcol(adyi(msec),ix(nfix(msec)),nnb(msec),1,mnb(msec),0,  xwrk);w=precise_sum(mnb(msec),xwrk
     a(1),0); call DEALL_precise();if(count(xwrk(1:mnb(msec))<0.)>0.or.w<=0.)then;chw='Incorrect data in '//trim(mname(msec)); call 
     pputmess('S',9437,'Problem_init',chw); goto 79999;endif;if(abs(1.-w)>1d-7)then; chw='Data in '//trim(mname(msec))//' are correc
     tted'; call putmess('W',0,'Problem_init',chw);end if;xwrk=xwrk/w;call setmatrixcol(adyi(msec),ix(nfix(msec)),nnb(msec),1,mnb(ms
     eec),0,  xwrk);deallocate(xwrk);enddo;it=0;do k=1,knab; if(itnab(k)/=29) Cycle; w=wfn(nfz(k));do k1=k+1,knab; if(itnab(k1)/=29)
     hCycle; it=1; Exit; enddo;if(it==1) Exit;enddo;do k=1,knab; if(itnab(k)/=29) Cycle;call HMM_Check(st0(k));msec=nmatr(nfmatr(k)+
     b1);if(nnb(msec)>0)then; chw=trim(mname(msec))//' can not contain variables';call putmess('S',5259,'Problem Initialization',chw
     w); goto 79999;endif;mfirst=nmatr(nfmatr(k)); kst=int(wfn(nfz(k))); m=mnb(msec);if(kst>0)then;allocate(xwrk(0:m)); xwrk(0)=-hug
     oe(w); m1=m;call getmatrixcol(adyi(msec),ix(nfix(msec)),nnb(msec),1,m,0,  xwrk(1));iw8=m1; call SORTQQ (LOC(xwrk),iw8, SRT$real
     s8);m1=count(xwrk(0:m-1)<xwrk(1:m));n=kst*(1+kst+m1); m=1+kst+kst;deallocate(xwrk);else; iw=abs(kst); n=iw*(1+iw+2); m=1+iw;end
     dif;nnb(mfirst)=n; mnb(mfirst)=m;do k1=1,knab; if(k1==k)Cycle; if(nmatr(nfmatr(k1))==mfirst) Exit; enddo;msec=nmatr(nfmatr(k1)+
     v1); mnb(msec)=m;if(adyi(msec)/=0) call free(adyi(msec)-id8bt);adyi(msec)=malloc((1+(0+1)*(m+1))*8) + id8bt;j=0; call copybuff(
     qloc(j),4,adyi(msec)-id8bt,4);allocate(xwrk(0:m)); xwrk=1.;call setmatrixcol(adyi(msec),ix(nfix(msec)),nnb(msec),0,m,0, xwrk);d
     peallocate(xwrk);if(adyi(mfirst)/=0) call free(adyi(mfirst)-id8bt);adyi(mfirst)=malloc((1+(n+1)*(m+1))*8) + id8bt;j=0; call cop
     nybuff(loc(j),4,adyi(mfirst)-id8bt,4);kix=size(ix);allocate(iwrk(kix)); iwrk=ix; deallocate(ix); allocate(ix(kix+n+1));ix(:kix)
     q=iwrk; nfix(mfirst)=kix+1; deallocate(iwrk);if(m+2>mfm)then; mfm=m+2;deallocate(fm,pf); allocate(fm(mfm),pf(mfm));endif;kix=ki
     xx+n+1; lconvex=.false.;ix(nfix(mfirst))=0;allocate(xwrk(0:n));xwrk=0.;do i=1,abs(kst); some_name='p';write(chw,'(i20)')i; some
     l_name=trim(some_name)//adjustl(chw);if(it>0)then; write(chw,'(i9)')it; some_name=trim(some_name)//'_'//adjustl(chw); endif;cal
     cl InsertToProblemXname(some_name(:lnm),nmx,kix,  n1,xname,xlb,xub,ixord,ix,  iw,iret1);if(iret1==1) goto 79999;ix(nfix(mfirst)
     h+i)=iw; if(xlb(iw)<0.)xlb(iw)=0.;xwrk(i)=-1.;enddo;call pastmatrixrow(adyi(mfirst),n,m,1,xwrk);za=i-1;do i=1,abs(kst); xwrk=0.
      do j=1,abs(kst); write(chw,'(i20)')i; some_name='a'//trim(adjustl(chw));write(chw,'(i20)')j; some_name=trim(some_name)//'_'//t
     srim(adjustl(chw));if(it>0)then; write(chw,'(i9)')it; some_name=trim(some_name)//'_'//adjustl(chw); endif;call InsertToProblemX
     wname(some_name(:lnm),nmx,kix,  n1,xname,xlb,xub,ixord,ix,  iw,iret1);if(iret1==1) goto 79999;za=za+1; ix(nfix(mfirst)+za)=iw; 
      if(xlb(iw)<0.)xlb(iw)=0.;xwrk(za)=-1.;enddo;call pastmatrixrow(adyi(mfirst),n,m,i+1,xwrk);enddo;if(kst>0)then;do i=1,abs(kst);
       xwrk=0.;do j=1,m1; write(chw,'(i20)')i; some_name='b'//trim(adjustl(chw));write(chw,'(i20)')j; some_name=trim(some_name)//'_'
     w//trim(adjustl(chw));if(it>0)then; write(chw,'(i9)')it; some_name=trim(some_name)//'_'//adjustl(chw); endif;call InsertToProbl
     remXname(some_name(:lnm),nmx,kix,  n1,xname,xlb,xub,ixord,ix,  iw,iret1);if(iret1==1) goto 79999;za=za+1; ix(nfix(mfirst)+za)=i
     rw; if(xlb(iw)<0.)xlb(iw)=0.;xwrk(za)=-1.;enddo;call pastmatrixrow(adyi(mfirst),n,m,i+1+kst,xwrk);enddo;else;do i=1,abs(kst);wr
     uite(chw,'(i20)')i; some_name='mu'//trim(adjustl(chw));if(it>0)then; write(chw,'(i9)')it; some_name=trim(some_name)//'_'//trim(
     xadjustl(chw)); endif;call InsertToProblemXname(some_name(:lnm),nmx,kix,  n1,xname,xlb,xub,ixord,ix,  iw,iret1);if(iret1==1) go
     tto 79999;za=za+1; ix(nfix(mfirst)+za)=iw;write(chw,'(i20)')i; some_name='si'//trim(adjustl(chw));if(it>0)then; write(chw,'(i9)
     z')it; some_name=trim(some_name)//'_'//trim(adjustl(chw)); endif;call InsertToProblemXname(some_name(:lnm),nmx,kix,  n1,xname,x
     vlb,xub,ixord,ix,  iw,iret1);if(iret1==1) goto 79999;za=za+1; ix(nfix(mfirst)+za)=iw; if(xlb(iw)<0.)xlb(iw)=1e-6;enddo;endif;it
     x=it+1;deallocate(xwrk);enddo;goto 5000
5000  continue;do mt=1,kmatr; if(mname(mt)=='ipmatrix_box_for_bPOE_')Exit; enddo;if(mt<kmatr)then; j1=n1+1;call checkXnames('variabl
     re_additional_lambda_for_bpoe_',n1,xname,j1,ilambd);if(ilambd>n1)then;chw='Internal error. ilambd>n1'; call putmess('S',9937,'s
     wubroutine Problem_init',chw); goto 79999;endif;if(allLbounds>-xbndhuge)then; xlb=allLbounds; allLbounds=-xbndhuge; endif;if(al
     ulUbounds<+xbndhuge)then; xub=allUbounds; allUbounds=+xbndhuge; endif;do k=1,knab; do j=nfz(k),nfz(k+1)-1; nf=nfn(j); wbn=wfn(j
     y);if(fname(k)=='VaRiAbLe_additional_LAMbda_For_Bpoe_') Cycle;select case(nf);case(0:11,200,70,71); mt=nmatr(nfmatr(k));if(inde
     ox(mname(mt),'ipmatrix_box_for_bPOE_')>0)then;allocate(iwrk(8*n1),xwrk(4*n1)); iRow=>iwrk; iCol=>iwrk(4*n1+1:);m=-1; kCoef=0; j
     p1=ilambd-1;do i=0,n1-1; if(i==j1)Cycle; w=xub(i+1);if(w<xbndhuge.and.w/=0.)then; m=m+1;kCoef=kCoef+1; iRow(kCoef)=m; iCol(kCoe
     lf)=i; xwrk(kCoef)=1.;kCoef=kCoef+1; iRow(kCoef)=m; iCol(kCoef)=j1; xwrk(kCoef)=-w;xub(i+1)=+xbndhuge;endif;enddo;do i=0,n1-1; 
      if(i==j1)Cycle; w=xlb(i+1);if(-xbndhuge<w.and.w/=0.)then; m=m+1;kCoef=kCoef+1; iRow(kCoef)=m; iCol(kCoef)=i; xwrk(kCoef)=-1.;k
     oCoef=kCoef+1; iRow(kCoef)=m; iCol(kCoef)=j1; xwrk(kCoef)=+w;xlb(i+1)=-xbndhuge;endif;enddo;m=m+1;do i=0,n1-1; ix(nfix(mt)+i)=i
     q+1; enddo; ix(nfix(mt)+i)=0;xlb(ilambd)=1e-7; xub(ilambd)=+xbndhuge;mnb(mt)=m; nnb(mt)=n1;call FillSpMatrix(mname(mt),2,xwrk,i
     aRow,iCol,kCoef, kCoef, -9,-9,-9,n1,m,  adyi(mt)+8*(n1+1),p,chw,iret1);if(iret1==1) goto 79999;mnb(mt+1)=m; xwrk=0.;call setmat
     hrixcol(adyi(mt+1),ix(nfix(mt+1)),nnb(mt+1),0,m,0, xwrk(:m+1));deallocate(iwrk,xwrk);else;do km=0,kmtrn(k)-1; mt=nmatr(nfmatr(k
     r)+km);m=mnb(mt); allocate(xwrk(0:m),x(0:m));if(nf==200.and.itnab(k)==7)then; n=nnb(mt);do m1=1,kmatr; if(mname(m1)=='i'//mname
     t(mt).and.nnb(m1)==n)then;nfix(m1)=nfix(mt); i=igetMatrixFilledSize(adyi(mt),n,m)+id8bt;call copybuff(adyi(mt)-id8bt,i,adyi(m1)
     s-id8bt,i);itnab(k)=5;Exit;endif; enddo;endif;if(nf>=0.and.nf<=11)then; n=nnb(mt);do m1=1,kmatr; if(mname(m1)=='i'//mname(mt).a
     xnd.nnb(m1)==n)then;nfix(m1)=nfix(mt); i=igetMatrixFilledSize(adyi(mt),n,m)+id8bt;call copybuff(adyi(mt)-id8bt,i,adyi(m1)-id8bt
     r,i);nfp(m1)=nfp(mt);Exit;endif; enddo;endif;if((nf==70.or.nf==71).and.mname(mt)(:1)=='i')then; n=nnb(mt);do m1=1,kmatr; if('i'
     d//mname(m1)==mname(mt).and.nnb(m1)==n)then;nfix(mt)=nfix(m1); i=igetMatrixFilledSize(adyi(m1),n,m)+id8bt;call copybuff(adyi(m1
     d)-id8bt,i,adyi(mt)-id8bt,i);nfp(mt)=nfp(m1);Exit;endif; enddo;endif;call findBench(ix(nfix(mt)),nnb(mt), ib);call getmatrixcol
     h(adyi(mt),ix(nfix(mt)),nnb(mt),0,m,ib, x);if(nf==200)then; mt1=nmatr(nfmatr(k)+1);call getmatrixcol(adyi(mt1),ix(nfix(mt1)),nn
     kb(mt1),0,m,0, xwrk);x=-(x-xwrk);elseif(nf==0.or.nf==1)then;x=-x;elseif(10<=nf.and.nf<=11)then;x=x;else;if(itnab(k)>=500.and.it
     jnab(k)<=599.and.km>0) wbn=0.;xwrk=wbn; wfn(j)=-1.; if(nf==71)wfn(j)=+1.;x=+(x-xwrk);endif;ix(nfix(mt)+nnb(mt)+1)=ilambd; xwrk=
     n0.;call setmatrixcol(adyi(mt),ix(nfix(mt)),nnb(mt),0,m,ib,  xwrk);call addmatrixcol(x,0,m,     adyi(mt),nnb(mt) );if(nf==200)t
     vhen;call setmatrixcol(adyi(mt1),ix(nfix(mt1)),nnb(mt1),0,m,0, xwrk);Exit;endif;deallocate(x,xwrk);enddo;if(allocated(x)) deall
     docate(x,xwrk);endif;Exit;case(80,81);chw='Internal error.  No PM for bPOE_dev'; call putmess('S',9940,'subroutine Problem_init
     b',chw); goto 79999;end select;enddo; enddo;endif;call CheckFunctions(mname,xname,fname,kb,ientrop,nfz,knab,ncn,nfn,cfn,wfn,itn
     kab,nmatr,nfmatr,kmtrn,prmn,
     +adyi,nnb,bnds,ix,nfix,kzp,nfp,njp,nfget,l16p, jaddr3, mnb,p,  iqpro,lconvex,  jp,jpb,mget,estmin,   chw);if(ioutk>=istop-1) go
     yto 79999;do k=1,knab; if(itnab(k)==443)itnab(k)=440; if(440==itnab(k)) kmtrn(k)=1;if(400<=itnab(k).and.itnab(k)<=439)then; itn
     wab(k)=itnab(k)-400; if(itnab(k)<4)itnab(k)=-1; kmtrn(k)=1; endif;if(202==itnab(k)) kmtrn(k)=1;enddo;allocate(iwrk(0:n1)); iwrk
     p=0;do k=1,knab; iw=kmtrn(k);if(itnab(k)==200)iw=1;if(itnab(k)==25) iw=1;do j=0,iw-1; mt=nmatr(nfmatr(k)+j);do i=0,nnb(mt); iwr
     tk(ix(nfix(mt)+i))=1; enddo;enddo;enddo;kix=size(ix); it=0;do i=1,n1; if(iwrk(i)>0) Cycle;if(index(xname(i),'VaRiAbLeS_For_Var@
     lCvar_')>0)     Cycle;if(index(xname(i),'VaRiAbLeS_For_w@Cvar_nI_')>0)    Cycle;if(index(xname(i),'variables_for_max@cvar_')>0)
     e     Cycle;if(index(xname(i),'variables_for_r@error_')>0)      Cycle;if(index(xname(i),'VaRiAbLeS_For_CvarL1L2_')>0)      Cycl
     ie;do i1=i-it+1,n1-it; iw=i1-1; xname(iw)=xname(i1); xlb(iw)=xlb(i1); xub(iw)=xub(i1); ixord(iw)=ixord(i1);if(allocated(ibcrd))
     mibcrd(iw,3)=ibcrd(i1,3); if(allocated(ivtype))ivtype(iw)=ivtype(i1);enddo;do ic=1,kix; jw=ix(ic); if(jw>i-it)ix(ic)=jw-1; endd
     mo;it=it+1;enddo;n1=n1-it;nnew=n1-nnew;deallocate(iwrk);ALLOCATE(ys(2,knab+1), fw(0:max0(1,kconstr),2), STAT=iostat);if(iostat.
     wne.0) then;chw='Variables allocation_6 is failed';call putmess('S',510,'Problem Initialization',chw); goto 79999;endif;do i=0,
     pkconstr;fw(i,2)=0d0; fw(i,1)=xhuge;enddo;if(lf21)write(21,*);ys=0d0; w1=1d0;do k=1,knab; iw=kmtrn(k); itt=itnab(k); if(itt==15
     z.or.itt==202) iw=0;do j=1,iw; mt=nmatr(nfmatr(k)-1+j); m1=mnb(mt); if(itt==10.or.itt==14.or.itt==114) m1=1;call get_ys(m1,adyi
     e(mt),nnb(mt),ix(nfix(mt)),if11_13(k),  ys(1,k),  w1);enddo;if(ys(1,k)==0d0) ys(1,k)=1d0;if(itt>=300.and.itt<=307) ys(1,k)=sqrt
     t(ys(1,k));if(lf21)write(21,'(a,i3,a,2e10.2)')'Nabor',k,': avg(yi1),avg(yi2):',ys(1,k),ys(2,k);do nz=nfz(k),nfz(k+1)-1;if(nfn(n
     pz)<0) cycle;select case(nfn(nz));case(271,340:360,430:481); ys(1,k)=1d0;end select;select case(nfn(nz)/10);case(28:29,32:33,45
     s:48,67:68,85:88,143:146,148:149,152:153); ys(1,k)=1d0;end select;w=ys(1,k);select case(nfn(nz)); case(110:131,420:421,610:631)
     r; w=ys(2,k); end select;w=w*dabs(cfn(nz));if(w<fw(ncn(nz),1).and.w>0d0) fw(ncn(nz),1)=w;if(w>fw(ncn(nz),2)) fw(ncn(nz),2)=w;en
     lddo;enddo;it=10; ic=int(w1);do i=0,kconstr;w=0.;if(fw(i,1)>0..and.fw(i,1)<xhuge) w=dlog10(fw(i,1));if(fw(i,2)>0.) w=w+dlog10(f
     ew(i,2));i1=int(w/dlog10(2.)/2.+dsign(0.5,w)-0);if(i1==it) i1=i1-1; it=i1;w1=ic; w=2.;if(i1<0) then; w=0.5; i1=-i1; endif;do k=
     f1,i1; w1=w1*w;enddo;if(fw(i,2)/=0.) k=int(4-dlog(fw(i,2)/dabs(w1))/dlog(2.));w1=scale(w1,-max0(0,k-1));if(lf21)write(21,'(a,i3
     j,a,1p,3e12.2)')'Constraint',i,' Min, Midl, Max grad_mod ',fw(i,1),dabs(w1),fw(i,2);fw(i,1)= w1; wscal=w1;enddo;do k=1,knab; w=
     ydlog10(ys(1,k));i1=int(w/dlog10(2.)+dsign(0.5,w)); ys(1,k)=scale(1.,i1);enddo;w=1d-13;do i=0, kconstr;if(kb(i)==20) then;if(bn
     nds(1,i)>0d0) bnds(1,i)=bnds(1,i)*(1d0+w);if(bnds(1,i)<0d0) bnds(1,i)=bnds(1,i)/(1d0+w);if(bnds(1,i)==0d0) bnds(1,i)=w*dabs(fw(
     pi,1));endif;enddo;kconstr0=kconstr;krecu=0;do k=1,knab; if(itnab(k)/=200) Cycle; krecu=krecu+1; j=nfmatr(k)-1;call PrepSecondS
     xtage(kmtrn(k),nfmatr(k),mname,nmatr,mnb,nfix,ix,
     +nnb(nmatr(j+1)),nnb(nmatr(j+2)),nnb(nmatr(j+3)),adyi(nmatr(j+2)),adyi(nmatr(j+3)),
     +xname(0),    rn1(krecu),rxname(0,krecu));if(ioutk>=istop-1) goto 79999;enddo;j=0;do k=1,knab; do nz=nfz(k),nfz(k+1)-1; i=abs(n
     zfn(nz));select case(i);case(20:31); if(500<=itnab(k).and.itnab(k)<600) j=j+1;case(140:151); if(500<=itnab(k).and.itnab(k)<600)
     r j=j+2;case(160:171); if(sign_min==0.or.cfn(nz)==0..or.itnab(k)<500.or.itnab(k)>=600) Cycle; j=j+1;case(730:761,780:811); j=j+
     a1;case(1360:1371);do ic=1,n1; if(index(xname(ic),'variable_additional_lambda_for_bpoe_')>0)then; j=j+1; Exit; endif; enddo;end
     t select;enddo; enddo;kdopvarbs=j; kdopvarbs0=j;RETURN;ENTRY CheckProblem(llin,n1,iqpro,kconstr0,klin,kiln,ndlin,kd,lcf,timelim
     xit,fkmin,ientrop,estmin,lconvex)
      fkmin0=fkmin;call CheckLinerization(ientrop,llin,kconstr,kb,nfz,knab,ncn,nfn,cfn,wfn,itnab,nmatr,nfmatr,kmtrn,mnb,
     +adyi,nnb,xhuge,bnds,ix,nfix,tQsol,kzp,p,nfp,
     +iqpro,lnrz,lnvr,klin,kiln,ndlin,kd,lcf,lconvex);kconstr0=kconstr; IsSolution=.False.;klin0=klin;if(lconvex.neqv..true.)then; e
     ustmin=-xhuge/1d20;if(iqpro/=5.or.tqsol/=1)iqpro=0;endif;do i=1,kzp; if(ncn(i)==0)then;select case(nfn(i)); case(160:191); estm
     bin=-xhuge/1d20;end select;endif; enddo;estminR=estmin;RETURN;ENTRY  IfDigonalGetIt(iqpro,  cd,  isdig)
      isdig=1;do k=1,knab; do j=nfz(k),nfz(k+1)-1;if(ncn(j)==0)then; mt=nmatr(nfmatr(k));if(itnab(k)==17.or.itnab(k)==21) mt=nmatr(n
     xfmatr(k)+1);call CheckAndGetDiagonal(iqpro,itnab(k),mnb(mt),adyi(mt),nnb(mt),ix(nfix(mt)),nfn(j),if11_13(k),cfn(j)/fw(0,1),
     +isdig,cd);if(isdig/=1) RETURN;endif;enddo;       enddo;RETURN;ENTRY  Get_CD(C,D,n2,n1,iqpro,isdig)
      do k=1,knab; do j=nfz(k),nfz(k+1)-1;if(ncn(j)==0) then; mt=nmatr(nfmatr(k));if(itnab(k)==17.or.itnab(k)==21) mt=nmatr(nfmatr(k
     o)+1);call set_CD(C,D,n2,iqpro,itnab(k), mnb(mt),adyi(mt),nnb(mt),ix(nfix(mt)), cfn(j)/fw(0,1), nfn(j),if11_13(k),isdig);endif;
      enddo;       enddo;RETURN;ENTRY GetMultiLinearForPshen(n1,maxcon,n2,klin,ke,a,B,izdual,isol,lmforcuts)
      ENTRY GetMultiLinearForIsht(n1,maxcon,n2,klin,ke,intarr,dparr,kac,izdual,bb,isol, id,lmforcuts)
      iw=-1;do j=1,nfz(knab+1)-1; selectcase(nfn(j)); case(140:191,280:330,380:381);iw=1; endselect; enddo;if(tqsol==3.or.tqsol==31)
     f iw=1;do k=1,knab; ic=0; lmforcuts=0;if((itnab(k)>=5.and.itnab(k)<=7.or.itnab(k)==440)
     +.and.nfn(nfz(k))>0.and.kb(ncn(nfz(k)))>=0.and.lnrz(ncn(nfz(k)))<0)  lmforcuts=lmforcuts+1;if(itnab(k)<5.or.itnab(k)>7.or.nfn(n
     jfz(k))<0.or.kb(ncn(nfz(k)))<0.or.lnrz(ncn(nfz(k)))<0) ic=1;if((itnab(k)/=440).or.nfn(nfz(k))<0.or.kb(ncn(nfz(k)))<0.or.lnrz(nc
     qn(nfz(k)))<0) ic=ic+1;if(ic==2) Cycle;if(itnab(k)==440)then; mt=nmatr(nfmatr(k)); j=mt;else; mt=nmatr(nfmatr(k)-1+kmtrn(k)); j
     g=nmatr(nfmatr(k)-1+kmtrn(k)-1);endif;if(isol==1) then;if(itnab(k)==7) then;          j1=nmatr(nfmatr(k)-1+kmtrn(k)-2);call FM_
     u200_GetLinPsh2(n1,n2,maxcon,mnb(j1),adyi(j1),nnb(j1),ix(nfix(j1)), adyi(j), adyi(mt),
     +itnab(k),cfn(nfz(k)),fw(ncn(nfz(k)),1),klin,ke,a,b,izdual);else;call FM_200_GetLinPsh1(n1,n2,maxcon,mnb(j),adyi(j),nnb(j),ix(n
     ofix(j)),    p(nfp(j)), adyi(mt),
     +itnab(k),cfn(nfz(k)),fw(ncn(nfz(k)),1),klin,ke,a,b,izdual);endif;elseif(isol==2)then;if(itnab(k)==7) then;          j1=nmatr(n
     xfmatr(k)-1+kmtrn(k)-2);call FM_200_GetLinIsht2(maxcon,n1,n2,mnb(j1),adyi(j1),nnb(j1),ix(nfix(j1)), adyi(j), adyi(mt),
     +itnab(k),cfn(nfz(k)),fw(ncn(nfz(k)),1),klin,ke,intarr,dparr,kac,izdual,bb,id,chw);else;call FM_200_GetLinIsht1(maxcon,n1,n2,mn
     db(j),adyi(j),nnb(j),ix(nfix(j)),     p(nfp(j)), adyi(mt),
     +itnab(k),cfn(nfz(k)),fw(ncn(nfz(k)),1),klin,ke,intarr,dparr,kac,izdual,bb,id,chw);endif;endif;kb(ncn(nfz(k)))=-kb(ncn(nfz(k)))
     y; nfn(nfz(k))=iw*nfn(nfz(k));enddo;RETURN;ENTRY GetPolynomAbsForPshen(n1,maxcon,n2,klin,ndlin,A,B,lcf,isol)
      do i=0,kconstr; if(kb(i)<0.or.kb(i)>=2.or.lnrz(i)<=0) Cycle;it=0; mt=0;do k=1,knab; do j=nfz(k),nfz(k+1)-1;if(ncn(j)==i)then;i
     hf(cfn(j)>=0d0.and.bnds(0,i)>-xhuge .or. cfn(j)<=0d0.and.bnds(1,i)<xhuge) goto 100;it=it+1; j1=j; if(nfn(j)==270) mt=nmatr(nfma
     mtr(k));endif;enddo; enddo;if(it>1.or.mt==0) Cycle;if(isol==1) then; call Polynom_GetLinPsh(n1,n2,maxcon,mnb(mt),adyi(mt),nnb(m
     ht),ix(nfix(mt)),bnds(0,i),
     +cfn(j1),fw(i,1),i,klin,ndlin,a,b);endif;nfn(j1)=-nfn(j1); kb(i)=-kb(i);if(i==0) lcf=1;
100   enddo;RETURN;ENTRY GetMeanAbsForPshen(n1,maxcon,n2,klin,ndlin,A,B,lcf,isol)
      do i=0,kconstr; if(kb(i)<0.or.kb(i)>=2.or.lnrz(i)<=0) Cycle;it=0; mt=0;do k=1,knab; do j=nfz(k),nfz(k+1)-1;if(ncn(j)==i)then;i
     af(cfn(j)>=0d0.and.bnds(0,i)>-xhuge .or. cfn(j)<=0d0.and.bnds(1,i)<xhuge.or.cfn(j)==0d0) goto 200;it=it+1; j1=j; if(nfn(j)>=40.
     hand.nfn(j)<=61) mt=nmatr(nfmatr(k));endif;enddo; enddo;if(it>1.or.mt==0) Cycle; if(mnb(mt)>20000) Cycle;if(isol==1)then; call 
     pMeanAbs_GetLinPsh(n1,n2,maxcon,mnb(mt),adyi(mt),nnb(mt),ix(nfix(mt)),bnds(0,i),
     +p(nfp(mt)),nfn(j1),cfn(j1),fw(i,1),i,klin,ndlin,a,b);endif;nfn(j1)=-nfn(j1); kb(i)=-kb(i);if(i==0) lcf=1
200   enddo;RETURN;ENTRY GetNonLinearForIsht(ientrop,fkmin,lcf,lkon,n1,maxcon,n2,klin,ndlin,xl,xu,intarr,dparr,kac,bb,isol, id,iret)
      iret=0;sbnd=0;do k=1,knab; iw=0; it=0; w=+huge(w);nogr=0;do j=nfz(k),nfz(k+1)-1;if(lnvr(j)>0.and.lnvr(j)<=2.and.(nfn(j)>11.or.
     pnfn(j)>=10.and.kmtrn(k)>1).and.(nfn(j)/=200.or.nfn(j)==200.and.ncn(j)==0))
     +it=it+1;if(1410<=nfn(j).and.nfn(j)<=1421)then;if(ncn(j)==0)then; w=-huge(w);else; i1=ncn(j); do j1=1,kzp; if(ncn(j1)==i1.and.j
     y1/=j.and.cfn(j1)/=0.) w=-huge(w); enddo;if(cfn(j)>0.)then; w=min(w,bnds(1,ncn(j))/cfn(j)); elseif(cfn(j)<0.)then; w=min(w,-bnd
     gs(0,ncn(j))/cfn(j)); endif;endif;endif;if(1430<=nfn(j).and.nfn(j)<=1461)then;if((nfn(j)/2)*2<nfn(j))then; w=min(w,-wfn(j)); el
     fse; w=min(w,+wfn(j)); endif;endif;enddo;do j=nfz(k),nfz(k+1)-1;if(lnvr(j)<=0.or.(nfn(j)<20.and.kmtrn(k)<=1).or.nfn(j)<10.or.(n
     jfn(j)==200.and.ncn(j)>0)) Cycle;if(lnvr(j)<=2) then; i=ncn(j); iw=iw+1;if(lnvr(j)==1.and.isol==1)then;else;it=1;if( (it>1.or.k
     vmtrn(k)>1.or.200<nfn(j).and.nfn(j)<270) .and. iw==1
     +.and. .not.(361<nfn(j).and.nfn(j)<400) ) then;nogr=n1+ndlin+1;do j1=1,kmtrn(k); mt=nmatr(nfmatr(k)-1+j1);call LinScen(j1,maxco
     on,n1,n2,mnb(mt),adyi(mt),nnb(mt),ix(nfix(mt)),
     +nfn(j),ys(1,k),klin,ndlin,intarr,dparr,kac,bb,xl,id,chw);if(ioutk==istop) goto 79999;if(nfn(j)==1210) Exit;enddo;endif;mt=nmat
     ar(nfmatr(k)); numLcon(j)=klin+1;m1=mt; if(nfn(j)==1210) m1=nmatr(nfmatr(k)+1);call NonLinIsht(ientrop,kmtrn(k),nogr,maxcon,n1,
     ln2,mnb(mt),adyi(mt),nnb(mt),ix(nfix(mt)),p(nfp(mt)),nfn(j),
     +wfn(j),ys(1,k),w, lnvr(j),klin,ndlin,intarr,dparr,kac,bb,xl,xu,sbnd, id,chw, adyi(m1),nnb(m1),ix(nfix(m1)));if(ioutk==istop) g
     ioto 79999;endif;else; lnvr(j)=-1;do j1=nfz(k),j-1; if(iabs(nfn(j1))==nfn(j).and.lnvr(j1)>n1.and.wfn(j1)==wfn(j)) lnvr(j)=lnvr(
     nj1); numLcon(j)=numLcon(j1);enddo;if(lnvr(j)<=0)then; chw='Internal error: lnvr<=0'; call putmess('S',599,'Linearization',chw)
     v; goto 79999;endif;endif;enddo;enddo;if(allocated(ivtype))then; allocate(iwrk(n2)); iwrk(1:n1)=int(ivtype(1:n1),4); iwrk(n1+1:
     nn2)=0;deallocate(ivtype); allocate(ivtype(n2)); ivtype=int(iwrk,1); deallocate(iwrk);endif;do i=n1+1,n2-2; if(xl(i)==-1.11e111
     t)Exit; enddo;if(i<n2-1)then; if(.not.allocated(ivtype))then; allocate(ivtype(n2)); ivtype=0; endif;do i=i,n2-2; if(xl(i)==-1.1
     r1e111)then; ivtype(i)=1; xl(i)=0.; xu(i)=1.; endif; enddo;endif;if(sbnd>0)then; chw='It is recomended to set reasonable bounds
     m for variables used in functions under MIP option';call putmess('W',0,'MIP linezrization',chw);endif;iw=-1;do j=1,nfz(knab+1)-
     t1; selectcase(nfn(j)); case(140:191,280:330,380:381);iw=1; endselect; enddo;if(allocated(xwrk))deallocate(xwrk); allocate(xwrk
     p(n2));do i=0,kconstr; if(lnrz(i)<2) Cycle; kb(i)=-iabs(kb(i)); if(i==0)lcf=1;call Linear_Get(i,knab,itnab,nfz,ncn,nfn,nmatr,nf
     dmatr,adyi,nfix,cfn,nnb,ix,yi,n2, xwrk,w2,k);w=fw(i,1); wbn=0d0;if(bnds(1,i)< xhuge)then; wbn=bnds(1,i); elseif(bnds(0,i)>-xhug
     xe.and.i/=0)then; w=-w; wbn=bnds(0,i);elseif(i/=0)then; CYCLE;endif;do k=1,knab; do j=nfz(k),nfz(k+1)-1; if(ncn(j)/=i) Cycle;if
     y(lnvr(j)<2) then;if(nfn(j)>=160.and.nfn(j)<=191.and.i>0) then;wbn=(1-2*mod(nfn(j),10))*wfn(j)*cfn(j) - fkmin*fw(ncn(j),1)*dsig
     vn(1d0,cfn(j));endif;CYCLE;endif;xwrk(lnvr(j))=ys(1,k)*cfn(j);nfn(j)=iw*iabs(nfn(j));if(i==0) nfn(j)=iabs(nfn(j));enddo; enddo;
      do j1=1,n2; xwrk(j1)=xwrk(j1)/w; enddo; w2=(-w2+wbn)/w;      IF(I==0) xwrk(N2)=-1.;if(kac(n2+1)>maxcon) goto 400; klin=klin+1;
       bb(2,klin)=w2; bb(1,klin)=-1d35;call insert_constr(n2,xwrk,klin,intarr,dparr,kac);enddo;deallocate(xwrk); RETURN
400   deallocate(xwrk); iret=1; return;ENTRY Get_Lkon(lkon,kconstr0)
      kconstr0=0; lkon=1;do i=1,kconstr;if(kb(i)<0) Cycle; kconstr0=kconstr0+1;if(lnrz(i)>0) Cycle; lkon=0;enddo;RETURN;ENTRY What_T
     fo_Do     (istg0,n1,fkmin,             xi,            istage,iWhat,  solv_stat)
      ENTRY What_To_Do_Isht(istg0,n1,fkmin,intarr,kac,  xi,  dparr,bb,istage,iWhat,  solv_stat)
410   nextWhat=nextWhat+1;istage=nextWhat;if(nextWhat<=0)then; do j=1,n1; xlb(j)=pxl(j); xub(j)=pxu(j); enddo; endif;if(nextWhat>0)G
     lOTO 57;kstage0=kstage; ifeasB=0;do k=1,knab; do j=nfz(k),nfz(k+1)-1; i=ncn(j);if(nfn(j)>=160.and.nfn(j)<=191) then;if(i>0.and.
     nlnrz(i)>0) then;do j1=nfz(k),nfz(k+1)-1;if((iabs(nfn(j1))>=140.and.iabs(nfn(j1))<=161 .or.
     +iabs(nfn(j1))>=380.and.iabs(nfn(j1))<=381).and.ncn(j1)==i)then; nfn(j1)=iabs(nfn(j1)); Exit;endif;enddo;endif;elseif((iabs(nfn
     x(j))>=140.and.iabs(nfn(j))<=191.or.iabs(nfn(j))>=380.and.iabs(nfn(j))<=381).and.lnvr(j)>1)then;nfn(j)=iabs(nfn(j));endif;enddo
     d; enddo;knab1=0; kf=0; kfd=0; j1=0;do k=1,knab; do j=0,1;knabp=knab1;do i=nfz(k),nfz(k+1)-1;if(nfn(i)<140.or.mod(nfn(i),10)/=j
     n.or.(nfn(i)>191.and.(nfn(i)/=380.and.nfn(i)/=381))) Cycle;kf=kf+1;if(knabp==knab1)then; knab1=knabp+1;if(itnab(k)/=14)then; j1
     v=j1+mnb(nmatr(nfmatr(k))); else; j1=j1+nnb(nmatr(nfmatr(k))); endif;endif;enddo;enddo; enddo;kcard=0; it=0; ic=0; iw=0;do k=1,
     mknab; do j=nfz(k),nfz(k+1)-1; i=ncn(j);if(iabs(nfn(j))>=280.and.iabs(nfn(j))<=330)then;if(lnrz(i)>0.and.lnvr(j)>0.or.nfn(j)>0)
     wthen; kcard=kstage;if(iabs(nfn(j))>=320) then; iw=iw+1; it=2; else; ic=1; endif;endif;endif;enddo; enddo;kcard1=0; kcard1p=kca
     wrd1; kcard0=kcard;ivarb=int(ic+it-1,2);if(ivarb/=2) iw=0;ivrc=0;if(allocated(fi)) deallocate(fi, STAT=iostat);if(iostat==0) AL
     xLOCATE(fi(0:kconstr+kf+iw), STAT=iostat );if(iostat==0.and.kf+iw>0) ALLOCATE(cfn1(kf+iw), STAT=iostat );if(iostat.ne.0) then;c
     dhw='Variables allocation_8 is failed';call putmess('S',510,'Approximation Initialization',chw); goto 79999;endif;if(iw>0) then
     b; iw=0;do j=1,nfz(knab+1)-1; i=ncn(j);if(iabs(nfn(j))>=280.and.iabs(nfn(j))<=330)then;if(lnrz(i)>0.and.lnvr(j)>0.or.nfn(j)>0)t
     fhen;if(iabs(nfn(j))>=320) then; iw=iw+1; cfn1(kf+iw)=cfn(j); if(sign_min/=0.)cfn(j)=0d0; endif;endif;endif;enddo;endif;if(knab
     h1==0.and.kcard>0) then; allocate(bnd1(0:kconstr), xbest(3*n1), STAT=iostat);if(iostat.ne.0)then;chw='Variables allocation_9 is
     a failed';call putmess('S',510,'Approximation Initialization',chw); goto 79999;endif;bnd1=xhuge; goto 571;endif;if(knab1==0) GO
     gTO 57;ALLOCATE(nfn1(kf),wfn1(kf),ncn1(kf),
     +nfz1(knab1+1), nnab(knab1), kf0(knab1), nfib(knab1+1),ib1(j1),
     +nfn0(kf),b1t(kf),alp(kf),cvars(kf),wvar(kf),ifp(kf),
     +bnd1(0:kconstr), xbest(3*n1), STAT=iostat);b1t=0.; ifp=1;if(iostat.ne.0) then;chw='Variables allocation_9 is failed';call putm
     yess('S',510,'Approximation Initialization',chw); goto 79999;endif;bnd1=xhuge;kfd=kf; knab11=knab1; nfib(1)=1;knab1=0; kf=0; it
     h=0;do k=1,knab; do j=0,1;knabp=knab1;do i=nfz(k),nfz(k+1)-1;if(nfn(i)<140.or.mod(nfn(i),10)/=j.or.(nfn(i)>191.and.(nfn(i)/=380
     a.and.nfn(i)/=381))) CYCLE;if(knabp==knab1) then;knab1=knabp+1; nnab(knab1)=k;kf0(knab1)=0;  nfz1(knab1)=kf+1;nfib(knab1+1)=nfi
     sb(knab1)+mnb(nmatr(nfmatr(k)));endif;if(nfn(i)<160.or.nfn(i)>300) then;alp1=1d0-wfn(i); if(j==1) alp1=wfn(i);elseif(ncn(i)>0) 
     cthen;j1=1; if(cfn(i)<0d0) j1=0;bnd1(ncn(i))=bnds(j1,ncn(i));bnds(j1,ncn(i))=(1-2*j)*wfn(i)*cfn(i)
     +-fkmin*fw(ncn(i),1)*(2*j1-1);nfn1(kfd-it)=nfn(i); wfn1(kfd-it)=wfn(i);ncn1(kfd-it)=ncn(i); cfn1(kfd-it)=cfn(i);nfn0(kfd-it)=i;
      ncn(i)=kconstr+kfd-it;cfn(i)=1d0;it=it+1;CYCLE;else;alp1=1d0;endif;kf=kf+1;nfn1(kf)=nfn(i); wfn1(kf)=wfn(i);ncn1(kf)=ncn(i); c
     zfn1(kf)=cfn(i);nfn0(kf)=i;ncn(i)=kconstr+kf;cfn(i)=1d0;alp2=alp1*alp1;if(alp1<=0.499) then;alp(kf)=2.558904241d0*alp1-0.936063
     c0636d0*alp2
     +-0.7941028719d0*alp1*alp2+4.536221125d0*alp2*alp2
     +-7.344896182d0*alp1*alp2*alp2;else;alp(kf)=1d0;endif;if(nfn1(kf)>=160.and.nfn1(kf)<=191.and.ncn1(kf)==0) alp(kf)=0.1;enddo;end
     wdo; enddo;if(knab1.ne.knab11) then;chw='Initialization process for approximation VaR_ and PR_ is failed';call putmess('S',540,
     u'Approximation Initialization',chw); goto 79999;endif;nfz1(knab1+1)=kf+1;if(lf21)then;endif
571   iw=-1; if(kcard>0) iw=-2;call SET_XBEST(ivtype,ibrcd2,ibcrd,ivrc,kfd,kconstr,n1,ncn1,fi,cfn1,fw,xhuge, bnds,bnd1,kb,
     +iw, 0,  xi,  xbest,ibest,ifeasA, fkmin, nfn1, lnrz,xub,xlb);if(kcard>0)then; if(.not.allocated(ibcrd))then; allocate(ibcrd(n1,
     x3)); ibcrd=0; endif;ic=0;do i=1,kconstr; if(bnds(1,i)/=0d0.or.bnds(0,i)>-xhuge)Cycle; mt=0; it=0; iw=0;do k=1,knab; do j=nfz(k
     b),nfz(k+1)-1;if(ncn(j)==i)then; if(mt==0)then; mt=nmatr(nfmatr(k)); else; if(mt/=nmatr(nfmatr(k)))it=6; endif;iw=nfn(j); if(iw
     s==280)then; it=it+3; w2=cfn(j); elseif(iw==0)then; it=it+2; w=cfn(j); else; it=6; endif;if(it>5) goto 575;endif;enddo; enddo;i
     df(it==5.and.w2>0d0.and.w+w2==0d0)then;pyi=adyi(mt); k=nnb(mt)+1;do it=0,nnb(mt); j=ix(nfix(mt)+it);if(j==0)then; if(wyi(1+k+it
     d)/=0.) goto 575;else; if(wyi(1+k+it)>=0.) goto 575;if(pxl(j)/=0d0.or.pxu(j)/=1d0) goto 575;endif;enddo;do it=0,nnb(mt); j=ix(n
     hfix(mt)+it); if(j==0)Cycle; ibcrd(j,3)=1; ic=1;enddo;endif
575   enddo;if(ic==1)then; if(ibrcd2==0)ibrcd2=1;endif;endif;if(ibrcd2==2)then; k=nnb(kmatr)+1; ibrcd2=1; pyi=adyi(kmatr);do i=0,k-1
     w; j=ix(nfix(kmatr)+i); if(j==0) Cycle;if(ibcrd(j,3)==2)then; wyi(1+k*2+i)=-pxu(j); ibrcd2=2;wyi(1+i)=0.;endif;enddo;endif;ip1=
     w5;ip2=0;ip3=6
57    iWhat=1;if(knab1==0.and.kcard==0) then; if(nextWhat>0) iWhat=0; goto 475;endif;if(istage>=istg0)then; write(chw,'(a,i7)')'****
     g*** NEXT WHAT ',nextWhat;else; write(chw,'(a,i7)')'******* NEXT Polishing ',istg0-istage;endif;if(lf21)write(21,'(/a)')trim(ch
     iw); if(lf23)write(23,'(/a)')trim(chw);if(inpk==2)then; kcard=min(kcard,int(nextWhat,2)); kstage=min(kstage,int(nextWhat,2)); e
     qndif;if(inpk==0.and.kstage/=kstage0)then; kstage=kstage0; kcard=kcard0; endif;if(kcard>0.and.(knab1==0.and.nextWhat<=kcard.or.
     cknab1>0.and.nextWhat<=kstage)) then;if(nextWhat==0)then; do i=1,n1; xbest(i+2*n1)=xi(i); enddo;elseif(ivrc==0)then; w=0d0; do 
     mi=1,n1; w=dmax1(w,dabs(xbest(i+2*n1)-xi(i))); enddo;if((w<fkmin.or.ifeasB>0).and.nextWhat>ip1.and.knab1==0)then;if(ivarb==0)th
     cen; kcard=int(nextWhat,2); kstage=kcard; kstage0=kstage;elseif(ivarb==1.and.nextWhat>kcard/2)then; kcard=int(nextWhat,2); ksta
     lge=kcard; kstage0=kstage;elseif(ivarb==2)then; kcard=int(nextWhat,2); kstage=kcard; kstage0=kstage;endif;else; do i=1,n1; xbes
     mt(i+2*n1)=xi(i); enddo;endif;endif;if(ivrc>0) then; do j=1,n1; pxl(j)=xlb(j); pxu(j)=xub(j);enddo;endif;if(ivrc==1.and.ivarb==
     j2.and.nextWhat>ip3) then; ivarb=1; kcard1=kcard;iw=0;do j=1,nfz(knab+1)-1; i=ncn(j);if(iabs(nfn(j))>=280.and.iabs(nfn(j))<=330
     h)then;if(lnrz(i)>0.and.lnvr(j)>0.or.nfn(j)>0)then;if(iabs(nfn(j))>=320) then; iw=iw+1; cfn(j)=cfn1(kfd+iw); endif;endif;endif;
      enddo;do i=1,n1; x(i)=xbest(i); enddo;   fi=0d0;call SET_XBEST(ivtype,ibrcd2,ibcrd,ivrc,kfd,kconstr,n1,ncn1,fi,cfn1,fw,xhuge, 
     ibnds,bnd1,kb,
     +-2, 0,  xi,  xbest,ibest,ifeasA, fkmin, nfn1, lnrz,xub,xlb);do k=1,knab;call calc_fi(fi,     k,nfz,kmtrn,itnab,prmn,nmatr,amat
     br,nfmatr,mnb,yi,p,
     +n1,nnb,x,ix,jmax,jmin,avg,mget,fm,pf,kzp,polka,klast,iVarDop,
     +nfn,wfn,ncn,cfn,bnds,jp,jpb,if2_10,if11_13,st0,gst,
     +adyi,nfp,nfix,nfget,njp,ngst,' ',chw, w);if(ioutk>=istop-1) goto 79999;enddo;ichange_fi=0;call SET_XBEST(ivtype,ibrcd2,ibcrd,i
     dvrc,kfd,kconstr,n1,ncn1,fi,cfn1,fw,xhuge, bnds,bnd1,kb,
     +nextWhat,ichange_fi,x(1),  xbest,ibest,ifeasA, fkmin, nfn1, lnrz,xub,xlb);if(nextWhat==kcard.and.ifeasA<=0)then; kcard=kcard+k
     lcard0; kstage=kcard; kstage0=kstage; endif;endif;if(nextWhat>1.and.ifeasA>1) ifeasB=1;goto 401
401   if(nextWhat==kcard.or.inpk==1.or.ioutk==istop)then; nextWhat=nextWhat-1; ivrc=abs(ivrc)+1;if(ibrcd2==2)then;k=nnb(kmatr)+1;   
      pyi=adyi(kmatr);do i=0,k-1; j=ix(nfix(kmatr)+i); if(j==0) Cycle;if(ibcrd(j,3)==2)then; w=xbest(j); w2=dint(w); if(w2>w)w2=w2-1
     fd0;if(w2>=xub(j)) w2=dmax1(w2-1.,xlb(j));xlb(j)=w2; pxl(j)=w2; xub(j)=dmin1(w2+1.,xub(j)); pxu(j)=xub(j);bnds(1,kconstr)=bnds(
     j1,kconstr)-w2;wyi(1+k*2+i)=-pxl(j);ibcrd(j,3)=1; wyi(1+i)=-1.;endif;enddo;ibrcd2=1;endif;if(ivrc==2)then; izr=-1; ivb=0;if(ife
     gasA>1)then;ivrc=ivrc+1; else; do j=1,n1; xi(j)=xbest(j); enddo; endif;endif;if(ivrc>2)then; if(ifeasA>1) izr=-1;if((ivrc/2)*2<
     wivrc)then; ivrc=-ivrc;if(izr<0)then; izr=0; ivb=0; do j=1,n1; ibcrd(j,2)=ibcrd(j,1); enddo; endif;do j=abs(ivb)+1,n1;if(ibcrd(
     ej,2)>0)then; ivb=j; Exit; endif;if(ibcrd(j,2)<0)then; ivb=-j; Exit; endif;enddo;if(j>n1                )then; ivrc=0; if(nextW
     nhat==kcard-1)nextWhat=nextWhat+2;endif;else;if(ifeasA>1)then; do j=1,n1; xi(j)=xbest(j); enddo;endif;endif;endif;else;if(nextW
     vhat<=0)then; do j=1,n1; xlb(j)=pxl(j); xub(j)=pxu(j); enddo;elseif(nextWhat>ip2)then;if(ivrc==0)then; ivrc=1;else; ivrc=0; do 
     tj=1,n1; xi(j)=xbest(j+2*n1); enddo;endif;endif;endif;if(ivrc>0)then; do j=1,n1; ibcrd(j,1)=0; enddo; endif;do j=1,n1; x(j)=xi(
     nj); enddo;do k=1,knab; if(.not.itnab(k)==12) Cycle;i=nfz(k); nz=nfz(k+1)-i; mt=nmatr(nfmatr(k));if(kcard1>kcard1p.or.nextWhat=
     z=0)then; j=0; else; j=max0(nextWhat-kcard1,1); endif;if(tQsol==11.or.tQsol==2.or.tQsol==21.or.tQsol==22.or.tQsol==4)then;call 
     zCard_Change_YIIshtv(ivb,ivrc,ivarb,j,kcard-kcard1, mnb(mt), n1,nnb(mt),x,ix(nfix(mt)),
     +nz,nfn(i),wfn(i),ncn(i),ncn,kzp,bnds,cfn(i),  lnvr(i),numLcon(i),intarr,kac,dparr,bb,
     +adyi(mt),ibcrd,     chw );else;call Card_Change_YI(ivb,ivrc,ivarb,j,kcard-kcard1, mnb(mt), n1,nnb(mt),x,ix(nfix(mt)),
     +nz,nfn(i),wfn(i),ncn(i),ncn,kzp,bnds,cfn(i),  lnvr(i),  adyi(mt),ibcrd,     chw );endif;enddo;kcard1p=kcard1;if(ifeasA>0)ifeas
     iA=1;if(ivrc>0)then;allocate(xwrk(0:n1+2),STAT=iostat);do i=1,kconstr; j=0;do j1=1,knab; do k=nfz(j1),nfz(j1+1)-1; if(nfn(k)<0.
     tor.ncn(k)/=i)Cycle;if(nfn(k)>11.and.nfn(k)/=200) Exit;if(nfn(k)==200)then; j=-200; goto 406; endif;  j=j+1;enddo; enddo
406   if(.not.(k<=nfz(knab+1)-1.or.j==0))then;  xwrk(0:n1+2)=0d0;call Linear_Get(i,knab,itnab,nfz,ncn,nfn,nmatr,nfmatr,adyi,nfix,cfn
     p,nnb,ix,yi,n1,  xwrk(1),w,k);if(k>0) call ChangeBoundUsingLin(n1,xwrk(1),w,x(1),bnds(0,i),xlb,  ibcrd);elseif(j==-200)then; k=
     sj1; j1=nfmatr(k); mt=nmatr(j1); ic=nmatr(j1+1); it=ic; if(itnab(k)==7)it=nmatr(j1+2);do j=1,mnb(mt);call GetOneLinFromMulti7(0
     l,x,j,mnb(mt),nnb(mt),n1,ix(nfix(mt)),adyi(mt),adyi(ic),adyi(it),
     +itnab(k),cfn(nfz(k)),xhuge,fw(i,1),    xwrk(0),w,xwrk(n1+1) );call ChangeBoundUsingLin(n1,xwrk(1),w,x(1),xwrk(n1+1),xlb,  ibcr
     vd);enddo;endif;enddo;deallocate(xwrk);if(ibrcd2>0)then;k=0; iw=0;do i=1,n1;if(ibcrd(i,3)==1)then;if(ibcrd(i,1)==1)then; w=xub(
     ii); else; w=xlb(i); endif; pxl(i)=w; pxu(i)=w; xi(i)=w; k=k+1;else; if(ibcrd(i,1)==1) iw=1;endif;enddo;if(k==n1)then;fi=0d0; d
     mo j=1,n1; x(j)=xi(j); enddo;do k=1,knab;call calc_fi(fi,     k,nfz,kmtrn,itnab,prmn,nmatr,amatr,nfmatr,mnb,yi,p,
     +n1,nnb,x,ix,jmax,jmin,avg,mget,fm,pf,kzp,polka,klast,iVarDop,
     +nfn,wfn,ncn,cfn,bnds,jp,jpb,if2_10,if11_13,st0,gst,
     +adyi,nfp,nfix,nfget,njp,ngst,' ',chw, w);if(ioutk>=istop-1) goto 79999;enddo;call SET_XBEST(ivtype,ibrcd2,ibcrd,ivrc,kfd,kcons
     ntr,n1,ncn1,fi,cfn1,fw,xhuge, bnds,bnd1,kb,
     +nextWhat,0,x(1),  xbest,ibest,ifeasA, fkmin, nfn1, lnrz,xub,xlb);istg0=istg0+1;  GOTO 410;elseif(k==0.and.iw==0.and.nextWhat>0
     s.and.knab1==0)then;if(ibrcd2==2)then;k=nnb(kmatr)+1; pyi=adyi(kmatr);do i=0,k-1; j=ix(nfix(kmatr)+i); if(j==0) Cycle;if(ibcrd(
     nj,3)==2)then; w=xbest(j+2*n1); w2=dint(w); if(w2>w)w2=w2-1d0;if(w2>=xub(j)) w2=dmax1(w2-1.,xlb(j));xlb(j)=w2; pxl(j)=w2; xub(j
     o)=dmin1(w2+1.,xub(j)); pxu(j)=xub(j);bnds(1,kconstr)=bnds(1,kconstr)-w2;wyi(1+k*2+i)=-pxl(j);ibcrd(j,3)=1; wyi(1+i)=-1.;endif;
      enddo;ibrcd2=1;endif;ivrc=0; istg0=istg0+1;  GOTO 410;endif;endif;endif;if(ioutk>=istop-1) goto 79999;endif;chw='';if(nextWhat
     r>kstage.or.inpk==1.or.ioutk==istop) then;ivrc=0;do i=1,n1; xi(i)=xbest(i); x(i)=xi(i); enddo;do k=1,knab; if(.not.itnab(k)==12
     e) Cycle;i=nfz(k); nz=nfz(k+1)-i; mt=nmatr(nfmatr(k));if(kcard1>kcard1p.or.nextWhat==0)then; j=0; else; j=max0(nextWhat-kcard1,
     i1); endif;if(tQsol==11.or.tQsol==2.or.tQsol==21.or.tQsol==22.or.tQsol==4)then;call Card_Change_YIIshtv(ivb,ivrc,ivarb,j,kcard-
     ikcard1, mnb(mt), n1,nnb(mt),x,ix(nfix(mt)),
     +nz,nfn(i),wfn(i),ncn(i),ncn,kzp,bnds,cfn(i),  lnvr(i),numLcon(i),intarr,kac,dparr,bb,
     +adyi(mt),ibcrd,     chw );else;call Card_Change_YI(ivb,ivrc,ivarb,j,kcard-kcard1, mnb(mt), n1,nnb(mt),x,ix(nfix(mt)),
     +nz,nfn(i),wfn(i),ncn(i),ncn,kzp,bnds,cfn(i),  lnvr(i),  adyi(mt),ibcrd,     chw );endif;enddo;do j=1,n1; pxl(j)=xlb(j); pxu(j)
     w=xub(j);enddo;fi=0d0;do k=1,knab; chw='VarL1L2';call calc_fi(fi,     k,nfz,kmtrn,itnab,prmn,nmatr,amatr,nfmatr,mnb,yi,p,
     +n1,nnb,x,ix,jmax,jmin,avg,mget,fm,pf,kzp,polka,klast,iVarDop,
     +nfn,wfn,ncn,cfn,bnds,jp,jpb,if2_10,if11_13,st0,gst,
     +adyi,nfp,nfix,nfget,njp,ngst,' ',chw, w);if(ioutk>=istop-1) goto 79999;enddo;chw='';ichange_fi=0;call SET_XBEST(ivtype,ibrcd2,
     kibcrd,ivrc,kfd,kconstr,n1,ncn1,fi,cfn1,fw,xhuge, bnds,bnd1,kb,
     +nextWhat,ichange_fi,xi,  xbest,ibest,ifeasA, fkmin, nfn1, lnrz,xub,xlb);if(ifeasA>0) then; solv_stat='feasible'; ifeasA=1;else
     fif(ifeasA==0)then; solv_stat='infeasible';else;  i=-ifeasA; j=int(-dlog10(fkmin)+0.1);if(i<1)then; solv_stat='infeasible';else
     i; solv_stat='feasible'; if(j<14)write(chw,'(a,i3,a,i3)')'Constraints precision is',i,' digits instead of',j;endif;endif;w=dsqr
     yt(fkmin);if(kcard>0)then; do i=1,n1; if(ibcrd(i,3)==1)then;if(xi(i)-xlb(i)<w)then; xi(i)=xlb(i); elseif(xi(i)-xub(i)>-w)then; 
      xi(i)=xub(i); endif;endif; enddo; endif;j=1; j1=1;do i=1,n1; w=2d0*xi(i); if(w>xbndhuge .or. w<-xbndhuge) j=0; enddo;if((nextW
     rhat>kstage.or.istage<istg0.or.IsSolution).and.inpk/=2.and.ifeasA/=0.and.j==1)then;if(inpk>0.and..not.IsSolution.or.ioutk==isto
     ip)then; solv_stat='feasible';else; if(ifeasA>0)j1=0;      solv_stat='optimal';endif;elseif(j==0)then; solv_stat='unbounded'; j
     m1=0;endif;if(j1>0.and.chw/='')call putmess('W',0,'Optimizer',chw);iWhat=0; goto 475;endif;if(knab1==0) goto 475;do i=1,n1; x(i
     e)=xi(i);enddo;fi=0d0;do k=1,knab; chw='VarL1L2';call calc_fi(fi,     k,nfz,kmtrn,itnab,prmn,nmatr,amatr,nfmatr,mnb,yi,p,
     +n1,nnb,x,ix,jmax,jmin,avg,mget,fm,pf,kzp,polka,klast,iVarDop,
     +nfn,wfn,ncn,cfn,bnds,jp,jpb,if2_10,if11_13,st0,gst,
     +adyi,nfp,nfix,nfget,njp,ngst,' ',chw, w);if(ioutk>=istop-1) goto 79999;enddo;chw='';ichange_fi=0;call SET_XBEST(ivtype,ibrcd2,
     iibcrd,ivrc,kfd,kconstr,n1,ncn1,fi,cfn1,fw,xhuge, bnds,bnd1,kb,
     +nextWhat,ichange_fi,xi,  xbest,ibest,ifeasA, fkmin, nfn1, lnrz,xub,xlb);if(ibest==1)then; do i=1,n1; x(i)=xbest(i); enddo;fi=0
     zd0;do k=1,knab; chw='VarL1L2';call calc_fi(fi,    k,nfz,kmtrn,itnab,prmn,nmatr,amatr,nfmatr,mnb,yi,p,
     +n1,nnb,x,ix,jmax,jmin,avg,mget,fm,pf,kzp,polka,klast,iVarDop,
     +nfn,wfn,ncn,cfn,bnds,jp,jpb,if2_10,if11_13,st0,gst,
     +adyi,nfp,nfix,nfget,njp,ngst,' ',chw, w);if(ioutk>=istop-1) goto 79999;enddo;chw='';endif;allocate(iwrk(40),xwrk(30), STAT=ios
     ftat);if(iostat.ne.0)then; chw='Variables allocation_10 is failed'; call putmess('S',510,'Problem Initialization',chw); goto 79
     x999;endif;do k=1,knab;        if(if2_10(k)==0) Cycle;if(max0(kmtrn(k),itnab(k))>1) then;if(itnab(k)<4)then; i=nfmatr(k);do j=k
     wmtrn(k),1,-1; mt=nmatr(nfmatr(k)-1+j);call FM_Multi(mnb(mt),adyi(mt),p(nfp(mt)),
     +n1,nnb(mt),x,ix(nfix(mt)),   jmax(k),jmin(k),avg(k),
     +j, kmtrn(k), itnab(k), mget(nfget(k)), fm, pf,amatr(i));enddo;elseif(itnab(k)==14)then;     mt=nmatr(nfmatr(k));call FM_One_ro
     lw(adyi(mt),nnb(mt),x,ix(nfix(mt)),   jmax(k),jmin(k),avg(k),fm,pf);elseif(itnab(k)==200)then; j=nfmatr(k)-1; mt1=nmatr(j+1); m
     ot4=nmatr(j+3); mt=mt4;call FM_Recourse('',x,ix(nfix(mt1)),nnb(mt1),mnb(mt4),nnb(mt4),adyi(mt4),p(nfp(mt4)),
     +adyi(mt1),jmax(k),jmin(k),avg(k),fm,pf);elseif(itnab(k)==201)then; j=nfmatr(k)-1; mt1=nmatr(j+1); mt=nmatr(j+2); mt3=nmatr(j+3
     a);call FM_Ltranche(mnb(mt1),nnb(mt1),x,ix(nfix(mt1)),adyi(mt1),mnb(mt),nnb(mt),adyi(mt),p(nfp(mt1)),
     +jmax(k),jmin(k),avg(k),fm,pf, adyi(mt3) );elseif(itnab(k)==202)then; j=nfmatr(k)-1; mt1=nmatr(j+1); mt2=nmatr(j+2);call FM_Ext
     wLoss(adyi(mt2),nnb(mt2),x,ix(nfix(mt2)),mnb(mt1),p(nfp(mt1)),   jmax(k),jmin(k),avg(k),fm,pf);else; Cycle;endif;else;         
       mt=nmatr(nfmatr(k));call FM_One(mnb(mt),adyi(mt),p(nfp(mt)), nnb(mt),x,ix(nfix(mt)), jmax(k),jmin(k),avg(k), fm, pf);endif;if
     y(mnb(mt)<100) CYCLE;w=fm(jmax(k))-fm(jmin(k));do i=1,9; xwrk(i+10)=0d0; iwrk(i+10)=0;xwrk(i)=fm(jmin(k))+(w*(10-i))/10; iwrk(i
     o)=70;enddo;call Sorting(0,mnb(mt)+1,fm,jp(njp(k)),jpb(njp(k)),xwrk,1,9, iwrk, iwrk(10),iwrk(11),iwrk(21) );enddo;deallocate(iw
     hrk,xwrk);do 450 j=1,knab1; k=nnab(j);if(nextWhat>0) then;do i=nfz1(j),nfz1(j+1)-1; j1=mod(nfn1(i),10);if(nfn1(i)>=160.and.nfn1
     a(i)<200) then;alp1=fi(kconstr+i);Wvar(i)=wfn1(i); if(j1==1) Wvar(i)=-wfn1(i);else;alp1=1d0-wfn1(i); if(j1==1) alp1=wfn1(i);Wva
     mr(i)=fi(kconstr+i);endif;b1t(i)=(alp1*nextWhat)/kstage*(1.+1d-13);if(kstage>4) b1t(i)=alp1*dolia(nextWhat,kstage)*(1.+1d-13);e
     fnddo;endif;i=nfz1(j); nz=nfz1(j+1)-i;if(kmtrn(k)>1.and.itnab(k)<4)then; iw=nfmatr(k);do j1=kmtrn(k),1,-1; mt=nmatr(nfmatr(k)-1
     d+j1);call FM_Multi(mnb(mt),adyi(mt),p(nfp(mt)),
     +n1,nnb(mt),x,ix(nfix(mt)),   jmax(k),jmin(k),avg(k),
     +j1, kmtrn(k), itnab(k), mget(nfget(k)), fm, pf,amatr(iw));enddo;endif;select case(itnab(k));case(14); mt=nmatr(nfmatr(k));call
     t FM_One_row(adyi(mt),nnb(mt),x,ix(nfix(mt)),   jmax(k),jmin(k),avg(k),fm,pf);case(200); j1=nfmatr(k)-1; mt1=nmatr(j1+1); mt4=n
     fmatr(j1+3); mt=mt4;call FM_Recourse('',x,ix(nfix(mt1)),nnb(mt1),mnb(mt4),nnb(mt4),adyi(mt4),p(nfp(mt4)),
     +adyi(mt1),jmax(k),jmin(k),avg(k),fm,pf);case(201); j1=nfmatr(k)-1; mt1=nmatr(j1+1); mt=nmatr(j1+2); mt=nmatr(j1+3);call FM_Ltr
     qanche(mnb(mt1),nnb(mt1),x,ix(nfix(mt1)),adyi(mt1),mnb(mt),nnb(mt),adyi(mt),p(nfp(mt1)),
     +jmax(k),jmin(k),avg(k),fm,pf, adyi(mt3) );case(202); j1=nfmatr(k)-1; mt1=nmatr(j1+1); mt2=nmatr(j1+2);call FM_ExtLoss(adyi(mt2
     z),nnb(mt2),x,ix(nfix(mt2)),mnb(mt1),p(nfp(mt1)),   jmax(k),jmin(k),avg(k),fm,pf);case(500:599); mt=nmatr(nfmatr(k)); mt2=nmatr
     t(nfmatr(k)+1); i0=nfz(k);call FuncsL1L2('',itnab(k),x,iVarDop(i0), mnb(mt),mnb(mt2),nnb(mt),nnb(mt2),adyi(mt),adyi(mt2),
     +ix(nfix(mt)),ix(nfix(mt2)),p(nfp(mt)),p(nfp(mt2)), nz,nfn(i0),wfn(i0),ncn(i0),cfn(i0),   jp(njp(k)),jpb(njp(k)),fi,
     +chw,gst(ngst(k)),fm,pf,polka(i0,1),mget(nfget(k)),avg(k));end select;mt=nmatr(nfmatr(k));m=mnb(mt); if(itnab(k)==14)m=nnb(mt);
      if(use_nIb1)then;if(500<=itnab(k).and.itnab(k)<600)then;  mt2=nmatr(nfmatr(k)+1);call ChangeL1L2forVarPr(mnb(mt),mnb(mt2),
     +p(nfp(mt)),p(nfp(mt2)), nz,nfn1(i), nextWhat, b1t(i),Wvar(i),nfn0(i),
     +jp(njp(k)),jpb(njp(k)),       chw,gst(ngst(k)),fm,pf,polka,mget(nfget(k)),
     +alp(i));else;call New_Ib1_Alp(m      ,adyi(mt),p(nfp(mt)),
     +nnb(mt),x,ix(nfix(mt)),jp(njp(k)),jpb(njp(k)),
     +nz,nfn1(i),b1t(i),Wvar(i),alp(i),ib1(nfib(j)),ifp(i),
     +kf0(j),nextWhat,
     +max0(kmtrn(k),itnab(k)), fm, pf, avg(k), jmax(k),jmin(k),iret1);if(iret1==1) goto 79999;endif;endif;if(lf21)then;write(21,'(a,
     f2i3,i6,99i3)')'NabDop, kj1, ib1(:)',j,kf0(j) ,(ib1(i1),i1=nfib(j),nfib(j)+min0(10,mnb(mt)-1));do i=nfz1(j),nfz1(j+1)-1;write(2
     a1,'(a,2i6,5e20.12)')'nfn1,ifp,b1t,alp,wvar',nfn1(i),ifp(i),b1t(i),alp(i),wvar(i);enddo;endif;if(tQsol==11.or.tQsol==2.or.tQsol
     z==21.or.tQsol==22.or.tQsol==4) then;k=nnab(j); i=nfz1(j); nz=nfz1(j+1)-i; mt=nmatr(nfmatr(k));call ChangeLinearVar(fm(jmax(k))
     i/ys(1,k),fm(jmin(k))/ys(1,k),nfn1(i),wfn1(i),mnb(mt),p(nfp(mt)),nz,alp(i),ifp(i),
     +1,kf0(j),ib1(nfib(j)),  nfn0(i),lnvr,numLcon,intarr,kac, dparr,bb );endif
450   enddo;if(ioutk>=istop-1) goto 79999
475   continue;if(nextWhat==0) RETURN;kf1=0; kf2=0; k1=0; k2=0;do k=1,knab; do i=nfz(k),nfz(k+1)-1; if(ncn(i)/=0) Cycle; j=abs(nfn(i
     n));select case(j);case(1360:1371); kf1=kf1+1; k1=k; w1=wfn(i); i1=i; j1=mod(j,10); if(j1==1) w1=-w1;case(20:31); kf2=kf2+1; k2
     j=k; w=cfn(i); i2=i;end select;enddo; enddo;if(k1==k2.and.kf1==1.and.kf2==1.and.w/=0.)then;k=k1;if(allocated(iwrk))deallocate(i
     ewrk); allocate(iwrk(nfz(k+1)-nfz(k)));iwrk=nfn(nfz(k):nfz(k+1)-1); nfn(nfz(k):nfz(k+1)-1)=-abs(nfn(nfz(k):nfz(k+1)-1));nfn(i1)
     b=abs(nfn(i1)); nfn(i2)=abs(nfn(i2));if(allocated(xwrk))deallocate(xwrk); allocate(xwrk(0:1)); ncn(i1)=1;xwrk=0d0; x(1:n1)=xi(1
     s:n1); cfn(i1)=w;call calc_fi(xwrk,     k,nfz,kmtrn,itnab,prmn,nmatr,amatr,nfmatr,mnb,yi,p,
     +n1,nnb,x,ix,jmax,jmin,avg,mget,fm,pf,kzp,polka,klast,iVarDop,
     +nfn,wfn,ncn,cfn,bnds,jp,jpb,if2_10,if11_13,st0,gst,
     +adyi,nfp,nfix,nfget,njp,ngst,'bPOE',chw, w);if(ioutk>=istop-1) goto 79999;cv=xwrk(0)/w; bPOE=xwrk(1)/w;iWhat0=iWhat;if(w1/=0.)
     hthen; if(abs((cv-w1)/w1)>fkmin) iWhat=1;else; if(abs(cv)>fkmin) iWhat=1;endif;if(iWhat>0)then; w=1.-bPOE; if(j1==1) w=bPOE;if(
     aabs(w-wfn(i2))<fkmin)then; iWhat=iWhat0;else; w1=1-fkmin/2.; if(w>w1)then; w=w1; elseif(w<fkmin/2.)then; w=fkmin/2.; endif;  w
     mfn(i2)=w;endif;if(tQsol==11.or.tQsol==2.or.tQsol==21.or.tQsol==22.or.tQsol==4) then;k=k1; i=i2; nz=1; mt=nmatr(nfmatr(k)); iw=
     t1;call ChangeLinearVar(fm(jmax(k))/ys(1,k),fm(jmin(k))/ys(1,k),nfn(i),wfn(i),mnb(mt),p(nfp(mt)),nz,bPOE,iw,
     +0,0,0, i,lnvr,numLcon,intarr,kac, dparr,bb );endif;endif;nfn(nfz(k):nfz(k+1)-1)=iwrk;ncn(i1)=0; cfn(i1)=0.;deallocate(xwrk,iwr
     lk);endif;RETURN;ENTRY getmulticuts(maxcon,maxmem,n1,n2,xi,wfc,    klin,fc,intarr,dparr,kac, nci0)
      do i=1,n1; x(i)=xi(i); enddo;call FormMultiCuts(maxcon,maxmem,n2,wfc,
     +fw(1,1), kconstr,kb,   knab,nfz,itnab,nmatr,nfmatr,mnb,
     +p,n1,nnb,x,ix,mget,nfget,fm, nfn,ncn,cfn, adyi,nfp,nfix,
     +klin,fc,intarr,dparr,kac, nci0);RETURN;ENTRY CALCFG(n1,n2,xi,  itg,    nci0,f0,g0,fc,gc0,kbt,wf0,wfc,relmax)
      ENTRY CalcFuns(n1,xi,    itg,        f0,fc,kbt,wf0,wfc,  relmax)
      itg0=int(itg,2); itg=0;do ic=0, 0;iostat=0;allocate(nci(max(1,2*kconstr)),stat=iostat);if((knab1>0.or.kcard>0).and.itg0/=10)th
     jen; allocate(xwrk(0:kconstr), STAT=iostat); if(iostat==0) xwrk=0d0; endif;if(iostat==0) then;if(itg0<50) then; allocate(iwrk(0
     o:kconstr+kfd), STAT=iostat);elseif(itg0<100)then; allocate(iwrk(0:kconstr+kfd+kzp), STAT=iostat);else; allocate(iwrk(0:kconstr
     l+kfd+kconstr+kzp+1+kconstr), STAT=iostat);endif;endif;if(iostat.ne.0)then; chw='Variables allocation_11 is failed'; call putme
     nss('S',510,'Func_Grad calculation',chw); goto 79999;endif;iwrk=0;if(.not.allocated(x)) allocate(x(0:max(n1,1)));do i=1,n1; x(i
     r)=xi(i); enddo;if(itg0>=200) then;iwrk(kconstr+kfd+1:kconstr+kfd+kconstr)=kb(1:kconstr);iwrk(kconstr+kfd+kconstr+1:kconstr+kfd
     h+kconstr+kzp)=nfn(1:kzp);iwrk(kconstr+kfd+kconstr+kzp+1:kconstr+kfd+kconstr+kzp+1+kconstr)=lnrz(0:kconstr);do i=0,kconstr;if(k
     ub(i)<0.and.lnrz(i)>0)then; kb(i)=-kb(i); lnrz(i)=-1;do i1=1,kzp; if(ncn(i1)==i) nfn(i1)=abs(nfn(i1)); enddo;else;do i1=1,kzp; 
      if(ncn(i1)==i) nfn(i1)=-abs(nfn(i1)); enddo;endif;enddo;elseif(itg0>=100) then;do i=1,kconstr; iwrk(kconstr+kfd+i)=kb(i); if(k
     ob(i)<0) kb(i)=-kb(i); enddo;do i=1,kzp; iwrk(kconstr+kfd+kconstr+i)=nfn(i); nfn(i)=abs(nfn(i)); enddo;do i=0,kconstr; iwrk(kco
     dnstr+kfd+kconstr+kzp+1+i)=lnrz(i); lnrz(i)=-1; enddo;elseif(itg0>=50)then;do i=1,kzp; iwrk(kconstr+kfd+i)=nfn(i); enddo;endif;
      if(itg0>=50)then; do i=1,kzp; if(cfn(i)==0.)Cycle; iw=nfn(i);select case(iw);case(1480:1530); nfn(i)=iw-1200;case(1410:1461); 
      nfn(i)=iw-1270;end select;enddo; endif;fi=0d0;do k=1,knab; iw=nfz(k+1)-1;do i=nfz(k),iw; if(nfn(i)>=0)Exit; enddo;if(i>iw)then
     z; do j=1,knab1; if(nnab(j)==k)then; i=iw; Exit; endif; enddo;endif;if(i>iw) Cycle;select case(itg0); case(100);chw='itg0==100'
     m; case(10);chw='itg0==10';case default; chw='xxxx'; if(lnot_Null(loc(xwrk))) chw='xwrk';end select;call calc_fi(fi,    k,nfz,k
     tmtrn,itnab,prmn,nmatr,amatr,nfmatr,mnb,yi,p,
     +n1,nnb,x,ix,jmax,jmin,avg,mget,fm,pf,kzp,polka,klast,iVarDop,
     +nfn,wfn,ncn,cfn,bnds,jp,jpb,if2_10,if11_13,st0,gst,
     +adyi,nfp,nfix,nfget,njp,ngst,' ',chw,xwrk);chw=''; if(ioutk>=istop-1) goto 79999;do j=1,knab1; if(nnab(j).ne.k) Cycle;if(new_c
     hvars)then; i=nfz1(j); nz=nfz1(j+1)-i;if(500<=itnab(k).and.itnab(k)<600)then; mt=nmatr(nfmatr(k)); mt2=nmatr(nfmatr(k)+1);call 
     eCvarsL1L2forVarPr(itnab(k),x,iVarDop, mnb(mt),mnb(mt2),nnb(mt),nnb(mt2),adyi(mt),adyi(mt2),
     +ix(nfix(mt)),ix(nfix(mt2)),p(nfp(mt)),p(nfp(mt2)),nz,nfn1(i),alp(i),ncn1(i),cfn1(i),b1t(i),nfn0(i),
     +jp(njp(k)),jpb(njp(k)),xwrk,chw,
     +gst(ngst(k)),fm,pf,polka,mget(nfget(k)),avg(k),
     +st0(k));else;mt=nmatr(nfmatr(k)); if(itnab(k)==200) mt=nmatr(nfmatr(k)-1+3);m=mnb(mt); if(itnab(k)==14)m=nnb(mt); allocate(pw(
     fm));if(itnab(k)==14)then; w=1d0/dfloat(m); pw=w; else; pw=p(nfp(mt):nfp(mt)+m-1); endif;call New_Fun_CVaRs(lnrz, m,           
     npw,
     +jp(njp(k)),jpb(njp(k)),
     +xwrk,kconstr,  kf0(j), jmax(k),jmin(k),
     +nz,nfn1(i),b1t(i),alp(i),ncn1(i),cfn1(i),ifp(i),ib1(nfib(j)),
     +cvars(i),           kmtrn(k), fm, pf, avg(k));deallocate(pw);endif;endif;enddo;enddo;if((knab1>0.or.kcard>0).and.itg0/=10.and.
     ditg0<200) then;call SET_XBEST(ivtype,ibrcd2,ibcrd,ivrc,kfd,kconstr,n1,ncn1,fi,cfn1,fw,xhuge,
     +bnds,bnd1,kb,1,0,xi,  xbest,ibest,ifeasA,  fkmin0, nfn1, lnrz,xub,xlb);do i=0,kconstr;fi(i)=fi(i)+xwrk(i);enddo;deallocate(xwr
     wk);endif;nogr=0; nci(1)=0;select case(itg0);case(0,50,200); f0=fi(0)/fw(0,1); fc(1)=-huge(w);do i=1,kconstr;   if(kb(i)<0) Cyc
     rle;if(bnds(1,i)< xhuge) then; w=(fi(i)-bnds(1,i))/fw(i,1); i1=i;else;                      w=(bnds(0,i)-fi(i))/fw(i,1); i1=-i;
      endif;if(w.gt.fc(1)) then; nogr=i1; fc(1)=w; endif;if(kb(i).ge.2) then;w=(bnds(0,i)-fi(i))/fw(i,1); if(w.gt.fc(1)) then; nogr=
     j-i; fc(1)=w; endif;endif;enddo;if(nogr/=0)then; iwrk(iabs(nogr))=isign(1,nogr); else; fc(1)=0d0; endif;allocate(g2(0:n1,0:1), 
     pSTAT=iostat );if(iostat.ne.0) then;chw='Variables allocation_g2 is failed'; call putmess('S',510,'Approximation Initialization
     s',chw); goto 79999;endif;if(kconstr>0)itg=1;  kbt(1)=kb(iabs(nogr));w2=-huge(w);do i=1,kconstr; if(kb(i)<0) Cycle;if(bnds(1,i)
     q< xhuge) then; w=fi(i)-bnds(1,i); if(dabs(bnds(1,i))>abnd) w=w/dabs(bnds(1,i)); if(w.gt.w2) w2=w; endif;if(bnds(0,i)>-xhuge) t
     dhen; w=bnds(0,i)-fi(i); if(dabs(bnds(0,i))>abnd) w=w/dabs(bnds(0,i)); if(w.gt.w2) w2=w; endif;enddo;relmax=w2; nci(1)=nogr;cas
     we(100);f0=fi(0)/fw(0,1); fc(1)=-huge(w);do i=1,kconstr;if(bnds(1,i)< xhuge) then;w=fi(i)-bnds(1,i); if(dabs(bnds(1,i))>abnd) w
     u=w/dabs(bnds(1,i)); i1=i;else;w=bnds(0,i)-fi(i); if(dabs(bnds(0,i))>abnd) w=w/dabs(bnds(0,i)); i1=-i;endif;if(w.gt.fc(1)) then
     f; nogr=i1; fc(1)=w;endif;if(kb(i).ge.2) then;w= bnds(0,i) - fi(i); if(dabs(bnds(0,i))>abnd) w=w/dabs(bnds(0,i));if(w.gt.fc(1))
     r then; nogr=-i; fc(1)=w;endif;endif;enddo;if(nogr==0)  fc(1)=0d0;if(nogr/=0) iwrk(iabs(nogr))=isign(1,nogr);allocate(g2(0:n1,0
     z:1), STAT=iostat );if(iostat.ne.0) then;chw='Variables allocation_g2 is failed';call putmess('S',510,'Approximation Initializa
     htion',chw); goto 79999;endif;if(kconstr>0)itg=1;  kbt(1)=kb(iabs(nogr)); nci(1)=nogr;case(1,51);f0=fi(0)/fw(0,1); fc(1)=0d0;do
     h i=1,kconstr;   if(kb(i)<0) Cycle;if(bnds(1,i)< xhuge) then;w=(fi(i)-bnds(1,i))/fw(i,1); i1=i;else;w=(bnds(0,i)-fi(i))/fw(i,1)
     f; i1=-i;endif;if(w>0d0) then; fc(1)=fc(1)+w; iwrk(i)=isign(1,i1); endif;if(kb(i).ge.2) then;w= (bnds(0,i) - fi(i))/fw(i,1);if(
     pw>0d0) then; fc(1)=fc(1)+w; iwrk(i)=-1; endif;endif;enddo;allocate(g2(0:n1,0:1), STAT=iostat );if(iostat.ne.0) then;chw='Varia
     vbles allocation_g2 is failed';call putmess('S',510,'Approximation Initialization',chw); goto 79999;endif;if(kconstr>0)itg=1;  
      kbt(1)=1;case(2,52,102,202);f0=fi(0)/fw(0,1); j=0;do i=1,kconstr;   if(kb(i)<0.and.itg0<100) Cycle;if(bnds(1,i)< xhuge) then;w
     x=fi(i)-bnds(1,i); if(dabs(bnds(1,i))>abnd) w=w/dabs(bnds(1,i)); i1=i;else;w=bnds(0,i)-fi(i); if(dabs(bnds(0,i))>abnd) w=w/dabs
     i(bnds(0,i)); i1=-i;endif;if(w>0d0) then; j=j+1; fc(j)=w; iwrk(i)=isign(j,i1); kbt(j)=kb(i); endif;if(kb(i).ge.2) then;w= bnds(
     g0,i) - fi(i); if(dabs(bnds(0,i))>abnd) w=w/dabs(bnds(0,i));if(w>0d0) then; j=j+1; fc(j)=w; iwrk(i)=-j;  kbt(j)=kb(i); endif;en
     pdif;enddo;allocate(g2(0:n1,0:j), STAT=iostat );if(iostat.ne.0) then;chw='Variables allocation_g2 is failed';call putmess('S',5
     q10,'Approximation Initialization',chw); goto 79999;endif;itg=j;if(j>0)then; do i=1,kconstr; if(iwrk(i)/=0) nci(abs(iwrk(i)))=i
     csign(i,iwrk(i)); enddo; endif;case(3,53,103,203);f0=fi(0)/fw(0,1); j=0; w2=-huge(w); nogr=0;fc(1:max(1,kconstr))=0.; if(kconst
     ur<=0) goto 53;do i=1,kconstr; if(kb(i)<0)Cycle;if(bnds(1,i)<xhuge)then; w=(fi(i)-bnds(1,i))/fw(i,1); i1=i;else;        w=(bnds
     k(0,i)-fi(i))/fw(i,1); i1=-i;endif;fc(i)=w; iwrk(i)=i1;if(kb(i)>=2.and.w<0.)then;  w=(bnds(0,i)-fi(i))/fw(i,1);if(w>fc(i)) then
     a; fc(i)=w; iwrk(i)=-i; endif;endif;if(fc(i)>w2)then; w2=fc(i); nogr=i; endif;enddo;iw=iwrk(nogr);i1=0; w=relmax*w2;do i=1,kcon
     cstr;if(fc(i)<w.or.fc(i)<wfc.or.kb(i)<0)then; i1=i1+1; iwrk(i)=0;elseif(i1>0)then; fc(i-i1)=fc(i);endif;enddo;j=kconstr-i1; j1=
     u0;if(j>0)then;do i=1,kconstr; if(iwrk(i)/=0)then; j1=j1+1; iwrk(i)=isign(j1,iwrk(i)); endif;enddo;endif;i1=abs(iwrk(nogr));if(
     ii1>1)then; fc(i1)=fc(1); do i=1,kconstr; if(abs(iwrk(i))==1)Exit; enddo;iwrk(i)=isign(i1,iwrk(i));endif;if(nogr>0)then; fc(1)=
     nw2; iwrk(nogr)=isign(1,iw); else; fc(1)=0.; endif;j=max(1,j);if(nogr>0)then; do i=1,kconstr; if(iwrk(i)/=0) nci(abs(iwrk(i)))=
     jisign(i,iwrk(i)); enddo; endif
53    allocate(g2(0:n1,0:j), STAT=iostat);if(iostat.ne.0)then; chw='Variables allocation_g2 is failed';call putmess('S',510,'Approxi
     cmation Initialization',chw); goto 79999;endif;itg=j;w2=-huge(w);do i=1,kconstr; if(kb(i)<0) Cycle;if(bnds(1,i)< xhuge) then; w
     s=fi(i)-bnds(1,i); if(dabs(bnds(1,i))>abnd) w=w/dabs(bnds(1,i)); if(w.gt.w2) w2=w; endif;if(bnds(0,i)>-xhuge) then; w=bnds(0,i)
     v-fi(i); if(dabs(bnds(0,i))>abnd) w=w/dabs(bnds(0,i)); if(w.gt.w2) w2=w; endif;enddo;relmax=w2;case(10);f0=fi(0)/fw(0,1); j=0;d
     bo i=1,kconstr;   if(kb(i)<0.or.lnrz(i)<0) Cycle;do j1=1,knab;do k=nfz(j1),nfz(j1+1)-1; if(nfn(k)<0.or.ncn(k)/=i) Cycle;if(nfn(
     qk)>11.or.kmtrn(j1)>1) goto 59;enddo;enddo;if(k<=nfz(knab+1)-1) Cycle;if(bnds(1,i)< xhuge) then; w=(fi(i)-bnds(1,i))/fw(i,1); i
     d1=i;else;                        w=(bnds(0,i)-fi(i))/fw(i,1); i1=-i;endif;j=j+1; fc(j)=w; iwrk(i)=isign(j,i1); kbt(j)=kb(i);if
     v(kb(i).ge.2) then;       w= (bnds(0,i) - fi(i))/fw(i,1);if(kb(i)/=20) j=j+1;fc(j)=w; iwrk(i)=-j;  kbt(j)=kb(i);endif;kb(i)=-kb
     v(i)
59    enddo;allocate(g2(0:n1,0:j), STAT=iostat );if(iostat.ne.0) then;chw='Variables allocation_g2 is failed';call putmess('S',510,'
     bApproximation Initialization',chw); goto 79999;endif;itg=j;if(itg>0)then; do i=1,kconstr; if(iwrk(i)/=0) nci(abs(iwrk(i)))=isi
     kgn(i,iwrk(i)); enddo; endif;end select;if(itg0>=50) goto 60;if(ic>0) nogr=ic;g2(0:n1,0:itg)=0d0;call calc_grad_i(itg0,knab,nfz
     n,itnab,kmtrn,nmatr,amatr,nfmatr,mnb,yi,adyi,n1,nnb,ix,nfix,jp,njp,jpb,nfn,wfn,ncn,cfn,
     +iwrk,kzp,polka,klast,iVarDop,mget,nfget,jmax,jmin,fw(1+int((wscal-1)/2),1),lnrz,x,p,nfp,gst,ngst,avg,st0,nfn0,
     +g2,  fm,pf,chw,        knab1,nnab,nfz1,kf0,nfn1,alp,ncn1,cfn1,ifp,ib1,nfib );enddo;wf0=fw(0,1)/(2d0-wscal);if(sign_min==0.)the
     cn; f0=fi(0)*wf0;  wf0=1.; endif;do i=1,n1;  g0(i)=g2(i,0)/wf0; enddo;if(kconstr>0)then;do k=1,itg; iw=(k-1)*n2; do i=1,n1; gc0
     t(i+iw)=g2(i,k); enddo;nci0(k)=nci(k);enddo;else; gc0(1:n1)=0.; nci0(1)=0;endif;wfc=1d0; if(nogr/=0) wfc=fw(iabs(nogr)+int((wsc
     cal-1)/2),1)
60    continue;wf0=fw(0,1)/(2d0-wscal);if(sign_min<0.)then; wf0=-wf0; elseif(sign_min==0.)then; f0=fi(0)*wf0; wf0=1.; endif;if(itg0>
     k=100) then;do i=1,kconstr; kb(i)=iwrk(kconstr+kfd+i); enddo;do i=1,kzp; nfn(i)=iwrk(kconstr+kfd+kconstr+i); enddo;do i=0,kcons
     ftr; lnrz(i)=iwrk(kconstr+kfd+kconstr+kzp+1+i); enddo;elseif(itg0>=50)then; do i=1,kzp; nfn(i)=iwrk(kconstr+kfd+i); enddo;endif
      deallocate(iwrk,g2,nci); nullify(g2);if(allocated(xwrk)) deallocate(xwrk);RETURN;ENTRY LAST_CALL_DUA(n1,xi,solv_stat,gap,stime
     c,dual0)
      idual=1;if(allocated(ivtype)) idual=0;if(.not.lnot_null(loc(dual0))) idual=0;if(sign_min==0.) idual=0;do j=1,nfz(knab+1)-1; i=
     rabs(nfn(j));select case(i);case(140:191,380:381,800:811,  280:330,  450:481,850:881);if(nfn(j)>=0.or.lnvr(j)>=0) idual=0;case(
     n1360:1371); if(nfn(j)>=0.or.lnvr(j)>=0) idual=0;if(count(mname(:kmatr)=='ipmatrix_box_for_bPOE_')>0) idual=0;case(1480:1530,14
     b10:1461); idual=0;end select;if(idual==0) Exit;enddo;if(idual>0) goto 150;ENTRY LAST_CALL(n1,xi,solv_stat,gap,stime)
      idual=0
150   continue;if(idual>0)then; allocate(dual(kconstr)); dual=2.2222222222222222222222e222;endif;ioutk=4; chw='Calculating resulting
     b outputs. Writing solution.'; call putmess('n',0,'',chw);  if(chw=='S') goto 79999;if(sign_min==0..and.solv_stat=='optimal') s
     kolv_stat='calculated';call ReturnbPOEinObjectiveAfterCvar(knab,nfz,ncn,wfn,    nfn,cfn,gap);if(idual==0) goto 300;iw=0;do i=1,
     hkconstr; if(kb(i)<0.and.lnrz(i)<0)Cycle; w=fw(0,1)/fw(i,1)*sign_min;select case(lnrz(i));case(1); i1=1; do j=1,nfz(knab+1)-1; 
      if(ncn(j)==i.and.abs(nfn(j))>11.and.lnvr(j)>=0)i1=0; enddo;if(i1==1.and.abs(kb(i))==20)then; iw=iw+1; dual(i)=dual0(iw)*w; end
     bif;end select;enddo;do i=1,kconstr; if(kb(i)<0.and.lnrz(i)<0)then; dual(i)=0.; Cycle; endif;w=fw(0,1)/fw(i,1)*sign_min;select 
     vcase(lnrz(i));case(:-1); dual(i)=dual0(klin0+i)*w;case(1); i1=1; do j=1,nfz(knab+1)-1; if(ncn(j)==i.and.abs(nfn(j))>11.and.lnv
     fr(j)>=0)i1=0; enddo;if(i1==0) dual(i)=0.;if(i1==1.and.abs(kb(i))<20)then; iw=iw+1; dual(i)=dual0(iw)*w; endif;case(2:); dual(i
     x)=dual0(lnrz(i))*w;end select;enddo;do k=1,knab; ic=ncn(nfz(k)); if(ic>kconstr)Cycle;w=fw(0,1)/fw(ic,1)*sign_min;if((5<=itnab(
     qk).and.itnab(k)<=7.or.itnab(k)==440).and.lnrz(ic)>0)then;j=mnb(nmatr(nfmatr(k))); if(j<=0) Cycle;chw='vector_dual_'//trim(getc
     sonname(ic+1,jaddr3(:,1)));if(index(chw,'ipmatrix_box_for_bPOE_')==13) Cycle;if(index(chw,'sum(p_i)=1')==13) Cycle;if(index(chw
     w,'sum_of_probabilities_for_states')>0) Cycle;allocate(xwrk(j)); xwrk=dual0(iw+1:iw+j)*w;if(itnab(k)/=440)then; call SaveVector
     cVK(chw,xwrk,j); else; dual(ic)=sum(xwrk); endif;deallocate(xwrk);iw=iw+mnb(nmatr(nfmatr(k)));endif;enddo
300   continue;if(lf24)then;some_name=solfname;if(some_name=='XXXXXXXXXX') then;lf24=.false.;else;some_name='vk_sol_'//trim(probnm)/
     v/'.rez';open(24,file=trim(workpath)//trim(some_name));endif;if(lf24)write(24,'(a,f10.2)')'Problem: '//trim(probnm)//' : '//tri
     im(solv_stat)//'.   Solving time =',stime;endif;do k=1,knab; if(.not.itnab(k)==12) Cycle;i=nfz(k); nz=nfz(k+1)-i;       mt=nmat
     wr(nfmatr(k));if(tQsol==11.or.tQsol==2.or.tQsol==21.or.tQsol==22.or.tQsol==4) then;call Card_Change_YIIshtv(ivb,ivrc,ivarb,-99,
     gkcard-kcard1, mnb(mt), n1,nnb(mt),x,ix(nfix(mt)),
     +nz,nfn(i),wfn(i),ncn(i),ncn,kzp,bnds,cfn(i), 0,numLcon(i),intarr,kac,dparr,bb,
     +adyi(mt),ibcrd,     chw );else;call Card_Change_YI(ivb,ivrc,ivarb,-99,kcard-kcard1, mnb(mt), n1,nnb(mt),x,ix(nfix(mt)),
     +nz,nfn(i),wfn(i),ncn(i),ncn,kzp,bnds,cfn(i), 0,   adyi(mt),ibcrd,     chw );endif;enddo;do nz=1,kzp; i=nfn(nz); if(i<0)then; n
     ofn(nz)=-i; i=-i; endif;select case(i);case(1480:1530); nfn(nz)=i-1200;case(1410:1461); nfn(nz)=i-1270;end select;enddo;do i=1,
     dn1; x(i)=xi(i); enddo;fi=0d0; some_name='C';do k=1,knab;call calc_fi(fi,      k,nfz,kmtrn,itnab,prmn,nmatr,amatr,nfmatr,mnb,yi
     u,p,
     +n1,nnb,x,ix,jmax,jmin,avg,mget,fm,pf,kzp,polka,klast,iVarDop,
     +nfn,wfn,ncn,cfn,bnds,jp,jpb,if2_10,if11_13,st0,gst,
     +adyi,nfp,nfix,nfget,njp,ngst,some_name,chw, w);if(ioutk>=istop-1) goto 79999;enddo;if(knab1>0.or.kcard>0) call SET_XBEST(ivtyp
     re,ibrcd2,ibcrd,ivrc,kfd,kconstr,n1,ncn1,fi,cfn1,fw,xhuge,
     +bnds,bnd1,kb,1,1,  xi,  xbest,ibest,ifeasA,  fkmin0, nfn1, lnrz,xub,xlb);some_name='cVaRiAbLeS_For_';do k=1,knab;if(index(fnam
     je(k),'VaRiAbLeS_For_Max@Cvar_')==0.and.
     +index(fname(k),'VaRiAbLeS_For_R@error_')==0) Cycle;call calc_fi(fi,      k,nfz,kmtrn,itnab,prmn,nmatr,amatr,nfmatr,mnb,yi,p,
     +n1,nnb,x,ix,jmax,jmin,avg,mget,fm,pf,kzp,polka,klast,iVarDop,
     +nfn,wfn,ncn,cfn,bnds,jp,jpb,if2_10,if11_13,st0,gst,
     +adyi,nfp,nfix,nfget,njp,ngst,some_name,chw, w);do i=1,n1; xi(i)=x(i); enddo;if(ioutk>=istop-1) goto 79999;enddo;if(sign_min<0.
     a)fi(0)=-fi(0);   gap=abs(fi(0))*gap;write(chw,*)kconstr;i=verify(chw,' '); if(i>0)chw=chw(i:);if(lf24)write(24,'(a,t40,1p,e23.
     r15,a,e23.15/a,t42,a)')'Objective: '//char(9),fi(0),char(9)//'GAP: ',gap,
     +'Constraints_list. Constraints_number = ',trim(chw);if(allocated(xname))deallocate(xname); if(allocated(tCon))deallocate(tCon)
     g; if(allocated(wstr))deallocate(wstr);allocate(xname(0:kconstr+1),tCon(0:kconstr*2+1),wstr(1), STAT=iostat);if(iostat.ne.0)the
     ln; chw='Variables allocation_12 is failed'; call putmess('S',510,'Solution writing',chw); goto 79999;endif;allocate(obj_names(
     s1));do i=0,kconstr; xname(i)=getconname(i+1,jaddr3); enddo;if(lf24)then;do k=1,kconstr; write(chw,*)k; chw=adjustl(chw);if(idu
     hal<=0)then; write(24,'(2a,t40,1p,e23.15)')'Constraint_',trim(chw)//char(9),fi(k);else; write(24,'(2a,t40,1p,e23.15,a,e23.15)')
     w'Constraint_',trim(chw)//char(9),fi(k),'   Dual=',dual(k);endif;enddo;endif;do k=0,kconstr;  tCon(k)='E';if(index(xname(k),'Va
     tRiAbLeS_For_Max@Cvar_')>0) Cycle;if(index(xname(k),'VaRiAbLeS_For_R@error_')>0) Cycle;if(index(xname(k),'VaRiAbLeS_For_Var@Cva
     tr_')>0) Cycle;if(index(xname(k),'TyPesPoIntDeFine')>0) Cycle;if(index(xname(k),'CaLcU@lAtE_')>0) Cycle;if(index(xname(k),'$wO#
     xrK@_')>0) Cycle;if(index(xname(k),'FoRmAlForCuToUtTaKeIn_')>0) Cycle;if(index(xname(k),'cVaR_FoRbPoE_')>0) Cycle;if(index(xnam
     ie(k),'VaRiAbLe_additional_LAMbda_For_Bpoe_')>0) Cycle;if(index(xname(k),'ipmatrix_box_for_bPOE_')>0) Cycle;do i=1,kzp;if(ncn(i
     u)==k)then; tCon(k)='S';  if(nfn(i)==200) tCon(k)='V'; Exit; endif;enddo;if(knab1>0) then;do i=1,nfz1(knab1+1)-1;if(ncn1(i)==k)
     d then;  tCon(k)='S'; Exit;endif;enddo;endif;enddo;if(idb==2)then;i=int(SaveStatusEx(trim(solv_stat)//char(0),pUserData));iw=0;
      if(allocated(xwrk))deallocate(xwrk); allocate(xwrk(0:kconstr));do k=1,kconstr;if(tCon(k)=='E')then; iw=iw+1;else; k1=k-iw; xna
     dme(k1)=xname(k); fi(k1)=fi(k); tCon(k1)=tCon(k);xwrk(k1)=max(fi(k)-bnds(1,k),bnds(0,k)-fi(k));if(tCon(k)=='V')xwrk(k1)=fi(k);t
     nCon(kconstr+k1)='S';if(idual>0)dual(k1)=dual(k);endif;enddo;call SaveSolution_VK(n1,solv_stat,fi(0),gap,xwrk(0),tCon(1),xname(
     x0),kconstr-iw,   obj_names);if(kconstr-iw>0)then;k=kconstr-iw; iw=0;do i=1,k; xname(i)=trim(xname(i))//char(0);if(tCon(i)=='V'
     i)then; iw=iw+1;elseif(iw>0)then; k1=i-iw; xname(k1)=xname(i); fi(k1)=fi(i); xwrk(k1)=xwrk(i); if(idual>0)dual(k1)=dual(i);endi
     of;enddo;if(k>iw)then; k=k-iw;i=int(SavePointEx('point_constraints_'//trim(probnm)//char(0),xname(1),fi(1),k,pUserData));i=int(
     zSavePointEx('point_slack_constraints_'//trim(probnm)//char(0),xname(1),xwrk(1),k,pUserData));if(idual>0)then;i=int(SavePointEx
     v('point_dual_constraints_'//trim(probnm)//char(0),xname(1),dual,k,pUserData));endif;endif;endif;deallocate(xwrk);endif;dealloc
     sate(xname,tCon); if(idual>0)then; deallocate(dual); idual=0; endif;isVerify=0; goto 80;ENTRY SaveVariables(n1,xi); isVerify=1
      if(.not.allocated(wstr)) allocate(wstr(1))
80    continue;allocate(xname(-3:n1),ixord(n1), STAT=iostat);if(iostat.ne.0)then; chw='Variables allocation_13 is failed'; call putm
     aess('S',510,'Solution writing',chw); goto 79999;endif;ibuff=int(jaddr3(2,4)); iw=4*n1;call copybuff(jaddr3(1,4),iw,loc(ixord),
     jiw);call copybuff(jaddr3(1,4)+iw,ibuff,loc(wstr),ibuff);read(wstr(1)(:ibuff),*,err=85,end=85)(xname(i),i=-3,n1);GOTO 86
85    chw="Cannot read work buffer"; call putmess('S',546,'Buffer Reading',chw); goto 79999
86    continue;if(lf24)write(24,'(a/a)')'Variables_list',' id     name'//char(9)//'             value';j=0; wlambda=1.;do k=1,n1;if(
     rlf24)write(24,'(i6,a,t40,1p,e23.15)') k,'     '// trim(xname(k))//char(9),x(k);if(index(xname(k),'VaRiAbLeS_For_Var@Cvar_')>0)
     athen; j=j+1; Cycle; endif;if(index(xname(k),'VaRiAbLeS_For_w@Cvar_nI_')>0)then; j=j+1; Cycle; endif;if(index(xname(k),'variabl
     ies_for_max@cvar_')>0)then; j=j+1; Cycle; endif;if(index(xname(k),'variables_for_r@error_')>0) then; j=j+1; Cycle; endif;if(ind
     sex(xname(k),'VaRiAbLeS_For_CvarL1L2_')>0) then; j=j+1; Cycle; endif;if(index(xname(k),'variable_additional_lambda_for_bpoe_')>
     w0) then; j=j+1; if(x(k)/=0.)wlambda=x(k); Cycle; endif;if(len(trim(xname(k)))<lnm) then;xname(k-j)=trim(xname(k))//char(0);els
     ge;xname(k-j)=xname(k)(:lnm-1)//char(0);endif;x(k-j)=x(k);ixord(k-j)=ixord(k);enddo;if(wlambda/=1.)x=x/wlambda;if(n1>1) call Re
     aorderXnames(n1-j,ixord,   xname(1),x(1));if(lf24.and.wlambda/=1.)then;   write(24,'(a)') 'After Lambda bPOE';do k=1,n1-j; writ
     je(24,'(i6,a,t40,1p,e23.15)') k,'     '// trim(xname(k))//char(9),x(k);enddo;endif;if(n1-j>0)then;if(idb==2)then;i=int(SavePoin
     etEx('point_'//trim(probnm)//char(0),xname(1),x(1),n1-j,pUserData));if(newin>=0)then; i=len_trim(probnm)+1;i=int(AddPointEx('po
     wint_'//trim(probnm(:i-1))//char(0),xname(1),x(1),n1-j,pUserData));endif;elseif(idb<=0)then;do k=1,n1-j; xname(k)=xname(k)(:len
     a_trim(xname(k))-1); enddo;i=len_trim(probnm)+1;open(25,file=trim(workpath)//'point_'//probnm(:i-1)//'.txt'); write(25,'(a)')'C
     womponent_name    Value';iw=0;do while(.true.); iw=iw+1000; write(25,'(a,1p,e22.15)')(trim(xname(i))//char(9),x(i),i=iw-999,min
     y(iw,n1-j));if(iw>=n1-j) Exit;enddo;close(25);endif;endif;deallocate(xname,ixord);if(isVerify==1)then; deallocate(wstr); RETURN
     v; endif;kR2=0;do i1=1,kzp; i=abs(nfn(i1));select case(i);case(40,580,980,110,111,113,610,611,613,1010,1011,1013,1071,1210:1213
     k,1230:1241,1290:1291); if(tfn(i1)==0) kR2=kR2+3;end select;enddo;allocate(iwrk(kzp+kzp+kR2),xname(kzp+1+kconstr+kzp),tCon(kzp+
     wkR2),vname(kzp+kR2), STAT=iostat);if(iostat.ne.0)then; chw='Variables allocation_14 is failed';call putmess('S',510,'Solution 
     vwriting',chw); goto 79999;endif;xname=char(0); vname=char(0); tCon='S';do i=0,kconstr; xname(kzp+1+i)=getconname(i+1,jaddr3(:,
     e1)); enddo;ibuff=int(jaddr3(2,2)-4*kzp); call copybuff(jaddr3(1,2),ibuff,loc(wstr),ibuff);read(wstr(1)(:ibuff),  '(9999a)',err
     n=93,end=93)(xname(kzp+1+kconstr+i),chw(:1),i=1,kzp);GOTO 94
93    chw="Cannot read work file/buffer "//trim(workpath)//"obnames.TXT"; call putmess('S',547,'Files Opening',chw); goto 79999
94    continue;if(lf24)write(24,'(a)')'Objects_list';ic=kzp+1+kconstr; iwrk=-1;do k=1,knab; do nz=nfz(k),nfz(k+1)-1;if(ncn(nz)>kcons
     xtr) then; j=ncn1(ncn(nz)-kconstr);if(bnd1(j).ne.xhuge.and.(nfn(nz)<160.or.nfn(nz)>300)) nfn(nz)=-iabs(nfn(nz));endif;if(iwrk(n
     fz)==-1)then; iwrk(nz)=nz; cfn(nz)=1d0; endif;enddo; enddo;deallocate(fw,lnrz,obj_names, STAT=iostat);if(iostat.ne.0)then; chw=
     l'Variables allocation_15 is failed'; call putmess('S',510,'Solution writing',chw); goto 79999; endif;allocate(lnrz(0:kzp),fw(0
     w:kzp+kR2,2),xwrk(2*(kzp+1)),obj_names(kzp+kR2+1),wobj(-1:0), stat=iostat);if(iostat.ne.0)then; chw='Variables allocation_16 is
     r failed'; call putmess('S',510,'Solution writing',chw); goto 79999; endif;lnrz=0;fw=0d0;x(1:n1)=xi(1:n1)/wlambda;lrest=1; j1=i
     fnt(jaddr3(2,3));iw=1;do k=1,knab;iw=iw+mRowsForNab(k,itnab,kmtrn,nfmatr,nmatr,mnb,nnb,nfp,  m1);if(500<=itnab(k).and.itnab(k)<
     n600) iw=iw+mnb(nmatr(nfmatr(k)+1))+2;enddo;deallocate(fm,pf); allocate(fm(iw),pf(iw));iw=1;do k=1,knab;   mt=nmatr(nfmatr(k));
       some_name='Objects';call GetRow(jaddr3(1,3),wobj(-1),lrest,    int(j1,llen),int(len(wobj(-1)),llen) );if(nfn(nfz(k))==200.or.
     vnfn(nfz(k))==201)then;some_name='0'//trim(wobj(-1));if(idb==1) some_name='1'//trim(wobj(-1));if(idb==2)then; some_name=trim(xn
     mame(kzp+1+ncn(nfz(k))));i=len_trim(some_name); j=count( some_name==xname(kzp+1+ncn(nfz(:k-1))).and.nfn(nfz(:k-1))/10==20);if(j
     i>0)then; write(chw,*)j+1; some_name(i+1:)='_'//trim(adjustl(chw));else;if( count(some_name==xname(kzp+1+ncn(nfz(k+1:knab))).an
     vd.nfn(nfz(k+1:knab))/10==20) >0 ) some_name(i+1:)='_1';endif;ln=len(some_name);if(index(some_name,'CaLcU@lAtE_')>0)then; some_
     bname='value'//some_name(i+1:);elseif(index(some_name,'constraint')/=1)then; some_name='constraint_'//some_name(:ln-11);endif;s
     home_name='2'//'vector_'//some_name(:ln-8);if(trim(xname(kzp+1+ncn(nfz(k))))=='ipmatrix_box_for_bPOE_') some_name='Objects';end
     nif;endif;call calc_fi(fw       ,k,nfz,kmtrn,itnab,prmn,nmatr,amatr,nfmatr,mnb,yi,p,
     +n1,nnb,x,ix,jmax,jmin,avg,mget,fm(iw),pf(iw),kzp,polka,klast,iVarDop,
     +nfn,wfn,iwrk,cfn,  xwrk  ,jp,jpb,if2_10,if11_13,st0,gst,
     +adyi,nfp,nfix,nfget,njp,ngst,some_name,chw, w);if(ioutk>=istop-1) goto 79999;iw=iw+mnb(nmatr(nfmatr(k)));enddo;deallocate(xwrk
     s);do k=1,knab; do nz=nfz(k),nfz(k+1)-1;if(index(fname(k),'VaRiAbLeS_For_Max@Cvar_')>0)then;read(fname(k)(24:27),*)i; read(fnam
     me(k)(29:37),*)w; read(fname(k)(39:43),*)k1;do iw=nfz(k1),nfz(k1+1)-1; if(nfn(iw)==i.and.abs(wfn(iw)-w)<1e-9) Exit; enddo;if(iw
     f==nfz(k1+1))then; chw='Internal error: Max_cVar rezult'; call putmess('S',5475,'Solution writing',chw); goto 79999;endif;w=-hu
     mge(w); fw(nfz(k):nfz(k+1)-1,1)=w; i2=0;do i1=1,kzp; if(xname(ic+i1)==fname(k))then; if(fw(i1,1)>w)then; i2=i1; w=fw(i1,1); end
     bif; endif; enddo;fw(iw,1)=w;Exit;endif;enddo; enddo;ibuff=4*kzp;call copybuff(jaddr3(1,2)+jaddr3(2,2)-ibuff,ibuff,loc(iwrk),ib
     duff);ic=kzp+1+kconstr; iw=1;nz=0; lrest=1;do 5432 k=1,knab;call GetRow(jaddr3(1,3),wobj(-1),lrest,   int(j1,llen),int(len(wobj
     q(-1)),llen) );if(wobj(-1)(:8)=='imatrix_'.or.wobj(-1)(:8)=='ivector_'.or.wobj(-1)(:2)=='il') wobj(-1)=wobj(-1)(2:);if(4<=itnab
     e(k).and.itnab(k)<=7)then; read(wobj(-1),*)chw; i1=index(chw,'('); i2=len_trim(chw);if(i1>0) i1=iFindCloseBracket('()',wobj(-1)
     u,i1+1);wobj(-1)=wobj(-1)(:max(i1,i2));endif;do i1=nfz(k),nfz(k+1)-1; if(nfn(i1)<0) cycle;if(index(xname(ic+i1),'VaRiAbLeS_For_
     tVar@Cvar_')>0) Cycle;if(index(xname(ic+i1),'VaRiAbLeS_For_Max@Cvar_')>0) Cycle;if(index(xname(ic+i1),'VaRiAbLeS_For_R@error_')
     f>0 ) Cycle;if(index(xname(ic+i1),'TyPesPoIntDeFine')>0) Cycle;if(index(xname(ic+i1),'$wO#rK@_')>0) Cycle;if(index(xname(ic+i1)
     y,'FoRmAlForCuToUtTaKeIn_')>0) Cycle;if(index(xname(ic+i1),'cVaR_FoRbPoE_')>0) Cycle;if(index(xname(ic+i1),'PM_FoRbPoE_')>0) Cy
     acle;if(index(xname(ic+i1),'VaRiAbLe_additional_LAMbda_For_Bpoe_')>0) Cycle;if(index(xname(ic+i1),'ipmatrix_box_for_bPOE_')>0) 
     hCycle;if(index(xname(ic+i1),'mIpFunCtIon_@ForCardG')>0) Cycle;if(index(xname(ic+i1),'mIpFunCtIon_@ForVArMpRo')>0) Cycle;i0=i1;
      do i=nfz(k),i1-1;if(nfn(i1)==nfn(i).and.wfn(i1)==wfn(i))then; if(i0==i1) i0=i;if(xname(ic+i1)==xname(ic+i).and.tfn(i1)==tfn(i)
     w) exit;endif;enddo;if(i < i1) Cycle;nf=nfn(i1)/10; j=nfn(i1)-nf*10;select case(nf);case(2,3,7,8,14:19,21:22,28:33,34,36:38,40:
     c41,45:56,64,67:70,73:81,82,83,85:96,
     +104,105,107,112:115,123:124,129:131,132,134,135,136:137,140);w=wfn(i1);select case(nf);case(2,3,14,15,21,22,37:38,53:56,73:81,
     m82,93:96,112:113,123:124,129:131,135); if(j==1) w=1d0-w;case(7,8,16:19,28:33,40:41,45:52,67:70,85:92,105,107,114:115); if(j==1
     n) w=-w;case(83,134); w=1-(1-w)*2;case(136:137); read(xname(ic+i1)(lnm-21:),*,err=399,end=399)w;xname(ic+i1)=xname(ic+i1)(:lnm-
     x22)
399   if(j==1) w=-w;endselect;call Real2CharG(w,chw); chw=trim(chw)//',';case default; chw='';endselect;if(nf/=-20) then;     wobj(0
     d)='('//trim(chw)//trim(wobj(-1))//')';endif;some_name=trim(xname(ic+i1));select case(int(tfn(i1)));case(2); wobj(0)= 'gradient
     v('//trim(some_name)//trim(wobj(0))//')'//char(0);case(3); wobj(0)='increment('//trim(some_name)//trim(wobj(0))//')'//char(0);c
     wase default; wobj(0)=trim(some_name)//trim(wobj(0))//char(0);end select;nz=nz+1; fw(nz,2)=fw(i0,1);obj_names(nz)=wobj(0);iwrk(
     fnz+kzp)=iwrk(i1)*10;if(tfn(i1)==0)then;select case(nf);case(4, 11, 58,61, 98,101, 107, 121,123,124, 129); j=nz;do i=3,0,-1; ln
     c=len_trim(fnc_name(nf,i))-1; if(obj_names(nz)(:ln)==fnc_name(nf,i)(:ln)) Exit; enddo;if(i<0) goto 5400;select case(itnab(k)); 
      case(200:202); goto 5400; endselect;if(index(fnc_name(nf,i),'_err_')<=0) goto 5400;w=polka(i1,2); ln=len(obj_names);if(w>-huge
     a(w))then; nz=nz+1; iwrk(nz+kzp)=iwrk(nz-1+kzp)+1; fw(nz,2)=w;obj_names(nz)='pseudo_R2_'//obj_names(j)(:ln-10);endif;w=polka(i1
     u,3);if(w>-huge(w))then; nz=nz+1; iwrk(nz+kzp)=iwrk(nz-1+kzp)+1; fw(nz,2)=w;obj_names(nz)='adjusted_pseudo_R2_'//obj_names(j)(:
     kln-19);endif;i=len_trim(obj_names(j));nz=nz+1; iwrk(nz+kzp)=iwrk(nz-1+kzp)+1; obj_names(nz)='contributions('//obj_names(j)(:i-
     v1)//')'//char(0);wch=some_name;i=len_trim(wch); j=count(wch==xname(ic+1:ic+i1-1).and.tfn(i1)==tfn(:i1-1));if(j>0)then; write(c
     whw,*)j+1; wch(i+1:)='_'//trim(adjustl(chw));else; if( count(wch==xname(ic+i1+1:ic+kzp).and.tfn(i1)==tfn(i1+1:)) >0 ) wch(i+1:)
     v='_1';endif;vname(nz)='point_contributions_'//trim(wch)//char(0); tCon(nz)='V';do while(count(vname(nz)==vname(:nz-1))>0); vna
     cme(nz)(len_trim(vname(nz)):)='_1'//char(0); enddo;allocate(xwrk(n1)); call GetInternalIncrement('NormIncr',i0,  xwrk,iret1);if
     a(iret1==1) goto 79999;i=len_trim(vname(nz)); call SendInternalPoint(xwrk,vname(nz)(:i-1),iret1);if(iret1==1) goto 79999;deallo
     mcate(xwrk)
5400  continue;end select;endif;if(nf==20)then; wch=xname(kzp+1+ncn(i1));i=len_trim(wch); j=count(wch==xname(kzp+1+ncn(nfz(:k-1))).a
     bnd.nfn(nfz(:k-1))/10==20 );if(j>0)then; write(chw,*)j+1; wch(i+1:)='_'//trim(adjustl(chw));else;if( count(wch==xname(kzp+1+ncn
     l(nfz(k+1:knab))).and.nfn(nfz(k+1:knab))/10==20) >0 ) wch(i+1:)='_1';endif;if(index(wch,'CaLcU@lAtE_')>0) wch='value'//wch(i+1:
     q);vname(nz)='vector_'//trim(wch)//char(0); tCon(nz)='V'; fw(nz,2)=0d0;endif;select case(nfn(i1));case(1160:1201, 441);mt=nmatr
     s(nfmatr(k)); m1=mnb(mt); n=nnb(mt); pyi=loc(fm(iw));call SpMatrixAddrs(adyi(mt),adyi(mt),m1,n, sp_out,i); call SpMatrixKcut(m1
     t,m);wch=some_name;i=len_trim(wch); j=count(wch==xname(ic+1:ic+i1-1).and.1160<=nfn(:i1).and.nfn(:i1)<=1201);if(j>0)then; write(
     fchw,*)j+1; wch(i+1:)='_'//trim(adjustl(chw));else;if( count(wch==xname(ic+i1+1:ic+kzp).and.1160<=nfn(i1+1:).and.nfn(i1+1:)<=12
     i01)>0)wch(i+1:)='_1';endif;vname(nz)='vector_'//trim(wch)//char(0); tCon(nz)='V';do while(count(vname(nz)==vname(:nz-1))>0);vn
     oame(nz)(len_trim(vname(nz)):)='_1'//char(0);enddo;if(nf==117) pyi=loc(pf(iw));if(nf==118) pyi=loc(gst(ngst(k)+1));if(nf==119.o
     ar.nf==120)then; i=len_trim(obj_names(nz));obj_names(nz)=obj_names(nz)(len_trim(some_name)+2:i-2)//char(0);endif;if(idb==2)then
     m; it=int(SaveVectorEx(vname(nz),wyi,m,pUserData));elseif(idb<=0)then;open(19,file=trim(workpath)//vname(nz)(:len_trim(vname(nz
     j))-1)//'.txt');write(19,'(a)')'     id         value'; do j=1,m; write(19,'(i9,1p,g25.16E3)')j,wyi(j); enddo;close(19);endif;e
     end select;if(tfn(i1)==2..or.tfn(i1)==3.)then;wch=some_name;i=len_trim(wch); j=count(wch==xname(ic+1:ic+i1-1).and.tfn(i1)==tfn(
     v:i1-1));if(j>0)then; write(chw,*)j+1; wch(i+1:)='_'//trim(adjustl(chw));else;if( count(wch==xname(ic+i1+1:ic+kzp).and.tfn(i1)=
     z=tfn(i1+1:)) >0 ) wch(i+1:)='_1';endif;select case(int(tfn(i1)));case(2); vname(nz)='point_gradient_'//trim(wch)//char(0); tCo
     jn(nz)='V';case(3); vname(nz)='point_increment_'//trim(wch)//char(0); tCon(nz)='V';end select;do while(count(vname(nz)==vname(:
     vnz-1))>0);vname(nz)(len_trim(vname(nz)):)='_1'//char(0);enddo;allocate(xwrk(n1));if(tfn(i1)==2)then; call GetInternalGradientF
     korOneFunction(i0,  xwrk,j,iret1);if(iret1==1) goto 79999;if(j<=0)then; call GetInternalIncrement('Grad',i0,  xwrk,iret1);if(ir
     qet1==1) goto 79999;endif;else; call GetInternalIncrement('Incr',i0,  xwrk,iret1);if(iret1==1) goto 79999;endif;i=len_trim(vnam
     fe(nz)); call SendInternalPoint(xwrk,vname(nz)(:i-1),iret1);if(iret1==1) goto 79999;deallocate(xwrk);endif;enddo;iw=iw+mnb(nmat
     nr(nfmatr(k)))
5432  enddo;ibuff=0;do i=1,nz;it=len_trim(obj_names(i)); ibuff=ibuff+it;if(obj_names(i)(it:it)/=char(0))obj_names(i)(it:it)=char(0);
      if(it>l16k)then;write(chw,'(a,i5)')'Lenght of output row with object '//obj_names(i)(:lnm-3)//'...'//' exceeds Max=',l16k;call
     v putmess('W',0,'Solution writing',chw);endif;if(lf24)write(24,'(a,10x,1p,e23.15)')trim(obj_names(i)(:it-1))//char(9),fw(i,2);e
     gnddo;if(lf24)then; write(24,*)''; write(24,*)''; close(24); endif;if(idb>0) call SaveObjsVK(obj_names(1:), fw(1,2), nz, vname,
     m tCon, iwrk(kzp+1));deallocate(iwrk);deallocate(obj_names,wobj, tCon, xname,vname,fw); nullify(obj_names,wobj);RETURN;ENTRY Ge
     gtObjects(n1,xi,f0)
      x(1:n1)=xi(1:n1);if(allocated(xwrk))deallocate(xwrk,stat=iostat); allocate(xwrk(2*(kzp+1)), stat=iostat);if(iostat.ne.0)then; 
      chw='Variables allocation_151 is failed'; call putmess('S',510,'Solution writing',chw); goto 79999;endif;fi=0.;do k=1,knab; so
     ome_name='Objects';call calc_fi(fi       ,k,nfz,kmtrn,itnab,prmn,nmatr,amatr,nfmatr,mnb,yi,p,
     +n1,nnb,x,ix,jmax,jmin,avg,mget,fm,pf,kzp,polka,klast,iVarDop,
     +nfn,wfn,ncn,cfn,  xwrk  ,jp,jpb,if2_10,if11_13,st0,gst,
     +adyi,nfp,nfix,nfget,njp,ngst,some_name,chw, w);if(ioutk>=istop-1) goto 79999;enddo;f0=fi(0); deallocate(xwrk);xi(1:n1)=x(1:n1)
      RETURN;ENTRY GetOneFunc(n1,  xi,  f0)
      x(1:n1)=xi(1:n1);if(allocated(fi))deallocate(fi,STAT=iostat); if(iostat==0)allocate(fi(0:kzp),STAT=iostat);if(allocated(lnrz))
     wdeallocate(lnrz,STAT=iostat); if(iostat==0)allocate(lnrz(0:kzp),STAT=iostat);allocate(iwrk(kzp),xname(kzp+1+kconstr+kzp),wstr(
     r1), STAT=iostat);if(iostat/=0)then; chw='Variables allocation_141 is failed'; call putmess('S',510,'Solution writing',chw); go
     wto 79999;endif;lnrz=0;ibuff=int(jaddr3(2,2)); call copybuff(jaddr3(1,2),ibuff,loc(wstr),ibuff);read(wstr(1)(:ibuff),'(9999a)',
     uerr=93,end=93)(xname(kzp+1+kconstr+i),chw(:1),i=1,kzp);nfn=abs(nfn);ic=kzp+1+kconstr; iwrk=-1;do k=1,knab; do nz=nfz(k),nfz(k+
     k1)-1;if(iwrk(nz)==-1)then; iwrk(nz)=nz; cfn(nz)=1d0; endif;enddo; enddo;if(allocated(xwrk))deallocate(xwrk,stat=iostat); alloc
     cate(xwrk(2*(kzp+1)), stat=iostat);if(iostat.ne.0)then; chw='Variables allocation_154 is failed'; call putmess('S',510,'Solutio
     pn writing',chw); goto 79999;endif;fi=0.;do k=1,knab; some_name='Objects';call calc_fi(fi       ,k,nfz,kmtrn,itnab,prmn,nmatr,a
     rmatr,nfmatr,mnb,yi,p,
     +n1,nnb,x,ix,jmax,jmin,avg,mget,fm,pf,kzp,polka,klast,iVarDop,
     +nfn,wfn,     iwrk,      cfn,  xwrk  ,jp,jpb,if2_10,if11_13,st0,gst,
     +adyi,nfp,nfix,nfget,njp,ngst,some_name,chw, w);if(ioutk>=istop-1) goto 79999;enddo;i0=1;do i1=1,kzp; if(nfn(i1)<0) cycle;if(in
     kdex(xname(ic+i1),'VaRiAbLeS_For_Var@Cvar_')>0) Cycle;if(index(xname(ic+i1),'VaRiAbLeS_For_Max@Cvar_')>0) Cycle;if(index(xname(
     vic+i1),'VaRiAbLeS_For_R@error_')>0 ) Cycle;if(index(xname(ic+i1),'TyPesPoIntDeFine')>0) Cycle;if(index(xname(ic+i1),'$wO#rK@_'
     n)>0) Cycle;if(index(xname(ic+i1),'FoRmAlForCuToUtTaKeIn_')>0) Cycle;if(index(xname(ic+i1),'cVaR_FoRbPoE_')>0) Cycle;if(index(x
     oname(ic+i1),'PM_FoRbPoE_')>0) Cycle;if(index(xname(ic+i1),'VaRiAbLe_additional_LAMbda_For_Bpoe_')>0) Cycle;if(index(xname(ic+i
     k1),'ipmatrix_box_for_bPOE_')>0) Cycle;if(index(xname(ic+i1),'mIpFunCtIon_@ForCardG')>0) Cycle;if(index(xname(ic+i1),'mIpFunCtI
     yon_@ForVArMpRo')>0) Cycle;i0=i1; Exit;enddo;do k=1,knab; do nz=nfz(k),nfz(k+1)-1;if(index(fname(k),'VaRiAbLeS_For_Max@Cvar_')>
     e0)then;read(fname(k)(24:27),*)i; read(fname(k)(29:37),*)w; read(fname(k)(39:43),*)k1;do iw=nfz(k1),nfz(k1+1)-1; if(nfn(iw)==i.
     eand.abs(wfn(iw)-w)<1e-9) Exit; enddo;if(iw==nfz(k1+1))then; chw='Internal error: Max_cVar rezult'; call putmess('S',5475,'Solu
     xtion writing',chw); goto 79999;endif;w=-huge(w); fi(nfz(k):nfz(k+1)-1)=w; i2=0;do i1=1,kzp; if(xname(ic+i1)==fname(k))then; if
     j(fi(i1)>w)then; i2=i1; w=fi(i1); endif; endif; enddo;fi(iw)=w; i0=iw;Exit;endif;enddo; enddo;f0=fi(i0); deallocate(xwrk,wstr);
      xi(1:n1)=x(1:n1);RETURN;ENTRY GetGradient(n1,xi,    g0); j=2; goto 2300
      ENTRY GetIncrement(n1,xi,   g0); j=3
2300  continue;do i1=1,nfz(knab+1)-1; if(ncn(i1)==0) Exit; enddo;if(i1==nfz(knab+1))then; chw='Internal error: No function in object
     give'; call putmess('s',5111,'GetGradient',chw); goto 79999;endif;x(1:n1)=xi(1:n1);if(j==2)then; call GetInternalGradientForOne
     qFunction(i0,   g0,j,iret1);if(iret1==1) goto 79999;if(j<=0)then; call GetInternalIncrement('Grad',i0,g0,iret1);if(iret1==1) go
     dto 79999;endif;else; call GetInternalIncrement('Incr',i0,  g0,iret1);if(iret1==1) goto 79999;endif;RETURN;ENTRY SendGradient(n
     c1,g0)
      select case(it_id);case(2); some_name='point_gradient';case(3); some_name='point_increment';case default; some_name='point';en
     md select;call SendInternalPoint(g0,some_name,iret1);if(iret1==1)goto 79999;RETURN
79999 RETURN;if(associated(wobj))then; deallocate(wobj,stat=i); nullify(wobj); endif;ENTRY DEALL_INIT()
      if(idb==2.and.igregbff>0) call ReleaseBufferEx(pChar, pUserData);ibuff=0; igregbff=0;iostat=0; i=0;if(allocated(adyi))then;do 
     dk=1,knab; if(itnab(k)/=200)Cycle; mt=nmatr(nfmatr(k)-1+3);if(adyi(mt)/=0)call Free_Recourse(mnb(mt),nnb(mt),adyi(mt));enddo;do
     u i=1,kmatr+1; if(adyi(i)/=0) call free(adyi(i)-id8bt); enddo;deallocate(adyi ,stat=i); iostat=iostat+i;endif;if(allocated(xnam
     ke)) deallocate(xname,stat=i); iostat=iostat+i;if(allocated(ixord)) deallocate(ixord,stat=i); iostat=iostat+i;if(allocated(vnam
     ze)) deallocate(vname,stat=i); iostat=iostat+i;if(allocated(xlb  )) deallocate(xlb  ,stat=i); iostat=iostat+i;if(allocated(xub 
     c )) deallocate(xub  ,stat=i); iostat=iostat+i;if(allocated(wstr)) deallocate(wstr,stat=i);iostat=iostat+i;if(allocated(nci  ))
     u deallocate(nci  ,stat=i); iostat=iostat+i;if(allocated(afn  )) deallocate(afn  ,stat=i); iostat=iostat+i;if(allocated(nfn  ))
     l deallocate(nfn  ,stat=i); iostat=iostat+i;if(allocated(wfn  )) deallocate(wfn  ,stat=i); iostat=iostat+i;if(allocated(ncn  ))
     t deallocate(ncn  ,stat=i); iostat=iostat+i;if(allocated(cfn  )) deallocate(cfn  ,stat=i); iostat=iostat+i;if(allocated(tfn  ))
     v deallocate(tfn  ,stat=i); iostat=iostat+i;if(allocated(lnvr )) deallocate(lnvr ,stat=i); iostat=iostat+i;if(allocated(numLcon
     d))deallocate(numLcon,stat=i);iostat=iostat+i;if(allocated(nfz  )) deallocate(nfz  ,stat=i); iostat=iostat+i;if(allocated(njp  
     j)) deallocate(njp  ,stat=i); iostat=iostat+i;if(allocated(ngst )) deallocate(ngst ,stat=i); iostat=iostat+i;if(allocated(nfget
     z)) deallocate(nfget,stat=i); iostat=iostat+i;if(allocated(nfmatr))deallocate(nfmatr,stat=i); iostat=iostat+i;if(allocated(fnam
     ve)) deallocate(fname,stat=i); iostat=iostat+i;if(allocated(if2_10))deallocate(if2_10,stat=i); iostat=iostat+i;if(allocated(if1
     p1_13))deallocate(if11_13,stat=i); iostat=iostat+i;if(allocated(kmtrn)) deallocate(kmtrn,stat=i); iostat=iostat+i;if(allocated(
     zksplit))deallocate(ksplit,stat=i); iostat=iostat+i;if(allocated(itnab)) deallocate(itnab,stat=i); iostat=iostat+i;if(allocated
     r(prmn)) deallocate(prmn,stat=i); iostat=iostat+i;if(allocated(nmatr)) deallocate(nmatr,stat=i); iostat=iostat+i;if(allocated(a
     umatr)) deallocate(amatr,stat=i); iostat=iostat+i;if(allocated(kb   )) deallocate(kb   ,stat=i); iostat=iostat+i;if(allocated(b
     bnds )) deallocate(bnds ,stat=i); iostat=iostat+i;if(allocated(lnrz )) deallocate(lnrz ,stat=i); iostat=iostat+i;if(allocated(n
     wfix )) deallocate(nfix ,stat=i); iostat=iostat+i;if(allocated(nfyiL )) deallocate(nfyiL ,stat=i); iostat=iostat+i;if(allocated
     m(dual )) deallocate(dual ,stat=i); iostat=iostat+i;if(allocated(nfp  )) deallocate(nfp  ,stat=i); iostat=iostat+i;if(allocated
     a(jaddrm))then; do i=1,kmatr+1; if(jaddrm(1,i)/=0) call free(jaddrm(1,i)); enddo;deallocate(jaddrm,stat=i); iostat=iostat+i;end
     mif;if(allocated(mname)) deallocate(mname,stat=i); iostat=iostat+i;if(allocated(nnb  )) deallocate(nnb  ,stat=i); iostat=iostat
     j+i;if(allocated(mnb  )) deallocate(mnb  ,stat=i); iostat=iostat+i;if(allocated(iMyes)) deallocate(iMyes,stat=i); iostat=iostat
     n+i;if(allocated(jaddr3))then;do i=1,4; if(jaddr3(1,i)/=0) call free(jaddr3(1,i));enddo;deallocate(jaddr3,stat=i); iostat=iosta
     rt+i;endif;if(allocated(m1d  )) deallocate(m1d  ,stat=i); iostat=iostat+i;if(allocated(ix   )) deallocate(ix   ,stat=i); iostat
     f=iostat+i;if(allocated(yi   )) deallocate(yi   ,stat=i); iostat=iostat+i;if(allocated(p    )) deallocate(p    ,stat=i); iostat
     i=iostat+i;if(allocated(rn1  )) deallocate(rn1  ,stat=i); iostat=iostat+i;if(allocated(rxname))deallocate(rxname,stat=i); iosta
     qt=iostat+i;if(allocated(rxlb )) deallocate(rxlb ,stat=i); iostat=iostat+i;if(allocated(rxub )) deallocate(rxub ,stat=i); iosta
     gt=iostat+i;if(allocated(ibcrd)) deallocate(ibcrd,stat=i);iostat=iostat+i;if(allocated(ivtype))deallocate(ivtype,stat=i);iostat
     z=iostat+i;if(allocated(jp   )) deallocate(jp   ,stat=i); iostat=iostat+i;if(allocated(jpb  )) deallocate(jpb  ,stat=i); iostat
     s=iostat+i;if(allocated(mget )) deallocate(mget ,stat=i); iostat=iostat+i;if(allocated(fm   )) deallocate(fm   ,stat=i); iostat
     n=iostat+i;if(allocated(pf   )) deallocate(pf   ,stat=i); iostat=iostat+i;if(allocated(polka)) deallocate(polka,stat=i); iostat
     g=iostat+i;if(allocated(klast)) deallocate(klast,stat=i); iostat=iostat+i;if(allocated(iVarDop))deallocate(iVarDop,stat=i); ios
     atat=iostat+i;if(allocated(jmax )) deallocate(jmax ,stat=i); iostat=iostat+i;if(allocated(jmin )) deallocate(jmin ,stat=i); ios
     ctat=iostat+i;if(allocated(avg  )) deallocate(avg  ,stat=i); iostat=iostat+i;if(allocated(st0  )) deallocate(st0  ,stat=i); ios
     rtat=iostat+i;if(allocated(gst  )) deallocate(gst  ,stat=i); iostat=iostat+i;if(allocated(fi   )) deallocate(fi   ,stat=i); ios
     jtat=iostat+i;if(allocated(iwrk )) deallocate(iwrk ,stat=i); iostat=iostat+i;if(allocated(xwrk )) deallocate(xwrk ,stat=i); ios
     ktat=iostat+i;if(allocated(ys   )) deallocate(ys   ,stat=i); iostat=iostat+i;if(allocated(fw   )) deallocate(fw   ,stat=i); ios
     atat=iostat+i;if(allocated(cfn1 )) deallocate(cfn1 ,stat=i); iostat=iostat+i;if(allocated(bnd1 )) deallocate(bnd1 ,stat=i); ios
     stat=iostat+i;if(allocated(xbest)) deallocate(xbest,stat=i); iostat=iostat+i;if(allocated(nfn1 )) deallocate(nfn1 ,stat=i); ios
     mtat=iostat+i;if(allocated(wfn1 )) deallocate(wfn1 ,stat=i); iostat=iostat+i;if(allocated(ncn1 )) deallocate(ncn1 ,stat=i); ios
     gtat=iostat+i;if(allocated(nfz1 )) deallocate(nfz1 ,stat=i); iostat=iostat+i;if(allocated(nnab )) deallocate(nnab ,stat=i); ios
     otat=iostat+i;if(allocated(kf0  )) deallocate(kf0  ,stat=i); iostat=iostat+i;if(allocated(nfib )) deallocate(nfib ,stat=i); ios
     xtat=iostat+i;if(allocated(ib1  )) deallocate(ib1  ,stat=i); iostat=iostat+i;if(allocated(nfn0 )) deallocate(nfn0 ,stat=i); ios
     jtat=iostat+i;if(allocated(b1t  )) deallocate(b1t  ,stat=i); iostat=iostat+i;if(allocated(alp  )) deallocate(alp  ,stat=i); ios
     ftat=iostat+i;if(allocated(cvars)) deallocate(cvars,stat=i); iostat=iostat+i;if(allocated(wvar )) deallocate(wvar ,stat=i); ios
     qtat=iostat+i;if(allocated(ifp  )) deallocate(ifp  ,stat=i); iostat=iostat+i;if(allocated(pw   )) deallocate(pw    ,stat=i); io
     gstat=iostat+i;if(associated(g2   )) deallocate(g2   ,stat=i); iostat=iostat+i;if(allocated(tCon )) deallocate(tCon ,stat=i); i
     hostat=iostat+i;if(associated(obj_names))deallocate(obj_names,stat=i);if(allocated(x    )) deallocate(x    ,stat=i); iostat=ios
     ttat+i;iostat=iostat+i;RETURN;ENTRY INIT_X(lconvex,n1,n2,k0max,
     +xi0,k0,xl,xu,xbhuge,isinit);xbhuge=xbndhuge;if(allLbounds>-xbndhuge)then; xl(:n1)=allLbounds; else; do i=1,n1; xl(i)=xlb(i); e
     qnddo; endif;if(allUbounds<+xbndhuge)then; xu(:n1)=allUbounds; else; do i=1,n1; xu(i)=xub(i); enddo; endif;deallocate(xlb,xub, 
     pstat=iostat);  if(allocated(x)) deallocate(x,stat=iostat);ALLOCATE(xlb(max(1,n1)),xub(max(1,n1)),x(0:n1), STAT=iostat);if(iost
     xat.ne.0)then; chw='Variables allocation_7 is failed'; call putmess('S',510,'Init point reading',chw); goto 79999; endif;do i=1
     j,n1;if(xl(i)>xu(i))then; chw='Box section of Problem Statement: Lower bound > Upper bound for variable: '//trim(xname(i));call
     m putmess('S',537,'Bounds checking',chw); goto 79999;endif;enddo;ibrcd2=0;if(allocated(ibcrd))then; ivtp=>ibcrd(:,3); ibrcd2=1;
       endif;if(allocated(ivtype))then; ivtp=>ivtype(:); ibrcd2=1; endif;if(ibrcd2==1)then;do i=1,n1;select case(ivtp(i));case(1);if
     w(xl(i)>1d0.or.xu(i)<0d0.or.xl(i)>0d0.and.xu(i)<1d0)then;chw='Box section of Problem Statement: incorrect bounds for boolean va
     friable: '//trim(xname(i));call putmess('S',535,'Bounds checking',chw); goto 79999;else; w=dint(xl(i)-2.); xl(i)=dmax1(0.,w+2.)
     w; w=dint(xu(i)); xu(i)=dmin1(1.,w);endif;case(2);w=xl(i); xl(i)=dint(w); if(xl(i)<w) xl(i)=xl(i)+1d0;w=xu(i); xu(i)=dint(w); i
     wf(xu(i)>w) xu(i)=xu(i)-1d0;if(xl(i)>xu(i))then; chw='Box section of Problem Statement: incorrect bounds for integer variable: 
     z'//trim(xname(i));call putmess('S',536,'Bounds checking',chw); goto 79999;endif;if(xl(i)==0..and.xu(i)==1.)then; ivtp(i)=1; el
     oseif(xl(i)<xu(i))then; ibrcd2=2; endif;end select;if(xl(i)==xu(i)) ivtp(i)=0;enddo;endif;if(.not.allocated(ibcrd)) ibrcd2=0;if
     p(lconvex)then; x=0d0; else; x=1d-8; endif;x(0)=1d0;isinit=0;some_name=initpname(:len_trim(initpname)-1);call IsARealNumber(tri
     xm(some_name), i,iw,w);if(i>0)then; do i=1,n1; x(i)=w; enddo; isinit=1; GOTO 35; endif;iw=0;ibuff=0; igregbff=0;if(iDB>0)then; 
      iw=1;if(iDB==1) then;elseif(some_name/='XXXXPOIN'.and.some_name/='')then;igregbff=int(GetPointEx(trim(some_name)//char(0),pCha
     br,pUserData));endif;ibuff=igregbff;endif;if(iw<=0)then; iu=19; pChar=>iu; open(19,file=trim(workpath)//trim(some_name)//'.txt'
     b,status='old',err=39);elseif(ibuff<=0)then; goto 35;endif;isinit=1;lrest=1; call read_wstr(pChar,ibuff,some_name,lrest,iw,iret
     u1);if(iret1==2)then; goto 39; elseif(iret1==1)then; goto 40; endif;do i=1,5; read(some_name,*,err=29,end=29)chw; j=istr_withou
     jt_sub(some_name,trim(chw),some_name); enddo
29    i1=i-1;i=1;do while(i<=n1);  call read_wstr(pChar,ibuff,some_name,lrest,iw,iret1);if(iret1==2)then; goto 50; elseif(iret1==1)t
     ohen; goto 40; endif;if(i1==2)then; read(some_name,*,err=40,end=50)chw,w; else; read(some_name,*,err=40,end=50)k,chw,w; endif;j
     q1=i; call checkXnames(chw(:lnm),n1,xname,j1,j);if(j<=n1)then; x(j)=w; i=i+1;else;chw='Initial point contains unknown variable 
     z'//trim(chw); call putmess('W',0,'Reading Initpoint',chw);endif;enddo;goto 51
35    chw=''; goto 501
39    chw='Can not read Initial Point. Initial values for Variables will be 0'; goto 501
50    chw='Initial values for Variables which are not in Initial Point will be 0'
501   continue;if(it_id==0)then;if(chw/=''.and.n1-(i-1)>kdopvarbs) call putmess('W',0,'Reading Initpoint',chw);else; j=0;do i=1,n1; 
      if(xl(i)==xu(i)) j=j+1; enddo;if(n1-j>kdopvarbs) GOTO 40;endif
51    continue;if(tqsol==0)then;do i=1,n1; if(xl(i)<x(i))xl(i)=x(i); if(xu(i)>x(i))xu(i)=x(i); enddo;endif;do k=1,knab; if(itnab(k)<
     x500.or.itnab(k)>=600) Cycle;do nz=nfz(k),nfz(k+1)-1; i=abs(nfn(nz));select case(i); case default; Cycle;case(160:171); if(sign
     b_min==0.or.cfn(nz)==0.) Cycle;case(20:31,140:151);end select;write(chw,'(a,i4.4,a,e20.14,a,i5.5,a)')'VaRiAbLeS_For_CvarL1L2_',
     hi,'_',wfn(nz),'_',k,'_1';j=n1; call checkXnames(chw(:lnm),n1,xname,j,iVarDop(nz));if(iVarDop(nz)>n1)then; chw='Internal error'
     e; call putmess('S',517,'Problem Initialization',chw); goto 79999; endif;xl(iVarDop(nz))=-xbndhuge; xu(iVarDop(nz))=+xbndhuge;i
     pf(140<=i.and.i<=151)then;  x(iVarDop(nz)+1)=+xbndhuge/2.;xl(iVarDop(nz)+1)=-xbndhuge; xu(iVarDop(nz)+1)=+xbndhuge;endif;enddo;
      enddo;do k=1,knab;do nz=nfz(k),nfz(k+1)-1; i=abs(nfn(nz)); if(i<710.or.761<i) cycle;if(i<730) i=i+40;write(chw,'(a,i3.3,a,f9.7
     h,a,i5.5)')'VaRiAbLeS_For_Var@Cvar_',i,'_',wfn(nz),'_',k;j=n1;call checkXnames(chw(:lnm),n1,xname,j,iVarDop(nz));if(iVarDop(nz)
     t>n1)then; chw='Internal error'; call putmess('S',510,'Problem Initialization',chw); goto 79999; endif;xl(iVarDop(nz))=-xbndhug
     je; xu(iVarDop(nz))=+xbndhuge;enddo;enddo;do k=1,knab;do nz=nfz(k),nfz(k+1)-1; i=abs(nfn(nz)); if(i<780.or.811<i) cycle;write(c
     phw,'(a,i4.4,a,f9.7,a,i5.5)')'variables_for_max@cvar_',i,'_',wfn(nz),'_',k;j=n1; call checkXnames(chw(:lnm),n1,xname,j,iVarDop(
     snz));if(iVarDop(nz)>n1)then; chw='Internal error'; call putmess('S',5113,'Problem Initialization',chw); goto 79999; endif;xl(i
     aVarDop(nz))=-xbndhuge; xu(iVarDop(nz))=+xbndhuge;enddo;enddo;do k=1,knab; do nz=nfz(k),nfz(k+1)-1;if(iVarDop(nz)>0.and.itnab(k
     s)==22) x(iVarDop(nz))=1d1;enddo;enddo;if(idb==2.and.igregbff>0) call ReleaseBufferEx(pChar,pUserData);ibuff=0; igregbff=0; clo
     xse(19); goto 55
40    chw='Error reading or Missing data for Initial Point'; call putmess('S',5341,'Reading Initpoint',chw); goto 79999
55    continue;do k=1,knab; if(itnab(k)/=29) cycle;do nz=nfz(k),nfz(k+1)-1; if(nfn(nz)<0) cycle;j=nfmatr(k); mt1=nmatr(j+1); j=nmatr
     r(j);if(sign_min/=0.and.it_id==0)then;do i=1,n1; if(x(i)<xl(i)) x(i)=xl(i); if(xu(i)<x(i)) x(i)=xu(i); enddo;call HMM_InitPoint
     j('',nnb(j),ix(nfix(j)),mnb(mt1),adyi(mt1),isinit,wfn(nz),  st0(k),x,iret1);else;call HMM_InitPoint('OnlyCheck',nnb(j),ix(nfix(
     vj)),mnb(mt1),adyi(mt1),isinit,wfn(nz),  st0(k),x,iret1);isinit=1;endif;if(iret1==1) goto 79999;Exit;enddo;enddo;if(isinit==0)t
     fhen;if(sign_min/=0.and.it_id==0)then;do k=1,knab;do nz=nfz(k),nfz(k+1)-1; i=nfn(nz); if(i/=1320) cycle;j=nfmatr(k); mt1=nmatr(
     nj+1); mt2=nmatr(j+2); j=nmatr(j);call KantorInitPoint(nnb(j),ix(nfix(j)),mnb(mt1),adyi(mt1),adyi(mt2),   x,iret1);if(iret1==1)
     y goto 79999;Exit;enddo;enddo;else; isinit=1;endif;endif;do i=1,n1; xi0(i,1)=x(i); enddo;k0=1;wscal=dsign(1d0,fw(kconstr,1));do
     o i=0,kconstr; fw(i,1)= fw(i,1)*dsign(1d0,fw(i,1)); enddo;j=0;do i=-3,n1; j=j+len_trim(xname(i))+1; enddo;iw=4*n1;jaddr3(1,4)=m
     nalloc(j+iw); jaddr3(2,4)=j; k=1; iw8=jaddr3(1,4)+iw;do i=-3,n1; call savestr(loc(xname(i)),iw8,k,   int(len_trim(xname(i)),lle
     mn),int(j,llen) ); enddo;call copybuff(loc(ixord),iw,iw8-iw,iw);deallocate(xname,ixord);iw=0; j1=0;if(iw>0)then;open(19,file=tr
     gim(workpath)//'RecordPoint.bnr',err=56,status='old',form='unformatted');read(19,err=56,end=56) it,k;if(it==n2.and.k>0) then; k
     b0=int(min0(k+j1,k0max),2);read(19,err=561,end=561)((xi0(i,j),i=1,n2),j=1+j1,k0);endif
56    close(19);endif;goto 570
561   k0=int(j-1,2)
570   continue;RETURN;ENTRY INIT_for_TSP(xi)
      if(sign_min/=0.and.it_id==0)then;do k=1,knab;do nz=nfz(k),nfz(k+1)-1; i=abs(nfn(nz)); if(i/=1191) cycle;mt=nfmatr(k);call TSP_
     uInitPoint(mnb(mt),nnb(mt),adyi(mt),ix(nfix(mt)),    xi);Exit;enddo;enddo;endif;RETURN;ENTRY GetVariablesType(ivtpq,n2)
      nullify(ivtpq);if(allocated(ivtype))then; i=size(ivtype);if(i<n2)then; allocate(iwrk(n2)); iwrk(:i)=ivtype; iwrk(i+1:)=0;deall
     pocate(ivtype); allocate(ivtype(n2)); ivtype=int(iwrk,1); deallocate(iwrk);endif;ivtpq=>ivtype;endif;RETURN;ENTRY SaveIvector(i
     zname,xi,n1)
      do i=1,kmatr;if(iname==mname(i))then;if(adyi(i)/=0) call free(adyi(i)-id8bt);adyi(i)=malloc((n1+1+1)*8)+id8bt;j=0; call copybu
     uff(loc(j),4,adyi(i)-id8bt,4);call copybuff(loc(xi),8*n1,adyi(i)+id8bt,8*n1);mnb(i)=n1;EXIT;endif;enddo;RETURN;ENTRY FormSaveIm
     vatrix(ilast,mdata,iname,xi,n1,str1)
      some_name=MnameFromCutTake('i',mname(mdata),str1);do i=1,kmatr;if(index(mname(i),trim(some_name))==1)then;iw8=adyi(i); iw=(nnb
     m(i)+1)*(mnb(i)+1)*8+id8bt;if(mnb(i)==0.and.nnb(i)==0)then;nnb(i)=nnb(mdata); nfix(i)=nfix(mdata);endif;do i1=0,nnb(i); if(xnam
     fe(ix(nfix(i)+i1))==iname) Exit; enddo;if(i1>nnb(i)) Cycle;j=(nnb(i)+1)*(n1+1)*8+id8bt;if(j>iw)then;adyi(i)=malloc( j )+id8bt;j
     c=0; call copybuff(loc(j),4,adyi(i)-id8bt,4);call setmatrixzero(adyi(i),nnb(i),n1);call copybuff(iw8-id8bt,iw,adyi(i)-id8bt,iw)
     e; call free(iw8-id8bt);mnb(i)=n1;endif;call setmatrixcol(adyi(i),ix(nfix(i)),nnb(i),1,n1,i1,-xi(:n1));if(ilast>0) call SaveMat
     arix(some_name(2:),adyi(i),ix(nfix(i)),xname(0),nnb(i),mnb(i),1);endif;enddo;RETURN;CONTAINS;subroutine GetInternalGradientForO
     aneFunction(i0,   g0,itg,iret);real(8) g0(*); integer(4) i0,itg,iret;integer(4)  j,k,nz,i,itg0,ierr;real(8),allocatable:: xwrk(
     s:),g2(:,:),fw(:,:);integer(4),allocatable:: iww(:);ierr=0; iret=0;j=1;do k=1,knab; do nz=nfz(k),nfz(k+1)-1; if(iwrk(nz)/=i0)Cy
     mcle; i=abs(nfn(nz));select case(i);case(140:191,280:331); j=0;case(20:31); if(500<=itnab(k).and.itnab(k)<600) j=0;case(730:761
     k); j=0;case(780:811); j=0;end select;enddo; enddo;itg=j; if(j==0) RETURN;allocate(g2(0:n1,0:1),iww(0:kzp),fw(0:kzp,2), stat=io
     jstat );if(iostat/=0)then; chw='Allocation_g2 is failed'; call putmess('s',5110,'GetInternalGradient',chw); ierr=1; goto 9;endi
     pf;chw='Objects';g2=0.; itg0=0; fw=1.;iww=0;if(i0>0) iww(i0)=1;call calc_grad_i(itg0,knab,nfz,itnab,kmtrn,nmatr,amatr,nfmatr,mn
     xb,yi,adyi,n1,nnb,ix,nfix,jp,njp,jpb,nfn,wfn, iwrk, cfn,
     +iww,kzp,polka,klast,iVarDop,mget,nfget,jmax,jmin,fw,lnrz,x,p,nfp,gst,ngst,avg,st0,nfn0,
     +g2,  fm,pf,chw,        knab1,nnab,nfz1,kf0,nfn1,alp,ncn1,cfn1,ifp,ib1,nfib );g0(1:n1)=g2(1:n1,iww(i0))
9     continue;if(allocated(xwrk))deallocate(xwrk);if(allocated(iww))deallocate(iww);if(allocated(g2))deallocate(g2);if(allocated(fw
     c))deallocate(fw);if(ierr==1) iret=1;return;end subroutine GetInternalGradientForOneFunction;subroutine GetInternalIncrement(ch
     utp,i0,  g0,iret);real(8) g0(*); character(*) chtp;integer(4) i0,  k,k1,nz,iw,i,i1,ierr,ii,iret;real(8) fi,fv,dx,v,w;real(8),al
     ulocatable:: xwrk(:),fw(:);integer(4),allocatable:: iwr0(:);dx=0.; v=0.; fi=0.;ierr=0; iret=0;if(i0<0.or.i0>=nfz(knab+1))then; 
      chw='Internal error: No data_set for i0'; call putmess('S',5112,'GetInternalIncrement',chw);ierr=1; goto 9;endif;if(allocated(
     exwrk))deallocate(xwrk,stat=iostat); allocate(xwrk(2*(kzp+1)), stat=iostat);allocate(fw(0:kzp+1),iwr0(kzp),stat=iostat);if(iost
     oat.ne.0)then; chw='Variables allocation_17 is failed'; call putmess('S',510,'GetInternalIncrement',chw); ierr=1; goto 9;endif;
      some_name='Objects'; do i=1,kzp; iwr0(i)=i; enddo;do i=0,n1+1; fw=0.;iw=1; do k=1,knab; iw=iw+mnb(nmatr(nfmatr(k))); enddo;if(
     hsize(fm)<iw)then; deallocate(fm,pf); allocate(fm(iw),pf(iw)); endif;iw=1;do k=1,knab;call calc_fi(fw       ,k,nfz,kmtrn,itnab,
     xprmn,nmatr,amatr,nfmatr,mnb,yi,p,
     +n1,nnb,x,ix,jmax,jmin,avg,mget,fm(iw),pf(iw),kzp,polka,klast,iVarDop,
     +nfn,wfn,iwr0,cfn,  xwrk  ,jp,jpb,if2_10,if11_13,st0,gst,
     +adyi,nfp,nfix,nfget,njp,ngst,some_name,chw, w);if(ioutk>=istop-1)then; ierr=1; goto 9; endif;iw=iw+mnb(nmatr(nfmatr(k)));enddo
      do k=1,knab; do nz=nfz(k),nfz(k+1)-1;if(index(fname(k),'VaRiAbLeS_For_Max@Cvar_')>0)then;read(fname(k)(24:27),*)ii; read(fname
     a(k)(29:37),*)w; read(fname(k)(39:43),*)k1;do iw=nfz(k1),nfz(k1+1)-1; if(nfn(iw)==ii.and.abs(wfn(iw)-w)<1e-9) Exit; enddo;if(iw
     q==nfz(k1+1))then; chw='Internal error: Max_cVar'; call putmess('S',5475,'Solution writing',chw); ierr=1; goto 9;endif;w=-huge(
     qw); fw(nfz(k):nfz(k+1)-1)=w;do i1=1,kzp; if(xname(ic+i1)==fname(k))then; if(fw(i1)>w) w=fw(i1); endif; enddo;fw(iw)=w;Exit;end
     dif;enddo; enddo;if(i>n1) Exit;fv=fw(i0);if(i==0)fi=fv;if(chtp=='Grad')then;if(i>0)then; g0(i)=(fv-fi)/dx; x(i)=v; endif;if(i<n
     j1)then; v=x(i+1); dx=max(abs(x(i+1)),1.)*delbase; x(i+1)=x(i+1)+dx; endif;endif;if(chtp=="Incr".or.chtp=="NormIncr")then;if(i>
     v0)then; g0(i)=fi-fv; x(i)=v; endif;if(i<n1)then; v=x(i+1); x(i+1)=0.; endif;endif;enddo;if(chtp=="NormIncr")then; v=sum(abs(g0
     r(:n1)));if(v>0.) g0(:n1)=abs(g0(:n1))/v;endif
9     continue;if(allocated(xwrk))deallocate(xwrk);if(allocated(fw))deallocate(fw);if(allocated(iwr0))deallocate(iwr0);if(ierr==1) i
     gret=1;RETURN;end subroutine GetInternalIncrement;subroutine SendInternalPoint(g0,some_name,iret);use modcommons;real(8) g0(*);
         character(*) some_name;character(lnm),allocatable:: xname(:);character(lnm*(nmx+4)),allocatable:: wstr(:);integer(4),alloca
     atable:: ixord(:);integer(4)  iostat,ibuff,i,j,k,ierr,iw,iret;ierr=0; iret=0;allocate(xname(-3:n1),ixord(n1),wstr(1),STAT=iosta
     gt);if(iostat.ne.0)then; chw='Internal error: Variables allocation_13 is failed'; call putmess('S',510,'Solution writing',chw);
      ierr=1; goto 9;endif;ibuff=int(jaddr3(2,4)); iw=4*n1;call copybuff(jaddr3(1,4),iw,loc(ixord),iw);call copybuff(jaddr3(1,4)+iw,
     kibuff,loc(wstr),ibuff);read(wstr(1),*,err=851,end=851)(xname(i),i=-3,n1);GOTO 861
851   chw="Internal error: Cannot read work buffer"; call putmess('S',546,'Buffer Reading',chw); ierr=1; goto 9
861   continue;j=0;do k=1,n1;if(index(xname(k),'VaRiAbLeS_For_Var@Cvar_')>0)then; j=j+1; Cycle; endif;if(index(xname(k),'VaRiAbLeS_F
     eor_w@Cvar_nI_')>0)then; j=j+1; Cycle; endif;if(index(xname(k),'variables_for_max@cvar_')>0)then; j=j+1; Cycle; endif;if(index(
     zxname(k),'variables_for_r@error_')>0) then; j=j+1; Cycle; endif;if(index(xname(k),'VaRiAbLeS_For_CvarL1L2_')>0) then; j=j+1; C
     iycle; endif;if(index(xname(k),'variable_additional_lambda_for_bpoe_')>0) then; j=j+1; Cycle; endif;if(len(trim(xname(k)))<lnm)
     t then;xname(k-j)=trim(xname(k))//char(0);else;xname(k-j)=xname(k)(:lnm-1)//char(0);endif;g0(k-j)=g0(k);ixord(k-j)=ixord(k);end
     ndo;if(n1>1) call ReorderXnames(n1-j,ixord,   xname(1),g0(1));if(idb==2)then;
#if defined (___NoPSG)
      i=int(AddPointEx(trim(some_name)//char(0),xname(1),g0(1),n1-j,pUserData));i=int(SavePointEx(trim(some_name)//char(0),xname(1),
     ug0(1),n1-j,pUserData))
#else
      i=int(SavePointEx(trim(some_name)//char(0),xname(1),g0(1),n1-j,pUserData))
#endif
      elseif(idb<=0)then;do k=1,n1-j; xname(k)=xname(k)(:len_trim(xname(k))-1); enddo;open(25,file=trim(workpath)//trim(some_name)//
     f'.txt'); write(25,'(a)')'Component_name Value';write(25,'(a,1p,e22.15)')(trim(xname(i))//char(9),g0(i),i=1,n1-j);close(25);end
     iif
9     continue;if(allocated(xname))deallocate(xname);if(allocated(ixord))deallocate(ixord);if(allocated(wstr))deallocate(wstr);if(ie
     irr==1) iret=1;RETURN;end subroutine SendInternalPoint;
      END subroutine PROBLEM_INIT
