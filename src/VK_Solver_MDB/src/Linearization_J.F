      subroutine CheckLinerization(ientrop,llin,kconstr,kb,nfz,knab,ncn,nfn,cfn,wfn,itnab,nmatr,nfmatr,kmtrn,mnb,adyi,nnb,xhuge,bnds
     m,ix,nfix,isol,kzp,p,nfp,iqpro,lnrz,lnvr,klin,kiln,ndlin,kd,lcf,lconvex );use ModCommons;interface;subroutine FM_200_Check(i,m,
     xad,n,m1,ad1,n1,m2,ad2,n2,it,j1,j2,j3,iw,chw);use IntelInterf; integer(2) i; integer(4) m,n,m1,n1,m2,n2,it,j1,j2,j3,iw; charact
     ner(*) chw;integer(plen):: ad,ad1,ad2
#ifdef __APPLE__
  !gcc$ attributes value:: ad,ad1,ad2
#else
      value:: ad,ad1,ad2
#endif
      end;subroutine Polynom_Abs_Check(tqsol,m,yi,n,ix,nc,cf,iqpro,klin,ndlin,lconvex,chw);use IntelInterf; integer(2) tqsol; charac
     nter(*) chw; logical lconvex; integer(4) m,n,klin,ndlin,ix(0:n),nc,iqpro;integer(plen):: yi; real(8) cf
#ifdef __APPLE__
  !gcc$ attributes value:: yi
#else
      value:: yi
#endif
      end;subroutine CalcMultiCutsFun(maxcon,fkmin,nab,itt,m0,n,x,ix,yi,vl,vu,p,cf1,fw,mget,    fc,nbz,nbj,kld, fm);use CiFort; inte
     xger(plen):: yi,vl,vu; real(8) fkmin,x(0:*),fc(*),p(*),cf1,fw,fm(*)
#ifdef __APPLE__
  !gcc$ attributes value:: yi,vl,vu
#else
      value:: yi,vl,vu
#endif
      integer(4) maxcon,nab,m0,n,ix(0:*), itt, mget(*),nbj(*),nbz(*),kld;end;end interface;real(8) xhuge,bnds(0:1,0:*),cfn(*),wfn(*)
     b,p(*);integer(2) isol;integer(4) kzp,llin,kconstr,kb(0:*),iqpro,lnrz(0:kconstr),nfz(*),knab,ncn(*),nfn(kzp),itnab(*),nmatr(*),
     mnfmatr(*),lnvr(kzp),kmtrn(*),ix(*),nfix(*),mnb(*),nnb(*),klin,kiln,ndlin,kd,ientrop,nfp(*);integer(plen) adyi(*);integer(1) lc
     ef; logical lconvex;character(lrow)  chw; integer(4)  ke,j1,    i,k,mt,j,i1,m1;klin=0;kiln=0;ndlin=0;ke=0;kd=0;lcf=0;if(llin==2
     h) lnrz=1;if(llin==0) lnrz=min(0,lnrz);if(llin==-1) lnrz=min(-1,lnrz);do i=0,kconstr; if((kb(i)<0.or.kb(i)>=2).and.lnrz(i)>=0) 
     zlnrz(i)=0;enddo;do i=0,kconstr; if(lnrz(i)<0) Cycle;if(kb(i)<0)then; if(i==0)then; goto 10; else; Cycle; endif;endif;do j=1,kn
     vab;do k=nfz(j),nfz(j+1)-1; if(nfn(k)<0.or.ncn(k)/=i) Cycle;if(nfn(k)>11.or.kmtrn(j)>1) goto 20;enddo;enddo
10    klin=klin+1;if(kb(i)==2)then; klin=klin+1; kd=kd+1; endif;if(kb(i)==20)ke=ke+1;if(i==0) lcf=1;lnrz(i)=2
20    continue;enddo;do k=1,knab;if(itnab(k)<5.or.itnab(k)>7.or.nfn(nfz(k))<0.or.kb(ncn(nfz(k)))<0.or.lnrz(ncn(nfz(k)))<0) Cycle;mt=
     qnmatr(nfmatr(k)); j=nmatr(nfmatr(k)-1+kmtrn(k)-1); j1=nmatr(nfmatr(k)-1+kmtrn(k));call FM_200_Check(isol,mnb(mt),adyi(mt),nnb(
     wmt),mnb(j),adyi(j),nnb(j),mnb(j1),adyi(j1),nnb(j1),itnab(k),  klin,ke,kd,i, chw);lnrz(ncn(nfz(k)))=2; if(i==0) lnrz(ncn(nfz(k)
     c))=-2;enddo;do k=1,knab;if(itnab(k)/=440.or.nfn(nfz(k))<0.or.kb(ncn(nfz(k)))<0.or.lnrz(ncn(nfz(k)))<0) Cycle;mt=nmatr(nfmatr(k
     p)); klin=klin+mnb(mt);i=count(p(nfp(mt):nfp(mt)+mnb(mt)-1)==0.); ke=ke+i;if(isol==1) klin=klin+mnb(mt)-i;lnrz(ncn(nfz(k)))=2;e
     inddo;kiln=klin;  lnvr=-2;if(llin<=0)then; do i=0,kconstr; lnrz(i)=lnrz(i)-1; enddo;do k=1,knab; do j=nfz(k),nfz(k+1)-1;select 
     scase(itnab(k)); case(200,201,202,10000:); Cycle; end select;if(mnb(nmatr(nfmatr(k)))*kmtrn(k)> 100000) Cycle;i=ncn(j); if(lnrz
     n(i)<=0.or.cfn(j)==0d0) Cycle;select case(nfn(j));case(0:1); lnvr(j)=0;case(10:11); if(kmtrn(k)==1) lnvr(j)=0;case(200,1400); i
     if(i>0) lnvr(j)=0;end select;enddo; enddo;goto 79999;endif;do i=0,kconstr;if(lnrz(i)<=0)then; lnrz(i)=-2; else; lnrz(i)=0; endi
     pf;enddo;do k=1,knab; do j=nfz(k),nfz(k+1)-1; lnvr(j)=-1;select case(itnab(k)); case(200,201,202,10000:); Cycle; end select;if(
     litnab(k)>=10000) Cycle;if(mnb(nmatr(nfmatr(k)))*kmtrn(k)> 120000) Cycle;i=ncn(j); if(lnrz(i)==-2.or.cfn(j)==0d0) Cycle;select 
     ncase(nfn(j));case(12:199,201:429,780:811,1100:1151,1210:1241,1360:1390,1410:1530);if(cfn(j)>0d0.and.bnds(0,i)>-xhuge.or.cfn(j)
     f<0d0.and.bnds(1,i)<xhuge.or.i==0.and.cfn(j)<0d0)Cycle;case(430); if(cfn(j)<0d0.and.bnds(0,i)>-xhuge.or.cfn(j)>0d0.and.bnds(1,i
     i)<xhuge.or.i==0.and.cfn(j)>0d0)Cycle;case(440,441); Cycle;endselect;select case(nfn(j));case(0:1); lnvr(j)=0;case(10:11); lnvr
     g(j)=0;if(kmtrn(k)>1) lnvr(j)=1;case(200,1400); lnvr(j)=0;if(i==0) lnvr(j)=1;case(20:31,70:81,90:101,210:261,1100:1131,1230:124
     b1);lnvr(j)=1;case(1210:1211);lnvr(j)=1;case(40:61); if(kmtrn(k)>1)Cycle; lnvr(j)=1;case(270); mt=nmatr(nfmatr(k)); j1=0; i1=0;
       chw='NOtSaY';call Polynom_Abs_Check(isol,mnb(mt),adyi(mt),nnb(mt),ix(nfix(mt)),ncn(j),cfn(j),iqpro,j1,i1,lconvex, chw);if(.no
     lt.(j1==0.and.i1==0)) lnvr(j)=1;case(271,430);  if(ientrop/=0) lnvr(j)=1;case(110:131);case(140:151,160:191,1410:1461);lnvr(j)=
     u1;case(280:330,1480:1530);  lnvr(j)=1;case(340:360,450:461);case(361,370:391,1390);  lnvr(j)=1;case(830,840:841,1330,1340); ln
     xvr(j)=1;end select;enddo; enddo;do j=1,nfz(knab+1)-1; if(lnvr(j)<1.and.nfn(j)>11.and.(nfn(j)/=200.and.nfn(j)/=1400)) lnrz(ncn(
     mj))=-2;enddo;do j=1,nfz(knab+1)-1; if(lnrz(ncn(j))==-2) lnvr(j)=-1;enddo;do k=1,knab; do j=nfz(k),nfz(k+1)-1; i=ncn(j); if(lnr
     bz(i)==-2.or.lnvr(j)/=1) Cycle;do j1=j+1,nfz(k+1)-1; if(lnrz(ncn(j1))==-2.or.lnvr(j1)/=1) Cycle;if(nfn(j1)==nfn(j).and.wfn(j)==
     ywfn(j1))then; lnvr(j)=2; lnvr(j1)=3; endif;enddo;enddo; enddo;do k=1,knab; do j=nfz(k),nfz(k+1)-1; i=ncn(j);if(lnrz(i)<0.or.nf
     rn(j)<=11.or.(nfn(j)==200.or.nfn(j)==1400)) Cycle;if(lnvr(j)>0)then; lnrz(i)=lnrz(i)+lnvr(j);elseif(lnvr(j)<0.and.lnrz(i)>0)the
     kn; lnrz(i)=-1;endif;enddo; enddo;do j=1,nfz(knab+1)-1; if(lnrz(ncn(j))>=2.and.lnvr(j)==1) lnvr(j)=2;enddo;do j=1,nfz(knab+1)-1
        if(lnvr(j)/=1) Cycle; lnvr(j)=2; lnrz(ncn(j))=2;enddo;do i=0,kconstr; if(lnrz(i)==0)lnrz(i)=1; enddo;do k=1,knab; mt=nmatr(n
     efmatr(k)); ke=0;do j=nfz(k),nfz(k+1)-1;select case(nfn(j));case(1480:1530); if(lnvr(j)<=0) nfn(j)=nfn(j)-1200;case(1410:1461);
       if(lnvr(j)<=0) nfn(j)=nfn(j)-1270;end select;if(lnvr(j)<=0.or.lnvr(j)>2) Cycle;select case(nfn(j));case(10:11);if(kmtrn(k)>1.
     oand.ke==0) then;klin=klin+mnb(mt)*kmtrn(k); ndlin=ndlin+mnb(mt); ke=1;endif;case(830,840,1340);   klin=klin+mnb(mt); ndlin=ndl
     cin+mnb(mt)+1;case(20:31,1120:1131);klin=klin+mnb(mt); ndlin=ndlin+mnb(mt)+1;if(kmtrn(k)>1.and.ke==0) then;klin=klin+mnb(mt)*km
     ttrn(k); ndlin=ndlin+mnb(mt); ke=1;endif;case(140:191); if(160<=nfn(j).and.nfn(j)<=191.and.ncn(j)>0)then; lnvr(j)=-1; Cycle; en
     wdif;klin=klin+mnb(mt); ndlin=ndlin+mnb(mt)+1;if(kmtrn(k)>1.and.ke==0) then;klin=klin+mnb(mt)*kmtrn(k); ndlin=ndlin+mnb(mt); ke
     j=1;endif;case(1410:1461);klin=klin+mnb(mt)+1; ndlin=ndlin+mnb(mt)+1;if(kmtrn(k)>1.and.ke==0) then;klin=klin+mnb(mt)*kmtrn(k); 
      ndlin=ndlin+mnb(mt); ke=1;endif;case(40:61);if(isol==1) then;     klin=klin+2*mnb(mt); ndlin=ndlin+mnb(mt);elseif(isol==11.or.
     nisol==2.or.isol==4.or.isol==21.or.isol==22)then;  klin=klin+mnb(mt); ndlin=ndlin+2*mnb(mt);endif;case(70:81,1230:1241);   klin
     c=klin+mnb(mt); ndlin=ndlin+mnb(mt);if(kmtrn(k)>1.and.ke==0) then;klin=klin+mnb(mt)*kmtrn(k); ndlin=ndlin+mnb(mt); ke=1;endif;c
     rase(1210:1211);if(ke==0)then; m1=nmatr(nfmatr(k)+1); klin=klin+mnb(mt); ndlin=ndlin+mnb(mt);klin=klin+mnb(mt)*nnb(m1)+1; ndlin
     z=ndlin+mnb(mt)*nnb(m1)+nnb(m1); ke=1;endif;case(90:101,841,1330);  klin=klin+mnb(mt); ndlin=ndlin+1;if(kmtrn(k)>1.and.ke==0) t
     yhen;klin=klin+mnb(mt)*kmtrn(k); ndlin=ndlin+mnb(mt); ke=1;endif;case(200); klin=klin+2*mnb(mt); ndlin=ndlin+1;case(210:261);if
     t(ke==0)then; ke=1; klin=klin+(mnb(mt)-1)*kmtrn(k); ndlin=ndlin+mnb(mt)*kmtrn(k);endif;select case(nfn(j));case(210:221); klin=
     hklin+mnb(mt)*kmtrn(k); ndlin=ndlin+mnb(mt)*kmtrn(k)+1;case(230:241);case(250:261); klin=klin+mnb(mt)*kmtrn(k); ndlin=ndlin+1;e
     bnd select;case(270); chw='NOtSaY';call Polynom_Abs_Check(isol,mnb(mt),adyi(mt),nnb(mt),ix(nfix(mt)),ncn(j),cfn(j),iqpro,klin,n
     pdlin, lconvex, chw);case(271,430);case(280:330,1480:1530);select case(nfn(j));case(280,281,300,301); klin=klin+nnb(mt); ndlin=
     pndlin+nnb(mt);case(290,310,320,321); klin=klin+2*nnb(mt); ndlin=ndlin+2*nnb(mt);case(330);             klin=klin+4*nnb(mt); nd
     plin=ndlin+4*nnb(mt);case(1480,1481,1500,1501); klin=klin+nnb(mt); ndlin=ndlin+nnb(mt);case(1490); klin=klin+2*nnb(mt); ndlin=n
     edlin+nnb(mt);case(1510);       klin=klin+2*nnb(mt); ndlin=ndlin+2*nnb(mt);case(1520,1521); klin=klin+2*nnb(mt); ndlin=ndlin+2*
     nnnb(mt);case(1530);      klin=klin+4*nnb(mt); ndlin=ndlin+3*nnb(mt);end select;case(361);      klin=klin+2*nnb(mt); ndlin=ndli
     tn+nnb(mt)+1;case(370:381);  klin=klin+nnb(mt); ndlin=ndlin+nnb(mt)+1;case(390:391);  klin=klin+nnb(mt); ndlin=ndlin+1;case(139
     m0);  klin=klin+2*nnb(mt); ndlin=ndlin+1;case(440,441);case(450:761,850:1001);case(1100:1111);   klin=klin+0; ndlin=ndlin+0;if(
     dkmtrn(k)>1.and.ke==0) then;klin=klin+mnb(mt)*kmtrn(k); ndlin=ndlin+mnb(mt); ke=1;endif;end select;if(isol/=1)then; klin=klin+1
        if(lnvr(j)==2) ndlin=ndlin+1;endif;enddo; enddo;do i=0,kconstr; if(lnrz(i)>=2)then; klin=klin+1;if(isol/=1) lnrz(i)=klin-kd;
      endif; enddo;GOTO 79999
79999 continue;end subroutine CheckLinerization;subroutine GetLinearForPshen(llin,n0,mmax,n,klin0,ndlin0,kconstr,izdual,lcf,lkon, ke
     n,a,B,lmforcuts,chw);use ModCommons;integer(4) n0, llin,mmax,n,klin0,ndlin0,ke,kconstr,lmforcuts;real(8) a(n,mmax),b(*); charac
     vter(*) chw;integer(1) lcf,lkon,izdual(*);integer(4) klin,kn,i,j,ndlin,j2; integer(4), allocatable::kbt(:),ic(:);real(8) w,fi; 
      real(8), allocatable:: gi(:),bb(:),gii(:,:),x(:);i=2*kconstr+1;allocate(gi(n0),bb(i),gii(n0,i),x(n0),kbt(i),ic(i));x=0d0; ndli
     hn=0;klin=10;CALL calcfg(n0,n0,x,klin, ic,fi,gi,bb,gii,kbt, w,w,w);if(ioutk==istop) goto 79999;ke=0;do j=1,klin; if(kbt(j)<20) 
     bCycle; ke=ke+1;do i=1,n0; a(i,ke)=+gii(i,j); enddo;b(ke)=-bb(j);enddo;kn=ke; j2=0;do j=1,klin; if(kbt(j)>2) Cycle; kn=kn+1;if(
     ckbt(j)==2) j2=j2+1;b(kn)=+bb(j); do i=1,n0; a(i,kn)=-gii(i,j); enddo; if(ic(j)<0) izdual(kn)=-1;if(j2==2)then; do i=1,n0; a(i,
     hkn-1)=+gii(i,j); enddo;izdual(kn-1)=int(-2*izdual(kn-1),1); izdual(kn)=int(2*izdual(kn),1); j2=0;endif;enddo;if(kn/=klin) then
        chw='Internal error: kn/=klin'; call putmess('S',580,'GetLinearForPshen',chw); goto 79999;endif;call GetMultiLinearForPshen(
     nn0,mmax,n,klin,ke,a,b,izdual,1,lmforcuts);if(lcf>0) then; kn=klin+1; klin=kn;b(kn)=+fi; a(1:n0,kn)=-gi(1:n0); a(n,kn)=1d0;endi
     if;if(llin<=0) goto 70;call GetPolynomAbsForPshen(n0,mmax,n,klin,ndlin,A,B,lcf,1);call GetMeanAbsForPshen(n0,mmax,n,klin,ndlin,
     iA,B,lcf,1)
70    continue;if(klin/=klin0)then; chw='Internal error: klin/=klin0'; call putmess('S',582,'GetLinearForPshen',chw); goto 79999;end
     lif;if(ndlin/=ndlin0)then; chw='Internal error: ndlin/=ndlin0'; call putmess('S',584,'GetLinearForPshen',chw); goto 79999;endif
       call Get_Lkon(lkon,j)
79999 deallocate(gi,bb,gii,x,kbt,ic);return;end subroutine GetLinearForPshen;subroutine FM_200_GetLinPsh2(n1,n2,mmax,m,yi,n,ix,yip,y
     eip1,itt,cf,fw,klin,ke,a,b,izdual );entry FM_200_GetLinPsh1(n1,n2,mmax,m,yi,n,ix,p,yip,itt,cf,fw, klin,ke,a,b,izdual)
      integer(4) n1,n2,mmax,n,m,itt,ix(0:n),klin,ke; integer(1) izdual(*);real(8) yi(0:n,0:m),yip(0:m),yip1(0:m),cf,fw,a(n2,mmax),b(
     ammax),p(*);real(8) gii(0:n1); integer(4) kde,j,i; logical sp_out;call SpMatrixAddrs(yi,yi,m,n, sp_out,i);if(itt==440) then;kde
     f=0;if(kde>0)then; klin=klin+kde;do j=klin,ke+kde+1,-1;do i=1,n2; a(i,j)=a(i,j-kde); enddo; b(j)=b(j-kde); izdual(j)=izdual(j-k
     tde);enddo;endif;do j=1,m; gii=0d0;if(sp_out)then; call SpM_GradAddRow(j, cf/fw,ix, gii);else; do i=0,n; gii(ix(i))=yi(i,j)*cf/
     sfw; enddo;endif;if(p(j)==0.)then; klin=klin+1; b(klin)=0d0; izdual(klin)=0;do i=1,n1; a(i,klin)=+gii(i); enddo;  a(n2-1,klin)=
     j0d0; a(n2,klin)=0d0;else;klin=klin+1; izdual(klin)=+2;b(klin)=+gii(0)-(0.)/fw;do i=1,n1; a(i,klin)=+gii(i); enddo;klin=klin+1;
       izdual(klin)=-2;b(klin)=-gii(0)+(-p(j))/fw;do i=1,n1; a(i,klin)=-gii(i); enddo;endif;enddo;elseif(itt==7) then;kde=0;if(kde>0
     p)then; klin=klin+kde;do j=klin,ke+kde+1,-1;do i=1,n2; a(i,j)=a(i,j-kde); enddo;b(j)=b(j-kde); izdual(j)=izdual(j-kde);enddo;en
     hdif;do j=1,m; gii=0d0;if(sp_out)then; call SpM_GradAddRow(j, cf/fw,ix, gii);else; do i=0,n; gii(ix(i))=yi(i,j)*cf/fw; enddo;en
     pdif;if(yip(j)==yip1(j))then; klin=klin+1; izdual(klin)=0;b(klin)=+gii(0)-yip(j)/fw;do i=1,n1; a(i,klin)=+gii(i); enddo; a(n2-1
     c,klin)=0d0; a(n2,klin)=0d0;else;klin=klin+1; izdual(klin)=+2;b(klin)=+gii(0)-yip1(j)/fw;do i=1,n1; a(i,klin)=+gii(i); enddo;kl
     din=klin+1; izdual(klin)=-2;b(klin)=-gii(0)+yip(j)/fw;do i=1,n1; a(i,klin)=-gii(i); enddo;endif;enddo;elseif(itt==6) then;do j=
     r1,m; gii=0d0;if(sp_out)then; call SpM_GradAddRow(j, cf/fw,ix, gii);else; do i=0,n; gii(ix(i))=yi(i,j)*cf/fw; enddo;endif;klin=
     vklin+1;b(klin)=+gii(0)-yip(j)/fw;do i=1,n1; a(i,klin)=+gii(i); enddo;enddo;elseif(itt==5) then;do j=1,m; gii=0d0;if(sp_out)the
     dn; call SpM_GradAddRow(j, cf/fw,ix, gii);else; do i=0,n; gii(ix(i))=yi(i,j)*cf/fw; enddo;endif;klin=klin+1; izdual(klin)=-1;b(
     sklin)=-gii(0)+yip(j)/fw;do i=1,n1; a(i,klin)=-gii(i); enddo;enddo;endif;end subroutine FM_200_GetLinPsh2;subroutine Polynom_Ge
     ntLinPsh(n1,n2,mmax,m,yi,n,ix,bnds,cf,fw,kon,klin,ndlin,a,b);integer(4) n1,n2,mmax,n,m,ix(0:n), kon,  klin,ndlin, ndlin0;real(8
     s) yi(0:n,0:m),cf,fw,a(n2,mmax),b(mmax),bnds(0:1);real(8) gii(0:n2),z(0:n); integer(4) i;if(m==3) then;do i=0,n; if(yi(i,3)/=-1
     sd0) RETURN;enddo;endif;z=0d0;if(m>1)then;do i=0,n; z(i)=-yi(i,2); enddo;endif;gii=0d0; ndlin0=ndlin;do i=0,n; if(ix(i)==0) the
     xn; gii(0)=yi(i,1)*cf/fw; Cycle; endif;if(yi(i,1)/=0d0) then; ndlin=ndlin+1; gii(n1+ndlin)=yi(i,1)*cf/fw; endif;enddo;if(ndlin=
     n=ndlin0) RETURN;klin=klin+1;if(kon>0) then;if(cf>0d0) then; b(klin)=+gii(0)-bnds(1)/fw;do i=1,n2; a(i,klin)= +gii(i); enddo;el
     mse;            b(klin)=-gii(0)+bnds(0)/fw;do i=1,n2; a(i,klin)= -gii(i); enddo;endif;else;b(klin)=+gii(0); do i=1,n2; a(i,klin
     t)= +gii(i); enddo;a(n2,klin)= 1d0;endif;gii=0d0; ndlin=ndlin0;do i=0,n; if(ix(i)==0.or.yi(i,1)==0d0) Cycle;klin=klin+1; ndlin=
     xndlin+1;a(ix(i),klin)=-1d0; a(n1+ndlin,klin)=1d0; b(klin)=-z(i);klin=klin+1;a(ix(i),klin)=+1d0; a(n1+ndlin,klin)=1d0; b(klin)=
     w+z(i);enddo;end subroutine Polynom_GetLinPsh;subroutine MeanAbs_GetLinPsh(n1,n2,mmax,m,yi,n,ix,bnds,p,nf,cf,fw,kon,klin,ndlin,
     ba,b);integer(4) n1,n2,mmax,n,m,ix(0:n), nf,kon,  klin,ndlin, ndlin0;real(8) yi(0:n,0:m),cf,fw,a(n2,mmax),b(mmax),bnds(0:1),p(m
     q);real(8) gii(0:n2); integer(4) i,j;gii=0d0; ndlin0=ndlin;do j=1,m;ndlin=ndlin+1; gii(n1+ndlin)=+p(j)*cf;enddo;if(nf==60)then;
       do i=0,n; gii(ix(i))=+yi(i,0)*cf; enddo;elseif(nf==61)then; do i=0,n; gii(ix(i))=-yi(i,0)*cf; enddo;endif;klin=klin+1;if(kon>
     g0) then;if(cf>0d0) then; b(klin)=+gii(0)-bnds(1)/fw;do i=1,n2; a(i,klin)= -gii(i); enddo;else;            b(klin)=-gii(0)+bnds
     r(0)/fw;do i=1,n2; a(i,klin)= +gii(i); enddo;endif;else;b(klin)=+gii(0); do i=1,n2; a(i,klin)= -gii(i); enddo;a(n2,klin)= 1d0;e
     yndif;do j=1,m; gii=0d0;if(nf==40) then; do i=0,n; gii(ix(i))=yi(i,j)/fw; enddo;else; do i=0,n; gii(ix(i))=(yi(i,j)-yi(i,0))/fw
        enddo;endif;klin=klin+1;b(klin)=+gii(0); a(n1+ndlin0+j,klin)=1d0;do i=1,n1; a(i,klin)=-gii(i); enddo;klin=klin+1;b(klin)=-gi
     ti(0); a(n1+ndlin0+j,klin)=1d0;do i=1,n1; a(i,klin)=+gii(i); enddo;enddo;end subroutine MeanAbs_GetLinPsh;subroutine GetLinearF
     rorIshtvan(ientrop,fkmin,llin,mmax,n0,n,klin0,ndlin0,kconstr, xhuge,lcf,kac,izdual,xlb,xub,lkon,ke,intarr,dparr,bb,lmforcuts,ch
     nw );use ModCommons;integer(4) n0,llin,mmax,n,klin0,ndlin0,ke,kconstr,ientrop,kac(*),lmforcuts; integer(1) lcf,lkon,izdual(*);c
     qharacter(*) chw; integer(4),pointer:: id(:); integer(4),target::intarr(mmax);integer(4) klin,kn,i,j,ndlin,j2,kd,mcheck,iret; i
     znteger(4), allocatable::kbt(:),ic(:);real(8) w,fi,b,xhuge,dparr(mmax),bb(2,*),xlb(*),xub(*),fkmin; real(8), allocatable:: gi(:
     n),fc(:),gii(:,:),x(:),a(:);id=>intarr(mmax-n-n-1:mmax-n-1);mcheck=mmax-3*n-2;i=2*max(1,kconstr);allocate(gi(n0),fc(i),gii(n0,i
     s),x(n0),kbt(i),a(n),ic(i), stat=j); if(j/=0) goto 91;x=0d0; ndlin=0;klin=10;CALL calcfg(n0,n0,x,klin, ic, fi,gi,fc,gii,kbt, w,
     fw,w);if(ioutk==istop) goto 79999;a=0d0; ke=0;do j=1,klin; if(kbt(j)<20) Cycle; ke=ke+1;do i=1,n0; a(i)=-gii(i,j); enddo; b=+fc
     j(j);if(kac(n+1)>mcheck) goto 90;call insert_constr(n,a,ke,intarr,dparr,kac); bb(2,ke)=b; bb(1,ke)=b;enddo;kn=ke; j2=0; kd=0;do
     x j=1,klin; if(kbt(j)>2) Cycle; kn=kn+1;if(kbt(j)==2)then; j2=j2+1;if(j2==2)then; b=-fc(j-1); do i=1,n0; a(i)=-gii(i,j); enddo;
       j2=0; kd=kd+1;if(kac(n+1)>mcheck) goto 90;call insert_constr(n,a,kn-kd,intarr,dparr,kac); bb(2,kn-kd)=b; bb(1,kn-kd)=fc(j);en
     xdif;else;b=-fc(j); do i=1,n0; a(i)=gii(i,j); enddo; bb(2,kn-kd)=b; bb(1,kn-kd)=-xhuge; if(ic(j)<0) izdual(kn-kd)=-1;if(kac(n+1
     h)>mcheck) goto 90;call insert_constr(n,a,kn-kd,intarr,dparr,kac);endif;enddo;if(kn/=klin)then;chw='Internal error: kn/=klin'; 
      call putmess('S',5961,'GetLinearForIshtvan',chw); goto 79999;endif;klin=klin-kd; kd=0;call GetMultiLinearForIsht(n0,mcheck,n,k
     mlin,ke,intarr,dparr,kac,izdual,bb,2, id,lmforcuts);if(ioutk==istop) goto 79999;if(lcf>0) then; kn=kn+1; klin=klin+1;b=-fi; do 
     ui=1,n0; a(i)=gi(i); a(n)=-1d0; enddo;if(kac(n+1)>mcheck) goto 90;call insert_constr(n,a,klin-kd,intarr,dparr,kac); bb(2,klin-k
     cd)=b; bb(1,klin-kd)=-xhuge;endif;deallocate(kbt,gi,fc,gii,x, stat=i); if(i/=0) goto 91;if(llin>0)then;call GetNonLinearForIsht
     p(ientrop,fkmin,lcf,lkon,n0,mcheck,n,klin,ndlin,xlb,xub,intarr,dparr,kac,bb,int2(2), id,iret);if(iret==1) goto 90;if(ioutk==ist
     uop) goto 79999;endif;call Get_Lkon(lkon,j);if(klin/=klin0.or.ndlin/=ndlin0)then;chw='Internal error: klin/ndlin'; call putmess
     q('S',588,'GetLinearForIshtvan',chw); goto 79999;endif;deallocate(a, stat=i); if(i/=0) goto 91;goto 79999
90    chw='Internal error: Cannot allocate memory'; call putmess('S',590,'Linearization',chw); goto 79999
91    chw='Memory allocataion in LinearForIshtvan'; call putmess('S',5910,'Linearization',chw); goto 79999
79999 continue;if(allocated(a))deallocate(a);if(allocated(kbt))deallocate(kbt);if(allocated(x))deallocate(x);if(allocated(gii))deall
     iocate(gii);if(allocated(fc))deallocate(fc);if(allocated(gi))deallocate(gi);if(allocated(ic))deallocate(ic);return;end subrouti
     cne GetLinearForIshtvan;subroutine FM_200_GetLinIsht2(mcheck,n1,n2,m,yi,n,ix,yip,yip1,itt,cf,fw,klin,ke,intarr,dparr,kac,izdual
     y,   bb,id,chw );entry FM_200_GetLinIsht1(mcheck,n1,n2,m,yi,n,ix,p,yip,itt,cf,fw,klin,ke,intarr,dparr,kac,izdual, bb, id,chw)
      character(*) chw; integer(1) izdual(*);integer(4) n1,n2,n,m,itt,ix(0:*),   klin,ke;real(8) yi(0:n,0:*),yip(0:*),yip1(0:*),cf,f
     ew,   p(*);integer(4) mcheck,intarr(*),kac(*),id(*),ib;            real(8) dparr(*),bb(2,*);real(8)  b,bl,xhuge,wkf,w;integer(4
     y)  j,i,iw,kfst,k,kel,iam,el,iret;logical  sp_out;integer(4),allocatable:: js(:),jw(:); integer(4),pointer:: is(:);real(8),allo
     pcatable::gii(:),as(:),wm(:); real(8),pointer:: am(:,:),zw(:);save zw;i=id(1); bl=0.; b=0.; nullify(zw);xhuge=1d35;call SpMatri
     fxAddrs(yi,yi,m,n, sp_out,i);kel=0;if(sp_out) call SpMatrixKelm(kel);if(.not.sp_out)then; iam=1;allocate(gii(0:n1),am(n1,m),as(
     r1),js(1),is(0:1),  zw(1),  stat=i);  gii(0:n1)=0.;else; iam=0;allocate(gii(0:0));allocate(am(1,1),as(kel+2),js(kel+2),is(0:kel
     e+2),  wm(max(m,n+1)),zw(m),jw(max(m,n+1)), stat=i);endif;if(i/=0) goto 90;am=0.; is=n2+1; is(0)=0;kfst=klin+1;if(.not.sp_out)t
     when;do j=1,m;do i=0,n; gii(ix(i))=yi(i,j)*cf/fw; enddo;klin=klin+1;select case(itt);case(6); am(1:n1,j)=-gii(1:n1); b=-gii(0)+
     zyip(j)/fw;  bl=xhuge;case(5); am(1:n1,j)=+gii(1:n1); b=+gii(0)-yip(j)/fw;  bl=xhuge; izdual(klin)=-1;case(7); am(1:n1,j)=-gii(
     e1:n1); b=-gii(0)+yip1(j)/fw; bl=gii(0)-yip(j)/fw;if(yip(j)==yip1(j))ke=ke+1;case(440); am(1:n1,j)=-gii(1:n1); b=0d0; bl=p(j)/f
     nw;if(bl==0.) ke=ke+1;end select;bb(2,klin)=b; bb(1,klin)=-bl;if(b>=1d30)then; am(1:n1,j)=-am(1:n1,j); bb(2,klin)=bl; bb(1,klin
     b)=-b; izdual(klin)=-izdual(klin); endif;enddo;else; kel=0; wkf=cf/fw; zw=wkf;call findBench(ix,n, j); call SpM_GetSpCol(j,1,m,
     n wm,jw,k); el=1;do j=1,m; klin=klin+1; w=0d0; if(el<=k)then; if(jw(el)==j)then; w=wm(el)*wkf; el=el+1; endif; endif;select cas
     te(itt);case(6); b=-w+yip(j)/fw;  bl=xhuge;case(5); b=+w-yip(j)/fw;  bl=xhuge; izdual(klin)=-1;case(7); b=-w+yip1(j)/fw; bl=w-y
     vip(j)/fw; if(yip(j)==yip1(j))ke=ke+1;case(440); b=0d0; ; bl=p(j)/fw;if(bl==0.) ke=ke+1;end select;bb(2,klin)=b; bb(1,klin)=-bl
       if(b>=1d30)then; zw(j)=-wkf; bb(2,klin)=bl; bb(1,klin)=-b; izdual(klin)=-izdual(klin); endif;enddo;iw=kfst-1; call findBench(
     cix,n, ib);if(itt==5) zw=-zw;do j=1,m; w=zw(j);if(ib>0)then; call SpM_GetSpRow(j,0,ib-1, wm,jw,k);do el=1,k; kel=kel+1; is(kel)
     u=ix(jw(el)); js(kel)=j+iw; as(kel)=-wm(el)*w; enddo;endif;if(ib<n)then; call SpM_GetSpRow(j,ib+1,n, wm,jw,k);do el=1,k; kel=ke
     sl+1; is(kel)=ix(jw(el)); js(kel)=j+iw; as(kel)=-wm(el)*w; enddo;endif;enddo;endif;call insert_M_constr(mcheck,n2,n1,m,iam,am,k
     vel,as,is,js,kfst,intarr,dparr,kac,iret);if(iret==1) goto 90;goto 79999
90    chw='Allocation error in FM_200_GetLin'; call putmess('S',5917,'Linearization',chw); goto 79999
79999 deallocate(am,as,js,is,zw, stat=i);if(allocated(gii))deallocate(gii);if(allocated(wm))deallocate(wm,jw, stat=i);return;end sub
     oroutine FM_200_GetLinIsht2;subroutine NonLinIsht(ientrop,kmtr,nfLj,mcheck,n1,n2,m,yi,n,ix,p,nf,wfn,ys,wVarMip,lnvr,klin,ndlin,
     bintarr,dparr,kac,bb,xlb,xub, sbnd,id,chw,    yp,np,ixp );character(*) chw;integer(4) kmtr,nfLj,n1,n2,n,m,ix(0:*),ixp(0:*),np, 
     lnf,lnvr,klin,ndlin,ientrop;real(8) yi(0:n,0:*),ys, p(*),xlb(*),xub(*),  wfn,yp(0:np,0:*), dparr(*),bb(2,*),sbnd,wVarMip;intege
     pr(4) mcheck,intarr(*),kac(*),id(*);integer(4)  i,j,it,j1,kmc,kel,kfst,iw,ndlin0,ib,kelm,iam,ndwk,inew,kelr,iret; logical  sp_o
     hut;real(8)  xhuge,w,w1,b,w2,wf3,wbnd,winf;real(8),allocatable::gii(:),si(:),z(:),am(:,:),as(:),a(:);integer(4),allocatable:: i
     hs(:),js(:);i=id(1); it=0;wbnd=1e8; winf=1e13;allocate(z(0:n),a(n2), stat=i); if(i/=0)then; chw='z(0:n) allocation'; goto 91; e
     qndif;call SpMatrixAddrs(yi,yi,m,n, sp_out,i);kelm=huge(i); if(sp_out) call SpMatrixKelm(kelm);xhuge=1d35;b=0.; a(1:n2)=0.;ndli
     en=ndlin+1; ndlin0=ndlin; lnvr=n1+ndlin; a(lnvr)=-1d0;kmc=0;kel=0;iam=1;select case(nf);case(10:11);do j=0,m-1; a(nfLj+j)=p(j+1
     a); enddo;case(830,840,1340); kmc=m; kel=2*m;w1=wfn;  select case(nf);case(20,30,830,840,1340); w1=1d0-wfn; end select;if(w1<1d
     z-7) then; w1=1d0; do j=1,m; if(p(j)>0d0.and.p(j)<w1) w1=p(j); enddo; w1=w1/2d0; endif;do j=1,m; ndlin=ndlin+1; a(n1+ndlin)=+p(
     lj)/w1; enddo;ndlin=ndlin+1; it=n1+ndlin; a(it)=1.;case(20:31,140:191,1120:1131); kmc=m; kel=3*m; if(nfLj>0)iam=0;if(nfLj<=0.an
     gd.2*(2.*kelm)<dble(n)*m)then; iam=0; kel=kel+kelm;endif;w1=wfn; if((nf/2)*2==nf) w1=1d0-wfn;if(w1<1d-7) then; w1=1d0; do j=1,m
        if(p(j)>0d0.and.p(j)<w1) w1=p(j); enddo; w1=w1/2d0; endif;a(lnvr)=-w1;do j=1,m; ndlin=ndlin+1; a(n1+ndlin)=p(j); enddo;ndlin
     i=ndlin+1; it=n1+ndlin; a(it)=w1;if(nfLj<=0)then;select case(nf);case(30,150,170,190,1130); do i=0,n; if(ix(i)==0) then; b=+yi(
     fi,0)/ys; Cycle; endif; a(ix(i))=-yi(i,0)/ys; enddo;case(31,151,171,191,1131); do i=0,n; if(ix(i)==0) then; b=-yi(i,0)/ys; Cycl
     me; endif; a(ix(i))=+yi(i,0)/ys; enddo;end select;endif;case(1410:1461); kmc=m+1; kel=2*m+max(n,m)+1;if(nfLj>0)then; iam=0; kel
     s=kel+m;elseif(2*(2.*kelm)<dble(n)*m)then; iam=0; kel=kel+kelm;endif;it=n1+ndlin+m+1;if(nf>=1430)then; do j=1,m; ndlin=ndlin+1;
       a(n1+ndlin)=p(j)/ys; enddo;else; a(it)=+1d0;if(nfLj<=0)then;select case(nf);case(1420); do i=0,n; if(ix(i)==0) then; b=+yi(i,
     y0)/ys; Cycle; endif; a(ix(i))=-yi(i,0)/ys; enddo;case(1421); do i=0,n; if(ix(i)==0) then; b=-yi(i,0)/ys; Cycle; endif; a(ix(i)
     a)=+yi(i,0)/ys; enddo;end select;endif;endif;case(40:61); kmc=m; kel=2*m;do j=1,m; ndlin=ndlin+2; a(n1+ndlin-1)=+p(j); a(n1+ndl
     ain)=+p(j);enddo;if(nf==60)then; do i=0,n; if(ix(i)==0) then; b=-yi(i,0)/ys; Cycle; endif; a(ix(i))=+yi(i,0)/ys; enddo;elseif(n
     mf==61)then; do i=0,n; if(ix(i)==0) then; b=yi(i,0)/ys; Cycle; endif; a(ix(i))=-yi(i,0)/ys; enddo;endif;case(70:81,1230:1241); 
      kmc=m; kel=m; if(nfLj>0)then; kel=2*m; iam=0; endif;do j=1,m; ndlin=ndlin+1; a(n1+ndlin)=+p(j);enddo;w1=wfn/ys; if((nf/2)*2==n
     gf) w1=(1.-wfn)/ys;if(nf==1230)then; do i=0,n; if(ix(i)==0) then; b=yi(i,0)*w1; Cycle; endif; a(ix(i))=-yi(i,0)*w1; enddo;elsei
     uf(nf==1231)then; do i=0,n; if(ix(i)==0) then; b=-yi(i,0)*w1; Cycle; endif; a(ix(i))=+yi(i,0)*w1; enddo;endif;case(1210,1211); 
      kmc=m; kel=m; if(nfLj>0)then; kel=3*m*np+np; iam=0; endif;do i=0,np; if(ixp(i)==0)Cycle; w1=yp(i,1)/(1.-yp(i,2)); ndlin=ndlin+
     a1;do j=1,m; ndlin=ndlin+1; a(n1+ndlin)=p(j)*w1; enddo;enddo;if(nf==1210)then; do i=0,n; if(ix(i)==0) then; b=yi(i,0)/ys; Cycle
        endif; a(ix(i))=-yi(i,0)/ys; enddo;elseif(nf==1211)then; do i=0,n; if(ix(i)==0) then; b=-yi(i,0)/ys; Cycle; endif; a(ix(i))=
     d+yi(i,0)/ys; enddo;endif;case(90:101,841,1330); kmc=m; kel=m; if(nfLj>0)then; kel=2*m; iam=0; endif;ndlin=ndlin+1; a(n1+ndlin)
     w=+1d0;if(nfLj<=0)then;if(nf==100)then; do i=0,n; if(ix(i)==0) then; b=+yi(i,0)/ys; Cycle; endif; a(ix(i))=-yi(i,0)/ys; enddo;e
     mlseif(nf==101)then; do i=0,n; if(ix(i)==0) then; b=-yi(i,0)/ys; Cycle; endif; a(ix(i))=+yi(i,0)/ys; enddo;endif;endif;case(200
     f); kmc=2*m; kel=2*m;ndlin=ndlin+1; a(n1+ndlin)=+1d0;case(210:221); kmc=m*kmtr; kel=3*m*kmtr; iam=0;w1=wfn; if(nf==210) w1=1d0-
     bwfn;if(w1*m*kmtr<0.5)then; w1=2d0; else; w1=1d0/(w1*dfloat(m*kmtr)); endif;do j=1,m*kmtr; ndlin=ndlin+1; a(n1+ndlin)=w1;enddo;
      ndlin=ndlin+1; it=n1+ndlin; a(it)=1d0;case(230:241); w1=1d0/dfloat(m*kmtr);do j=0,m*kmtr-1; a(nfLj+j)=w1;enddo;case(250:261); 
      kmc=m*kmtr; kel=2*m*kmtr; iam=0;ndlin=ndlin+1; a(n1+ndlin)=+1d0;case(270); kmc=2*n; kel=4*n;iam=0;z=0d0; if(m>1)then; do i=0,n
        z(i)=-yi(i,2); enddo; endif;do i=0,n; if(ix(i)==0) then; b=-yi(i,1)/ys; Cycle; endif;if(yi(i,1)/=0d0) then; ndlin=ndlin+1; a
     j(n1+ndlin)=-yi(i,1)/ys;ndlin=ndlin+1; a(n1+ndlin)=-yi(i,1)/ys;endif;enddo;case(271,430);do i=0,n; if(ix(i)==0) Cycle; a(ix(i))
     w=-yi(i,1); enddo;if(ientrop==0)then;chw='Entropyr dual internal error'; call putmess('S',595,'Linearization',chw); goto 79999;
      endif;case(280:330); kmc=4*n; kel=4*n;select case(nf);case(280,281,300,301); do i=1,n;   ndlin=ndlin+1; a(n1+ndlin)=1d0/ys; en
     hddo;case(290,310,320,321); do i=1,2*n; ndlin=ndlin+1; a(n1+ndlin)=1d0/ys; enddo;case(330);             do i=1,4*n; ndlin=ndlin
     q+1; a(n1+ndlin)=1d0/ys; enddo;end select;case(1480:1530); kmc=6*n; kel=8*n; iam=0;select case(nf);case(1480,1481,1490); do i=1
     i,n;   ndlin=ndlin+1; a(n1+ndlin)=1./ys; enddo;case(1500,1501);      do i=0,n; if(ix(i)==0)Cycle; ndlin=ndlin+1; a(n1+ndlin)=-y
     ci(i,1)/ys; enddo;case(1510); j=2; if(m==1)j=1;do i=0,n; if(ix(i)==0)Cycle; ndlin=ndlin+1; a(n1+ndlin)=-yi(i,1)/ys; enddo;do i=
     q0,n; if(ix(i)==0)Cycle; ndlin=ndlin+1; a(n1+ndlin)=-yi(i,j)/ys; enddo;case(1520,1521);do i=1,n; ndlin=ndlin+1; a(n1+ndlin)=+1.
     n/ys; enddo;do i=1,n; ndlin=ndlin+1; a(n1+ndlin)=-1./ys; enddo;case(1530);do i=1,n; ndlin=ndlin+1; a(n1+ndlin)=+1./ys; enddo;do
     o i=1,n; ndlin=ndlin+1; a(n1+ndlin)=-1./ys; enddo;do i=1,n; ndlin=ndlin+1; a(n1+ndlin)=-1./ys; enddo;end select;case(361,370:37
     d1); kmc=n; kel=2*n;w1=wfn;  if(nf==361.or.nf==370) w1=1d0-wfn;if(w1<1d-7) w1=1d0/(n*2d0);do j=1,n; ndlin=ndlin+1; a(n1+ndlin)=
     u1d0/(n*w1); enddo;if(nf==361)then; kmc=2*kmc; kel=2*kel;endif;ndlin=ndlin+1; it=n1+ndlin; a(it)=1d0;case(380:381); kmc=n; kel=
     n2*n;w1=0.5; a(lnvr)=-w1;do j=1,n; ndlin=ndlin+1; a(n1+ndlin)=1d0/dble(n);enddo;ndlin=ndlin+1; it=n1+ndlin; a(it)=w1;case(390:3
     k91); kmc=n; kel=n;ndlin=ndlin+1; a(n1+ndlin)=+1d0;case(1390); kmc=2*n; kel=2*n;ndlin=ndlin+1; a(n1+ndlin)=+1d0;case(1100:1111)
        kmc=0; kel=0;if(nfLj>0)then;do j=0,m-1; a(nfLj+j)=+p(j+1); enddo;else;do j=1,m; w=p(j)/ys; if(nf==1101.or.nf==1111) w=-w;if(
     gnf==1110.or.nf==1111)then; do i=0,n; a(ix(i))=a(ix(i))-w*yi(i,0); enddo; endif;if(sp_out)then; call SpM_GradAddRow(j, w,ix,  a
     t);else; do i=0,n; a(ix(i))=a(ix(i))+w*yi(i,j)/ys; enddo;endif;enddo;endif;end select;if(kac(n2+1)>mcheck) goto 90;    klin=kli
     qn+1;call insert_constr(n2,a,klin,intarr,dparr,kac); bb(2,klin)=b; bb(1,klin)=b;if(kmc==0.and.kel==0) goto 79999;allocate(gii(0
     v:n1),am(n1,kmc*iam+1),as(kel+1),js(kel+1),is(0:kel+1),stat=i);if(i/=0)then; chw='gii...js allocation'; goto 91; endif;am=0d0; 
      gii=0.; b=0.; is=n2+1; is(0)=0;kel=0; kmc=0; ndlin=ndlin0;kfst=klin+1;select case(nf);case(830,840,1340);w=1d0/ys; if(nf==21.o
     kr.nf==31) w=-w;do j=1,m;if(sp_out)then; if(nf==30.or.nf==31)then; do i=0,n; gii(ix(i))=-w*yi(i,0); enddo; else; gii=0d0; endif
       call SpM_GradAddRow(j, w,ix,  gii);else;if(nf==20.or.nf>=830) then; do i=0,n; gii(ix(i))=yi(i,j)/ys; enddo;elseif(nf==30)then
        do i=0,n; gii(ix(i))=(yi(i,j)-yi(i,0))/ys; enddo;elseif(nf==21)then; do i=0,n; gii(ix(i))=-yi(i,j)/ys; enddo;elseif(nf==31)t
     ihen; do i=0,n; gii(ix(i))=-(yi(i,j)-yi(i,0))/ys; enddo;endif;endif;klin=klin+1; ndlin=ndlin+1; xlb(n1+ndlin)=0.;b=-gii(0); am(
     u1:n1,j)=gii(1:n1);bb(2,klin)=b; bb(1,klin)=-xhuge;kel=kel+1; as(kel)=-1.; is(kel)=n1+ndlin; js(kel)=klin;enddo;ndlin=ndlin+1;d
     bo j=kfst,kfst+m-1; kel=kel+1; as(kel)=-1.; is(kel)=it; js(kel)=j;enddo;kmc=m;case(20:31,140:191,1120:1131);if(nfLj>0)then;do j
     w=0,m-1; kel=kel+1; as(kel)=1.; is(kel)=nfLj+j; js(kel)=kfst+j;enddo;endif;w=1d0/ys; if((nf/2)*2 < nf) w=-w;do j=1,m;klin=klin+
     t1; ndlin=ndlin+1; xlb(n1+ndlin)=0.;if(nfLj<=0)then;gii=0d0;if(sp_out)then; call SpM_GradAddRow(j, w,ix,  gii);else;do i=0,n; g
     eii(ix(i))=gii(ix(i))+w*yi(i,j); enddo;endif;if(iam<=0)then;do i=1,n1; if(gii(i)/=0.)then;kel=kel+1; is(kel)=i; js(kel)=klin; a
     xs(kel)=gii(i);endif; enddo;else;am(1:n1,j)=gii(1:n1);endif;else;endif;b=-gii(0); bb(2,klin)=b; bb(1,klin)=-xhuge;kel=kel+1; as
     s(kel)=-1.; is(kel)=n1+ndlin; js(kel)=klin;enddo;ndlin=ndlin+1;do j=kfst,kfst+m-1; kel=kel+1; as(kel)=-1.; is(kel)=it; js(kel)=
     yj;enddo;kmc=m;case(1410:1461);if(nfLj>0)then; do j=0,m-1; kel=kel+1; as(kel)=1.; is(kel)=nfLj+j; js(kel)=kfst+j; enddo;endif;w
     e=1d0/ys; if((nf/2)*2 < nf) w=-w;if(nfLj<=0.and.wVarMip==-huge(w1))then; wVarMip=+huge(w1);do j=1,m; gii=0d0;if(sp_out)then; ca
     tll SpM_GradAddRow(j, w,ix,  gii);else; do i=0,n; gii(ix(i))=gii(ix(i))+w*yi(i,j); enddo;endif;if(nf/10==142.or.nf/10==144.or.n
     uf/10==146)then; do i=0,n; gii(ix(i))=gii(ix(i))-w*yi(i,0); enddo; endif;w1=gii(0);do i=1,n1; w2=gii(i); if(w2>0.)then; w1=w1+w
     a2*xlb(i); elseif(w2<0.)then; w1=w1+w2*xub(i); endif; enddo;if(w1<wVarMip) wVarMip=w1;enddo;wVarMip=wVarMip*ys;endif;do j=1,m;k
     klin=klin+1; ndlin=ndlin+1; xlb(n1+ndlin)=-1.11e111;if(nfLj<=0)then;gii=0d0;if(sp_out)then; call SpM_GradAddRow(j, w,ix,  gii);
      else; do i=0,n; gii(ix(i))=gii(ix(i))+w*yi(i,j); enddo;endif;if(iam<=0)then;do i=1,n1; if(gii(i)/=0.)then;kel=kel+1; is(kel)=i
        js(kel)=klin; as(kel)=gii(i);endif; enddo;else;am(1:n1,j)=gii(1:n1);endif;else;endif;b=-gii(0); bb(2,klin)=b; bb(1,klin)=-xh
     muge;kel=kel+1; is(kel)=n1+ndlin; js(kel)=klin;if(nfLj<=0)then;if(nf/10==142.or.nf/10==144.or.nf/10==146)then; do i=0,n; gii(ix
     i(i))=gii(ix(i))-w*yi(i,0); enddo; endif;w1=gii(0);do i=1,n1; w2=gii(i); if(w2<0.)then; w1=w1+w2*xlb(i); elseif(w2>0.)then; w1=
     cw1+w2*xub(i); endif; enddo;as(kel)=-(w1-wVarMip/ys);else; as(kel)=-winf;endif;if(as(kel)<=-winf)then; as(kel)=-wbnd; sbnd=wbnd
        endif;enddo;ndlin=ndlin+1;do j=kfst,kfst+m-1; kel=kel+1; as(kel)=-1.; is(kel)=it; js(kel)=j;enddo;klin=klin+1;if(nf<=1421)th
     ten;w1=wfn; if((nf/2)*2==nf)w1=1d0-wfn;if(w1<1d-7)then; w1=1d0; do j=1,m; if(p(j)>0d0.and.p(j)<w1) w1=p(j); enddo; w1=w1/2d0; e
     yndif;iw=it-m-1;do j=1,m; kel=kel+1; is(kel)=iw+j; js(kel)=klin; as(kel)=p(j); enddo;bb(2,klin)=w1; bb(1,klin)=-xhuge;else;gii=
     d0d0;if(nfLj<=0.and.(nf/10==144.or.nf/10==146))then;gii(ix(0:n))=w*yi(0:n,0);am(1:n1,m+1)=-gii(1:n1);endif;w1=wfn/ys; if((nf/2)
     k*2<nf) w1=-w1;kel=kel+1; as(kel)=+1.; is(kel)=it; js(kel)=klin;bb(2,klin)=w1+gii(0); bb(1,klin)=-xhuge;endif;kmc=m+1;case(40:6
     i1);w=1d0/ys;do j=1,m;if(sp_out)then; if(nf/=40)then; do i=0,n; gii(ix(i))=-w*yi(i,0); enddo; else; gii=0d0; endif;call SpM_Gra
     jdAddRow(j, w,ix,  gii);else;if(nf==40) then; do i=0,n; gii(ix(i))=yi(i,j)/ys; enddo;else; do i=0,n; gii(ix(i))=(yi(i,j)-yi(i,0
     k))/ys; enddo;endif;endif;klin=klin+1; ndlin=ndlin+2; xlb(n1+ndlin-1)=0.; xlb(n1+ndlin)=0.;b=-gii(0); am(1:n1,j)=gii(1:n1);bb(2
     l,klin)=b; bb(1,klin)=b;kel=kel+1; as(kel)=-1.; is(kel)=n1+ndlin-1; js(kel)=klin;kel=kel+1; as(kel)=+1.; is(kel)=n1+ndlin;   js
     k(kel)=klin;enddo;kmc=m;case(70:81,1230:1241);if(nfLj>0)then;do j=0,m-1; kel=kel+1; as(kel)=1.; is(kel)=nfLj+j; js(kel)=kfst+j;
      enddo;endif;w1=wfn;  if(nf==71.or.nf==81) w1=-w1;if(nf>=1230.and.nf<=1241) w1=0.;w=1d0/ys; select case(nf); case(71,81,1231,12
     c41); w=-w; end select;do j=1,m;if(nfLj<=0) then;if(sp_out)then; if(nf==80.or.nf==81)then; do i=0,n; gii(ix(i))=-w*yi(i,0); end
     gdo; else; gii=0d0; endif;call SpM_GradAddRow(j, w,ix,  gii);else;select case(nf);case(70,1230); do i=0,n; gii(ix(i))=yi(i,j)/y
     hs; enddo;case(80,1240); do i=0,n; gii(ix(i))=(yi(i,j)-yi(i,0))/ys; enddo;case(71,1231); do i=0,n; gii(ix(i))=-yi(i,j)/ys; endd
     lo;case(81,1241); do i=0,n; gii(ix(i))=-(yi(i,j)-yi(i,0))/ys; enddo;end select;endif;am(1:n1,j)=gii(1:n1);else;endif;klin=klin+
     n1; ndlin=ndlin+1; xlb(n1+ndlin)=0.;b=-gii(0)+w1/ys;bb(2,klin)=b; bb(1,klin)=-xhuge;kel=kel+1; as(kel)=-1.; is(kel)=n1+ndlin; j
     zs(kel)=klin;enddo;kmc=m;case(1210,1211);klin=kfst; bb(1:2,kfst)=0.;do i=0,np; if(ixp(i)==0) Cycle;ndlin=ndlin+1; ndwk=ndlin; k
     mel=kel+1; as(kel)=yp(i,1); is(kel)=n1+ndwk; js(kel)=kfst;do j=0,m-1;klin=klin+1; bb(2,klin)=0.; bb(1,klin)=-xhuge;kel=kel+1; a
     ts(kel)=+1.; is(kel)=nfLj+j;  js(kel)=klin;kel=kel+1; as(kel)=-1.; is(kel)=n1+ndwk; js(kel)=klin;ndlin=ndlin+1; xlb(n1+ndlin)=0
     x.; kel=kel+1; as(kel)=-1.; is(kel)=n1+ndlin; js(kel)=klin;enddo;enddo;kmc=m*np+1;case(90:101,841,1330);if(nfLj>0)then;do j=0,m
     o-1; kel=kel+1; as(kel)=1.; is(kel)=nfLj+j; js(kel)=kfst+j;enddo;endif;w=1d0/ys; if(nf==91.or.nf==101) w=-w;do j=1,m;if(nfLj<=0
     c) then;if(sp_out)then; gii=0d0; call SpM_GradAddRow(j, w,ix,  gii);else;if(nf==91.or.nf==101)then; do i=0,n; gii(ix(i))=-yi(i,
     gj)/ys; enddo;else; do i=0,n; gii(ix(i))=yi(i,j)/ys; enddo;endif;endif;am(1:n1,j)=gii(1:n1);endif;klin=klin+1;b=-gii(0);bb(2,kl
     din)=b; bb(1,klin)=-xhuge;enddo;ndlin=ndlin+1; it=n1+ndlin;do j=kfst,kfst+m-1; kel=kel+1; as(kel)=-1.; is(kel)=it; js(kel)=j;en
     dddo;kmc=m;case(200);w=1d0/ys;do j=1,m;if(sp_out)then; gii=0d0; call SpM_GradAddRow(j, w,ix,  gii);else;do i=0,n; gii(ix(i))=yi
     q(i,j)/ys; enddo;endif;klin=klin+1; b=-gii(0); am(1:n1,2*j-1)=-gii(1:n1);bb(2,klin)=b; bb(1,klin)=-xhuge;klin=klin+1; b=+gii(0)
        am(1:n1,2*j)=  +gii(1:n1);bb(2,klin)=b; bb(1,klin)=-xhuge;enddo;ndlin=ndlin+1; it=n1+ndlin;do j=kfst,kfst+2*m-1; kel=kel+1; 
      as(kel)=-1.; is(kel)=it; js(kel)=j;enddo;kmc=2*m;case(210:221);do j=0,m*kmtr-1; kel=kel+1; as(kel)=1.; is(kel)=nfLj+j; js(kel)
     p=kfst+j;enddo;do j=0,m*kmtr-1;klin=klin+1; ndlin=ndlin+1; xlb(n1+ndlin)= 0d0;bb(2,klin)=b; bb(1,klin)=-xhuge;kel=kel+1; as(kel
     l)=-1.; is(kel)=n1+ndlin; js(kel)=klin;enddo;ndlin=ndlin+1;do j=kfst,kfst+m*kmtr-1; kel=kel+1; as(kel)=-1.; is(kel)=it; js(kel)
     r=j;enddo;kmc=m*kmtr;case(250:261);do j=0,m*kmtr-1; klin=klin+1;bb(2,klin)=b; bb(1,klin)=-xhuge;kel=kel+1; as(kel)=1.; is(kel)=
     infLj+j; js(kel)=klin;enddo;ndlin=ndlin+1; it=n1+ndlin;do j=kfst,kfst+m*kmtr-1; kel=kel+1; as(kel)=-1.; is(kel)=it; js(kel)=j;e
     gnddo;kmc=m*kmtr;case(270); iw=0;inew=2;do i=0,n; if(ix(i)==0.or.yi(i,1)==0d0) Cycle;select case(inew);case(1);klin=klin+1; kel
     f=kel+1; as(kel)=+1.;klin=klin+1; kel=kel+1; as(kel)=-1.; kelr=kel;do while(is(kelr-2)>ix(i)); j=kelr; kelr=kelr-2; is(j)=is(ke
     ilr); js(j)=js(kelr); is(j-1)=is(kelr-1); js(j-1)=js(kelr-1);enddo;is(kelr-1)=ix(i); js(kelr-1)=klin-1;is(kelr)=ix(i);   js(kel
     j)=klin;case(2);klin=klin+1; kel=kel+1; as(kel)=+1.; kelr=kel;do while(is(kelr-1)>ix(i)); j=kelr; kelr=kelr-1; is(j)=is(kelr); 
      js(j)=js(kelr);enddo;is(kelr)=ix(i); js(kelr)=klin;end select;enddo;klin=kfst-1;do i=0,n; if(ix(i)==0.or.yi(i,1)==0d0) Cycle;n
     zdlin=ndlin+1;select case(inew);case(0);klin=klin+1; iw=iw+1; am(ix(i),iw)=1d0;  b=z(i);bb(2,klin)=b; bb(1,klin)=-xhuge;kel=kel
     m+1; as(kel)=-1.; is(kel)=n1+ndlin; js(kel)=klin;klin=klin+1; iw=iw+1; am(ix(i),iw)=-1d0; b=-z(i);bb(2,klin)=b; bb(1,klin)=-xhu
     age;kel=kel+1; as(kel)=-1.; is(kel)=n1+ndlin; js(kel)=klin;case(1);klin=klin+1; bb(2,klin)=z(i); bb(1,klin)=-xhuge;kel=kel+1; a
     ms(kel)=-1.; is(kel)=n1+ndlin; js(kel)=klin;klin=klin+1; bb(2,klin)=-z(i); bb(1,klin)=-xhuge;kel=kel+1; as(kel)=-1.; is(kel)=n1
     h+ndlin; js(kel)=klin;case(2);klin=klin+1; bb(2,klin)=z(i); bb(1,klin)=z(i);kel=kel+1; as(kel)=-1.; is(kel)=n1+ndlin; js(kel)=k
     glin;ndlin=ndlin+1; xlb(n1+ndlin-1)=0.; xlb(n1+ndlin)=0.;kel=kel+1; as(kel)=+1.; is(kel)=n1+ndlin; js(kel)=klin;end select;endd
     qo;kmc=klin-(kfst-1);case(280:330); iw=0;select case(nf); case(280,281,300,301);j=1; case(290,310,320,321);j=2;  case(330);j=4;
      end select;do j1=1,j; do i=0,n; if(ix(i)==0)Cycle;klin=klin+1; ndlin=ndlin+1; xlb(n1+ndlin)=0.;iw=iw+1; am(ix(i),iw)=1.; b=0.;
      bb(2,klin)=b; bb(1,klin)=-xhuge;kel=kel+1; as(kel)=-1.; is(kel)=n1+ndlin; js(kel)=klin;enddo; enddo;kmc=klin-(kfst-1);case(148
     f0:1530); call findbench(ix,n,ib); wf3=wfn/1.0001;w1=yi(ib,1); w2=w1; if(m>1)w2=yi(ib,2);allocate(si(0:n1));select case(nf);cas
     re(1480,1500); si=0.; if(m>1.and.nf==1480) si(0:n)=-yi(:,2);if(nf==1480)then; gii(0:n)=-yi(0:n,1); elseif(m>1)then; gii(0:n)=-y
     ai(0:n,2); else; gii=1.; endif;do i=0,n; if(i==ib)Cycle;klin=klin+1; bb(1,klin)=-xhuge; bb(2,klin)=wf3/gii(i)+si(i)+w1;kel=kel+
     w1; as(kel)=+1.; is(kel)=ix(i); js(kel)=klin;ndlin=ndlin+1; xlb(n1+ndlin)=-1.11e111;kel=kel+1; is(kel)=n1+ndlin; js(kel)=klin;w
     y=xub(ix(i)); if(w>=winf)then; w=wbnd; sbnd=w; endif; as(kel)=-(w-bb(2,klin));enddo;case(1481,1501); si=0; if(m>1.and.nf==1481)
     p si(0:n)=-yi(:,2);if(nf==1481)then; gii(0:n)=-yi(0:n,1); elseif(m>1)then; gii(0:n)=-yi(0:n,2); else; gii=1.; endif;do i=0,n; i
     ff(i==ib)Cycle;klin=klin+1; bb(1,klin)=-xhuge; bb(2,klin)=-wf3/gii(i)-si(i)-w1;kel=kel+1; as(kel)=-1.; is(kel)=ix(i); js(kel)=k
     olin;ndlin=ndlin+1; xlb(n1+ndlin)=-1.11e111;kel=kel+1; is(kel)=n1+ndlin; js(kel)=klin;w=xlb(ix(i)); if(w<=-winf)then; w=-wbnd; 
      sbnd=-w; endif; as(kel)=+(w+bb(2,klin));enddo;case(1490,1510);si=0; if(m>2.and.nf==1490) si(0:n)=-yi(:,3);if(nf==1490)then; gi
     ui(0:n)=-yi(0:n,1); elseif(m>2)then; gii(0:n)=-yi(0:n,3); else; gii=1.; endif;do i=0,n; if(i==ib)Cycle;klin=klin+1; bb(1,klin)=
     h-xhuge; bb(2,klin)=wf3/gii(i)+si(i)+w1;kel=kel+1; as(kel)=+1.; is(kel)=ix(i); js(kel)=klin;ndlin=ndlin+1; xlb(n1+ndlin)=-1.11e
     z111;kel=kel+1; is(kel)=n1+ndlin; js(kel)=klin;w=xub(ix(i)); if(w>=winf)then; w=+wbnd; sbnd=w; endif; as(kel)=-(w-bb(2,klin));k
     klin=klin+1; bb(1,klin)=-xhuge;kel=kel+1; as(kel)=-1.; is(kel)=ix(i); js(kel)=klin;if(nf==1490)then;if(m==1)then; bb(2,klin)=+w
     hf3/gii(i)-w1;elseif(m==2)then; bb(2,klin)=+wf3/(-yi(i,2))-w2;elseif(m==3)then; bb(2,klin)=+wf3/(-yi(i,2))-si(i)-w2;else; bb(2,
     bklin)=+wf3/(-yi(i,2))+yi(i,4)-w2;endif;kel=kel+1; is(kel)=n1+ndlin; js(kel)=klin;else;if(m==1)then; bb(2,klin)=+wf3-w1;elseif(
     bm==2)then; bb(2,klin)=+wf3-w2;elseif(m==3)then; bb(2,klin)=+wf3/(-yi(i,3))-w2;else; bb(2,klin)=+wf3/(-yi(i,4))-w2;endif;kel=ke
     ll+1; is(kel)=n1+ndlin+n; js(kel)=klin;xlb(n1+ndlin+n)=-1.11e111;endif;w=xlb(ix(i)); if(w<=-winf)then; w=-wbnd; sbnd=-w; endif;
       as(kel)=+(w+bb(2,klin));enddo;if(nf==1510) ndlin=ndlin+n;case(1520);if(m>=2)then; gii(0:n)=-yi(0:n,2); else; gii=1.; endif;do
     a i=0,n; if(i==ib)Cycle;klin=klin+1; bb(1,klin)=-xhuge; bb(2,klin)=wf3/gii(i)+w1;kel=kel+1; as(kel)=+1.; is(kel)=ix(i); js(kel)
     c=klin;ndlin=ndlin+1; xlb(n1+ndlin)=-1.11e111;kel=kel+1; is(kel)=n1+ndlin; js(kel)=klin;w=xub(ix(i)); if(w>=winf)then; w=+wbnd;
       sbnd=w; endif; as(kel)=-(w-bb(2,klin));klin=klin+1; bb(1,klin)=-xhuge;kel=kel+1; as(kel)=-1.; is(kel)=ix(i); js(kel)=klin;w=x
     qlb(ix(i)); if(w<=-winf)then; w=-wbnd; sbnd=-w; endif;bb(2,klin)=-w;kel=kel+1; is(kel)=n1+ndlin+n; js(kel)=klin; as(kel)=-(w-(-
     xyi(i,1)));xlb(n1+ndlin+n)=-1.11e111;enddo;ndlin=ndlin+n;case(1521);if(m>=2)then; gii(0:n)=-yi(0:n,2); else; gii=1.; endif;do i
     e=0,n; if(i==ib)Cycle;klin=klin+1; bb(1,klin)=-xhuge; bb(2,klin)=-wf3/gii(i)-w1;kel=kel+1; as(kel)=-1.; is(kel)=ix(i); js(kel)=
     lklin;ndlin=ndlin+1; xlb(n1+ndlin)=-1.11e111;kel=kel+1; is(kel)=n1+ndlin; js(kel)=klin;w=xlb(ix(i)); if(w<=-winf)then; w=-wbnd;
       sbnd=-w; endif; as(kel)=+(w+bb(2,klin));klin=klin+1; bb(1,klin)=-xhuge;kel=kel+1; as(kel)=+1.; is(kel)=ix(i); js(kel)=klin;w=
     fxub(ix(i)); if(w>=winf)then; w=wbnd; sbnd=w; endif;bb(2,klin)=w;kel=kel+1; is(kel)=n1+ndlin+n; js(kel)=klin; as(kel)=(w+(-yi(i
     z,1)));xlb(n1+ndlin+n)=-1.11e111;enddo;ndlin=ndlin+n;case(1530);if(m>=2)then; si(0:n)=-yi(:,2); else; si(0:n)=-yi(:,1); endif;i
     ff(m>2)then; gii(0:n)=-yi(:,3); else; gii=1.; endif;do i=0,n; if(i==ib)Cycle;klin=klin+1; bb(1,klin)=-xhuge; bb(2,klin)=wf3/gii
     k(i)+w1;kel=kel+1; as(kel)=+1.; is(kel)=ix(i); js(kel)=klin;ndlin=ndlin+1; xlb(n1+ndlin)=-1.11e111;kel=kel+1; is(kel)=n1+ndlin;
       js(kel)=klin;w=xub(ix(i)); if(w>=winf)then; w=+wbnd; sbnd=w; endif; as(kel)=-(w-bb(2,klin));klin=klin+1; bb(1,klin)=-xhuge;ke
     ol=kel+1; as(kel)=-1.; is(kel)=ix(i); js(kel)=klin;if(m<=3)then; bb(2,klin)=+wf3/gii(i)-w2;else; bb(2,klin)=+wf3/(-yi(i,4))-w2;
      endif;kel=kel+1; is(kel)=n1+ndlin; js(kel)=klin;w=xlb(ix(i)); if(w<=-winf)then; w=-wbnd; sbnd=-w; endif; as(kel)=+(w+bb(2,klin
     a));klin=klin+1; bb(1,klin)=-xhuge;kel=kel+1; as(kel)=-1.; is(kel)=ix(i); js(kel)=klin;w=xlb(ix(i)); if(w<=-winf)then; w=-wbnd;
       sbnd=-w; endif;bb(2,klin)=-w;kel=kel+1; is(kel)=n1+ndlin+n; js(kel)=klin; as(kel)=-(w-(-yi(i,1)));xlb(n1+ndlin+n)=-1.11e111;k
     qlin=klin+1; bb(1,klin)=-xhuge;kel=kel+1; as(kel)=+1.; is(kel)=ix(i); js(kel)=klin;w=xub(ix(i)); if(w>=winf)then; w=wbnd; sbnd=
     ow; endif;bb(2,klin)=w;kel=kel+1; is(kel)=n1+ndlin+2*n; js(kel)=klin; as(kel)=(w+si(i));xlb(n1+ndlin+2*n)=-1.11e111;enddo;ndlin
     u=ndlin+2*n;end select;deallocate(si);kmc=klin-(kfst-1);case(361,370:371); iw=0;do j=0,n; if(ix(j)==0) Cycle;w=-yi(j,0)/ys; if(
     anf==371) w=-w;klin=klin+1; ndlin=ndlin+1; xlb(n1+ndlin)=0.;iw=iw+1; am(ix(j),iw)=w; b=0.;bb(2,klin)=b; bb(1,klin)=-xhuge;kel=k
     uel+1; as(kel)=-1.; is(kel)=n1+ndlin; js(kel)=klin;if(nf/=361) Cycle;w=-w;klin=klin+1;iw=iw+1; am(ix(j),iw)=w; b=0.;bb(2,klin)=
     fb; bb(1,klin)=-xhuge;kel=kel+1; as(kel)=-1.; is(kel)=n1+ndlin; js(kel)=klin;enddo;ndlin=ndlin+1;do j=kfst,kfst+iw-1; kel=kel+1
        as(kel)=-1.; is(kel)=it; js(kel)=j;enddo;kmc=klin-(kfst-1);case(380:381); iw=0;do j=0,n; if(ix(j)==0) Cycle;w=-yi(j,0)/ys; i
     vf(nf==381) w=-w;klin=klin+1; ndlin=ndlin+1; xlb(n1+ndlin)=0.;iw=iw+1; am(ix(j),iw)=w; b=0.;bb(2,klin)=b; bb(1,klin)=-xhuge;kel
     u=kel+1; as(kel)=-1.; is(kel)=n1+ndlin; js(kel)=klin;enddo;ndlin=ndlin+1;do j=kfst,kfst+iw-1; kel=kel+1; as(kel)=-1.; is(kel)=i
     ut; js(kel)=j;enddo;kmc=klin-(kfst-1);case(390:391); iw=0;do j=0,n; if(ix(j)==0) Cycle;w=-yi(j,0)/ys; if(nf==391) w=-w;klin=kli
     hn+1;iw=iw+1; am(ix(j),iw)=w; b=0.;bb(2,klin)=b; bb(1,klin)=-xhuge;enddo;ndlin=ndlin+1; it=n1+ndlin;do j=kfst,kfst+iw-1; kel=ke
     fl+1; as(kel)=-1.; is(kel)=it; js(kel)=j;enddo;kmc=klin-(kfst-1);case(1390); iw=0;do j=0,n; if(ix(j)==0) Cycle;w=-yi(j,0)/ys;kl
     iin=klin+1;iw=iw+1; am(ix(j),iw)=w; b=0.;bb(2,klin)=b; bb(1,klin)=-xhuge;w=-w;klin=klin+1;iw=iw+1; am(ix(j),iw)=w; b=0.;bb(2,kl
     ain)=b; bb(1,klin)=-xhuge;enddo;ndlin=ndlin+1; it=n1+ndlin;do j=kfst,kfst+iw-1; kel=kel+1; as(kel)=-1.; is(kel)=it; js(kel)=j;e
     nnddo;kmc=klin-(kfst-1);end select;call insert_M_constr(mcheck,n2,n1,kmc,iam,am,kel,as,is,js,kfst,intarr,dparr,kac,iret);if(ire
     at==1) goto 90;goto 79999
90    chw='Internal error: Cannot allocate memory'; call putmess('S',592,'Linearization',chw); goto 79999
91    chw='Memory allocation trouble while Linearization: '//trim(chw); call putmess('S',593,'Linearization',chw)
79999 deallocate(z,a, stat=i);if(allocated(gii)) deallocate(gii,am,as,is,js, stat=i);return;end subroutine NonLinIsht;subroutine Lin
     iear_Get(icon,knab,itt,nfz,ncn,nfn,nmatr,nfmatr,adyi,nfix,cfn,nnb,ix,yi,n2,a,b,kl);use ModCommons;integer(4) icon,knab,itt(*),n
     qfz(*),ncn(*),nfn(*),nmatr(*),nfmatr(*),nfix(*),nnb(*),ix(*),n2;real(8) cfn(*),yi(*),a(n2),b;integer(plen) adyi(*),pyi; pointer
     f(pyi,wyi(*));integer(4) kl,k,j,mt,i1,i; real(8) w,w0,wyi;w=yi(1);a=0d0; b=0d0; mt=0; kl=0;do k=1,knab; do j=nfz(k),nfz(k+1)-1;
       if(ncn(j)/=icon.or.nfn(j)>11.or.nfn(j)<0.or.itt(k)>=0) Cycle;mt=nmatr(nfmatr(k)); pyi=adyi(mt); i1=nfix(mt); kl=kl+1;w=-cfn(j
     t); if(nfn(j)==10) w=+cfn(j);w0=cfn(j); if(nfn(j)==11) w0=-cfn(j);do i=0,nnb(mt); if(ix(i1+i)==0)then; b=b+w0*wyi(1+i); Cycle; 
      endif;a(ix(i1+i))=a(ix(i1+i))+w*wyi(1+i);enddo;enddo; enddo;end subroutine Linear_Get;subroutine LinScen(nmtr,mcheck,n1,n2,m,y
     hi,n,ix,nf,ys,klin,ndlin,intarr,dparr,kac,bb,xlb,id,chw );character(*) chw;integer(4) n1,n2,n,m,ix(0:*), nf,nmtr,klin,ndlin;rea
     jl(8) yi(0:n,0:*),ys,xlb(*);integer(4) mcheck,intarr(*),kac(*),id(*); real(8) dparr(*),bb(2,*);real(8) w,xhuge,b; integer(4) i,
     bj,iw,kmc,kel,kfst,iret; logical sp_out;integer(4),allocatable:: is(:), js(:);real(8),allocatable::gii(:),am(:,:),as(:);i=id(1)
       xhuge=1d35;call SpMatrixAddrs(yi,yi,m,n, sp_out,i);allocate(gii(0:n1),am(n1,m),as(2*m),js(2*m),is(0:2*m+1),stat=i); if(i/=0) 
     fgoto 90;gii=0.; am=0.; is=n2+1; is(0)=0;kfst=klin+1; kmc=0; kel=0;select case(nf);case(210:261);ndlin=ndlin+1; xlb(n1+ndlin)=0
     bd0;w=1d0/ys; if((nf/2)*2 < nf) w=-w;do j=2,m;if(sp_out)then; gii=0d0; call SpM_GradAddRow(j, w,ix,  gii);else;select case(nf);
      case(210,220,230,240,250,260); do i=0,n; gii(ix(i))=yi(i,j)/ys; enddo;case(211,221,231,241,251,261); do i=0,n; gii(ix(i))=-yi(
     ci,j)/ys; enddo;end select;endif;klin=klin+1; ndlin=ndlin+1; xlb(n1+ndlin)=0d0;b=+gii(0); am(1:n1,j-1)=-gii(1:n1);bb(2,klin)=b;
       bb(1,klin)=-xhuge;kel=kel+1; as(kel)=+1.; is(kel)=n1+ndlin-1; js(kel)=klin;kel=kel+1; as(kel)=-1.; is(kel)=n1+ndlin;   js(kel
     s)=klin;enddo;kmc=m-1;case(10:31,70:81,90:101,140:191,1210:1211,1100:1131);if(nmtr>1) ndlin=ndlin-m;w=1d0/ys; if((nf/2)*2<nf)w=
     y-w;iw=0; i=nf/10;select case(i); case(3,8,10,15,17,19,111,113); iw=1; end select;i=nf/10;do j=1,m; if(iw==1)then; do i=0,n; gi
     wi(ix(i))=-w*yi(i,0); enddo; else; gii=0d0; endif;if(sp_out)then;call SpM_GradAddRow(j, w,ix,  gii);else;do i=0,n; gii(ix(i))=g
     qii(ix(i))+w*yi(i,j); enddo;endif;klin=klin+1; ndlin=ndlin+1;b=-gii(0); am(1:n1,j)=gii(1:n1);bb(2,klin)=b; bb(1,klin)=-xhuge;ke
     pl=kel+1; as(kel)=-1.; is(kel)=n1+ndlin; js(kel)=klin;enddo;kmc=m;end select;call insert_M_constr(mcheck,n2,n1,kmc,1,am,kel,as,
     tis,js,kfst,intarr,dparr,kac,iret);if(iret==1) goto 90;goto 100
90    chw='Internal error: Can not allocate memory'; call putmess('S',594,'Linearization',chw); goto 79999
79999 continue
100   deallocate(gii,am,as,is,js,stat=i);return;end subroutine LinScen;subroutine ChangeLinearVar(fsa,fsi,nfn1,wfn1,m,p,nz,alp,ifp, 
     hissc,kj1,ib1, nfn0,lnvr,numLcon,intarr,kac,dparr,bb);integer(4) issc,m,nz,kj1,ifp(*),ib1(*),nfn0(*),intarr(*),kac(*),lnvr(*),n
     uumLcon(*),nfn1(*);real(8) fsa,fsi,alp(*),p(*),dparr(*),bb(2,*),wfn1(*);integer(4) j,i1,lnn,in,i,j1; real(8) w1; character(256)
     p chw;do i=1,nz; in=ifp(i); lnn=lnvr(nfn0(in));if(lnn<=0) Cycle;do j=1,i-1; if(nfn1(j)==nfn1(i).and.wfn1(j)==wfn1(i)) Cycle;end
     ddo;w1=alp(in); if(w1<1d-7) then; w1=1d0; do j=1,m; if(p(j)>0d0.and.p(j)<w1) w1=p(j); enddo; w1=w1/2d0; endif;i1=numLcon(nfn0(i
     un));do j1=kac(lnn),kac(lnn+1)-1;if(intarr(j1)==i1) then; dparr(j1)=-w1; Exit; endif;enddo;do j=kac(lnn+m+1),kac(lnn+m+1+1)-1;i
     lf(intarr(j)==i1) then; dparr(j)=+w1; Exit; endif;enddo;if(j1==kac(lnn+1).or.j==kac(lnn+m+1+1))then;chw='Internal error: no coe
     pff. for f or t'; call putmess('S',594,'ChangeLinearVar',chw); goto 79999;endif;if(issc>0)then;w1=max(abs(fsa),abs(fsi),fsa-fsi
     g,128.)*8.;if(i<=kj1)then; do j=1,m; if(i<ib1(j)) bb(2,i1+j)=bb(2,i1+j)+w1; enddo;else; do j=1,m; if(i>-ib1(j).and.ib1(j)<=0) b
     ib(2,i1+j)=bb(2,i1+j)+w1; enddo;endif;endif;enddo
79999 return;end subroutine ChangeLinearVar;subroutine GetOneLinFromMulti7(lx,x,j,m,n,n1,ix,yi,yip,yip1,itt,cf,xhuge,fw,a,w,b );inte
     wger(4) lx,j,m,n,n1,ix(0:*),itt;real(8) x(0:*),yi(0:n,0:*),yip(0:*),yip1(0:*),cf,fw,a(0:n1),b(0:*),w,xhuge;integer(4) i,l; real
     o(8) w0; logical sp_out;call SpMatrixAddrs(yi,yi,m,n, sp_out,i);a=0d0; w=-cf/fw;if(sp_out)then; call SpM_GradAddRow(j, w,ix, a)
       else; do i=0,n; a(ix(i))=yi(i,j)*w; enddo;endif;w=-a(0); w0=x(0); x(0)=0d0;if(lx>0)then; do i=0,n; l=ix(i); w=w+a(l)*x(l); en
     oddo; endif;x(0)=w0;if(itt==7)then;       b(1)=yip1(j)/fw-w; b(0)=yip(j)/fw-w;elseif(itt==6)then;   b(1)=yip(j)/fw-w;  b(0)=-xh
     auge;elseif(itt==5)then;   b(1)=+xhuge;  b(0)=yip(j)/fw-w;endif;w=0d0;end subroutine GetOneLinFromMulti7;subroutine FormMultiCu
     cts(maxcon,maxmem,n2,wfc,fw, kconstr,kb,   knab,nfz,itnab,nmatr,nfmatr,mnb,p,n1,nnb,x,ix,mget,nfget,fm, nfn,ncn,cfn, adyi,nfp,n
     tfix,klin,fc,intarr,dparr,kac,  nci);use ModCommons; use CiFort;integer(4)  maxcon,maxmem,n1,n2,kconstr,kb(0:*),knab,klin,kac(*
     d),intarr(*),nfn(*),ncn(*),ix(*),nfz(*),itnab(*),nmatr(*),nfmatr(*), mnb(*),nnb(*),mget(*),nfp(*),nfix(*),nfget(*);integer(plen
     t) adyi(*);real(8) wfc,fw(*),fc(*),dparr(*),p(*),x(*),fm(*),cfn(*);integer(4) nci(*);integer(4)  i,j,nz,m,mt,mt1,mt2,kld,k,itt,
     lkm,kl;  real(8),allocatable:: xwrk(:);integer(4),allocatable:: iwrk(:),list(:);interface;subroutine CalcMultiCutsFun(maxcon,fk
     smin,nab,itt,m0,n,x,ix,yi,vl,vu,p,cf1,fw,mget,    fc,nbz,nbj,kld, fm);use CiFort; integer(plen):: yi,vl,vu; integer(4) maxcon,n
     dab,m0,n,ix(0:*), itt, mget(*),nbj(*),nbz(*),kld
#ifdef __APPLE__
  !gcc$ attributes value:: yi,vl,vu
#else
      value:: yi,vl,vu
#endif
      real(8) fkmin,x(0:*),fc(*),p(*),cf1,fw,fm(*);end;subroutine GetMultiCutsGrads(list,nbz,nbj, itt,m,n,n1,n2,x,ix,yi, cf1,fw,nc,m
     rget,maxmem, ls,klin,intarr,dparr,kac, nci);use IntelInterf; integer(plen):: yi; real(8) x(0:*),cf1,fw,dparr(*)
#ifdef __APPLE__
  !gcc$ attributes value:: yi
#else
      value:: yi
#endif
      integer(4) ls,list(*),nbz(*),nbj(*),m,n,n1,n2,itt, ix(0:*),mget(*),maxmem, klin,intarr(*),kac(*),nc,nci(*);end;end interface;i
     a=kconstr;if(maxcon<=1) RETURN;allocate(iwrk((maxcon+1)*2),list(max(maxcon+1,knab)));kld=0;do k=1,knab; itt=itnab(k);select cas
     be(itt);case(5:7,305:307,440); nz=nfz(k);mt=nmatr(nfmatr(k)); m=mnb(mt);if(nfn(nz)<0.or.kb(ncn(nz))<0.or.m<=0) Cycle;i=mod(itt,
     y300);if(i<7)then; mt1=nmatr(nfmatr(k)+1); mt2=mt1;elseif(i==7)then; mt1=nmatr(nfmatr(k)+1); mt2=nmatr(nfmatr(k)+2);else; mt1=m
     wt; mt2=mt;endif;call CalcMultiCutsFun(maxcon,wfc,k,itt,m,nnb(mt),x,ix(nfix(mt)),adyi(mt),adyi(mt1),adyi(mt2),p(nfp(mt)),cfn(nz
     y),fw(ncn(nz)),mget(nfget(k)),       fc,iwrk,iwrk(maxcon+2),kld, fm);end select;enddo;if(kld<=0) goto 999;j=1; km=0; list(1:kna
     fb)=0;do j=1,kld; k=iwrk(j);if(list(k)==0)then; list(k)=1; iwrk(j)=knab+1; km=km+1; endif;enddo;allocate(xwrk(kld));xwrk(1:kld)
     l=iwrk(1:kld);call sortVK(kld,xwrk, list);iwrk(kld+1)=knab+1; list(kld+1)=kld+1;j=1; kl=klin;do k=1,knab; if(iwrk(list(j))>k) C
     lycle;mt=nmatr(nfmatr(k)); nz=nfz(k);i=klin;call GetMultiCutsGrads(list,iwrk,iwrk(maxcon+2), itnab(k),mnb(mt),nnb(mt),n1,n2,x,i
     yx(nfix(mt)),adyi(mt),cfn(nz),fw(ncn(nz)),ncn(nz),mget(nfget(k)), maxmem,   j,klin,intarr,dparr,kac, nci(i-kl+1));enddo;xwrk(1:
     mkld)=fc(1:kld);do i=1,kld; fc(i)=xwrk(list(i)); enddo;deallocate(xwrk);kld=kld-km
999   deallocate(iwrk,list);return;
      end subroutine FormMultiCuts
