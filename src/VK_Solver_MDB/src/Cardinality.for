      subroutine Card_Check(mname,m,yi,n,ix,nz,nf,wfm,
     +
     +chw     );integer(4) m,n,nz, ix(0:*),nf(*);real(8) wfm(*),yi(0:n,0:*);character(*) mname,chw;real(8)  wf,xhuge,w; integer(4)  
     li,nfi,ii,ib,ierr;integer(4)  j,ichk0;character  ch1*1;real(8),pointer :: v1(:),v2(:),v3(:),v4(:),sv(:);nullify(v1,v2,v3,v4,sv)
      xhuge=huge(wf)/2d0;if(m<1)then; chw='Matrix '//trim(mname)//' does not contain row with numerical data';call putmess('S',570,'
     lCardinality_Group_Check',chw);RETURN;endif;call findBench(ix,n, ib);if(sum(abs(yi(ib,1:m)))>0.)then;chw='Benchmark is present 
     lin the matrix '//trim(mname); call putmess('W',0,'Cardinality_Group_Checking',chw);endif;do ii=1,nz; nfi=iabs(nf(ii));i=nfi/10
     l; j=nfi-i*10;if(wfm(ii)<0d0.and.j==0 .or. wfm(ii)>0d0.and.j==1)then;chw='Problem Statement: threshold value <= 0 in Cardinalit
     ly group function. It should be > 0';call putmess('S',571,'Cardinality_Group_Checking',chw); goto 79999;endif;w=abs(wfm(ii));ie
     lrr=0;select case(nfi);case(280,281,300,301,320,321); if(m>2) ierr=1;case(290,310,330); if(m>4) ierr=2;end select;if(ierr>0)the
     ln;chw='Matrix '//trim(mname)//' in a function in Cardinality group has too many rows. '//
     +'Number of numerical rows should be in range [1, 4] depending on a function';call putmess('S',5701,'Cardinality_Group_Check',c
     lhw);endif;allocate(v1(0:n),v2(0:n),v3(0:n),v4(0:n),sv(4),stat=j);if(j/=0)then; chw='Can not allocate arrays in Cardinality Che
     lck function';call putmess('S',5711,'Cardinality_Group_Checking',chw); goto 79999;endif;sv=0.; v1(0:n)=0.; v2(0:n)=0.; v3(0:n)=
     l0.;do j=1,m; write(ch1,'(i1)')j;sv(j)=yi(ib,j);ichk0=0;select case(nfi);case(280,281); if(j==1)ichk0=1;case(290); if(j==1)then
     l; ichk0=1; sv(2)=sv(1); endif; if(j==2)ichk0=1;if(j==3)then; v1=-yi(:,j); v2=v1; endif; if(j==4)v2=-yi(:,j);case(300,301); if(
     lj==2)ichk0=1;case(310); if(j==3.or.j==4)ichk0=1;case(320,321); if(j==2)then; ichk0=1; v3=-yi(:,j); endif;  if(j==1)then; v1=-y
     li(:,j); v3=1.; endif;case(330); if(j==3.or.j==4)ichk0=1;if(j==1)then; v1=-yi(:,j); v2=v1; sv(2)=sv(1); v3=1.; v4=1.; endif; if
     l(j==2) v2=-yi(:,j);if(j==3)then; v3=-yi(:,j); v4=v3; endif; if(j==4) v4=-yi(:,j);end select;if(ichk0==1)then;do i=0,n; if(i==i
     lb) Cycle; if(yi(i,j)>=0d0)then;chw='Not positive element in the '//trim(mname)//' in row '//ch1//' ,defining a function in Car
     ldinality group.';call putmess('S',573,'Cardinality_Group_Checking',chw); goto 79999;endif; enddo;endif;enddo;select case(nfi);
      case(290,310); do i=0,n; if(i==ib)Cycle; if(v1(i)+sv(1)<v2(i)+sv(2))Exit; enddo;case(320);  do i=0,n; if(i==ib)Cycle; if(v1(i)
     l<=w/v3(i)+sv(1))Exit; enddo;case(321);  do i=0,n; if(i==ib)Cycle; if(v1(i)<=w/v3(i)-sv(1))Exit; enddo;case(330);do i=0,n; if(i
     l==ib)Cycle; if(sv(1)<sv(2))Exit;if(v1(i)<=w/v3(i)+sv(1).or.v2(i)<=w/v4(i)-sv(2))Exit;enddo;case(1480:1530); i=n+1;end select;i
     lf(i<=n)then;chw='Inconsistent parameters and data in Cardinaliry Group function with '//trim(mname);call putmess('S',579,'Card
     linality_Group_Checking',chw); goto 79999;endif;enddo
79999 continue;if(associated(v1)) deallocate (v1);if(associated(v2)) deallocate (v2);if(associated(v3)) deallocate (v3);if(associate
     ld(v4)) deallocate (v4);if(associated(sv)) deallocate (sv);return;end subroutine Card_Check;subroutine Card_Change_YI(ivb,ivrc,
     livarb,nextWhat,kcard, m,n1,n,x,ix,nz,nf,wfm,nc,ncn,kzp,bnds,cf,lnvr,
     +yi,ibcrd,
     +
     +chw     );use IntelInterf;use modcommons;ENTRY Card_Change_YIIshtv(ivb,ivrc,ivarb,nextWhat,kcard,m,n1,n,x,ix,nz,nf,wfm,nc,ncn,
     lkzp,bnds,cf,lnvr,numLcon,intarr,kac,
     +dparr,bb, yi,
     +ibcrd,
     +chw );integer(4) ivb,ivrc,nextWhat,m,n1,n,nz,kzp, ix(0:*),nf(*),nc(*),intarr(*),kac(*),lnvr(*),numLcon(*),ncn(*);real(8) x(0:*
     l),wfm(*),cf(*),dparr(*),bb(2,*),yi(0:n,0:*),bnds(0:1,0:*);integer(1) ibcrd(n1,3); integer(2) ivarb,kcard; character(*) chw;rea
     ll(8) f,wf,tb,xhuge,tb0,wf1,Ai,Bi,be1,delt,w2,w;real(8),pointer :: wu(:),mu(:),bet(:);integer(4) i,j,iret,ii,ndl,ncr,nfi,ks,iv,
     liv1;integer(4) ksp,ksd,ks1,j2,lnn,i1,nu,j1,i2; integer(plen) ksd8;real(8), allocatable:: zi(:,:),w2m(:); integer(4),allocatabl
     le:: lr(:,:),np(:);integer(4), allocatable:: ns2(:);real(8) enk,xbndhuge; real(8), pointer::lb(:),ub(:);common/shr/enk,xbndhuge
     l,lb,ub;nullify(wu,mu,bet);allocate(wu(0:min(n,int(1.4e1,4))),mu(0:min(n,int(1.6e1,4))),bet(0:min(n,int(0.15e2,4))));if(nextWha
     lt>0) then;allocate(zi(0:n*2,4),lr(0:n*2+1,4),np(4));ncr=m;  xhuge=huge(wf)/2d0;do ii=1,nz; nfi=iabs(nf(ii)); if(cf(ii)==0d0) C
     lycle;if(.not.(nfi==290.or.nfi>=310.and.nfi<=330))then; ncr=ncr+1; Cycle; endif;ndl=0; wf=dabs(wfm(ii))
5     ndl=ndl+1; ncr=ncr+1;call form_wumubet(n,m,nz,nfi,ix,ndl,wf,yi,xhuge,x,  wu,mu,bet,iret);if(iret==1) goto 15;do i=0,n; j=ix(i)
     l; if(j==0)Cycle;  lr(i,ndl)=0;if(bet(i)==0d0.or.mu(i)==xhuge)Cycle;tb0=wu(i)/mu(i); wf1=dabs(wfm(ii)/mu(i));wf1=0d0;if(bet(i)*
     lmu(i)>0d0)then; tb=tb0-wf1; zi(i,ndl)=x(j)-tb; lr(i,ndl)=+1;else;                     tb=tb0+wf1; zi(i,ndl)=tb-x(j); lr(i,ndl)
     l=-1;endif;enddo
15    if(ndl==1.or.nfi==330.and.ndl<=3) goto 5;ksd=0;  if(ivarb==0) GOTO 17;do i=0,n; if(ix(i)==0)Cycle;do j=1,ndl; if(lr(i,j)==0)Cy
     lcle; w=zi(i,j);do j1=j+1,ndl; if(lr(i,j)*lr(i,j1)>=0) Cycle;if(zi(i,j1)+w>0d0) ksd=ksd+1;enddo;enddo;enddo;if(ksd==0) GOTO 171
      allocate(ns2(ksd+1),w2m(ksd+1));ksp=0;do i=0,n; if(ix(i)==0)Cycle;ks=0;do j=1,ndl; if(lr(i,j)==0)Cycle; w=zi(i,j); j1=ks
181   if(j1>=1) then; if(w<zi(i,np(j1)))then; np(j1+1)=np(j1); j1=j1-1; goto 181; endif;endif;np(j1+1)=j; ks=ks+1;j1=ncr-ndl+j; w=da
     lbs(yi(i,j1)); if(w==1d30.or.w==1d29) yi(i,j1)=1d26*lr(i,j);enddo;do j=1,ks; w=zi(i,np(j));do j1=j+1,ks; if(lr(i,np(j))==lr(i,n
     lp(j1))) Cycle;if(zi(i,np(j1))+w>0d0)then;w2=dabs(zi(i,np(j1))-w)/2d0;j2=ksp
19    if(j2>=1) then; if(w2>w2m(ns2(j2)))then; ns2(j2+1)=ns2(j2); j2=j2-1; goto 19; endif;endif;ksp=ksp+1; ns2(j2+1)=ksp; w2m(ksp)=w
     l2;endif;enddo;enddo;enddo;if(kcard<=0.or.nextWhat<=0) then; ksd=ksp;elseif(ivarb==1)then; ksd=(2*ksp*nextWhat)/kcard+1;elseif(
     livarb==2)then; ksd=1;endif;ksd=min0(ksp,ksd); ksd8=ksd; call SORTQQ (LOC(ns2),ksd8, SRT$INTEGER4)
17    continue;ksp=0; ks1=1;do i=0,n; if(ix(i)==0)Cycle;ks=0;do j=1,ndl; if(lr(i,j)==0)Cycle; w=zi(i,j); j1=ks
18    if(j1>=1) then; if(w<zi(i,np(j1)))then; np(j1+1)=np(j1); j1=j1-1; goto 18; endif;endif;np(j1+1)=j; ks=ks+1;j1=ncr-ndl+j; w=dab
     ls(yi(i,j1)); if(w==1d30.or.w==1d29) yi(i,j1)=1d26*lr(i,j);enddo;do j=1,ks; w=zi(i,np(j)); iv=0; if(dabs(yi(i,ncr-ndl+np(j)))==
     l1d30) iv=1;do j1=j+1,ks; if(lr(i,np(j))==lr(i,np(j1))) Cycle;iv1=0; if(dabs(yi(i,ncr-ndl+np(j1)))==1d30) iv1=1;if(zi(i,np(j1))
     l+w>0d0) then; ksp=ksp+1;if(ksd<=0)then; if(iv==0) yi(i,ncr-ndl+np(j1))=1d30*lr(i,np(j1));elseif(ks1<=ksd.and.ksp==ns2(ks1).and
     l.ivarb/=2)then;ks1=ks1+1; if(iv==0) yi(i,ncr-ndl+np(j1))=1d30*lr(i,np(j1));elseif(iv==0.and.iv1==0)then; yi(i,ncr-ndl+np(j1))=
     l1d29*lr(i,np(j1)); yi(i,ncr-ndl+np(j))=1d29*lr(i,np(j));endif;endif;enddo;enddo;enddo;if(allocated(ns2))deallocate(w2m,ns2)
171   enddo;endif;if(nextWhat>0.and.ivrc<0) then;ncr=m;  xhuge=huge(wf)/2d0;do ii=1,nz; nfi=iabs(nf(ii)); if(cf(ii)==0d0) Cycle;ndl=
     l0; wf=dabs(wfm(ii))
510   ndl=ndl+1; ncr=ncr+1;call form_wumubet(n,m,nz,nfi,ix,ndl,wf,yi,xhuge,x,  wu,mu,bet, iret);if(iret==1) goto 1510;i1=abs(ivb);do
     l i=0,n; j=ix(i); if(j==0)Cycle; if(dabs(yi(i,ncr))==1d30)Cycle;if(j/=i1.or.bet(i)==0d0.or.mu(i)==xhuge)Cycle;tb0=wu(i)/mu(i); 
      wf1=dabs(wfm(ii)/mu(i));if(bet(i)*mu(i)>0d0)then;if(ivb>0)then; ub(j)=dmin1(ub(j),tb0-wf1);else; lb(j)=dmax1(lb(j),tb0+wf1);en
     ldif;else;if(ivb>0)then; lb(j)=dmax1(lb(j),tb0+wf1);else; ub(j)=dmin1(ub(j),tb0-wf1);endif;endif;Exit;enddo
1510  if(nfi==290.or.nfi>=310.and.nfi<=330) then; if(ndl==1.or.nfi==330.and.ndl<=3) goto 510;endif;enddo;endif;if(nextWhat>0.and.ivr
     lc>0) then;if(allocated(ns2))deallocate(w2m,ns2); allocate(w2m(2*n),ns2(2*n));ncr=m;  xhuge=huge(wf)/2d0;do ii=1,nz; nfi=iabs(n
     lf(ii)); if(cf(ii)==0d0) Cycle;if(nfi>=320.and.ivarb==2) then; ncr=ncr+2; if(nfi==330) ncr=ncr+2; Cycle;endif;ksp=0; ks1=0; f=0
     ld0; w2=0d0;ndl=0; wf=dabs(wfm(ii))
51    ndl=ndl+1; ncr=ncr+1;call form_wumubet(n,m,nz,nfi,ix,ndl,wf,yi,xhuge,x,  wu,mu,bet, iret);if(iret==1) goto 151;do i=0,n; j=ix(
     li); if(j==0)Cycle; if(dabs(yi(i,ncr))==1d30)Cycle;be1=bet(i);  ibcrd(j,1)=-1;if(be1==0d0) Cycle; if(mu(i)==xhuge)then; ksp=ksp
     l+1; w2=w2+be1; Cycle; endif;tb0=wu(i)/mu(i); wf1=dabs(wfm(ii)/mu(i)); w=dabs(tb0)*1d-2; if(w==0d0)w=1d-5;if(be1*mu(i)>0d0)then
     l; tb=tb0-wf1;if(dmin1(x(j),ub(j))-tb0>0d0)then;ks1=ks1+1; if(be1>0d0) f=f+be1; be1=dabs(be1);zi(ks1,1)=(x(j)-tb)/be1; zi(ks1,2
     l)=dmax1(lb(j),tb0+w); zi(ks1,3)=dmin1(ub(j),tb); ns2(ks1)=j; w2m(ks1)=be1;else; ub(j)=dmin1(ub(j),tb); if(be1<0d0) f=f+be1;end
     lif;else;                     tb=tb0+wf1;if(dmax1(x(j),lb(j))-tb0<0d0)then;ks1=ks1+1; if(be1>0d0) f=f+be1; be1=dabs(be1);zi(ks1
     l,1)=(-x(j)+tb)/be1; zi(ks1,2)=dmin1(ub(j),tb0-w); zi(ks1,3)=dmax1(lb(j),tb); ns2(ks1)=-j; w2m(ks1)=be1;else; lb(j)=dmax1(lb(j)
     l,tb); if(be1<0d0) f=f+be1;endif;endif;enddo
151   if(nfi==290.or.nfi>=310.and.nfi<=330) then; if(ndl==1.or.nfi==330.and.ndl<=3) goto 51;endif;ndl=0; i=nc(ii); do j=1,kzp; if(nc
     ln(j)==i) ndl=ndl+1; enddo;if(cf(ii)>0d0)then; wf1=bnds(1,i)/cf(ii); elseif(cf(ii)<0d0)then; wf1=bnds(0,i)/cf(ii); else; Cycle;
       endif;if(ndl>1) wf1=xhuge;do i=0,ks1+1; lr(i,1)=i+1; lr(i,2)=i-1; enddo;i2=1;if(w2+f>wf1)then;call CVaR_2(ks1,w2m,w2+f-wf1,zi
     l(1,1),lr(0,1),lr(0,2),0,ks1+1,-1.,
     +i1, ksd,w,delt);i=lr(0,1); i2=lr(i1,1);do while(i/=i2); j=ns2(i);if(j>0)then; ub(j)=zi(i,3); else; lb(-j)=zi(i,3); endif;   i=
     llr(i,1);enddo;endif;i=i2;do while(i/=ks1+1); j=ns2(i); ibcrd(iabs(j),1)=1;if(j>0)then; lb(j)=zi(i,2); else; ub(-j)=zi(i,2); en
     ldif;   i=lr(i,1);enddo;enddo;deallocate(w2m,ns2);endif;ncr=m;  xhuge=huge(wf)/2d0;do 25 ii=1,nz; nfi=iabs(nf(ii));delt=1d-5;de
     llt=dmax1(1d-8,dabs(wfm(ii))/3.);ndl=0; wf=dabs(wfm(ii))
10    ndl=ndl+1; ncr=ncr+1;call form_wumubet(n,m,nz,nfi,ix,ndl,wf,yi,xhuge,x,  wu,mu,bet, iret);if(iret==1) goto 20;if(nextWhat>0.an
     ld.cf(ii)/=0d0) then;if(ivrc<=0) then;do i=0,n; j=ix(i); if(j==0)Cycle; if(bet(i)==0d0.or.mu(i)==xhuge)Cycle;tb0=wu(i)/mu(i); w
     lf1=dabs(wfm(ii)/mu(i)); w=dabs(yi(i,ncr));if(bet(i)*mu(i)>0d0)then; tb=tb0-wf1; if(x(j)>tb.and.w/=1d30.and.w/=1d29) yi(i,ncr)=
     lx(j);else;                     tb=tb0+wf1; if(x(j)<tb.and.w/=1d30.and.w/=1d29) yi(i,ncr)=x(j);endif;enddo;else;do i=0,n; j=ix(
     li); if(j==0)Cycle; if(ibcrd(j,3)/=1)Cycle;w=dabs(yi(i,ncr)); if(w/=1d30.and.w/=1d29) yi(i,ncr)=1d0;enddo;endif;else;do i=0,n; 
      j=ix(i); if(j==0)Cycle;if(bet(i)==0d0.or.mu(i)==xhuge)Cycle;if(mu(i)*bet(i)>=0d0)then; yi(i,ncr)=ub(j); if(nfi>=320) yi(i,ncr)
     l=1d26;else;                      yi(i,ncr)=lb(j); if(nfi>=320) yi(i,ncr)=-1d26;endif;enddo;endif;if(nextWhat<=-99) goto 20;lnn
     l=lnvr(ii);if(lnn<=0) goto 20;   i1=numLcon(ii);do i=1,ii-1; if(nf(i)==nf(ii).and.wfm(i)==wfm(ii)) goto 20; enddo;nu=0;do i=0,n
     l; j=ix(i); if(j==0)Cycle; nu=nu+1;f=0d0; Ai=0d0; Bi=0d0; be1=0d0;if(bet(i)==0d0)then;elseif(mu(i)==xhuge)then; Bi=bet(i);else;
       be1=bet(i); tb0=wu(i)/mu(i); wf1=dabs(wfm(ii)/mu(i));if(be1<0)then; f=be1; be1=-be1; endif;if(bet(i)*mu(i)>0d0)then; tb=tb0-w
     lf1;Ai=be1/(yi(i,ncr)+delt-tb); Bi=f+Ai*(0d0-tb);else; tb=tb0+wf1;Ai=be1/(yi(i,ncr)-delt-tb); Bi=f+Ai*(0d0-tb);endif;endif;i2=i
     l1+n*(ndl-1)+nu; lb(lnn+n*(ndl-1)+nu)=0d0;if(bet(i)<0d0) lb(lnn+n*(ndl-1)+nu)=bet(i);if(dabs(Ai)<1d-9)then; lb(lnn+n*(ndl-1)+nu
     l)=Bi;w=min(abs(lb(j)),abs(ub(j)));if(w==abs(lb(j)))then; Ai=-1d-3; Bi=(Bi-Ai*lb(j))-1.;else; Ai=+1d-3; Bi=(Bi-Ai*ub(j))-1.;end
     lif;endif;if(dabs(yi(i,ncr))==1d30) lb(lnn+n*(ndl-1)+nu)=lb(lnn+n*(ndl-1)+nu)+be1;do j1=kac(j),kac(j+1)-1;if(intarr(j1)==i2) th
     len; dparr(j1)=Ai; Exit; endif;enddo;bb(2,i2)=-Bi;if(j1==kac(j+1)) goto 30;enddo
20    if(nfi==290.or.nfi>=310.and.nfi<=330) then; if(ndl==1.or.nfi==330.and.ndl<=3) goto 10;endif
25    enddo;if(allocated(zi))deallocate(zi,lr,np);if(associated(wu)) deallocate (wu);if(associated(mu)) deallocate (mu);if(associate
     ld(bet)) deallocate (bet);RETURN
30    chw='Internal error Number Card_Change_VarBnds'; call putmess('S',572,'Card_Change_VarBnds',chw);END SUBROUTINE Card_Change_YI
      subroutine Card_FunCalc(mname,m,yi,n1,n,x,ix,nz,nf,wfm,nc,cf,chw,
     +fi,xwrk );integer(4) m,n1,n,nz, ix(0:*),nf(*),nc(*);real(8) x(0:*),wfm(*),cf(*),fi(0:*),yi(0:n,0:*),xwrk(0:*);real(8) f,f1,tb,
     ldelt,wf,xhuge,tb0,wf1,be1;real(8),pointer :: wu(:),mu(:),bet(:);character(*) chw, mname;integer(4) i,j,iret,ndl,nfi,ii,ncr;log
     lical lf00, lf19, lf20, lf21, lf22, lf23, lf24;common/funits/ lf00,lf19,lf20,lf21,lf22,lf23,lf24;i=n1;if(chw=='itg0==10') RETUR
     lN;if(chw=='cardzero') return;allocate(wu(0:n),mu(0:n),bet(0:n));ncr=m;  xhuge=huge(wf)/2d0;do ii=1,nz; nfi=nf(ii); if(nfi<=0)C
     lycle;if(nfi>=1480) nfi=nfi-1200;delt=1d-5;delt=dmax1(1d-8,dabs(wfm(ii))/3.);ndl=0; wf=dabs(wfm(ii))
10    f=0d0; f1=0d0;  ndl=ndl+1; ncr=ncr+1;call form_wumubet(n,m,nz,nfi,ix,ndl,wf,yi,xhuge,x,  wu,mu,bet, iret);if(iret==1) goto 20;
      do i=0,n; j=ix(i); if(j==0)Cycle;if(bet(i)==0d0)Cycle;if(mu(i)==xhuge)then; f=f+bet(i); f1=f1+bet(i); Cycle;endif;be1=bet(i); 
      tb0=wu(i)/mu(i); wf1=dabs(wfm(ii)/mu(i));if(be1<0)then; f=f+be1; f1=f1+be1; be1=-be1; endif;if(dabs(yi(i,ncr))==1d30)then; f=f
     l+be1; f1=f1+be1; Cycle; endif;if(bet(i)*mu(i)>0d0)then; tb=tb0-wf1;if(x(j)>tb)then;  if(x(j)>tb0) f1=f1+be1;f=f+be1*(x(j)-tb)/
     l(yi(i,ncr)+delt-tb);endif;else;   tb=tb0+wf1;if(x(j)<tb)then;  if(x(j)<tb0) f1=f1+be1;f=f+be1*(x(j)-tb)/(yi(i,ncr)-delt-tb);en
     ldif;endif;enddo;if(mname/=' '.or.chw=='itg0==100')then;if(lf21)write(21,'(a,2i5,2e20.10)')'Card_nepreryvnaia/diskretnaia: nf,n
     ldl,f,f1',nfi,ndl,f,f1;f=f1;endif;fi(nc(ii))=fi(nc(ii))+ cf(ii)*f1;if(chw=='xwrk') xwrk(nc(ii))=xwrk(nc(ii))+ cf(ii)*(f-f1)
20    continue;if(nfi==290.or.(nfi>=310.and.nfi<=330))then; if(ndl==1.or.nfi==330.and.ndl<=3) goto 10;endif;enddo;deallocate(wu,mu,b
     let);return;end subroutine Card_FunCalc;subroutine Card_GradCalc(lnrz,m,yi,n1,n,x,ix,nz,nf,wfm,nc,cf,chw,
     +g,
     +isg, fw );integer(4) lnrz(0:*),m,n1,n,nz, ix(0:*),nf(*),nc(*); character(*) chw;real(8) x(0:*),wfm(*),cf(*),g(0:n1,0:*),fw(*),
     lyi(0:n,*);integer(4) isg(0:*);real(8) w,delt,xhuge,tb0,wf,wf1,be1,tb;real(8),pointer :: wu(:),mu(:),bet(:);integer(4) i,i1,ii,
     lj,iret,ndl,nfi,ncr;if(chw=='itg0==10') RETURN;if(chw=='cardzero') return;allocate(wu(0:n),mu(0:n),bet(0:n));ncr=m;  xhuge=huge
     l(wf)/2d0;do ii=1,nz; nfi=nf(ii); if(nfi<=0)Cycle;if(.not.(nc(ii)==0.or.isg(nc(ii))/=0) .or. lnrz(nc(ii))>0) then;ncr=ncr+1; if
     l(nfi==290.or.nfi>=310.and.nfi<=321) ncr=ncr+1; if(nfi==330) ncr=ncr+3;Cycle;endif;i1=isg(nc(ii));w=cf(ii); if(i1/=0) w=w/fw(nc
     l(ii));if(i1<0) then; i1=-i1; w=-w; endif;wf=dabs(wfm(ii));delt=1d-5;delt=dmax1(1d-8,dabs(wfm(ii))/3.);ndl=0
10    ndl=ndl+1; ncr=ncr+1;call form_wumubet(n,m,nz,nfi,ix,ndl,wf,yi,xhuge,x,  wu,mu,bet, iret);if(iret==1) goto 20;do i=0,n; j=ix(i
     l); if(j==0)Cycle;if(bet(i)==0d0.or.mu(i)==xhuge)Cycle;be1=bet(i); tb0=wu(i)/mu(i); wf1=dabs(wfm(ii)/mu(i));if(be1<0) be1=-be1;
      if(bet(i)*mu(i)>0d0)then; tb=tb0-wf1; if(x(j)>tb) g(j,i1)=g(j,i1)+w*be1/(yi(i,ncr)+delt-tb);else;                     tb=tb0+w
     lf1; if(x(j)<tb) g(j,i1)=g(j,i1)+w*be1/(yi(i,ncr)-delt-tb);endif;enddo
20    if(nfi==290.or.nfi>=310.and.nfi<=330)then; if(ndl==1.or.nfi==330.and.ndl<=3) goto 10;endif;enddo;deallocate(wu,mu,bet);end sub
     lroutine Card_GradCalc;subroutine form_wumubet(n,m,nz,nf,ix,ndl, wf,yi,xhuge,x,
     +wu,mu,bet, iret);integer(4) n,m,nz,nf,ix(0:*),iret;real(8) wf,yi(0:n,0:*),x(0:*),xhuge,   wu(0:*),mu(0:*),bet(0:*), wu0,g1,g2,
     ls0,amu,w;integer(4) i,i1; integer(4) ndl;real(8) enk,xbndhuge; real(8), pointer::lb(:),ub(:);common/shr/enk,xbndhuge,lb,ub;i=n
     lz;iret=0;wu0=0.; s0=0.;select case(nf);case(280); do i=0,n; if(ix(i)==0)then; wu0=wf; s0=+yi(i,1); else; mu(i)=-yi(i,1); endif
     l; bet(i)=1d0; enddo;case(281); do i=0,n; if(ix(i)==0)then; wu0=wf; s0=-yi(i,1); else; mu(i)=+yi(i,1); endif; bet(i)=1d0; enddo
      case(290);if(ndl==1)then; do i=0,n; if(ix(i)==0)then; wu0=wf; s0=+yi(i,1); else; mu(i)=-yi(i,1); endif; bet(i)=1d0; enddo;else
     l;   if(m>=2) then;  do i=0,n; if(ix(i)==0)then; wu0=wf; s0=-yi(i,2); else; mu(i)=+yi(i,2); endif; bet(i)=1d0; enddo;else;     
            do i=0,n; if(ix(i)==0)then; wu0=wf; s0=-yi(i,1); else; mu(i)=+yi(i,1); endif; bet(i)=1d0; enddo;endif;endif;case(300); i
     lf(m>=2)then; do i=0,n; if(ix(i)==0)then; wu0=wf; s0=+yi(i,1); else; mu(i)=-yi(i,2); endif; bet(i)=-yi(i,1);enddo;else;        
       do i=0,n; if(ix(i)==0)then; wu0=wf; s0=+yi(i,1); else; mu(i)=1d0;     endif; bet(i)=-yi(i,1); enddo;endif;case(301); if(m>=2)
     lthen; do i=0,n; if(ix(i)==0)then; wu0=wf; s0=-yi(i,1); else; mu(i)=+yi(i,2); endif; bet(i)=-yi(i,1);enddo;else;         do i=0
     l,n; if(ix(i)==0)then; wu0=wf; s0=-yi(i,1); else; mu(i)=-1d0;    endif; bet(i)=-yi(i,1); enddo;endif;case(310);if(ndl==1)then;i
     lf(m>=3)then; do i=0,n; if(ix(i)==0)then; wu0=wf; s0=+yi(i,1); else; mu(i)=-yi(i,3); endif; bet(i)=-yi(i,1); enddo;else;       
        do i=0,n; if(ix(i)==0)then; wu0=wf; s0=+yi(i,1); else; mu(i)=1d0;      endif; bet(i)=-yi(i,1); enddo;endif;else;if(m>=4)then
     l;     do i=0,n; if(ix(i)==0)then; wu0=wf; s0=-yi(i,2); else; mu(i)=+yi(i,4); endif; bet(i)=-yi(i,2); enddo;elseif(m>=2)then; d
     lo i=0,n; if(ix(i)==0)then; wu0=wf; s0=-yi(i,2); else; mu(i)=-1d0;     endif; bet(i)=-yi(i,2); enddo;else;             do i=0,n
     l; if(ix(i)==0)then; wu0=wf; s0=-yi(i,1); else; mu(i)=-1d0;     endif; bet(i)=-yi(i,1); enddo;endif;endif;case(320);if(m>=2) th
     len;if(ndl==1)then; do i=0,n; if(ix(i)==0)then; wu0=wf; s0=+yi(i,1); else; mu(i)=-yi(i,2); endif; bet(i)=1d0; enddo;else;      
           do i=0,n; if(ix(i)==0)then; wu0=-0d0; s0=0d0;    else; mu(i)=-yi(i,2); endif; bet(i)=-1d0; enddo; endif;else;if(ndl==1)th
     len; do i=0,n; if(ix(i)==0)then; wu0=wf; s0=+yi(i,1); else; mu(i)=1d0; endif; bet(i)=1d0; enddo;else;           do i=0,n; if(ix
     l(i)==0)then; wu0=-0d0; s0=0d0; else; mu(i)=1d0; endif; bet(i)=-1d0; enddo; endif;endif;case(321);if(m>=2) then;if(ndl==1)then;
       do i=0,n; if(ix(i)==0)then; wu0=wf; s0=-yi(i,1); else; mu(i)=+yi(i,2); endif; bet(i)=1d0; enddo;else;           do i=0,n; if(
     lix(i)==0)then; wu0=-0d0; s0=0d0;    else; mu(i)=+yi(i,2); endif; bet(i)=-1d0; enddo; endif;else;if(ndl==1)then; do i=0,n; if(i
     lx(i)==0)then; wu0=wf; s0=-yi(i,1); else; mu(i)=-1d0; endif; bet(i)=1d0; enddo;else;           do i=0,n; if(ix(i)==0)then; wu0=
     l-0d0; s0=0d0; else; mu(i)=-1d0; endif; bet(i)=-1d0; enddo; endif;endif;case(330);select case(ndl);case(1); if(m>=3)then; do i=
     l0,n; if(ix(i)==0)then; wu0=wf; s0=+yi(i,1); else; mu(i)=-yi(i,3); endif; bet(i)=1d0; enddo;else;         do i=0,n; if(ix(i)==0
     l)then; wu0=wf; s0=+yi(i,1); else; mu(i)=+1d0;     endif; bet(i)=1d0; enddo;endif;case(2); if(m>=3)then; do i=0,n; if(ix(i)==0)
     lthen; wu0=-0d0; s0=0d0;    else; mu(i)=-yi(i,3); endif; bet(i)=-1d0; enddo;else;         do i=0,n; if(ix(i)==0)then; wu0=-0d0;
       s0=0d0;    else; mu(i)=+1d0;     endif; bet(i)=-1d0; enddo;endif;case(3); if(m>=4)then;     do i=0,n; if(ix(i)==0)then; wu0=w
     lf; s0=-yi(i,2); else; mu(i)=yi(i,4); endif; bet(i)=1d0; enddo;elseif(m>=2)then; do i=0,n; if(ix(i)==0)then; wu0=wf; s0=-yi(i,2
     l); else; mu(i)=-1d0;    endif; bet(i)=1d0; enddo;else;             do i=0,n; if(ix(i)==0)then; wu0=wf; s0=-yi(i,1); else; mu(i
     l)=-1d0;    endif; bet(i)=1d0; enddo;endif;case(4); if(m>=4)then; do i=0,n; if(ix(i)==0)then; wu0=-0d0; s0=0d0;    else; mu(i)=
     l+yi(i,4); endif; bet(i)=-1d0; enddo;else;         do i=0,n; if(ix(i)==0)then; wu0=-0d0; s0=0d0;    else; mu(i)=-1d0;     endif
     l; bet(i)=-1d0; enddo;endif;end select;case default; iret=1; return;end select;do i=0,n; i1=ix(i); if(i1==0)Cycle;g1=dmin1(lb(i
     l1),x(i1)); g2=dmax1(ub(i1),x(i1)); if(mu(i)<0d0)then;  w=g1; g1=g2; g2=w;  endif;amu=abs(mu(i));wu(i)=wu0+s0*amu;if(nf==280)th
     len; if(m==2)wu(i)=wu(i)-yi(i,2)*amu;elseif(nf==281)then; if(m==2)wu(i)=wu(i)+yi(i,2)*amu;elseif(nf==290.and.m>2)then;if(ndl==1
     l)then; wu(i)=wu(i)-yi(i,3)*amu;elseif(m==3)then; wu(i)=wu(i)+yi(i,3)*amu; else; wu(i)=wu(i)+yi(i,4)*amu;endif;elseif(nf>310) t
     lhen;if(ndl==2)then;if(nf==320)wu(i)=wu(i)-yi(i,1)*amu; if(nf==321)wu(i)=wu(i)-yi(i,1)*amu; if(nf==330)wu(i)=wu(i)-yi(i,1)*amu;
      elseif(ndl==4)then;if(m>=2)then; wu(i)=wu(i)-yi(i,2)*amu; else; wu(i)=wu(i)-yi(i,1)*amu; endif;endif;endif;if(g2*mu(i)<=wu(i).
     lor.bet(i)==0d0) then;  bet(i)=0d0;elseif(g1*mu(i)>wu(i)) then; mu(i)=xhuge;else;endif;enddo;return;
      end subroutine form_wumubet
