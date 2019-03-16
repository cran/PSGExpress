      subroutine FuncsL1L2(mname,itt,x,iVarDop, m1,m2,n1,n2,yi1,yi2,ix1,ix2,p1,p2, nz,nf,wf,nc,cf,
     +jp1,jpb1,fi,chw,
     +gst,fm1,pf1,polka,mget,avg);use CiFort;integer(4) itt,iVarDop(*),m1,m2,n1,n2,ix1(0:*),ix2(0:*),nz,nf(*),nc(*),mget(*);integer(
     l4),target::jp1(0:*),jpb1(0:*);integer(4),pointer::jp2(:),jpb2(:);real(8) x(0:*),yi1(0:n1,0:*),yi2(0:n2,0:*),wf(*),cf(*),fi(0:*
     l),polka(*),p2(*);real(8),target::p1(*),gst(*),fm1(*),pf1(*);real(8),pointer::gst1(:),gst2(:),fm2(:),pf2(:);real(8) avg,avg1,av
     lg2,w,sp1,sp2,thr0,wp,fmax,fmin,thr,thr1,thr2,wsp2,ss1,fj,wsp1,ss2;integer(4) ipmx(2),ipmn(2),mdiscr,jmax1,jmin1,jmax2,jmin2,i,
     liw,jw,iz,nf1,iva,kup,kle,j1m,j1x,j2m,j2x,j2,j1;character(*) chw, mname;if(chw=='itg0==10') RETURN;mdiscr=1000000;call r8_point
     ler_set(fm1(m1+1),1,m2,fm2);call r8_pointer_set(pf1(m1+1),1,m2,pf2);call i4_pointer_set(jp1(m1+2),0,m2+1,jp2);call i4_pointer_s
     let(jpb1(m1+2),0,m2+1,jpb2);call FM_One(m1,yi1,p1,n1,x,ix1, jmax1,jmin1,avg1,fm1,pf1);call FM_One(m2,yi2,p2,n2,x,ix2, jmax2,jmi
     ln2,avg2,fm2,pf2);select case(itt);case(501,503,511,513,540,542,550,552); avg2=-avg2; fm2=-fm2; pf2=-pf2; i=jmax2; jmax2=jmin2;
       jmin2=i;endselect;select case(itt);case(501,503,551,553,510,512,540,542);avg1=-avg1; fm1(1:m1)=-fm1(1:m1); pf1(1:m1)=-pf1(1:m
     l1); i=jmax1; jmax1=jmin1; jmin1=i;endselect;i=4*nz; mget(i+1)=jmin1; mget(i+2)=jmin2; mget(i+3)=jmax1; mget(i+4)=jmax2;call co
     lpybuff(loc(avg1),8,loc(mget(i+5)),8);call copybuff(loc(avg2),8,loc(mget(i+7)),8);avg=avg1+avg2;if(mname=='Objects'.or.mname=='
     lC'.or.chw=='VarL1L2')then;call CVaR_2(m1,p1,1.,fm1,jp1,jpb1,0,m1+1,1.,     iw,jw,w,sp1);call CVaR_2(m2,p2,1.,fm2,jp2,jpb2,0,m2
     l+1,1.,     iw,jw,w,sp2);do iz=1,nz; nf1=nf(iz); if(nf1<0.or.cf(iz)==0.) Cycle;select case(nf1);case(20,30,21,31,140,150,141,15
     l1);iva=iVarDop(iz); if(140<=nf1.and.nf1<=151) iva=iva+1;thr0=x(iva); wp=wf(iz);if(nf1==21.or.nf1==31.or.nf1==141.or.nf1==151) 
     lwp=1.-wp;fmax= fm1(jmax1)+fm2(jmax2);fmin= fm1(jmin1)+fm2(jmin2);thr0=(fmax+fmin)/2.
100   continue;thr=thr0;if(nf1==30.or.nf1==31.or.nf1==150.or.nf1==151) thr=thr0+avg;thr1=thr-fm2(jmax2); thr2=thr-fm1(jmax1);call So
     lrting(0,m1+1,fm1,jp1,jpb1,  thr1,1,1,  kup, kle, ipmx, ipmn );j1m=ipmn(1); j1x=ipmx(1);call Sorting(0,m2+1,fm2,jp2,jpb2,  thr2
     l,1,1,  kup, kle, ipmx, ipmn );j2m=ipmn(1); j2x=ipmx(1);wsp2=0.; j2=j2x; ss1=0.; sp1=0.; j1=j1m;do while(j1/=0); w=fm1(j1);if(j
     l2<=m2.and.j2>0)then;   do while(w+fm2(j2)>thr); wsp2=wsp2+p2(j2); j2=jp2(j2); if(j2>m2)Exit;enddo;            endif;sp1=sp1+p1
     l(j1)*wsp2; j1=jpb1(j1);enddo;w=1.-wp;if(w==sp1)then; goto 200;else; thr=thr0;if(sp1>w)then; fmin=thr0; else; fmax=thr0; endif;
      thr0=(fmin+fmax)/2.;if(abs(thr-thr0)<1e-9*max(1.,fmax-fmin)) goto 200;endif;goto 100
200   continue;x(iva)=thr0;end select;enddo;endif;do iz=1,nz; nf1=nf(iz); if(nf1<0.or.cf(iz)==0.) Cycle;gst1=>gst((iz-1)*(m1+m2)+1:i
     lz*(m1+m2));gst2=>gst1(m1+1:);select case(nf1);case(10,11); fj=avg1+avg2;case(140,150,141,151);fj=x(iVarDop(iz)+1);case(20,30,2
     l1,31); thr0=x(iVarDop(iz));thr=thr0; wp=wf(iz);if(nf1==30.or.nf1==31) thr=thr0+avg;if(nf1==21.or.nf1==31) wp=1.-wp;thr1=thr-fm
     l2(jmax2); thr2=thr-fm1(jmax1);call Sorting(0,m1+1,fm1,jp1,jpb1,  thr1,1,1,  kup, kle, ipmx, ipmn );j1m=ipmn(1); j1x=ipmx(1);if
     l(j1x>0) call CVaR_2(m1,p1,1.,fm1,jp1,jpb1,jpb1(j1x),jp1(j1m),1.,
     +iw,jw,w,sp1);call Sorting(0,m2+1,fm2,jp2,jpb2,  thr2,1,1,  kup, kle, ipmx, ipmn );j2m=ipmn(1); j2x=ipmx(1);if(j2x>0) call CVaR
     l_2(m2,p2,1.,fm2,jp2,jpb2,jpb2(j2x),jp2(j2m),1.,
     +iw,jw,w,sp2);wsp2=0.; j2=j2x; ss1=0.; sp1=0.; j1=j1m;do while(j1/=0); w=fm1(j1);if(j2<=m2.and.j2>0)then;   do while(w+fm2(j2)>
     lthr); wsp2=wsp2+p2(j2); j2=jp2(j2); if(j2>m2)Exit;enddo;            endif;ss1=ss1+pf1(j1)*wsp2; gst1(j1)=p1(j1)*wsp2; sp1=sp1+
     lgst1(j1); j1=jpb1(j1);enddo;wsp1=0.; j1=j1x; ss2=0.; sp2=0.; j2=j2m;do while(j2/=0); w=fm2(j2);if(j1<=m1.and.j1>0)then;   do w
     lhile(w+fm1(j1)>thr); wsp1=wsp1+p1(j1); j1=jp1(j1); if(j1>m1)Exit;enddo;            endif;ss2=ss2+pf2(j2)*wsp1; gst2(j2)=p2(j2)
     l*wsp1; j2=jpb2(j2);enddo;polka(iz)=sp1;i=4*(iz-1);mget(i+1)=j1m; mget(i+2)=j2m; mget(i+3)=j1x; mget(i+4)=j2x;fj=thr0+1./(1.-wp
     l)*(ss1+ss2-thr*sp1);case(70,80,71,81); thr=wf(iz);if(nf1==71.or.nf1==81) thr=-thr;if(nf1==80.or.nf1==81) thr=thr+avg;thr1=thr-
     lfm2(jmax2); thr2=thr-fm1(jmax1);call Sorting(0,m1+1,fm1,jp1,jpb1,  thr1,1,1,  kup, kle, ipmx, ipmn );j1m=ipmn(1); j1x=ipmx(1);
      if(j1x>0) call CVaR_2(m1,p1,1.,fm1,jp1,jpb1,jpb1(j1x),jp1(j1m),1.,
     +iw,jw,w,sp1);call Sorting(0,m2+1,fm2,jp2,jpb2,  thr2,1,1,  kup, kle, ipmx, ipmn );j2m=ipmn(1); j2x=ipmx(1);if(j2x>0) call CVaR
     l_2(m2,p2,1.,fm2,jp2,jpb2,jpb2(j2x),jp2(j2m),1.,
     +iw,jw,w,sp2);wsp2=0.; j2=j2x; ss1=0.; sp1=0.; j1=j1m;do while(j1/=0); w=fm1(j1);if(j2<=m2.and.j2>0)then;   do while(w+fm2(j2)>
     lthr); wsp2=wsp2+p2(j2); j2=jp2(j2); if(j2>m2)Exit;enddo;            endif;ss1=ss1+pf1(j1)*wsp2; gst1(j1)=p1(j1)*wsp2; sp1=sp1+
     lgst1(j1); j1=jpb1(j1);enddo;wsp1=0.; j1=j1x; ss2=0.; sp2=0.; j2=j2m;do while(j2/=0); w=fm2(j2);if(j1<=m1.and.j1>0)then;   do w
     lhile(w+fm1(j1)>thr); wsp1=wsp1+p1(j1); j1=jp1(j1); if(j1>m1)Exit;enddo;            endif;ss2=ss2+pf2(j2)*wsp1; gst2(j2)=p2(j2)
     l*wsp1; j2=jpb2(j2);enddo;polka(iz)=sp1;i=4*(iz-1);mget(i+1)=j1m; mget(i+2)=j2m; mget(i+3)=j1x; mget(i+4)=j2x;fj=ss1+ss2-thr*sp
     l1;case(90,91); fj= fm1(jmax1)+fm2(jmax2);case(100,101); fj= fm1(jmax1)+fm2(jmax2)-avg1-avg2;case(160:171);thr=wf(iz);if(nf1==1
     l61.or.nf1==171) thr=-thr;if(nf1==170.or.nf1==171) thr=thr+avg;thr1=thr-fm2(jmax2); thr2=thr-fm1(jmax1);call Sorting(0,m1+1,fm1
     l,jp1,jpb1,  thr1,1,1,  kup, kle, ipmx, ipmn );j1m=ipmn(1); j1x=ipmx(1);if(j1x>0) call CVaR_2(m1,p1,1.,fm1,jp1,jpb1,jpb1(j1x),j
     lp1(j1m),1.,
     +iw,jw,w,sp1);call Sorting(0,m2+1,fm2,jp2,jpb2,  thr2,1,1,  kup, kle, ipmx, ipmn );j2m=ipmn(1); j2x=ipmx(1);if(j2x>0) call CVaR
     l_2(m2,p2,1.,fm2,jp2,jpb2,jpb2(j2x),jp2(j2m),1.,
     +iw,jw,w,sp2);wsp2=0.; j2=j2x; ss1=0.; sp1=0.; j1=j1m;do while(j1/=0); w=fm1(j1);if(j2<=m2.and.j2>0)then;   do while(w+fm2(j2)>
     lthr); wsp2=wsp2+p2(j2); j2=jp2(j2); if(j2>m2)Exit;enddo;            endif;gst1(j1)=p1(j1)*wsp2; sp1=sp1+gst1(j1); j1=jpb1(j1);
      enddo;polka(iz)=sp1; i=4*(iz-1);mget(i+1)=j1m; mget(i+2)=j2m; mget(i+3)=j1x; mget(i+4)=j2x;fj=sp1;case(1360:1371);wp=wf(iz);if
     l(nf1==1361.or.nf1==1371) wp=-wp;fmax= fm1(jmax1)+fm2(jmax2);fmin= fm1(jmin1)+fm2(jmin2);if((nf1==1360.or.nf1==1361).and.(wp<=a
     lvg.or.wp>=fmax))then;if(wp>=fmax)then; sp1=0.; else; sp1=1.; endif;goto 210;endif;if((nf1==1370.or.nf1==1371).and.(wp<=0..or.w
     lp>=fmax-avg))then;if(wp>=fmax-avg)then; sp1=0.; else; sp1=1.; endif;goto 210;endif;thr0=(fmax+fmin)/2.;call CVaR_2(m1,p1,1.,fm
     l1,jp1,jpb1,0,m1+1,1.,     iw,jw,w,sp1);call CVaR_2(m2,p2,1.,fm2,jp2,jpb2,0,m2+1,1.,     iw,jw,w,sp2)
110   continue;thr=thr0;if(nf1==1370.or.nf1==1371) thr=thr0+avg;thr1=thr-fm2(jmax2); thr2=thr-fm1(jmax1);call Sorting(0,m1+1,fm1,jp1
     l,jpb1,  thr1,1,1,  kup, kle, ipmx, ipmn );j1m=ipmn(1); j1x=ipmx(1);call Sorting(0,m2+1,fm2,jp2,jpb2,  thr2,1,1,  kup, kle, ipm
     lx, ipmn );j2m=ipmn(1); j2x=ipmx(1);wsp2=0.; j2=j2x; ss1=0.; sp1=0.; j1=j1m;do while(j1/=0); w=fm1(j1);if(j2<=m2.and.j2>0)then;
         do while(w+fm2(j2)>thr); wsp2=wsp2+p2(j2); j2=jp2(j2); if(j2>m2)Exit;enddo;            endif;ss1=ss1+pf1(j1)*wsp2; sp1=sp1+
     lp1(j1)*wsp2; j1=jpb1(j1);enddo;wsp1=0.; j1=j1x; ss2=0.; sp2=0.; j2=j2m;do while(j2/=0); w=fm2(j2);if(j1<=m1.and.j1>0)then;   d
     lo while(w+fm1(j1)>thr); wsp1=wsp1+p1(j1); j1=jp1(j1); if(j1>m1)Exit;enddo;            endif;ss2=ss2+pf2(j2)*wsp1; j2=jpb2(j2);
      enddo;fj=thr0+1./sp1*(ss1+ss2-thr*sp1);w=wp;if(w==fj)then; goto 210;else; thr=thr0;if(fj<w)then; fmin=thr0; else; fmax=thr0; e
     lndif;thr0=(fmin+fmax)/2.;if(abs(thr-thr0)<1e-9*max(1.,fmax-fmin)) goto 210;endif;goto 110
210   continue;fj=sp1;case default; fj=0.;if(cf(iz)/=0.)then;write(chw,'(a,i6,a)')'Internal error. PSG can not calculate function ',
     lnf1,' for difference of losses';call putmess('S',5150,'Function calculation',chw);endif;end select;fi(nc(iz))=fi(nc(iz))+ cf(i
     lz)*fj;enddo;END subroutine FuncsL1L2;subroutine GradFuncsL1L2(itt,iVarDop,n12, m1,m2,n1,n2,yi1,yi2,ix1,ix2, nz,nf,wf,nc,cf,jpb
     l1, chw, fw,
     +gst,polka,mget,isg,
     +g);use CiFort;integer(4) itt,n12,iVarDop(*),m1,m2,n1,n2,ix1(0:*),ix2(0:*),nz,nf(*),nc(*),isg(0:*),mget(*);integer(4),target:: 
     ljpb1(0:*);integer(4),pointer::jpb2(:);real(8) yi1(0:n1,0:*),yi2(0:n2,0:*),wf(*),cf(*),polka(*),g(0:n12,0:*),fw(*);real(8),targ
     let::gst(*);real(8),pointer::gst1(:),gst2(:);character(*) chw;integer(4) iz,nf1,j1,i,j,j1m,j2m,jt,jmin1,jmin2,jmax1,jmax2;real(
     l8) wp,wg,w1,w2,sp1,fj;if(chw=='itg0==10') RETURN;call i4_pointer_set(jpb1(m1+2),0,m2+1,jpb2);i=4*nz; jmin1=mget(i+1); jmin2=mg
     let(i+2); jmax1=mget(i+3); jmax2=mget(i+4);do iz=nz,1,-1; nf1=nf(iz); if(nf1<0)Cycle; wp=wf(iz);if(nc(iz)==0.or.isg(nc(iz))/=0)
     l then; j1=isg(nc(iz));wg=cf(iz); if(j1/=0) wg=wg/fw(nc(iz));if(j1<0) then; j1=-j1; wg=-wg; endif;gst1=>gst((iz-1)*(m1+m2)+1:iz
     l*(m1+m2));gst2=>gst1(m1+1:);w1=wg; w2=wg;select case(itt); case(501,503,551,553,510,512,540,542); w1=-wg; endselect;select cas
     le(itt); case(501,503,511,513,540,542,550,552); w2=-wg; endselect;select case(nf1);case(10,11);do i=0,n1; j=ix1(i); g(j,j1)=g(j
     l,j1)+ w1*yi1(i,0); enddo;do i=0,n2; j=ix2(i); g(j,j1)=g(j,j1)+ w2*yi2(i,0); enddo;case(20,30,21,31); sp1=polka(iz);i=4*(iz-1);
       j1m=mget(i+1); j2m=mget(i+2);if(nf1==21.or.nf1==31) wp=1.-wp;jt=j1m; w1=w1/(1.-wp);do while(jt/=0); do i=0,n1; j=ix1(i); g(j,
     lj1)=g(j,j1)+ w1*gst1(jt)*yi1(i,jt); enddo; jt=jpb1(jt);enddo;if(nf1==30.or.nf1==31)then; do i=0,n1; j=ix1(i); g(j,j1)=g(j,j1)-
     l w1*sp1*yi1(i,0); enddo;endif;jt=j2m; w2=w2/(1.-wp);do while(jt/=0); do i=0,n2; j=ix2(i); g(j,j1)=g(j,j1)+ w2*gst2(jt)*yi2(i,j
     lt); enddo; jt=jpb2(jt);enddo;if(nf1==30.or.nf1==31)then; do i=0,n2; j=ix2(i); g(j,j1)=g(j,j1)- w2*sp1*yi2(i,0); enddo;endif;j=
     liVarDop(iz);g(j,j1)=g(j,j1)+ wg*(1.-1./(1.-wp)*sp1);case(70,80,71,81); sp1=polka(iz);i=4*(iz-1); j1m=mget(i+1); j2m=mget(i+2);
      jt=j1m;do while(jt/=0); do i=0,n1; j=ix1(i); g(j,j1)=g(j,j1)+ w1*gst1(jt)*yi1(i,jt); enddo; jt=jpb1(jt);enddo;jt=j2m;do while(
     ljt/=0); do i=0,n2; j=ix2(i); g(j,j1)=g(j,j1)+ w2*gst2(jt)*yi2(i,jt); enddo; jt=jpb2(jt);enddo;if(nf1==80.or.nf1==81)then;do i=
     l0,n1; j=ix1(i); g(j,j1)=g(j,j1)- w1*sp1*yi1(i,0); enddo;do i=0,n2; j=ix2(i); g(j,j1)=g(j,j1)- w2*sp1*yi2(i,0); enddo;endif;cas
     le(90,91);do i=0,n1; j=ix1(i); g(j,j1)=g(j,j1)+ w1*yi1(i,jmax1); enddo;do i=0,n2; j=ix2(i); g(j,j1)=g(j,j1)+ w2*yi2(i,jmax2); e
     lnddo;case(100,101);do i=0,n1; j=ix1(i); g(j,j1)=g(j,j1)+ w1*(yi1(i,jmax1)-yi1(i,0)); enddo;do i=0,n2; j=ix2(i); g(j,j1)=g(j,j1
     l)+ w2*(yi2(i,jmax2)-yi2(i,0)); enddo;case default; fj=0.;if(cf(iz)/=0.)then;write(chw,'(a,i6,a)')'Internal error. PSG can not 
     lcalculate gradient for function ',nf1,' for difference of losses';call putmess('S',5160,'Function calculation',chw);endif;end 
     lselect;endif;enddo;END subroutine GradFuncsL1L2;subroutine ChangeL1L2forVarPr(m1,m2,p1,p2, nz,nf, nextWhat,b1t,Wvar,nfn0,
     +jp1,jpb1,chw,
     +gst,fm1,pf1,polka,mget,
     +alp);use CiFort;integer(4) m1,m2,nz,nf(*),mget(*),nfn0(*);integer(4),target::jp1(0:*),jpb1(0:*);integer(4),pointer::jp2(:),jpb
     l2(:);real(8) polka(*),p2(*);real(8),target::p1(*),gst(*),fm1(*),pf1(*);real(8),pointer::gst1(:),gst2(:),fm2(:),pf2(:);real(8) 
     lavg;integer(4) ipmx(2),ipmn(2);character(*) chw;integer(4) nextWhat,i,jmin1,jmin2,jmax1,jmax2,iz,nf1,kup,kle,j1m,j1x,iw,jw,j2m
     l,j2x,j2,j1;real(8) b1t(*),Wvar(*),alp(*),avg1,avg2,thr,thr1,thr2,w,sp1,sp2,wsp2,ss1,fj;if(nextWhat<=0)then;b1t(1:nz)=0d0;RETUR
     lN;endif;if(chw=='itg0==10') RETURN;call r8_pointer_set(fm1(m1+1),1,m2,fm2);call r8_pointer_set(pf1(m1+1),1,m2,pf2);call i4_poi
     lnter_set(jp1(m1+2),0,m2+1,jp2);call i4_pointer_set(jpb1(m1+2),0,m2+1,jpb2);i=4*nz; jmin1=mget(i+1); jmin2=mget(i+2); jmax1=mge
     lt(i+3); jmax2=mget(i+4);call copybuff(loc(mget(i+5)),8,loc(avg1),8);call copybuff(loc(mget(i+7)),8,loc(avg2),8);avg=avg1+avg2;
      do iz=1,nz; nf1=nf(iz); if(nf1<0) Cycle;gst1=>gst((iz-1)*(m1+m2)+1:iz*(m1+m2));gst2=>gst1(m1+1:);thr=Wvar(iz);select case(nf1)
     l; case(150,170,151,171);  thr=thr+avg; end select;thr1=thr-fm2(jmax2); thr2=thr-fm1(jmax1);call Sorting(0,m1+1,fm1,jp1,jpb1,  
     lthr1,1,1,  kup, kle, ipmx, ipmn );j1m=ipmn(1); j1x=ipmx(1);if(j1x>0) call CVaR_2(m1,p1,1.,fm1,jp1,jpb1,jpb1(j1x),jp1(j1m),1.,
     +iw,jw,w,sp1);call Sorting(0,m2+1,fm2,jp2,jpb2,  thr2,1,1,  kup, kle, ipmx, ipmn );j2m=ipmn(1); j2x=ipmx(1);if(j2x>0) call CVaR
     l_2(m2,p2,1.,fm2,jp2,jpb2,jpb2(j2x),jp2(j2m),1.,
     +iw,jw,w,sp2);wsp2=0.; j2=j2x; ss1=0.; sp1=0.; j1=j1m;do while(j1/=0); w=fm1(j1);if(j2<=m2.and.j2>0)then;   do while(w+fm2(j2)>
     lthr); wsp2=wsp2+p2(j2); j2=jp2(j2); if(j2>m2)Exit;enddo;            endif;gst1(j1)=p1(j1)*wsp2; sp1=sp1+gst1(j1); j1=jpb1(j1);
      enddo;polka(nfn0(iz))=sp1; i=4*(iz-1);mget(i+1)=j1m; mget(i+2)=j2m; mget(i+3)=j1x; mget(i+4)=j2x;fj=sp1;alp(iz)=(sp1-b1t(iz))*
     l2.;if(alp(iz)>1.)alp(iz)=0.99999;if(alp(iz)<0.)alp(iz)=0.00001;enddo;end subroutine ChangeL1L2forVarPr;subroutine CvarsL1L2for
     lVarPr(itt,x,iVarDop, m1,m2,n1,n2,yi1,yi2,ix1,ix2,p1,p2, nz,nf,alp,nc,cf,b1t,nfn0,
     +jp1,jpb1,fi,chw,
     +gst,fm1,pf1,polka,mget,avg,
     +st0);use CiFort;integer(4) itt,iVarDop(*),m1,m2,n1,n2,ix1(0:*),ix2(0:*),nz,nf(*),nc(*),mget(*),nfn0(*);integer(4),target::jp1(
     l0:*),jpb1(0:*);integer(4),pointer::jp2(:),jpb2(:);real(8) x(0:*),yi1(0:n1,0:*),yi2(0:n2,0:*),alp(*),cf(*),fi(0:*),polka(*),avg
     l,b1t(*),st0,p2(*);real(8),target::p1(*),gst(*),fm1(*),pf1(*);real(8),pointer::gst1(:),gst2(:),fm2(:),pf2(:);character(*) chw;i
     lnteger(4) jmax1,jmax2,ipmx(2),ipmn(2),iz,nf1,kup,kle,j1m,j1x,iw,jw,j2m,j2x,j2,j1,i,jmin1,jmin2;real(8) thr0,thr,thr1,thr2,w,sp
     l1,sp2,wsp1,wsp2,ss1,ss2,fj1,fj2,wp1,wp2,fj,avg1,avg2;if(chw=='itg0==10') RETURN;call r8_pointer_set(fm1(m1+1),1,m2,fm2);call r
     l8_pointer_set(pf1(m1+1),1,m2,pf2);call i4_pointer_set(jp1(m1+2),0,m2+1,jp2);call i4_pointer_set(jpb1(m1+2),0,m2+1,jpb2);call F
     lM_One(m1,yi1,p1,n1,x,ix1, jmax1,jmin1,avg1,fm1,pf1);call FM_One(m2,yi2,p2,n2,x,ix2, jmax2,jmin2,avg2,fm2,pf2);select case(itt)
      case(501,503,511,513,540,542,550,552); avg2=-avg2; fm2=-fm2; pf2=-pf2; i=jmax2; jmax2=jmin2; jmin2=i;endselect;select case(itt
     l);case(501,503,551,553,510,512,540,542);avg1=-avg1; fm1(1:m1)=-fm1(1:m1); pf1(1:m1)=-pf1(1:m1); i=jmax1; jmax1=jmin1; jmin1=i;
      endselect;avg=avg1+avg2;do iz=1,nz; nf1=nf(iz); if(nf1<0) Cycle;gst1=>gst((iz-1)*(m1+m2)+1:iz*(m1+m2));gst2=>gst1(m1+1:);thr0=
     lx(iVarDop(nfn0(iz))); thr=thr0;select case(nf1); case(150,170,151,171); thr=thr0+avg; end select;thr1=thr-fm2(jmax2); thr2=thr
     l-fm1(jmax1);call Sorting(0,m1+1,fm1,jp1,jpb1,  thr1,1,1,  kup, kle, ipmx, ipmn );j1m=ipmn(1); j1x=ipmx(1);if(j1x>0) call CVaR_
     l2(m1,p1,1.,fm1,jp1,jpb1,jpb1(j1x),jp1(j1m),1.,
     +iw,jw,w,sp1);call Sorting(0,m2+1,fm2,jp2,jpb2,  thr2,1,1,  kup, kle, ipmx, ipmn );j2m=ipmn(1); j2x=ipmx(1);if(j2x>0) call CVaR
     l_2(m2,p2,1.,fm2,jp2,jpb2,jpb2(j2x),jp2(j2m),1.,
     +iw,jw,w,sp2);wsp2=0.; j2=j2x; ss1=0.; sp1=0.; j1=j1m;do while(j1/=0); w=fm1(j1);if(j2<=m2.and.j2>0)then;   do while(w+fm2(j2)>
     lthr); wsp2=wsp2+p2(j2); j2=jp2(j2); if(j2>m2)Exit;enddo;            endif;ss1=ss1+pf1(j1)*wsp2; gst1(j1)=p1(j1)*wsp2; sp1=sp1+
     lgst1(j1); j1=jpb1(j1);enddo;wsp1=0.; j1=j1x; ss2=0.; sp2=0.; j2=j2m;do while(j2/=0); w=fm2(j2);if(j1<=m1.and.j1>0)then;   do w
     lhile(w+fm1(j1)>thr); wsp1=wsp1+p1(j1); j1=jp1(j1); if(j1>m1)Exit;enddo;            endif;ss2=ss2+pf2(j2)*wsp1; gst2(j2)=p2(j2)
     l*wsp1; j2=jpb2(j2);enddo;i=4*(iz-1);mget(i+1)=j1m; mget(i+2)=j2m;wp1=1.-(alp(iz)+b1t(iz));fj1=thr0*(1.-wp1)+(ss1+ss2-thr*sp1);
      st0=(1.-wp1)-sp1;thr=thr+(fm2(jmax2)+fm1(jmax1)-thr)/(alp(iz)+b1t(iz))*alp(iz);thr1=thr-fm2(jmax2); thr2=thr-fm1(jmax1);call S
     lorting(0,m1+1,fm1,jp1,jpb1,  thr1,1,1,  kup, kle, ipmx, ipmn );j1m=ipmn(1); j1x=ipmx(1);call Sorting(0,m2+1,fm2,jp2,jpb2,  thr
     l2,1,1,  kup, kle, ipmx, ipmn );j2m=ipmn(1); j2x=ipmx(1);wsp2=0.; j2=j2x; ss1=0.; j1=j1m;do while(j1/=0); w=fm1(j1);if(j2<=m2.a
     lnd.j2>0)then;   do while(w+fm2(j2)>thr); wsp2=wsp2+p2(j2); j2=jp2(j2); if(j2>m2)Exit;enddo;            endif;ss1=ss1+pf1(j1)*w
     lsp2; w=p1(j1)*wsp2;gst1(j1)=gst1(j1)-w               ; j1=jpb1(j1);enddo;wsp1=0.; j1=j1x; ss2=0.; sp2=0.; j2=j2m;do while(j2/=
     l0); w=fm2(j2);if(j1<=m1.and.j1>0)then;   do while(w+fm1(j1)>thr); wsp1=wsp1+p1(j1); j1=jp1(j1); if(j1>m1)Exit;enddo;          
        endif;ss2=ss2+pf2(j2)*wsp1; w=p2(j2)*wsp1; sp2=sp2+w;gst2(j2)=gst2(j2)-w               ; j2=jpb2(j2);enddo;wp2=1.-sp2;fj2=ss
     l1+ss2;select case(nf1); case(150,170,151,171); fj2=fj2-avg*sp2; end select;w=sp1-sp2;call copybuff(loc(w),8,loc(mget(i+3)),8);
      polka(nfn0(iz))=wp2-wp1;fj=(fj1-fj2)/(wp2-wp1);fi(nc(iz))=fi(nc(iz))+ cf(iz)*fj;enddo;end subroutine CvarsL1L2forVarPr;subrout
     line GradCvarsL1L2forVarPr(itt,iVarDop,n12, m1,m2,n1,n2,yi1,yi2,ix1,ix2, nz,nf,    nc,cf,jpb1, chw, fw,st0,nfn0,
     +gst,polka,mget,isg,
     +g);use CiFort;integer(4) itt,iVarDop(*),n12,m1,m2,n1,n2,ix1(0:*),ix2(0:*),nz,nf(*),nc(*),isg(0:*),mget(*),nfn0(*);integer(4),t
     larget:: jpb1(0:*);integer(4),pointer::jpb2(:);real(8) yi1(0:n1,0:*),yi2(0:n2,0:*),    cf(*),polka(*),g(0:n12,0:*),fw(*),st0;re
     lal(8),target::gst(*);real(8),pointer::gst1(:),gst2(:);character(*) chw;integer(4) iz,nf1,j1,i,j,j1m,j2m,jt;real(8) wg,dsp,plk,
     lw1,w2;if(chw=='itg0==10') RETURN;call i4_pointer_set(jpb1(m1+2),0,m2+1,jpb2);do iz=nz,1,-1; nf1=nf(iz); if(nf1<0)Cycle;if(nc(i
     lz)==0.or.isg(nc(iz))/=0) then; j1=isg(nc(iz));wg=cf(iz); if(j1/=0) wg=wg/fw(nc(iz));if(j1<0) then; j1=-j1; wg=-wg; endif;gst1=
     l>gst((iz-1)*(m1+m2)+1:iz*(m1+m2));gst2=>gst1(m1+1:);w1=wg; w2=wg;select case(itt); case(501,503,551,553,510,512,540,542); w1=-
     lwg; endselect;select case(itt); case(501,503,511,513,540,542,550,552); w2=-wg; endselect;i=4*(iz-1);j1m=mget(i+1); j2m=mget(i+
     l2);call copybuff(loc(mget(i+3)),8,loc(dsp),8);plk=abs(polka(nfn0(iz)));jt=j1m; w1=w1/plk;do while(jt/=0); do i=0,n1; j=ix1(i);
       g(j,j1)=g(j,j1)+ w1*gst1(jt)*yi1(i,jt); enddo; jt=jpb1(jt);enddo;jt=j2m; w2=w2/plk;do while(jt/=0); do i=0,n2; j=ix2(i); g(j,
     lj1)=g(j,j1)+ w2*gst2(jt)*yi2(i,jt); enddo; jt=jpb2(jt);enddo;select case(nf1); case(150,170,151,171);do i=0,n1; j=ix1(i); g(j,
     lj1)=g(j,j1)- w1*dsp*yi1(i,0); enddo;do i=0,n1; j=ix1(i); g(j,j1)=g(j,j1)- w2*dsp*yi2(i,0); enddo;endselect;j=iVarDop(nfn0(iz))
      g(j,j1)=g(j,j1)+ wg*st0/plk;endif;enddo;
      END subroutine GradCvarsL1L2forVarPr
