      subroutine GRAD_ALL(chw,itt,m,yi,p,n1,n,ix,x,jp,jpb,mget,
     +nz,nf,wf,nc,cf,g,  isg,kzp,polka,klast,iVarDop,jmax,jmin,avg,st0,gst0, fw);integer(4) itt,m,n1,n,jmax,jmin,nz,ix(0:*),jp(0:*),
     ljpb(0:*),nf(*),nc(*),klast(*),iVarDop(*),
     +isg(0:*),mget(*),kzp;real(8) yi(0:n,0:*),p(*),wf(*),cf(*),g(0:n1,0:*),polka(*),avg,st0,fw(*),x(0:*);real(8), target:: gst0(0:*
     l);character(*) chw;real(8),pointer::gst(:);real(8)  w, w1,spl,st1,ww;real(8),allocatable:: spy(:),spf(:),spfs(:);integer(4) ib
     lench,i,j,k,iz,ngap,kf,kpl,iw, igap,iw1,iw2,ngap1, if281,jf,j0,i400, niz(nz),i2,kiz,ir,klst;logical sp_out;call SpMatrixAddrs(y
     li,yi,m,n, sp_out,i);i=0;do iz=1,nz; select case(abs(nf(iz))); case(110:131); if(i==0)i=1; case(420); i=2; endselect;enddo;i=(n
     l+1)*i; gst=>gst0(i:i+m);call findBench(ix,n, ibench);if281=0;do iz=1,nz; if(nf(iz)<0) Cycle;if(nc(iz)/=0.and.isg(nc(iz))==0) C
     lycle; j=isg(nc(iz)); j0=j;w=cf(iz); if(j/=0) w=w/fw(nc(iz));if(j<0) then; j=-j; w=-w; endif;select case(nf(iz));case(20:81, 36
     l1,370:371, 400:411, 830,840, 1230:1241, 1340, 1360:1371);  if281=if281+1;case(10);     do i=0,n; g(ix(i),j)=g(ix(i),j)+w*yi(i,
     l0); enddo;case(0,1,11); do i=0,n; g(ix(i),j)=g(ix(i),j)-w*yi(i,0); enddo;case(1250,1260); if281=if281+1;g(iVarDop(iz),j)=g(iVa
     lrDop(iz),j)-w*polka(iz);case(1251,1261); if281=if281+1;g(iVarDop(iz),j)=g(iVarDop(iz),j)+w*(1.-polka(iz));case(90,841,1330);if
     l(sp_out)then; call SpM_GradAddRow(jmax,w,ix,  g(0,j));else; do i=0,n; g(ix(i),j)=g(ix(i),j)+w*yi(i,jmax); enddo;endif;case(91)
     l;if(sp_out)then; call SpM_GradAddRow(jmin, -w,ix,  g(0,j));else; do i=0,n; g(ix(i),j)=g(ix(i),j)-w*yi(i,jmin); enddo;endif;cas
     le(390);    if(jmax<=ibench)then; i=jmax-1; else; i=jmax; endif; g(ix(i),j)=g(ix(i),j)-w*yi(i,1);case(391);    if(jmin<=ibench)
     lthen; i=jmin-1; else; i=jmin; endif; g(ix(i),j)=g(ix(i),j)+w*yi(i,1);case(1390);   if(jmax<=ibench)then; i=jmax-1; else; i=jma
     lx; endif;if(yi(i,1)<0.and.x(ix(i))>0)then; g(ix(i),j)=g(ix(i),j)-w*yi(i,1); else; g(ix(i),j)=g(ix(i),j)+w*yi(i,1); endif;case(
     l100);if(sp_out)then; do i=0,n; g(ix(i),j)=g(ix(i),j)-w*yi(i,0); enddo;call SpM_GradAddRow(jmax, w,ix,  g(0,j));else; do i=0,n;
       g(ix(i),j)=g(ix(i),j)+w*(yi(i,jmax)-yi(i,0)); enddo;endif;case(101);if(sp_out)then; do i=0,n; g(ix(i),j)=g(ix(i),j)+w*yi(i,0)
     l; enddo;call SpM_GradAddRow(jmin, -w,ix,  g(0,j));else; do i=0,n; g(ix(i),j)=g(ix(i),j)+w*(-yi(i,jmin)+yi(i,0)); enddo;endif;c
     lase(110:131,420:421);select case (nf(iz));case(110); w1=st0+avg*avg; if(w1<=0d0) goto 20;  w=w/dsqrt(w1);do i=0,n; g(ix(i),j)=
     lg(ix(i),j)+w*(gst0(i)+avg*yi(i,0)); enddo;case(111); w=w*2d0;do i=0,n; g(ix(i),j)=g(ix(i),j)+w*(gst0(i)+avg*yi(i,0)); enddo;ca
     lse(120); if(st0==0d0) goto 20; w=w/dsqrt(st0);do i=0,n; g(ix(i),j)=g(ix(i),j)+w*gst0(i); enddo;case(121); w=w*2d0;do i=0,n; g(
     lix(i),j)=g(ix(i),j)+w*gst0(i); enddo;case(420); w=w*2d0; iw=n+1;do i=0,n; g(ix(i),j)=g(ix(i),j)+w*gst0(i+iw); enddo;case(421);
       st1=gst0(ibench); if(st1==0d0) goto 20;w=w/dsqrt(st1); iw=n+1;do i=0,n; g(ix(i),j)=g(ix(i),j)+w*gst0(i+iw); enddo;case(130); 
      if(st0==0d0) goto 20; w1=1d0/dsqrt(st0);do i=0,n; g(ix(i),j)=g(ix(i),j)+w*(gst0(i)*w1+yi(i,0)); enddo;case(131); if(st0==0d0) 
     lgoto 20; w1=1d0/dsqrt(st0);do i=0,n; g(ix(i),j)=g(ix(i),j)+w*(gst0(i)*w1-yi(i,0)); enddo;end select;case(200,1400);jf=jmax; w1
     l=-w;if(jf<=0)then; jf=jmin; if(j0>=0) w1=-w1; endif;if(sp_out)then; call SpM_GradAddRow(jf, w1,ix,  g(0,j));else; do i=0,n; g(
     lix(i),j)=g(ix(i),j)+w1*yi(i,jf); enddo;endif;case(201);jf=jmax; w1=w;if(jf<=0)then; jf=jmin; if(j0>=0) w1=-w1; endif;call Mult
     liQuadrGrad(ix,x,sp_out,jf,m,n,n1,mget,w1,yi,   g(0,j));case(270); do i=0,n; g(ix(i),j)=g(ix(i),j)-w*yi(i,1)*gst0(i); enddo;cas
     le(271); do i=0,n; g(ix(i),j)=g(ix(i),j)+w*gst0(i); enddo;case(430); do i=0,n; g(ix(i),j)=g(ix(i),j)-w*yi(i,1)/x(ix(i)); enddo;
      case(770); do i=0,n; g(ix(i),j)=g(ix(i),j)+w*yi(i,klast(iz)); enddo;case(820); i2=jp(0); w=w/(1d0-wf(iz)); ww=0.; kiz=klast(iz
     l); ir=kiz; if(ir<=ibench) ir=ir-1;if(kiz<n+1)then; ww=yi(ir,1); else; ww=+1e15; endif;do while(i2/=klast(iz)); ir=i2; if(ir<=i
     lbench) ir=ir-1; i2=jp(i2);g(ix(ir),j)=g(ix(ir),j)+w*(-yi(ir,1)+ww);enddo;case(821); i2=jpb(n+1); w=w/wf(iz); ww=0.; kiz=klast(
     liz); ir=kiz; if(ir<=ibench) ir=ir-1;if(kiz>0)then; ww=yi(kiz,1); else; ww=+1e15; endif;do while(i2/=klast(iz)); ir=i2; if(ir<=
     libench) ir=ir-1; i2=jpb(i2);g(ix(ir),j)=g(ix(ir),j)+w*(yi(ir,1)-ww);enddo;case(1070);do i=0,n; g(ix(i),j)=g(ix(i),j)-yi(i,0)*w
     l*gst0(i); enddo;case(1071);do i=0,n; if(ix(i)==0)Cycle;if(sp_out)then; call SpM_ColVect(m,i,gst0(1),w1); else; w1=dot_product(
     lyi(i,1:m),gst0(1:m)); endif;g(ix(i),j)=g(ix(i),j)+w*w1;enddo;end select
20    enddo;if(if281.eq.0) GOTO 150;if(chw=='itg0==10' ) RETURN;kpl=0; i400=0;do iz=1,nz;  if(.not.(nc(iz)==0.or.isg(nc(iz))/=0))Cyc
     lle; ir=nf(iz); klst=klast(iz);selectcase(ir); case(20:81, 361,370:371, 400:411, 830,840, 1230:1241, 1250:1261, 1340, 1360:1371
     l);select case(ir); case(1360:1371);if((ir==1360.or.ir==1370).and.klst==0)Cycle;if((ir==1361.or.ir==1371).and.klst==m)Cycle;if(
     lir==1370.and.klst==m)Cycle;if(ir==1371.and.klst==0)Cycle;if(ir==1360.and.klst==m .or. ir==1361.and.klst==0)then;w=cf(iz); j=is
     lg(nc(iz)); if(j<0)then; j=-j; w=-w; endif;if(j/=0) w=w/fw(nc(iz));if(ir==1361)w=-w;do i=0,n; g(ix(i),j)=g(ix(i),j)+w*yi(i,0); 
      enddo;Cycle;endif;end select;kpl=kpl+1; j=kpl; klst=klast(iz); w1=polka(iz);do while(j.gt.1); if(klst.ge.klast(j-1)) Exit;klas
     lt(j)=klast(j-1); polka(j)=polka(j-1); niz(j)=niz(j-1);j=j-1;enddo;klast(j)=klst; niz(j)=iz; polka(j)=w1; if(400<=ir.and.ir<=41
     l1) i400=1;endselect;enddo;if(kpl==0) GOTO 150;igap=0; iw2=0;do i=1,kpl;  iw1=klast(i);iw=iw1-iw2; iw2=iw1;if(iw >= igap) then;
      igap=iw; ngap=i;endif;enddo;if(m-klast(kpl)>=igap) ngap=kpl+1;if(itt==114)           ngap=kpl+1;if(itt==24)            ngap=kp
     ll+1;if(itt==202.and.yi(ibench,0)<huge(w)/128)then; ngap=int(yi(ibench,0));else;if(i400>0)then; allocate(spfs(0:n)); spfs=0d0;d
     lo j=1,m; w=gst(j);if(sp_out)then; call SpM_AddRow(j, w,  spfs);else; do i=0,n; spfs(i)=spfs(i)+w*yi(i,j); enddo;endif;enddo;en
     ldif;endif;ngap1=ngap-1;allocate(spy(0:n)); if(i400>0)then; allocate(spf(0:n)); spf(0:n)=0.; endif;if(1<=ngap1) then;spy=0d0; k
     l=1; kf=0; if(i400>0)then; spf(0:n)=0d0; spl=0d0; endif;iw=klast(k); j=jp(0);do while(j <= m+1)
100   if(kf.eq.iw) then; jf=j;if(itt==114)then; if(j<=ibench)then; i=j-1; else; i=j; endif;if( -yi(i,1)*x(ix(i))<0.) polka(k)=-polka
     l(k);endif;CALL add_grad(sp_out,avg,ibench,k,nz,niz,nf,wf,ngap,n,m,spy,  spf,spfs,spl,
     +kzp,polka,jf,yi,cf,ix,isg ,g,nc,n1,fw);k=k+1; if(k.gt.ngap1) EXIT;iw=klast(k);goto 100;endif;if(itt==14.or.itt==114)then; if(j
     l<=ibench)then; i=j-1; else; i=j; endif;spy(i)=-yi(i,1)/n; if(itt==114)then; if( sign(1.,spy(i))/=sign(1.,x(ix(i))))spy(i)=-spy
     l(i); endif;else;if(sp_out)then; call SpM_AddRow(j, p(j),  spy);else;  do i=0,n; spy(i)=spy(i)+p(j)*yi(i,j); enddo;endif;if(i40
     l0>0)then;if(sp_out)then; call SpM_AddRow(j, gst(j),  spf);else;  do i=0,n; spf(i)=spf(i)+gst(j)*yi(i,j); enddo;endif;spl=spl+g
     lst(j);endif;endif;j=jp(j); kf=kf+1;enddo;endif;if(kpl>=ngap) then;spy=0d0; k=kpl; kf=0; if(i400>0)then; spf(0:n)=0d0; spl=0d0;
       endif;iw=m-klast(k); j=jpb(m+1);do while(j >=0)
200   if(kf.eq.iw) then; jf=jp(j);CALL add_grad(sp_out,avg,ibench,k,nz,niz,nf,wf,ngap,n,m,spy,  spf,spfs,spl,
     +kzp,polka,jf,yi,cf,ix,isg ,g,nc,n1,fw);k=k-1; if(k.lt.ngap) EXIT;iw=m-klast(k);goto 200;endif;if(itt==14.or.itt==114)then; if(
     lj<=ibench)then; i=j-1; else; i=j; endif;spy(i)=-yi(i,1)/n; if(itt==114)then; if(sign(1.,spy(i))/=sign(1.,x(ix(i)))) spy(i)=-sp
     ly(i); endif;else;if(sp_out)then; call SpM_AddRow(j,p(j),  spy);else;  do i=0,n; spy(i)=spy(i)+p(j)*yi(i,j); enddo;endif;if(i40
     l0>0)then;if(sp_out)then; call SpM_AddRow(j,gst(j),  spf);else;  do i=0,n; spf(i)=spf(i)+gst(j)*yi(i,j); enddo;endif;spl=spl+gs
     lt(j);endif;endif;j=jpb(j); kf=kf+1;enddo;endif;deallocate(spy)
150   continue;if(allocated(spf))deallocate(spf,  stat=i);if(allocated(spfs))deallocate(spfs,  stat=i);return;end subroutine GRAD_AL
     lL;SUBROUTINE findBench(ix,n, ibench);integer(4) ix(0:*),n, ibench;do ibench=0,n; if(ix(ibench)==0)Return; enddo;end;subroutine
     l CalcScenGrads(chyi,chw,m,n,ix,x,jp,jpb,nz,nf,nc,isg,klast,jmax,jmin,p,avg,pf,iscen0,nscen,     gst0,yi);use CiFort; use modco
     lmmons;integer(4) m,n,jmax,jmin,nz,ix(0:*),jp(0:*),jpb(0:*),nf(*),nc(*),klast(*),isg(0:*),nscen;real(8) avg,yi(0:n,0:*),x(0:*),
     lp(*),gst0(0:*),pf(*);character(*) chyi,chw;integer(1) iscen0(m);real(8),allocatable:: xw(:),grds(:); real(8),pointer::px8(:);i
     lnteger(4),allocatable:: iscen(:);integer(4) i,ii,j,j1,iz,if281,if113,iall,kmax,kmin,ibench,jmm,ngap;real(8) w;kmin=0; kmax=0;i
     lf(chw=='itg0==10' ) RETURN;allocate(iscen(0:m+1));iscen(1:m)=iscen0(1:m);iall=0; if281=0; if113=0; jmm=0;do iz=1,nz; if(nf(iz)
     l<0) Cycle;if(nc(iz)/=0.and.isg(nc(iz))==0) Cycle;select case(nf(iz));case(10,11);     iall=1; Exit;case(20:21,70:71,400:401,13
     l60:1361); if281=if281+1;case(30:61,80:81); iall=1; Exit;case(90); iscen(jmax)=1; jmm=1;case(91); iscen(jmin)=1; jmm=1;case(100
     l,101); iall=1; Exit;case(110:131); iall=1; if113=1; Exit;case(410:411); iall=1; Exit;case(1230:1241); iall=1; Exit;case(1370:1
     l371); iall=1; Exit;end select;enddo;if(nscen==m) iall=1;ngap=1;if(iall==1) goto 150;if(if281==0) goto 150;kmax=-1; kmin=m+1;do
     l iz=1,nz; if(.not.(nc(iz)==0.or.isg(nc(iz))/=0))Cycle;selectcase(nf(iz));case(20,70,400,1360); if(klast(iz)>kmax)then; kmax=kl
     last(iz); ngap=ngap+1; endif;case(21,71,401,1361); if(klast(iz)<kmin) kmin=klast(iz);endselect;enddo;if(kmin-1<=kmax+1) iall=1
150   continue;if(iall==1)then; iscen=1; nscen=m;elseif(if281>0)then;if(kmax>-1)then; j=jp(0); do i=1,kmax+1; iscen(j)=1; j=jp(j); e
     lnddo;endif;if(kmin<=m)then; j=jpb(m+1); do i=m,kmin,-1; iscen(j)=1; j=jpb(j); enddo;endif;endif;if(iall==1.or.if281>0.or.jmm==
     l1.or.nscen>0)then;i=0;do j=1,m; if(iscen(j)==1)then; i=i+1; iscen(i)=j; endif; enddo;nscen=i;endif;if(nscen>0)then;allocate(xw
     l(n),grds(n*nscen)); iz=0;do i=0,n; ii=ix(i); if(ii==0)Cycle; iz=iz+1; xw(iz)=x(ii); enddo;i=index(chyi,char(9));  nullify(px8)
      ii=int(RunGradientExternalFunctionDirEx(trim(chyi(:i-1))//char(0),chyi(i+1:),n,xw, nscen,iscen(1), px8,grds, pUserData));if(ii
     l<0)then; chw='External function '//trim(chyi(:i-1))//' has not returned gradient';call putmess('S',5503,'External gradients ca
     llculation',chw); goto 79999;endif;do j1=0,nscen-1; j=iscen(j1+1); ii=0;do i=0,n; if(ix(i)==0)Cycle; ii=ii+1;yi(i,j)=grds(j1*n+
     lii);enddo;enddo;endif;call findBench(ix,n, ibench);yi(:,0)=0.;if(nscen==m)then;do j=1,m; w=p(j);do i=0,n; if(ix(i)==0) Cycle; 
      yi(i,0)=yi(i,0)+w*yi(i,j); enddo;enddo;yi(ibench,0)=+huge(w)/128;else;yi(ibench,0)=ngap;endif;if(if113==1)then;do i=0,n; gst0(
     li)=-avg*yi(i,0); do j=1,m; gst0(i)=gst0(i)+pf(j)*yi(i,j); enddo; enddo;endif
79999 deallocate(iscen);if(allocated(xw))deallocate(xw,grds);return;end subroutine CalcScenGrads;SUBROUTINE ADD_GRAD(sp_out,avg,iben
     lch,k,nz,niz,nf,wf,ngap,n,m,spy,   spf,spfs,spl,
     +kzp,plk,jf,yi,cf,       ix, isg,g,nc,n1,fw);use modcommons;integer(4) k,nz,niz(nz),nf(nz),ngap,jf,n,m,ix(0:n),nc(nz),n1,kzp;re
     lal(8)    wf(nz),spy(0:n),spf(0:n),spfs(0:n),spl, plk(*),yi(0:n,0:m),cf(nz),    g(0:n1,0:*),avg;integer(4) iz,nf1,j,i, ibench;r
     leal(8)    w,w1,sp,dsp,wm, fw(*);integer(4) isg(0:*);logical sp_out;iz=niz(k); nf1=nf(iz);j=isg(nc(iz)); w=cf(iz); if(j/=0) w=w
     l/fw(nc(iz));if(j<0) then; j=-j; w=-w; endif;n=min0(n,14+7);select case(nf1);case(40:81,1230:1241,1250:1261);if( k < ngap) then
      sp=plk(k); wm=wf(iz);else; wm=1d0-wf(iz);sp=1d0-plk(k); w=-w;nf1=nf1+1-2*mod(nf1,10);endif;select case(nf1);case(40,41);do i=0
     l,n; g(ix(i),j)=g(ix(i),j)+w*( 2d0*spy(i)-yi(i,0));enddo;case(50,51);do i=0,n; g(ix(i),j)=g(ix(i),j)+w*2d0*( spy(i)-yi(i,0)*sp)
      enddo;case(60);do i=0,n; g(ix(i),j)=g(ix(i),j)+w*(2d0*( spy(i)-yi(i,0)*sp)+yi(i,0));enddo;case(61);do i=0,n; g(ix(i),j)=g(ix(i
     l),j)+w*(2d0*( spy(i)-yi(i,0)*sp)-yi(i,0));enddo;case(70,1250);do i=0,n; g(ix(i),j)=g(ix(i),j)+w*spy(i);enddo;case(71,1251);do 
     li=0,n; g(ix(i),j)=g(ix(i),j)+w*(spy(i)-yi(i,0));enddo;case(80,81,1260,1261);do i=0,n; g(ix(i),j)=g(ix(i),j)+w*( spy(i)-yi(i,0)
     l*sp);enddo;case(1230,1231); w1=1.-wm;do i=0,n; g(ix(i),j)=g(ix(i),j)+w*(spy(i)-w1*yi(i,0));enddo;case(1240,1241);do i=0,n; g(i
     lx(i),j)=g(ix(i),j)+w*(spy(i)-yi(i,0)*sp);enddo;endselect;case(400:411);if(k<ngap)then; sp=plk(k);else; sp=1d0-plk(k); nf1=nf1+
     l1-2*mod(nf1,10);endif;wm=wf(iz); w=2d0*w;select case(nf1);case(400);do i=0,n; g(ix(i),j)=g(ix(i),j)+w*( spf(i)-wm*spy(i) );end
     ldo;case(401);do i=0,n; g(ix(i),j)=g(ix(i),j)+w*( (spfs(i)-spf(i))-wm*(yi(i,0)-spy(i)) );enddo;case(410);do i=0,n; g(ix(i),j)=g
     l(ix(i),j)+w*( spf(i)-(wm+avg)*(spy(i)-yi(i,0)*sp)-spl*yi(i,0) );enddo;case(411);do i=0,n; g(ix(i),j)=g(ix(i),j)+w*( (spfs(i)-s
     lpf(i))-(wm+avg)*(-spy(i)+yi(i,0)*sp)-(avg-spl)*yi(i,0) );enddo;end select;case(20:31,361,370:371,830,840,1340,1360:1371);if ( 
     lk < ngap ) then;dsp=plk(k); wm=wf(iz);else;w=-w; dsp=-plk(k); wm=1d0-wf(iz);nf1=nf1+1-2*mod(nf1,10);endif;select case(nf1);cas
     le(20,830,840,1340,1360); if(nf1/=1360)then; w=w/(1.-wm); else; w=w/plk(iz+kzp); endif;if(sp_out)then; do i=0,n; g(ix(i),j)=g(i
     lx(i),j)+w*spy(i); enddo;call SpM_GradAddRow(jf, w*dsp,ix,  g(0,j));else;   do i=0,n; g(ix(i),j)=g(ix(i),j)+w*(spy(i)+dsp*yi(i,
     ljf)); enddo;endif;case(370); w = w/(1.-wm);do i=0,n; g(ix(i),j)=g(ix(i),j)+w*spy(i); enddo;if(jf<=ibench)then; i=jf-1; else; i
     l=jf; endif;  g(ix(i),j)=g(ix(i),j)-w*dsp*yi(i,1);case(361); w = w/(1.-wm);do i=0,n; g(ix(i),j)=g(ix(i),j)+w*spy(i); enddo;if(j
     lf<=ibench)then; i=jf-1; else; i=jf; endif;  g(ix(i),j)=g(ix(i),j)-w*dsp*yi(i,1);case(30,1370); w1=1.-wm; if(nf1/=1370)then; w=
     lw/w1; else; w=w/plk(iz+kzp); endif;if(sp_out)then; do i=0,n; g(ix(i),j)=g(ix(i),j)+w*(spy(i)-yi(i,0)*w1); enddo;call SpM_GradA
     lddRow(jf, w*dsp,ix,  g(0,j));else;do i=0,n; g(ix(i),j)=g(ix(i),j)+w*(spy(i)+dsp*yi(i,jf)-yi(i,0)*w1); enddo;endif;case(21,1361
     l); if(nf1/=1361)then; w=w/wm; else; w=w/plk(iz+kzp); endif;if(sp_out)then; do i=0,n; g(ix(i),j)=g(ix(i),j)+w*(spy(i)-yi(i,0));
       enddo;call SpM_GradAddRow(jf, w*dsp,ix,  g(0,j));else;   do i=0,n; g(ix(i),j)=g(ix(i),j)+w*(spy(i)+dsp*yi(i,jf)-yi(i,0)); end
     ldo;endif;case(371); w = w/wm;do i=0,n; g(ix(i),j)=g(ix(i),j)+w*spy(i); enddo;if(jf<=ibench)then; i=jf-1; else; i=jf; endif;  g
     l(ix(i),j)=g(ix(i),j)-w*(dsp*yi(i,1)-yi(i,0));case(31,1371); w1=1.-wm; if(nf1/=1371)then; w=w/wm; else; w=w/plk(iz+kzp); endif;
      if(sp_out)then; do i=0,n; g(ix(i),j)=g(ix(i),j)+w*(spy(i)-yi(i,0)*w1); enddo;call SpM_GradAddRow(jf, w*dsp,ix,  g(0,j));else;d
     lo i=0,n; g(ix(i),j)=g(ix(i),j)+w*(spy(i)+dsp*yi(i,jf)-yi(i,0)*w1); enddo;endif;end select;end select;RETURN;END SUBROUTINE ADD
     l_GRAD;subroutine Eut_Grad_(m,yi,n1,n,x,ix,nz,nf,wf,nc,cf,p, isg,fw,gst,chw,
     +g,
     +fm,pf);integer(4) m,n1,n,ix(0:n), nz, nf(nz),nc(nz); character(*) chw;real(8) yi(0:n,0:m),p(m),wf(nz),cf(nz), x(0:n1),fw(*),g(
     l0:n1,0:*), fm(*), pf(*), gst(0:*);integer(4) isg(0:*);real(8) xw(0:n), w,w1,fj,wt,w2,xlinear; integer(4) i,j,iz,j1;logical sp_
     lout;if(chw=='itg0==10') RETURN;fj=xLinear(n,x,yi,ix,xw);call SpMatrixAddrs(yi,yi,m,n, sp_out,i);if(nz>1.or.nf(1)/=351)then;if(
     lsp_out)then;do j=1,m; call SpM_RowVect(m,n,j,xw,  fj); fm(j)=fj; enddo;else;do j=1,m; fj=0d0; do i=0,n; fj=fj+yi(i,j)*xw(i); e
     lnddo;  fm(j)=fj; enddo;endif;endif;do iz=1,nz;if(nc(iz)==0.or.isg(nc(iz))/=0) then; j1=isg(nc(iz));w=cf(iz); if(j1/=0) w=w/fw(
     lnc(iz));if(j1<0) then; j1=-j1; w=-w; endif;wt=wf(iz); w1=wt-1d0; w2=wt*wt;select case(nf(iz));case(340); do j=1,m; pf(j)=wt*p(
     lj)*dexp(wt*fm(j)); enddo;case(350); do j=1,m; pf(j)=-p(j)/fm(j); enddo;case(360); do j=1,m; pf(j)=w2*p(j)*dexp(w1*dlog(-fm(j))
     l); enddo;case(351);do j=1,m; pf(j)=-p(j)*gst(j); enddo;case(3510000); do j=1,m; pf(j)=2d0*p(j)*gst(j); enddo;end select;if(sp_
     lout)then;do i=0,n; call SpM_ColVect(m,i,pf,fj);g(ix(i),j1)=g(ix(i),j1)-w*fj;enddo;else;do i=0,n;g(ix(i),j1)=g(ix(i),j1)-w*dot_
     lproduct(pf(1:m),yi(i,1:m));enddo;endif;endif;enddo;end subroutine Eut_Grad_;subroutine ExtFunc_Grad_(chyi,n1,n,x,ix,nz,nf,nc,c
     lf, isg,fw,chw,
     +g);use ModCommons; use CiFort;integer(4) n1,n,ix(0:n), nz,nf(*), nc(nz);real(8) cf(nz), x(0:n1),fw(*),g(0:n1,0:*);integer(4) i
     lsg(0:*);character(*) chyi,chw;real(8) w; integer(4) i,ii,j,iz,j1;real(8), allocatable:: xw(:),gex(:);if(chw=='itg0==10') RETUR
     lN;do iz=1,nz; if(nf(iz)>0) goto 100; enddo;RETURN
100   allocate(xw(n),gex(n)); iz=0;do i=0,n; j=ix(i); if(j==0)Cycle; iz=iz+1; xw(iz)=x(j); enddo;i=index(chyi,char(9));ii=int(RunGra
     ldientExternalFunctionEx(trim(chyi(:i-1))//char(0), n, chyi(i+1:), xw, gex, pUserData));if(ii<0)then; chw='External function '/
     l/trim(chyi(:i-1))//' has not returned gradient';call putmess('S',5509,'External gradients calculation',chw); goto 200;endif;do
     l iz=1,nz;if(nc(iz)==0.or.isg(nc(iz))/=0) then; j1=isg(nc(iz));w=cf(iz); if(j1/=0) w=w/fw(nc(iz));if(j1<0) then; j1=-j1; w=-w; 
      endif;j=0;do i=0,n; ii=ix(i); if(ii==0)Cycle; j=j+1;g(ii,j1)=g(ii,j1)+w*gex(j);enddo;endif;enddo
200   deallocate(xw,gex);return;end subroutine ExtFunc_Grad_;subroutine Prmulti_pen_ni_Grad(m,n,n1,x, yim,yis,ixm,ixs, nz,nf,wf,nc,c
     lf, isg,fw, gst,
     +g,
     +chw);integer(4) m,n,n1,nz,ixm(0:n),ixs(0:n),nf(*),nc(*);real(8) x(0:n1),yim(0:n,0:*),yis(0:n,0:*),wf(*),cf(*); character(*) ch
     lw;real(8) fw(*),g(0:n1,0:*);integer(4) isg(0:*);real(8),target:: gst(*);       real(8), allocatable:: xw(:),p2(:);real(8),poin
     lter:: mu(:),si(:),pd(:);real(8)  w,ww,wg,zw,fj,p1,d1,a2,a1; save a2,a1;data a2/-4.97477633087420d-01/,
     +a1/+1.65844422531015d-01/;integer(4) nfi,i,j,iz,j1;logical sp_out;w=cf(1);if(chw=='itg0==10') RETURN;mu=>gst(:m); si=>gst(m+1:
     l2*m); pd=>gst(2*m+1:3*m); allocate(xw(0:n),p2(m));do i=0,n; j=ixm(i); if(j/=0)then; xw(i)=x(j); else;  xw(i)=-x(j); endif; end
     ldo;do iz=nz,1,-1; nfi=nf(iz); ww=wf(iz); if(nfi<0) Cycle;if(nc(iz)==0.or.isg(nc(iz))/=0) then; j1=isg(nc(iz));wg=1.0; if(j1/=0
     l) wg=wg/fw(nc(iz));if(j1<0) then; j1=-j1; wg=-wg; endif;do j=1,m;select case(nfi);case(450,470); zw=( ww-mu(j))/si(j);   case(
     l451,471); zw=(-ww+mu(j))/si(j);case(460,480); zw= ww/si(j);           case(461,481); zw=-ww/si(j);end select;if(iz<nz)then;if(
     lzw<-5d0)then; w=a2*zw; pd(j)=2.0*w+a1;else; call cdf_nst_(zw, p1,d1); pd(j)=d1/p1;endif;else;endif;pd(j)=pd(j)/si(j); p2(j)=pd
     l(j)*zw/si(j); if(nfi==451.or.nfi==471)pd(j)=-pd(j);enddo;i=nfi/10; i=mod(i,2);if(i==0) goto 100;call SpMatrixAddrs(yim,yim,m,n
     l, sp_out,i);if(sp_out)then;do i=0,n; if(ixm(i)==0)Cycle; call SpM_ColVect(m,i,pd,fj); g(ixm(i),j1)=g(ixm(i),j1)+wg*fj; enddo;e
     llse;do i=0,n; if(ixm(i)==0)Cycle; fj=0d0; do j=1,m; fj=fj+pd(j)*yim(i,j); enddo;g(ixm(i),j1)=g(ixm(i),j1)+wg*fj;enddo;endif
100   call SpMatrixAddrs(yis,yis,m,n, sp_out,i);if(sp_out)then;do i=0,n; if(ixs(i)==0)Cycle; call SpM_ColVect(m,i,p2,fj); g(ixs(i),j
     l1)=g(ixs(i),j1)-wg*fj*xw(i); enddo;else;do i=0,n; if(ixs(i)==0)Cycle; fj=0d0; do j=1,m; fj=fj+p2(j)*yis(i,j); enddo;g(ixs(i),j
     l1)=g(ixs(i),j1)    -    wg*fj*xw(i);enddo;endif;endif;enddo;deallocate(xw,p2);return;end subroutine Prmulti_pen_ni_Grad;subrou
     ltine AvgFunc_ni_Grad(m,n,n1,x, yim,yis,p,ixm,ixs, nz,nf,wf,nc,cf,iVarDop, isg,fw, gst,
     +g,
     +chw);integer(4) m,n,n1,nz,ixm(0:n),ixs(0:n),nf(*),nc(*),iVarDop(*);real(8) x(0:n1),yim(0:n,0:*),yis(0:n,0:*),wf(*),cf(*); char
     lacter(*) chw;real(8) fw(*),g(0:n1,0:*),p(*);integer(4) isg(0:*);real(8),target:: gst(*);       real(8), allocatable:: xw(:),p2
     l(:);real(8),pointer:: mu(:),si(:),pd(:);real(8)  w,ww,wg,zw,fj,p1,d1,wm,wd,sg; real(8),save:: a2,a1,a0;data a2/-4.974776330874
     l20d-01/,
     +a1/+1.65844422531015d-01/,
     +a0/-1.79883545360616d-00/;integer(4) nfi,i,j,iz,j1,kiz;logical sp_out;wd=0.;if(chw=='itg0==10') RETURN;mu=>gst(:m); si=>gst(m+
     l1:2*m); pd=>gst(2*m+1:3*m); allocate(xw(0:n),p2(m));do i=0,n; j=ixm(i); if(j/=0)then; xw(i)=x(j); else;  xw(i)=-x(j); endif; e
     lnddo;do iz=nz,1,-1; nfi=nf(iz); if(nfi<0) Cycle;if(nc(iz)==0.or.isg(nc(iz))/=0) then; j1=isg(nc(iz));wg=cf(iz); if(j1/=0) wg=w
     lg/fw(nc(iz));if(j1<0) then; j1=-j1; wg=-wg; endif;if(chw=='Objects'.and.750<=nfi.and.nfi<=761) nfi=nfi-40;sg=1.; if(mod(nfi,10
     l)==1) sg=-1.; select case(nfi); case(680,681,700,701,740,741,750:761); sg=0.; end select;ww=wf(iz); wm=0.; kiz=iVarDop(iz);sel
     lect case(nfi);case(710:721,730:741); ww=0.; wm=x(kiz); wd=wf(iz);select case(nfi); case(730,740); wd=1.-wd; end select;select 
     lcase(nfi); case(730:741); sg=sg/wd; end select;end select;do j=1,m;select case(nfi);case(670,690,710,730); zw=(ww-mu(j)+wm)/si
     l(j);   case(671,691,711,731); zw=(-ww+mu(j)+wm)/si(j);case(680,700,720,740); zw=(ww+wm)/si(j);         case(681,701,721,741); 
      zw=(-ww+wm)/si(j);end select;select case(nfi);case(670:681);if(iz<nz)then;if(zw<-5d0)then; w=a2*zw; pd(j)=(2.0*w+a1)*dexp(((w+
     la1)*zw+a0));else; call cdf_nst_(zw, p1,d1); pd(j)=d1;endif;endif;pd(j)=p(j)*pd(j)/si(j); p2(j)=pd(j)*zw/si(j);  pd(j)=pd(j)*sg
      case(690:701); call cdf_nst_(zw, p1,w); call pdf_nst_(zw, d1,w);p2(j)=p(j)*d1/si(j); pd(j)=p(j)*(1.-p1)*sg;case(710:721);if(iz
     l<nz)then;if(zw<-5d0)then; w=a2*zw; pd(j)=(2.0*w+a1)*dexp(((w+a1)*zw+a0));else; call cdf_nst_(zw, p1,d1); pd(j)=d1;endif;endif;
      pd(j)=p(j)*pd(j)/si(j); p2(j)=pd(j)*zw/si(j);  pd(j)=pd(j)*sg;case(730:741); call cdf_nst_(zw, p1,w); call pdf_nst_(zw, d1,w);
      p2(j)=p(j)*d1/si(j)/wd; pd(j)=p(j)*(1.-p1)*sg;case(750:761);Exit;end select;enddo;if(sg==0.) goto 100;call SpMatrixAddrs(yim,y
     lim,m,n, sp_out,i);if(sp_out)then;do i=0,n; if(ixm(i)==0)Cycle; call SpM_ColVect(m,i,pd,fj); g(ixm(i),j1)=g(ixm(i),j1)+wg*fj; e
     lnddo;else;do i=0,n; if(ixm(i)==0)Cycle; fj=0d0; do j=1,m; fj=fj+pd(j)*yim(i,j); enddo;g(ixm(i),j1)=g(ixm(i),j1)+wg*fj;enddo;en
     ldif
100   continue;select case(nfi);case(710:721); fj=0d0;do j=1,m; fj=fj-pd(j); enddo;g(kiz,j1)=g(kiz,j1)+wg*fj*dsign(1.,sg);case(750:7
     l61);g(kiz,j1)=g(kiz,j1)+wg*1.;CYCLE;case(730:741); fj=0d0;do j=1,m; fj=fj-pd(j); enddo;g(kiz,j1)=g(kiz,j1)+wg*(1.+fj*dsign(1.,
     lsg));end select;call SpMatrixAddrs(yis,yis,m,n, sp_out,i);if(sp_out)then;do i=0,n; if(ixs(i)==0)Cycle; call SpM_ColVect(m,i,p2
     l,fj); g(ixs(i),j1)=g(ixs(i),j1)-wg*fj*xw(i); enddo;else;do i=0,n; if(ixs(i)==0)Cycle; fj=0d0; do j=1,m; fj=fj+p2(j)*yis(i,j)*x
     lw(i); enddo;g(ixs(i),j1)=g(ixs(i),j1)    -    wg*fj;enddo;endif;if(chw=='Objects'.and.750<=nf(iz).and.nf(iz)<=761)then; w=g(ki
     lz,j1);if(w/=0.)then; do i=0,n; g(ixm(i),j1)=-g(ixm(i),j1)/w; enddo;endif;endif;endif;enddo;deallocate(xw,p2);return;end subrou
     ltine AvgFunc_ni_Grad;subroutine wCvar_ni_Grad(n,n1,x,yim,yis,ix, nz,nf,wf,nc,cf, isg,fw,gst,polka,
     +g,
     +chw);integer(4) n,n1,nz,ix(0:n),nf(*),nc(*);real(8) x(0:*),yim(0:n,0:*),yis(0:n,0:*),wf(*),cf(*),polka(*);real(8) fw(*),g(0:n1
     l,0:*); character(*) chw;integer(4) isg(0:*);real(8),target:: gst(*);real(8),pointer:: pm(:),pr(:);real(8)  w,ww,wg,zw,p1,d1,wm
     l,wp,mu,si;integer(4) nfi,i,j,iz,j1;w=x(0);if(chw=='itg0==10') RETURN;pm=>gst(:n+1); pr=>gst(n+1+1:2*(n+1));do iz=nz,1,-1; nfi=
     lnf(iz); if(nfi<0)Cycle; wp=wf(iz);if(nc(iz)==0.or.isg(nc(iz))/=0) then; j1=isg(nc(iz));wg=cf(iz); if(j1/=0) wg=wg/fw(nc(iz));i
     lf(j1<0) then; j1=-j1; wg=-wg; endif;wm=polka(iz);select case(nfi);case(1350); ww=wg/(1.-wp);if(iz<nz)then;do i=0,n; if(ix(i)==
     l0) Cycle; mu=-yim(i,1); w=-yis(i,1);if(w<=0d0)then; si=1d-7; else; si=dsqrt(w); endif;zw=(-mu+wm)/si;call cdf_nst_(zw, p1,w); 
      call pdf_nst_(zw, d1,w);pr(i+1)=1.-p1; pm(i+1)=(d1-pr(i+1)*zw)*si;enddo;endif;do i=0,n; j=ix(i); g(j,j1)=g(j,j1)+ ww*pm(i+1);e
     lnddo;end select;endif;enddo;end subroutine wCvar_ni_Grad;subroutine AllFunc_nid_Grad(m,n,n1,x, yim,ix, nz,nf,wf,nc,cf, isg,fw,
     l st0,gst0,
     +p,
     +g,
     +chw);integer(4) m,n,n1,nz,ix(0:n),nf(*),nc(*);real(8) x(0:n1),yim(0:n,0:*),wf(*),cf(*),p(*),st0; character(*) chw;real(8) fw(*
     l),g(0:n1,0:*);integer(4) isg(0:*);real(8),target:: gst0(0:*);real(8),pointer:: mu,si;real(8)  w,ww,wg,zw,p1,p2,d1,d2,sg,wm,wp;
      integer(4) nfi,i,j,iz,j1;i=m; w=x(0); w=p(1);if(chw=='itg0==10') RETURN;mu=>gst0(n+1); si=>gst0(n+2);do iz=nz,1,-1; nfi=nf(iz)
     l; wp=wf(iz); if(nfi<0) Cycle;if(nc(iz)==0.or.isg(nc(iz))/=0) then; j1=isg(nc(iz));wg=cf(iz); if(j1/=0) wg=wg/fw(nc(iz));if(j1<
     l0) then; j1=-j1; wg=-wg; endif;sg=1.; if(mod(nfi,10)==1) sg=-1.;    j=nfi/10;select case(j); case(48,50,52,54,56, 59,62,  88,9
     l0,92,94,96, 99, 102  ); sg=0.;end select;if(mod(nfi,10)==1)then;select case(nfi); case(531,541,551,561,  931,941,951,961); wp=
     l1.-wp;case default; wp=-wp;end select;endif;zw=0.; wm=mu*sg;select case(j); case(47:52,58,  87:92,98); zw=(wp-wm)/si; call cdf
     l_nst_(zw, p1,p2); call pdf_nst_(zw, d1,d2);end select;w=0.; ww=0.;select case(j);case(47:48, 87:88);w=p2*zw/si*(wg/si); ww=p2/
     lsi*(wg*sg);case(49:50, 89:90);w=d1*(wg/si); ww=(1.-p1)*(wg*sg);case(51:52, 91:92);w=2.*(1.-p1)*(wg); ww=2.*((d1-(1.-p1)*zw)*si
     l)*(wg*sg);case(53:54, 93:94); call invers_cdf_nst_(wp, zw); call pdf_nst_(zw, d1,d2);w=d1/(1.-wp)*(wg/si); ww=(wg*sg);case(55:
     l56, 95:96); call invers_cdf_nst_(wp,zw);w=zw*(wg/si); ww=(wg*sg);case(57, 97);w=0.; ww=(wg*sg);case(58, 98);w=2.*d1*(wg/si); w
     lw=(1.-2.*p1)*(wg*sg);case(59:60, 99:100);w=0.797884560802866*(wg/si); ww=(wg*sg);case(61,101);  w=2.*(wg); ww=2.*wm*(wg*sg);if
     l(nfi==610.or.nfi==1010)then;  d2=2.*dsqrt(st0+wm*wm);if(d2/=0.)then; w=w/d2; ww=ww/d2; endif;endif;case(62,102);w=(wg/si); ww=
     l0.; if(nfi==621.or.nfi==1021) w=2.*(wg);case(63,103);w=(wg/si); ww=(wg*sg);case(64,104); w=(0.5*wp*st0+wm)*wp; if(w>700.)then;
       d2=huge(d2); elseif(w<-700.)then; d2=0.; else; d2=-dexp(w); endif;w=-wp*wp*d2*(wg); ww=-wp*d2*(wg*sg);end select;if(w==0.)the
     ln; if(ww/=0.)then; do i=0,n; j=ix(i); g(j,j1)=g(j,j1)+ ww*yim(i,1); enddo; endif;else;if(ww==0.)then; do i=0,n; j=ix(i); g(j,j
     l1)=g(j,j1)+ w*gst0(i); enddo;else;           do i=0,n; j=ix(i); g(j,j1)=g(j,j1)+ w*gst0(i) + ww*yim(i,1); enddo;endif;endif;en
     ldif;enddo;end subroutine AllFunc_nid_Grad;subroutine Pr_ND_Grad(nm,m,n,n1, yim,ix, nz,nf,wf,nc, isg,fw, gst,
     +g,
     +chw);integer(4) n,n1,nz,ix(0:n),nf(*),nc(*),nm,m;real(8) yim(0:n,0:*),wf(*), fw(*),g(0:n1,0:*); character(*) chw;integer(4) is
     lg(0:*);real(8),target:: gst(*);real(8),pointer:: mu,si,gst0(:);  integer(4) nfi,i,j,iz,j1;real(8)  w,ww,wg,zw,p1,d1,sg,wp; rea
     ll(8),save:: a2,a1;data a2/-4.97477633087420d-01/,
     +a1/+1.65844422531015d-01/;w=0.; ww=0.;if(chw=='itg0==10') RETURN;mu=>gst(nm-1); si=>gst(m+nm-1); gst0=>gst(2*m+(n+1)*(nm-2)+1:
     l2*m+(n+1)*(nm-1));do iz=nz,1,-1; nfi=nf(iz); wp=wf(iz); if(nfi<0) Cycle;if(nc(iz)==0.or.isg(nc(iz))/=0) then; j1=isg(nc(iz));w
     lg=1d0;if(j1/=0) wg=wg/fw(nc(iz));if(j1<0) then; j1=-j1; wg=-wg; endif;sg=1.; if(mod(nfi,10)==1)then; sg=-1.; wp=-wp; endif;j=n
     lfi/10;  selectcase(j); case(86, 88); sg=0.; endselect;zw=(wp-mu*sg)/si;select case(j);case(85:86, 87:88);if(zw<-5d0)then; w=(2
     l.0*a2*zw+a1)/si*(wg); else; call cdf_nst_(zw, p1,d1); w=d1/p1/si*(wg); endif;ww=w*sg; w=w*zw/si;end select;do i=0,n; j=ix(i); 
      g(j,j1)=g(j,j1)+ w*gst0(i+1) + ww*yim(i,nm-1); enddo;endif;enddo;end subroutine Pr_ND_Grad;subroutine cvar2_Grads(m,n,n1, yi,p
     l,ix, nz,nf,wf,nc,cf, isg,fw, gst,
     +polka,kzp,klast,list,
     +g,
     +chw);integer(4) m,n,n1,nz,kzp,ix(0:*),nf(*),nc(*),list(*),klast(*);real(8) yi(0:n,0:*),wf(*),cf(*); character(*) chw;real(8) f
     lw(*),g(0:n1,0:*),p(*);integer(4) isg(0:*);real(8),target:: gst(*),polka(kzp,*);real(8)  w,w1,wg,wlog;integer(4)  nfi,i,j,iz,j1
     l,kls,j2;logical  sp_out;real(8),pointer :: wgp(:),wgb(:),wgf(:),clf(:);if(chw=='itg0==10') RETURN;wgp=>gst(1:m); wgb=>gst(m+1:
     l2*m);wgf=>polka(:,4); clf=>polka(:,5);call SpMatrixAddrs(yi,yi,m,n, sp_out,i);do iz=1,nz; nfi=nf(iz); if(nfi<0) Cycle;if(nc(iz
     l)/=0.and.isg(nc(iz))==0)Cycle;wg=cf(iz); j1=isg(nc(iz)); if(j1/=0) wg=wg/fw(nc(iz));if(j1<0)then; j1=-j1; wg=-wg; endif;kls=kl
     last(iz);select case(nfi);case(1290,1300,1310); w1=wg/(1.-wf(iz)); if(kls>0) wlog=1.+log(clf(iz));do j2=1,kls; j=list(j2); w=w1
     l*(p(j)*wlog+wgp(j2)); if(j2==kls) w=w1*(polka(iz,1)*wlog+wgf(iz));if(sp_out)then; call SpM_GradAddRow(j, w,ix,  g(0,j1));else;
       do i=0,n; g(ix(i),j1)=g(ix(i),j1)+w*yi(i,j); enddo;endif;enddo;if(nfi/=1300)then; w=wg; do i=0,n; g(ix(i),j1)=g(ix(i),j1)-w*y
     li(i,0); enddo;endif;case(1291,1301,1311); w1=wg/wf(iz); if(kls<=m) wlog=1.+log(clf(iz));do j2=m,kls,-1; j=list(j2); w=w1*(p(j)
     l*wlog+wgb(j2)); if(j2==kls) w=w1*(polka(iz,1)*wlog+wgf(iz));if(sp_out)then; call SpM_GradAddRow(j, -w,ix,  g(0,j1));else; do i
     l=0,n; g(ix(i),j1)=g(ix(i),j1)-w*yi(i,j); enddo;endif;enddo;if(nfi/=1301)then; w=wg; do i=0,n; g(ix(i),j1)=g(ix(i),j1)+w*yi(i,0
     l); enddo;endif;end select;enddo;return;end subroutine cvar2_Grads;subroutine Ro_err_Grad(m,n,n1, yi,ix, nz,nf,nc,cf, isg,fw, g
     lst,
     +klast,list,p,
     +g,
     +chw);integer(4) m,n,n1,nz,ix(0:*),nf(*),nc(*),list(*),klast(*);real(8) yi(0:n,0:*),cf(*),p(*); character(*) chw;real(8) fw(*),
     lg(0:n1,0:*);integer(4) isg(0:*);real(8),target:: gst(*);real(8) w,wg; logical sp_out;integer(4) nfi,i,j,iz,j1,kls,j2;real(8),p
     lointer:: wgp(:);if(chw=='itg0==10') RETURN;wgp=>gst(1:m);call SpMatrixAddrs(yi,yi,m,n, sp_out,i);do iz=1,nz; nfi=nf(iz); if(nf
     li<0) Cycle;if(nc(iz)/=0.and.isg(nc(iz))==0)Cycle;wg=cf(iz); j1=isg(nc(iz)); if(j1/=0) wg=wg/fw(nc(iz));if(j1<0)then; j1=-j1; w
     lg=-wg; endif;kls=klast(1);do j2=1,kls; j=list(j2); w=wg*wgp(j2)*p(j);if(sp_out)then; call SpM_GradAddRow(j, w,ix,  g(0,j1));el
     lse; do i=0,n; g(ix(i),j1)=g(ix(i),j1)+w*yi(i,j); enddo;endif;enddo;w=wg; do i=0,n; g(ix(i),j1)=g(ix(i),j1)-w*yi(i,0); enddo;en
     lddo;return;end subroutine Ro_err_Grad;subroutine GradTSPCuts(n1,n,ix,nz,nc,cf, isg,fw,gst,chw, g);integer(4) n1,n,ix(0:*), nz,
     l nc(*); character(*) chw;real(8) cf(*),fw(*),g(0:n1,0:*),gst(0:*);integer(4) isg(0:*);real(8) w; integer(4) i,iz,j1;if(chw=='i
     ltg0==10') RETURN;do iz=1,nz;if(nc(iz)==0.or.isg(nc(iz))/=0) then; j1=isg(nc(iz));w=cf(iz); if(j1/=0) w=w/fw(nc(iz));if(j1<0) t
     lhen; j1=-j1; w=-w; endif;do i=0,n; g(ix(i),j1)=g(ix(i),j1)+w*gst(i); enddo;endif;enddo;end subroutine GradTSPCuts;subroutine G
     lradKantor(n1,n,ix,nz,nc,cf,isg,fw,gst,chw, g);integer(4) n1,n,ix(0:*), nz, nc(*); character(*) chw;real(8) cf(*),fw(*),g(0:n1,
     l0:*),gst(0:*);integer(4) isg(0:*);real(8) w; integer(4) i,iz,j1;if(chw=='itg0==10') RETURN;do iz=1,nz;if(nc(iz)==0.or.isg(nc(i
     lz))/=0) then; j1=isg(nc(iz));w=cf(iz); if(j1/=0) w=w/fw(nc(iz));if(j1<0) then; j1=-j1; w=-w; endif;do i=0,n; g(ix(i),j1)=g(ix(
     li),j1)+w*gst(i); enddo;endif;enddo;
      end subroutine GradKantor
