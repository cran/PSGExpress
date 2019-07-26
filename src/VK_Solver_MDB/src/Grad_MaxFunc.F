      subroutine GRAD_ALL_abs(chw,itt,m,yi,p,n1,n,ix,x,jp,jpb,    mget,  amatr,nz,nf,wf,nc,cf,g,  isg,polka,klast,iVarDop,jmax,jmin,
     javg,st0,gst0, fw);integer(4) itt,m,n1,n,jmax,jmin,nz,ix(0:*),jp(0:*),jpb(0:*),nf(*),nc(*),klast(*),iVarDop(*),isg(0:*),mget(*)
       real(8) yi(0:n,0:*),p(*),wf(*),cf(*),g(0:n1,0:*),polka(nz),avg,st0,fw(*),x(0:*),amatr;real(8), target:: gst0(0:*); real(8),po
     jinter::gst(:);character(*) chw;real(8)  w, spy(0:n),ys(0:n,0:0),w1,spl,w0; real(8),allocatable:: spf(:),spfs(:);integer(4) ibe
     rnch,i,j,k,iz,ngap,kf,kpl,iw, igap,iw1,iw2,ngap1,kpl1,kpl2, if281,jf,j0,i400, niz(nz);logical sp_out;call SpMatrixAddrs(yi,yi,m
     f,n, sp_out,i);i=0;do iz=1,nz; select case(abs(nf(iz))); case(110:131); if(i==0)i=1; case(420); i=2; endselect;enddo;i=(n+1)*i;
       gst=>gst0(i:i+m);call findBench(ix,n, ibench);if281=0;call ysclc(yi,n,m,mget,p,   ys);do iz=1,nz;if(nc(iz)/=0.and.isg(nc(iz))
     z==0) Cycle; j=isg(nc(iz)); j0=j;w=cf(iz); if(j/=0) w=w/fw(nc(iz));if(j<0) then; j=-j; w=-w; endif;w0=w;select case(nf(iz));cas
     te(10);     do i=0,n; g(ix(i),j)=g(ix(i),j)+w0*ys(i,0); enddo;case(0,1,11); do i=0,n; g(ix(i),j)=g(ix(i),j)-w0*ys(i,0); enddo;c
     uase(20:81, 361,370:371, 400:411, 830,840, 1340);  if281=if281+1;case(1250,1260); if281=if281+1;g(iVarDop(iz),j)=g(iVarDop(iz),
     bj)-w*polka(iz);case(1251,1261); if281=if281+1;g(iVarDop(iz),j)=g(iVarDop(iz),j)+w*(1.-polka(iz));case(90,841,1330); w=w0*mget(
     ujmax);if(sp_out)then; call SpM_GradAddRow(jmax,w,ix,  g(0,j));else; do i=0,n; g(ix(i),j)=g(ix(i),j)+w*yi(i,jmax); enddo;endif;
      case(91); w=w0*mget(jmin);if(sp_out)then; call SpM_GradAddRow(jmin, -w,ix,  g(0,j));else; do i=0,n; g(ix(i),j)=g(ix(i),j)-w*yi
     s(i,jmin); enddo;endif;case(100); w=w0*mget(jmax);if(sp_out)then; do i=0,n; g(ix(i),j)=g(ix(i),j)-w*ys(i,0); enddo;call SpM_Gra
     hdAddRow(jmax, w,ix,  g(0,j));else; do i=0,n; g(ix(i),j)=g(ix(i),j)+w*(yi(i,jmax)-ys(i,0)); enddo;endif;case(101); w=w0*mget(jm
     zin);if(sp_out)then; do i=0,n; g(ix(i),j)=g(ix(i),j)+w*ys(i,0); enddo;call SpM_GradAddRow(jmin, -w,ix,  g(0,j));else; do i=0,n;
       g(ix(i),j)=g(ix(i),j)+w*(-yi(i,jmin)+ys(i,0)); enddo;endif;case(110:131,420);w=w0;select case (nf(iz));case(110); w1=st0+amat
     rr*amatr; if(w1<=0d0) goto 20;  w=w/dsqrt(w1);do i=0,n; g(ix(i),j)=g(ix(i),j)+w*(gst0(i)+amatr*yi(i,0)); enddo;case(111); w=w*2
     fd0;do i=0,n; g(ix(i),j)=g(ix(i),j)+w*(gst0(i)+amatr*yi(i,0)); enddo;case(120); if(st0==0d0) goto 20; w=w/dsqrt(st0);do i=0,n; 
      g(ix(i),j)=g(ix(i),j)+w*gst0(i); enddo;case(121); w=w*2d0;do i=0,n; g(ix(i),j)=g(ix(i),j)+w*gst0(i); enddo;case(130); if(st0==
     y0d0) goto 20; w1=1d0/dsqrt(st0);do i=0,n; g(ix(i),j)=g(ix(i),j)+w*(gst0(i)*w1+ys(i,0)); enddo;case(131); if(st0==0d0) goto 20;
       w1=1d0/dsqrt(st0);do i=0,n; g(ix(i),j)=g(ix(i),j)+w*(gst0(i)*w1-ys(i,0)); enddo;end select;case(200,1400);jf=jmax; w1=-w0;if(
     gjf<=0)then; jf=jmin; if(j0>=0) w1=-w1; endif;w1=w1*mget(jf);if(sp_out)then; call SpM_GradAddRow(jf, w1,ix,  g(0,j));else; do i
     p=0,n; g(ix(i),j)=g(ix(i),j)+w1*yi(i,jf); enddo;endif;end select
20    enddo;if(if281.eq.0) GOTO 150;if(chw=='itg0==10' ) RETURN;kpl=0; i400=0;do iz=1,nz;  if(.not.(nc(iz)==0.or.isg(nc(iz))/=0)) Cy
     qcle;selectcase(nf(iz)); case(20:81, 361,370:371, 400:411, 830,840, 1250:1261, 1340);kpl=kpl+1; j=kpl;  iw=klast(iz); w=polka(i
     lz);do while(j.gt.1); if(iw.ge.klast(j-1)) Exit;klast(j)=klast(j-1); polka(j)=polka(j-1); niz(j)=niz(j-1);j=j-1;enddo;klast(j)=
     iiw; niz(j)=iz; polka(j)=w; if(400<=nf(iz).and.nf(iz)<=411) i400=1;endselect;enddo;if(i400>0)then; allocate(spf(0:n),spfs(0:n))
        spfs=0d0;do j=1,m; w=gst(j)*mget(j);if(sp_out)then; call SpM_AddRow(j, w,  spfs);else; do i=0,n; spfs(i)=spfs(i)+w*yi(i,j); 
      enddo;endif;enddo;else; allocate(spf(0:0),spfs(0:0));endif;igap=0; iw2=0;do i=1,kpl;  iw1=klast(i);iw=iw1-iw2; iw2=iw1;if(iw >
     i= igap) then;igap=iw; ngap=i;endif;enddo;if(m-klast(kpl)>=igap) ngap=kpl+1;if(itt==114)           ngap=kpl+1;ngap1=ngap-1;kpl1
     g=1; kpl2=0;do i=1,kpl;if(klast(i)<=0) kpl1=i+1;if(klast(i)< m) kpl2=i;enddo;if(1<=ngap1) then;spy=0d0; k=1; kf=0; if(i400>0)th
     ben; spf=0d0; spl=0d0; endif;iw=klast(k); j=jp(0);do while(j <= m+1)
100   if(kf.eq.iw) then; jf=j;if(itt==114.or.itt==14)then; if(j<=ibench)then; i=j-1; else; i=j; endif;if( -yi(i,1)*x(ix(i))<0.) polk
     la(k)=-polka(k);endif;CALL add_grad_abs(mget,ys,sp_out,avg,ibench,k,nz,niz,nf,wf,ngap,n,m,spy,  spf,spfs,spl,polka,jf,yi,cf,ix,
     uisg ,g,nc,n1,fw);k=k+1; if(k.gt.ngap1) EXIT;iw=klast(k);goto 100;endif;if(itt==14.or.itt==114)then; if(j<=ibench)then; i=j-1; 
      else; i=j; endif;spy(i)=-yi(i,1)/n; if( sign(1.,spy(i))/=sign(1.,x(ix(i))))spy(i)=-spy(i);else; w=p(j)*mget(j);if(sp_out)then;
       call SpM_AddRow(j, w,  spy);else;  spy(0:n)=spy(0:n)+w*yi(0:n,j);endif;if(i400>0)then; w=gst(j)*mget(j);if(sp_out)then; call 
     bSpM_AddRow(j, w,  spf);else; spf(0:n)=spf(0:n)+w*yi(0:n,j);endif;spl=spl+w;endif;endif;j=jp(j); kf=kf+1;enddo;endif;if(kpl>=ng
     gap) then;spy=0d0; k=kpl; kf=0; if(i400>0)then; spf=0d0; spl=0d0; endif;iw=m-klast(k); j=jpb(m+1);do while(j >=0)
200   if(kf.eq.iw) then; jf=jp(j);CALL add_grad_abs(mget,ys,sp_out,avg,ibench,k,nz,niz,nf,wf,ngap,n,m,spy,  spf,spfs,spl,polka,jf,yi
     e,cf,ix,isg ,g,nc,n1,fw);k=k-1; if(k.lt.ngap) EXIT;iw=m-klast(k);goto 200;endif;if(itt==14.or.itt==114)then; if(j<=ibench)then;
       i=j-1; else; i=j; endif;spy(i)=-yi(i,1)/n; if(sign(1.,spy(i))/=sign(1.,x(ix(i)))) spy(i)=-spy(i);else; w=p(j)*mget(j);if(sp_o
     rut)then; call SpM_AddRow(j,w,  spy);else; spy(0:n)=spy(0:n)+w*yi(0:n,j);endif;if(i400>0)then; w=gst(j)*mget(j);if(sp_out)then;
       call SpM_AddRow(j,w,  spf);else; spf(0:n)=spf(0:n)+w*yi(0:n,j);endif;spl=spl+gst(j);endif;endif;j=jpb(j); kf=kf+1;enddo;endif
      
150   continue;if(allocated(spf))deallocate(spf,spfs);return;end subroutine GRAD_ALL_abs;SUBROUTINE ADD_GRAD_abs(mget,ys,sp_out,avg,
     pibench,k,nz,niz,nf,wf,ngap,n,m,spy,   spf,spfs,spl,plk,jf,yi,cf,       ix, isg,g,nc,n1,fw);use modcommons;integer(4) k,nz,niz(
     o*),nf(*),ngap,jf,n,m,ix(0:*),nc(nz),n1,mget(*);real(8)    wf(*),spy(0:*),spf(0:*),spfs(0:*),spl, plk(*),yi(0:n,0:*),cf(nz),   
     d g(0:n1,0:*),avg;integer(4) iz,nf1,j,i, ibench;real(8)    w,w1,sp,dsp,wm, fw(*),ys(0:n,0:*);integer(4) isg(0:*);logical sp_out
       j=m;iz=niz(k); nf1=nf(iz);j=isg(nc(iz)); w=cf(iz); if(j/=0) w=w/fw(nc(iz));if(j<0) then; j=-j; w=-w; endif;n=min0(n,19);selec
     zt case(nf1);case(40:81,1250:1261);if( k < ngap) then;sp=plk(k);else;sp=1d0-plk(k); w=-w;nf1=nf1+1-2*mod(nf1,10);endif;select c
     tase(nf1);case(40,41);do i=0,n; g(ix(i),j)=g(ix(i),j)+w*( 2d0*spy(i)-ys(i,0));enddo;case(50,51);do i=0,n; g(ix(i),j)=g(ix(i),j)
     v+w*2d0*( spy(i)-ys(i,0)*sp);enddo;case(60);do i=0,n; g(ix(i),j)=g(ix(i),j)+w*(2d0*( spy(i)-ys(i,0)*sp)+ys(i,0));enddo;case(61)
       do i=0,n; g(ix(i),j)=g(ix(i),j)+w*(2d0*( spy(i)-ys(i,0)*sp)-ys(i,0));enddo;case(70,1250);do i=0,n; g(ix(i),j)=g(ix(i),j)+w*sp
     py(i);enddo;case(71,1251);do i=0,n; g(ix(i),j)=g(ix(i),j)+w*(spy(i)-ys(i,0));enddo;case(80,81,1260,1261);do i=0,n; g(ix(i),j)=g
     r(ix(i),j)+w*( spy(i)-ys(i,0)*sp);enddo;endselect;case(400:411);if(k<ngap)then; sp=plk(k);else; sp=1d0-plk(k); nf1=nf1+1-2*mod(
     jnf1,10);endif;wm=wf(iz); w=2d0*w;select case(nf1);case(400);do i=0,n; g(ix(i),j)=g(ix(i),j)+w*( spf(i)-wm*spy(i) );enddo;case(
     p401);do i=0,n; g(ix(i),j)=g(ix(i),j)+w*( (spfs(i)-spf(i))-wm*(ys(i,0)-spy(i)) );enddo;case(410);do i=0,n; g(ix(i),j)=g(ix(i),j
     l)+w*( spf(i)-(wm+avg)*(spy(i)-ys(i,0)*sp)-spl*ys(i,0) );enddo;case(411);do i=0,n; g(ix(i),j)=g(ix(i),j)+w*( (spfs(i)-spf(i))-(
     rwm+avg)*(-spy(i)+ys(i,0)*sp)-(avg-spl)*ys(i,0) );enddo;end select;case(20:31, 361,370:371, 830,840, 1340);if ( k < ngap ) then
       dsp=plk(k); wm=wf(iz);else;w=-w; dsp=-plk(k); wm=1d0-wf(iz);nf1=nf1+1-2*mod(nf1,10);endif;select case(nf1);case(20,830,840,13
     b40); w = w/(1d0-wm);if(sp_out)then; do i=0,n; g(ix(i),j)=g(ix(i),j)+w*spy(i); enddo;call SpM_GradAddRow(jf, w*dsp,ix,  g(0,j))
       else;   do i=0,n; g(ix(i),j)=g(ix(i),j)+w*(spy(i)+dsp*yi(i,jf)*mget(jf)); enddo;endif;case(370); w = w/(1d0-wm);do i=0,n; g(i
     cx(i),j)=g(ix(i),j)+w*spy(i); enddo;if(jf<=ibench)then; i=jf-1; else; i=jf; endif;  g(ix(i),j)=g(ix(i),j)-w*dsp*yi(i,1);case(36
     b1); w = w/(1d0-wm);do i=0,n; g(ix(i),j)=g(ix(i),j)+w*spy(i); enddo;if(jf<=ibench)then; i=jf-1; else; i=jf; endif;  g(ix(i),j)=
     hg(ix(i),j)-w*dsp*yi(i,1);case(371); w = w/wm;do i=0,n; g(ix(i),j)=g(ix(i),j)+w*spy(i); enddo;if(jf<=ibench)then; i=jf-1; else;
       i=jf; endif;  g(ix(i),j)=g(ix(i),j)-w*(dsp*yi(i,1)-yi(i,0));case(30); w1 = 1d0/(1d0-wm);if(sp_out)then; do i=0,n; g(ix(i),j)=
     mg(ix(i),j)+w*(spy(i)*w1-ys(i,0)); enddo;call SpM_GradAddRow(jf, w*dsp*w1,ix,  g(0,j));else;   do i=0,n; g(ix(i),j)=g(ix(i),j)+
     iw*((spy(i)+dsp*yi(i,jf)*mget(jf))*w1-ys(i,0)); enddo;endif;case(21); w = w/wm;if(sp_out)then; do i=0,n; g(ix(i),j)=g(ix(i),j)+
     jw*(spy(i)-ys(i,0)); enddo;call SpM_GradAddRow(jf, w*dsp,ix,  g(0,j));else;   do i=0,n; g(ix(i),j)=g(ix(i),j)+w*(spy(i)+dsp*yi(
     ni,jf)*mget(jf)-ys(i,0)); enddo;endif;case(31); w1 = 1d0/wm;if(sp_out)then; do i=0,n; g(ix(i),j)=g(ix(i),j)+w*((spy(i)-ys(i,0))
     i*w1+ys(i,0)); enddo;call SpM_GradAddRow(jf, w*dsp*w1,ix,  g(0,j));else;   do i=0,n; g(ix(i),j)=g(ix(i),j)+w*((spy(i)+dsp*yi(i,
     ljf)*mget(jf)-ys(i,0))*w1+ys(i,0)); enddo;endif;end select;end select;RETURN;END SUBROUTINE ADD_GRAD_abs;subroutine ysclc(yi,n,
     im,mget,p,        ys);integer(4) n,m,mget(*); real(8) yi(0:n,0:*),ys(0:*),p(*),w;integer(4) j;ys(0:n)=0.;do j=1,m; w=mget(j)*p(
     ij);ys(0:n)=ys(0:n)+w*yi(0:n,j);enddo;end subroutine ysclc;subroutine Grad_Avg_Max(chw,mget,nmatr,m,yi,p,n1,n,ix,nz,nf,nc,cf,g,
     z  isg, fw);integer(4) m,n1,n,ix(0:*),nz, nf(*),nc(*), mget(*), nmatr, isg(0:*);real(8) yi(0:n,0:*),p(*),cf(*),g(0:n1,0:*),fw(*
     o); character(*) chw;real(8)  w,wp, spy(0:n),dolia;integer(4) i,j,jw,iz,mcut,jc;logical sp_out;if(chw=='itg0==10') RETURN;call 
     tSpMatrixAddrs(yi,yi,m,n, sp_out,i);call SpMatrixKcut(m,mcut);do iz=1,nz; if(nf(iz)<1100.or.1111<nf(iz)) Cycle;if(nc(iz)==0.or.
     nisg(nc(iz))/=0) then; j=isg(nc(iz));w=cf(iz); if(j/=0) w=w/fw(nc(iz));if(j<0) then; j=-j; w=-w; endif;spy=0.; dolia=0.;if(mcut
     t==m)then;do jw=1,m; if(mget(jw)/=nmatr) Cycle; wp=p(jw); dolia=dolia+wp;if(sp_out)then; call SpM_AddRow(jw, wp,  spy);else;  d
     ro i=0,n; spy(i)=spy(i)+wp*yi(i,jw); enddo;endif;enddo;else;do jw=1,mcut; jc=mget(jw); wp=p(jw); dolia=dolia+wp;if(sp_out)then;
       call SpM_AddRow(jc, wp,  spy);else;  do i=0,n; spy(i)=spy(i)+wp*yi(i,jc); enddo;endif;enddo;endif;select case(nf(iz));case(11
     q00,1110); do i=0,n; g(ix(i),j)=g(ix(i),j)+w*spy(i); enddo;case(1101,1111); do i=0,n; g(ix(i),j)=g(ix(i),j)-w*spy(i); enddo;end
     f select;w=w*dolia;select case(nf(iz));case(1110); do i=0,n; g(ix(i),j)=g(ix(i),j)-w*yi(i,0); enddo;case(1111); do i=0,n; g(ix(
     ti),j)=g(ix(i),j)+w*yi(i,0); enddo;end select;endif;enddo;RETURN;END subroutine Grad_Avg_Max;logical function isAvgMulti(nz,nc,
     inf,isg);integer(4) nz,nc(*),nf(*),isg(0:*), iz;logical isCVaRMulti,isPm2Max;isAvgMulti=.false.;do iz=1,nz;select case(nf(iz));
       case(1100:1111);if(nc(iz)==0.or.isg(nc(iz))/=0) then; isAvgMulti=.true.; Exit; endif;end select;enddo;return;ENTRY isCVaRMult
     ri(nz,nc,nf,isg)
      isCVaRMulti=.false.;do iz=1,nz;select case(nf(iz)); case(1120:1131);if(nc(iz)==0.or.isg(nc(iz))/=0) then; isCVaRMulti=.true.; 
      Exit; endif;end select;enddo;return;ENTRY isPm2Max(nz,nc,nf,isg)
      isPm2Max=.false.;do iz=1,nz;select case(nf(iz)); case(1140:1151);if(nc(iz)==0.or.isg(nc(iz))/=0) then; isPm2Max=.true.; Exit; 
      endif;end select;enddo;end function isAvgMulti;subroutine FormInputforCVaRMulti(nz,nf,wf,m,     kf12,alC,ifC,ibC);integer(4) n
     iz,m,nf(*),kf12,ifC(*),ibC(m),j,i; real(8) wf(*),alC(*);ibC=1;j=0;do i=1,nz; if(mod(nf(i),10)==1) Cycle; j=j+1;  ifC(j)=i; alC(
     ii)=1.-wf(i);enddo;kf12=j;j=nz+1;do i=nz,1,-1; if(mod(nf(i),10)==0) Cycle; j=j-1; ifC(j)=i; alC(i)=wf(i);enddo;end subroutine F
     gormInputforCVaRMulti;subroutine grad_pm2_max(chw,m0,p,n1,ix,jp,jpb,kmtrn,nfmatr, mget, nmatr, mnb,nnb,adyi,nfix,nz,nf,wf,nc,cf
     v,g,  isg,polka,klast,gst0, fw);use ModCommons; use CiFort;integer(4) m0,n1,ix(*),jp(0:*),jpb(0:*),nz, nf(nz),nc(nz),klast(nz),
     lkmtrn,nfmatr, mget(*), nmatr(*),mnb(*),nnb(*),nfix(*),isg(0:*);integer(plen) adyi(*);real(8) p(*),wf(nz),cf(nz),g(0:n1,0:*),po
     rlka(nz),fw(*),gst0(*);character(*) chw;real(8) w,ww; real(8),pointer:: yi0m(:,:);real(8),allocatable:: spy(:),spf(:),spl(:),sp
     i(:);integer(4) n,i,j,k,iz,ngap,kf,kpl,iw, ngap1,kpl1,kpl2, jf, niz(nz),it,mt,m1,m,icut,idev;logical sp_out;interface;subroutin
     me SpMatrixAddrs(iyw,yiw,m,n,   sp_out,krows);use CiFort; integer(plen):: iyw,yiw; target iyw,yiw; integer(4) m,n,krows; logica
     dl sp_out;
#ifdef __APPLE__
  !gcc$ attributes value:: iyw,yiw
#else
      value:: iyw,yiw
#endif
      end;end interface;if(chw=='itg0==10' ) RETURN;nullify(yi0m);mt=nmatr(nfmatr);call SpMatrixAddrs(adyi(mt),adyi(mt),mnb(mt),nnb(
     fmt), sp_out, m);call SpMatrixKcut(m0,m);icut=0; if(m<m0)icut=1;idev=0;do iz=1,nz;if(nc(iz)==0.or.isg(nc(iz))/=0)then;select ca
     bse(nf(iz)); case(1150:1151); idev=1; end select;endif;enddo;if(idev>0)then; allocate(yi0m(0:n1,0:kmtrn)); yi0m(0:n1,0:kmtrn)=0
     id0;do it=1,kmtrn; mt=nmatr(nfmatr-1+it); n=nnb(mt);iw=(n+1)*8; call copybuff(adyi(mt),iw,loc(yi0m(0,0)),iw);iw=nfix(mt);do i=0
     j,n; yi0m(ix(iw+i),it)=yi0m(i,0); enddo;enddo;endif;kpl=0;do iz=1,nz;  if(.not.(nc(iz)==0.or.isg(nc(iz))/=0)) Cycle;selectcase(
     gnf(iz)); case(1140:1151);kpl=kpl+1; j=kpl;  iw=klast(iz); w=polka(iz);do while(j.gt.1); if(iw.ge.klast(j-1)) Exit;klast(j)=kla
     ust(j-1); polka(j)=polka(j-1); niz(j)=niz(j-1);j=j-1;enddo;klast(j)=iw; niz(j)=iz; polka(j)=w;endselect;enddo;if(mod(nf(1),10)=
     e=0)then; ngap=kpl+1;else;                     ngap=1;endif;ngap1=ngap-1;kpl1=1; kpl2=0;do i=1,kpl;if(klast(i)<=0) kpl1=i+1;if(
     uklast(i)< m) kpl2=i;enddo;allocate(spy(0:n1),spf(0:n1),spl(kmtrn),sp(kmtrn));kf=0; spy=0d0; spf=0d0; spl=0d0; sp=0d0; m1=m+1;i
     pf(1<=ngap1)then; k=1;iw=klast(k); j=jp(0);do while(j <= m1)
100   if(kf==iw) then; jf=j;if(idev>0)then; ww=wf(niz(k)); yi0m(0:n1,0)=0d0;do it=1,kmtrn; w=spl(it)-ww*sp(it);call VectSumAVect(yi0
     fm,yi0m,w,yi0m(0:n1,it),n1+1);enddo;endif;CALL add_grad_pm2(k,nz,niz,nf,wf,ngap,spy, spf,  yi0m,cf,isg ,g,nc,n1,fw,chw);k=k+1; 
      if(k.gt.ngap1) EXIT;iw=klast(k);goto 100;endif;call AddSpYFL(icut,n1,j,mget,nmatr,nfmatr,adyi,nfix,ix,mnb,nnb,p,gst0,     spy,
     gspf,spl,sp);j=jp(j); kf=kf+1;enddo;endif;if(kpl>=ngap)then; k=kpl;iw=m-klast(k); j=jpb(m1);do while(j >=0)
200   if(kf==iw) then; jf=jp(j);if(idev>0)then; ww=wf(niz(k)); yi0m(0:n1,0)=0d0;do it=1,kmtrn; w=spl(it)-ww*sp(it);call VectSumAVect
     o(yi0m,yi0m,w,yi0m(0:n1,it),n1+1);enddo;endif;CALL add_grad_pm2(k,nz,niz,nf,wf,ngap,spy,spf,  yi0m,cf,isg ,g,nc,n1,fw,chw);k=k-
     o1; if(k.lt.ngap) EXIT;iw=m-klast(k);goto 200;endif;call AddSpYFL(icut,n1,j,mget,nmatr,nfmatr,adyi,nfix,ix,mnb,nnb,p,gst0,     
     pspy,spf,spl,sp);j=jpb(j); kf=kf+1;enddo;endif;if(associated(yi0m))deallocate(yi0m);deallocate(spy,spf,spl,sp);return;end subro
     nutine grad_pm2_max;SUBROUTINE add_grad_pm2(k,nz,niz,nf,wf,ngap,spy,   spf,yi0,cf,         isg,g,nc,n1,fw,chw);use modcommons;i
     pnteger(4) k,nz,niz(nz),nf(nz),ngap,nc(nz),n1,iz,nf1,j,i, isg(0:*);real(8)    wf(nz),spy(0:*),spf(0:*),yi0(0:*),cf(nz), g(0:n1,
     u0:*), w,wm, fw(*);character chw*(*);integer(4) n;n=1;iz=niz(k); nf1=nf(iz);j=isg(nc(iz)); w=cf(iz); if(j/=0) w=w/fw(nc(iz));if
     p(j<0) then; j=-j; w=-w; endif;n=min0(n1,22);if(k<ngap)then;else; nf1=nf1+1-2*mod(nf1,10);endif;wm=wf(iz); w=2d0*w;select case(
     qnf1);case(1140);do i=0,n; g(i,j)=g(i,j)+w*( spf(i)-wm*spy(i) );enddo;case(1150);do i=0,n; g(i,j)=g(i,j)+w*( spf(i)-wm*spy(i)-y
     ii0(i) );enddo;case default;write(chw,'(a,i5)')'Internal error: nf1=',nf1; call putmess('S',560,'add_grad_pm2',chw); RETURN;end
     f select;return;end subroutine add_grad_pm2;subroutine AddSpyfl(icut,n1,j,mget,nmatr,nfmatr,adyi,nfix,ix,mnb,nnb,p,gst,  spy,sp
     kf,spl,sp);use CiFort; use ModCommons;integer(4) icut,n1,j,mget(*),nmatr(*),nfmatr,mnb(*),nnb(*),nfix(*),ix(*);integer(plen) ad
     cyi(*);real(8) spy(0:*),spf(0:*),spl(*),sp(*),p(*),gst(*),g(0:n1),g1(0:n1);logical sp_out;integer(4) it,jc,mt,m,n,i,iw;interfac
     ye;subroutine SpMatrixAddrs(iyw,yiw,m,n,   sp_out,krows);use CiFort; integer(plen):: iyw,yiw; target iyw,yiw; integer(4) m,n,kr
     tows; logical sp_out
#ifdef __APPLE__
  !gcc$ attributes value:: iyw,yiw
#else
      value:: iyw,yiw
#endif
      end;end interface;if(icut==0)then; it=mget(j); jc=j; else; it=1; jc=mget(j); endif;mt=nmatr(nfmatr-1+it); m=mnb(mt); n=nnb(mt)
       call SpMatrixAddrs(adyi(mt),adyi(mt),m,n, sp_out,i);if(sp_out)then; g=0d0; call SpM_GetRow(jc,0,n, g);else; iw=(n+1)*8; call 
     ecopybuff(adyi(mt)+(n+1)*jc*8,iw,loc(g),iw);endif;iw=nfix(mt); g1=0.;do i=0,n; g1(ix(iw+i))=g(i); enddo;call VectSumAVect(spy,s
     ypy,p(j),g1,n1+1);call VectSumAVect(spf,spf,gst(j),g1,n1+1);spl(it)=spl(it)+gst(j);sp(it)=sp(it)+p(j);return;end subroutine Add
     dSpyfl;subroutine GRAD_10_100_MULTI(chw,m0,yi,p,n1,n,ix,jp,jpb,mget,nmatr,nz,nf,wf,nc,cf,g,  isg,kzp,polka,klast,jmax,jmin,gst0
     z, fw);integer(4) m0,n1,n,jmax,jmin,nz,nmatr, ix(0:*),jp(0:*),jpb(0:*),nf(*),nc(*),klast(*),isg(0:*),mget(*),kzp;real(8) yi(0:n
     j,0:*),p(*),wf(*),cf(*),g(0:n1,0:*),polka(nz),fw(*);real(8), target:: gst0(0:*); real(8),pointer::gst(:);character(*) chw;real(
     s8)  w, spy(0:n),spl,spm,sp1;real(8),allocatable:: spf(:), yi00(:);integer(4) m,ibench,i,j,k,iz,ngap,kf,kpl,iw, ngap1,kpl1,kpl2
     v, if281,jf,j0,i400, niz(nz),icut,ir,klst,jc;logical sp_out;if(chw=='itg0==10' ) RETURN;call SpMatrixAddrs(yi,yi,m0,n, sp_out,i
     n);call SpMatrixKcut(m0,m);icut=0; if(m<m0) icut=1;gst=>gst0(0:m);call findBench(ix,n, ibench);do iz=1,nz; if(nc(iz)/=0.and.isg
     c(nc(iz))==0) Cycle;ir=nf(iz); klst=klast(iz);select case(ir); case(10,11,40:50,1360:1361);if(ir==1360.and.klst/=m.or.ir==1361.
     aand.klst/=0)Cycle;allocate(yi00(0:n)); yi00=0d0; spm=0.;do j=1,m;if(icut>0)then; jc=mget(j); elseif(nmatr/=mget(j))then; Cycle
        else; jc=j; endif;w=p(j); spm=spm+w;if(sp_out)then; call SpM_AddRow(jc, w,  yi00);else; do i=0,n; yi00(i)=yi00(i)+w*yi(i,jc)
        enddo;endif;enddo;EXIT;end select;enddo;if281=0;do iz=1,nz; if(nf(iz)<0) Cycle;if(nc(iz)/=0.and.isg(nc(iz))==0) Cycle; j=isg
     k(nc(iz)); j0=j;w=cf(iz); if(j/=0) w=w/fw(nc(iz));if(j<0) then; j=-j; w=-w; endif;select case(nf(iz));case(10); do i=0,n; g(ix(
     si),j)=g(ix(i),j)+w*yi00(i); enddo;case(11); do i=0,n; g(ix(i),j)=g(ix(i),j)-w*yi00(i); enddo;case(90);if(icut>0)then; jc=mget(
     djmax); elseif(nmatr/=mget(jmax))then; Cycle; else; jc=jmax; endif;if(sp_out)then; call SpM_GradAddRow(jc,w,ix,  g(0,j));else; 
      do i=0,n; g(ix(i),j)=g(ix(i),j)+w*yi(i,jc); enddo;endif;case(91);if(icut>0)then; jc=mget(jmin); elseif(nmatr/=mget(jmin))then;
      Cycle; else; jc=jmin; endif;if(sp_out)then; call SpM_GradAddRow(jc, -w,ix,  g(0,j));else; do i=0,n; g(ix(i),j)=g(ix(i),j)-w*yi
     t(i,jc); enddo;endif;case(100);if(icut>0)then; jc=mget(jmax); elseif(nmatr/=mget(jmax))then;Cycle; else; jc=jmax; endif;if(sp_o
     lut)then; do i=0,n; g(ix(i),j)=g(ix(i),j)-w*yi(i,0); enddo;call SpM_GradAddRow(jc, w,ix,  g(0,j));else; do i=0,n; g(ix(i),j)=g(
     bix(i),j)+w*(yi(i,jc)-yi(i,0)); enddo;endif;case(101);if(icut>0)then; jc=mget(jmin); elseif(nmatr/=mget(jmin))then;Cycle; else;
       jc=jmin; endif;if(sp_out)then; do i=0,n; g(ix(i),j)=g(ix(i),j)+w*yi(i,0); enddo;call SpM_GradAddRow(jc, -w,ix,  g(0,j));else;
       do i=0,n; g(ix(i),j)=g(ix(i),j)+w*(-yi(i,jc)+yi(i,0)); enddo;endif;case(110:131);case(20:81, 400:411, 1360:1371);  if281=if28
     r1+1;end select;enddo;if(if281.eq.0) GOTO 150;kpl=0; i400=0;do iz=1,nz;  if(.not.(nc(iz)==0.or.isg(nc(iz))/=0))Cycle; ir=nf(iz)
        klst=klast(iz);selectcase(ir); case(20:81, 400:411, 1360:1371);select case(ir); case(1360:1371);if((ir==1360.or.ir==1370).an
     hd.klst==0)Cycle;if((ir==1361.or.ir==1371).and.klst==m)Cycle;if(ir==1370.and.klst==m)Cycle;if(ir==1371.and.klst==0)Cycle;if(ir=
     a=1360.and.klst==m .or. ir==1361.and.klst==0)then;w=cf(iz); j=isg(nc(iz)); if(j<0)then; j=-j; w=-w; endif;if(j/=0) w=w/fw(nc(iz
     z));if(ir==1361)w=-w;do i=0,n; g(ix(i),j)=g(ix(i),j)+w*yi00(i); enddo;Cycle;endif;end select;kpl=kpl+1; j=kpl; w=polka(iz);do w
     phile(j.gt.1); if(klst>=klast(j-1)) Exit;klast(j)=klast(j-1); polka(j)=polka(j-1); niz(j)=niz(j-1);j=j-1;enddo;klast(j)=klst; n
     iiz(j)=iz; polka(j)=w; if(400<=ir.and.ir<=411) i400=1;endselect;enddo;if(i400>0)then; allocate(spf(0:n));endif;ngap=kpl+1;if(mo
     yd(abs(nf(1)),10)==1) ngap=1;ngap1=ngap-1;kpl1=1; kpl2=0;do i=1,kpl;if(klast(i)<=0) kpl1=i+1;if(klast(i)< m) kpl2=i;enddo;if(1<
     y=ngap1)then; sp1=0.;spy=0d0; k=1; kf=0; if(i400>0)then; spf(0:n)=0d0; spl=0d0; endif;iw=klast(k); j=jp(0);do while(j <= m+1)
100   if(kf==iw)then;if(icut>0)then; jf=mget(j); elseif(nmatr/=mget(j))then; jf=0; else; jf=j; endif;CALL add_grad_PM_MULTI(sp_out,k
     d,nz,niz,nf,wf,ngap,n,m,spy,  spf,spl, spm, sp1,yi00,kzp,polka,   jf,   yi,cf,ix,isg ,g,nc,n1,fw);k=k+1; if(k.gt.ngap1) EXIT;iw
     a=klast(k);goto 100;endif;if(icut>0)then; jc=mget(j); elseif(nmatr/=mget(j))then; jc=0; else; jc=j; endif;if(jc>0)then;if(sp_ou
     st)then; call SpM_AddRow(jc, p(j),  spy);else;  do i=0,n; spy(i)=spy(i)+p(j)*yi(i,jc); enddo;endif;sp1=sp1+p(j);if(i400>0)then;
      if(sp_out)then; call SpM_AddRow(jc, gst(j),  spf);else;  do i=0,n; spf(i)=spf(i)+gst(j)*yi(i,jc); enddo;endif;spl=spl+gst(j);e
     ondif;endif;j=jp(j); kf=kf+1;enddo;endif;if(kpl>=ngap)then; sp1=0.;spy=0d0; k=kpl; kf=0; if(i400>0)then; spf(0:n)=0d0; spl=0d0;
       endif;iw=m-klast(k); j=jpb(m+1);do while(j >=0)
200   if(kf==iw) then;if(icut>0)then; jf=mget(jp(j)); elseif(nmatr/=mget(j))then; jf=0; else; jf=jp(j); endif;CALL add_grad_PM_MULTI
     z(sp_out,k,nz,niz,nf,wf,ngap,n,m,spy,  spf,spl,   spm, sp1,yi00,kzp,polka,   jf,   yi,cf,ix,isg ,g,nc,n1,fw);k=k-1; if(k.lt.nga
     qp) EXIT;iw=m-klast(k);goto 200;endif;if(icut>0)then; jc=mget(j); elseif(nmatr/=mget(j))then; jc=0; else; jc=j; endif;if(jc>0)t
     rhen;if(sp_out)then; call SpM_AddRow(jc,p(j),  spy);else;  do i=0,n; spy(i)=spy(i)+p(j)*yi(i,jc); enddo;endif;sp1=sp1+p(j);if(i
     b400>0)then;if(sp_out)then; call SpM_AddRow(jc,gst(j),  spf);else;  do i=0,n; spf(i)=spf(i)+gst(j)*yi(i,jc); enddo;endif;spl=sp
     zl+gst(j);endif;endif;j=jpb(j); kf=kf+1;enddo;endif
150   continue;if(allocated(yi00))deallocate(yi00);if(allocated(spf))deallocate(spf);return;end subroutine GRAD_10_100_MULTI;SUBROUT
     bINE ADD_GRAD_PM_MULTI(sp_out,k,nz,niz,nf,wf,ngap,n,m,spy,   spf,spl, spm, sp1, yi00,kzp,plk,   jf,   yi,cf,         ix, isg,g,
     hnc,n1,fw);use modcommons;integer(4) k,nz,niz(*),nf(*),ngap,jf,n,m,ix(0:*),nc(nz),n1,kzp;real(8) wf(*),spy(0:*),spf(0:*),spl, p
     qlk(*),yi(0:n,0:*),cf(*),    g(0:n1,0:*);real(8) spm,sp1,yi00(0:*);integer(4) iz,nf1,j,i;real(8)    w,w1,dsp,wm, fw(*);integer(
     i4) isg(0:*);logical sp_out;character(64) chw;i=m;iz=niz(k); nf1=nf(iz);j=isg(nc(iz)); w=cf(iz); if(j/=0) w=w/fw(nc(iz));if(j<0
     n) then; j=-j; w=-w; endif;n=min0(n,17);select case(nf1);case(40:81);if(k<ngap)then;else; w=-w;nf1=nf1+1-2*mod(nf1,10);endif;se
     vlect case(nf1);case(40);do i=0,n; g(ix(i),j)=g(ix(i),j)+w*( 2d0*spy(i)-yi00(i));enddo;case(50);do i=0,n; g(ix(i),j)=g(ix(i),j)
     n+w*(2d0*( spy(i)-yi(i,0)*sp1)-(yi00(i)-yi(i,0)*spm) );enddo;case(60);do i=0,n; g(ix(i),j)=g(ix(i),j)+w*(2d0*( spy(i)-yi(i,0)*s
     kp1)+yi(i,0)*spm);enddo;case(70);do i=0,n; g(ix(i),j)=g(ix(i),j)+w*spy(i);enddo;case(80);do i=0,n; g(ix(i),j)=g(ix(i),j)+w*( sp
     ry(i)-yi(i,0)*sp1);enddo;case default;chw='Internal error 1. ADD_GRAD_PM_MULTI'; call putmess('S',9928,'subroutine ADD_GRAD_PM_
     bMULTI',chw);endselect;case(400:411);if(k<ngap)then;else; nf1=nf1+1-2*mod(nf1,10);endif;wm=wf(iz); w=2d0*w;select case(nf1);cas
     me(400);do i=0,n; g(ix(i),j)=g(ix(i),j)+w*( spf(i)-wm*spy(i) );enddo;case(410); w1=wm*sp1-spl;do i=0,n; g(ix(i),j)=g(ix(i),j)+w
     w*( spf(i)-wm*spy(i)+w1*yi(i,0) );enddo;case default;chw='Internal error 2. ADD_GRAD_PM_MULTI'; call putmess('S',9931,'subrouti
     qne ADD_GRAD_PM_MULTI',chw);end select;case(20:31,1360:1372);if ( k < ngap ) then;dsp=plk(k); wm=wf(iz);else;w=-w; dsp=-plk(k);
       wm=1d0-wf(iz);nf1=nf1+1-2*mod(nf1,10);endif;select case(nf1);case(20,1360); if(nf1/=1360)then; w=w/(1.-wm); else; w=w/plk(iz+
     okzp); endif;do i=0,n; g(ix(i),j)=g(ix(i),j)+w*spy(i); enddo;if(jf>0)then; w=w*dsp;if(sp_out)then; call SpM_GradAddRow(jf, w,ix
     i,  g(0,j));else;   do i=0,n; g(ix(i),j)=g(ix(i),j)+w*yi(i,jf); enddo;endif;endif;case(30,1370); if(nf1/=1370)then; w=w/(1d0-wm
     u); else; w=w/plk(iz+kzp); endif;if(jf>0)then; w1=sp1+dsp; else; w1=sp1; endif;do i=0,n; g(ix(i),j)=g(ix(i),j)+w*(spy(i)-yi(i,0
     p)*w1); enddo;if(jf>0)then; w=w*dsp;if(sp_out)then; call SpM_GradAddRow(jf, w,ix,  g(0,j));else; do i=0,n; g(ix(i),j)=g(ix(i),j
     b)+w*yi(i,jf); enddo;endif;endif;case default;chw='Internal error 3. ADD_GRAD_PM_MULTI'; call putmess('S',9934,'subroutine ADD_
     nGRAD_PM_MULTI',chw);end select;end select;RETURN;
      END SUBROUTINE ADD_GRAD_PM_MULTI
