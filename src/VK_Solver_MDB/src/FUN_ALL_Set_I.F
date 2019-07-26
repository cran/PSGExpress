      subroutine Fun_ALL(mname,ch, m,mf,yi,p,n1,n,x,ix,jp,jpb,if210,if113,nz,nf,wf,nc,cf,fi,    kzp,polka,klast,iVarDop,jmax,jmin,av
     ag0,st0,gst0,kmatr,itnab,   fm,pf);use FuncNames; use ModCommons; use CiFort;character(*) mname,ch;integer(4) m,mf,n1,n,if210,i
     jf113,nz,kmatr,itnab,kzp;integer(4) ix(0:*),nf(*),nc(*),jmax,jmin,klast(*),iVarDop(*);integer(4) jp(0:*), jpb(0:*);real(8) avg0
     r,st0,p(*),x(0:*),wf(*),cf(*),fi(0:*);real(8) yi(0:n,0:*),polka(*),fm(*),pf(*);real(8),target:: gst0(0:*);real(8), pointer:: gs
     dt(:);real(8) alp1m,alpm,gap,x2linear,fmax,fmin,w,w1,w2,wm,w11,w22, fall(0:kfn,0:1);integer(4) i,j,k,iz,kcv,ngap,kf,kpl,iw,kpl1
     w,kpl2,idi,ngap1,igap,iw1,iw2,mdiscr, i400, krows,ibench,kbpoe;character chw*256;integer(4) niz(nz),jl(nz),jvar(nz),jpmax(nz+1)
     p,jpmin(nz+1);real(8) spf(nz),sp(nz),avg,t(nz),spf2(nz),avg2,f,adjcoef;real(8) dconf1, dconf2, dconf22;real(8),allocatable:: xw
     h(:),yiben(:);common /dconf/dconf1, dconf2, dconf22;integer(4), allocatable:: list(:);logical use_nOder, use_nIb1, use_PrSet, n
     xew_cvars,use_Quadro;common/use_control/ use_nOder, use_nIb1, use_PrSet, new_cvars,use_Quadro;i=n1; avg2=0.; ngap=0;i=0; j=0;do
     n iz=1,nz; iw=nf(iz); if(iw<0)then; iw=-iw; j=j+1; endif;select case(iw); case(110:131); if(i==0)i=1; case(420); i=2; endselect
       enddo;if(j==nz) RETURN;i=(n+1)*i; gst=>gst0(i:i+m);call findBench(ix,n, ibench);fall=0d0; st0=0.; allocate(xw(0:n));if(kmatr=
     a=1.and.itnab<0)then;avg=x2Linear(n,x,yi,ix,xw,fall(0,0));avg0=avg;fall(0,1)=fall(0,0);fall(1,0)=avg;   fall(1,1)=-avg;krows=m;
      if(if210.eq.1)then;call Fun_fm(m,yi,p,n,xw,fm,pf,fall,jmax,jmin,krows);endif;if(if113/=0)then;if(if113==-4.or.if113==-1.or.if1
     c13==4)then;CALL ST_FUN(mname,nf,nz,ix,-1,n,xw,yi(0,1),fall,avg,st0,gst0,  m,yi,fm,pf);endif;if(if113==4)then;CALL ST_FUN(mname
     x,nf,nz,ix, 2,n,xw,yi(0,krows+1),fall,avg,st0,gst0,  m,yi,fm,pf);elseif(if113>0)then;CALL ST_FUN(mname,nf,nz,ix,if113,n,xw,yi(0
     k,krows+1),fall,avg,st0,gst0,  m,yi,fm,pf);elseif(if113<-1)then;CALL ST_FUN(mname,nf,nz,ix,-4,n,xw,yi(0,1),fall,avg,st0,gst0,  
     hm,yi,fm,pf);endif;endif;elseif(itnab==200.or.itnab==201.or.itnab==202)then;fall(1,0)=avg0;   fall(1,1)=-avg0;avg=avg0;i=-1; if
     k(itnab==202) i=-2;if(if113==-4.or.if113==-1.or.if113==4)then;CALL ST_FUN(mname,nf,nz,ix,i,n,xw,yi(0,1),fall,avg,st0,gst0,  m,y
     oi,fm,pf);endif;fall(9,0)=fm(jmax);fall(10,0)=fm(jmax)-avg;fall(9,1)=-fm(jmin);fall(10,1)=-fm(jmin)+avg;else;fall(1,0)=avg0;   
       fall(1,1)=-avg0;avg=0d0;if(if113==-4.or.if113==-1.or.if113==4)then;CALL ST_FUN(mname,nf,nz,ix,-1,n,xw,yi(0,1),fall,avg,st0,gs
     kt0,  m,yi,fm,pf);endif;fall(23,0)=avg0;   fall(23,1)=-avg0;fall(24,0)=avg0;   fall(24,1)=-avg0;fall(110,0)=avg0;   fall(110,1)
     g=-avg0;fall(111,0)=avg0;   fall(111,1)=-avg0;if(jmin.ne.0)then; w=-fm(jmin);fall(9,1)=w;fall(39,1)=w;fall(10,1)=w;fall(25,1)=w
       fall(26,1)=w;elseif(jmax.ne.0) then;fall(20,0)=fm(jmax);fall(20,1)=fm(jmax);fall(140,0)=fm(jmax);endif;if(jmax.ne.0)then; w=f
     rm(jmax);fall(9,0)=w;fall(39,0)=w;fall(10,0)=w;fall(25,0)=w;fall(26,0)=w;fall(84,1)=w;fall(133,0)=w;fall(139,0)=w;elseif(jmin.n
     le.0) then;fall(20,0)=fm(jmin);fall(20,1)=fm(jmin);fall(140,0)=fm(jmin);endif;endif;deallocate(xw);if(ioutk>=istop-1) goto 7999
     a9;do iz=1,nz;     if(nf(iz).lt.0) Cycle;i=nf(iz)/10; j=nf(iz)-i*10;fi(nc(iz))=fi(nc(iz))+ cf(iz)*fall(i,j);if(i==11.and.mname=
     g='Objects')then;polka(iz+kzp)=fall(61,j);polka(iz+kzp*2)=fall(62,j);endif;enddo;if(if210.ne.1) GOTO 150;mdiscr=1000000;kcv=0; 
      kbpoe=0;do iz=1,nz; i=nf(iz);select case(i);case(20:31, 140:151, 210:221, 361,370:381, 820,821,830,840, 1120:1131,1340);kcv=kc
     lv+1; niz(kcv)=iz; polka(kcv)=wf(iz);case(1360:1371); kbpoe=kbpoe+1;end select;enddo;if(ch=='itg0==10' ) RETURN;if(kcv==0) GOTO
     s 100;do i=2,kcv;w=polka(i); iw=niz(i); j=i;do while(j.gt.1); if(w.le.polka(j-1)) Exit;polka(j)=polka(j-1); niz(j)=niz(j-1); j=
     qj-1;enddo;polka(j)=w; niz(j)=iw;enddo;gap=0d0; w1=1d0;do i=1,kcv;if(w1-polka(i) >= gap) then;gap=w1-polka(i); ngap=i;endif;w1=
     lpolka(i);enddo;if(w1>=gap)  ngap=kcv+1;do i=1,ngap-1;polka(i)=1d0-polka(i);enddo;alp1m=-1; alpm=-1;if(ngap.gt.1)then;alp1m=pol
     wka(ngap-1);if(alp1m<dconf2) alp1m=dconf2; if(alp1m>dconf22) alp1m=dconf22;endif;if(ngap.le.kcv) then;alpm=polka(ngap);if(alpm<
     edconf2) alpm=dconf2; if(alpm>dconf22) alpm=dconf22;endif;if(use_nOder) then;do i=1,ngap-1; t(i)=polka(i); enddo;do i=ngap,kcv;
       t(i)=1d0-polka(i); enddo;do i=1,kcv;if(t(i)<dconf2) t(i)=dconf2; if(t(i)>dconf22) t(i)=dconf22;enddo;CALL New_Ordering(t,kcv,
     am,mf,jmin,jmax,avg0,fm,p,pf,   jp,jpb);else;chw='Internal: This Fun_ALL works only with use_nOder=.true.'; call putmess('S',50
     l9,'Function_calculating',chw); goto 150;endif;idi=nz-kcv;if(alp1m.gt.0d0) then;w=0d0; w2=0d0; k=1; j=jp(0); w1=0d0;do i=1,m
70    w=w1+p(j);if(w.ge.polka(k)) then; iw=k+idi;sp(iw)=polka(k)-w1;  jl(iw)=i-1;spf(k)=w2+fm(j)*sp(iw);jvar(k)=j;k=k+1; if(k==ngap)
     j EXIT; goto 70;endif;w2=w2+pf(j); w1=w; j=jp(j);enddo;endif;if(alpm.gt.0d0) then;w=0d0; w2=0d0; k=kcv; j=jpb(m+1); w1=0d0;do i
     d=1,m
80    w=w1+p(j);if(w.ge.polka(k)) then;  iw=k+idi;sp(iw)=polka(k)-w1;     jl(iw)=mf-i;spf(k)=w2+fm(j)*sp(iw); sp(iw)=p(j)-sp(iw);jva
     sr(k)=j;k=k-1; if(k==ngap-1) EXIT; goto 80;endif;w2=w2+pf(j); w1=w; j=jpb(j);enddo;endif;do i=1,kcv;iz=niz(i); wm=polka(i);if (
     y i < ngap ) then;w1= spf(i)/wm;w2=(spf(i)-avg0)/(1d0-wm);else;w2= -spf(i)/wm;w1=(-spf(i) + avg0)/(1d0-wm);endif;select case(nf
     d(iz));case(20,210,220,361,370,820,830,840,1120,1340);w = w1;case(30,1130);w = w1-avg;case(21,211,221,371,821,        1121);w =
     p w2;case(31,1131);w = w2+avg;case(140,150,380);j=jvar(i); w = fm(j);  w2=w;if(m<=mdiscr) then;if(jp(j)<m+1)then; w1=fm(jp(j));
       else; w1=w; endif;if(p(j)>0.)then;w = w -(w-w1)*dmin1(sp(i+idi)/p(j),1.);else;w = w -(w-w1)*0.5d0;endif;endif;if(nf(iz)==150)
     t w=w-avg;case(141,151,381);j=jvar(i); w = -fm(j); w2=w;if(m<=mdiscr) then;if(jpb(j)>0)then; w1=-fm(jpb(j)); else; w1=w; endif;
      if(p(j)>0.)then;w = w1 +(w-w1)*dmin1(sp(i+idi)/p(j),1.);else;w = w1 +(w-w1)*0.5d0;endif;endif;if(nf(iz)==151) w=w+avg;end sele
     gct;fi(nc(iz))=fi(nc(iz))+ cf(iz)*w;enddo;do i=kcv,1,-1;niz(i+idi)=niz(i);enddo
100   continue;kpl=0; i400=0;do iz=1,nz; i=nf(iz);select case(i);case(40:81, 160:191, 400:411, 1140:1151, 1230:1241, 1250:1261); w=w
     zf(iz); iw=nf(iz)/10;selectcase(iw); case(123:124); w=0.; endselect;selectcase(iw); case(5,6,8,17,19,41,124,126); w=w+avg; ends
     nelect;selectcase(i); case(160,170,180,190); w=w+1d-13; case(161,171,181,191); w=w-1d-13; endselect;selectcase(iw); case(125:12
     v6); w=w+x(iVarDop(iz)); endselect;kpl=kpl+1; niz(kpl)=iz; polka(kpl)=w;select case(i); case(400:411,1140:1151); i400=i400+1;if
     b(i400==1)then; avg2=0d0; do j=1,m; avg2=avg2+pf(j)*fm(j); gst(j)=pf(j); enddo;endif;endselect;end select;enddo;ngap=1; ngap1=0
        if(kpl.eq.0) GOTO 140;do i=2,kpl;w=polka(i); iw=niz(i); j=i;do while(j.gt.1); if(w.le.polka(j-1)) Exit;polka(j)=polka(j-1); 
      niz(j)=niz(j-1);j=j-1;enddo;polka(j)=w; niz(j)=iw;enddo;fmax= fall(9,0); fmin=-fall(9,1);kpl1=1; kpl2=0;do i=1,kpl;if(polka(i)
     i>=fmax) kpl1=i+1;if(polka(i)>=fmin) kpl2=i;enddo;klast(:nz)=0;call Sorting(0,m+1,fm,jp,jpb,polka,kpl1,kpl2, klast, iw, jpmax,j
     wpmin );do i=kpl1+1,kpl2;klast(i)=klast(i)+klast(i-1);enddo;do i=kpl2+1,kpl;klast(i)=m;enddo;igap=0; iw2=0;do i=1,kpl;  iw1=kla
     nst(i);iw=iw1-iw2; iw2=iw1;if(iw.gt.igap) then;igap=iw; ngap=i;endif;enddo;if(m-klast(kpl).gt.igap) ngap=kpl+1;ngap1=ngap-1;kpl
     i1=1; kpl2=0;do i=1,kpl;if(klast(i)<=0) kpl1=i+1;if(klast(i)< m) kpl2=i;enddo;do i=1,kpl;sp(i)=0d0; spf(i)=0d0; spf2(i)=0d0;end
     odo;if(kpl1<=ngap1) then;i=kpl1; kf=0; iw=klast(i); j=jp(0);do while(j <= m)
10    if(kf.eq.iw) then;i=i+1;  if(i.gt.ngap1) EXIT;iw=klast(i);sp(i)=sp(i-1); spf(i)=spf(i-1); spf2(i)=spf2(i-1);goto 10;endif;spf(
     si)=spf(i)+pf(j); if(i400>0) spf2(i)=spf2(i)+pf(j)*fm(j);sp(i)=sp(i)+p(j);j=jp(j); kf=kf+1;enddo;endif;if(kpl2>=ngap) then;i=kp
     ul2; kf=0; iw=m-klast(i); j=jpb(m+1);do while(j > 0)
20    if(kf.eq.iw) then;i=i-1;  if(i.lt.ngap) EXIT;iw=m-klast(i);sp(i)=sp(i+1); spf(i)=spf(i+1); spf2(i)=spf2(i+1);goto 20;endif;spf
     c(i)=spf(i)+pf(j); if(i400>0) spf2(i)=spf2(i)+pf(j)*fm(j);sp(i)=sp(i)+p(j);j=jpb(j); kf=kf+1;enddo;endif;do i=1,kpl;iz=niz(i); 
       wm=polka(i);if ( i < ngap) then;w1=  spf(i) - wm*sp(i); w11=sp(i); w22=1d0-sp(i);w2=  w1 - avg0 + wm;else;w2= -spf(i) + wm*sp
     c(i); w22=sp(i); w11=1d0-sp(i);w1=  w2 + avg0 - wm;endif;select case(nf(iz));case(40,50,60,61);w = w1 + w2;if(nf(iz)==60) w=w+a
     bvg0;if(nf(iz)==61) w=w-avg0;case(70,80,1230,1240,1250,1260);w = w1; if(nf(iz)==1230) w=w-(1.-wf(iz))*avg;case(71,81,1231,1241,
     c1251,1261);w = w2; if(nf(iz)==1231) w=w+wf(iz)*avg;case(400,410,1140,1150);if(i<ngap)then; w = spf2(i) +(- 2d0*spf(i) + wm*sp(
     ri))*wm;else; w = (avg2-spf2(i)) +(- 2d0*(avg0-spf(i)) + wm*(1d0-sp(i)))*wm;endif;case(401,411,1141,1151);if(i<ngap)then; w = (
     vavg2-spf2(i)) +(- 2d0*(avg0-spf(i)) + wm*(1d0-sp(i)))*wm;else; w = spf2(i) +(- 2d0*spf(i) + wm*sp(i))*wm;endif;case(160,170,18
     c0,190);w = w11;if(m<=mdiscr) then;if(klast(i)<m.and.klast(i)>0) then;do iw=i+1,kpl; if(klast(iw)>klast(i))exit; enddo;w22=fm(j
     apmax(iw));do iw=i,2,-1; if(klast(iw-1)<klast(i))exit; enddo;w1=fm(jpmin(iw))-w22;w2=polka(i)-w22;if(w2>=0d0.and.w2<w1.or.w2>0d
     t0.and.w2==w1) w=w11-p(jpmin(iw))*w2/w1;endif;endif;case(161,171,181,191);w = w22;if(m<=mdiscr) then;if(klast(i)<m.and.klast(i)
     s>0) then;do iw=i,2,-1; if(klast(iw-1)<klast(i))exit; enddo;w11=fm(jpmin(iw));do iw=i+1,kpl; if(klast(iw)>klast(i))exit; enddo;
      if(fm(jpmax(iw))==wm)then;w=0.; j=jpmax(iw); w1=p(j); j=jp(j);do while(jpb(j)/=jpmin(iw)); if(fm(j)==wm) w=w+p(j);j=jp(j);endd
     vo;w=w22-w1-w;else;w1=w11-fm(jpmax(iw));w2=w11-polka(i);if(w2>=0d0.and.w2<w1.or.w2>0d0.and.w2==w1) w=w22-p(jpmax(iw))*w2/w1;end
     nif;elseif(klast(i)==0)then;do iw=i+1,kpl; if(klast(iw)>klast(i))exit; enddo;if(fm(jpmax(iw))==wm)then;w=0.; j=jpmax(iw); w1=p(
     nj); j=jp(j);do while(jpb(j)/=jpmin(iw)); if(fm(j)==wm) w=w+p(j);j=jp(j);enddo;w=w22-w1-w;endif;endif;endif;end select;if(w<0d0
     m.and.(nf(iz)/10/=6))then;if(w<-1d-12)then; write(chw,'(a,1p,e10.3,a)')"Func. 40-81 =", w,' <0'; call putmess('W',0,'Function_c
     xalculating',chw);endif;w=0d0;endif;fi(nc(iz))=fi(nc(iz))+ cf(iz)*w;if(mname=='Objects')then;select case(nf(iz));case(40,1230:1
     a241); f=w;allocate(yiben(m),list(m));call AdjustCoeffAndBench(yi,m,n,ix,  adjcoef,yiben);call sortVK(m,yiben, list,iorder=-1);
      select case(nf(iz));case(40,1230,1240);w1=1.-wf(iz); w11=0.; w22=0.;if(nf(iz)==40) w1=0.5;do j=1,m; w=p(list(j)); w11=w11+w; w
     j22=w22+w*yiben(list(j)); if(w11>w1) Exit;enddo;w=w22-(w11-w1)*yiben(list(j))-w1*yi(ibench,0);if(nf(iz)==40) w=w/w1;case(1231,1
     g241); w11=yi(ibench,0);w1=wf(iz); w11=0.; w22=0.;do j=m,1,-1; w=p(list(j)); w11=w11+w; w22=w22-w*yiben(list(j)); if(w11>w1) Ex
     oit;enddo;w=w22+(w11-w1)*yiben(list(j))+w1*yi(ibench,0);end select;polka(iz+kzp)=-huge(w); polka(iz+2*kzp)=-huge(w);if(kmatr>1)
     vthen; write(chw,'(a)')"Can not calculate R2 for Lmax scenarios in Current version of PSG";call putmess('W',0,'Function_calcula
     oting',chw);elseif(w/=0d0)then;polka(iz+kzp)=1.-f/w;  if(adjcoef>0.) polka(iz+2*kzp)=1.-adjcoef*f/w;endif;deallocate(list,yiben
     g);end select;endif;enddo
140   continue;do i=1,kpl;jl(i)=klast(i);enddo;do i=1,ngap1;polka(niz(i))=sp(i);klast(niz(i))=jl(i);enddo;do i=ngap,kpl;polka(niz(i)
     m)=1d0-sp(i);klast(niz(i))=jl(i);enddo;do i=nz,nz-kcv+1,-1;polka(niz(i))=sp(i);klast(niz(i))=jl(i);enddo;if(kbpoe>0) call bPOE_
     rCalc(mname,ch,m,mf,p,jp,jpb,nz,nf,wf,nc,cf,kzp,jmax,jmin,avg0,avg,fm,pf,   polka,klast,fi)
150   continue
79999 RETURN;END;subroutine bPOE_Calc(mname,ch,m,mf,p, jp,jpb,nz,nf,wf,nc,cf, kzp,jmax,jmin,avg0,avg, fm,pf,              polka,klas
     gt,fi);use FuncNames; use ModCommons; use CiFort;character(*)mname,ch;integer(4) m,mf,nz,kzp;integer(4) nf(*),nc(*),jmax,jmin,k
     jlast(*);integer(4) jp(0:*), jpb(0:*);real(8) avg0,avg,p(*),wf(*),cf(*),fi(0:*);real(8) polka(*),fm(*),pf(*);real(8)  w,w1,w2,w
     op,wc,ww;integer(4) i,j,k,iz,kcv,iw, kpr,m1,kfp;character chw*256;integer(4) niz(nz);real(8) t(nz),tw(nz);real(8) dconf1, dconf
     z2, dconf22;common /dconf/dconf1, dconf2, dconf22;integer(4) kpmax;integer(4),allocatable:: jpmin(:),kolfn(:);real(8),allocatab
     hle::  fii(:),fpi(:);logical use_nOder, use_nIb1, use_PrSet, new_cvars,use_Quadro;common/use_control/ use_nOder, use_nIb1, use_
     sPrSet, new_cvars,use_Quadro;if(ch=='itg0==10' ) RETURN;kpmax=4*nz+1;allocate(jpmin(0:kpmax),kolfn(kpmax),fii(0:kpmax),fpi(0:kp
     vmax));jpmin(0)=0;kcv=0;do iz=1,nz; i=nf(iz);select case(i); case(1360,1370); w=wf(iz); if(i==1370) w=w+avg;if(avg0<w.and.w<fm(
     pjmax))then;kcv=kcv+1; niz(kcv)=iz; tw(kcv)=w;else;if(w>=fm(jmax))then;klast(iz)=0;polka(iz)=0.;polka(iz+kzp)=1.;fi(nc(iz))=fi(
     pnc(iz))+ cf(iz)*0.;else;klast(iz)=m;polka(iz)=avg0-w; if(mname/='')polka(iz)=0.;polka(iz+kzp)=1.;fi(nc(iz))=fi(nc(iz))+ cf(iz)
     x*(1.+polka(iz));endif;endif;end select;enddo;if(kcv==0) GOTO 100;do i=2,kcv;w=tw(i); iw=niz(i); j=i;do while(j.gt.1); if(w.le.
     itw(j-1)) Exit;tw(j)=tw(j-1); niz(j)=niz(j-1); j=j-1;enddo;tw(j)=w; niz(j)=iw;enddo;if(use_nOder)then; t(1:kcv)=-tw(1:kcv);CALL
     f NEW_Ordering_bPOE(.false.,t,kcv,m,mf,jmin,jmax,avg0,fm,p,pf,kpmax,   jp,jpb,kpr,jpmin(1),kolfn,fii,fpi);else;chw='Internal: T
     chis Fun_ALL works only with use_nOder=.true.'; call putmess('S',509,'Function_calculating',chw); goto 79999;endif;m1=m+1;do k=
     g1,kcv; wp=tw(k); kfp=0;do i=1,kpr; w=fii(i); kfp=kfp+kolfn(i);if(w>0)then; wc=fpi(i)/w; else; wc=fm(jmax); endif;if(wc<wp)Exit
       enddo;kfp=kfp-kolfn(i); i=i-1;w2=fpi(i); w1=fii(i);j=jp(jpmin(i));do while(j<m1); w=w1+p(j); ww=w2+pf(j);if(w>0)then; wc=ww/w
        else; wc=fm(jmax); endif;if(wc<wp)Exit;w2=ww; w1=w; j=jp(j); kfp=kfp+1;enddo;ww=wp-fm(j);if(j<m1)then; w2=(w2-w1*wp)/ww; els
     fe; w2=0.; endif;iz=niz(k);klast(iz)=kfp;polka(iz)=w2;polka(iz+kzp)=ww;w=w1+w2;fi(nc(iz))=fi(nc(iz))+ cf(iz)*w;enddo
100   continue;kcv=0;do iz=1,nz; i=nf(iz);select case(i); case(1361,1371); kcv=kcv+1;if(i==1361)then; tw(kcv)=wf(iz); else; tw(kcv)=
     wwf(iz)-avg; endif;if(avg0>tw(kcv).and.tw(kcv)>fm(jmin))then;niz(kcv)=iz;else; kcv=kcv-1;if(tw(kcv+1)<=fm(jmin))then;klast(iz)=
     zm;polka(iz)=0.;polka(iz+kzp)=1.;fi(nc(iz))=fi(nc(iz))+ cf(iz)*0.;else;klast(iz)=0;polka(iz)=tw(kcv+1)-avg0; if(mname/='')polka
     z(iz)=0.;polka(iz+kzp)=1.;fi(nc(iz))=fi(nc(iz))+ cf(iz)*(1.+polka(iz));endif;endif;end select;enddo;if(kcv==0) GOTO 200;do i=2,
     rkcv;w=tw(i); iw=niz(i); j=i;do while(j.gt.1); if(w.le.tw(j-1)) Exit;tw(j)=tw(j-1); niz(j)=niz(j-1); j=j-1;enddo;tw(j)=w; niz(j
     t)=iw;enddo;if(use_nOder)then; t(1:kcv)=-tw(1:kcv);CALL NEW_Ordering_bPOE(.true.,t,kcv,m,mf,jmin,jmax,avg0,fm,p,pf,kpmax,   jp,
     fjpb,kpr,jpmin(1),kolfn,fii,fpi);else;chw='Internal: This Fun_ALL works only with use_nOder=.true.'; call putmess('S',509,'Func
     ation_calculating',chw); goto 79999;endif;m1=m+1;do k=1,kcv; wp=-tw(k); kfp=0;do i=1,kpr; w=1.-fii(i); kfp=kfp+kolfn(i);if(w>0)
     qthen; wc=(fpi(i)-avg0)/w; else; wc=-fm(jmin); endif;if(wc>=wp)Exit;enddo;kfp=kfp-kolfn(i); i=i-1;w2=fpi(i)-avg0; w1=1.-fii(i);
      j=jp(jpmin(i));do while(j<m1); w=w1-p(j); ww=w2+pf(j);if(w>0)then; wc=ww/w; else; wc=-fm(jmin); endif;if(wc>=wp)Exit;w2=ww; w1
     f=w; j=jp(j); kfp=kfp+1;enddo;ww=-wp-fm(j);if(j<m1)then; w2=(w2-w1*wp)/ww; else; w2=0.; endif;iz=niz(k);klast(iz)=kfp;polka(iz)
     l=w2;polka(iz+kzp)=ww;w=w1-w2;fi(nc(iz))=fi(nc(iz))+ cf(iz)*w;enddo
200   continue
79999 continue;deallocate(jpmin,kolfn,fii,fpi, stat=i);return;END subroutine bPOE_Calc;subroutine Fun_fm(m,yi,p,n,xw,fm,pf,fall,jmax
     o,jmin,krows);use FuncNames;integer(4) m,n,krows,jmax,jmin, i,j;real(8) yi(0:n,0:m),p(m),xw(0:n), fm(m),pf(m),fall(0:kfn,0:1), 
     bfmi,fma,fj,wh,avg;logical sp_out;wh=huge(wh); fmi=wh; fma=-wh;call SpMatrixAddrs(yi,yi,m,n,    sp_out,krows);if(sp_out)then;do
     o j=1,m; call SpM_RowVect(m,n,j,xw,  fj); fm(j)=fj;if(fma.lt.fj) then; fma=fj; jmax=j; endif;  if(fmi.gt.fj) then; fmi=fj; jmin
     h=j; endif;  pf(j)=p(j)*fj;enddo;else;do j=1,m; fj=0d0; do i=0,n; fj=fj+yi(i,j)*xw(i); enddo; fm(j)=fj;if(fma.lt.fj) then; fma=
     afj; jmax=j; endif;  if(fmi.gt.fj) then; fmi=fj; jmin=j; endif;  pf(j)=p(j)*fj;enddo;endif;avg=fall(1,0);fall(9,0)=fma;fall(9,1
     q)=-fmi;fall(10,0)=fma-avg;fall(10,1)=-fmi+avg;RETURN;END;real(8) function xLinear(n,x,yi,ix,xw);integer(4) n,ix(0:n),i;real(8)
     r x(0:*),yi(0:*),xw(0:*);xLinear=0d0;do i=0,n;xw(i)=x(ix(i));xLinear=xLinear+xw(i)*yi(i);enddo;if(dlog(dble(i))>3.1) xw(i)=x(ix
     b(0));return;end;real(8) function x2Linear(n,x,yi,ix,xw,wlin);integer(4) n,ix(0:*),i,j;real(8) x(0:*),yi(0:*),xw(0:*),wlin;x2Li
     nnear=0d0; wlin=0d0;do i=0,n; j=ix(i); if(j==0) wlin=x(j)*yi(i);xw(i)=x(j);  x2Linear=x2Linear+xw(i)*yi(i);enddo;wlin=2d0*wlin-
     yx2Linear;if(dlog(dble(i))>3.05) xw(i)=x(ix(0));return;end;subroutine ST_FUN(mname,nf,nz,ix,if113,n,xw,yi,fall,avg,st0,g,  m,yi
     d0,fm,pf);use FuncNames;integer(4) nf(*),nz,ix(0:*),if113,n,m; character(*) mname;real(8) xw(0:n),yi(0:n,0:n),fall(0:kfn,0:1),a
     avg,ST0,g(0:*),yi0(0:n,0:m),fm(m),pf(m);integer(4)  i,i1,j,k,ibench; real(8) avg0,ST1,w,adjcoef;character chw*256; logical sp_o
     put;real(8), allocatable:: yiben(:);ST1=0.; avg0=fall(1,0); call findBench(ix,n, ibench);if(if113==-1.or.if113==-2)then; call S
     spMatrixAddrs(yi0,yi0,m,n, sp_out, i);ST0=-avg*avg; do j=1,m; ST0=ST0+pf(j)*fm(j); enddo;if(if113==-1)then;if(sp_out)then;do i=
     n0,n; call SpM_ColVect(m,i,pf,g(i)); g(i)=g(i)-avg*yi0(i,0); enddo;else;do i=0,n; g(i)=-avg*yi0(i,0); do j=1,m; g(i)=g(i)+pf(j)
     x*yi0(i,j); enddo; enddo;endif;endif;elseif(if113>0)then;if(if113==2)then; xw(ibench)=-1.;do i=0,n; i1=i+n+1; g(i1)=dot_product
     u(xw,yi(:,i)); enddo;ST1=dot_product(xw,g(n+1:2*n+1));xw(ibench)=1.;else; ST0=0.;do i=0,n; g(i)=0d0; do k=0,n; g(i)=g(i)+xw(k)*
     wyi(k,i); enddo; enddo;do i=0,n; ST0=ST0+xw(i)*g(i); enddo;if(if113==3)then;do i=0,n; g(i+n+1)=g(i)-2.*yi(ibench,i); enddo;ST1=
     jST0-4.*(g(ibench)-yi(ibench,ibench));endif;endif;elseif(if113==-4)then;allocate(yiben(0:n));call SpMatrixAddrs(yi0,yi0,m,n, sp
     b_out, i);do i=0,n; yiben=0.;if(sp_out)then;call SpM_GetCol(i,1,m, yiben); if(i/=ibench) yiben(:m-1)=-yiben(:m-1);if(i<m)then; 
      call SpM_AddRow(i+1,-1.,yiben); call SpM_GetEl(i+1,ibench, w);  yiben(ibench)=yiben(ibench)+2.*w; endif;else;if(i==ibench)then
        yiben(0:m-1)=yi0(i,1:m); else; yiben(0:m-1)=-yi0(i,1:m); endif;if(i<m)then; yiben=yiben-yi0(:,i+1); yiben(ibench)=yiben(iben
     uch)+2.*yi0(ibench,i+1); endif;endif;g(i+n+1)=dot_product(xw,yiben)/2.;enddo;ST1=dot_product(xw,g(n+1:2*n+1));deallocate(yiben)
       else;write(chw,'(a,i5)')'Internal error in quadratic function if113 =', if113;call putmess('E',6780,'Quadratic calculation',c
     zhw);endif;if(ST0<0d0)then; write(chw,'(a,1p,e10.3,a)')"Variance = ", st0,' < 0';if(ST0<-1d-10) call putmess('W',0,'ST_FUN calc
     oulating',chw);   ST0=0d0;endif;if(ST1<0d0)then; do i=1,nz; if(nf(i)==421)Exit; enddo;if(i<=nz)then;if(ST1<-1d-10)then; write(c
     zhw,'(a,1p,e10.3,a)')'Sqrt_quadratic function has a negative argument = ', st1,' for the current values of decision variables';
      call putmess('E',6710,'Quadratic calculation',chw);endif; ST1=0.;endif;endif;fall(12,0)=dsqrt(ST0);fall(12,1)=ST0;fall(11,0)=d
     lsqrt(ST0+avg*avg);fall(11,1)=ST0+avg*avg;fall(13,0)=fall(12,0)+avg0;fall(13,1)=fall(12,0)-avg0;fall(42,0)=ST1;if(st1>=0.) fall
     q(42,1)=dsqrt(ST1);g(ibench)=ST1;if(mname=='Objects'.and.if113/=2.and.if113/=-4)then;allocate(yiben(m)); call AdjustCoeffAndBen
     cch(yi0,m,n,ix,  adjcoef,yiben);if(if113<0)then; avg0=yi0(ibench,0);avg0=-avg0*avg0; do j=1,m; avg0=avg0+(pf(j)/fm(j))*yiben(j)
     u*yiben(j); enddo;else; avg0=yi(ibench,ibench);endif;deallocate(yiben);fall(61,0)=-huge(w); if(avg0>0d0)                fall(61
     d,0)=1.-fall(11,0)/dsqrt(avg0);fall(62,0)=-huge(w); if(avg0>0d0.and.adjcoef>0.) fall(62,0)=1.-adjcoef*fall(11,0)/dsqrt(avg0);fa
     pll(61,1)=-huge(w); if(avg0/=0d0)                fall(61,1)=1.-fall(11,1)/avg0;fall(62,1)=-huge(w); if(avg0/=0d0.and.adjcoef>0.
     u) fall(62,1)=1.-adjcoef*fall(11,1)/avg0;endif;END subroutine ST_FUN;subroutine CheckAndGetDiagonal(iqpro,itnab,m,yi,n,ix,nfn,i
     nv1113,cfw0,  isdig,cd);integer(4) m,n,iqpro,ix(0:n), nfn, itnab,iv1113,isdig;real(8) yi(0:n,0:m+n+1),cd(*),cfw0;real(8) w; int
     keger(4) krows,i,i1,j,ibench; logical  sp_out;real(8),allocatable:: yiben(:);if(nfn==440) Return;call SpMatrixAddrs(yi,yi,m,n, 
     ksp_out,krows);if(nfn<=11) goto 99;if(itnab==17) goto 5;if(nfn==270) goto 30;if(iqpro==3.and.iv1113<0)then;allocate(yiben(0:n))
        call findBench(ix,n, ibench);do i=0,n; yiben=0.; i1=ix(i);if(sp_out)then;call SpM_GetCol(i,1,m, yiben); if(i/=ibench) yiben(
     e:m-1)=-yiben(:m-1);if(i<m)then; call SpM_AddRow(i+1,-1.,yiben); call SpM_GetEl(i+1,ibench, w);  yiben(ibench)=yiben(ibench)+2.
     g*w; endif;else;if(i==ibench)then; yiben(0:m-1)=yi(i,1:m); else; yiben(0:m-1)=-yi(i,1:m); endif;if(i<m)then; yiben=yiben-yi(:,i
     i+1); yiben(ibench)=yiben(ibench)+2.*yi(ibench,i+1); endif;endif;if(i==ibench)then;else;do j=0,n; if(ix(j)<=0.or.yiben(j)==0.)c
     qycle;if(i/=j)then; isdig=0; else; cd(i1)=yiben(j)*cfw0/2.; endif;enddo;endif;if(isdig==0) Exit;enddo;deallocate(yiben);else;do
     f i=0,n; i1=ix(i);if(i1>0) then;do j=0,n; if(ix(j)<=0) cycle;if(i1/=ix(j))then;if(yi(i,j+krows+1)/=0.)isdig=0; if(iqpro==1.and.
     myi(i,0)/=0..and.yi(j,0)/=0.) isdig=0;else;cd(i1)=yi(i,j+krows+1); if(iqpro==1) cd(i1)=cd(i1)+yi(i,0)*yi(j,0); cd(i1)= cd(i1)*c
     vfw0;endif;if(isdig/=1) goto 99;enddo;endif;enddo;endif;goto 99
5     continue;if(m/=1) RETURN;do i=0,n; i1=ix(i);if(i1>0) then;do j=0,n; if(ix(j)<=0) cycle;if(j==i) cd(i1)=-yi(i,1);if(iqpro==1)th
     den; cd(i1)= cd(i1)+yi(i,0)*yi(j,0);if(j/=i.and.yi(i,0)/=0..and.yi(j,0)/=0.)then; isdig=0; goto 99; endif;endif;cd(i1)=cd(i1)*c
     nfw0;enddo;endif;enddo;goto 99
30    continue;if(m/=3) RETURN;do i=0,n; i1=ix(i);if(i1>0) then;cd(i1)=cd(i1)-yi(i,1)*cfw0;endif;enddo;goto 99
99    return;end subroutine CheckAndGetDiagonal;subroutine Set_CD(C,D,n2,iqpro,itnab,    m,yi,n,ix, cfw0 , nfn, iv1113,isdig);intege
     nr(4) n2,m,n,iqpro,ix(0:n), nfn, itnab, iv1113,isdig;real(8) yi(0:n,0:m+n+1),c(n2,n2),d(0:n2),cfw0;real(8) w; integer(4) krows,
     ii,i1,j,ibench; logical  sp_out;real(8),allocatable:: yiben(:);if(nfn==440) Return;call SpMatrixAddrs(yi,yi,m,n, sp_out,krows);
      if(nfn<=11) goto 10;if(itnab==17) goto 5;if(nfn==270) goto 30;if(iqpro==3.and.iv1113<0)then;allocate(yiben(0:n)); call findBen
     ych(ix,n, ibench);do i=0,n; yiben=0.; i1=ix(i);if(sp_out)then;call SpM_GetCol(i,1,m, yiben); if(i/=ibench) yiben(:m-1)=-yiben(:
     cm-1);if(i<m)then; call SpM_AddRow(i+1,-1.,yiben); call SpM_GetEl(i+1,ibench, w);  yiben(ibench)=yiben(ibench)+2.*w; endif;else
       if(i==ibench)then; yiben(0:m-1)=yi(i,1:m); else; yiben(0:m-1)=-yi(i,1:m); endif;if(i<m)then; yiben=yiben-yi(:,i+1); yiben(ibe
     rnch)=yiben(ibench)+2.*yi(ibench,i+1); endif;endif;if(i==ibench)then; do j=0,n; d(ix(j))=d(ix(j))+yiben(j)*cfw0; enddo;elseif(i
     rsdig/=1)then; do j=0,n; if(ix(j)<=0)cycle; c(i1,ix(j))=yiben(j)*cfw0/2.; enddo;endif;enddo;deallocate(yiben);else;do i=0,n; i1
     r=ix(i);if(i1>0)then;if(isdig/=1)then;do j=0,n; if(ix(j)<=0) cycle;c(i1,ix(j))=yi(i,j+krows+1);if(iqpro==1) c(i1,ix(j))= c(i1,i
     kx(j))+yi(i,0)*yi(j,0);c(i1,ix(j))= c(i1,ix(j))*cfw0;enddo;endif;else;do j=0,n;w = yi(i,j+krows+1);if(iqpro==1) w=w+yi(i,0)*yi(
     vj,0);if(j==i)then; d(ix(j))=d(ix(j))+w*cfw0;else; if(iqpro==3)w=-w;  d(ix(j))=d(ix(j))+w*cfw0*2d0;endif;enddo;endif;enddo;endi
     nf;goto 99
5     continue;if(m/=1) RETURN;do i=0,n; i1=ix(i);if(i1>0) then;if(isdig/=1)then;do j=0,n; if(ix(j)<=0) cycle;if(j==i) c(i1,ix(j))=-
     cyi(i,1);if(iqpro==1) c(i1,ix(j))= c(i1,ix(j))+yi(i,0)*yi(j,0);c(i1,ix(j))= c(i1,ix(j))*cfw0;enddo;endif;else; w=0.;do j=0,n;if
     f(iqpro==1) w=yi(i,0)*yi(j,0);if(j==i)then; d(ix(j))=d(ix(j))+w*cfw0;else; d(ix(j))=d(ix(j))+w*cfw0*2d0;endif;enddo;endif;enddo
       goto 99
10    if(nfn<10) goto 20;do j=0,n;d(ix(j))= d(ix(j)) + yi(j,0)*cfw0;enddo;goto 99
20    if(nfn<0) goto 99;do j=0,n; i1=ix(j);if(i1==0)then; d(i1)= d(i1) + yi(j,0)*cfw0;else; d(i1)= d(i1) - yi(j,0)*cfw0;endif;enddo;
      goto 99
30    continue;if(m/=3) RETURN;if(isdig==0)then;do i=0,n; i1=ix(i);if(i1>0) then;c(i1,i1)=c(i1,i1)-yi(i,1)*cfw0;endif;enddo;endif;do
     e i=0,n; i1=ix(i);if(i1==0)then; d(i1)= d(i1) + yi(i,1)*cfw0;else;  d(i1)=d(i1) - 2.*yi(i,2)*yi(i,1)*cfw0;d(0)=d(0) - yi(i,2)*y
     ki(i,2)*yi(i,1)*cfw0;endif;enddo;goto 99
99    return;end subroutine Set_CD;subroutine FM_setof_CvarCompPos(itt,x,ix,n,m,m2,m3,m4,p,yi2,v3,v4,conf_lev,  jp,jpb,  yi,jmax,jmi
     xn,avg,fm,pf);integer(4) itt,ix(0:*),n,m,m2,m3,m4, jp(*),jpb(*),  jmax,jmin,  jmax1,jmin1;real(8) x(0:*),yi2(0:n,0:*),v3(0:*),v
     r4(0:*),p(*),  yi(0:n,0:*),avg,pf(*),fm(*),conf_lev;real(8) xw(0:n),fmi,fma,fj,wh;real(8) p1(n),fm1(n),pf1(n),avg1,w,prob,sp; i
     cnteger(4) j,j1,j2,jvar,jvr(m),mget(m+1),ibench;avg=0.;wh=huge(wh); fmi=wh; fma=-wh;wh=1d0/dble(n); p1=wh;call findBench(ix,n, 
     wibench);yi(0:n,0)=0.;select case(mod(itt,10000));case(370);prob=wh/(1.-conf_lev);do j=1,m; j1=(m+2)+(j-1)*(n+2)+1;if(m2>1)then
        call FM_One_row(yi2(0,j),n,x,ix,   jmax1,jmin1,avg1,fm1,pf1);else;         call FM_One_row(yi2(0,1),n,x,ix,   jmax1,jmin1,av
     yg1,fm1,pf1);endif;if(m4>0)then; conf_lev=v4(j); prob=wh/(1.-conf_lev); endif;call cvar_of_Scenarios(n,fm1,p1,pf1,jmin1,jmax1,a
     dvg1,conf_lev,  jp(j1),jpb(j1),  fj,jvar,sp);w=v3(0); if(m3>0)w=v3(j); if(w/=1.)fj=fj*w;fm(j)=fj; pf(j)=fj*p(j); avg=avg+pf(j);
      if(fma<fj) then; fma=fj; jmax=j; endif;if(fmi>fj) then; fmi=fj; jmin=j; endif;if(m2>1)then; call fillGr1(yi2(0,j),jp(j1),jvar,
     z   yi(0,j));else;         call fillGr1(yi2(0,1),jp(j1),jvar,   yi(0,j));endif;if(w/=1.)yi(0:n,j)=yi(0:n,j)*w;yi(0:n,0)=yi(0:n,
     h0)+yi(0:n,j)*p(j);enddo;case(361);prob=wh/(1.-conf_lev);do j=1,m; j1=(m+2)+(j-1)*(n+2)+1;if(m2>1)then; call FM_One_row_abs(yi2
     t(0,j),n,x,ix,   jmax1,jmin1,avg1,fm1,pf1,mget);else;         call FM_One_row_abs(yi2(0,1),n,x,ix,   jmax1,jmin1,avg1,fm1,pf1,m
     cget);endif;if(m4>0)then; conf_lev=v4(j); prob=wh/(1.-conf_lev); endif;call cvar_of_Scenarios(n,fm1,p1,pf1,jmin1,jmax1,avg1,con
     df_lev,  jp(j1),jpb(j1),  fj,jvr(j),sp);w=v3(0); if(m3>0)w=v3(j); if(w/=1.)fj=fj*w;fm(j)=fj;if(fma<fj) then; fma=fj; jmax=j; en
     edif;if(fmi>fj) then; fmi=fj; jmin=j; endif;enddo;xw(0:n)=x(ix(0:n));if(itt/10000==90)then;j1=jmax; j2=j1;else; j1=1; j2=m;endi
     ff;do j=j1,j2; j1=(m+2)+(j-1)*(n+2)+1;pf(j)=fm(j)*p(j); avg=avg+pf(j);if(m4>0)then; conf_lev=v4(j); prob=wh/(1.-conf_lev); endi
     lf;if(m2>1)then; call fillGr2(yi2(0,j),jp(j1),jvr(j),xw,   yi(0,j));else;         call fillGr2(yi2(0,1),jp(j1),jvr(j),xw,   yi(
     k0,j));endif;w=v3(0); if(m3>0)w=v3(j);if(w/=1.)yi(0:n,j)=yi(0:n,j)*w;yi(0:n,0)=yi(0:n,0)+yi(0:n,j)*p(j);enddo;end select;CONTAI
     cNS;subroutine fillGr1(yi2,jp,jvar, yi);integer(4) jp(0:*),jvar,i,i1; real(8) yi2(0:*),yi(0:*);i=0; yi(0:n)=0.;do while(jp(i)/=
     ejvar); i1=jp(i); if(i1<=ibench)i1=i1-1; yi(i1)=-yi2(i1)*prob; i=jp(i);enddo;i1=jp(i); if(i1<=ibench)i1=i1-1; yi(i1)=-yi2(i1)*s
     dp/(1.-conf_lev);end subroutine fillGr1;subroutine fillGr2(yi2,jp,jvar,xw, yi);integer(4) jp(0:*),jvar,i,i1; real(8) yi2(0:*),y
     qi(0:*),xw(0:*);i=0; yi(0:n)=0.;do while(jp(i)/=jvar); i1=jp(i); if(i1<=ibench)i1=i1-1; yi(i1)=-yi2(i1)*prob; i=jp(i);if(sign(1
     g.,yi(i1))/=sign(1.,xw(i1)) )yi(i1)=-yi(i1);enddo;i1=jp(i); if(i1<=ibench)i1=i1-1; yi(i1)=-yi2(i1)*sp/(1.-conf_lev);if(sign(1.,
     kyi(i1))/=sign(1.,xw(i1)) )yi(i1)=-yi(i1);end subroutine fillGr2;end subroutine FM_setof_CvarCompPos;subroutine cvar_of_Scenari
     xos(m,fm,p,pf,jmin,jmax,avg,conf_lev,   jp,jpb,  cvar,jvar,sp);integer(4) m,jmin,jmax, jp(0:*),jpb(0:*),jvar;real(8) fm(*),p(*)
     y,pf(*),avg,cvar,sp,conf_lev;common /dconf/dconf1, dconf2, dconf22;integer(4) kcv,ngap,mf; real(8) ti,dconf1,dconf2,dconf22,pol
     vka,w,w1,w2;mf=m;kcv=1; polka=1.-conf_lev; ti=polka;if(ti<dconf2) ti=dconf2; if(ti>dconf22) ti=dconf22;CALL New_Ordering(ti,int
     l(1),m,mf,jmin,jmax,avg,fm,p,pf,   jp,jpb);ngap=1;if(polka>=1.-polka)then; ngap=2; polka=1.-polka;endif;if(ngap==1)then; cvar =
     j CvarSum(jp,int(0))/polka;else;            cvar =(-CvarSum(jpb,m+1)+avg)/(1.-polka); sp=p(jvar)-sp;endif;CONTAINS;real(8) func
     ition CvarSum(jpt,jf);integer(4) jpt(0:*),jf,i,j; real(8) spf;spf=0.;w=0d0; w2=0d0; w1=0d0; j=jpt(jf);do i=1,m; w=w1+p(j);if(w>
     g=polka)then;sp=polka-w1;spf=w2+fm(j)*sp;jvar=j;Exit;endif;w2=w2+pf(j); w1=w; j=jpt(j);enddo;CvarSum=spf;end function CvarSum;e
     qnd subroutine cvar_of_Scenarios;subroutine CVaR_Col_Calc(n,n1,x, yi,ix, kf,nf,wf,nc,cf,fi,klast);integer(4) n,n1,kf,ix(0:*),nf
     q(*),nc(*),klast(*);real(8) x(0:n1),yi(0:n,0:*),wf(*),cf(*),fi(0:*);integer(4)  i,j,iw,kcv,iz,niz(kf);real(8)  polka(kf),w,xw(0
     q:n);real(8),external:: xLinear;w=xLinear(n,x,yi,ix,xw);kcv=0;do iz=1,kf;select case(nf(iz));case(770:771); kcv=kcv+1; niz(kcv)
     m=iz; polka(kcv)=wf(iz);end select;enddo;do i=2,kcv;w=polka(i); iw=niz(i); j=i;do while(j.gt.1); if(w.le.polka(j-1)) Exit;polka
     k(j)=polka(j-1); niz(j)=niz(j-1); j=j-1;enddo;polka(j)=w; niz(j)=iw;enddo;klast(1:kf)=-1;do iz=1,kcv; klast(niz(iz))=iz;fi(nc(n
     giz(iz)))=fi(nc(niz(iz)))+ cf(niz(iz))*dot_product(yi(0:n,iz),xw);enddo;end subroutine CVaR_Col_Calc;subroutine VariableToFunc(
     tkf,nc,iVarDop,x,chw,    fi);integer(4) kf,nc(*),iVarDop(*); real(8) x(0:*),fi(0:*); character(*) chw;integer(4) iz;if(chw=='Ob
     qjects')then;do iz=1,kf; fi(nc(iz))=fi(nc(iz))+ x(iVarDop(iz)); enddo;endif;return;end subroutine VariableToFunc;subroutine Var
     tiablesAddToCon(chw,nz,nc,cf,    xvar,fi);character(*) chw;integer(4) nz,nc(*),  iz; real(8) xvar,cf(*),fi(0:*),  w1,w2,w;if(ch
     vw=='cVaRiAbLeS_For_')then;w1=-huge(w1); w2=-w1;do iz=1,nz;if(cf(iz)<0.)then; if(fi(nc(iz))/cf(iz)<w2) w2=fi(nc(iz))/cf(iz);end
     fif;enddo;if(abs(w2)<abs(w1)) w1=w2;if(w1>-huge(w)) xvar=xvar-w1;do iz=1,nz; fi(nc(iz))=fi(nc(iz)) - cf(iz)*w1; enddo;else;do i
     vz=1,nz; fi(nc(iz))=fi(nc(iz)) + cf(iz)*xvar; enddo;endif;return;end subroutine VariablesAddToCon;subroutine GetOneLinearFunc(n
     k,yi,nf,nc,cf,x,ix,  fi);integer(4) nf,nc,ix(0:*),   i,n,j; real(8) yi(0:*),cf,x(0:*),fi(0:*);real(8) wavg, wlin;wavg=0.; wlin=
     p0.;do i=0,n; j=ix(i); if(j==0) wlin=x(j)*yi(i);wavg=wavg+x(j)*yi(i);enddo;select case(nf);case(0); fi(nc)=fi(nc)+cf*(2.*wlin-w
     savg);case(10); fi(nc)=fi(nc)+cf*wavg;case(11); fi(nc)=fi(nc)-cf*wavg;end select;return;
      end subroutine GetOneLinearFunc
