      subroutine ralgb4_St(mxmem,klin,intarr,bul,dparr,n0,alp,nh,q1,q2,intp,iprint,iriab,
     +kconstr,maxitn,epsx0,epsg0,fkmin0,timelimit,estmin,del0,
     +x,h0,b,
     +fr,xr,kiter,iend,
     +g,g1,g2,  wch, wf0,wfc);USE ModCommons;real(8) enk,xbndhuge; real(8), pointer::xl(:),xu(:);common/shr/enk,xbndhuge,xl,xu;integ
     ler(4) mxmem,klin,intarr(*);real(8) bul(*),dparr(*);integer(4) n0,nh,kconstr,maxitn,intp,iprint,itn,iend,lp,nls,nlsa,ls,lsa, ko
     lldr,istep,kiter;integer(4) n,i; integer(1) iriab, ikold;real(8) g(n0),x(n0),b(n0,n0),xr(n0),g1(n0),g2(n0),vectmod,prodvect,d_t
     lime,estmin,del,del0,f00r,fppp,enkp,fp,fd,fiir;real(8),allocatable:: x1(:),xt(:),xd(:),gd(:);character(*) wch;real(8) alp,h0,q1
     l,q2,f,fr,wf0,wfc,dzero,w,hs,fii,fiiw,dx,dg,d,f0,f0r, frr,bet,timelimit,
     +epsx0,epsg0,fkmin0, epsx,epsg,fkmin, enkmin;common/iterex/itn;interface;subroutine calcfg_St(istep,enkmin,ls,mxmem,klin,intarr
     l,bul,dparr,itn,n,f,g,x,xt,f0,fim,iriab,kconstr,eps0, wf0,wfc);integer(4) istep,ls,mxmem,klin,itn,n,kconstr; integer(4),target:
     l:intarr(*); integer(1) iriab;real(8) enkmin,bul(3,*),dparr(*),f,g(n),x(n),xt(n),f0,fim,eps0,wf0,wfc;end;end interface;dzero=1.
     ld-50;  n=n0; bet=1./alp-1.; hs=h0; ls=0; del=del0;itn=0; lp=itn+intp;fkmin=dmax1(10.*fkmin0,0.1); epsx=dmax1(10.*epsx0,0.1); e
     lpsg=dmax1(10.*epsg0,0.1);allocate(x1(n0),xt(n0),xd(n0),gd(n0)); enkmin=0;istep=0;call calcfg_st(istep,enkmin,ls,mxmem,klin,int
     larr,bul,dparr,itn,n,f,g,x,xt,f0,fii,iriab,kconstr,fkmin, wf0,wfc);if(ioutk==istop) goto 3999;fr=f; frr=f; fiiw=fii*wfc; f0r=f0
     l; fiir=fii; f00r=f0; fppp=fii;if(iriab<=1)then; enkp=enk;if(f0+enk*fii/=f)then; wch='Internal error 1 Bul'; call putmess('S',4
     l07,'Optimization',wch);if(lf22) write(22,'(/a,99e20.12)')'must be f0+enk*fii-f=0 ',f0,enk,fii,f,f0+enk*fii-f;goto 3999;endif;e
     lndif;xr=xt;koldr=int(-5*alog(10.*n));call StepGradCalc(g);if(ioutk==istop) goto 3999;g1=g;nls=0; nlsa=0; ls=0;if(intp.ge.0) th
     len;  write(iprint,3000); write(iprint,3100) itn,f,fr,fii,nlsa,nls,ls,  dx,hs,dg;endif;itn=0
1000  iend=0;do 2000 itn=itn+1,maxitn;  if(VectMod(g,n)<=epsg) iend=2;call MatrMultVect(g2,b,g,n,n);call DiffVect(g,g2,g1,n);call Ed
     linVect(g,g,n);d=bet*ProdVect(g,g2,n);call VectSumAVect(g1,g2,d,g,n);call EdinVect(g2,g1,n);do i=1,n; d=bet*ProdVect(b(1,i),g,n
     l);call VectSumAVect(b(1,i),b(1,i),d,g,n);enddo;call VectMultMatr(g,g2,b,n,n);dg=VectMod(g,n);ls=0; lsa=0; dx=0.d0;d=1d0;  ikol
     ld=1;x1=x;do while(d>0d0.and.ls<=500);ls=ls+1;  dx=dx+hs*dg; call VectSumAVect(x,x,-hs,g,n);istep=0; fp=f;call calcfg_st(istep,
     lenkmin,ls,mxmem,klin,intarr,bul,dparr,itn,n,f,g2,x,xt,f0,fii,iriab,kconstr,fkmin, wf0,wfc);if(ioutk==istop) goto 3999;if(iriab
     l<=1)then; w=f0+enk*fii;if(w/=f)then; wch='Internal error 2 Bul'; call putmess('S',407,'Optimization',wch);if(lf22) write(22,'(
     l/a,99e20.12)')'must be f0+enk*fii-f=0 ',f0,enk,fii,f, f0+enk*fii-f;goto 3999;endif;if(enk/=enkp)then; fr=f0r+enk*fiir; enkp=en
     lk; frr=f00r+enk*fppp;endif;endif;if(f <= fr) then; fr=f; f0r=f0; fiir=fii; fiiw=fii*wfc; xr=xt;if((frr-f)*2/(dabs(frr)+dabs(f)
     l)>fkmin.and.(frr-f)*dabs(wf0)>fkmin) then; frr=f; ikold=0; f00r=f0; fppp=fii;endif;endif;if(ls.gt.nh) hs=hs*q2;if(f>fp) d=-1.;
      enddo;del=min(10*hs,del);call StepGradCalc(g2);if(ioutk==istop) goto 3999;d=ProdVect(g,g2,n);if(ls.eq.1) hs=hs*q1;nls=nls+ls; 
       nlsa=nlsa+ls;g=g2;if(itn.ge.lp.and.intp.ge.0) then; write(iprint,3100) itn,f,fr,fii,nlsa,nls,ls   ,dx, hs,dg;nlsa=0;  lp=lp+i
     lntp;endif;if(dg<1d-10) then; hs=h0;b=0d0; do i=1,n; b(i,i)=1d0; enddo;endif;if(dabs(dx).lt.epsx.and.koldr>25) iend=3;if(itn==m
     laxitn) iend=4;call Check_stop_whatch(1,w); if(w>timelimit) iend=23;if(ls > 500) iend=5;do i=1,n; w=2d0*xt(i); if(w>xbndhuge.or
     l.w<-xbndhuge)iend=5; enddo;if(ikold>0) then; koldr=koldr+1;if(koldr > 100*alog10(n+0.5)+10) iend=1;else; koldr=0;endif;if((f0r
     l*dabs(wf0)-estmin)<fkmin.and.fiiw<fkmin) iend=1;wch='';if(d_time('s',int2(2))>2.or.iend.ne.0.or.itn==1) then;d=d_time('s',int2
     l(0)); write(wch,'(i10.10)')itn; i=verify(wch,'0'); if(i<1.or.i>10)i=10;write(wch,'(a,2(a,e18.12))')'Ext.iteration='//wch(i:10)
     l,'  Objective=',f0r*wf0,'  Residual=',fiiw;endif;call putmess('n',0,' ',wch);if(iend/=0.or.wch=='S'.or.inpk==1) Exit
2000  enddo;h0=1d0; if(dg/=0d0) b=b/dg;if(.not.(wch=='S'.or.inpk==1).and.(fkmin>fkmin0.or.epsx>epsx0.or.epsg>epsg))then;if(fiir>dsqr
     lt(fkmin))then; enkmin=min(1d7,dmax1(enk,enkmin)*5.);if(iend==5) x=x1;endif;if(intp.ge.0) write(iprint,'(a)')'-----------------
     l----------------------------------------------------------------------';fkmin=dmax1(fkmin0,fkmin/10.); epsx=dmax1(epsx0,epsx/1
     l0.); epsg=dmax1(epsg0,epsg/10.);hs=epsx*10.;b=0d0; do i=1,n; b(i,i)=1.; enddo;goto 1000;endif;if(intp.ge.0) then;  write(iprin
     lt,3100) itn,f,fr,fii,nlsa,nls,ls,   dx, hs,dg;endif;goto 3999
 3000 format(/10x,' Listing of nonsmooth optimization process'/2x,
     +'  Itn.',7x,'..f(x)..',12x,'..f(x_r)..',9x,'..fii(x)..',
     +7x,'nlsa',4x,'nls',4x,'ls', 9x,'Last_dx',16x,'hs',14x,'|unit_g2*B|')
 3100 format(2x,i5,3(2x,1pd18.10),3(2x,i5),3(2x,e19.10))
 3999 kiter=itn;deallocate(x1,xt,xd,gd);return;CONTAINS;subroutine StepGradCalc(g2);real(8) g2(n);xd=x; istep=1;do i=1,n; xd(i)=xd(i
     l)+del;call calcfg_st(istep,enkmin,ls,mxmem,klin,intarr,bul,dparr,itn,n,fd,g2,xd,xt,f0,fii,iriab,kconstr,fkmin, wf0,wfc);if(iri
     lab<=1)then; w=f0+enk*fii;if(w/=fd)then; wch='Internal error 2 Bul'; call putmess('S',407,'Optimization',wch);if(lf22) write(22
     l,'(/a,99e20.12)')'must be f0+enk*fii-fd=0 ',f0,enk,fii,fd,f0+enk*fii-fd;RETURN;endif;if(enk/=enkp)then; fr=f0r+enk*fiir; enkp=
     lenk; frr=f00r+enk*fppp;endif;endif;if(fd<= fr) then; fr=fd;f0r=f0; fiir=fii; fiiw=fii*wfc; xr=xt;if((frr-fd)*2/(dabs(frr)+dabs
     l(fd))>fkmin.and.(frr-fd)*dabs(wf0)>fkmin) then; frr=fd; ikold=0; f00r=f0; fppp=fii;endif;endif;xd(i)=x(i);gd(i)=(fd-f)/del;end
     ldo;g2=gd;end subroutine StepGradCalc;end subroutine ralgb4_St;subroutine calcfg_St(istep,enkmin,ls,mxmem,klin,intarr,bul,dparr
     l,itn,n,f,g,x,xt,f0,fim,iriab,kconstr,eps0, wf0,wfc);USE ModCommons;integer(4) istep,ls,mxmem,klin,itn,n,kconstr; integer(4),ta
     lrget::intarr(*); integer(1) iriab;real(8) enkmin,bul(3,*),dparr(*),f,g(n),x(n),xt(n),f0,fim,eps0,wf0,wfc;integer(4) ib,i,n1,it
     ln0,   imax,lsp;integer(4) kt,j;  integer(2) itg;  integer(1) ig(n),ivar;real(8) wm,w,fii,angle(n),alp,eps,pi2a,t01,t3,t,prodve
     lct, eps2,   fiip,enkp;real(8),allocatable:: xi(:,:);real(8), pointer::xl(:),xu(:);real(8) enk,xbndhuge, wh1,wh2;common/shr/enk
     l,xbndhuge,xl,xu;character chw*128;integer(4),pointer::kac(:); real(8) alr,w1,w0;real(8),allocatable:: gii(:,:),ge(:),gn(:),fc(
     l:); integer(4),allocatable::kbt(:);ib=max0(1,kconstr+klin);allocate(gii(n,ib),ge(n),gn(n),fc(ib), kbt(ib),stat=i);if(i/=0)then
     l; chw='Can not allocate arrays'; call putmess('S',417,'Ralg_Calcfg',chw); goto 79999;endif;eps2=eps0/1d2; eps=eps0;kac=>intarr
     l(mxmem+1-n-1:mxmem);ivar=3;if(itn==0) then; itn0=itn; alr=0.5; w1=0d0; w0=w1; endif;xt(1:n)=x(1:n);if(ivar==3) then;alp=1.1d0;
      pi2a=dasin(1d0)/alp;t01=5d-1/dsin(pi2a);t3=t01*2d0*pi2a;ig=1;  wh1=xbndhuge/2d0; wh2=xbndhuge*1.9d0;do i=1,n; wm=xu(i)-xl(i);i
     lf(wm>wh2) Cycle;if(wm<wh1) then;if(wm<=0d0) then; xt(i)=xu(i); ig(i)=0; Cycle; endif;w=xt(i)-xu(i); ib=0;if(w>0d0)then; ib=int
     l(w/wm+1); xt(i)=xt(i)-ib*wm;else; w=xl(i)-xt(i);if(w>0d0)then; ib=int(w/wm+1); xt(i)=xt(i)+ib*wm;endif;endif;if(mod(ib,2)>0)th
     len; xt(i)=xu(i)+xl(i)-xt(i); ig(i)=-1;endif;if(1d0<=alp.and.alp<10.) then;w=0.5d0*(xu(i)+xl(i)); angle(i)=2d0*pi2a*(xt(i)-w)/w
     lm;if(itn>0)then; xt(i)=t01*wm*dsin(angle(i))+w;else;if(ib==0) x(i)=dasin((xt(i)-w)/(t01*wm))/(2d0*pi2a/wm)+w;endif;ig(i)=int(i
     lg(i)*2,1);endif;else;w=xt(i)-xu(i);if(w>0d0)then;   xt(i)=xu(i)+xu(i)-xt(i); ig(i)=-1;else; w=xl(i)-xt(i);if(w>0d0)then; xt(i)
     l=xl(i)+xl(i)-xt(i); ig(i)=-1;endif;endif;endif;enddo;endif;n1=n;select case(iriab);case(0);   itg=50;case(1,2); itg=51;case(3)
     l;   itg=52;end select;CALL CalcFuns(n1,xt,itg,       f,fc,kbt, wf0,wfc,w);if(ioutk==istop) goto 79999;if(klin<=0) goto 100;do 
     lj=itg+1,itg+klin; fc(j)=0d0; do i=1,n; gii(i,j)=0d0; enddo; enddo;do i=1,n; do j=kac(i),kac(i+1)-1; gii(i,itg+intarr(j))=dparr
     l(j); enddo; enddo;kt=0;do j=itg+1,itg+klin; kt=kt+1; w=ProdVect(gii(1,j),xt,n); wm=bul(2,kt);fc(j)=0d0; kbt(j)=1;  if(wm==bul(
     l1,kt)) kbt(j)=20;if(w>wm)then; w=w-wm; if(iriab>=2.and.dabs(wm)/=0d0) w=w/dabs(wm); fc(j)=w;else; wm=bul(1,kt);if(w<wm)then; w
     l=wm-w; if(iriab>=2.and.dabs(wm)/=0d0) w=w/dabs(wm); fc(j)=w;endif;endif;enddo;itg=int(itg+klin,2)
100   continue;fim=0d0; f0=f; imax=-1;do i=1,itg; if(fc(i)>fim)then; fim=fc(i); imax=i; endif; enddo;fii=fim;if(ioutk==istop) goto 7
     l9999;if(ivar==3) then;do i=1,n;if(ig(i)<0)then;  g(i)=-g(i);do j=1,itg; gii(i,j)=-gii(i,j); enddo;elseif(ig(i)==0) then; g(i)=
     l0d0;do j=1,itg; gii(i,j)=0d0; enddo;endif;if(abs(ig(i))>1) then; w=t3*dcos(angle(i)); g(i)=g(i)*w;do j=1,itg; gii(i,j)=gii(i,j
     l)*w; enddo;endif;enddo;endif;fii=fc(1);select case(iriab);case(0,1);fii=fim; if(imax>0) gii(1:n,1)=gii(1:n,imax);if(fii>0d0) t
     lhen;if(istep==0)then;w=ProdVect(g,g,n); wm=ProdVect(g,gii,n);enk=dmax1(enk*0.985,enkmin);if(wm/=0d0)then;w=min(1d9,2*dabs(w/wm
     l));if(enk<w) enk=dmin1(w,1d9);endif;endif;f=F+ENK*fii; call VectSumAVect(g,g,enk,gii,n);endif;lsp=ls; fiip=fii;if(ls==1) enkp=
     lenk;case(2);if(fii>eps) then;  f=F+1d13*FII; call VectMultScal(g,gii,1d0,n);endif;case(3);ge=0d0; gn=0d0; wm=0d0; w=0d0;do j=1
     l,itg;if(kbt(j)/=20) then; if(fc(j)<=0d0)Cycle; wm=wm+fc(j);call EdinVect(gii(1,j),gii(1,j),n); call VectSumAVect(gn,gn,1d0,gii
     l(1,j),n);else;  w=w+fc(j);if(fc(j)>eps) then;call EdinVect(gii(1,j),gii(1,j),n); call VectSumAVect(ge,ge,1d0,gii(1,j),n);endif
      endif;enddo;call EdinVect(ge,ge,n);fii=wm+w;if(wm>eps) then;  f=wm*1d10;call EdinVect(gn,gn,n);if(ProdVect(ge,gn,n)<=0d0) then
      call VectMultScal(g,gn,1d0,n);else;call VectSumAVect(g,gn,1d0,ge,n); call VectMultScal(g,g,0.5d0,n);endif;else;call EdinVect(g
     l,g,n);if(itn>=itn0+10) then; itn0=itn;w0=w1; w1=w;if(itn>=20) then;if(w1<=0.00000000001) then; alr=alr-0.1;else;t=1+100/10;if(
     lw0/w1<dexp(dlog(w1/eps)/t)) then; alr=alr+0.1;else; alr=alr-0.1;endif;endif;endif;endif;if(alr>0.8)alr=0.8; if(alr<0.2)alr=0.2
      if(w<=eps) then;  f=F+w;call VectSumAVect(g,g,1d0,ge,n); call VectMultScal(g,g,0.5d0,n);else;  f=1d14*eps*(1d0+w);if(ProdVect(
     lge,g,n)<0d0) then;call VectMultScal(g,ge,1d0,n);else;w=(1d0-alr)/alr;call VectSumAVect(g,ge,w,g,n); call VectMultScal(g,g,alr,
     ln);endif;endif;endif;end select
79999 deallocate(gii,ge,gn,fc,kbt,  stat=i);if(allocated(xi)) deallocate(xi,  stat=i);return;
      end subroutine calcfg_St
