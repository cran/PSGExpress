      subroutine opti (aij0,inda0,aij,inda,jas,kb1,kb2,kb3,jbas,at,jats
     *,mark,kac,colend,colendx
     *,ketac,rng,xs,ketah,iinda,pivrows,pivots
     *,objrow,djrow,colw,pi,inref,nrc)
      include 'mifdp.inc'
      include 'mifcomx.inc'
      integer(4) NRC,ISTOP,LOG_PR,INPK,INPP,IDB;real(8) FEAS0,FEAS1,FEASCUM,BTRA0,BTRA1,BTRACUM,FTRA0,FTRA1,FTRACUM,PRIM0,PRIM1,PRIM
     lCUM,DJCA0,DJCA1,DJCACUM,FECH0,FECH1,
     +FECHCUM,PIDE0,PIDE1,PIDECUM,PIVT0,PIVT1,PIVTCUM,TRSF0,TRSF1,TRSFCUM;integer(1) un; common/un/un;character workpath*256;common 
     l/contr/ inpk,inpp,log_pr,istop,iDB, workpath;integer jas,jbas,jats;integer(2) inref(nrc);integer inda0(*),inda(*),pivrows(m1);
      integer kb1(m1), kb2(m1), kb3(m1);integer mark(nrc),kac(nrc),colend(n1),colendx(n1),ketac(jbas);integer ketah(jjas),iinda(jjas
     l);double precision rng(nrc),xs(nrc+1);double precision aij0(*),aij(*),at(jats),objrow(*);double precision pivots(m1),pi(m1),dj
     lrow(nrc);real   colw(nrc);integer(4) jjj;double precision obj,obj1,w;integer itn1;common /timing/ feas0,feas1,feascum,btra0,bt
     lra1,btracum
     \,ftra0,ftra1,ftracum,prim0,prim1,primcum,djca0,djca1,djcacum
     \,fech0,fech1,fechcum
     \,pide0,pide1,pidecum, pivt0,pivt1,pivtcum, trsf0,trsf1,trsfcum;intrinsic abs;w=objrow(1);jjj=jjas/3-1;call prim (aij0,inda0,  
     l  at,jats,mark,kac,colend,colendx,
     *rng,nrc,xs,     djrow,colw);if (ktra.eq.2) then;call tra1 (ketac,xs,n,neta+1);endif;if (inop1.eq.0) then;iposi=1;else;itn1=min
     lr;obj1=0.0;if (ninf.gt.0) obj1=sinf;call ftra (aij,inda,jas,at,jats,ketac,ketah,
     *pivrows,pivots);majr=majr+1;call djca (kb3,m,at,jats);if (ivan.ne.1) iposi=3
10    if (ketpnt.gt.(jas-m-2)) then;iposi=6;ivan=0;endif;if (ivan.ne.1) goto 20;idgtyp=0;if (ninf.gt.0) then;call pide
     \(kb1,kb2,kb3,m,at,jats,xs,rng,
     \jjj,iinda(1),iinda(jjj+1));if(istop>0) RETURN;else;call pivt (kb1,kb2,m,at,jats,xs,rng,kb3);endif;if (ipiro.eq.-1) iposi=2;if 
     l(ms.eq.0) iposi=4;if (iposi.gt.0) then;ivan=0;else;call trsf (aij,inda,jas,kb1,kb2,kb3
     *,at,jats,mark,ketac,rng,xs,ketah
     *,pi,djrow,colw,inref,kac,colend
     *,pivrows,pivots);if (devex.eq.1) then;if (devfail.eq.1) goto 99;endif;ivv=0;if (iposi.eq.7.and.devreq.gt.0) then;goto 99;endif
      kcik=kcik+1;if (kcik.gt.kmax) then;dk=deln;iposi=5;kellin=1;kcik=0;goto 99;endif;if (devex.eq.1) then;goto 20;endif;if (minr.e
     lq.itn1+mm) then;obj=0.0;if (ninf.gt.0) obj=sinf;if (abs(obj-obj1).lt.erel) then;ivan=0;goto 10;endif;endif;call djcx (kb3,m,at
     l,jats);endif;goto 10
20    if (iposi.gt.0.and.iposi.lt.6.and.minr.ne.itn1) iposi=0;if (log_pr>-1 .and. minr.gt.idupr) call dump (kb1,mark,xs);if (neta.gt
     l.ivpr) iposi=5;endif
99    return;end subroutine opti;subroutine feas (kb1,kb2,kb3,   at,     xs,rng   )
      include 'mifcomx.inc'
      integer(1) un; common/un/un;integer kb1,kb2,kb3;double precision at,xs,rng;dimension kb1(*),kb2(*),kb3(*),at(*),xs(*),rng(*);i
     lnteger i,it;double precision tzer,xbi;tzer=dk;ninf=0;sinf=0.0;do 10 i=1,m;at(i)=0.0;kb3(i)=0;it=kb2(i);xbi=xs(kb1(i));if (xbi.
     lne.0.0.and.it.ne.3) then;if (xbi.lt.-tzer) then;kb3(i)=1;at(i)=1.0;ninf=ninf+1;sinf=sinf+xbi+tzer;else;if (it.lt.2.and.xbi.gt.
     lrng(kb1(i))+tzer) then;kb3(i)=2;at(i)=-1.0;ninf=ninf+1;sinf=sinf-xbi+rng(kb1(i))+tzer;endif;endif;endif
10    continue;if (ninf.eq.0) then;at(mc)=ez;else;at(mc)=0.0;endif;if (idyn.eq.1) then;do 31 i=1,m1;if (kb1(i).le.n1) then;at(m1+i)=
     l1.0;else;at(m1+i)=0.0;endif
31    continue;endif;if (ideg.eq.1) then;do 32 i=1,m;xbi=xs(kb1(i));it=kb2(i);at(m2+i)=0.0;if (xbi.le.0.0.and.xbi.ge.-tzer) at(m2+i)
     l=1.0;if (it.eq.1.and.xbi.ge.rng(kb1(i)).and.
     *xbi.le.rng(kb1(i))+tzer) at(m2+i)=-1.0
32    continue;at(m3) = 0.0;endif;if (iadc.eq.1.and.ninf.gt.0) then;do 20 i=m3+1,m4; at(i)=0.0
20    continue;at(m3+mc)=ez;endif;return;end subroutine feas;subroutine prim
     \(aij,inda,    at,jats,mark,kac,colend,colendx
     \,rng,nrc,xs,      djrow,colw)
      include 'mifcomx.inc'
      integer(1) un; common/un/un;integer jats,nrc;integer inda(nzr),mark(nrc);integer kac(n1+1),colend(n1),colendx(n1);double preci
     lsion aij(nzr),at(jats),djrow(nrc);double precision xs(nrc),rng(nrc);real   colw(nrc);integer km,mj,ican;double precision sj,sg
     l,fks,din,deg,adc,sdjx;common /minit/  sj(6),sg(6),fks(6)
     1,din(6),deg(6),adc(6),sdjx(6),km(6),mj(6),ican(6);integer i,j,k,mm1,js,jm,mrj,jav,ja,jf,jsc,jsize;double precision bdav,djp,dj
     ln,dj1,www,wd,   wdyn,wdeg,wadc;intrinsic abs,max,dmax1;pointer (pwd1,wdyn),(pwd2,wdeg),(pwd3,wadc);pwd1=loc(wd1); pwd2=loc(wd2
     l); pwd3=loc(wd3);mm1=0; bdav=0.;if ((ivv.eq.1.and.iapr.eq.1).or.iapr.eq.2) then;kvec=n1;endif;if (iadc.eq.1.and.ninf.gt.0) the
     ln;bdav=bda;if (abs(xs(n+1)).gt.eopt) bdav=abs(sinf*bda/xs(n+1));endif;inop1=0;inop2=0;mm=0;k=1;j=0
208   j=j+1;if (j.le.m) then;jm=j+n1;goto 260;endif;k=2;jsc=0
203   jsc=jsc+1;if (jsc.gt.nsc) goto 250;nsp=nsp+1;if (nsp.gt.nsc) nsp=1;jsize=ksv(nsp)-ksk(nsp)+1;js=0;mm1=0;j=ksp(nsp)
210   js=js+1;if (js.gt.jsize) goto 205;j=j+1;if (j.gt.ksv(nsp)) j=ksk(nsp);jm=j
260   jav=0;mrj=mark(jm);if (mrj.gt.3.and.mrj.lt.9) goto 211;if (mrj.eq.0) goto 211;if (djset.eq.1) then;dj = djrow(jm) * ez;else;if
     l (k.eq.1) then;dj=at(jm-n1);else;djp=0.0;djn=0.0;do 30 i=kac(jm),colendx(jm);dj1=at(inda(i))*aij(i);if (dj1.gt.0) then;djp=djp
     l+dj1;else;djn=djn+dj1;endif
30    continue;dj=djp+djn;endif;endif;if (ktra.gt.4.and.un>=0) write(un,1000) jm,mrj,dj
1000  FORMAT (' PRIM: J=',I4,'  MRJ=',I2,'  DJ=',F8.3);dir=1.0;if (mrj.eq.9) then;dj=-dj;dir=-1.0;endif;if (dj.lt.-eopt) then;jav=1;
      else;if (mrj.eq.3) then;dj=-dj;dir=-1.0;if (dj.lt.-eopt) jav=1;endif;endif;if (jav.eq.1) then;wdyn=1.0;if (k.eq.2) then;mm1=mm
     l1+1;endif;if (devex.eq.1) then;goto 222;endif;if (idyn.eq.1) then;if (k.eq.1) then;wdyn=at(m1+jm-n1);else;wdyn=0.0;do 110 i=ka
     lc(jm),colend(jm); wdyn=at(m1+inda(i))*aij(i)+wdyn
110   continue;endif;www=abs(wdyn);wdyn=dmax1(1.0d0,www);endif;if (iadc.eq.1.and.ninf.gt.0) then;if (k.eq.1) then;wd=at(m3+jm-n1);el
     lse;wd=0.0;do 130 i=kac(jm),colendx(jm); wd=at(m3+inda(i))*aij(i)+wd
130   continue;endif;if (dir.lt.0.0) then;wadc=dj-bdav*wd;else;wadc=dj+bdav*wd;endif;endif;if (ideg.eq.1) then;if (k.eq.1) then;wdeg
     l=abs(at(m2+jm-n1));else;wdeg=0.0;do 120 i=kac(jm),colend(jm); wdeg=at(m2+inda(i))*aij(i)+wdeg
120   continue;wdeg=abs(wdeg);endif;endif
222   if (k.eq.1) then;inop1=inop1+1;else;inop2=inop2+1;endif;call sel1(jm,colw);endif
211   if(k==1)then; goto 208;elseif(k==2)then; goto 202; endif
202   if (mm1.lt.kvec) goto 210
205   ksp(nsp)=jm;if (.not.(jsc.ge.ksec.and.mm.eq.nn)) goto 203
250   do 40 i=1,mm;ja=(i-1)*m1;jf=ja+m1;do 50 k=ja+1,jf; at(k)=0.0
50    continue;j=km(i);mj(i)=mark(j);fks(i)=rng(j);ican(i)=1;if (j.gt.n1) then;at(ja+j-n1)=1.0;else;do 60 k=kac(j),colendx(j); at(ja
     l+inda(k))=aij(k)
60    continue;endif
40    continue;if (ivv.eq.1.and.iapr.eq.1) kvec=max(m/10,inop2/4,nn);inop1=inop1+inop2;return;end subroutine prim;subroutine sel1(j,
     lcolw)
      include 'mifcomx.inc'
      integer(1) un; common/un/un;integer j,km,mj,ican;real    colw(j);double precision sj,sg,fks,din,deg,adc,sdjx,    wdyn,wdeg,wad
     lc;common /minit/  sj(6),sg(6),fks(6)
     1,din(6),deg(6),adc(6),sdjx(6),km(6),mj(6),ican(6);integer ksor,i,j1,k,l,l1,lev,kt,ks;double precision w,djx;dimension ksor(6);
      pointer (pwd1,wdyn),(pwd2,wdeg),(pwd3,wadc);ks=0;pwd1=loc(wd1); pwd2=loc(wd2); pwd3=loc(wd3);w=0.; l=0; l1=0;l=0;if (devex.eq.
     l1) then;djx = dj / colw(j);if (mm.lt.nn) then;mm = mm + 1;l = mm;else;ks = 1;do k=1,nn;if (sdjx(k).gt.sdjx(ks)) then;ks = k;en
     ldif;enddo;if (djx.lt.sdjx(ks)) then;l = ks;endif;endif;else;djx = dj;if (mm.lt.nn) then;mm=mm+1;l=mm;if (idyn.eq.1) w=dj/wdyn;
      else;lev=0;kt=nn;do 310 i=1,nn; ksor(i)=i
310   continue;if (ideg.eq.1) then;lev=1;do 220 i=1,kt;do 230 j1=i+1,kt;if (deg(ksor(i)).lt.deg(ksor(j1))) then;k=ksor(i);ksor(i)=ks
     lor(j1);ksor(j1)=k;endif
230   continue
220   continue;ks=kt;do 240 i=1,kt;if (wdeg.gt.deg(ksor(i))) then;ks=i-1;goto 250;endif
240   continue
250   if (ks.eq.0) goto 300;l=ksor(1);l1=l;kt=ks;endif;if (iadc.eq.1.and.ninf.gt.0) then;lev=lev+1;do 320 i=1,kt;do 330 j1=i+1,kt;if
     l (adc(ksor(i)).lt.adc(ksor(j1))) then;k=ksor(i);ksor(i)=ksor(j1);ksor(j1)=k;endif
330   continue
320   continue;ks=kt;do 340 i=1,kt;if (wadc.gt.adc(ksor(i))) then;ks=i-1;goto 350;endif
340   continue
350   if (ks.eq.0) goto 400;kt=ks;l1=ksor(1);endif;if (idyn.eq.1) then;lev=lev+1;w=dj/wdyn;do 120 i=1,kt;do 130 j1=i+1,kt;if (din(ks
     lor(i)).lt.din(ksor(j1))) then;k=ksor(i);ksor(i)=ksor(j1);ksor(j1)=k;endif
130   continue
120   continue;ks=kt;do 140 i=1,kt;if (w.gt.din(ksor(i))) then;ks=i-1;goto 150;endif
140   continue
150   if (ks.eq.0) goto 400;kt=ks;l1=ksor(1);endif;if ((ideg.eq.0.and.iadc.eq.0.and.idyn.eq.0).or.
     1(iadc.eq.1.and.ninf.eq.0)) then;ks=1;do 100 k=1,nn; if (sj(k).gt.sj(ks)) ks=k
100   continue;if (dj.lt.sj(ks)) l=ks;goto 300;endif
400   if (lev.le.1.and.ks.eq.0) goto 300;L=L1;GOTO 300;endif;endif
300   if (l.ne.0) then;sj(l)=dj;sdjx(l) = djx;sg(l)=dir;km(l)=j;if (idyn.eq.1) din(l)=w;if (ideg.eq.1) deg(l)=wdeg;if (iadc.eq.1) ad
     lc(l)=wadc;endif;return;end subroutine sel1;subroutine djca (kb3,mr,at,jats)
      include 'mifcomx.inc'
      integer(1) un; common/un/un;integer kb3,mr,jats,km,mj,ican;double precision at,sj,sg,fks,din,deg,adc,sdjx;common /minit/  sj(6
     l),sg(6),fks(6)
     1,din(6),deg(6),adc(6),sdjx(6),km(6),mj(6),ican(6);dimension kb3(mr+1),at(jats);integer i,j,k;double precision a;ivan=0;do 10 k
     l=1,mm;if (ican(k).ne.1) goto 10;a=0.0;j=(k-1)*m1;if (ninf.eq.0) then;a=at(j+m1)*ez;else;do 20 i=1,m;if (kb3(i).ne.0.and.at(j+i
     l).ne.0.0) then;if (kb3(i).eq.1) then;a=a+at(j+i);else;a=a-at(j+i);endif;endif
20    continue;endif;if (mj(k).eq.3.and.a.gt.eopt) sg(k)=-1.0;sj(k)=a*sg(k);if (sj(k).lt.-eopt) ivan=1
10    continue;return;end subroutine djca;subroutine djcx (kb3,mr,at,jats)
      include 'mifcomx.inc'
      integer(1) un; common/un/un;integer kb3,mr,jats,km,mj,ican;double precision at,sj,sg,fks,din,deg,adc,sdjx;common /minit/  sj(6
     l),sg(6),fks(6)
     1,din(6),deg(6),adc(6),sdjx(6),km(6),mj(6),ican(6);dimension kb3(mr+1),at(jats);integer i,j,k;double precision a;ivan=0;do 10 k
     l=1,mm;if (ican(k).ne.1) goto 10;a=0.0;j=(k-1)*m1;if (ninf.eq.0) then;a=at(j+m1)*ez;else;do 20 i=1,m;if (kb3(i).ne.0.and.at(j+i
     l).ne.0.0) then;if (kb3(i).eq.1) then;a=a+at(j+i);else;a=a-at(j+i);endif;endif
20    continue;endif;if (mj(k).eq.3.and.a.gt.eopt) sg(k)=-1.0;sj(k)=a*sg(k);if (sj(k).lt.-eopt) ivan=1
10    continue;return;end subroutine djcx;subroutine rese (mark,rng,xs,nrc,erel,kellin);integer(1) un; common/un/un;integer(4) nrc, 
     lkellin, mark(nrc);double precision erel, rng(nrc), xs(nrc);integer kc,j,mj;double precision r;intrinsic abs;kc=0;kellin=0;do 1
     l0 j=1,nrc;mj=mark(j);if (mj.gt.3.and.mj.lt.9) goto 10;if (mj.eq.9.or.mj.eq.0) then;r=abs(xs(j)-rng(j));xs(j)=rng(j);if (r.gt.e
     lrel) then;kc=kc+1;endif;endif;if (mj.lt.3) then;r=abs(xs(j));xs(j)=0.0;if (r.gt.erel) then;kc=kc+1;endif;endif
10    continue;if (kc.gt.0) kellin=1;return;end subroutine rese;subroutine pari(idb)
      include 'mifdp.inc'
      include 'mifcomx.inc'
      integer(4) idb;iscale = 0;big=1.0d30;eabs=1.0d-35;if(idb==1) then;epiv=1.0d-9;eopt=1.0d-8;erel=1d-10;ktra=0;ivfr=60;iduf=99999
     l9;else;epiv=1.0d-8;eopt=1.0d-6;erel=1.0d-10;ktra=0;ivfr=30;iduf=9999;endif;ez=+1.0;ma=-1;nn=nsub;kr=30;lp=2;ninf=1;idyn=0;ideg
     l=0;iadc=0;bda=0.25;devex  = 0;devreq = 0;devrat = 0.4;iapr=1;return;end subroutine pari;subroutine par2
      include 'mifdp.inc'
      include 'mifcomx.inc'
      kmax=10000;del=1.0e-6;deln=0.5*del;delk=0.99*del;tau=(delk-deln)*10e-4;dk=deln;return;end subroutine par2;subroutine tra1 (ket
     lac,xs,nrc,neta1)
      include 'mifdp.inc'
      include 'mifcomx.inc'
      integer(1) un; common/un/un;integer ketac,nrc,neta1,i;double precision xs,w;dimension ketac(neta1),xs(*);w=xs(1); i=nrc;jetap=
     lketac(neta+1)-ketac(1);return;end subroutine tra1;subroutine lbas (mr,kb1,kb2,mark,kac,rng,ketac,xs,rhs,ch,nrc)
      include 'mifdp.inc'
      include 'mifcomx.inc'
      integer(1) un; common/un/un;integer mr,nrc,kb1(mr+1),kb2(mr+1),mark(n+1),ketac(m2);integer kac(nrc);double precision rng(n),xs
     l(n+1),rhs(m1);character ch;integer i,j;ivpr=ivfr;neta=0;irang=0;ketac(1)=kac(n1+1);ketpnt=ketac(1);do 10 i=1,m1;kb1(i)=n1+i;xs
     l(kb1(i))=rhs(i);kb2(i)=mark(n1+i); mark(n1+i)=mark(n1+i)+4
10    continue;if (ch.eq.'2') then;do 20 j=1,n1;if (mark(j).eq.1) then;mark(j)=9;xs(j)=rng(j);endif
20    continue;endif;return;end subroutine lbas;subroutine trsf (aij,inda,jas,kb1,kb2,kb3
     *,at,jats,mark,ketac,rng,xs,ketah
     *,pi,djrow,colw,inref,kac,colend
     *,pivrows,pivots)
      include 'mifdp.inc'
      include 'mifcomx.inc'
      integer(1) un; common/un/un;real(8) FEAS0,FEAS1,FEASCUM,BTRA0,BTRA1,BTRACUM,FTRA0,FTRA1,FTRACUM,PRIM0,PRIM1,PRIMCUM,DJCA0,DJCA
     l1,DJCACUM,FECH0,FECH1,
     +FECHCUM,PIDE0,PIDE1,PIDECUM,PIVT0,PIVT1,PIVTCUM,TRSF0,TRSF1,TRSFCUM;integer jas,jats;integer inda(nffr:jas),mark(n),ketac(jas)
     l,ketah(jas);integer kb1(m1), kb2(m1), kb3(m1);integer kac(n1+1), colend(n1);integer pivrows(m1);double precision aij(nffr:jas)
     l,at(jats),rng(n),xs(n+1);double precision pi(m1),djrow(n),pivots(m1);real   colw(n);integer(2) inref(n);integer km,mj,ican;dou
     lble precision sj,sg,fks,din,deg,adc,sdjx;common /minit/  sj(6),sg(6),fks(6)
     1,din(6),deg(6),adc(6),sdjx(6),km(6),mj(6),ican(6);integer i,ii,j,k,ka,kf,ks,ki,l,ibe,it,ke,jch;integer mrj,jvarin,ninf1;double
     l precision pivot,rec,y,w,w1,w2,pt,sig,u;double precision tq, gamma, gamma2, rat, djp, djn;common /timing/ feas0,feas1,feascum,
     lbtra0,btra1,btracum
     \,ftra0,ftra1,ftracum,prim0,prim1,primcum,djca0,djca1,djcacum
     \,fech0,fech1,fechcum
     \,pide0,pide1,pidecum, pivt0,pivt1,pivtcum, trsf0,trsf1,trsfcum;intrinsic abs, sqrt;gamma2=0.; gamma=0.;if (idgtyp.eq.1) then;l
     lp=lp+1;endif;minr=minr+1;ii=(ms-1)*m1;if (ipiro.le.0) goto 100;teta=teta*sg(ms);pivot=at(ii+ipiro);rec=1.0/pivot;if (devex.eq.
     l1) then;do i=1,m1;pi(i) = 0.0;enddo;pi(ipiro) = 1.0;call btradv(aij,inda,jas,pi,jats,ketac,jats,ketah,
     *pivrows,pivots);tq = 0.0;do i=1,m;w = at(ii+i);if (inref(kb1(i)).eq.1.and.w.ne.0.0) then;tq = tq + w * w;endif;enddo;jvarin = 
     lkm(ms);if (inref(jvarin).eq.1) then;tq = tq + 1.0;endif;tq = sqrt(tq);gamma  = tq/pivot;gamma2 = djrow(jvarin)/pivot;rat = tq/
     lcolw(jvarin);devfail = 0;if (rat.lt.devrat) then;call devrec(inref,mark,colw,n);devfail = 1;if (ktra.gt.1) then;if(un>=0)write
     l(un,'(a,i7,a,f10.4)')
     *'Rectify at itn', minr,'  Ratio=',rat;endif;minr = minr - 1;goto 50;endif;endif;neta=neta+1;ketac(neta)=ketpnt;ks=ketpnt;inda(
     lks)=ipiro;aij(ks)=at(ii+ipiro);do 10 i=1,m1;y=at(ii+i);if (y.eq.0.0) goto 10;xs(kb1(i))=xs(kb1(i))-teta*y;at(ii+i)=-y*rec;if (
     li.eq.ipiro) goto 10;ks=ks+1;inda(ks)=i;aij(ks)=y
10    continue;ks=ks+1;ketah(neta)=ks-ketpnt;ieta=ieta+ketah(neta);ketpnt=ks;ketac(neta+1)=ks;at(ii+ipiro)=rec;ki=kb1(ipiro);ibe=km(
     lms);xs(ibe)=teta+xs(ibe);kb1(ipiro)=ibe;km(ms)=ki;if (ipvtip.eq.2.and.kb2(ipiro).eq.1) then;sg(ms)=-1.0;mark(ki)=9;else;sg(ms)
     l=1.0;mark(ki)=mark(ki)-4;endif;fks(ms)=rng(ki);it=mj(ms);if (it.eq.9) it=1;kb2(ipiro)=it;mj(ms)=mark(ki);if (mj(ms).eq.0) ican
     l(ms)=0;mark(ibe)=it+4;ke=ketac(neta);ka=ke+1;kf=ke+ketah(neta)-1;do 20 k=1,mm;if (ican(k).ne.1.or.k.eq.ms) goto 20;l=(k-1)*m1;
      w2=at(l+ipiro);if (w2.ne.0.0) then;pt=w2/aij(ke);at(l+ipiro)=pt;do 30 i=ka,kf;ks=inda(i);w=at(l+ks);w1=w-aij(i)*pt;at(l+ks)=w1
30    continue;endif
20    continue;if (devex.eq.1) then;do 310 j=1,n;mrj=mark(j);if (mrj.gt.3.and.mrj.lt.9) then;goto 310;endif;if (j.gt.n1) then;w = pi
     l(j-n1);else;djp=0.0;djn=0.0;do 130 i=kac(j),colend(j);w = pi(inda(i))*aij(i);if (w.gt.0.0) then;djp = djp+w;else;djn = djn+w;e
     lndif
130   continue;w = djp + djn;endif;w1 = w * gamma;if (colw(j).lt.abs(w1)) then;colw(j) = abs(w1);endif;djrow(j) = djrow(j) - gamma2 
     l* w
310   continue;colw(ibe) = 1.0;djrow(ibe) = 0.0;endif;goto 200
100   sig=sg(ms);ibe=km(ms);ki=ibe;sg(ms)=-sig;ipvtip=0;if (sig.lt.0.0) then;jch=1;xs(ki)=0.0;else;jch=9;xs(ki)=rng(ki);endif;mark(k
     lm(ms))=jch;mj(ms)=jch;u=fks(ms);teta=u*sig;do 40 i=1,m1;if (at(ii+i).ne.0.0) then;xs(kb1(i))=xs(kb1(i))-at(ii+i)*teta;endif
40    continue
200   dk=dk+tau;if (ninf.gt.0) then;ninf1 = ninf;call fech (kb1,kb2,kb3,m,xs,rng);if (devreq.gt.0.and.ninf.eq.0) then;iposi = 7;endi
     lf;endif;jetap=ketac(neta+1)-ketac(1);if (ktra.le.2) goto 50;i=ibe;if (ibe.gt.n1) i=-ibe+n1;j=ki;if (ki.gt.n1) j=-ki+n1;if(un>=
     l0)write (un,1000) minr,i,j,-xs(n+1),teta,ipiro,ipvtip,sj(ms),
     *ninf,sinf,ieta
1000  format (i5,2i6,g11.4,f7.2,i5,i2,g9.2,i4,g10.3,i5);if (ipiro.eq.0) then;if(un>=0)write(un,'(1x)');else;if(un>=0)write(un,'(g10.
     l3)') pivot;endif
50    continue;return;end subroutine trsf;subroutine fech (kb1,kb2,kb3,mr,xs,rng)
      include 'mifcomx.inc'
      integer kb1,kb2,kb3,mr,km,mj,ican;double precision xs,rng,sj,sg,fks,din,deg,adc,sdjx;common /minit/  sj(6),sg(6),fks(6)
     *,din(6),deg(6),adc(6),sdjx(6),km(6),mj(6),ican(6);dimension kb1(mr+1),kb2(mr+1),kb3(mr+1),xs(n),rng(n);integer i;double precis
     lion tzer,xbi;ninf=0;sinf=0.0;tzer=dk;do 10 i=1,m;kb3(i)=0;xbi=xs(kb1(i));if (xbi.ne.0.0.and.kb2(i).ne.3) then;if (xbi.lt.-tzer
     l) then;kb3(i)=1;ninf=ninf+1;sinf=sinf+xbi+tzer;else;if (kb2(i).lt.2.and.xbi.gt.rng(kb1(i))+tzer) then;kb3(i)=2;ninf=ninf+1;sin
     lf=sinf-xbi+rng(kb1(i))+tzer;endif;endif;endif
10    continue;return;end subroutine fech;subroutine pivt (kb1,kb2,mr,at,jats,xs,rng,p)
      include 'mifcomx.inc'
      integer kb1,kb2,mr,jats,km,mj,ican,p;double precision at,xs,rng,sj,sg,fks,din,deg,adc,sdjx;common /minit/  sj(6),sg(6),fks(6)
     *,din(6),deg(6),adc(6),sdjx(6),km(6),mj(6),ican(6);dimension kb1(mr),kb2(mr),at(jats),xs(n+1),rng(n),p(mr);integer i,k,l,ii,ij1
     l,ij2,ipt,ipt1,ipt2,jp,jch,idgty,itop,j;double precision a,eps,teta1,teta2,tzer,tzer1,rc,y,den;logical mehet,jo;intrinsic abs;a
     l=-big;   den=0.;ipiro=0;ms=-1;eps=epiv;mehet=.false.
10    if (ms.eq.0) mehet=.true.;ms=0;k=1
15    if (k.le.mm.and.ipiro.ne.-1) then;if (sj(k).gt.-eopt.or.ican(k).ne.1) goto 20;l=(k-1)*m1;ii=0;ij1=0;ij2=0;ipt=0;ipt1=0;ipt2=0;
      teta1=big;teta2=big;tzer=dk;tzer1=tzer;jp=0;itop=0;do 30 i=1,m;if (abs(at(l+i)).lt.eabs.or.kb2(i).eq.3) goto 30;y=at(l+i)*sg(k
     l);if (y.gt.0.0.or.kb2(i).lt.2) then;itop=itop+1;p(itop)=i;if (y.gt.0.0) then;rc=(xs(kb1(i))+tzer1)/y;jp=1;else;rc=(xs(kb1(i))-
     lrng(kb1(i))-tzer1)/y;jp=2;endif;if (rc.lt.teta1) then;teta1=rc;ij1=i;ipt1=jp;endif;endif
30    continue;if (ij1.ne.0) then;den = abs(at(l+ij1));endif;if (ij1.ne.0) then;if (den.gt.eps) then;goto 50;endif;endif;tzer1=0.0;d
     lo 300 j=1,itop;i=p(j);y=at(l+i)*sg(k);if (y.gt.0.0) then;rc=xs(kb1(i))/y;jp=1;else;rc=(xs(kb1(i))-rng(kb1(i)))/y;jp=2;endif;if
     l (jp.eq.1.and.xs(kb1(i)).gt.-tzer.and.rc.le.teta1
     *.and.abs(y).gt.den) then;den=abs(y);teta2=rc;ij2=i;ipt2=jp;endif;if (jp.eq.2.and.xs(kb1(i)).lt.rng(kb1(i))+tzer
     \.and.rc.le.teta1.and.abs(y).gt.den) then;den=abs(y);teta2=rc;ij2=i;ipt2=jp;endif
300   continue
50    if (jp.ne.0) then;if (teta1.lt.teta2) then;ii=ij1;ipt=ipt1;else;teta1=teta2;ii=ij2;ipt=ipt2;endif;endif;jch=MJ(K);IF (jch.EQ.9
     l) jch=1;IF (II.EQ.0.AND.jch.GT.1) THEN;IPIRO=-1;MS=K;ELSE;JO=.TRUE.;IF (II.EQ.0) teta1=FKS(K);IF (jch.EQ.1.AND.teta1.GT.FKS(K)
     l) THEN;teta1=FKS(K);II=0;IPT=0;ENDIF;IF (II.NE.0) THEN;IF (ABS(AT(L+II)).LT.EPS) JO=.FALSE.;ENDIF;IF (JO) THEN;idgty=0;if (ii.
     lne.0) then;if (teta1.lt.abs(tau/at(l+ii))) then;teta1=abs(tau/at(l+ii));idgty=1;endif;endif;RC=ABS(teta1*SJ(K));IF (RC.GT.A) T
     lHEN;A=RC;TETA=TETA1;idgtyp=idgty;MS=K;IPIRO=II;IPVTIP=IPT;ENDIF;ENDIF;ENDIF
20    K=K+1;GOTO 15;ENDIF;IF (.NOT.MEHET) EPS=EPIV/10.0;IF (MS.EQ.0.AND..NOT.MEHET) GOTO 10;RETURN;END subroutine pivt;subroutine du
     lmp (kb1,mark,xs)
      include 'mifcomx.inc'
      integer(4) INPK,INPP,LOG_PR,ISTOP,IDB;integer(1) un; common/un/un;character workpath*256;common /contr/ inpk,inpp,log_pr,istop
     l,iDB, workpath;integer kb1,mark;double precision xs;character pname*40,dfil*40,snam*50;character(8) objname,rhsname,rngname,bn
     ldname;common /names1/  pname,dfil,objname,rhsname,rngname,bndname;dimension kb1(m1),mark(n+1),xs(n+1);integer ic,ii,ka,kf,i,j,
     lk,kbj,kb1j;dimension ic(10);intrinsic mod;data snam /'01020304050607080910111213141516171819202122232425'/;iduc=iduc+1;if (dfi
     ll(1:4).eq.'milp') then;ii=mod((iduc-1),25)+1;dfil(5:6)=snam((2*ii-1):(2*ii));endif;open (kr,file=trim(workpath)//dfil,status='
     lunknown');write (kr,'(a,a20,a,i6,a,i6,a,e11.5,a,i5,a,e10.4)')
     *'prob: ',pname,' it=',majr,'/',minr,' obj=',-xs(n+1)
     *,' inf=',ninf,'/',sinf;write (kr,'(i6,a)') iduc,'. dump';write (kr,'(a,2i7)') 'size:',m1,n1;kbj = kb1(m1);do 10 ii=1,m1,10;ka=
     lii;kf=ii+9;if (kf.gt.m1) kf=m1;i=0;do 20 j=ka,kf;i=i+1;if (j.eq.objind) then;ic(i) = - objind;kbj=kb1(j);else;kb1j=kb1(j);if (
     lj.eq.m1) then;kb1j=kbj;endif;if (kb1j.gt.n1) then;k=n1-kb1j;if (k.gt.-objind) then;ic(i)=k;else;ic(i)=k-1;endif;else;ic(i)=kb1
     lj;endif;endif
20    continue;k=10;if (ii+9.gt.m1) k=mod(m1,10);write (kr,'(10i7)') (ic(j),j=1,k)
10    continue;k=0;do 30 j=1,n; if (mark(j).eq.9) k=k+1
30    continue;write (kr,'(i7)') k;i=0;do 40 j=1,n;if (mark(j).eq.9) then;k=j;if (j.gt.n1) then;k=n1-j;if (k.le.-objind) then;k=k-1;
      endif;endif;i=i+1;ic(i)=k;if (i.eq.10) then;write (kr,'(10i7)') (ic(k),k=1,i);i=0;endif;endif
40    continue;if (i.ne.0) write (kr,'(10i7)') (ic(k),k=1,i);if(un>=0)write(un,'(1x,i5,3a)') iduc,'. dumpfile: ',dfil,' created';idu
     lpr=minr+iduf;close(kr);return;end subroutine dump;subroutine solu (aij,inda,jas,kb1,kb3,at,jats,chs,
     *mrow,ak,ncol,rname,cname,relk,
     *mark,kac,colendx,rng,xs,objrow)
      include 'mifcomx.inc'
      integer(4) INPK,INPP,LOG_PR,ISTOP,IDB;integer(1) un; common/un/un;character workpath*256;common /contr/ inpk,inpp,log_pr,istop
     l,iDB, workpath;integer inda,jas,jats,mrow,ncol,mark,kac,colendx;double precision aij,at,ak,rng,xs,objrow;integer kb1(m1), kb3(
     lm1);dimension aij(nzr),inda(nzr),at(jats);dimension ak(ncol),xs(mrow+ncol+1);character pname*40,dfil*40,ch,chs,relk*1;characte
     lr(8) objname,rhsname,rngname,bndname,rname,cname;common /names1/  pname,dfil,objname,rhsname,rngname,bndname;dimension rname(m
     lrow),cname(ncol),relk(mrow);if(inpk<=0) then; ch='Y'; goto 200; endif;call pard;if (iduf.lt.9999.and.un>=0) write(un,102) dfil
     l,iduc
102   format (1X,'Last dump on file ',A,' is #',I4);write(*,'(1x,a)')'Do you want a detailed solution output? (y/n) ';read (*,'(a)')
     l ch
200   continue;if (ch.eq.'Y'.or.ch.eq.'y') then;call solf (aij,inda,jas,kb1,kb3,m,at,jats,chs,
     *mrow,ak,ncol,rname,cname,relk,
     *mark,kac,colendx,rng,xs,objrow);endif;return;
      end subroutine solu
