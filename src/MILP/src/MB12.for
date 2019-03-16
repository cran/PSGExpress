      subroutine dual
     \(mrow, ncol, nvar, bchmax, nsmax, memveg, pname,
     \aij0, inda0, aij, inda,
     \kb1, kb2,
     \kb3, at,
     \rhs, rng,
     \colbeg, colend, colendx,
     \mark, ketac, ketah,
     \rname, cname, relk,
     \pivots, pivrows,
     \jc, jd, je,
     \djrow, prow,
     \rw, inref,
     \xs, iinda, iindael, iflag,
     \timelimit);use IntelInterf
      include 'mifdp.inc'
      include 'mifcomx.inc'
      integer mrow, ncol, nvar, bchmax, nsmax, memveg, iindael;integer inda(nffr:memveg), mark(nvar+1),                             
     l inda0(nzr);integer kb1(m1), kb2(m1), kb3(m1);integer colbeg(ncol+1), colend(ncol), colendx(ncol);integer ketac(2*m1+bchmax), 
     lketah(2*m1+bchmax), pivrows(m1);integer jc(m1+4), jd(m1+4), je(m1+4);integer iinda(iindael),iflag(0:mrow+ncol);integer(2) inre
     lf(nvar);double precision aij(nffr:memveg), at(nsmax*m1), rhs(m1),rng(nvar),   aij0(nzr);double precision pivots(m1), djrow(nva
     lr), prow(nvar);double precision xs(nvar+1);real(4) rw(nvar);character(8) rname(m1), cname(ncol);character(1) relk(m1);characte
     lr pname*40;integer(4) LOG_PR,ISTOP,INPK,INPP,IDB;real(8) D_TIME,W,TIMELIMIT,FEAS0,FEAS1,FEASCUM,BTRA0,BTRA1,BTRACUM,FTRA0,FTRA
     l1,FTRACUM,PRIM0,PRIM1,PRIMCUM,DJCA0,DJCA1,
     +DJCACUM,FECH0,FECH1,FECHCUM,PIDE0,PIDE1,PIDECUM,PIVT0,PIVT1,PIVTCUM,TRSF0,TRSF1,TRSFCUM;integer(1) un; common/un/un;character(
     l256) workpath,                    wch;common /contr/ inpk,inpp,log_pr,istop,iDB, workpath;character pn*40,dfil*40,chs;characte
     lr(8) objname,rhsname,rngname,bndname;common /names1/  pn,dfil,objname,rhsname,rngname,bndname;integer(2) itim(4), jtim(4);char
     lacter ich*1;integer(4) jas,jats;integer(4) i,j,k,irc,jp,jq,ibsw,jjj;integer(4) ki, dninf, dninf1, dninf2, minrph1,invwas;integ
     ler(4) ibs(3), varnum, dsetcnt, lstbsw, iflagopt;double precision eps,rowmax,xbp,ddj,dsinf,vp,wt,dtheta;double precision rtim1,
     lrtim2,rtim4,rtim5,timinv,epsx;common /timing/ feas0,feas1,feascum,btra0,btra1,btracum
     \,ftra0,ftra1,ftracum,prim0,prim1,primcum,djca0,djca1,djcacum
     \,fech0,fech1,fechcum
     \,pide0,pide1,pidecum, pivt0,pivt1,pivtcum, trsf0,trsf1,trsfcum;integer,allocatable:: iq(:);rtim1=0.; rtim2=0.; rtim4=0.;  rtim
     l5=0.; timinv=0.;dninf1 = 0;dninf2 = 0;invwas = 0;iflagopt = 0;irc=0;kcik=0;minr=0;jats = (mrow+1) * nsmax;GOTO 215
215   ich='4';if (devreq.gt.0) then;dsetcnt = 0;call devsetd (m,n,mark,inref,rw,dsetcnt);devex=1;if(log_pr>-1.and.un>=0) write(un,*)
     l'Dual  D E V E X  pricing started';endif;call par2;idupr=iduf;call gettim(itim(1),itim(2),itim(3),itim(4));if(log_pr>-1.and.un
     l>=0) write(un,'(1x,a,4(i2.2,a))')
     1'MILP dual started at ',
     2itim(1),':',itim(2),':',itim(3),'.',itim(4);ivpr=ivfr;kellin=1;ivpr=ivfr;nadr=colbeg(n1+1);nfree=memveg-nadr-1;jjj=iindael-m3-
     l1;jas=memtop;timinv=0.0;wt = -big;do 121 i=0,n;iflag(i)=0
121   continue;allocate(iq(n)); iq=0
99    continue;call invert (aij0,inda0,aij,inda,jas,rhs,kb1,kb2,kb3,
     *at(1),mark,colbeg,colendx,
     *ketac,ketah,xs,jc,jd,je,pivrows,pivots,
     *iinda(1),iinda(m1+1),iinda(m2+1),iinda(m3+1),jjj,
     *at(m1+1),ibs,mrow,ncol);if(istop>0) goto 999;ivpr=neta+ivfr;invwas = 1;if (ktra.ge.1.and.minr.gt.0) then;if(un>=0)write(un,'(2
     l(a,i6,2x),2(a,e12.6,2x),a,f10.2)')
     *'Inv:', ivc, 'Iter:', minr,
     *'Obj=', -xs(n+1), 'Inf=', sinf,
     *'Elapsed:', rtim4-rtim1;endif;wch='';if(d_time('s',int2(2))>2) then;w=d_time('s',int2(0)); write(wch,'(i10.10)')minr; i=verify
     l(wch,'0'); if(i<1.or.i>10)i=10;write(wch,'(a,2(a,e18.12))')'Iteration='//wch(i:10),'  Objective=',-xs(n+1),'  Residual=', -sin
     lf;call putmess('n',0,' ',wch);endif;if(inpk>=0.and.(wch=='S'.or.wch=='T'))then; iposi=0; goto 999; endif;if(minr>10*nvar) then
     l; iposi=5; goto 999; endif;call Check_stop_whatch(1,w); if(w>timelimit) iposi=24;if(iposi==24) goto 999;if (iflag(0).ne.0) the
     ln;do 120 i=0,n;iflag(i)=0
120   continue;endif;call udrow (m+1,djrow,nffr,aij0,inda0,aij,inda,at,colbeg,colendx
     *,mark,kb1,ketac,ketah,pivrows,pivots
     *,m1,m2,n1,nzr,rowmax,erel)
101   k = 4*n1+5;eps = eopt + dk;eps = eopt;call dfeas (djrow,m1,n1,n,mark,dk,dsinf,dninf,iinda(k+1)
     \,at(m1+1),at(m2+1),aij0,inda0,nzr,colbeg,colend,colendx
     \,ibsw,iinda(k+n+1),rng,at(1),xs);if (dninf1.gt.0.and.dninf.eq.0.and.dninf2.eq.0) then;minrph1 = minr;dninf2 = 1;endif;dninf1 =
     l dninf;if (ktra.ge.1.and.dninf.gt.0.and.invwas.eq.1) then;if(un>=0)write(un,'(a,i6,3x,g14.8)') 'Dual infeasibilities:',dninf,d
     lsinf;invwas = 0;endif;if (ibsw.gt.0.and.dninf.eq.0) then;mm = 1;call ftra (aij,inda,jas,at,jats,ketac,ketah,
     *pivrows,pivots);do 370 i=1,m1;xs(kb1(i)) = xs(kb1(i)) - at(i)
370   continue;endif;if (dninf.gt.0) then;mm = 1;call ftra (aij,inda,jas,at(m1+1),jats,ketac,ketah,
     *pivrows,pivots);endif
100   eps = dk + eopt;if (dninf.gt.0) then;call dprice1
     \(m,  ipiro,ipvtip,at(m1+1),   eps,   kb2,vp
     \,devex,rw,iflag);ddj = vp;else;call dprice2
     \(m,n,ipiro,ipvtip,xs,rng,eps,kb1,kb2,ninf,sinf,ddj
     \,devex,rw,iflag);endif;if(ipiro.eq.0) then;call rese (mark,rng,xs,n,erel,kellin);if(iflag(0).gt.0) then;if(iflagopt.eq.0) then
      iflagopt = 1;goto 99;else;call tra1 (ketac,xs,n,neta+1);if(Log_pr>-1) then;if(un>=0)write(un,'(a,i6,a)') 'All',iflag(0),
     *' candidates flagged, cannot continue.';call gettim(jtim(1),jtim(2),jtim(3),jtim(4));if(un>=0)write(un,*) 'Objective value=',-
     lxs(n+1);call dump (kb1,mark,xs);if(un>=0)write(un,'(1x,8(a,i2.2))')
     *'Optimization started at ',
     *itim(1),':',itim(2),':',itim(3),'.',itim(4),
     *', finished at ',
     *jtim(1),':',jtim(2),':',jtim(3),'.',jtim(4);if(un>=0)write(un,'(1x,a,f21.2,a)') 'Solution time=',rtim2-rtim1,' sec';if(un>=0)w
     lrite(un,'(1x,a,f14.2,3x,a,i8)')
     *'Total inversion time=',timinv, '  No. of inversions  =   ',ivc;endif;iposi = 9;goto 999;endif;endif;if(kellin.eq.1)then;goto 
     l99;endif;if(log_pr>-1)then;if (dninf.eq.0) then;if(un>=0)write(un,'(a,i6,a,f12.2,a)')
     \'Dual phase-1 terminated in',minrph1,' iterations and'
     \,rtim5-rtim1,' seconds';if(un>=0)write(un,'(a,i6,a)')
     *'Dual reached optimal solution after',minr,' iterations';else;if(un>=0)write(un,'(a,i6,a)')
     *'Dual infeasible after',minr,' iterations';endif;endif;call invert (aij0,inda0,aij,inda,jas,rhs,kb1,kb2,kb3,
     *at(1),mark,colbeg,colendx,
     *ketac,ketah,xs,jc,jd,je,pivrows,pivots,
     *iinda(1),iinda(m1+1),iinda(m2+1),iinda(m3+1),jjj,
     *at(m1+1),ibs,mrow,ncol);if(istop>0) goto 999;if(log_pr>-1)then;if(un>=0)write(un,*) 'Objective value=',-xs(kb1(mc));call dump 
     l(kb1,mark,xs);if(un>=0)write(un,'(a,2x,i2)') ' IPOSI=',iposi;if(un>=0)write(un,'(1x,2a,2x,2(a,I7))')
     *'PROBLEM: ',PNAME,'SIZE: m=',M,'  n=',N1;if(ktra.eq.0) call tra1 (ketac,xs,n,neta+1);if(un>=0)write(un,'(1x,8(a,i2.2))')
     *'Optimization started at ',
     *itim(1),':',itim(2),':',itim(3),'.',itim(4),
     *', finished at ',
     *jtim(1),':',jtim(2),':',jtim(3),'.',jtim(4);if(un>=0)write(un,'(1x,a,f21.2,a)') 'Solution time=',rtim2-rtim1,' sec';if(un>=0)w
     lrite(un,'(1x,a,f14.2,3x,a,i8)')
     *'Total inversion time=',timinv, '  No. of inversions  =   ',ivc;endif;j = 0;do i=1,n;if (iq(i).gt.0) then;if(log_pr>-1.and.un>
     l=0)write(un,'(2(i6,a))')
     \i,'  breakpoints used',iq(i),'  times';j = j + iq(i);endif;enddo;if(log_pr>-1.and.un>=0) write(un,'(a,18x,i6)') 'Total:', j;ip
     losi=1;goto 999;endif;call udrow (ipiro,prow,nffr,aij0,inda0,aij,inda,at,colbeg,colendx
     *,mark,kb1,ketac,ketah,pivrows,pivots
     *,m1,m2,n1,nzr,rowmax,erel);prow(kb1(ipiro)) = 1.0;ki = kb1(ipiro);if (dninf.eq.0) then;if (ipvtip.eq.1) then;xbp = -xs(ki);els
     le;xbp =  xs(ki) - rng(ki);endif;k = 4*n1 + 5;lstbsw = k+n+1;call dpiv2a
     \(    ipvtip,jq,m,n1,n,    nffr,djrow,prow,xs,xbp,mark,at,rng,
     \aij,inda,colbeg,colendx,dk,dtheta,
     \iinda(1),iinda(k),iinda(lstbsw),iflag,
     \ibsw,big,eabs,erel,epiv,rowmax,jp   );if(istop>0) goto 999;if (jp.eq.0) then;if(log_pr>-1.and.un>=0) write(un,'(2(a,i6),a,e16.
     l6)')
     \'Row:',ipiro,'  Var:',varnum(kb1(ipiro),n1),
     \'  Violation:',xbp;if(log_pr>-1.and.un>=0) write(un,'(a)') 'Dual unbounded, stop';iposi=2;goto 999;endif;else;k=4*n1+5;lstbsw 
     l= k+n+1;call dpiv1a
     \(   ipvtip,jq,  n1,n,    djrow,prow,   vp,dsinf,wt,dtheta
     \,mark,
     \iinda(1),iinda(k),
     \ibsw,big,eabs,erel,epiv,dk,    jp,   iq);if(istop>0) goto 999;if (jp.eq.0) then;if(log_pr>-1.and.un>=0) write(un,*) 'After ite
     lration',minr,
     \'  ERROR: dual unbounded in Phase-1, stop';iposi=2;goto 999;endif;endif;if (jq.ne.0) then;do 50 i=1,m1;at(i)=0.0
50    continue;if (jq.gt.n1) then;at(jq-n1)=1.0;else;do 60 k=colbeg(jq),colendx(jq);at(inda0(k))=aij0(k)
60    continue;endif;mm = 1;call ftra (aij,inda,jas,at,jats,ketac,ketah,pivrows,pivots);endif;epsx=0.00001*erel;if (abs(at(ipiro)).l
     lt.epsx) then;at(ipiro)=0.0;endif;if (abs(at(m+1)).lt.epsx) then;at(m+1)=0.0;endif;at(m+1) = djrow(jq);if (at(ipiro)*prow(jq).l
     lt.0.0) then;iflag(ipiro)=1;iflag(0) = iflag(0) + 1;if(log_pr>-1.and.un>=0) write(un,'(a,i6,2a,i7,2(a,g14.5))')
     \'Row',ipiro,' flagged (2)',' Col=',varnum(jq,n1),
     \' prow BTR= ',prow(jq),' FTR= ',at(ipiro);goto 998;endif;if (dninf.eq.0) then;if (at(m+1)*djrow(jq).lt.0.0) then;iflag(ipiro)=
     l1;iflag(0) = iflag(0) + 1;if(log_pr>-1.and.un>=0) write(un,'(a,i6,2a,i7,2(a,g14.5))')
     \'Row',ipiro,' flagged (2)',' Col=',varnum(jq,n1),
     \' djrow BTR= ',djrow(jq),' FTR= ',at(m+1);goto 998;endif;endif;ki = kb1(ipiro);eps=erel;call dtrsf
     *(m1,   n,    nffr,ipiro,ipvtip,jq,neta,ketpnt,    dsetcnt
     *,aij,inda,   kb1,kb2
     *,at,    mark,ketac,rng,xs,ketah,ieta
     *,djrow,prow
     *,devex,devrat,inref,rw,dtheta
     *,        eps,mc);if(istop>0) goto 999;if (ibsw.gt.0) then;call dfcorr (aij0,inda0,aij,inda,colbeg,colendx,jas,at,jats,rng,mark
     \,ketac,ketah,pivrows,pivots,xs,kb1,iinda(lstbsw),ibsw);endif;dk=dk+tau+tau;minr=minr+1;if(log_pr>-1 .and. minr.gt.idupr) call 
     ldump (kb1,mark,xs);kcik=kcik+1;if(kcik.gt.kmax) then;call rese (mark,rng,xs,n,erel,kellin);dk=deln;iposi=5;kellin=1;kcik=0;got
     lo 99;endif;if (ktra.gt.1) then;if(un>=0)write(un,'(i5,3i6,1x,g14.8,i3,i5,1x,i5,2(1x,e12.5))')
     \minr,varnum(jq,n1),varnum(ki,n1),ipiro,-xs(kb1(mc)),ipvtip,ibsw
     \,ninf,ddj,prow(jq);endif;if (dninf.eq.0) then;k = 4*n1+5;eps = eopt + dk;endif;if(neta.ge.ivpr) then;goto 99;endif
998   if(dninf.gt.0) then;goto 101;else;goto 100;endif
999   continue;deallocate(iq);if(inpk<=0.and.inpp==0) RETURN;chs='';call solu (aij0,inda0,jas,kb1,kb3,at,jats,chs,
     *m,djrow,n1,rname,cname,relk,
     *mark,colbeg,colend,rng,xs,prow);end subroutine dual;subroutine dtrsf
     *(m1,  n,    nffr,ipiro,ipvtip,jq,neta,ketpnt,    dsetcnt
     *,aij,inda,   kb1,kb2
     *,at,   mark,ketac,rng,xs,ketah,ieta
     *,djrow,prow
     *,devex,devrat,inref,rw,dtheta
     *,        eps,mc);integer m1,n,jq,neta,ipiro,ipvtip,ieta,ketpnt,devex;integer nffr,inda(nffr:*),mark(n),ketac(*),ketah(*);integ
     ler kb1(m1), kb2(m1);integer(2) inref(n);real(4) rw(m1);double precision aij(nffr:*),at(m1),rng(n),xs(n+1);double precision djr
     low(n),prow(n),devrat;integer i,it,j,ks,ki,dsetcnt;double precision pivot,rec,teta,y,w,dtheta,gamma,xbp;double precision eps, t
     lp;integer(4) LOG_PR,ISTOP,MC,INPK,INPP,IDB;integer(1) un; common/un/un;character workpath*256;common /contr/ inpk,inpp,log_pr,
     listop,iDB, workpath;intrinsic abs, sqrt;pivot=at(ipiro);xbp = xs(kb1(ipiro));if (pivot.eq.0.0) then;if(log_pr>-1.and.un>=0) wr
     lite(un,'(a,i6,2x,a,i6)')
     \'Pivot = 0.0; incoming:',jq,'pivot row:',ipiro;istop=121; RETURN;endif;ki=kb1(ipiro);rec=1.0/pivot;if (ipvtip.le.1) then;teta=
     lxs(kb1(ipiro))/pivot;else;teta=(xs(kb1(ipiro))-rng(kb1(ipiro)))/pivot;endif;neta=neta+1;ketac(neta)=ketpnt;ks=ketpnt;inda(ks)=
     lipiro;aij(ks)=at(ipiro);do 10 i=1,m1;if (i.eq.mc) then;goto 10;endif;y=at(i);if (y.eq.0.0) goto 10;w=xs(kb1(i))-teta*y;if (abs
     l(w).lt.abs(xs(kb1(i))*eps)) then;w=0.0;endif;xs(kb1(i))=w;if (i.eq.ipiro) goto 10;ks=ks+1;inda(ks)=i;aij(ks)=y
10    continue;ks=ks+1;ketah(neta)=ks-ketpnt;ieta=ieta+ketah(neta);ketpnt=ks;xs(jq)=teta+xs(jq);if (ipvtip.le.1) then;xs(ki)=0.0;els
     le;xs(ki)=rng(ki);endif;kb1(ipiro)=jq;if (ipvtip.eq.2.and.kb2(ipiro).eq.1) then;mark(ki)=9;else;mark(ki)=mark(ki)-4;endif;it=ma
     lrk(jq);if (it.eq.9) it=1;kb2(ipiro)=it;mark(jq)=it+4;xs(kb1(mc)) = xs(kb1(mc)) - dtheta * xbp;prow(ki) = 1.0;djrow(ki) = 0.0;d
     lo 20 j=1,n;if (mark(j).gt.3.and.mark(j).lt.9) goto 20;y = djrow(j) - dtheta*prow(j);if (abs(y).lt.eps*abs(djrow(j))) y = 0.0;d
     ljrow(j) = y
20    continue;djrow(jq)=0.0;djrow(ki) = -dtheta;if (devex.eq.1) then;tp = 0.0;do 30 j=1,n;if (inref(j).eq.1) then;tp = tp + prow(j)
     l * prow(j);endif
30    continue;tp = sqrt(tp);if (tp/rw(ipiro).lt.devrat) then;if(log_pr>-1.and.un>=0) write(un,'(i5,a,f12.2,a,f12.2)') dsetcnt,
     \'. Reference framework reset, true:',tp,
     \'  estimated:',rw(ipiro);call devsetd (m1-1,n,mark,inref,rw,dsetcnt);goto 999;endif;gamma = tp/pivot;do 40 i=1,m1-1;if (abs(ga
     lmma*at(i)).gt.rw(i)) then;rw(i) = real(abs(gamma*at(i)),4);endif
40    continue;rw(ipiro) = real(abs(gamma),4);endif
999   return;end subroutine dtrsf;subroutine getnexti (i,jp,t,indt);integer i,jp,indt(0:jp),j,jj;double precision t(0:jp),w;jj = i;d
     lo 36 j=i+1,jp;if (t(j).lt.t(jj)) then;jj=j;endif
36    continue;if (jj.ne.i) then;j=jj;w=t(i);t(i)=t(j);t(j)=w;jj=indt(i);indt(i)=indt(j);indt(j)=jj;endif;return;end subroutine getn
     lexti;subroutine udrow (irow,row,nffr,aij0,inda0,aij,inda,vcol,colbeg,colendx
     *,mark,kb1,ketac,ketah,pivrows,pivots
     *,m1,m2,n1,nzr,rowmax,erel);integer m1,m2,n1,nzr,nffr;integer irow,inda(nffr:*),colbeg(n1),colendx(n1),mark(m1+n1),     inda0(n
     lzr);integer kb1(m1),ketac(m2),ketah(m2),pivrows(m1);double precision row(m1+n1),aij(nffr:*),pivots(m1),vcol(m1),      aij0(nzr
     l);integer i,j,n,mrj;double precision w,djp,djn,rowmax,erel,vnorm,epsthr;intrinsic abs;i=kb1(1);n = m1 + n1 - 1;do 100 i=1,m1;v
     lcol(i) = 0.0
100   continue;vcol(irow) = 1.0;call btrand (aij,inda,vcol,m1,ketac,ketah,
     *pivrows,pivots,vnorm);epsthr = vnorm * erel;rowmax = 0.0;do 120 j=1,n;mrj = mark(j);if (mrj.gt.3.and.mrj.lt.9) then;row(j) = 0
     l.0;else;if (j.gt.n1) then;w = vcol(j-n1);if (abs(w).lt.epsthr) then;w = 0.0;endif;row(j) = w;else;djp=0.0;djn=0.0;do 40 i=colb
     leg(j),colendx(j);w = vcol(inda0(i))*aij0(i);if (w.gt.0) then;djp = djp+w;else;djn = djn+w;endif
40    continue;w = djp + djn;if (abs(w).lt.erel*abs(djp).or.abs(w).lt.epsthr) then;w = 0.0;endif;row(j) = w;endif;endif
120   continue;rowmax = vnorm;return;end subroutine udrow;subroutine dfeas (djrow,m1,n1,n,mark,eps,dsinf,dninf,dinfls
     \,vr,hr,aij,inda,nzr,colbeg,colend,colendx
     \,ibsw,indt,rng,at,xs);integer(4) KTRA,K,MN,MXN,nffr; real(8) UJ;integer m1,n1,n,ibsw;integer mark(n),dninf,dinfls(n),nzr,inda(
     lnzr);integer colbeg(n1), colend(n1), colendx(n1);integer indt(n1);double precision djrow(n),eps,dsinf,vr(m1),hr(m1),aij(nzr);d
     louble precision rng(n),at(m1),xs(n),  w;COMMON /PARAMI/ MN(11),KTRA,MXN(38),nffr;integer(1) un; common/un/un;integer i,j,mrj,v
     larnum;intrinsic abs;w=hr(1);dsinf=0.0;dninf=0;ibsw = 0;do 100 j=1,n;mrj=mark(j);if (mrj.gt.3.and.mrj.lt.9) goto 100;if (mrj.eq
     l.0) goto 100;if (mrj.lt.3) then;if (djrow(j).lt.-eps) then;if (mrj.eq.1) then;ibsw = ibsw + 1;indt(ibsw) = -j;goto 100;endif;d
     lsinf=dsinf+djrow(j)+eps;dninf=dninf+1;dinfls(dninf) = -j;if (ktra.gt.2) then;if(un>=0)write(un,'(a,i6,g15.6)') 'Inf: M',varnum
     l(j,n1),djrow(j);endif;endif;elseif (mrj.eq.3) then;if (abs(djrow(j)).gt.eps) then;dsinf=dsinf-abs(djrow(j))+eps;dninf=dninf+1;
      if (ktra.gt.2) then;if(un>=0)write(un,'(a,i6,g15.6)')
     \'Inf: P',varnum(j,n1),-abs(djrow(j));endif;if (djrow(j).lt.0.0) then;dinfls(dninf) = -j;else;dinfls(dninf) = j;endif;endif;els
     leif (mrj.eq.9.and.djrow(j).gt.eps) then;ibsw = ibsw + 1;indt(ibsw) = j;goto 100;endif
100   continue;if (dninf.gt.0) then;do 140 i=1,m1;vr(i) = 0.0
140   continue;do 110 j=1,dninf;if (dinfls(j).gt.0) then;if (dinfls(j).gt.n1) then;vr(dinfls(j)-n1)=vr(dinfls(j)-n1) - 1.0;else;do 1
     l20 i=colbeg(dinfls(j)),colend(dinfls(j));vr(inda(i)) = vr(inda(i)) - aij(i)
120   continue;endif;else;if (-dinfls(j).gt.n1) then;vr(-dinfls(j)-n1)=vr(-dinfls(j)-n1) + 1.0;else;do 130 i=colbeg(-dinfls(j)),cole
     lnd(-dinfls(j));vr(inda(i)) = vr(inda(i)) + aij(i)
130   continue;endif;endif
110   continue;endif;if (ibsw.gt.0.and.dninf.eq.0) then;do 240 i=1,m1;at(i)=0.0
240   continue;do 250 k=1,ibsw;j=abs(indt(k));if (mark(j).eq.1) then;mark(j)=9;xs(j)=rng(j);if (j.gt.n1) then;at(j-n1) = at(j-n1) + 
     lrng(j);else;uj = rng(j);do 230 i=colbeg(j),colendx(j);at(inda(i)) = at(inda(i)) + uj * aij(i)
230   continue;endif;elseif (mark(j).eq.9) then;mark(j)=1;xs(j)=0.0;if (j.gt.n1) then;at(j-n1) = at(j-n1) - rng(j);else;uj = rng(j);
      do 260 i=colbeg(j),colendx(j);at(inda(i)) = at(inda(i)) - uj * aij(i)
260   continue;endif;endif
250   continue;endif;return;end subroutine dfeas;subroutine dprice1
     \(m,  ipiro,ipvtip,vr,   eps,   kb2,vp
     \,devex,rw,iflag);integer m,ipiro,ipvtip,devex;integer kb2(m),iflag(0:m);real(4) rw(m);double precision eps,vr(m),vp,vabs;integ
     ler i,it,itp;double precision w,v;w=eps;ipiro=0;itp=-1;if (devex.eq.0) then;do 100 i=1,m;if (iflag(i).eq.1) goto 100;it=kb2(i);
      if (it.eq.3) then;goto 100;endif;v = vr(i);vabs = abs(v);if (vabs.gt.w) then;if (itp.eq.0) then;if ((it.eq.0).and.vabs.gt.w) t
     lhen;w = vabs;ipiro=i;endif;else;if (it.eq.0.or.it.eq.1) then;goto 130;elseif (v.gt.0.0) then;goto 130;else;goto 100;endif
130   if (vabs.gt.w) then;w = vabs;ipiro=i;itp=it;endif;endif;endif
100   continue;else;do 110 i=1,m;if (iflag(i).eq.1) goto 110;it=kb2(i);if (it.eq.3) then;goto 110;endif;v = vr(i)/rw(i);vabs = abs(v
     l);if (vabs.gt.w) then;if (itp.eq.0) then;if ((it.eq.0).and.vabs.gt.w) then;w = vabs;ipiro=i;endif;else;if (it.eq.0.or.it.eq.1)
     l then;goto 120;elseif (v.gt.0.0) then;goto 120;else;goto 110;endif
120   if (vabs.gt.w) then;w = vabs;ipiro=i;itp=it;endif;endif;endif
110   continue;endif;if (ipiro.gt.0) then;vp = vr(ipiro);if (vp.gt.0) then;ipvtip = 1;else;ipvtip = 2;endif;endif;return;end subrout
     line dprice1;subroutine dprice2
     \(m,n,ipiro,ipvtip,xs,rng,eps,kb1,kb2,ninf,sinf,ddj
     \,devex,rw,iflag);integer m,n,ipiro,ipvtip,ninf,devex;integer kb1(m),kb2(m),iflag(0:m);real(4) rw(m);double precision eps,xs(n)
     l,rng(n),sinf,ddj;integer i,it,itp;double precision w,w1,xbi;w=-eps;sinf=0.0;ninf=0;ipiro=0;itp=-1;if (devex.eq.0) then;do 100 
     li=1,m;if (iflag(i).eq.1) goto 100;it=kb2(i);if (it.eq.3) then;goto 100;endif;xbi=xs(kb1(i));if (xbi.lt.-eps) then;sinf = sinf 
     l+ xbi + eps;ninf = ninf + 1;if (itp.eq.0) then;if ((it.eq.0).and.xbi.lt.w) then;w = xbi;ipiro=i;ipvtip=1;endif;else;if (xbi.lt
     l.w) then;w = xbi;ipiro=i;ipvtip=1;itp=it;endif;endif;elseif (it.le.1.and.xbi.gt.rng(kb1(i))+eps) then;w1 = rng(kb1(i))-xbi;sin
     lf = sinf + w1 + eps;ninf = ninf + 1;if (itp.eq.0) then;if ((w1.lt.w).and.(it.eq.0)) then;w = w1;ipiro=i;ipvtip=2;endif;else if
     l (w1.lt.w) then;w = w1;ipiro=i;ipvtip=2;itp = it;endif;endif
100   continue;else;do 110 i=1,m;if (iflag(i).eq.1) goto 110;it=kb2(i);if (it.ne.3) then;xbi=xs(kb1(i));if (xbi.lt.-eps) then;sinf =
     l sinf + xbi + eps;ninf = ninf + 1;if (itp.eq.0) then;if ((it.eq.0).and.xbi/rw(i).lt.w) then;w = xbi/rw(i);ipiro=i;ipvtip=1;end
     lif;else;if (xbi/rw(i).lt.w) then;w = xbi/rw(i);ipiro=i;ipvtip=1;itp=it;endif;endif;elseif (it.le.1.and.xbi.gt.rng(kb1(i))+eps)
     l then;w1 = rng(kb1(i))-xbi;sinf = sinf + w1 + eps;ninf = ninf + 1;if (itp.eq.0) then;if ((it.eq.0).and.(w1/rw(i).lt.w)) then;w
     l = w1/rw(i);ipiro=i;ipvtip=2;endif;else;if (w1/rw(i).lt.w) then;w = w1/rw(i);ipiro=i;ipvtip=2;itp=it;endif;endif;endif;endif
110   continue;endif;ddj = w;return;end subroutine dprice2;integer function varnum (j,n1);integer n1,j;if (j.gt.n1) then;varnum = n1
     l - j;else;varnum = j;endif;return;end function varnum;subroutine devsetd(m,n,mark,inref,rw,dsetcnt);integer m,n,i,j,dsetcnt;in
     lteger mark(n);real(4) rw(m);integer(2) inref(n);do 100 i=1,m;rw(i) = 1.0
100   continue;do 110 j=1,n;if (mark(j).gt.3.and.mark(j).lt.9) then;inref(j)=1;else;inref(j)=0;endif
110   continue;dsetcnt = dsetcnt + 1;return;end subroutine devsetd;subroutine dfcorr (aij0,inda0,aij,inda,colbeg,colendx,jas,at,jats
     l,rng,mark
     \,ketac,ketah,pivrows,pivots,xs,kb1,listbsw,ibsw)
      include 'mifcomx.inc'
      integer inda(nffr:*),jas,jats,ketac(1),ketah(1),kb1(m1),ibsw,         inda0(nzr);integer listbsw(ibsw),colbeg(n1), colendx(n1)
     l,mark(n),pivrows(1);double precision aij(nffr:*),at(m1),pivots(1),xs(n+1),                aij0(nzr);double precision rng(n);in
     lteger i,j,k;double precision uj;do 110 i=1,m+1;at(i)=0.0
110   continue;do 120 k=1,ibsw;j=listbsw(k);if (mark(j).eq.1) then;mark(j)=9;xs(j)=rng(j);if (j.gt.n1) then;at(j-n1) = at(j-n1) + rn
     lg(j);else;uj = rng(j);do 130 i=colbeg(j),colendx(j);at(inda0(i)) = at(inda0(i)) + uj * aij0(i)
130   continue;endif;else;mark(j)=1;xs(j)=0.0;if (j.gt.n1) then;at(j-n1) = at(j-n1) - rng(j);else;uj = rng(j);do 140 i=colbeg(j),col
     lendx(j);at(inda0(i)) = at(inda0(i)) - uj * aij0(i)
140   continue;endif;endif
120   continue;mm = 1;call ftra (aij,inda,jas,at,jats,ketac,ketah,
     *pivrows,pivots);do 70 i=1,m1;xs(kb1(i)) = xs(kb1(i)) - at(i)
70    continue;return;
      end subroutine dfcorr
