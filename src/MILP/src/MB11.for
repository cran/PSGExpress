      subroutine primal
     \(mrow, ncol, nvar, bchmax, nsmax, memveg, pname,
     \aij0, inda0,  aij, inda,
     \kb1, kb2,
     \kb3, at,
     \rhs, rng,
     \kac, colend, colendx,
     \mark, ketac, ketah,
     \rname, cname, relk,
     \pivots, pivrows,
     \jc, jd, je,
     \djrow, objrow,
     \colw, inref,
     \xs, iinda, iindael,
     \timelimit);use IntelInterf
      include 'mifdp.inc'
      include 'mifcomx.inc'
      integer(4) ISTOP,I,INPK,INPP,LOG_PR,IDB;real(8) FEASCUM,BTRACUM,FTRACUM,PRIMCUM,DJCACUM,FECHCUM,PIDECUM,PIVTCUM,TRSFCUM,D_TIME
     l,W,TIMELIMIT,XS_SINF,AXSINF,FEAS0,FEAS1,
     +BTRA0,BTRA1,FTRA0,FTRA1,PRIM0,PRIM1,DJCA0,DJCA1,FECH0,FECH1,PIDE0,PIDE1,PIVT0,PIVT1,TRSF0,TRSF1;integer(1) un; common/un/un;ch
     laracter(256)  workpath,             wch;common /contr/ inpk,inpp,log_pr,istop,iDB, workpath;integer mrow, ncol, nvar, bchmax, 
     lnsmax, memveg, iindael;integer inda(nffr:memveg), mark(nvar+1),                              inda0(nzr);integer kb1(m1), kb2(m
     l1), kb3(m1);integer kac(ncol+1), colend(ncol), colendx(ncol);integer ketac(2*m1+bchmax), ketah(2*m1+bchmax), pivrows(m1);integ
     ler jc(m1+4), jd(m1+4), je(m1+4);integer iinda(iindael);integer(2) inref(nvar);double precision aij(nffr:memveg), at(nsmax*m1),
     l rhs(m1),rng(nvar),   aij0(nzr);double precision pivots(m1), djrow(nvar), objrow(nvar);double precision xs(nvar+1);real(4) col
     lw(nvar);character(8) rname(m1), cname(ncol);character(1) relk(m1);character pname*40;character pn*40,dfil*40,ch,chs;character(
     l8) objname,rhsname,rngname,bndname;common /names1/  pn,dfil,objname,rhsname,rngname,bndname;integer(2) itim(4), jtim(4);charac
     lter ich*1;integer(4) jas,jats,irmax;integer(4) j,irc,numbaj,jjj,ninf1,idynx,idegx,iadcx;integer(4) jbas, nnx;integer(4) ibs(3)
      double precision ak,rtim1,rtim2,rtim3,rtim4,timinv;dimension ak(1);common /timing/ feas0,feas1,feascum,btra0,btra1,btracum
     \,ftra0,ftra1,ftracum,prim0,prim1,primcum,djca0,djca1,djcacum
     \,fech0,fech1,fechcum
     \,pide0,pide1,pidecum, pivt0,pivt1,pivtcum, trsf0,trsf1,trsfcum;idynx=0; idegx=0; iadcx=0; rtim1=0.; rtim2=0.; rtim3=0.; rtim4=
     l0.; timinv=0.; nnx=0;feascum=0.0;btracum=0.0;ftracum=0.0;primcum=0.0;djcacum=0.0;fechcum=0.0;pidecum=0.0;pivtcum=0.0;trsfcum=0
     l.0;jas = memtop;jbas = 3 * mrow;jats = (mrow+1) * nsmax;irmax=2;iresul=0
95    call spari
96    call spprep;timinv = 0.0;irc=0;kcik=0;ich='4'; GOTO 215
215   CONTINUE;call par2;djset = 0;ivc=0;ivcr=0;minr=0;majr=0;numbaj=0;idupr=iduf;kellin=1;nadr=kac(n1+1);nfree=memveg-nadr-1;lp=0;i
     lvpr=ivfr
10    if (kellin.ne.1) goto 30;jjj=iindael-m3-1;ninf1=ninf;call invert (aij0,inda0,aij,inda,jas,rhs,kb1,kb2,kb3,
     *at(1),mark,kac,colendx,
     *ketac,ketah,xs,jc,jd,je,pivrows,pivots,
     *iinda(1),iinda(m1+1),iinda(m2+1),iinda(m3+1),jjj,
     *at(m1+1),ibs,mrow,ncol);if(istop>0) goto 998;if (ktra.ge.1.and.minr.gt.0) then;if(un>=0)write(un,'(2(a,i6,2x),2(a,e12.6,2x),a,
     lf10.2)')
     *'Inv:', ivc, 'Iter:', minr,
     *'Obj=', -xs(n+1), 'Inf=', sinf,
     *'Elapsed:', rtim4-rtim1;if (ktra.ge.2) then;if(un>=0)write(un,'(a,i6,3x,a,i7,2x,a,i6,a,i6,2x,a,f9.2)')
     *'NZ in B: ',IBS(1),
     *'NZ in Eta: ',IBS(3),
     *'Triang: ',IBS(2),'/',M,
     *'InvTime: ',rtim4-rtim3;endif;endif;wch='';if(d_time('s',int2(2))>2) then;w=d_time('s',int2(0)); write(wch,'(i10.10)')minr; i=
     lverify(wch,'0'); if(i<1.or.i>10)i=10;write(wch,'(a,2(a,e18.12))')'Iteration='//wch(i:10),'  Objective=',-xs(n+1),'  Residual='
     l,-sinf;call putmess('n',0,' ',wch);endif;if(inpk>=0.and.(wch=='S'.or.wch=='T'))then; iposi=0; goto 998; endif;if(minr>10*nvar)
     l then; iposi=5; goto 999; endif;call Check_stop_whatch(1,w); if(w>timelimit) iposi=24;if(iposi==24) goto 999;if (djset.eq.1) t
     lhen;call djcomp(aij,inda,kac,mark,kb1,djrow,objrow,at(m3+1)
     *,ketac,ketah,pivrows,pivots
     *,m,n,jas,jats);endif;ivv=1;ivpr=neta+ivfr;iposi=0;if (ketpnt.gt.(jas-m-2)) then;iposi=6;ivan=0;goto 70;endif;if (iposi.gt.0) g
     loto 60
20    ninf1=ninf;call feas(kb1,kb2,kb3,     at,     xs,rng  );if (minr.eq.0) then;call tra1 (ketac,xs,n,neta+1);endif
21    if (devex.ge.1.and.ninf.gt.0) then;if (ninf1.eq.0) then;devex=0;nn=nnx;iapr=1;djset=0;endif;else;if ((ninf1.gt.0.and.ninf.eq.0
     l).or.iposi.eq.7) then;if (devreq.gt.0) then;call devset(aij,inda,kac,mark,kb1,djrow,objrow,at(m3+1)
     *,ketac,ketah,pivrows,pivots,inref,colw
     *,m,n,jas,jats);nnx=nn;nn=devreq;if(un>=0)write(un,*)'D E V E X  pricing started';endif;iposi = 0;endif;endif;if (djset.eq.0) t
     lhen;call btra (aij,inda,jas,at,jats,ketac,ketah,
     *pivrows,pivots);endif;call opti (aij0,inda0,aij,inda,jas,kb1,kb2,kb3,jbas,at,jats
     *,mark,kac,colend,colendx
     *,ketac,rng,xs,ketah,iinda,pivrows,pivots
     *,objrow,djrow,colw,at(m3+1),inref,n);if(istop>0) goto 998;if (iposi.eq.7.and.devreq.gt.0) goto 21;if (iposi.eq.0) goto 20
60    if (numbaj.gt.0.and.iposi.ne.3.and.iposi.ne.4) then;numbaj=0;idyn=idynx;ideg=idegx;iadc=iadcx;endif;if (numbaj.eq.0.and.(iposi
     l.eq.3.or.iposi.eq.4)) then;numbaj=1;idynx=idyn;idegx=ideg;iadcx=iadc;if (idyn.eq.0) then;idyn=1;ideg=0;else;idyn=0;endif;goto 
     l20;endif
70    if (iposi.gt.2.and.ivv.eq.1) kellin=0;if (iposi.lt.3) kellin=0;goto 10
30    continue;call rese (mark,rng,xs,n,erel,kellin);irc=irc+1;if(irc==irmax)then; xs_sinf=-xs(n+1)-sinf; axsinf=abs(xs_sinf); if(ax
     lsinf<1.) axsinf=1d0; endif;if (kellin.eq.1) then;if(irc<=irmax)then; goto 10;else;if(abs(xs_sinf-(-xs(n+1)-sinf))/(irc-irmax)/
     laxsinf>erel)then; goto 10;endif;endif;else;endif;dk=deln;if(inpk<=0) GOTO 35
100   format (1x,a);if (iposi.eq.6.and.un>=0) write(un,100)
     1'E T A   S P A C E   O V E R F L O W';if (iposi.gt.2.and.inop1.gt.nn.and.un>=0) write(un,100)
     1'P R O B L E M   U N S O L V A B L E';if (iposi.eq.2.and.un>=0) write(un,100)
     1'S O L U T I O N   U N B O U N D E D';if (iposi.eq.1.or.((iposi.eq.3.or.iposi.eq.4).and.inop1.le.nn))
     1then;if (ninf.gt.0) then;if(un>=0)write(un,100) 'P R O B L E M   I N F E A S I B L E';else;if(un>=0)write(un,100) 'O P T I M A
     l L   S O L U T I O N';endif;endif;call gettim(jtim(1),jtim(2),jtim(3),jtim(4));if(un>=0)write(un,'(a,2x,i2)') ' IPOSI=',iposi;
      if(un>=0)write(un,'(1x,2a,2x,2(a,I7))')
     *'PROBLEM: ',PNAME,'SIZE: m=',M,'  n=',N1;if (ktra.eq.0) call tra1 (ketac,xs,n,neta+1);if(un>=0)write(un,'(1x,8(a,i2.2))')
     *'Optimization started at ',
     *itim(1),':',itim(2),':',itim(3),'.',itim(4),
     *', finished at ',
     *jtim(1),':',jtim(2),':',jtim(3),'.',jtim(4);if(un>=0)write(un,'(1x,a,f21.2,a)') 'Solution time=',rtim2-rtim1,' sec';if(un>=0)w
     lrite(un,'(1x,a,f14.2,3x,a,i8)')
     *'Total inversion time=',timinv, '  No. of inversions  =   ',ivc
35    continue;call feas(kb1,kb2,kb3,   at,     xs,rng  );call btra (aij,inda,jas,at,jats,ketac,ketah,
     *pivrows,pivots);call tra1 (ketac,xs,n,neta+1)
998   if(inpk<=0.and.inpp==0) RETURN;if (n1.lt.(jas+1-kac(n1+1))) then;chs = '1';call solu (aij0,inda0,jas,kb1,kb3,at,jats,chs,
     *m,djrow,n1,rname,cname,relk,
     *mark,kac,colend,rng,xs,objrow);else;if(un>=0)write(un,*) ' No space to provide names';chs='2';call solu (aij0,inda0,jas,kb1,kb
     l3,at,jats,chs,
     *m,ak,n1,rname,cname,relk,
     *mark,kac,colend,rng,xs,objrow);endif;if(inpk<=0) RETURN;write (*,'(1x,a)')'DO YOU WANT A DUMP OF THE SOLUTION ? (Y/N) '; read 
     l(*,'(a)') ch;if (ch.eq.'y'.or.ch.eq.'y') then;write(*,'(1x,a)') 'DUMP FILE (quit=*): '; read (*,'(a)') dfil;if (dfil(1:1).ne.'
     l*') call dump (kb1,mark,xs);endif;if (iresul.gt.0.and.iscale.gt.0) goto 999;write (*,103)
103   format(1x,'Continue? (2:New run, 3:End) ');read (*,'(a)') ch;if (ch.eq.'2') then;do 40 j=1,n
50    if (mark(j).gt.3) then;mark(j)=mark(j)-4;goto 50;endif
40    continue;if (iapr.eq.1) goto 95;goto 96;endif
999   return;
      end
