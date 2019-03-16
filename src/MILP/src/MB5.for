      subroutine crash
     x(aij,inda,jnda,jd,ksor,ksorp,jcol,kac,colend,colendx,
     xkb1,mark,flink,blink,crdisk)
      include 'mifdp.inc'
      include 'mifcomx.inc'
      integer(4) JAV,KK,ICRASH,INPK,INPP,LOG_PR,ISTOP,IDB;integer(1) un; common/un/un;character workpath*256;common /contr/ inpk,inp
     lp,log_pr,istop,iDB, workpath;integer flink(n),blink(n);integer crdisk;double precision aij(nzr);integer inda(nzr),jnda(nzr);in
     lteger jd(m+1),ksor(m+1),ksorp(m+1),jcol(n1);integer kb1(m),mark(n);integer kac(n1+1), colend(n1), colendx(*);integer jk,rprtab
     l(0:5),cprtab(0:5),rsltab(0:5),csltab(0:5);integer it,jcl,irpr,ipr,jcpr,jpr,ka,kf;integer i,j,k,irt,jct, mxrwln, ibig, inz;doub
     lle precision tim,timcra;i=colendx(1);if(un>=0)write(un,'(/,1x,a)') 'CRASH-2: ';jav=0;if (crdisk.ne.0) then;open
     \(33,file=trim(workpath)//'milpcras.tmp',form='unformatted',status='unknown');write (33) (aij(i),i=1,kac(n1+1)-1);close (33);en
     ldif;call clocki(tim);rprtab(3)=0;rprtab(2)=1;rprtab(1)=2;rprtab(0)=3;cprtab(0)=0;cprtab(1)=1;cprtab(2)=2;cprtab(3)=3;do 110 it
     l = 0,5;rsltab(it) = 0;csltab(it) = 0
110   continue;do 10 i=1,m; jd(i)=0
10    continue;do 20 j=1,n1;jk=0;if (mark(j).eq.0) goto 25;do 30 k=kac(j),colend(j);if (mark(n1+inda(k)).eq.3) goto 30;jk=jk+1;jd(in
     lda(k))=jd(inda(k))+1
30    continue
25    jcol(j)=jk
20    continue;mxrwln = 0;do 15 i=1,m;if (jd(i).GT.mxrwln) mxrwln = jd(i)
15    continue;do 16 j=m+1,m+mxrwln;flink(j) = j;blink(j) = j
16    continue;do 17 i=1,m;if (jd(i).GT.0) then;j = jd(i);k = flink(j+m);flink(j+m) = i;flink(i) = k;blink(k) = i;blink(i) = j+m;end
     lif
17    continue;ksor(1)=1;ksorp(1)=1;do 40 i=1,m;ksor(i+1)=ksor(i)+jd(i);ksorp(i+1)=ksor(i+1)
40    continue;do 50 j=1,n1;if (mark(j).eq.0) goto 50;do 60 i=kac(j),colend(j);if (mark(n1+inda(i)).eq.3) GOTO 60;jnda(ksorp(inda(i)
     l))=j;ksorp(inda(i))=ksorp(inda(i))+1
60    continue
50    continue;ibig=+10*(n1+1)
500   continue;irpr=-ibig;ka=0;do 70 j=m+1,m+mxrwln;i=flink(j)
72    if (i.LE.m) then;it=mark(n1+i);ipr=rprtab(it);if (ipr.gt.irpr) then;irpr=ipr;ka=i;endif;i=flink(i);goto 72;endif;if (ka.GT.0) 
     lgoto 502
70    continue
502   continue;if (ka.gt.0) then;irt = mark(n1+ka);jcpr=-10*(m+1);kf=0;do 80 j=ksor(ka),ksor(ka+1)-1;jcl=jnda(j);jk=jcol(jcl);if (jk
     l.gt.0) then;it=mark(jcl);jpr=cprtab(it)-10*jk;if (jpr.gt.jcpr) then;jcpr=jpr;kf=jcl;jct=it;endif;endif
80    continue;if (kf.gt.0) then;kb1(ka)=kf;jav=jav+1;do 90 j=ksor(ka),ksor(ka+1)-1;jcl=jnda(j);jk=jcol(jcl);if (jk.gt.0) then;jcol(
     ljcl)=0;do 100 k=kac(jcl),colend(jcl);i=inda(k);if (jd(i).GT.0) then;flink(blink(i)) = flink(i);blink(flink(i)) = blink(i);jd(i
     l)=jd(i)-1;if (jd(i).GT.0) then;inz = jd(i);kk = flink(inz+m);flink(inz+m) = i;flink(i) = kk;blink(kk) = i;blink(i) = inz+m;end
     lif;endif
100   continue;endif
90    continue;jd(ka)=0;rsltab(irt)=rsltab(irt)+1;csltab(jct)=csltab(jct)+1;endif;endif;if (ka.gt.0.and.kf.gt.0) goto 500;call clock
     li(timcra);if(un>=0)write(un,'(i5,a,f10.2,a)')
     xjav,' vectors found in',timcra-tim,' secs';if (crdisk.ne.0) then;open
     \(33,file=trim(workpath)//'milpcras.tmp',form='unformatted',status='unknown');read (33) (aij(i),i=1,kac(n1+1)-1);close (33,stat
     lus='delete');endif;icrash=1;end;subroutine scal (aij,inda,rhs,r1,r2,rnf,kac,colend,colendx,
     \rng,mark,cnf)
      include 'mifdp.inc'
      include 'mifcomx.inc'
      integer(4) INPK,J,I,MENET,MI,INPP,LOG_PR,ISTOP,IDB;real(8) E1,E2,W,R,S1,S2,C1,C2,S;integer(1) un; common/un/un;character workp
     lath*256;common /contr/ inpk,inpp,log_pr,istop,iDB, workpath;real(8), pointer::prnsc(:),pcnsc(:),prng(:),paij(:); logical lpall
     loc;common/pForScale/prnsc,pcnsc,prng,paij,lpalloc;double precision aij(nzr),rhs(m),r1(m),r2(m),rnf(m);double precision rng(n),
     lcnf(n1);integer inda(nzr),kac(n1+1),colend(n1),colendx(n1),mark(*);double precision bg;bg = 0.5 * big;if(inpk>0.and.un>=0) wri
     lte(un,'(1X,A)') 'SCALING in progress ';if (iscale.eq.0) then;if(lpalloc)then; prng=rng; paij=aij;else;open (33,file=trim(workp
     lath)//'milprng.tmp',form='unformatted',status='unknown');write (33) (rng(j),j=1,n);open (33,file=trim(workpath)//'milpaij.tmp'
     l,form='unformatted',status='unknown');write (33) (aij(i),i=1,kac(n1+1)-1);close (33);endif;endif;e1=kac(n1+1)-kac(1);e2=10.0;i
     lf (iscale.eq.0) then;do 300 i=1,m; rnf(i)=1.0
300   continue;do 301 j=1,n1; cnf(j)=1.0
301   continue;else;if(lpalloc)then; rnf=prnsc; cnf=pcnsc;else;open (33,file=trim(workpath)//'milprnsc.tmp',form='unformatted',statu
     ls='unknown');read (33) (rnf(i),i=1,m);close (33);open (33,file=trim(workpath)//'milpcnsc.tmp',form='unformatted',status='unkno
     lwn');read (33) (cnf(j),j=1,n1);close (33);endif;endif;menet=0
500   menet=menet+1;if (menet.gt.4) goto 510;do 10 i=1,m;r1(i)=big; r2(i)=0.0
10    continue;do 30 j=1,n1;do 40 i=kac(j),colend(j);mi=inda(i);if (mark(n1+mi).eq.3.or.mark(n1+mi).eq.7) goto 40;w=abs(aij(i));if (
     lw.lt.r1(mi)) r1(mi)=w;if (w.gt.r2(mi)) r2(mi)=w
40    continue
30    continue;do 50 i=1,m;r1(i)=sqrt(r1(i)*r2(i));if (r1(i).eq.0.0) r1(i)=1.0;if (mark(n1+i).eq.3.or.mark(n1+i).eq.7) r1(i)=1.0;r1(
     li)=1.0/r1(i);r=r1(i);if (abs(rng(n1+i)).lt.bg) rng(n1+i)=rng(n1+i)*r;rnf(i)=rnf(i)*r;if (rhs(i).ne.0.0) rhs(i)=rhs(i)*r
50    continue;do 60 j=1,n1;do 70 i=kac(j),colend(j);aij(i)=aij(i)*r1(inda(i))
70    continue
60    continue;s1=0.0;s2=0.0;do 80 j=1,n1;c1=big;c2=0.0;do 90 i=kac(j),colend(j);mi=inda(i);if (mark(n1+mi).eq.3.or.mark(n1+mi).eq.7
     l) goto 90;w=abs(aij(i));if (w.lt.c1) c1=w;if (w.gt.c2) c2=w
90    continue;s=sqrt(c1*c2);if (s.eq.0.0) s=1.0;if (abs(rng(j)).lt.bg) rng(j)=rng(j)*s;s=1.0/s;cnf(j)=cnf(j)*s;do 100 i=kac(j),cole
     lndx(j);aij(i)=aij(i)*s;if (mark(inda(i)+n1).ne.3) then;s1=aij(i)*aij(i)+s1;s2=s2+abs(aij(i));endif
100   continue
80    continue;if (abs((s1-s2*s2/e1)/e1).gt.e2) goto 500;menet=menet+1
510   do 110 i=1,m; r2(i)=0.0
110   continue;do 130 j=1,n1;do 140 i=kac(j),colend(j);mi=inda(i);if (mark(n1+mi).eq.3.or.mark(n1+mi).eq.7) goto 140;w=abs(aij(i));i
     lf (w.gt.r2(mi)) r2(mi)=w
140   continue
130   continue;do 150 i=1,m;if (r2(i).eq.0.0) r2(i)=1.0;if (mark(n1+i).eq.3.or.mark(n1+i).eq.7) r2(i)=1.0;r2(i)=1.0/r2(i);r=r2(i);if
     l (abs(rng(n1+i)).lt.bg) rng(n1+i)=rng(n1+i)*r;rnf(i)=rnf(i)*r; if (rhs(i).ne.0.0) rhs(i)=rhs(i)*r
150   continue;do 160 j=1,n1;do 170 i=kac(j),colend(j);aij(i)=aij(i)*r2(inda(i))
170   continue
160   continue;do 180 j=1,n1;c2=0.0;do 190 i=kac(j),colend(j);mi=inda(i);if (mark(n1+mi).eq.3.or.mark(n1+mi).eq.7) goto 190;w=abs(ai
     lj(i));if (w.gt.c2) c2=w
190   continue;if (c2.eq.0.0) c2=1.0;if (abs(rng(j)).lt.bg) rng(j)=rng(j)*c2;c2=1.0/c2;cnf(j)=cnf(j)*c2;do 200 i=kac(j),colendx(j);a
     lij(i)=aij(i)*c2
200   continue
180   continue;if(lpalloc)then; prnsc=rnf; pcnsc=cnf;else;open (33,file=trim(workpath)//'milprnsc.tmp',form='unformatted',status='un
     lknown');write (33) (rnf(i),i=1,m);close (33);open (33,file=trim(workpath)//'milpcnsc.tmp',form='unformatted',status='unknown')
      write (33) (cnf(j),j=1,n1);close (33);endif;iscale=iscale+1;if(inpk>0.and.un>=0)   write(un,'(1x,a,i5,a)') 'SCALING: ',menet-1
     l,' pass(es)';
      end
