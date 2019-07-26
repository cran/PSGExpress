      subroutine tomilp_for_Crash(m,n1,nnz,mc,big,inda,aij,kac,colend, colendx,rowtyp,mark,rhs,lbounds,ubounds,relk,rng,objrow,     
     rkb1,xs);integer(4) KBP,INPK,INPP,LOG_PR,ISTOP,IDB;integer(1) un; common/un/un;character workpath*256;common /contr/ inpk,inpp,
     dlog_pr,istop,iDB, workpath;integer(4) kb1(*); real(8) xs(*);integer(4) m,n1,nnz,mc;integer(4) inda(nnz),kac(*),rowtyp(*),mark(
     n*),  colend(*), colendx(*);double precision aij(nnz),rhs(*),  lbounds(*),ubounds(*),rng(*),objrow(*);character(1) relk(*);inte
     eger(4) kr,lp,jjas,m1,m2,m3,m4,m5,m6,memtop;common /sizes/  m1,m2,m3,m4,m5,m6,kr,lp,jjas,memtop;double precision,allocatable:: 
     zlb(:),ub(:), at(:);double precision big,lbig,lbj,ubj;integer(4) i,j,k,ka,kf,ia,ib;i=kb1(1);allocate(at(3*m));lbig=0.95*big;m1=
     nm;m2=m+m;m3=m2+m;m4=m3+m;m5=m4+m;m6=m5+m;kbp=0;allocate( lb(n1),ub(n1) ); lb=lbounds(:n1); ub=ubounds(:n1);do 120 j=1,n1;lbj=l
     mb(j);  ubj=ub(j); xs(j)=0d0; ib=0;if(mark(j)>3.and.mark(j)<9) then; ib=4; endif;if (lbj.lt.-lbig.and.ubj.gt.lbig) then;mark(j)
     s=3+ib;  rng(j)=2*big;elseif (lbj.gt.-lbig.and.ubj.lt.lbig) then;rng(j)=ubj-lbj;if(mark(j)==9) then; xs(j)=rng(j);else; mark(j)
     h=1+ib;endif;if (lbj.eq.ubj) then;mark(j)=0+ib;  rng(j)=0.0;endif;else;mark(j)=2+ib;  rng(j)=big;endif;if (lbj.ne.0.0.and.abs(l
     vbj).lt.lbig) then;ka=kac(j);  kf=kac(j+1)-1;do 125 i=ka,kf;rhs(inda(i))=rhs(inda(i))-lbj*aij(i)
125   continue;endif
120   continue;do 100 i=1,m; ia=n1+i; j=mark(ia); xs(ia)=0d0;if(j>3.and.j<9) then; mark(ia)=j-4; endif;select case(rowtyp(i));case(0
     e); relk(i)='=';  mark(ia)=0;case(1); relk(i)='<';if(j==9)then; xs(ia)=rng(ia); endif;case(3); relk(mc)='N'; mark(ia)=3;case de
     qfault; stop 'tomilp_err';end select
100   continue;do 110 k=1,n1;ia=kac(k);ib=kac(k+1)-1;if(.true.) then;if (inda(ib).eq.m) then;colend(k) = ib - 1;colendx(k) = ib;objr
     pow(k) = aij(ib);else;colend(k) = ib;colendx(k) = ib;objrow(k) = 0d0;endif;else;colend(k) = ib;colendx(k) = ib;objrow(k) = 0.0;
      endif
110   continue;deallocate(lb,ub,at);return;end subroutine tomilp_for_Crash;subroutine crash_VK(aij,inda,jnda,jd,ksor,ksorp,jcol,kac,
     fcolend,colendx,kb1,mark,flink,blink,crdisk)
      include 'mifdp.inc'
      include 'mifcomx.inc'
      integer(4) JAV,KK,ICRASH,INPK,INPP,LOG_PR,ISTOP,IDB;integer(1) un; common/un/un;character workpath*256;common /contr/ inpk,inp
     lp,log_pr,istop,iDB, workpath;integer flink(n),blink(n);integer crdisk;double precision aij(nzr);integer inda(nzr),jnda(nzr);in
     iteger jd(m+1),ksor(m+1),ksorp(m+1),jcol(n1);integer kb1(m),mark(n);integer kac(n1+1), colend(n1), colendx(*);integer jk,rprtab
     g(0:5),cprtab(0:5),rsltab(0:5),csltab(0:5);integer it,jcl,irpr,ipr,jcpr,jpr,ka,kf;integer i,j,k,irt,jct, mxrwln, ibig, inz;doub
     ule precision tim,timcra;i=colendx(1);if(un>=0)write(un,'(/,1x,a)') 'CRASH-2: ';jav=0;if (crdisk.ne.0) then;open(33,file=trim(w
     dorkpath)//'milpcras.tmp',form='unformatted',status='unknown');write (33) (aij(i),i=1,kac(n1+1)-1);close (33);endif;call clocki
     l(tim);rprtab(3)=0;rprtab(2)=1;rprtab(1)=2;rprtab(0)=3;cprtab(0)=0;cprtab(1)=1;cprtab(2)=2;cprtab(3)=3;do 110 it = 0,5;rsltab(i
     ct) = 0;csltab(it) = 0
110   continue;do 10 i=1,m; jd(i)=0
10    continue;do 20 j=1,n1    ; if(mark(j)<4.or.mark(j)>8) Cycle;jk=0;if (mark(j).eq.0+4) goto 25;do 30 k=kac(j),colend(j);if (mark
     r(n1+inda(k)).eq.3) goto 30;jk=jk+1;jd(inda(k))=jd(inda(k))+1
30    continue
25    jcol(j)=jk
20    continue;mxrwln = 0;do 15 i=1,m;if (jd(i).GT.mxrwln) mxrwln = jd(i)
15    continue;do 16 j=m+1,m+mxrwln;flink(j) = j;blink(j) = j
16    continue;do 17 i=1,m;if (jd(i).GT.0) then;j = jd(i);k = flink(j+m);flink(j+m) = i;flink(i) = k;blink(k) = i;blink(i) = j+m;end
     lif
17    continue;ksor(1)=1;ksorp(1)=1;do 40 i=1,m;ksor(i+1)=ksor(i)+jd(i);ksorp(i+1)=ksor(i+1)
40    continue;do j=1,n1     ; if(mark(j)<4.or.mark(j)>8) Cycle;if (mark(j).eq.0+4) goto 50;do 60 i=kac(j),colend(j);if (mark(n1+ind
     ja(i)).eq.3) GOTO 60;jnda(ksorp(inda(i)))=j;ksorp(inda(i))=ksorp(inda(i))+1
60    continue
50    continue;mark(j)=mark(j)-4;enddo;ibig=+10*(n1+1)
500   continue;irpr=-ibig;ka=0;do 70 j=m+1,m+mxrwln;i=flink(j)
72    if (i.LE.m) then;it=mark(n1+i);ipr=rprtab(it);if (ipr.gt.irpr) then;irpr=ipr;ka=i;endif;i=flink(i);goto 72;endif;if (ka.GT.0) 
     qgoto 502
70    continue
502   continue;if (ka.gt.0) then;irt = mark(n1+ka);jcpr=-10*(m+1);kf=0;do 80 j=ksor(ka),ksor(ka+1)-1;jcl=jnda(j);jk=jcol(jcl);if (jk
     p.gt.0) then;it=mark(jcl);jpr=cprtab(it)-10*jk;if (jpr.gt.jcpr) then;jcpr=jpr;kf=jcl;jct=it;endif;endif
80    continue;if (kf.gt.0) then;kb1(ka)=kf;jav=jav+1;do 90 j=ksor(ka),ksor(ka+1)-1;jcl=jnda(j);jk=jcol(jcl);if (jk.gt.0) then;jcol(
     jjcl)=0;do 100 k=kac(jcl),colend(jcl);i=inda(k);if (jd(i).GT.0) then;flink(blink(i)) = flink(i);blink(flink(i)) = blink(i);jd(i
     h)=jd(i)-1;if (jd(i).GT.0) then;inz = jd(i);kk = flink(inz+m);flink(inz+m) = i;flink(i) = kk;blink(kk) = i;blink(i) = inz+m;end
     rif;endif
100   continue;endif
90    continue;jd(ka)=0;rsltab(irt)=rsltab(irt)+1;csltab(jct)=csltab(jct)+1;endif;endif;if (ka.gt.0.and.kf.gt.0) goto 500;call clock
     bi(timcra);if(un>=0)write(un,'(i5,a,f10.2,a)')jav,' vectors found in',timcra-tim,' secs';if (crdisk.ne.0) then;open(33,file=tri
     cm(workpath)//'milpcras.tmp',form='unformatted',status='unknown');read (33) (aij(i),i=1,kac(n1+1)-1);close (33,status='delete')
     b;endif;icrash=1;
      end subroutine crash_VK
