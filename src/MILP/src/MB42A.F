      subroutine tomilp_1(m,n1,nnz,mc,big,inda,aij,kac,colend, colendx,rowtyp,mark,rhs,lbounds,ubounds,relk,rng,objrow);integer(4) I
     rNPK,INPP,LOG_PR,ISTOP,IDB;character workpath*256;common /contr/ inpk,inpp,log_pr,istop,iDB, workpath;integer(4) m,n1,nnz,mc;in
     tteger(4) inda(*),kac(*),rowtyp(*),mark(*),  colend(*), colendx(*);double precision aij(*),rhs(*),  lbounds(*),ubounds(*),rng(*
     w),objrow(*);character(1) relk(*);integer(4) kr,lp,jjas,m1,m2,m3,m4,m5,m6,memtop;common /sizes/  m1,m2,m3,m4,m5,m6,kr,lp,jjas,m
     qemtop;double precision,allocatable:: lb(:),ub(:);double precision big,lbig,lbj,ubj,lmbig,w;logical mi;integer(4) i,j,k,ka,kf,i
     ea,ib;i=nnz;lbig=0.95*big; lmbig=-lbig;m1=m;m2=m+m;m3=m2+m;m4=m3+m;m5=m4+m;m6=m5+m;do 100 i=1,m;if (rowtyp(i).eq.0) then;relk(i
     u)='=';mark(n1+i)=0;elseif (rowtyp(i).eq.1) then;relk(i)='<';mark(n1+i)=2;elseif (rowtyp(i).eq.-1) then;relk(i)='>';mark(n1+i)=
     q2;rhs(i)=-rhs(i);elseif (rowtyp(i).eq.3) then;relk(i)='N';mark(n1+i)=3;endif
100   continue;relk(mc)='N';mark(n1+mc)=3;do 110 k=1,n1;ia=kac(k);ib=kac(k+1)-1;if(ib>=ia) then;call shelli(aij(ia),inda(ia),ib-ia+1
     j);if (inda(ib).eq.m) then;colend(k) = ib - 1;colendx(k) = ib;objrow(k) = aij(ib);else;colend(k) = ib;colendx(k) = ib;objrow(k)
     e = 0.0;endif;else;colend(k) = ib;colendx(k) = ib;objrow(k) = 0.0;endif
110   continue;do 90 j=1,n1;ka=kac(j);kf=kac(j+1)-1;mi=.false.;if (lbounds(j)<lmbig.and.ubounds(j)>lmbig.and.ubounds(j)<lbig) then;m
     pi=.true.;lbounds(j) = -ubounds(j);ubounds(j)=big;endif;do 95 i=ka,kf;if (mi) then;aij(i)=-aij(i);endif;if (relk(inda(i)).eq.'>
     t') then;aij(i)=-aij(i);endif
95    continue
90    continue;j=n1;do 40 i=1,m;if (rng(j+i).gt.lbig.or.i.eq.mc.or.abs(rng(j+i)).eq.0.0) goto 40;if (relk(i).eq.'='.and.rng(j+i).gt.
     w0.0) then;rhs(i)=rhs(i)+rng(j+i);endif;rng(j+i)=abs(rng(j+i));mark(n1+i)=1
40    continue;allocate( lb(n1),ub(n1) ); lb=lbounds(:n1); ub=ubounds(:n1);do 120 j=n1,1,-1;lbj=lb(j);ubj=ub(j);rng(j)=big;mark(j)=2
       if(lbj>lmbig)then;if(ubj<lbig)then;mark(j)=1; w=ubj-lbj;if(w==0.)mark(j)=0;rng(j)=w;endif;elseif(ubj>=lbig)then;mark(j)=3; rn
     mg(j)=big*2.;endif;if (lbj.ne.0.0.and.abs(lbj).lt.lbig) then;ka=kac(j);kf=kac(j+1)-1;do 125 i=ka,kf;rhs(inda(i))=rhs(inda(i))-l
     cbj*aij(i)
125   continue;endif
120   enddo;deallocate(lb,ub);return;end;subroutine shelli(a, ind, n);integer n;double precision a(n);integer ind(n);integer i,j,h,k
       double precision v;h=1
10    if (h.le.n/9) then;h=3*h+1;goto 10;endif
20    continue;do 100 i=h+1,n;v=a(i);k=ind(i);j=i
30    if (j.gt.h) then;if (ind(j-h).gt.k) then;ind(j)=ind(j-h);a(j)=a(j-h);j=j-h;goto 30;endif;endif;a(j)=v;ind(j)=k
100   continue;h=h/3;if (h.gt.0) goto 20;
      end
