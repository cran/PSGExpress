      subroutine tomilp_CtPl
     \(m,n1,nnz,mc,big,
     \inda,aij,
     \kac,colend, colendx,
     \rowtyp,mark,rhs,
     \lbounds,ubounds,
     \relk,rng,
     \objrow,     kb1,xs);integer(4) KBP,ISTOP,INPK,INPP,LOG_PR,IDB;integer(1) un; common/un/un;character workpath*256;common /contr
     l/ inpk,inpp,log_pr,istop,iDB, workpath;integer(4) kb1(*); real(8) xs(*);integer(4) m,n1,nnz,mc;integer(4) inda(nnz),kac(*),row
     ltyp(*),mark(*),  colend(*), colendx(*);double precision aij(nnz),rhs(*),  lbounds(*),ubounds(*),rng(*),objrow(*);character(1) 
     lrelk(*);integer(4) kr,lp,jjas,m1,m2,m3,m4,m5,m6,memtop;common /sizes/  m1,m2,m3,m4,m5,m6,kr,lp,jjas,memtop;double precision,al
     llocatable:: lb(:),ub(:), at(:);double precision big,lbig,lbj,ubj;integer(4) i,j,k,ka,kf,ia,ib;allocate(at(3*m));lbig=0.95*big;
      m1=m;m2=m+m;m3=m2+m;m4=m3+m;m5=m4+m;m6=m5+m;kbp=0;allocate( lb(n1),ub(n1) ); lb=lbounds(:n1); ub=ubounds(:n1);do 120 j=1,n1;lb
     lj=lb(j);  ubj=ub(j); xs(j)=0d0;if(mark(j)>3.and.mark(j)<9) then; kbp=kbp+1; kb1(kbp)=j; endif;if (lbj.lt.-lbig.and.ubj.gt.lbig
     l) then;mark(j)=3;  rng(j)=2*big;elseif (lbj.gt.-lbig.and.ubj.lt.lbig) then;rng(j)=ubj-lbj;if(mark(j)==9) then; xs(j)=rng(j);el
     lse; mark(j)=1;endif;if (lbj.eq.ubj) then;mark(j)=0;  rng(j)=0.0;endif;else;mark(j)=2;  rng(j)=big;endif;if (lbj.ne.0.0.and.abs
     l(lbj).lt.lbig) then;ka=kac(j);  kf=kac(j+1)-1;do 125 i=ka,kf;rhs(inda(i))=rhs(inda(i))-lbj*aij(i)
125   continue;endif
120   continue;mark(n1+mc)=7;do 100 i=1,m; ia=n1+i; j=mark(ia); xs(ia)=0d0;if(j>3.and.j<9) then; kbp=kbp+1; kb1(kbp)=ia; mark(ia)=j-
     l4;endif;select case(rowtyp(i));case(0); relk(i)='=';  mark(ia)=0;case(1); relk(i)='<';if(j==9)then; xs(ia)=rng(ia); endif;case
     l(3); relk(mc)='N'; mark(ia)=3;case default; stop 'tomilp_err';end select
100   continue;do 110 k=1,n1;ia=kac(k);ib=kac(k+1)-1;if(ib>=ia) then;if (inda(ib).eq.m) then;colend(k) = ib - 1;colendx(k) = ib;objr
     low(k) = aij(ib);else;colend(k) = ib;colendx(k) = ib;objrow(k) = 0d0;endif;else;colend(k) = ib;colendx(k) = ib;objrow(k) = 0.0;
      endif
110   continue;if(kbp/=m) then; if(un>=0)write(un,*)'tomilp_CtPl: kbp/=m',kbp,m; istop=3;endif;deallocate(lb,ub,at);return;end subro
     lutine tomilp_CtPl;subroutine Clocki(r8);real(8) r8; r8=0d0;return; end;subroutine ChngMrkXs(n1,m1,rng,mark,xs);integer(4) n1,m
     l1, mark(*),i;real(8) rng(*), xs(*);do i=1,n1+m1; if(mark(i)==9)then; xs(i)=rng(i); endif; enddo;
      end
