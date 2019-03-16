      subroutine solutnx (mark,rhs,at,xs,colbeg,colendx,
     \aij0,inda0,aij,inda,kb1,kb2,pivots,pivrows,ketac,ketah)
      include 'mifdp.inc'
      include 'mifcomx.inc'
      real(8) RANGE; integer(4) ID;integer inda(nffr:memtop),mark(n+1),kb1(m1),kb2(m1),ketac(*),     inda0(nzr);integer colbeg(n1+1)
     l,colendx(n1),ketah(*);integer pivrows(m1);double precision aij(nffr:memtop),at(m4),xs(n+1),rhs(m1),         aij0(nzr);double p
     lrecision pivots(m1);integer(4) i,j,mj,kbi,np1;do 10 j=1,n;mj=mark(j);if (mj .gt. 3 .and. mj .lt. 9) mark(j)=mj-4
10    continue;do 20 i=1,m1;j=kb1(i);mj=mark(j);if (mj .eq. 9) mj=1;kb2(i)=mj;mark(j)=mj+4
  20  continue;do 30 i=1,m1; at(i)=rhs(i)
  30  continue;np1=n+1;xs(np1)=rhs(m+1);do 40 i=n1+1,n1+m;if ((mark(i).eq.9).or.((mark(i).lt.4).and.(xs(i).ne.0.0))) then;range= xs(
     li);at(i-n1)=at(i-n1)-xs(i);endif
  40  continue;do 60 j=1,n1;if ((mark(j).eq.9).or.((mark(j).lt.4).and.(xs(j).ne.0.0))) then;range= xs(j);do 50 i=colbeg(j),colendx(j
     l);id=inda0(i); at(id)=at(id)-range*aij0(i)
  50  continue;endif
  60  continue;call ftranl(m,nffr,irang,pivots,pivrows,ketac(1),ketah(1),
     *at(1),inda,aij);call ftranu(nffr,irang,pivrows,ketac(m1+1),ketah(m1+1),
     *at(1),inda,aij);do 70 i=1,m1;kbi=kb1(i);xs(kbi)=at(i)
70    continue;return;
      end subroutine solutnx
