      subroutine devset(aij,inda,kac,mark,kb1,djrow,objrow,pi
     *,ketac,ketah,pivrows,pivots,inref,colw
     *,mr,nrc,jas,jats)
      include 'mifcomx.inc'
      integer mr,nrc,jas,jats;integer inda(*),kac(n1+1),mark(n),kb1(m1);integer ketac(2*m),ketah(2*m);integer pivrows(mr);integer(2)
     l inref(nrc);real colw(nrc);double precision aij(*),djrow(nrc),objrow(n);double precision pi(mr),pivots(mr);devex = 1;iapr = 2;
      call djcomp(aij,inda,kac,mark,kb1,djrow,objrow,pi
     *,ketac,ketah,pivrows,pivots
     *,mr,nrc,jas,jats);call devrec(inref,mark,colw,nrc);return;end subroutine devset;subroutine djcomp(aij,inda,kac,mark,kb1,djrow,
     lobjrow,pi,
     *ketac,ketah,pivrows,pivots,
     *mr,nrc,jas,jats)
      include 'mifcomx.inc'
      integer mr,nrc,jas,jats;integer inda(nffr:jas),kac(n1+1),mark(n),kb1(m1),ketac(m2),ketah(m2);integer pivrows(mr);double precis
     lion aij(nffr:jas),djrow(nrc),objrow(nrc),pi(mr);double precision pivots(mr);integer i,j,mrj;double precision w,djp,djn;do 100 
     li=1,m;pi(i)=objrow(kb1(i))
100   continue;pi(m+1)=0.0;call btradv(aij,inda,jas,pi,jats,ketac,jats,ketah,
     *pivrows,pivots);do 110 j=1,n;mrj=mark(j);if (mrj.gt.3.and.mrj.lt.9) then;djrow(j)=0.0;else;if (j.gt.n1) then;djrow(j)=-pi(j-n1
     l);else;djp=0.0;djn=0.0;do 30 i=kac(j),kac(j+1)-1;w = pi(inda(i))*aij(i);if (w.gt.0) then;djp = djp+w;else;djn = djn+w;endif
30    continue;djrow(j) = objrow(j) - (djp+djn);endif;endif
110   continue;djset = 1;return;end subroutine djcomp;subroutine devrec(inref,mark,colw,nrc);integer   nrc;integer(2) inref(nrc);int
     leger   mark(nrc);real      colw(nrc);integer j,mrj;do 100 j=1,nrc;colw(j) =1.0;mrj=mark(j);if (mrj.gt.3.and.mrj.lt.9) then;inr
     lef(j) = 0;else;inref(j) = 1;endif
100   continue;return;
      end
