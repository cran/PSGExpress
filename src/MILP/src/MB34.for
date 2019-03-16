      SUBROUTINE FTRANL (M,nffr,PIVOTN,PIVOTS,PIVROWS,BPNT,EPNT,
     *VECTOR,FACIND,FACTOR);INTEGER M,nffr,PIVOTN,PIVROWS(M+1),BPNT(M+1);integer EPNT(M+1),facind(nffr:*);double precision PIVOTS(M+
     l1),VECTOR(M+1),factor(nffr:*);integer(4) I,J,PNT1,PNT2;real(8)    S;DO 10 I=1,PIVOTN;S=VECTOR(PIVROWS(I));IF (S.NE.0) THEN;s=s
     l/pivots(i);PNT1=BPNT(I);PNT2=EPNT(I);VECTOR(PIVROWS(I))=S;DO 20 J=PNT1,PNT2;VECTOR(FACIND(J))=VECTOR(FACIND(J))-S*FACTOR(J)
  20  CONTINUE;ENDIF
  10  CONTINUE;RETURN;end subroutine ftranl;SUBROUTINE FTRANU (nffr,PIVOTN,PIVROWS,BPNT,EPNT,VECTOR,   FACIND,FACTOR);integer(4) nff
     lr,PIVOTN,PIVROWS(*),BPNT(*),EPNT(*),  FACIND(nffr:*);real(8) VECTOR(*),FACTOR(nffr:*);integer(4) I,J,PNT1,PNT2;real(8)    S;DO
     l 10 I=PIVOTN,1,-1;S=VECTOR(PIVROWS(I));IF(S.NE.0)THEN;PNT1=BPNT(I);PNT2=EPNT(I);DO 20 J=PNT1,PNT2;VECTOR(FACIND(J))=VECTOR(FAC
     lIND(J))-S*FACTOR(J)
  20  CONTINUE;ENDIF
  10  CONTINUE;RETURN;END;SUBROUTINE BTRANU (nffr,FREE,PIVOTN,PIVROWS,BPNT,
     XEPNT,VECTOR,FACIND,FACTOR);integer(4) nffr,free,pivotn,pivrows(*),bpnt(*),epnt(*),facind(nffr:free);real(8) vector(*),factor(n
     lffr:free);integer(4) I,J,K,L,PNT1,PNT2;real(8)    SS;DO 10 I=1,PIVOTN;L=PIVROWS(I);SS=-VECTOR(L);PNT1=BPNT(I);PNT2=EPNT(I);DO 
     l20 J=PNT1,PNT2;K=FACIND(J);SS=SS+VECTOR(K)*FACTOR(J)
  20  CONTINUE;VECTOR(L)=-SS
  10  CONTINUE;RETURN;end subroutine btranu;SUBROUTINE BTRANL (nffr,PIVOTN,PIVOTS,PIVROWS,BPNT,EPNT,VECTOR,
     *FACIND,FACTOR);integer(4) nffr,pivotn,pivrows(*),bpnt(*),epnt(*),facind(nffr:*);real(8) pivots(*),vector(*),factor(nffr:*);int
     leger(4) I,II,J,K,L,PNT1,PNT2;real(8)    SS;DO 10 II=1,PIVOTN;I=PIVOTN-II+1;L=PIVROWS(I);SS=-VECTOR(L);PNT1=BPNT(I);PNT2=EPNT(I
     l);DO 20 J=PNT1,PNT2;K=FACIND(J);SS=SS+VECTOR(K)*FACTOR(J)
  20  CONTINUE;VECTOR(L)=-SS/PIVOTS(I)
  10  CONTINUE;RETURN;end subroutine btranl;subroutine btranud (nffr,free,pivotn,pivrows,bpnt,
     \epnt,vector,facind,factor,vnorm);integer nffr,free,pivotn,pivrows(*),bpnt(*),epnt(*),facind(nffr:free);double precision vector
     l(*),factor(nffr:free),vnorm;integer i,j,k,l,pnt1,pnt2;double precision ss,tmp;intrinsic abs;do 10 i=1,pivotn;l=pivrows(i);ss=-
     lvector(l);pnt1=bpnt(i);pnt2=epnt(i);do 20 j=pnt1,pnt2;k=facind(j);tmp = vector(k)*factor(j);if (abs(tmp).gt.vnorm) then;vnorm 
     l= abs(tmp);endif;ss = ss + tmp
  20  continue;vector(l)=-ss
  10  continue;return;end;subroutine btranld(nffr,pivotn,pivots,pivrows,bpnt
     \,epnt,vector,facind,factor,vnorm);integer(4) nffr,pivotn,pivrows(*),bpnt(*),epnt(*),facind(nffr:*);double precision vnorm,pivo
     lts(*),vector(*),factor(nffr:*);integer i,ii,j,k,l,pnt1,pnt2;double precision ss,tmp;intrinsic abs;do 10 ii=1,pivotn;i=pivotn-i
     li+1;l=pivrows(i);ss=-vector(l);pnt1=bpnt(i);pnt2=epnt(i);do 20 j=pnt1,pnt2;k=facind(j);tmp = vector(k)*factor(j);if (abs(tmp).
     lgt.vnorm) then;vnorm = abs(tmp);endif;ss = ss + tmp
  20  continue;vector(l)=-ss/pivots(i)
  10  continue;return;
      end
