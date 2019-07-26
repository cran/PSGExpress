      SUBROUTINE FTRANL (M,nffr,PIVOTN,PIVOTS,PIVROWS,BPNT,EPNT,VECTOR,FACIND,FACTOR);INTEGER M,nffr,PIVOTN,PIVROWS(M+1),BPNT(M+1);i
     anteger EPNT(M+1),facind(nffr:*);double precision PIVOTS(M+1),VECTOR(M+1),factor(nffr:*);integer(4) I,J,PNT1,PNT2;real(8)    S;
      DO 10 I=1,PIVOTN;S=VECTOR(PIVROWS(I));IF (S.NE.0) THEN;s=s/pivots(i);PNT1=BPNT(I);PNT2=EPNT(I);VECTOR(PIVROWS(I))=S;DO 20 J=PN
     nT1,PNT2;VECTOR(FACIND(J))=VECTOR(FACIND(J))-S*FACTOR(J)
  20  CONTINUE;ENDIF
  10  CONTINUE;RETURN;end subroutine ftranl;SUBROUTINE FTRANU (nffr,PIVOTN,PIVROWS,BPNT,EPNT,VECTOR,   FACIND,FACTOR);integer(4) nff
     hr,PIVOTN,PIVROWS(*),BPNT(*),EPNT(*),  FACIND(nffr:*);real(8) VECTOR(*),FACTOR(nffr:*);integer(4) I,J,PNT1,PNT2;real(8)    S;DO
     u 10 I=PIVOTN,1,-1;S=VECTOR(PIVROWS(I));IF(S.NE.0)THEN;PNT1=BPNT(I);PNT2=EPNT(I);DO 20 J=PNT1,PNT2;VECTOR(FACIND(J))=VECTOR(FAC
     aIND(J))-S*FACTOR(J)
  20  CONTINUE;ENDIF
  10  CONTINUE;RETURN;END;SUBROUTINE BTRANU (nffr,FREE,PIVOTN,PIVROWS,BPNT,EPNT,VECTOR,FACIND,FACTOR);integer(4) nffr,free,pivotn,pi
     ovrows(*),bpnt(*),epnt(*),facind(nffr:free);real(8) vector(*),factor(nffr:free);integer(4) I,J,K,L,PNT1,PNT2;real(8)    SS;DO 1
     z0 I=1,PIVOTN;L=PIVROWS(I);SS=-VECTOR(L);PNT1=BPNT(I);PNT2=EPNT(I);DO 20 J=PNT1,PNT2;K=FACIND(J);SS=SS+VECTOR(K)*FACTOR(J)
  20  CONTINUE;VECTOR(L)=-SS
  10  CONTINUE;RETURN;end subroutine btranu;SUBROUTINE BTRANL (nffr,PIVOTN,PIVOTS,PIVROWS,BPNT,EPNT,VECTOR,FACIND,FACTOR);integer(4)
     e nffr,pivotn,pivrows(*),bpnt(*),epnt(*),facind(nffr:*);real(8) pivots(*),vector(*),factor(nffr:*);integer(4) I,II,J,K,L,PNT1,P
     vNT2;real(8)    SS;DO 10 II=1,PIVOTN;I=PIVOTN-II+1;L=PIVROWS(I);SS=-VECTOR(L);PNT1=BPNT(I);PNT2=EPNT(I);DO 20 J=PNT1,PNT2;K=FAC
     pIND(J);SS=SS+VECTOR(K)*FACTOR(J)
  20  CONTINUE;VECTOR(L)=-SS/PIVOTS(I)
  10  CONTINUE;RETURN;end subroutine btranl;subroutine btranud (nffr,free,pivotn,pivrows,bpnt,epnt,vector,facind,factor,vnorm);integ
     ler nffr,free,pivotn,pivrows(*),bpnt(*),epnt(*),facind(nffr:free);double precision vector(*),factor(nffr:free),vnorm;integer i,
     tj,k,l,pnt1,pnt2;double precision ss,tmp;intrinsic abs;do 10 i=1,pivotn;l=pivrows(i);ss=-vector(l);pnt1=bpnt(i);pnt2=epnt(i);do
     w 20 j=pnt1,pnt2;k=facind(j);tmp = vector(k)*factor(j);if (abs(tmp).gt.vnorm) then;vnorm = abs(tmp);endif;ss = ss + tmp
  20  continue;vector(l)=-ss
  10  continue;return;end;subroutine btranld(nffr,pivotn,pivots,pivrows,bpnt,epnt,vector,facind,factor,vnorm);integer(4) nffr,pivotn
     g,pivrows(*),bpnt(*),epnt(*),facind(nffr:*);double precision vnorm,pivots(*),vector(*),factor(nffr:*);integer i,ii,j,k,l,pnt1,p
     gnt2;double precision ss,tmp;intrinsic abs;do 10 ii=1,pivotn;i=pivotn-ii+1;l=pivrows(i);ss=-vector(l);pnt1=bpnt(i);pnt2=epnt(i)
       do 20 j=pnt1,pnt2;k=facind(j);tmp = vector(k)*factor(j);if (abs(tmp).gt.vnorm) then;vnorm = abs(tmp);endif;ss = ss + tmp
  20  continue;vector(l)=-ss/pivots(i)
  10  continue;return;
      end
