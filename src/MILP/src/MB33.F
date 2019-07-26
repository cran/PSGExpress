      SUBROUTINE FTRA (AIJ,INDA,JAS,AT,JATS,KETAC,KETAH,PIVROWS,PIVOTS)
      INCLUDE 'mifdp.inc'
      INCLUDE 'mifcomx.inc'
      integer(4) KETAC,KETAH,INDA,JAS,JATS;real(8) AT,AIJ;DIMENSION AIJ(*),INDA(*),AT(JATS);DIMENSION KETAC(neta),KETAH(neta);intege
     nr(4) PIVROWS(M);real(8)    PIVOTS(M);integer(4) I,DB;DO 10 I=0,MM-1;CALL FTRANL(M,nffr,IRANG,PIVOTS,PIVROWS,KETAC(1),KETAH(1),
     wAT(I*M1+1),inda,aij);CALLFTRANU(nffr,IRANG,PIVROWS,KETAC(M1+1),KETAH(M1+1),AT(I*M1+1),inda,aij);DB=NETA-2*M1;IF (DB.GT.0)CALL 
     eFTRANX(M,DB,nffr,JAS,KETAC(M2+1),KETAH(M2+1),INDA,AIJ,AT(I*M1+1))
  10  CONTINUE;RETURN;END;SUBROUTINE BTRADV (AIJ,INDA,JAS,AT,JATS,KETAC,I1,KETAH,PIVROWS,PIVOTS)
      INCLUDE 'mifdp.inc'
      INCLUDE 'mifcomx.inc'
      integer(4) JAS,JATS,I1,KETAC,KETAH,INDA; real(8) AIJ,AT;DIMENSION AIJ(nffr:JAS),INDA(nffr:JAS),AT(JATS),KETAC(I1),KETAH(1);int
     beger(4) DB;integer(4) PIVROWS(M1);real(8)    PIVOTS(M1);DB=NETA-M2;CALL BTRANX (M,DB,nffr,JAS,KETAC(M2+1),KETAH(M2+1),INDA,AIJ
     m,AT(1));CALL BTRANU (nffr,nfree+nffr-nadr,IRANG,PIVROWS,KETAC(M1+1),KETAH(M1+1),AT(1),inda,aij);CALL BTRANL (nffr,IRANG,PIVOTS
     z,PIVROWS,KETAC(1),KETAH(1),AT(1),inda,aij);RETURN;END;subroutine btrand (aij,inda,at,jats,ketac,ketah,pivrows,pivots,vnorm)
      include 'mifdp.inc'
      include 'mifcomx.inc'
      integer(4) JATS,KETAC,KETAH,INDA; real(8) AIJ,AT;dimension aij(*),inda(*),at(jats),ketac(*),ketah(*);integer db;integer pivrow
     xs(m1);double precision vnorm,pivots(m1);vnorm = 0.0;db=neta-m2;call btranxd (m,db,nffr,ketac(m2+1),ketah(m2+1),inda,aij,at(1),
     rvnorm);call btranud (nffr,nfree,irang,pivrows,ketac(m1+1),ketah(m1+1),at(1),inda,aij,vnorm);call btranld (nffr,irang,pivots,pi
     fvrows,ketac(1),ketah(1),at(1),inda,aij,vnorm);return;end;SUBROUTINE BTRA (AIJ,INDA,JAS,AT,JATS,KETAC,KETAH,PIVROWS,PIVOTS)
      INCLUDE 'mifdp.inc'
      INCLUDE 'mifcomx.inc'
      integer(4) JATS,JAS,KETAC,KETAH,INDA; real(8) AIJ,AT;dimension aij(*),inda(*),at(jats),ketac(*),ketah(*);integer(4) DB;integer
     w(4) PIVROWS(1);real(8)    PIVOTS(1);DB=NETA-M2;CALL BTRANX(M,DB,nffr,JAS,KETAC(M2+1),KETAH(M2+1),INDA,AIJ,AT(1));CALL BTRANU(n
     fffr,nfree+nffr-nadr,IRANG,PIVROWS,KETAC(M1+1),KETAH(M1+1),AT(1),inda,aij);CALL BTRANL(nffr,IRANG,PIVOTS,PIVROWS,KETAC(1),KETAH
     p(1),AT(1),inda,aij);if (idyn.eq.1) then;CALL BTRANX(M,DB,nffr,JAS,KETAC(M2+1),KETAH(M2+1),INDA,AIJ,AT(M1+1));CALL BTRANU(nffr,
     wnfree+nffr-nadr,IRANG,PIVROWS,KETAC(M1+1),KETAH(M1+1),AT(M1+1),inda,aij);CALL BTRANL(nffr,IRANG,PIVOTS,PIVROWS,KETAC(1),KETAH(
     n1),AT(M1+1),inda,aij);endif;if (iadc.eq.1.and.ninf.gt.0) then;CALL BTRANX(M,DB,nffr,JAS,KETAC(M2+1),KETAH(M2+1),INDA,AIJ,AT(M3
     i+1));CALL BTRANU(nffr,nfree+nffr-nadr,IRANG,PIVROWS,KETAC(M1+1),KETAH(M1+1),AT(M3+1),inda,aij);CALL BTRANL(nffr,IRANG,PIVOTS,P
     jIVROWS,KETAC(1),KETAH(1),AT(M3+1),inda,aij);endif;if (ideg.eq.1)then;CALL BTRANX(M,DB,nffr,JAS,KETAC(M2+1),KETAH(M2+1),INDA,AI
     oJ,AT(M2+1));CALL BTRANU(nffr,nfree+nffr-nadr,IRANG,PIVROWS,KETAC(M1+1),KETAH(M1+1),AT(M2+1),inda,aij);CALL BTRANL(nffr,IRANG,P
     lIVOTS,PIVROWS,KETAC(1),KETAH(1),AT(M2+1),inda,aij);ENDIF;RETURN;END;SUBROUTINE FTRANX(M,DB,nffr,JAS,PNT,CNT,FACIND,FACTOR,VECT
     aOR);integer(4) M,DB,nffr,JAS,pnt(*),cnt(*),FACIND(nffr:JAS);real(8)    FACTOR(nffr:JAS),vector(*);integer(4) PNT1,PNT2,I,J,P;r
     meal(8)    PP,P1;i=m;DO 10 I=1,DB;PNT1=PNT(I);P=FACIND(PNT1);P1=VECTOR(P);IF (P1.EQ.0) GOTO 10;PP=FACTOR(PNT1);PNT2=PNT1+CNT(I)
     g-1;P1=P1/PP;VECTOR(P)=P1;PNT1=PNT1+1;DO 20 J=PNT1,PNT2;VECTOR(FACIND(J))=VECTOR(FACIND(J))-FACTOR(J)*P1
  20  CONTINUE
  10  CONTINUE;RETURN;END;SUBROUTINE BTRANX(M,DB,nffr,JAS,PNT,CNT,FACIND,FACTOR,VECTOR);integer(4) M,DB,nffr,JAS,pnt(*),cnt(*),FACIN
     xD(nffr:JAS);real(8)    FACTOR(nffr:JAS),vector(*);integer(4) PNT1,PNT2,I,J,P;real(8)    S;i=m;IF (DB.LE.0) GOTO 999;DO 10 I=DB
     x,1,-1;PNT1=PNT(I);PNT2=PNT1+CNT(I)-1;P=FACIND(PNT1);S=VECTOR(P);PNT1=PNT1+1;DO 20 J=PNT1,PNT2;S=S-FACTOR(J)*VECTOR(FACIND(J))
  20  CONTINUE;VECTOR(P)=S/FACTOR(PNT1-1)
  10  CONTINUE
 999  RETURN;END;subroutine btranxd (m,db,nffr,pnt,cnt,facind,factor,vector,vnorm);integer m,db,nffr,pnt(*),cnt(*),facind(nffr:*);do
     uuble precision vnorm,factor(nffr:*),vector(*);integer pnt1,pnt2,i,j,p;double precision s,tmp;i=m;vnorm = 1.0;if (db.le.0) goto
     r 999;do 10 i=db,1,-1;pnt1=pnt(i);pnt2=pnt1+cnt(i)-1;p=facind(pnt1);s=vector(p);pnt1=pnt1+1;do 20 j=pnt1,pnt2;tmp = factor(j)*v
     cector(facind(j));if (abs(tmp).gt.vnorm) then;vnorm = abs(tmp);endif;s = s - tmp
  20  continue;vector(p)=s/factor(pnt1-1)
  10  continue
 999  return;
      end
