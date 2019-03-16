      subroutine invert (aij0,inda0,aij,inda,jas,rhs,kb1,kb2,kb3,
     *at,mark,colbeg,colendx,
     *ketac,ketah,xs,jc,jd,je,pivrows,pivots,
     *jp1,jp2,jp3,jp,jjp,
     *iat,ibs,mr,nc)
      include 'mifdp.inc'
      include 'mifcomx.inc'
      integer mr,nc;integer(4) JJP,JAS,ISTOP,INPK,INPP,LOG_PR,IDB;integer(1) un; common/un/un;character(256) workpath;common /contr/
     l inpk,inpp,log_pr,istop,iDB, workpath;double precision aij0(*),aij(*),at(m4);double precision xs(mr+nc+1),rhs(m1),pivots(m1);i
     lnteger kb1(m1), kb2(m1), kb3(m1);integer inda0(*),inda(*),jc(m1),jd(m1),je(m1),ketac(m2),ketah(m2);integer mark(mr+nc),colbeg(
     lnc+1),colendx(n1);integer pivrows(m1),jp1(m1),jp2(m1),jp3(m1),jp(jjp),iat(*);integer i,nx,triang,ibs(3);real(8)    tp1,tp2;TP1
     l=1.0D-10;TP2=5.0D-02;IVC=IVC+1;NADR=colbeg(N1+1);nx = n1 + 1;jas=memtop;CALL INVERTX (NX,M+1,nffr,JAS,IRANG,KETAC(1),KETAC(M1+
     l1),
     *KETAH(1),KETAH(M1+1),KB1,KB2,KB3,IAT(1),IAT(M1+1),
     *PIVROWS,JC,JD,JE,
     *AT(1),PIVOTS,IAT(M2+1),inda0,aij0,INDA,AIJ,colbeg,colendx,
     *TP1,TP2,TRIANG,
     *JJP,JP,JP1,JP2,JP3,
     *IAT(M3+1),IAT(M4+1),IAT(M5+1),IBS,N1);if(istop>0) RETURN;IETA=0;DO 30 I=1,M1;IF (I.LE.IRANG) THEN;IETA=IETA+KETAH(I)+KETAH(I+M
     l1);ENDIF
  30  CONTINUE;IBS(3) = IETA;IF (TRIANG.EQ.M1) GOTO 999
 999  NADR=1;DO 50 I=1,M1;JC(I)=0;KETAH(  I)=KETAC(  I)+KETAH(  I)-1;KETAH(M1+I)=KETAC(M1+I)+KETAH(M1+I)-1
  50  CONTINUE;NETA   = M2;KETPNT = KETAH(NETA)+1;JAS    = KETAC(M1)-2;NFREE  = JAS-KETPNT;IF (NFREE.LT.N) THEN;if(un>=0)write(un,*)
     l 'NOT ENOUGH MEMORY AFTER INVERT';istop=311; RETURN;ENDIF;call solutnx (mark,rhs,at,xs,colbeg,colendx,
     \aij0,inda0,aij,inda,kb1,kb2, pivots,pivrows,ketac,ketah);return;
      end subroutine invert
