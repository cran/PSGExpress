      SUBROUTINE PARA
      INCLUDE 'mifdp.inc'
      INCLUDE 'mifcomx.inc'
      integer(4) K,I,LN,K1,IDIRO;integer kpara;parameter (kpara =10);character(5) CH(kpara),SP*1;CHARACTER PUF*5,PUFIX*20,PUFIXX(20)
     l,FORMA*4,FORM(4);EQUIVALENCE (FORMA,FORM(1)),(PUFIX,PUFIXX(1));DATA CH/'epiv=','eopt=','erel=','diro=','nsub=','ivfr=','dufr='
     l,
     1'trac=','deve=','ok'/;FORMA='(I9)';SP=' '
220   WRITE (*,'(1X,A,/)')
     1'Modification: parameter=value (type "ok" to escape)';READ (*,'(2A)') PUF,PUFIX;K=0;DO 230 I=1,kpara; IF (CH(I).EQ.PUF) K=I
230   continue;IF (K.EQ.0) THEN;WRITE (*,'(1X,A,/)') 'UNNKNOWN IDENTIFIER';GOTO 220;ENDIF;IF (K.EQ.kpara) GOTO 300;LN=20;DO 301 I=20
     l,1,-1;IF (PUFIXX(I).EQ.SP) THEN;LN=LN-1;ELSE;GOTO 302;ENDIF
301   CONTINUE
302   IF (K.GT.3.AND.(LN.LT.1.OR.LN.GT.9)) THEN;WRITE (*,'(1X,A)') 'Integer too long';GOTO 220;ELSE;WRITE (FORM(3),'(I1)') LN;ENDIF;
      select case(k);case(1:3);goto 10; case(4);goto 40; case(5);goto 50; case(6);goto 60; case(7);goto 70; case(8);goto 80; case(9)
     l;goto 90;end select
10    continue;select case(k);case(1); READ (PUFIX,'(G15.5)',ERR=900) EPIV;case(2); READ (PUFIX,'(G15.5)',ERR=900) EOPT;case(3); REA
     lD (PUFIX,'(G15.5)',ERR=900) EREL;end select;GOTO 300
40    READ (PUFIX,FORMA,ERR=900) K1;IF (K1.EQ.1.OR.K1.EQ.-1) THEN;IDIRO=K1;ELSE;WRITE (*,'(1X,A,/)') 'Error: diro must be 1 or -1';G
     lOTO 220;ENDIF;IF (IDIRO.EQ.1) THEN;EZ=-1.0;MA=1;ELSEIF (IDIRO.EQ.-1) THEN;EZ=1.0;MA=-1;ENDIF;GOTO 300
50    READ (PUFIX,FORMA,ERR=900) K1;IF (K1.GE.1.AND.K1.LE.NSUB) THEN;NN=K1;GOTO 300;ELSE;WRITE (*,'(1X,A,I2,/)') 'Error: nsub must b
     le between 1 and',NSUB;GOTO 220;ENDIF
60    READ (PUFIX,FORMA,ERR=900) K1;IF (K1.GT.0) THEN;IVFR=K1;GOTO 300;ELSE;WRITE (*,'(1X,A,/)') 'Error: ivfr must be positive';GOTO
     l 220;ENDIF
70    READ (PUFIX,FORMA,ERR=900) K1;IF (K1.GT.0) THEN;IDUF=K1;GOTO 300;ELSE;WRITE (*,'(1X,A,/)') 'Error: dufr must be positive';GOTO
     l 220;ENDIF
80    READ (PUFIX,FORMA,ERR=900) K1;IF (K1.GE.0) THEN;KTRA=K1;GOTO 300;ELSE;WRITE (*,'(1X,A,/)') 'Error: trac must be positive';GOTO
     l 220;ENDIF
90    READ (PUFIX,FORMA,ERR=900) K1;IF (K1.EQ.0.OR.K1.EQ.1.OR.K1.EQ.2) THEN;DEVREQ=K1;GOTO 300;ELSE;WRITE (*,'(1X,A,/)') 'Error: dev
     le must be 0 or 1 or 2';GOTO 220;ENDIF
900   WRITE (*,'(1X,A,/)') 'Data type error';GOTO 220
300   RETURN;END;SUBROUTINE PARD
      INCLUDE 'mifdp.inc'
      INCLUDE 'mifcomx.inc'
      integer(4) IDIRO;WRITE (*,'(/,1X,A)') 'RUN PARAMETERS'
110   FORMAT (1X,A,E10.4,A);WRITE (*,110)
     1'epiv=',EPIV,'...pivot tolerance';WRITE (*,110)
     1'eopt=',EOPT,'...primal optimality tolerance';WRITE (*,110)
     1'erel=',EREL,
     2'...relative zero tolerance in additive operations';IDIRO=1;IF (EZ.GT.0.0) IDIRO=-1;WRITE (*,120)
     1'diro=',IDIRO,
     2'...........direction of optimization: 1=max, -1=min';WRITE (*,120)
     1'nsub=',NN,
     2'...........number of vectors in suboptimization';WRITE (*,120)
     1'trac=',KTRA,'...........trace level of iteration report';WRITE (*,130)
     1'ivfr=',IVFR,'........frequency of regular reinversions';WRITE (*,130)
     1'dufr=',IDUF,'........frequency of dumping the current basis';WRITE (*,130)
     *'deve=',devreq,'........Devex pricing in Phase-2: 0=off, 1 or 2 =
     *on'
120   FORMAT (1X,A,I2,A)
130   FORMAT (1X,A,I5,A);END;SUBROUTINE ALTU
      INCLUDE 'mifdp.inc'
      INCLUDE 'mifcomx.inc'
      integer(4) K,I,LN,K1; real(8) RAT;integer ipar;parameter (ipar = 7);character(4) CH(ipar),SP*1;CHARACTER ICH,PUF*4,PUFIX*20,PU
     lFIXX(20),FORMA*4,FORM(4);EQUIVALENCE (FORMA,FORM(1)),(PUFIX,PUFIXX(1));DATA CH/'dyn=','deg=','adc=','lam=','aut=','rat=','ok'/
      FORMA='(I9)';SP=' ';CALL ALDI
210   WRITE(*,'(2(/,1X,A))') 'TUNING ALGORITHM',
     1'Give command (1:Display, 2:Modify, 3:Sprice, 4:Return): ';READ (*,'(A)') ICH;IF (ICH.EQ.'1') CALL ALDI;IF (ICH.EQ.'3') CALL S
     lPRI;IF (ICH.EQ.'4') GOTO 300;IF (ICH.NE.'2') GOTO 210
220   WRITE (*,'(1X,A)')
     1'Modification: parameter=value (escape: "ok")';READ (*,'(2A)') PUF,PUFIX;K=0;DO 230 I=1,IPAR; IF (CH(I).EQ.PUF) K=I
230   continue;IF (K.EQ.0) THEN;WRITE (*,'(1X,A,/)') 'UNNKNOWN IDENTIFIER';GOTO 220;ENDIF;IF (K.EQ.IPAR) GOTO 210;LN=20;DO 301 I=20,
     l1,-1;IF (PUFIXX(I).EQ.SP) THEN;LN=LN-1;ELSE;GOTO 302;ENDIF
301   CONTINUE
302   IF (LN.LT.1.OR.LN.GT.9) THEN;WRITE (*,'(1X,A)') 'Integer too long';GOTO 220;ELSE;WRITE (FORM(3),'(I1)') LN;ENDIF;select case(k
     l);case(1:3);goto 10; case(4);goto 40; case(5);goto 50; case(6);goto 60;end select
10    READ (PUFIX,FORMA,ERR=900) K1;IF (K1.EQ.0.OR.K1.EQ.1) THEN;select case(k);case(1); IDYN=k1;case(2); IDEG=k1;case(3); IADC=k1;e
     lnd select;GOTO 210;ELSE;GOTO 910;ENDIF
40    READ (PUFIX,'(G15.5)',ERR=900) BDA;GOTO 210
50    READ (PUFIX,FORMA,ERR=900) K1;IF (K1.EQ.0.OR.K1.EQ.1) THEN;IAPR=K1;GOTO 210;ELSE;GOTO 910;ENDIF
60    READ (PUFIX,'(G15.5)',ERR=900) rat;GOTO 210
900   WRITE (*,'(1X,A,/)') 'Data type error';GOTO 220
910   WRITE (*,'(1X,A,/)') 'Error: value must be 0 or 1';GOTO 220
300   IF (IAPR.EQ.1) THEN;CALL SPARI;CALL SPPREP;ENDIF;RETURN;END;SUBROUTINE ALDI
      INCLUDE 'mifdp.inc'
      INCLUDE 'mifcomx.inc'
      WRITE (*,'(/,1X,A)') 'ALGORITHMIC PARAMETERS (0:no, 1:yes)';WRITE (*,120)
     1'aut=',iapr,'........automatic sectional and cyclic pricing';WRITE (*,120)
     1'dyn=',idyn,'........dynamic scaling of reduced costs';WRITE (*,120)
     1'deg=',ideg,'........anti-degeneracy column selection';WRITE (*,120)
     1'adc=',iadc,'........adaptive composite phase-I algorithm';WRITE (*,130)
     1'lam=',bda,'....relative weight of the true obj. when adc=1';WRITE (*,130)
     1'rat=',devrat,'....Devex approx. failure ratio'
120   FORMAT (1X,A,I2,A)
130   FORMAT (1X,A,F6.3,A);END;SUBROUTINE SPRI
      INCLUDE 'mifdp.inc'
      INCLUDE 'mifcomx.inc'
      integer(4) KPAR,K,I,LN,K1,MSP,NP;character(4) CH(8),sp*1;CHARACTER ICH,PUF*4,PUFIX*20,PUFIXX(20),FORMA*4,FORM(4);EQUIVALENCE (
     lFORMA,FORM(1)),(PUFIX,PUFIXX(1));DATA CH/'msp=','nsc=','ksc=','kvc=','aut=','npa=','deg=','ok'/;sp=' ';kpar=8;FORMA='(I9)'
215   CALL SPARD;IF (IAPR.EQ.1) GOTO 999
210   WRITE(*,'(/,1X,A)')
     1'Give command (1:Display, 2:Modify, 3:Return): ';READ (*,'(A)') ICH;IF (ICH.EQ.'1') GOTO 215;IF (ICH.EQ.'3') GOTO 300;IF (ICH.
     lNE.'2') GOTO 210
220   WRITE (*,'(1X,A)')
     1'Modification: parameter=value (type "ok" to escape)';READ (*,'(2A)') PUF,PUFIX;K=0;DO 230 I=1,kpar; IF (CH(I).EQ.PUF) K=I
230   continue;IF (K.EQ.0) THEN;WRITE (*,'(1X,A,/)') 'UNKNOWN IDENTIFIER';GOTO 220;ENDIF;IF (K.EQ.kpar) GOTO 215;LN=20;DO 301 I=20,1
     l,-1;IF (PUFIXX(I).EQ.sp) THEN;LN=LN-1;ELSE;GOTO 302;ENDIF
301   CONTINUE
302   IF (LN.GT.9) THEN;WRITE (*,'(1X,A,/)') 'Integer too long';GOTO 220;ELSEif (ln.lt.1) then;WRITE (*,'(1X,A,/)') 'This is not a n
     lumber';GOTO 220;else;WRITE (FORM(3),'(I1)') LN;ENDIF;READ (PUFIX,FORMA,ERR=900) K1;select case(k);case(1);goto 10; case(2);got
     lo 20; case(3);goto 30; case(4);goto 40; case(5);goto 50; case(6);goto 60; case(7);goto 70;end select
10    if (k1.eq.1.or.k1.eq.2) then;msp=k1;goto 211;else;write (*,100) 'msp must be 1 or 2';goto 220;endif
20    if (k1.gt.0.and.k1.le.n1) then;nsc=k1;if (ksec.gt.nsc) ksec=nsc;goto 211;else;write (*,110) 'nsc must be 1 -',n1;goto 220;endi
     lf
30    if (k1.gt.0.and.k1.le.nsc) then;ksec=k1;goto 211;else;write (*,110) 'ksc must be 1 -',nsc;goto 220;endif
40    if (k1.gt.0.and.k1.le.n1) then;kvec=k1;goto 211;else;write (*,110) 'kvc must be 1 -',n1;goto 220;endif
50    if (k1.ge.0.and.k1.le.1) then;iapr=k1;goto 211;else;write (*,100) 'aut must be 0 or 1';goto 220;endif
60    if (k1.ge.0.and.k1.le.3) then;np=k1;goto 211;else;write (*,100) 'npa must be 0-3';goto 220;endif
70    if (k1.eq.0.or.k1.eq.1) then;ideg=k1;goto 211;else;write (*,100) 'deg must be 0 or 1';goto 220;endif
900   WRITE (*,100) 'Data type error';GOTO 220
211   GOTO 220
100   format (1x,a,/)
110   format (1x,a,i5,/)
300   CALL SPPREP
999   RETURN;END;SUBROUTINE SPARD
      INCLUDE 'mifdp.inc'
      INCLUDE 'mifcomx.inc'
      IF (IAPR.EQ.1) THEN;WRITE(*,'(/,1X,A)')
     1'SECTIONAL AND CYCLIC PRICING ARE AUTOMATIC';GOTO 999;ENDIF;WRITE (*,'(/,1X,A)') 'PARAMETERS OF SECTIONAL PRICING';WRITE (*,10
     l0) 'nsc=',nsc,'  (1-n)......number of sectors';WRITE (*,100) 'ksc=',ksec,
     1'  (1-nsc)....number of sectors to be scanned per iteration';WRITE (*,100) 'kvc=',kvec,
     1'  (1-n)......number of candidates per sector'
100   FORMAT (1x,a,i4,a)
999   RETURN;END;SUBROUTINE SPARI
      INCLUDE 'mifdp.inc'
      INCLUDE 'mifcomx.inc'
      nsc=1;ksec=1;kvec=n1;END;SUBROUTINE SPPREP
      INCLUDE 'mifdp.inc'
      INCLUDE 'mifcomx.inc'
      REAL PERC1,SIZE;integer(4) i;IF (NSC.GT.N1) NSC=N1;IF (NSC.GT.50) NSC=50;PERC1=100.0/NSC;SIZE=PERC1*n1/100.0;KSK(1)=1;DO 10 I=
     l1,NSC-1;KSV(I)=idint(I*SIZE+0.5);KSK(I+1)=KSV(I)+1;KSP(I)=KSV(I)
10    CONTINUE;KSV(NSC)=N1;KSP(NSC)=N1;NSP=NSC;IF (KTRA.GT.0) THEN;WRITE (*,'(1X,A)') 'Sector boundaries:';DO 20 I=1,NSC;WRITE (*,'(
     l1X,3(A,I5))') 'I=',I,'  KSK=',KSK(I),'  KSV=',KSV(I)
20    CONTINUE;ENDIF;RETURN;
      END
