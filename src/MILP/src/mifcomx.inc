
C*                      'MIFCOMX.INC' include file of scalar commons
      integer         M1,M2,M3,M4,M5,M6,KR,LP,JJAS,MEMTOP
      COMMON /SIZES/  M1,M2,M3,M4,M5,M6,KR,LP,JJAS,MEMTOP
      integer         M,N,N1,MC,NN,MM,MA,NETA,INOP1,INOP2,NINF,
     X       KTRA,IVFR,IVPR,IVC,IVV,MAJR,MINR,IDUF,IDUPR,IDUC,MS,ITN,
     X       IT1,NEMX,JETAP,IPIRO,IPVTIP,MZ,NZ,NNZ,IVAN,KELLIN,IPOSI,
     X       KETPNT,IRANG,NADR,NFREE,IETA,INVTIME,INVTIM,IVCR,
     X       DEVEX,DEVFAIL,DEVREQ,DJSET,IALG,NSUB,NZR,OBJIND,   nffr    ! nffr - pervyj @lement posle matricy nffr=kac(n1+1)
      COMMON /PARAMI/ M,N,N1,MC,NN,MM,MA,NETA,INOP1,INOP2,NINF,
     X       KTRA,IVFR,IVPR,IVC,IVV,MAJR,MINR,IDUF,IDUPR,IDUC,MS,ITN,
     X       IT1,NEMX,JETAP,IPIRO,IPVTIP,MZ,NZ,NNZ,IVAN,KELLIN,IPOSI,
     X       KETPNT,IRANG,NADR,NFREE,IETA,INVTIME,INVTIM,IVCR,
     X       DEVEX,DEVFAIL,DEVREQ,DJSET,IALG,NSUB,NZR,OBJIND,   nffr
      double precision EPIV,EOPT,EREL,EABS,EZ,SINF,DJ,DIR,BIG,DEVRAT
     X                 ,objval
      COMMON /PARAMR/  EPIV,EOPT,EREL,EABS,EZ,SINF,DJ,DIR,BIG,DEVRAT
     X                 ,objval
      double precision WD1,WD2,WD3,BDA
      integer         IDYN,IDEG,IADC,IAPR
      COMMON /PARALG/ WD1,WD2,WD3,BDA,IDYN,IDEG,IADC,IAPR
      integer         KVEC,KSEC,NSC,NSP,KSK,KSV,KSP,ISC
      COMMON /SECTN/  KVEC,KSEC,NSC,NSP,KSK(50),KSV(50),KSP(50),ISC(6)
      double precision DEL,DELN,DELK,TAU,DK,TETA
      integer         IDGTYP,KMAX,KCIK
      COMMON /MADEG1/ DEL,DELN,DELK,TAU,DK,TETA,IDGTYP,KMAX,KCIK
      integer         ISCALE,ISTART,IRESUL
      COMMON /STATUS/ ISCALE,ISTART,IRESUL

!$OMP THREADPRIVATE(/sizes/,/parami/,/paramr/,/paralg/,/sectn/,/madeg1/,/status/)