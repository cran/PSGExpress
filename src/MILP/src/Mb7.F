      subroutine ident(jas,kod,mxrow,mxcol,mxnz);use IntelInterf
      include 'mifdp.inc'
      include 'mifcomx.inc'
      integer jas,kod,mxrow,mxcol,mxnz;character(64) fej1*58,fej2,fej3*60,fej11*79;character(20) id1,fn*16;character(1) id(20),fej31
     x(60),fejdat*19;integer(2) i1,j1,k1;integer isnr,j,j2;intrinsic char,ichar;equivalence (id1,id(1)),(fej3,fej31(1));j=mxnz;call 
     tgetdat(i1,j1,k1);FN='MILP.EXE';ISNR=1;J=1999;J2=12;ID1='S.Uryasev';ID(2)=CHAR(26);FEJ1='MILP linear programming V5.03 (c) Maro
     zs 1993-1997';FEJ3='Authorized user: ';IF (KOD.EQ.1) THEN;write (fej11,'(a,i5.5,i5.5,i7.7,i2.2,i1)')fej1,mxrow,mxcol,jas,isnr,n
     tsub;write (*,'(/,1x,a)') fej11;ELSEIF (KOD.EQ.2) THEN;WRITE (FEJ11,'(A,2I4.4,I5.5,I4.4,I1)')FEJ1,MZ,NZ,JAS,ISNR,NSUB;WRITE (KR
     b,'(/,A)') FEJ11;WRITE (KR,'(A)') FEJ2;WRITE (KR,'(2A,/)') FEJ3,FEJDAT;ELSEIF (KOD.EQ.3) THEN;ENDIF;
      END
