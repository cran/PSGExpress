      subroutine rdum (kb1,mark,xs,rng)
      include 'mifdp.inc'
      include 'mifcomx.inc'
      integer(4) INPK,INPP,LOG_PR,ISTOP,IDB;character workpath*256;common /contr/ inpk,inpp,log_pr,istop,iDB, workpath;integer kb1(m
     l1),mark;double precision xs,rng;dimension mark(n+1),xs(n),rng(n);character dfil*40;character(5) fa;dimension ic(10);integer i,
     lic,icij,ii,ij,js,k,kf,kk;intrinsic mod;do 10 i=1,m1; kb1(i)=n1+i
10    continue
900   write (*,'(1x,a)') 'Basis file: '; read (*,'(a)') dfil;open(kr,file=trim(workpath)//dfil,status='old',iostat=js);if (js.ne.0) 
     lthen;write(*,'(1x,a)') 'File does not exist, give new name!';goto 900;endif;read (kr,'(a)');read (kr,'(i6)') iduc;write(*,'(1x
     l,a,i5)') 'Start from dump #',iduc;read (kr,'(a,i7)') fa,k;i=0;do 20 ii=1,k,10;kf=10;if (ii+9.gt.k) kf=mod(k,10);read (kr,*) (i
     lc(ij),ij=1,kf);do 30 ij=1,kf;kk=0;icij = ic(ij);if (icij.gt.0.and.icij.le.n1) then;kk=icij;else;if (icij.lt.0.and.icij.ge.-m1)
     l then;if (-icij.lt.objind) then;kk=n1-icij;elseif (-icij.gt.objind) then;kk=n1-icij-1;else;kk=n1+m1;endif;endif;endif;if (kk.n
     le.0.and.kk.ne.n1+m1.and.i.lt.m) then;i=i+1;kb1(i)=kk;endif
30    continue
20    continue;read (kr,*) k;do 40 ii=1,k,10;kf=10;if (ii+9.gt.k) kf=mod(k,10);read (kr,*) (ic(ij),ij=1,kf);do 50 ij=1,kf;kk=0;icij=
     lic(ij);if (icij.gt.0.and.icij.le.n1) then;kk=icij;else;if (icij.lt.0.and.icij.ge.-m1) then;if (icij.gt.-objind) then;kk=n1-ici
     lj;elseif (icij.lt.-objind) then;kk=n1-icij-1;else;kk=n1+m1;endif;endif;endif;if (kk.ne.0.and.kk.lt.n1+m1) then;if (mark(kk).eq
     l.1) then;mark(kk)=9;xs(kk)=rng(kk);endif;endif
50    continue
40    continue;close(kr);end;subroutine solf (aij,inda,jas,kb1,kb3,mr,at,jats,chs,
     1mrow,ak,ncol,rname,cname,relk,
     2mark,kac,colend,rng,xs,objrow)
      INCLUDE 'mifdp.inc'
      INCLUDE 'mifcomx.inc'
      integer(4) INPK,INPP,LOG_PR,ISTOP,IDB;character workpath*256;common /contr/ inpk,inpp,log_pr,istop,iDB, workpath;real(8), poin
     lter::prnsc(:),pcnsc(:),prng(:),paij(:); logical lpalloc;common/pForScale/prnsc,pcnsc,prng,paij,lpalloc;integer inda,jas,kb1,kb
     l3,mr,jats,mrow,ncol,mark,kac,colend;double precision aij,at,ak,rng,xs,objrow;dimension aij(nzr),inda(nzr),kb1(mr),kb3(mr),at(j
     lats);dimension ak(ncol),xs(mrow+ncol+1),objrow(ncol);character fn*40,ch,ch1,chs,cn*8,rn*8;character sor1*55,sor2*24,sor3*46,so
     lr4*24;character pname*40,dfil*40,relk*1;character(8) objname,rhsname,rngname,bndname,rname,cname;common /names1/  pname,dfil,o
     lbjname,rhsname,rngname,bndname;dimension rname(mrow),cname(ncol),relk(mrow);dimension mark(mrow+ncol),kac(ncol+1),colend(ncol)
     l,rng(mrow+ncol);integer i,ii,isol,j,js,k,ka,kf,k1;double precision a,r,w,lbig;intrinsic mod;i=jas;lbig = 0.99 * big;if(inpk<=0
     l) goto 1000
900   WRITE(*,'(1X,A)') 'OUTPUT FILE (quit=*): '; read (*,'(a)') fn;if (fn(1:1).eq.'*') return;open (kr,file=fn,status='new',iostat=
     ljs);if (js.ne.0) then;write(*,'(1x,a)') 'File already exists, give new name!';goto 900;endif
1000  continue;iresul=iresul+1;if (iscale.gt.0) then;if(lpalloc)then; at(m+1:m+m)=prnsc; rng(1:n1)=pcnsc;else;open(33,file=trim(work
     lpath)//'milprnsc.tmp',form='unformatted',status='unknown');read (33) (at(i),i=m+1,m+m);close (33);open(33,file=trim(workpath)/
     l/'milpcnsc.tmp',form='unformatted',status='unknown');read (33) (rng(j),j=1,n1);close (33);endif;do 200 i=1,m; xs(n1+i)=xs(n1+i
     l)/at(m+i)
200   continue;do 210 j=1,n1; xs(j)=xs(j)*rng(j)
210   continue;r=1.0;do 220 i=1,m;at(i)=at(i)*at(m+i)/r
220   continue;if(lpalloc)then;  rng(1:n)=prng; aij(1:kac(n1+1)-1)=paij;else;open (33,file=trim(workpath)//'milprng.tmp',form='unfor
     lmatted',status='unknown');READ (33) (RNG(J),J=1,N);CLOSE (33);open (33,file=trim(workpath)//'milpaij.tmp',form='unformatted',s
     ltatus='unknown');READ (33) (AIJ(I),I=1,KAC(N1+1)-1);CLOSE (33);endif;ENDIF;if(inpk<=0) RETURN;open (33,file=trim(workpath)//'m
     lilprhs.tmp',form='unformatted',status='unknown');READ (33) (AT(I),I=M+1,M+M);CLOSE (33);open (33,file=trim(workpath)//'milplb.
     ltmp',form='unformatted',status='unknown');READ (33) (AK(J),J=1,N1);CLOSE (33);CN='        ';RN='        ';IF (NINF.EQ.0.AND.IN
     lOP1.EQ.0) ISOL=1;IF (NINF.EQ.0.AND.INOP1.GT.0) THEN;IF (IPOSI.GT.2.AND.IPOSI.LT.6) THEN;ISOL=6;ELSE;ISOL=4;ENDIF;ENDIF;IF (NIN
     lF.GT.0.AND.INOP1.GT.0) THEN;IF (IPOSI.GT.2.AND.IPOSI.LT.6) THEN;ISOL=7;ELSE;ISOL=5;ENDIF;ENDIF;IF (NINF.GT.0.AND.INOP1.EQ.0) I
     lSOL=3;IF (IPOSI.EQ.2) ISOL=2;if(inpk<=0) goto 1010;WRITE (KR,'(A,I3)') 'SOLUTION STATUS:',ISOL;WRITE (KR,'(3A,2X,G20.12)')
     1'Objective function [',RNAME(MC),']: ',-XS(KB1(MC));WRITE (KR,'(I5,A)') NINF,' Infeasible variable(s)'
1010  continue;DO 30 J=1,N1;II=J;DJ=0.0;KA=KAC(II);KF=Colend(II);A=0.0;DO 40 I=KA,KF;K=INDA(I);IF (K.EQ.MC) A=AIJ(I);DJ=AT(K)*AIJ(I)
     l+DJ
40    continue;a = objrow(ii);dj = a + dj;W=AK(II);IF (MARK(J).EQ.3.OR.MARK(J).EQ.7) W=0.0;CH=' ';IF (MARK(J).EQ.9) THEN;CH='U';W=AK
     l(II)+RNG(J);ENDIF;k=0;do 100 k1=1,m;if (kb1(k1).ne.j) goto 100;k=k1;goto 110
100   continue
110   IF (K.GT.0) THEN;W=W+XS(KB1(K));CH='B';IF (KB3(K).EQ.1) CH='M';IF (KB3(K).EQ.2) CH='P';ENDIF;IF (CHS.EQ.'1') CN=CNAME(II);IF (
     lMOD(II,18).EQ.1) CALL TOPF(1);WRITE (SOR1,'(I5,2X,A8,2X,A,2F12.3,1X,F12.3)')
     1II,CN,CH,W,A,AK(II);IF (RNG(J).LT.lbig.AND.RNG(J).GT.-lbig) THEN;WRITE (SOR2,'(2X,F10.3,F12.3)') AK(II)+RNG(J),DJ;ELSE;WRITE (
     lSOR2,'(4X,A,F12.3)') 'INFINITE',DJ;ENDIF;if(inpk>0) WRITE (KR,'(2A)') SOR1,SOR2
30    CONTINUE;DO 50 I=1,M;DJ=AT(I);CH=' ';IF (CHS.EQ.'1') RN=RNAME(I);IF (MARK(I).EQ.9) CH='U';W=XS(n1+I);k=0;do 120 k1=1,m;if (kb1
     l(k1).ne.n1+i) goto 120;k=k1;goto 130
120   continue
130   IF (K.GT.0) THEN;W=XS(KB1(K));CH='B';IF(KB3(K).EQ.1) CH='M';IF(KB3(K).EQ.2) CH='P';ENDIF;IF (MOD(I,18).EQ.1) CALL TOPF(2);CH1=
     lRELK(I);IF (I.EQ.MC) CH1='Z';WRITE (SOR3,'(I5,2X,A8,2X,A,F12.3,3X,A,F12.3)')
     1-I,RN,CH,W,CH1,AT(M+I);IF (RNG(n1+I).LT.lbig) THEN;WRITE (SOR4,'(2X,F10.3,F12.3)') RNG(n1+I),DJ;ELSE;WRITE (SOR4,'(4X,A,F12.3)
     l') 'INFINITE',DJ;ENDIF;if(inpk>0) WRITE (KR,'(2A)') SOR3,SOR4
50    CONTINUE;CLOSE(KR);END;SUBROUTINE TOPF (IK)
      INCLUDE 'mifdp.inc'
      INCLUDE 'mifcomx.inc'
      integer ik;CHARACTER PNAME*40,DFIL*40;CHARACTER SOR1*31,SOR2*48,SOR3*39;character(8) OBJNAME,RHSNAME,RNGNAME,BNDNAME;COMMON /N
     lAMES1/  PNAME,DFIL,OBJNAME,RHSNAME,RNGNAME,BNDNAME;WRITE (KR,'(2A)') 'PROBLEM: ',PNAME;SOR1='INDEX    NAME   STAT     VALUE ';
      SOR2='  OBJ.COEFF.    LW.BOUND    UP.BOUND    SH.PRICE';SOR3=' REL     R.H.S.       RANGE    SH.PRICE';IF (IK.EQ.1) THEN;WRITE
     l(KR,'(2A,/)') SOR1,SOR2;ELSE;WRITE (KR,'(2A,/)') SOR1,SOR3;ENDIF;
      END
