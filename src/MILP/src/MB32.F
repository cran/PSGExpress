      subroutine invertx (n,m,nffr,free,pivotn,pntl,pntu,countl,countu,bhead,crow,ccol,cpermf,rpermf,pivrows,begc,nextc,mark,workr,p
     nivots,pivcols,inda0,aij0,facind,factor,colbeg,colendx,tpiv1,tpiv2,triang,rendmem,rindex,rpnt,cfill,rfill,begr,nextr,prevr,ibs,
     nn1);integer(4) ISTOP,INPK,INPP,LOG_PR,IDB;integer(1) un; common/un/un;character(256) workpath;common /contr/ inpk,inpp,log_pr,
     fistop,iDB, workpath;integer(4) n,n1,m,pivotn,nffr,free,triang,rendmem,pntl(m),pntu(m),countl(m),countu(m),bhead(m),crow(m),cco
     ql(m),cpermf(m),rpermf(m),pivrows(m),begc(m),nextc(m),mark(m),pivcols(m),facind(nffr:free),colbeg(n),colendx(n),rindex(rendmem)
     q,rpnt(m),      inda0(nffr-1),cfill(m),rfill(m),begr(m),nextr(m),prevr(m),ibs(3);double precision workr(m),pivots(m),factor(nff
     qr:free),tpiv1,tpiv2,    aij0(nffr-1);integer(4) i,j,k,l,o,pnt1,pnt2,cfirst,clast,freemem,gto,prow,pcol,endmem,pnt,nadr,prevcol
     x,markowitz,ppnt1,ppnt2,pntx,pnty,ii,fill,maxfill,pcnt,rfirst,rlast,rfreemem,rpnt1,rpnt2,kk,rcnt,singinv,singinv2,singular;doub
     xle precision s,pivot;intrinsic dabs
   1  FORMAT(' PIVOT REJECTED : ',D11.5)
   6  FORMAT(' NOT ENOUGH MEMORY TO TRANSPOSE U')
   8  FORMAT(' NOT ENOUGH MEMORY IN THE COLUMN FILE ');singinv=0
98765 markowitz=0;singinv2=0;endmem=free;pivotn=0;nadr=colbeg(n);cfirst=0;rfirst=0;clast =0;rlast =0;do 11 i=1,m;pivcols(i)=0;pivrow
     qs(i)=0;ccol(i)=0;crow(i)=0;begc(i)=0;nextc(i)=0;workr(i)=0.0;mark(i)=0;pivots(i)=1.0;countu(i)=0
  11  continue;do 15 i=1,m;j=bhead(i);if (j.gt.n1) then;j=j-n1;ccol(j)=ccol(j)+1;else;do 12 l=colbeg(j),colendx(j);ccol(inda0(l))=cc
     nol(inda0(l))+1
  12  continue;endif
  15  continue;pnt=nadr;do 16 i=1,m;pntu(i)=pnt;rfill(i)=pnt;pnt=pnt+ccol(i);if (cfirst.eq.0) then;cfirst=i;rfirst=i;else;cpermf(cla
     kst)=i;rpermf(rlast)=i;endif;pntl  (i)=clast;countl(i)=rlast;clast=i;rlast=i;if (pnt+m.ge.free) then;if(un>=0)write(un,8);istop
     q=321; RETURN;endif
  16  continue;cpermf(clast)=0;rpermf(rlast)=0;j=(pntu(m)-pntu(1)+ccol(m));ibs(1) = j;if (free.le.(pntu(m)+ccol(m)+j)) then;if(un>=0
     r)write(un,6);istop=322; RETURN;endif;do 17 i=1,m;j=bhead(i);if (j.gt.n1) then;j=j-n1;factor(rfill(j))=1.0;facind(rfill(j))=i;r
     lfill(j)=rfill(j)+1;else;do 13 l=colbeg(j),colendx(j);k=inda0(l);facind(rfill(k))=i;factor(rfill(k))=aij0(l);rfill(k)=rfill(k)+
     i1
  13  continue;endif
  17  continue;do 25 i=1,m;pnt1=pntu(i);pnt2=pnt1+ccol(i)-1;do 21 j=pnt1,pnt2;crow(facind(j))=crow(facind(j))+1
  21  continue
  25  continue;do 55 i=1,m;begr(i)=0;prevr(i)=0
  55  continue;i=rfirst
  56  if(i.ne.0)then;j=crow(i);if(j.gt.0)then;o=begr(j);nextr(i)=o;begr(j)=i;if(o.ne.0)prevr(o)=i;endif;prevr(i)=0;i=rpermf(i);goto 
     c56;endif
  40  i=begr(1)
  41  if (i.ne.0) then;j=bhead(i);if (j.gt.n1) then;j=j-n1;pcol=j;pivot=1.0;else;do 45 l=colbeg(j),colendx(j);if (countu(inda0(l)).e
     rq.0) goto 46
  45  continue;if(un>=0)write(un,*)'error';istop=328; RETURN
  46  pivot=aij0(l); pcol=inda0(l);if (dabs(pivot).lt.tpiv1) then;if(un>=0)write(un,1)pivot;i=nextr(i);goto 41;endif;endif;prow=i;co
     ountu(pcol)=1;pivotn=pivotn+1;pivcols(pivotn)=prow;pivrows(pivotn)=pcol;pivots(pivotn)=pivot;prevcol=pntl(pcol);o=cpermf(pcol);
      if (prevcol.ne.0) then;cpermf(prevcol)=o;else;cfirst=o;endif;if (o.eq.0) then;clast=prevcol;else;pntl(o)=prevcol;endif;prevcol
     n=countl(prow);o=rpermf(prow);if (prevcol.ne.0) then;rpermf(prevcol)=o;else;rfirst=o;endif;if (o.eq.0) then;rlast=prevcol;else;
      countl(o)=prevcol;endif;o=nextr(prow);ii=prevr(prow);if (ii.eq.0) then;begr(1)=o;else;nextr(ii)=o;endif;if (o.ne.0) prevr(o)=i
     zi;pnt1=pntu(pcol);pnt2=pnt1+ccol(pcol)-1;pntx=endmem-ccol(pcol)+1;endmem=pntx;do 48 j=pnt1,pnt2;if (facind(j).eq.prow) goto 48
       o=facind(j);facind(pntx)=o;factor(pntx)=factor(j)/pivot;pntx=pntx+1;kk=crow(o);crow(o)=crow(o)-1;k=nextr(o);ii=prevr(o);if(ii
     y.eq.0)then;begr(kk)=k;else;nextr(ii)=k;endif;if (k.ne.0) prevr(k)=ii;kk=kk-1;if(kk.gt.0)then;k=begr(kk);nextr(o)=k;begr(kk)=o;
      if (k.ne.0) prevr(k)=o;endif;prevr(o)=0
  48  continue;crow(prow)=crow(prow)-1;ccol(pcol)=ccol(pcol)-1;goto 40;endif;if (pivotn.eq.m) then;ibs(2) = m;triang = m;goto 999;en
     hdif;pnt=1;i=rfirst
  20  if (i.ne.0) then;rpnt(i)=pnt;rfill(i)=pnt;pnt=pnt+crow(i);i=rpermf(i);goto 20;endif;rfreemem=rendmem-pnt;if(rfreemem.lt.2*m)th
     den;if(un>=0)write(un,8);istop=323; RETURN;endif;i=cfirst
  26  if(i.ne.0)then;pnt1=pntu(i);pnt2=pnt1+ccol(i)-1;do 27 j=pnt1,pnt2;k=facind(j);rindex(rfill(k))=i;rfill(k)=rfill(k)+1
  27  continue;i=cpermf(i);goto 26;endif;do 51 i=1,m;begc(i)=0;countu(i)=0
  51  continue;i=cfirst
  52  if(i.ne.0)then;j=ccol(i);if(j.gt.0)then;o=begc(j);nextc(i)=o;begc(j)=i;if(o.ne.0)countu(o)=i;endif;countu(i)=0;i=cpermf(i);got
     no 52;endif;freemem=free-pntu(clast)-ccol(clast)-1
  30  pcol=begc(1)
  31  if (pcol.ne.0)then;pnt1=pntu(pcol);prow=facind(pnt1);pivot=factor(pnt1);if(dabs(pivot).lt.tpiv1)then;if(un>=0)write(un,1)pivot
       pcol=nextc(pcol);goto 31;endif;gto=-1;goto 400;endif;triang=pivotn;ibs(2) = triang
  50  if (pivotn.eq.m) goto 999;call findpivot(m,nffr,free,crow,facind,tpiv1,tpiv2,pcol,prow,pivot,pntu,ccol,markowitz,begc,nextc,fa
     octor,begr,nextr,rpnt,rendmem,rindex);if (pcol.eq.0) goto 900;gto=1
 400  IF (FREEMEM.LT.2*M)CALL CCOMPRESS(M,nffr,FREE,FREEMEM,ENDMEM,NADR,PNTU,CCOL,CFIRST,CPERMF,FACIND,FACTOR);PCNT=CCOL(PCOL);RCNT=
     tCROW(PROW);PNTX=ENDMEM-PCNT+1;PNT=PNTX;PNTY=PNTX-RCNT+1;ENDMEM=PNTY;PPNT1=PNTU(PCOL);PPNT2=PPNT1+PCNT-2;FREEMEM=PNTY-CCOL(CLAS
     tT)-PNTU(CLAST)-1;PREVCOL=PNTL(PCOL);O=CPERMF(PCOL);IF(PREVCOL.NE.0)THEN;CPERMF(PREVCOL)=O;ELSE;CFIRST=O;ENDIF;IF(O.EQ.0)THEN;C
     eLAST=PREVCOL;ELSE;PNTL(O)=PREVCOL;ENDIF;PREVCOL=COUNTL(PROW);O=RPERMF(PROW);IF(PREVCOL.NE.0)THEN;RPERMF(PREVCOL)=O;ELSE;RFIRST
     h=O;ENDIF;IF(O.EQ.0)THEN;RLAST=PREVCOL;ELSE;COUNTL(O)=PREVCOL;ENDIF;O=NEXTR(PROW);II=PREVR(PROW);IF(II.EQ.0)THEN;BEGR(RCNT)=O;E
     bLSE;NEXTR(II)=O;ENDIF;IF(O.NE.0)PREVR(O)=II;O=NEXTC(PCOL);II=COUNTU(PCOL);IF(II.EQ.0)THEN;BEGC(PCNT)=O;ELSE;NEXTC(II)=O;ENDIF;
      IF(O.NE.0)COUNTU(O)=II;MAXFILL=PCNT-1;IF(MARKOWITZ.EQ.0)THEN;DO 410 J=PPNT1,PPNT2;O=FACIND(J);FACIND(PNTX)=O;FACTOR(PNTX)=FACT
     dOR(J)/PIVOT;PNTX=PNTX+1;KK=CROW(O)-1;CROW(O)=KK;PNT1=RPNT(O);PNT2=PNT1+KK-1;DO 405 K=PNT1,PNT2;IF(RINDEX(K).EQ.PCOL)THEN;RINDE
     dX(K)=RINDEX(PNT2+1);GOTO 418;ENDIF
 405  CONTINUE
 418  K=NEXTR(O);II=PREVR(O);IF(II.EQ.0)THEN;BEGR(KK+1)=K;ELSE;NEXTR(II)=K;ENDIF;IF(K.NE.0)PREVR(K)=II;IF(KK.GT.0)THEN;K=BEGR(KK);NE
     eXTR(O)=K;BEGR(KK)=O;IF(K.NE.0)PREVR(K)=O;ENDIF;PREVR(O)=0
 410  CONTINUE;PPNT1=RPNT(PROW);PPNT2=PPNT1+RCNT-2;DO 407 K=PPNT1,PPNT2;IF(RINDEX(K).EQ.PCOL)THEN;RINDEX(K)=RINDEX(PPNT2+1);GOTO 408
       ENDIF
 407  CONTINUE
 408  DO 413 K=PPNT1,PPNT2;I=RINDEX(K);PCNT=CCOL(I);PNT1=PNTU(I);PNT2=PNT1+PCNT-1;DO 412 J=PNT1,PNT2;IF(FACIND(J).EQ.PROW)THEN;CCOL(
     wI)=PCNT-1;FACIND(PNTY)=I;FACTOR(PNTY)=FACTOR(J);PNTY=PNTY+1;FACIND(J)=FACIND(PNT2);FACTOR(J)=FACTOR(PNT2);O=NEXTC(I);II=COUNTU
     t(I);IF(II.EQ.0)THEN;BEGC(PCNT)=O;ELSE;NEXTC(II)=O;ENDIF;IF(O.NE.0)COUNTU(O)=II;PCNT=PCNT-1;IF(PCNT.GT.0)THEN;O=BEGC(PCNT);NEXT
     gC(I)=O;BEGC(PCNT)=I;IF(O.NE.0)COUNTU(O)=I;ENDIF;COUNTU(I)=0;GOTO 413;ENDIF
 412  CONTINUE
 413  CONTINUE;ELSE;DO 420 J=PPNT1,PPNT2;O=FACIND(J);FACIND(PNTX)=O;FACTOR(PNTX)=FACTOR(J)/PIVOT;WORKR(O)=FACTOR(PNTX);MARK(O)=1;PNT
     uX=PNTX+1;rfill(o)=-1;pnt1=rpnt(o);pnt2=pnt1+crow(o)-2;do 417 k=pnt1,pnt2;if(rindex(k).eq.pcol)then;rindex(k)=rindex(pnt2+1);go
     mto 420;endif
 417  continue
 420  continue;ppnt1=pnt;ppnt2=pntx-1;rpnt1=rpnt(prow);rpnt2=rpnt1+rcnt-2;do 427 k=rpnt1,rpnt2;if(rindex(k).eq.pcol)then;rindex(k)=r
     sindex(rpnt2+1);goto 428;endif
 427  continue
 428  do 423 kk=rpnt1,rpnt2;i=rindex(kk);pcnt=ccol(i);cfill(i)=pcnt-1;pnt1=pntu(i);pnt2=pnt1+pcnt-1;do 422 j=pnt1,pnt2;if(facind(j).
     oeq.prow)then;s=factor(j);facind(pnty)=i;factor(pnty)=factor(j);pnty=pnty+1;facind(j)=facind(pnt2);factor(j)=factor(pnt2);o=nex
     htc(i);k=countu(i);if(k.eq.0)then;begc(ccol(i))=o;else;nextc(k)=o;endif;if(o.ne.0)countu(o)=k;fill=maxfill;pnt2=pnt2-1;do 424 k
     m=pnt1,pnt2;o=facind(k);if(mark(o).ne.0)then;factor(k)=factor(k)-s*workr(o);fill=fill-1;mark(o)=0;endif
 424  continue;ii=cpermf(i);if(ii.eq.0)then;k=endmem-pnt2-1;else;k=pntu(ii)-pnt2-1;endif;if(fill.gt.k)then;if (freemem.lt.m)then;cal
     dl ccompress(m,nffr,free,freemem,endmem,nadr,pntu,ccol,cfirst,cpermf,facind,factor);pnt1=pntu(i);pnt2=pnt1+pcnt-2;if(istop>0) R
     iETURN;endif;if(i.ne.clast)then;l=pntu(clast)+ccol(clast);pntu(i)=l;do 430 k=pnt1,pnt2;facind(l)=facind(k);factor(l)=factor(k);
      l=l+1
 430  continue;pnt1=pntu(i);pnt2=l-1;prevcol=pntl(i);if(prevcol.eq.0)then;cfirst=ii;else;cpermf(prevcol)=ii;endif;pntl(ii)=prevcol;c
     lpermf(clast)=i;pntl(i)=clast;clast=i;cpermf(clast)=0;endif;endif;do 425 k=ppnt1,ppnt2;o=facind(k);if(mark(o).eq.0)then;mark(o)
     z=1;else;pnt2=pnt2+1;factor(pnt2)=-s*factor(k);facind(pnt2)=o;rfill(o)=rfill(o)+1;endif
 425  continue;pnt2=pnt2+1;ccol(i)=pnt2-pnt1;if(i.eq.clast)then;freemem=endmem-pnt2-1;endif;if(pnt2.gt.pnt1)then;o=begc(ccol(i));nex
     stc(i)=o;begc(ccol(i))=i;if(o.ne.0)countu(o)=i;endif;countu(i)=0;goto 423;endif
 422  continue
 423  continue;do 429 j=ppnt1,ppnt2;o=facind(j);mark(o)=0;pnt2=rpnt(o)+crow(o)-1;ii=rpermf(o);if(ii.eq.0)then;k=rendmem-pnt2-1;else;
      k=rpnt(ii)-pnt2-1;endif;if(k.lt.rfill(o))then;if(rfreemem.lt.m)call rcompress(m,rfirst,rpermf,rindex,rendmem,rfreemem,rpnt,cro
     gw);if(istop>0) RETURN;if(ii.ne.0)then;pnt1=rpnt(o);pnt2=pnt1+crow(o)-2;pnt=rpnt(rlast)+crow(rlast);rpnt(o)=pnt;do 445 l=pnt1,p
     unt2;rindex(pnt)=rindex(l);pnt=pnt+1
 445  continue;prevcol=countl(o);if(prevcol.eq.0)then;rfirst=ii;else;rpermf(prevcol)=ii;endif;countl(ii)=prevcol;rpermf(rlast)=o;cou
     ontl(o)=rlast;rlast=o;rpermf(rlast)=0;endif;endif;kk=crow(o);k=nextr(o);ii=prevr(o);if(ii.eq.0)then;begr(kk)=k;else;nextr(ii)=k
       endif;if(k.ne.0)prevr(k)=ii;kk=kk+rfill(o);crow(o)=kk;if(kk.gt.0)then;k=begr(kk);nextr(o)=k;begr(kk)=o;if(k.ne.0)prevr(k)=o;e
     dndif;prevr(o)=0;if(o.eq.rlast)rfreemem=rendmem-crow(o)-rpnt(o)
 429  continue;do 440 j=ppnt1,ppnt2;rfill(facind(j))=rpnt(facind(j))+crow(facind(j))-1
 440  continue;rpnt1=endmem;rpnt2=rpnt1+rcnt-2;do 450 j=rpnt1,rpnt2;o=facind(j);pnt1=pntu(o)+cfill(o);pnt2=pntu(o)+ccol(o)-1;do 460 
     ok=pnt1,pnt2;rindex(rfill(facind(k)))=o;rfill(facind(k))=rfill(facind(k))-1
 460  continue
 450  continue;endif;pivotn=pivotn+1;pivcols(pivotn)=prow;pivrows(pivotn)=pcol;pivots(pivotn)=pivot;crow(prow)=rcnt-1;ccol(pcol)=max
     mfill;if(gto<=0)then; goto 30; else; goto 50; endif
 900  l=cfirst;j=pivotn+1;i=rfirst;singinv = singinv + 1;singinv2 = singinv2 + 1;singular = 0
 940  if (i.gt.0) then;pivrows(j)=l;pivcols(j)=i;bhead(i)=l;singular = singular + 1;ccol(l)=0;crow(i)=0;j=j+1;l=cpermf(l);i=rpermf(i
     j);goto  940;endif
 999  pnt=free;do 1020 i=1,m;countl(i)=crow(pivcols(i));pntl(i)=pnt-countl(i)-ccol(pivrows(i));pnt=pntl(i);begc(i)=bhead(pivcols(i))
      
1020  continue;do 1030 i=1,m;bhead(pivrows(i))=begc(i)
1030  continue;if (singinv2.gt.0.and.singinv.gt.0) then;if (singinv.eq.1) then;if(un>=0)write(un,'(a,i6,a)')'reinvert to repair ',si
     ungular,' singularities';goto 98765;else;if(un>=0)write(un,*) 'unrepairable singularity in invert, stop';istop=324; RETURN;endi
     wf;endif;do 1100 i=1,m;countu(i)=0;nextc(pivcols(i))=i
1100  continue;do 1110 i=1,m;pnt1=pntl(i)+countl(i);pnt2=pnt1+ccol(pivrows(i))-1;do 1120 j=pnt1,pnt2;o=nextc(facind(j));countu(o)=co
     puntu(o)+1
1120  continue
1110  continue;pntu(1)=nadr;do 1150 i=1,m-1;pntu(i+1)=pntu(i)+countu(i);begc(i)=pntu(i)
1150  continue;begc(m)=pntu(m);if(pntu(m)+countu(m).ge.pntl(m))then;if(un>=0)write(un,6);istop=325; RETURN;endif;do 1130 i=1,m;ii=pi
     tvrows(i);pnt1=pntl(i)+countl(i);pnt2=pnt1+ccol(ii)-1;do 1140 j=pnt1,pnt2;o=nextc(facind(j));k=begc(o);facind(k)=ii;factor(k)=f
     bactor(j);begc(o)=k+1
1140  continue
1130  continue;do 1145 i=pivotn+1,m;countu(i)=0
1145  continue;pnt=free;do 1050 i=1,m;pnt1=pntl(i);pnt2=pnt1+countl(i)-1;do 1060 j=pnt2,pnt1,-1;pnt=pnt-1;facind(pnt)=facind(j);fact
     nor(pnt)=factor(j)
1060  continue;pntl(i)=pnt
1050  continue;return;end subroutine invertx;subroutine ccompress(m,nffr,free,freemem,endmem,nadr,pnt,count,cfirst,cpermf,facind,fac
     vtor);integer(4) m,nffr,free,freemem,endmem,pnt(m),facind(nffr:free),count(m),cpermf(m),cfirst,nadr;double precision factor(nff
     ur:free);integer(4) i,j,pnt1,pnt2,pnt0;integer(4) ISTOP,INPK,INPP,LOG_PR,IDB;integer(1) un; common/un/un;character(256) workpat
     nh;common /contr/ inpk,inpp,log_pr,istop,iDB, workpath
   1  FORMAT(' C O M P R E S S   C O L U M N   F I L E')
   2  FORMAT(' NOT ENOUGH MEMORY DETECTED IN SUBROUTINE CCOMPRESS');if(un>=0)write(un,1);pnt0=nadr;i=cfirst
  40  if(i.le.0)goto 30;pnt1=pnt(i);pnt(i)=pnt0;if(pnt1.eq.pnt0)then;pnt0=pnt0+count(i);goto 10;endif;pnt2=pnt1+count(i)-1;do 20 j=p
     tnt1,pnt2;facind(pnt0)=facind(j);factor(pnt0)=factor(j);pnt0=pnt0+1
  20  continue
  10  i=cpermf(i);goto 40
  30  freemem=endmem-pnt0-1;if(freemem.lt.2*m)then;if(un>=0)write(un,2);istop=326; RETURN;endif;return;end subroutine ccompress;subr
     foutine rcompress(m,first,perm,index,endmem,freemem,pnt,count);integer(4) m,first,endmem,freemem,perm(m),index(endmem),pnt(m),c
     count(m);integer(4) i,j,ppnt,pnt1,pnt2;integer(4) ISTOP,INPK,INPP,LOG_PR,IDB;integer(1) un; common/un/un;character(256) workpat
     vh;common /contr/ inpk,inpp,log_pr,istop,iDB, workpath
   1  FORMAT(' C O M P R E S S   R O W   F I L E')
   2  FORMAT(' NOT ENOUGH MEMORY DETECTED IN SUBROUTINE RCOMPRESS');if(un>=0)write(un,1);ppnt=1;i=first
   5  if(i.eq.0)goto 20;pnt1=pnt(i);if(ppnt.eq.pnt1)then;ppnt=ppnt+count(i);goto 15;endif;pnt2=pnt1+count(i)-1;pnt(i)=ppnt;do 10 j=p
     xnt1,pnt2;index(ppnt)=index(j);ppnt=ppnt+1
  10  continue
  15  i=perm(i);goto 5
  20  freemem=endmem-ppnt;if(freemem.lt.m)then;if(un>=0)write(un,2);istop=327; RETURN;endif;return;end subroutine rcompress;subrouti
     ene findpivot(m,nffr,free,crow,facind,tpiv1,tpiv2,pcol,prow,pivot,pntu,ccol,markowitz,begc,nextc,factor,begr,nextr,rpnt,rendmem
     d,rindex);integer(4) m,nffr,free,crow(m),facind(nffr:free),pcol,prow,pntu(m),markowitz,begc(m),nextc(m),begr(m),nextr(m),rpnt(m
     v),rendmem,rindex(rendmem),ccol(m);double precision tpiv1,tpiv2,factor(nffr:free),pivot;integer(4) i,j,pnt1,pnt2,pnt,cikl,nz,nn
     iz,col,row,gto,c,cc,pntnew;double precision ms,s,ss,snew;intrinsic dabs;markowitz=m*m+1;   ss=0.; snew=0.; pntnew=0; pnt=0;cikl
     n= 5;prow= 0;pcol= 0;nz  = 0
  10  nz  = nz+1;nnz=(nz-1)*(nz-1);row=begr(nz);gto=1
  30  if(row.ne.0)then;c=m+1;pnt1=rpnt(row);pnt2=pnt1+nz-1;do 40 j=pnt1,pnt2;if(ccol(rindex(j)).lt.c)then;col=rindex(j);c=ccol(col);
      endif
  40  continue;c=c-1;if(c.lt.m)goto 100;goto 160;endif;nnz=nnz+nz-1;col = begc(nz);gto=-1
  20  if(col.ne.0)then;c=nz-1;goto 100;endif;if(nz.lt.m)goto 10;goto 999
 100  pnt1=pntu(col);pnt2=pnt1+c;do 110 j=pnt1,pnt2;if((crow(facind(j))-1)*c.le.markowitz)goto 120
 110  continue;goto 160
 120  ms=0;do 125 j=pnt1,pnt2;s=dabs(factor(j));if(ms.lt.s)ms=s
 125  continue;if(ms.lt.tpiv1)goto 160;cc=m+1;ms=ms*tpiv2;do 130 j=pnt1,pnt2; i=cc-crow(facind(j));if(i<0)then; goto 130; elseif(i>0
     c)then; goto 150; else; goto 140; endif
 140  if(ss.lt.dabs(factor(j)))then;ss=dabs(factor(j));pntnew=j;endif;goto 130
 150  s=factor(j);if(dabs(s).gt.ms)then;ss=dabs(s);pntnew=j;cc=crow(facind(j));endif
 130  continue;if(cc.le.m)then;j=(cc-1)*c; i=markowitz-j;if(i<0)then; goto 160; elseif(i>0)then; goto 180; else; goto 170; endif
 170  if(snew.lt.ss/ms)then;prow=facind(pntnew);pcol=col;pivot=factor(pntnew);pnt=pntnew;snew=ss/ms;endif;goto 160
 180  markowitz=j;prow=facind(pntnew);pcol=col;pivot=factor(pntnew);pnt=pntnew;snew=ss/ms;endif
 160  cikl=cikl-1;if((cikl.le.0).and.(markowitz.le.m*m))goto 999;if (gto.lt.0)then;if(markowitz.le.nnz)goto 999;col=nextc(col);goto 
     e20;else;if (markowitz.le.nnz) goto 999;row=nextr(row);goto 30;endif
 999  if (pcol.gt.0) then;i=pntu(pcol)+ccol(pcol)-1;facind(pnt)=facind(i);factor(pnt)=factor(i);facind(i)=prow;factor(i)=pivot;endif
     c;return;
      end subroutine findpivot
