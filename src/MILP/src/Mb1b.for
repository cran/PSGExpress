      subroutine pide (kb1,kb2,kb3,mr,at,jats,xs,rng,jjpz,inn,t)
      include 'mifcomx.inc'
      integer(4) ISTOP,KKK,INPK,INPP,LOG_PR,IDB;integer(1) un; common/un/un;integer kb1,kb2,kb3,mr,jats,jjpz,inn,km,mj,ican;double p
     lrecision at,xs,rng,t,sj,sg,fks,din,deg,adc,sdjx;common /minit/  sj(6),sg(6),fks(6),din(6),deg(6),adc(6),sdjx(6),km(6),mj(6),ic
     lan(6);dimension kb1(mr),kb2(mr),kb3(mr),at(jats),xs(*),rng(*);dimension t(0:jjpz-1),inn(0:jjpz);integer i,j,jpz,k,l,jr,jp,ka,k
     lk,vinn,jnp;double precision a,eps,y,wt,ra,teta1,w,w1,rc,v;logical mehet;intrinsic abs,mod,max;character workpath*256;common /c
     lontr/ inpk,inpp,log_pr,istop,iDB, workpath;teta1=0;jpz=jjpz-1;a=-big;ipiro=-1;ms=-1;eps=epiv;mehet=.false.
10    if (ms.eq.0) mehet=.true.;ms=0;k=1
15    if (k.le.mm.and.ipiro.ne.0) then;if (sj(k).gt.-eopt.or.ican(k).ne.1) goto 20;l=(k-1)*m1;jr=0;jp=0;t(0)=-big;do 30 i=1,m;if (ab
     ls(at(l+i)).lt.eabs.or.kb2(i).eq.3) goto 30;y=at(l+i)*sg(k);j=0;if (y.lt.0.0) then;if (kb3(i).ne.2) then;if (kb2(i).lt.2) j=2;i
     lf (kb3(i).eq.1) j=j+1;endif;else;if (kb3(i).ne.1) then;j=1;if (kb3(i).eq.2) j=3;endif;endif;if (j.ne.0) then;if (mod(j,2).eq.1
     l) then;jp=jp+1;if (jp.gt.jpz) then;if(un>=0)write(un,*) 'Too many break points, buffer overflow';istop=101; RETURN;endif;INN(J
     lP)=I;t(jp)=(xs(kb1(i))+dk)/y;ENDIF;IF (J.GT.1) THEN;JP=JP+1;IF (JP.GT.JPZ) THEN;if(un>=0)write(un,*) 'Too many break points, b
     luffer overflow';istop=102; RETURN;ENDIF;INN(JP)=-I;t(jp)=(xs(kb1(i))-rng(kb1(i))-dk)/y;ENDIF;ENDIF
30    CONTINUE;if (jp.gt.1) then;do 502 kkk=jp/2, 1, -1;v=t(kkk);vinn=inn(kkk);kk=kkk
501   if (kk+kk .gt. jp) goto 500;j = kk + kk;if (j.lt.jp) then;if (t(j).gt.t(j+1)) j = j + 1;endif;if (v.le.t(j)) goto 500;t(kk)=t(
     lj);inn(kk)=inn(j);kk=j;goto 501
500   t(kk)=v;inn(kk)=vinn
502   continue;endif;t(jp+1)=0.0;jnp=jp;wt=sinf;ra=-sj(k);i=1
35    if (ra.le.0.0.or.i.gt.jp) goto 25;v=t(1);vinn=inn(1);t(1)=t(jnp);inn(1)=inn(jnp);t(jnp)=v;inn(jnp)=vinn;jnp=jnp-1;kk=1;v=t(kk)
      vinn=inn(kk)
521   if (kk+kk .gt. jnp) goto 520;j=kk+kk;if (j.lt.jnp) then;if (t(j).gt.t(j+1)) j=j+1;endif;if (v.le.t(j)) goto 520;t(kk)=t(j);inn
     l(kk)=inn(j);kk=j;goto 521
520   t(kk)=v;inn(kk)=vinn;kk=jp-i+1;jr=abs(inn(kk));wt=wt+ra*(t(kk)-t(kk+1));teta1=t(kk);w=abs(at(l+jr));w1=ra-w;if (abs(w1).lt.ere
     ll*abs(ra)) then;w1=0.0;endif;ra=w1;i=i+1;goto 35
25    i=i-1;IF (RA.GT.0.0.AND.(MJ(K).GT.1.AND.MJ(K).LT.9)) THEN;IF (KM(K).GT.N1) THEN;KA=N1-KM(K);ELSE;KA=KM(K);ENDIF;if(un>=0)write
     l(un,'(1X,A,I7)') 'PIVOT1 ERROR IN COLUMN',KA;ICAN(K)=0;ELSE;IF (RA.GT.0.0) THEN;IPIRO=0;MS=K;ELSE;IF((mj(k).eq.1.or.mj(k).eq.9
     l).and.T(jp-i+1).GT.FKS(K))THEN;IPIRO=0;MS=K;ELSE
305   IF (ABS(AT(L+JR)).GT.EPS) THEN;IF (WT.GT.A) THEN;IPIRO=JR;MS=K;A=WT;TETA=TETA1;IPVTIP=1;IF (INN(jp-i+1).LT.0) IPVTIP=2;IF (KTR
     lA.GT.3) THEN;IF (KM(K).GT.N1) THEN;KA=N1-KM(K);ELSE;KA=KM(K);ENDIF
100   FORMAT(' IN ',I6,' JP=',I4,' K=',I4,' PR=',I5,' PV',G12.4,
     1' W',G12.4,' DJ',G12.4);if(un>=0)write(un,100) KA,jp,i,JR,AT(L+JR),WT,SJ(K);ENDIF;ENDIF;ELSE;IF (I.GT.1) THEN;W=ABS(AT(L+JR));
      W1=RA+W;RC=MAX(RA,W);IF (W1.LT.EREL*RC) W1=0.0;RA=W1;WT=WT-RA*(T(jp-i+1)-T(jp-i+2));I=I-1;TETA1=T(jp-i+1);JR=ABS(INN(jp-i+1));
      GOTO 305;ENDIF;ENDIF;ENDIF;ENDIF;ENDIF
20    K=K+1;GOTO 15;ENDIF;IF (.NOT.MEHET) EPS=EPIV/10.0;IF (MS.EQ.0.AND..NOT.MEHET) GOTO 10;RETURN;END subroutine pide;subroutine dp
     liv1a
     \(     ipvtip,jq,  n1,n,    djrow,prow,   wp,dsinf,wt,dtheta
     \,mark,
     \t,indt,
     \ibsw,big,eps,erel,epiv,dk,     jp,     iq);integer(4) N,LOG_PR,ISTOP,MN,KTRA,MXN,INPK,INPP,IDB,NFFR;real(8) THETA;integer iq(n
     l);integer ipvtip,jq,n1,ibsw,jp;integer mark(n),indt(0:n1+1);double precision djrow(n),prow(n),t(0:n1+1);double precision big,e
     lps,erel,epiv,wp,dsinf,dk;integer i,ii,jj,j,k,mrj,jr,jnp;double precision eabs,epsthr,pivot,epivd,vp,w1,apj,w;double precision 
     ldtheta, wt, dj;COMMON /PARAMI/ MN(11),KTRA,MXN(38),nffr;integer(1) un; common/un/un;character(256) workpath;common /contr/ inp
     lk,inpp,log_pr,istop,iDB, workpath;intrinsic abs;theta=big;eabs=eps;epivd = epiv * 10.0;epsthr= epiv;jp=0;jq=0;vp = abs(wp);wt 
     l= dsinf;do 700 j=1,n;mrj=mark(j);if (mrj.le.1.or.mrj.gt.3) then;goto 700;endif;apj=prow(j);if (abs(apj).lt.eabs) goto 700;dj =
     l djrow(j);if (ipvtip.eq.1) then;apj = -apj;endif;if (apj.gt.0.0) then;if (dj.gt.dk) then;jp=jp+1;t(jp)=abs((dj+dk)/apj);indt(j
     lp)=j;if (mrj.eq.3) then;jp=jp+1;t(jp)=abs((dj-dk)/apj);indt(jp)=-j;endif;elseif
     \(-dk.le.dj.and.dj.le.dk) then;jp=jp+1;t(jp)=abs((dj+dk)/apj);indt(jp)=j;endif;else;if (dj.lt.-dk) then;jp=jp+1;t(jp)=abs((dj-d
     lk)/apj);indt(jp)=-j;if (mrj.eq.3) then;jp=jp+1;t(jp)=abs((dj+dk)/apj);indt(jp)=j;endif;elseif
     \(-dk.le.dj.and.dj.le.dk.and.mrj.eq.3) then;jp=jp+1;t(jp)=abs((dj-dk)/apj);indt(jp)=-j;endif;endif
700   continue;t(0)=0.0;ibsw=0;jnp=jp;i=1
35    if (i.gt.jp) goto 26;jj=i;do 36 j=i+1,jp;if (t(j).lt.t(jj)) then;jj=j;endif
36    continue;if (jj.ne.i) then;j=jj;w=t(i);t(i)=t(j);t(j)=w;jj=indt(i);indt(i)=indt(j);indt(j)=jj;endif;jr=abs(indt(i));wt = wt + 
     lvp * (t(i)-t(i-1));w1 = vp - abs(prow(jr));if (abs(w1).lt.erel*abs(vp)) then;w1 = 0.0;endif;vp = w1;if (vp.gt.0.0) then;i=i+1;
      goto 35;else;goto 25;endif
26    i=i-1
25    ii = i;jq = abs(indt(i));pivot=prow(jq);if (indt(i).lt.0.0) then;dtheta = (djrow(jq)-dk)/pivot;else;dtheta = (djrow(jq)+dk)/pi
     lvot;endif;if (abs(pivot).lt.epivd) then;do 115 j=ii+1,jp;call getnexti (j,jp,t,indt);i = i + 1;if (abs(prow(abs(indt(i)))).gt.
     lepsthr) then;if(log_pr>-1.and.un>=0)write(un,'(a,g16.8,a)')
     \'Pivot ',prow(abs(indt(i-1))),'  too small, next taken';do 116 k=ii,i-1;prow(abs(indt(k)))=0.0
116   continue;goto 25;endif
115   continue;if(log_pr>-1.and.un>=0) write(un,'(a,i5,a)')
     \'All potential pivots out of',jp,' are too small, stop';istop=122; RETURN;endif;if (ii.gt.0) then;iq(ii) = iq(ii) + 1;endif;re
     lturn;end subroutine dpiv1a;subroutine dpiv2a
     \(     ipvtip,jq,m,n1,n,    nffr,djrow,prow,xs,xbp,mark,at,rng,
     \aij,inda,colbeg,colendx,dk,dtheta,
     \t,indt,listbsw,iflag,
     \ibsw,big,eps,erel,epiv,rowmax,jp   );integer(4) NFFR,LOG_PR,ISTOP,INPK,INPP,IDB;integer ipvtip,jq,m,n1,n,ibsw,jp;integer mark(
     ln),inda(nffr:*),colbeg(n1),colendx(n1),indt(0:n1+1);integer iflag(0:n),listbsw(n1);double precision aij(nffr:*),djrow(n),prow(
     ln),t(0:n1+1);double precision big,eps,xbp,rowmax,erel,epiv,dk,dtheta;double precision at(m+1),rng(n),xs(n);integer i,ii,jj,j,k
     l,mrj,jnp,jr;double precision ptheta,theta,uj,eabs,epsthr,pivot,epivd,apj,w;integer(1) un; common/un/un;character(256) workpath
      common /contr/ inpk,inpp,log_pr,istop,iDB, workpath;intrinsic abs;theta=big;eabs=eps;epsthr=rowmax*epiv*100.0;epivd = epiv * 1
     l0.0;jp=0;jq=0;do 100 j=1,n;if (iflag(j).eq.1) goto 100;mrj=mark(j);if (mrj.gt.3.and.mrj.lt.9) then;goto 100;endif;if (mrj.eq.0
     l) goto 100;apj=prow(j);if (abs(apj).lt.eabs) goto 100;if (ipvtip.eq.1) then;if (mrj.eq.9.and.apj.lt.0.0) then;goto 100;elseif 
     l(mrj.le.2.and.apj.gt.0.0) then;goto 100;endif;else;if (mrj.eq.9.and.apj.gt.0.0) then;goto 100;elseif (mrj.le.2.and.apj.lt.0.0)
     l then;goto 100;endif;endif;jp=jp+1;if (mrj.ge.3) then;t(jp) = abs((djrow(j) - dk) / apj);else;t(jp) = abs((djrow(j) + dk) / ap
     lj);endif;indt(jp)=j
100   continue;if (jp.eq.0) then;goto 999;endif;t(0)=0.0;ibsw=0;jnp=jp;i=1
35    if (i.gt.jp) goto 26;jj=i;do 36 j=i+1,jp;if (t(j).lt.t(jj)) then;jj=j;endif
36    continue;if (jj.ne.i) then;j=jj;w=t(i);t(i)=t(j);t(j)=w;jj=indt(i);indt(i)=indt(j);indt(j)=jj;endif;jr=indt(i);w = xbp - abs(p
     lrow(jr)) * rng(jr);if (abs(w).lt.erel*abs(xbp)) then;w = 0.0;endif;xbp = w;if (xbp.lt.-dk) then;goto 25;endif;ptheta=xbp/prow(
     ljr);if (mark(jr).eq.1.or.mark(jr).eq.9) then;if (abs(ptheta).gt.rng(jr)) then;ibsw=ibsw+1;listbsw(ibsw) = jr;else;goto 25;endi
     lf;else;goto 25;endif;i=i+1;goto 35
26    if (i.ge.jp) then;jp = 0;goto 999;endif;i = i - 1
25    ii = i;pivot=prow(indt(i));jq=indt(i);if (ipvtip.eq.1) then;dtheta=-t(i);if (dtheta.gt.0.0) then;dtheta = -t(i);endif;else;dth
     leta= t(i);if (dtheta.lt.0.0) then;dtheta = t(i);endif;endif;if (abs(pivot).lt.epivd) then;do 115 j=ii+1,jp;call getnexti (j,jp
     l,t,indt);i = i + 1;if (abs(prow(indt(i))).gt.epsthr) then;if(log_pr>-1.and.un>=0)write(un,'(a,g16.8,a)')
     \'Pivot ',prow(indt(i-1)),'  too small, next taken';do 116 k=ii,i-1;prow(indt(k))=0.0
116   continue;goto 25;endif
115   continue;if(log_pr>-1.and.un>=0) write(un,'(a,i5,a)')
     \'All potential pivots out of',jp,' are too small, stop';istop=123; RETURN;endif;goto 999;if (ibsw.gt.0) then;do 110 i=1,m+1;at
     l(i)=0.0
110   continue;do 120 k=1,ibsw;j=listbsw(k);if (mark(j).eq.1) then;mark(j)=9;xs(j)=rng(j);if (j.gt.n1) then;at(j-n1) = at(j-n1) + rn
     lg(j);else;uj = rng(j);do 130 i=colbeg(j),colendx(j);at(inda(i)) = at(inda(i)) + uj * aij(i)
130   continue;endif;else;mark(j)=1;xs(j)=0.0;if (j.gt.n1) then;at(j-n1) = at(j-n1) - rng(j);else;uj = rng(j);do 140 i=colbeg(j),col
     lendx(j);at(inda(i)) = at(inda(i)) - uj * aij(i)
140   continue;endif;endif
120   continue;endif
999   return;
      end subroutine dpiv2a
