      subroutine TakeSpMatrix(mname,Elem,iRow,iCol,kCoef, sp_out, iid,iprob,ibench,n,m,
     +yiw,p,chw,iret);use ModCommons;integer(4) kCoef,n,m,iid,iprob,ibench,irow(kcoef),icol(kcoef),iret;real(8) Elem(kcoef),p(m);   
      logical sp_out;character(*) mname,chw;integer(4)  kelm;integer(4) k1,k2,nk,i,ic,ir,iret1; real(8) w,xhuge;real(8),target:: yiw
     l(0:n,0:m-1);iret=0;if(idb<=-2) RETURN;xhuge=huge(w);if(idb>0)then;do i=1,kCoef; w=Elem(i);if(isnan(w).or.abs(w)>=xhuge)then;wr
     lite(chw,'(3a,i7)')'Internal error: Incorrect element value in pmatrix ',trim(mname),'. String # ',i; GOTO 300;endif;enddo;endi
     lf;if(sp_out)then;kElm=kCoef;if(iprob>=0) kElm=kElm-m;if(iid>=0)   kElm=kElm-m;call FillSpMatrix(mname,idb,Elem,iRow,iCol,kCoef
     l, kelm, iid,iprob,ibench,n,m,
     +yiw,p,chw,iret1);if(iret1==1) goto 79999;else; yiw=0d0; p=0d0;nk=n;if(iid.ge.0) nk=nk+1;if(iprob.ge.0) nk=nk+1;if(ibench.lt.0)
     lthen; nk=nk-1; endif;k1=min0(iid,iprob); k2=max0(iid,iprob);if(k2<0)k2=nk+10; if(k1<0)then; k1=k2; k2=nk+10; endif;do i=1,kCoe
     lf;if(idb<=0)then;if(iid<0)then; read(33,*,end=100,err=100)ir,ic,w;  ir=ir-1; ic=ic-1;else; read(33,*,end=100,err=100)ir,ir,ic,
     lw;  ir=ir-1; ic=ic-1;endif;else; ir=irow(i); ic=icol(i); w=Elem(i);endif;if(ir<0.or.ir>=m.or.ic<0.or.ic>nk)then;write(chw,'(3a
     l,3i9,e10.2)')'Internal error: Incorrect row or column index in ',trim(mname),
     +'. String #,iRow,iCol,Value=',i,ir,ic,w;GOTO 300;endif;if(iid>0)then; write(chw,'(a)')'Integrnal error: Incorrect id position 
     lin pmatrix'; GOTO 300;endif;if(ic<k1)then; if(ic/=ibench)then; yiw(ic,ir)=-w; else; yiw(ic,ir)=w; endif;elseif(ic==iprob)then;
       p(ir+1)=w;elseif(ic<k2)then; if(ic-1/=ibench)then; yiw(ic-1,ir)=-w; else; yiw(ic-1,ir)=w; endif;else; if(ic-2/=ibench)then; y
     liw(ic-2,ir)=-w; else; yiw(ic-2,ir)=w; endif;endif;enddo;endif;return
100   write(chw,'(3a,i7)')'Error in reading matrix file ',trim(mname),'. String # ',i
300   call putmess('E',7480,'Pmatrix Reading',chw)
79999 continue;iret=1; return;end subroutine TakeSpMatrix;SUBROUTINE SpMatrixAddrs_0(iyw,m,n,kelm,
     +a0,a1,a2,a3,a4,a5,krows);use modcommons; use CiFort;integer(4) iyw(*),m,n,krows,k,list(*); integer(plen) a0,a1,a2,a3,a4,a5;rea
     ll(8) yiw(*),w,ww; logical sp_out;       target iyw,yiw;integer(4) i1,i2,j1,j2,ix(0:*),je(*); real(8) v1m(*),wm(*),sm,v0n(0:*),
     lcj,g1(0:*),g(0:*);integer(4) i,it,iw,j,jw;integer(4),save:: kmem,mb,nb;integer(4),pointer,save:: icl(:),ifer(:);integer(4) kbl
     locks,ibench,kelm,iout,jd,il,ja,jp,jb,jb1,io;integer(INT_PTRKIND),save:: pkbl,pibn;pointer (pkbl,kblocks),(pibn,ibench);real(8)
     l,pointer,save:: yie(:); real(8) wt;integer(4),pointer:: iclw(:); integer(4),pointer,save:: iwrk(:),ilst(:);real(8),pointer:: x
     lwrk(:);character(256) chw;nullify(yie,iwrk);i=1+1+(m+2)/2+(kelm+1)/2+kelm;krows=i/(n+1)+1;a0=loc(iyw); a1=a0+8; a2=a1+8; i=(m+
     l2)/2; a3=a2+i*8;i=(kelm+1)/2; a4=a3+i*8; a5=a4+kelm*8;RETURN;ENTRY SpMatrixAddrs(iyw,yiw,m,n,   sp_out,krows)
      i=n+2; krows=m;sp_out=.false.; if(m>0)then; if(yiw(i)==huge(yiw))sp_out=.true.; endif;pkbl=loc(yiw)-id8bt;if(kblocks>=1)then; 
      sp_out=.true.; krows=m/kblocks; mb=krows; nb=n/kblocks; yie=>yiw(i:i+mb*(nb+1));pibn=loc(yiw)-id8bt+4;kmem=m*(nb+1); RETURN;en
     ldif;if(.not.sp_out)RETURN;nb=n; mb=m;i=1+i*2; kmem=iyw(i);i=i+2; ifer=>iyw(i:i+m);krows=(m+2)/2; i=i+krows*2; icl=>iyw(i:i+kme
     lm-1);krows=(kmem+1)/2; i=(i-1)/2+krows+1; yie=>yiw(i:i+kmem-1);i=1+1+(m+2)/2+(kmem+1)/2+kmem; krows=i/(n+1)+1;RETURN;ENTRY SpM
     latrixKcut(m,  krows)
      krows=m; if(kblocks<0) krows=-m/kblocks;RETURN;ENTRY SpMatrixKelm(n)
      n=kmem;RETURN;ENTRY SpMatrixDataPrint(m,n,iout)
      if(.not.(iout>7.and.iout<100)) RETURN;do j=1,m; do i=ifer(j),ifer(j+1)-1; write(iout,'(i5,a,i6,a,g15.9)')j,char(9),icl(i)+1,ch
     lar(9),yie(i);enddo; enddo;RETURN;ENTRY SpM_GetEl(j1,i1,   sm)
      sm=0d0;if(kblocks<=0)then;do i=ifer(j1),ifer(j1+1)-1; if(icl(i)==i1)then; sm=yie(i); Exit; endif;enddo;else; i=(i1-1)/nb; iw=i
     l1;if(i>0)then; iw=i1-i*nb; if(iw<=ibench)iw=iw-1; endif;jw=1; it=nb+1;if(1+i*mb<=j1.and.j1<=mb+i*mb)then;do j=1+i*mb,mb+i*mb; 
      if(j==j1)then; sm=yie(jw+iw); Exit; endif; jw=jw+it;enddo;endif;endif;RETURN;ENTRY SpM_ColVectM(m,i1,v1m,   wm)
      wm(1:m)=0d0;if(kblocks<=0)then;do j=1,m; do i=ifer(j),ifer(j+1)-1; if(icl(i)==i1)then; wm(j)=yie(i)*v1m(j); Exit; endif;enddo;
          enddo;else; i=(i1-1)/nb; iw=i;if(i>0)then; iw=i1-i*nb; if(iw<=ibench)iw=iw-1; endif;jw=1; it=nb+1;do j=1+i*mb,mb+i*mb; wm(
     lj)=yie(jw+iw)*v1m(j); jw=jw+it;enddo;endif;RETURN;ENTRY SpM_ColVect(m,i1,v1m,   sm)
      sm=0d0;if(kblocks<=0)then;do j=1,m; do i=ifer(j),ifer(j+1)-1; if(icl(i)==i1)then; sm=sm+yie(i)*v1m(j); Exit; endif;enddo;    e
     lnddo;else; i=(i1-1)/nb; iw=i1;if(i>0)then; iw=i1-i*nb; if(iw<=ibench)iw=iw-1; endif;jw=1; it=nb+1;do j=1+i*mb,mb+i*mb; sm=sm+y
     lie(jw+iw)*v1m(j); jw=jw+it;enddo;endif;RETURN;ENTRY SpM_ColSpVect(m,i1, wm,je,k,   sm)
      sm=0d0;if(kblocks<=0)then;do jw=1,k; j=je(jw); do i=ifer(j),ifer(j+1)-1; if(icl(i)==i1)then; sm=sm+yie(i)*wm(jw); Exit; endif;
      enddo;    enddo;else; i=(i1-1)/nb; iw=i1;if(i>0)then; iw=i1-i*nb; if(iw<=ibench)iw=iw-1; endif;jw=1; it=nb+1;do j=1+i*mb,mb+i*
     lmb; sm=sm+yie(jw+iw)*v1m(j); jw=jw+it;enddo;endif;RETURN;ENTRY SpM_RowVect(m,n,j1,v0n,  sm)
      sm=0d0;if(kblocks<=0)then;do i=ifer(j1),ifer(j1+1)-1; sm=sm+yie(i)*v0n(icl(i)); enddo;else; j=(j1-1)/mb; jw=j1-j*mb;jw=(jw-1)*
     l(nb+1)+1;do i=0,nb; iw=i+j*nb;if(j>0)then;if(i<ibench)then; iw=iw+1;elseif(i==ibench)then; iw=i;endif;endif;sm=sm+yie(jw+i)*v0
     ln(iw);enddo;endif;RETURN;ENTRY SpM_RowVectCj(m,n,j1,ix,v0n,  sm,cj)
      sm=0d0; cj=0d0;if(kblocks<=0)then;do i=ifer(j1),ifer(j1+1)-1; sm=sm+yie(i)*v0n(icl(i)); if(ix(icl(i))==0) cj=yie(i); enddo;els
     le; j=(j1-1)/mb; jw=j1-j*mb;jw=(jw-1)*(nb+1)+1;do i=0,nb; iw=i+j*nb;if(j>0)then;if(i<ibench)then; iw=iw+1;elseif(i==ibench)then
     l; iw=i;endif;endif;sm=sm+yie(jw+i)*v0n(iw); if(ix(iw)==0) cj=yie(jw+i);enddo;endif;RETURN;ENTRY SpM_ColPermut(m,i1,j1)
      if(kblocks<=0)then;do j=1,m; do i=ifer(j),ifer(j+1)-1; if(icl(i)==i1)then; icl(i)=j1; elseif(icl(i)==j1)then; icl(i)=i1; endif
      enddo;    enddo;else; i=(i1-1)/nb; iw=i1;if(i>0)then; iw=i1-i*nb; if(iw<=ibench)iw=iw-1; endif;j=(j1-1)/nb; jw=j1;if(j>0)then;
       jw=j1-j*nb; if(jw<=ibench)jw=jw-1; endif;i=1; it=nb+1;do j=1,mb; wt=yie(i+iw); yie(i+iw)=yie(i+jw); yie(i+jw)=wt; i=i+it;endd
     lo;endif;RETURN;ENTRY SpM_ColSign(m,i1)
      if(kblocks<=0)then;do j=1,m; do i=ifer(j),ifer(j+1)-1; if(icl(i)==i1)then; yie(i)=-yie(i); Exit; endif;enddo;    enddo;else; i
     l=(i1-1)/nb; iw=i1;if(i>0)then; iw=i1-i*nb; if(iw<=ibench)iw=iw-1; endif;i=1; it=nb+1;do j=1,mb; yie(i+iw)=-yie(i+iw); i=i+it;e
     lnddo;endif;RETURN;ENTRY SpM_MatrSign(m)
      if(kblocks<=0)then; i=ifer(1); j=ifer(m+1)-1;yie(i:j)=-yie(i:j);else;chw='Internal error 1. kblocks>0'; call putmess('S',9983,
     l'subroutine SpM_MatrSing',chw); RETURN;endif;RETURN;ENTRY SpM_GetCol(j1,i1,i2, wm)
      jd=i1-1;if(kblocks<=0)then;do j=i1,i2; do i=ifer(j),ifer(j+1)-1; if(icl(i)==j1)then; wm(j-jd)=yie(i); Exit; endif;enddo;      
      enddo;else; i=(j1-1)/nb; iw=j1;if(i>0)then; iw=j1-i*nb; if(iw<=ibench)iw=iw-1; endif;jw=1; it=nb+1;do j=max0(1+i*mb,i1),min0(m
     lb+i*mb,i2); wm(j-jd)=yie(jw+iw); jw=jw+it;enddo;endif;RETURN;ENTRY SpM_ChangeCol(iyw,yiw,j1,i1,i2, wm)
      il=0;  goto 20;ENTRY SpM_ChangeColList(iyw,yiw,j1,list,i2, wm)
      il=1; it=0
20    nullify(ilst);if(kblocks>0)then;chw='Internal error 1. kblocks>0'; call putmess('S',9943,'subroutine SpM_ChangeCol',chw); RETU
     lRN;endif;allocate(iwrk(i2)); iwrk=0;iw=0;do i=1,i2; if(wm(i)/=0.) iwrk(i)=1;if(il==0)then; it=i+i1-1; else; it=list(i); endif;
      do j=ifer(it),ifer(it+1)-1; if(icl(j)>=j1) Exit; enddo;if(j<ifer(it+1))then; if(icl(j)==j1) iwrk(i)=iwrk(i)-1; endif;if(iwrk(i
     l)==0)then; if(wm(i)/=0.) yie(j)=wm(i);else; iw=iw+iwrk(i); iwrk(i)=j*iwrk(i);endif;enddo;if(count(iwrk/=0)==0)then; deallocate
     l(iwrk); RETURN; endif;allocate(xwrk(max(kmem+iw,i2))); allocate(iclw(kmem+iw));if(il==1)then; allocate(ilst(i2)); xwrk(:i2)=li
     lst(:i2); call sortVK(i2,xwrk,ilst); endif;ja=1; jp=0;if(il==0)then;do i=1,i2;if(iwrk(i)/=0)then; jb=abs(iwrk(i)); jb1=jb-1;do 
     lj=ja,jb1; iclw(jp+j)=icl(j); xwrk(jp+j)=yie(j); enddo;if(iwrk(i)<0)then; jp=jp-1; ja=jb+1;else; xwrk(jb+jp)=wm(i); iclw(jb+jp)
     l=j1; jp=jp+1; ja=jb;endif;endif;it=i+i1; ifer(it)=ifer(it)+jp;enddo;else;do io=1,i2; i=ilst(io);if(iwrk(i)/=0)then; jb=abs(iwr
     lk(i)); jb1=jb-1;do j=ja,jb1; iclw(jp+j)=icl(j); xwrk(jp+j)=yie(j); enddo;if(iwrk(i)<0)then; jp=jp-1; ja=jb+1;else; xwrk(jb+jp)
     l=wm(i); iclw(jb+jp)=j1; jp=jp+1; ja=jb;endif;endif;if(io<i2)then; il=list(ilst(io+1)); else; il=mb+1; endif;do it=list(i)+1,il
     l; ifer(it)=ifer(it)+jp; enddo;enddo;endif;do it=it+1,mb+1; ifer(it)=ifer(it)+jp; enddo;jb=kmem; do j=ja,jb; xwrk(j+jp)=yie(j);
       iclw(j+jp)=icl(j); enddo;kmem=kmem+iw;i=1+(nb+2)*2;     iyw(i)=kmem;i=i+2;iw=(mb+2)/2;      i=i+iw*2;         icl=>iyw(i:i+km
     lem-1); do j=1,kmem; icl(j)=iclw(j); enddo;iw=(kmem+1)/2;    i=(i-1)/2+iw+1;   yie=>yiw(i:i+kmem-1); do j=1,kmem; yie(j)=xwrk(j
     l); enddo;deallocate(xwrk,iclw,iwrk);if(associated(ilst))deallocate(ilst);RETURN;ENTRY SpM_ChangeRow(iyw,yiw,i1,j1,j2, wm)
      if(associated(iwrk))deallocate(iwrk); allocate(iwrk(j1:j2)); iwrk=0;jd=j1-1;if(kblocks<=0)then;iw=0; it=ifer(i1);do j=j1,j2; i
     lf(wm(j-jd)/=0.) iwrk(j)=1;do i=it,ifer(i1+1)-1; if(icl(i)>=j) Exit; enddo; it=i+1;if(i<ifer(i1+1))then; if(icl(i)==j) iwrk(j)=
     liwrk(j)-1; endif;if(iwrk(j)==0)then; if(wm(j-jd)/=0.) yie(i)=wm(j-jd);else; iw=iw+iwrk(j); iwrk(j)=i*iwrk(j);endif;enddo;if(co
     lunt(iwrk/=0)==0)then; deallocate(iwrk); RETURN; endif;allocate(xwrk(kmem+iw)); allocate(iclw(kmem+iw));ja=1; jp=0;do j=j1,j2;i
     lf(iwrk(j)/=0)then; jb=abs(iwrk(j)); jb1=jb-1;iclw(ja+jp:jb1+jp)=icl(ja:jb1);xwrk(ja+jp:jb1+jp)=yie(ja:jb1);if(iwrk(j)<0)then; 
      jp=jp-1; ja=jb+1;else; xwrk(jb+jp)=wm(j-jd); iclw(jb+jp)=j; jp=jp+1; ja=jb;endif;endif;enddo;do i=i1+1,mb; ifer(i)=ifer(i)+jp;
       enddo;jb=kmem; xwrk(ja+jp:jb+jp)=yie(ja:jb); iclw(ja+jp:jb+jp)=icl(ja:jb);kmem=kmem+iw;i=1+(nb+2)*2;     iyw(i)=kmem;i=i+2;iw
     l=(mb+2)/2;      i=i+iw*2;          icl=>iyw(i:i+kmem-1); icl=iclw;iw=(kmem+1)/2;    i=(i-1)/2+iw+1;    yie=>yiw(i:i+kmem-1); y
     lie=xwrk;deallocate(xwrk,iclw,iwrk);else;chw='Internal error 1. kblocks>0'; call putmess('S',9948,'subroutine SpM_ChangeRow',ch
     lw);endif;RETURN;ENTRY SpM_GetSpCol(j1,i1,i2, wm,je,k)
      k=0;if(kblocks<=0)then;do j=i1,i2; do i=ifer(j),ifer(j+1)-1; if(icl(i)==j1)then; k=k+1; wm(k)=yie(i); je(k)=j; Exit; endif;end
     ldo;      enddo;else; i=(j1-1)/nb; iw=j1;if(i>0)then; iw=j1-i*nb; if(iw<=ibench)iw=iw-1; endif;jw=1; it=nb+1;do j=max0(1+i*mb,i
     l1),min0(mb+i*mb,i2); if(yie(jw+iw)/=0.)then; k=k+1; wm(k)=yie(jw+iw); je(k)=j; endif; jw=jw+it;enddo;endif;RETURN;ENTRY SpM_Ge
     ltSpRow(i1,j1,j2, wm,je,k)
      k=0;if(kblocks<=0)then;do i=ifer(i1),ifer(i1+1)-1; j=icl(i); if(j<j1.or.j>j2)Cycle; k=k+1; wm(k)=yie(i); je(k)=j; enddo;else; 
      j=(i1-1)/mb; jw=i1-j*mb;jw=(jw-1)*(nb+1)+1;do i=0,nb; iw=i+j*nb;if(j>0)then;if(i<ibench)then; iw=iw+1;elseif(i==ibench)then; i
     lw=i;endif;endif;if(iw<j1.or.iw>j2)Cycle;k=k+1; wm(k)=yie(jw+i); je(k)=iw;enddo;endif;RETURN;ENTRY SpM_GetRow(i1,j1,j2, wm)
      jd=j1-1;if(kblocks<=0)then;do i=ifer(i1),ifer(i1+1)-1; j=icl(i); if(j<j1.or.j>j2)Cycle; wm(j-jd)=yie(i); enddo;else; j=(i1-1)/
     lmb; jw=i1-j*mb;jw=(jw-1)*(nb+1)+1;do i=0,nb; iw=i+j*nb;if(j>0)then;if(i<ibench)then; iw=iw+1;elseif(i==ibench)then; iw=i;endif
      endif;if(iw<j1.or.iw>j2)Cycle;wm(iw-jd)=yie(jw+i);enddo;endif;RETURN;ENTRY SpM_AssElem(m,j1,i1,sm,   j2)
      j2=0;if(kblocks<=0)then;do j=ifer(i1),ifer(i1+1)-1; if(icl(j)==j1)then; yie(j)=sm; j2=1; Exit; endif;enddo;else; i=(i1-1)/mb; 
      iw=i1;if(i>0)then; iw=i1-i*mb; endif;j=(j1-1)/nb; jw=j1;if(j>0)then; jw=j1-j*nb; if(jw<=ibench)jw=jw-1; endif;i=1+(nb+1)*(iw-1
     l); yie(i+jw)=sm;endif;RETURN;ENTRY SpM_RowVect_2(m,n,j1,ix,v0n,  sm)
      sm=0d0;if(kblocks<=0)then;do i=ifer(j1),ifer(j1+1)-1; j=icl(i); if(ix(j)/=0)then; sm=sm-yie(i)*v0n(j)*v0n(j); else; sm=sm+yie(
     li); endif; enddo;else; j=(j1-1)/mb; jw=j1-j*mb;jw=(jw-1)*(nb+1)+1;do i=0,nb; iw=i+j*nb;if(j>0)then;if(i<ibench)then; iw=iw+1;e
     llseif(i==ibench)then; iw=i;endif;endif;if(ix(iw)/=0)then; sm=sm-yie(jw+i)*v0n(iw)*v0n(iw); else; sm=sm+yie(jw+i); endif;enddo;
      endif;RETURN;ENTRY SpM_SumMin(m,n,ix,   w,sm)
      if(kblocks<=0)then;do i=1,ifer(m+1)-1; if(ix(icl(i))==0) Cycle; ww=dabs(yie(i));if(ww>0d0.and.ww<1d10)then; w=w+ww; if(ww<sm)s
     lm=ww; endif;enddo;else; iw=0;do j=1,mb; do i=0,nb; iw=iw+1; if(ix(i)==0) Cycle; ww=dabs(yie(iw));if(ww>0d0.and.ww<1d10)then; w
     l=w+ww; if(ww<sm)sm=ww; endif;enddo;     enddo;w=(w*m)/mb;endif;RETURN;ENTRY SpM_GradAddRow(j1,w,ix,  g1)
      if(kblocks<=0)then;do i=ifer(j1),ifer(j1+1)-1; g1(ix(icl(i)))=g1(ix(icl(i)))+w*yie(i); enddo;else; j=(j1-1)/mb; jw=j1-j*mb;jw=
     l(jw-1)*(nb+1)+1;do i=0,nb; iw=i+j*nb;if(j>0)then;if(i<ibench)then; iw=iw+1;elseif(i==ibench)then; iw=i;endif;endif;g1(ix(iw))=
     lg1(ix(iw))+w*yie(jw+i);enddo;endif;RETURN;ENTRY SpM_AddRow(j1,w,  g)
      if(kblocks<=0)then;do i=ifer(j1),ifer(j1+1)-1; g(icl(i))=g(icl(i))+w*yie(i); enddo;else; j=(j1-1)/mb; jw=j1-j*mb;jw=(jw-1)*(nb
     l+1)+1;do i=0,nb; iw=i+j*nb;if(j>0)then;if(i<ibench)then; iw=iw+1;elseif(i==ibench)then; iw=i;endif;endif;g(iw)=g(iw)+w*yie(jw+
     li);enddo;endif;RETURN;
      END SUBROUTINE SpMatrixAddrs_0
