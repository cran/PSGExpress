      subroutine Get_col(m,nk,i,Elem,n,     i1,yi,iout);integer(4) m,nk,i,n,i1,iout,     j;real(8)  Elem(0:nk,*),yi(0:n,0:*),   w,xh
     luge;xhuge=huge(w); iout=0;if(i1<0) i1=0;yi(i1,1:m)=-Elem(i,1:m);do j=1,m; w=Elem(i,j);if(isnan(w).or.abs(w)>=xhuge)then; iout=
     lj; RETURN; endif;enddo;i1=i1+1;return;end subroutine;subroutine Get_prob(m,nk,i,Elem,n,i1,yi,  iout);integer(4) m, nk,i,n,i1,i
     lout,   j;real(8)  Elem(0:nk,m), yi(0:n,m), w,xhuge;xhuge=huge(w); iout=0;if(i1<0) i1=0;yi(i1,1:m)=+Elem(i,1:m);do j=1,m; w=Ele
     lm(i,j);if(isnan(w).or.abs(w)>=xhuge)then; iout=j; RETURN; endif;enddo;i1=i1+1;return;end subroutine;subroutine Get_col_Cov(m,n
     lk,i,Elem,n,i1,yi,  iout);integer(4) m, nk,i,n,i1, iout, j;real(8)  Elem(0:nk,m), yi(0:n,0:m), w,xhuge;xhuge=huge(w); iout=0;if
     l(i1<0) i1=0;yi(i1,1:m)=+Elem(i,1:m);do j=1,m; w=Elem(i,j);if(isnan(w).or.abs(w)>=xhuge)then; iout=j; RETURN; endif;enddo;i1=i1
     l+1;return;end subroutine;subroutine FillSpMatrix(mname,idb,Elem,iRow,iCol,kCoef, kelm0, iid,iprob,ibench,n,m,
     +yiw,p,       chw,iret);use CiFort;integer(4) kCoef,n,m,idb,iid,iprob,ibench,irow(kcoef),icol(kcoef),kelm0,iret;real(8) Elem(kc
     loef),p(m),yiw;character(*) mname,chw;integer(4) k1,k2,i,i1,i2,ir1,ir,ic,idi,nk; real(8) w;integer(4) kelm,iformal,ifer(0:m),ic
     ll(kelm0); real(8) SPkod,yie(kelm0);pointer(a0,SPkod),(a2,ifer),(a3,icl),(a4,yie),(a1,kelm),(a5,iformal);real(8) xwrk(:); integ
     ler(4) list(:); allocatable xwrk,list;interface;subroutine SpMatrixAddrs_0(w,m,n,kelm,  a0,a1,a2,a3,a4,a5,krows);use IntelInter
     lf;real(8),target::w; integer(4) m,n,krows,kelm; integer(plen) a0,a1,a2,a3,a4,a5;end;end interface;iret=0;if(idb>0)then;if(maxv
     lal(iRow(:kcoef))>=1e7.or.maxval(iCol(:kcoef))>=1e7)then;chw='Internal error: iRow or iCol > 1e7'; call putmess('S',7481,'FillS
     lpMatrix',chw); goto 79999;endif;allocate(xwrk(kcoef),list(kcoef),stat=i);if(i/=0)then; chw='Can not allocate arrays'; call put
     lmess('S',7483,'FillSpMatrix',chw); goto 79999;endif;do i=1,kcoef; xwrk(i)=iRow(i)*1e7+iCol(i); enddo;call sortVK(kcoef,xwrk,li
     lst);endif;call SpMatrixAddrs_0(yiw,m,n,kElm0, a0,a1,a2,a3,a4,a5,i);iformal=1;SPkod=huge(SPkod); kelm=kelm0;nk=n;if(iid.ge.0) n
     lk=nk+1;if(iprob.ge.0) nk=nk+1;if(ibench.lt.0)then; nk=nk-1; endif;k1=min0(iid,iprob); k2=max0(iid,iprob);if(k2<0)k2=nk+10; if(
     lk1<0)then; k1=k2; k2=nk+10; endif;ir1=-1; i1=1;do i=1,kCoef;if(idb<=0)then;if(iid<0)then; read(33,*,end=100,err=100)ir,ic,w;  
      ir=ir-1; ic=ic-1;else; read(33,*,end=100,err=100)ir,ir,ic,w;  ir=ir-1; ic=ic-1;endif;else; i2=list(i); ir=irow(i2); ic=icol(i2
     l); w=Elem(i2);endif;if(ir<0.or.ir>=m.or.ic<0.or.ic>nk)then;write(chw,'(3a,i7)')'Incorrect row or column number in matrix ',tri
     lm(mname),'. Element #',i;call putmess('S',7485,'FillSpMatrix',chw); GOTO 79999;endif;if(iid>0)then; write(chw,'(3a,i7)')'Incor
     lrect id position'; call putmess('S',7487,'FillSpMatrix',chw); GOTO 79999;endif;if(ir>ir1)then; do i2=ir1+1,ir; ifer(i2)=i1; en
     lddo; ir1=ir; endif;if(ic<k1)then; idi=0;elseif(ic==iprob)then; p(ir+1)=w; idi=-1;elseif(ic==iid)then; idi=-1;elseif(ic<k2)then
     l; idi=1;else; idi=2;endif;if(idi>=0)then; icl(i1)=ic-idi;if(icl(i1)/=ibench)then; yie(i1)=-w; else; yie(i1)=w; endif;  i1=i1+1
      endif;enddo;do i2=ir1+1,m; ifer(i2)=i1; enddo;if(allocated(xwrk))deallocate(xwrk,list);return
100   write(chw,'(3a,i7)')'Error in reading matrix file ',trim(mname),'. Element ',i;call putmess('S',7489,'FillSpMatrix',chw); GOTO
     l 79999
79999 continue;if(allocated(xwrk))deallocate(xwrk,list);iret=1; return;
      end subroutine FillSpMatrix
