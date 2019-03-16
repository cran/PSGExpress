      subroutine Sorting(j0,jm1,fm,jp,jpb,polka,kpl1,kpl2,
     +klast,krest, jpmax,jpmin );integer(4) j0,jm1,kpl1,kpl2,jp(0:*),jpb(0:*);real(8) fm(*),polka(kpl2);integer(4) klast(kpl2),krest
     l,jpmax(kpl2+1),jpmin(kpl2+1);integer(4) jl(kpl2),jpr,j,jnxt,i,iw,inxt,i1,jf(kpl2+1);real(8) fj, pmax(kpl2+1),pmin(kpl2+1),w, x
     lhuge;logical lprint_sort;jpmax=0; jpmin=0;xhuge=huge(w)/2.;pmax=-xhuge; pmin=xhuge;krest=0;do i=kpl1,kpl2; jl(i)=j0;klast(i)=0
      enddo;jpr=j0; j=jp(j0); jnxt=jp(j);do while(j.ne.jm1); fj=fm(j);w=xhuge;do i=kpl1,kpl2;if(fj.gt.polka(i)) then;if(klast(i)==0)
     l then; jf(i)=j;pmax(i)=fj; jpmax(i)=j;pmin(i)=fj; jpmin(i)=j;else;if(fj>pmax(i)) then;pmax(i)=fj; jpmax(i)=j;elseif(fj<pmin(i)
     l) then;pmin(i)=fj; jpmin(i)=j;endif;endif;klast(i)=klast(i)+1;iw=jl(i);if(jpr.ne.iw) then;inxt=jp(iw);jp(iw)=j; jpb(j)=iw;else
      inxt=-1;endif;do while(jnxt.ne.jm1);fj=fm(jnxt);if(fj<=polka(i).or.fj>w) EXIT;if(fj>pmax(i)) then;pmax(i)=fj; jpmax(i)=jnxt;el
     lseif(fj<pmin(i)) then;pmin(i)=fj; jpmin(i)=jnxt;endif;klast(i)=klast(i)+1;j=jnxt; jnxt=jp(j);enddo;if(inxt>=0) then;jp(jpr)=jn
     lxt; jpb(jnxt)=jpr;jpb(inxt)=j; jp(j)=inxt;endif;do i1=i,kpl2;if(jl(i1)==iw) jl(i1)=j;enddo;GOTO 100;endif;w=polka(i);enddo;if(
     lkrest==0) jf(i)=j;krest=krest+1;if(fj>pmax(i)) then;pmax(i)=fj; jpmax(i)=j;endif;if(fj<pmin(i)) then;pmin(i)=fj; jpmin(i)=j;en
     ldif
100   continue;jpr=jpb(jnxt);j=jnxt;jnxt=jp(j);enddo;do i=kpl1,kpl2; if(klast(i)<=1) cycle;j=jpmax(i);if(j.ne.jf(i))then; call inser
     lt_f(jpb,jp,jp(j),j,jf(i),jpb(j));jf(i)=j;endif;j=jpmin(i); jpr=jp(j);do i1=i+1,kpl2; if(klast(i1)>0) exit; enddo;if(i1<=kpl2.o
     lr.krest>0)then; iw=jf(i1); else; iw=jm1; endif;if(j==jpmax(i))then;jpmin(i)=jpb(iw);else;call insert_f(jpb,jp,jpr,j,iw,jpb(j))
      endif;enddo;if(krest>1) then; j=jpmax(i);if(j.ne.jf(i))then; call insert_f(jpb,jp,jp(j),j,jf(i),jpb(j));jf(i)=j;endif;j=jpmin(
     li); jpr=jp(j);if(j==jpmax(i))then;jpmin(i)=jpb(jm1);else;call insert_f(jpb,jp,jpr,j,jm1,jpb(j));endif;endif;lprint_sort=.false
     l.;if(lprint_sort) then;do i=kpl1,kpl2+1;if(i>kpl2)then; inxt=krest; else; inxt=klast(i);endif;if(inxt<=0) cycle;j=jpmax(i); i1
     l=jpmin(i); iw=jf(i);write(35,'(a,i4,7i7,4e15.7,3i7,2e15.7)')
     +'i,klast,jpx(i),jpi(i),jpb(jpx),jp(jpi),jp(jpb(jpx)),jpb(jp(jpi))'
     +//',pmax(i),pmin(i),jf(i),jpb(jf),jp(jpb(jf)),fm(jf),fm(jpb(jf))',
     +i,inxt,j,i1,jpb(j),jp(i1),jp(jpb(j)),jpb(jp(i1)),
     +pmax(i),fm(j),pmin(i),fm(i1)
     +,iw,jpb(iw),jp(jpb(iw)),fm(iw);enddo;write(35,*)' ';endif;return;end subroutine Sorting;subroutine sortVK(m,fm, list,inverslis
     lt, iouttype,iorder);use IntelInterf;integer (SIZEOF_SIZE_T) array_len, array_size;integer(4),optional:: iouttype,iorder,invers
     llist(*);integer(4) m,list(*),ifm(*); real(8) fm(*);integer(4),allocatable:: isf(:);integer(4) i,iwod,j,inpt;character(32) chw;
      inpt=0; goto 100;ENTRY sortVKint4(m,ifm, list,inverslist, iouttype,iorder)
      inpt=1
100   continue;iwod=-1;if(present(iorder))then;if(iorder==-1)then; iwod=1;elseif(iorder/=1)then;chw='Internal error 1. iorder/=1'; c
     lall putmess('S',9949,'subroutine sortVK',chw);endif;endif;do i=1,m; list(i)=i; enddo;array_len = m; array_size = 4;if(inpt==0)
     lthen; CALL qsort(list,array_len,array_size,cmp_func_real);else; CALL qsort(list,array_len,array_size,cmp_func_int4);endif;if(p
     lresent(inverslist))then;if(.not.present(iouttype))then;do j=1,m; inverslist(list(j))=j; enddo;elseif(iouttype==1)then;do j=1,m
     l; inverslist(list(j))=j; enddo;endif;endif;if(present(iouttype))then;if(iouttype==2)then; allocate(isf(m)); isf=list(:m);do i=
     l2,m; list(isf(i-1)+1)=isf(i); enddo;list(isf(m)+1)=m+1;if(present(inverslist))then; inverslist(m+2)=isf(m);do i=m-1,1,-1; inve
     lrslist(isf(i+1)+1)=isf(i); enddo;inverslist(isf(1)+1)=0;endif;deallocate(isf);elseif(iouttype/=1)then;chw='Internal error 2. i
     louttype/=1'; call putmess('S',9952,'subroutine sortVK',chw);endif;endif;return;CONTAINS
#ifdef __GNUC__
      integer(4) function cmp_func_real(i1, i2)
#else
      integer(2) function cmp_func_real(i1, i2)
#endif
      integer(4) i1,i2; real(8) w1,w2;w1=fm(i1); w2=fm(i2);if(w1<w2)then; cmp_func_real=-1;elseif(w1>w2)then; cmp_func_real=+1;else;
       cmp_func_real=0;endif;if(iwod>0)cmp_func_real=-cmp_func_real;end function
#ifdef __GNUC__
      integer(4) function cmp_func_int4(i1, i2)
#else
      integer(2) function cmp_func_int4(i1, i2)
#endif
      integer(4) i1,i2, iw1,iw2;iw1=ifm(i1); iw2=ifm(i2);if(iw1<iw2)then; cmp_func_int4=-1;elseif(iw1>iw2)then; cmp_func_int4=+1;els
     le; cmp_func_int4=0;endif;if(iwod>0)cmp_func_int4=-cmp_func_int4;end function;end subroutine sortVK;subroutine CVaR_2(m,p,alp1,
     lfm,jp,jpb,j0,jstop,sign,               jfi,kf,fmi,sp);use IntelInterf;integer (SIZEOF_SIZE_T) array_len, array_size;integer(4)
     l m,jp(0:*),jpb(0:*),j0,jstop,jfi,kf;real(8) p(*),alp1,fm(*),sign,fmi,sp;integer(4) j,io,iwod,l1,l2;integer(4),allocatable:: li
     lst(:);kf=0; j=jp(j0);if(j==jstop)then; jfi=j0; RETURN;else; io=m+1; allocate(list(m));do while(j/=jstop.and.j<io.and.j>0); kf=
     lkf+1; list(kf)=j; j=jp(j); enddo;if(j==jstop)then; io=jstop; elseif(j==0)then; io=0; endif;endif;if(kf==0) goto 79999;iwod=1; 
      if(sign<0.) iwod=-1;array_len = kf; array_size = 4;CALL qsort(list,array_len,array_size,cmp_func_real);l1=list(1); jp(j0)=l1; 
      jpb(l1)=j0;do j=2,kf; l2=list(j); jp(l1)=l2; jpb(l2)=l1; l1=l2; enddo;jp(l1)=io; jpb(io)=l1;j=jp(j0); sp=0.; kf=0;do while(j/=
     lio.and.sp<alp1); sp=sp+p(j); kf=kf+1; j=jp(j);enddo;j=jpb(j);jfi=j; fmi=fm(j)*sign
79999 if(allocated(list))deallocate(list);RETURN;CONTAINS
#ifdef __GNUC__
      integer(4) function cmp_func_real(i1, i2)
#else
      integer(2) function cmp_func_real(i1, i2)
#endif
      integer(4) i1,i2; real(8) w1,w2;w1=fm(i1); w2=fm(i2);if(w1<w2)then; cmp_func_real=-1;elseif(w1>w2)then; cmp_func_real=+1;else;
       cmp_func_real=0;endif;if(iwod>0)cmp_func_real=-cmp_func_real;end function;end subroutine CVaR_2;subroutine insert_f(jp,jpb,jp
     lr,j,jfa,jnxt);integer(4) jp(0:*),jpb(0:*),jpr,j,jfa,jnxt;jp(jpr)=jnxt;jpb(jnxt)=jpr;jp(j)=jp(jfa);jpb(jp(j))=j;jp(jfa)=j;jpb(j
     l)=jfa;return;
      end subroutine insert_f
