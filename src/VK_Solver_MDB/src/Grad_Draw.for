      subroutine Grad_Draw_Mult(m,mfull,yi,n1,n,ix,jp,jpb, nz,nf,wf,nc,cf,
     +g,
     +isg, polka0,klast0, mget, nmatr, kmatr, jmax,jmin, fw );integer(4) m,mfull,n1,n,    nz, mget(*), nmatr, kmatr, jmax,jmin;integ
     ler(4) klast0(nz); real(8) polka0(nz);integer(4) ix(0:n),jp(0:*),jpb(0:*), nf(nz),nc(nz),klast(nz);real(8) g(0:n1,0:*), yi(0:n,
     l0:*), cf(nz),polka(nz),wf(nz), fw(*);integer(4) mm,mm1,i,j,j0,j1,j2,jk,kf,ik,iz,k,kpl, io, jd;integer(4),allocatable:: iw(:),n
     liz(:);real(8) w,w1,wc,pj; real(8),allocatable:: sp(:),wp(:);logical sp_out;integer(4) isg(0:*);io=0;call SpMatrixAddrs(yi,yi,m
     lfull,n, sp_out,i);mm=m*kmatr; mm1=mm+1;j0=m*(nmatr-1); jk=m*nmatr+1;pj=1d0/dfloat(mm);allocate(sp(nz),niz(nz));kpl=0;do iz=1,n
     lz;if(.not.(nc(iz)==0.or.isg(nc(iz))/=0)) Cycle;select case(nf(iz));case(230,240,231,241);klast(iz)=mm; polka(iz)=0; sp(iz)=1d0
      case(250,260,251,261);klast(iz)=mm1; polka(iz)=pj; sp(iz)=pj;case(211,221);klast(iz)=mm-klast0(iz)-1;polka(iz)=pj-polka0(iz);s
     lp(iz)=wf(iz);case(210,220);klast(iz)=klast0(iz);polka(iz)=polka0(iz);sp(iz)=1d0-wf(iz);case default;Cycle;end select;kpl=kpl+1
     l; j=kpl;  i=klast(iz);do while(j.gt.1); if(i.ge.klast(j-1))Exit;klast(j)=klast(j-1); niz(j)=niz(j-1);j=j-1;enddo;klast(j)=i; n
     liz(j)=iz;io=mod(nf(iz),10);enddo;if(kpl==0) goto 500;allocate(iw(m),wp(m));  wp=0d0; iw=0; kf=0;if(io==0) then; j1=jp(0);do k=
     l1,kpl; ik=klast(k);if(ik>mm)then; iw=0; wp=0d0; kf=ik; j1=jmax; endif;wc=cf(niz(k))/sp(niz(k));io=isg(nc(niz(k))); if(io<0) th
     len; io=-io; wc=-wc; endif;if(io/=0) wc=wc/fw(nc(niz(k)));do while(.true.); j=j1-j0;if(kf==ik) then;do j2=1,m; if(iw(j2)<=0) cy
     lcle;wp(j2)=dfloat(iw(j2))*pj;enddo;if(j0<j1.and.j1<jk) then;w1=polka((niz(k)));do j2=mget(j1),j; wp(j2)=wp(j2)+w1;enddo;endif;
      jd=0; if(m<mfull) jd=j0;do j2=1,m; if(wp(j2)<=0d0) cycle; w=wp(j2)*wc;if(sp_out)then; call SpM_GradAddRow(j2+jd,-w,ix,  g(0,io
     l));else; do i=0,n; g(ix(i),io)=g(ix(i),io)-w*yi(i,j2+jd); enddo;endif;enddo;goto 200;endif;if(j0<j1.and.j1<jk) then;do j2=mget
     l(j1),j; iw(j2)=iw(j2)+1;enddo;endif;j1=jp(j1); kf=kf+1;enddo
200   enddo;else;j1=jpb(mm1);do k=1,kpl; ik=klast(k);if(ik>mm)then; iw=0; wp=0d0; kf=ik; j1=jmin ;endif;wc=cf(niz(k))/sp(niz(k));io=
     lisg(nc(niz(k))); if(io<0) then; io=-io; wc=-wc; endif;if(io/=0) wc=wc/fw(nc(niz(k)));wc=-wc;do while(.true.); j=j1-j0;if(kf==i
     lk) then;do j2=1,m; if(iw(j2)<=0) cycle;wp(j2)=dfloat(iw(j2))*pj;enddo;if(j0<j1.and.j1<jk) then;w1=polka((niz(k)));do j2=mget(j
     l1),j; wp(j2)=wp(j2)+w1;enddo;endif;jd=0; if(m<mfull) jd=j0;do j2=1,m; if(wp(j2)<=0d0) cycle; w=wp(j2)*wc;if(sp_out)then; call 
     lSpM_GradAddRow(j2+jd,-w,ix,  g(0,io));else; do i=0,n; g(ix(i),io)=g(ix(i),io)-w*yi(i,j2+jd); enddo;endif;enddo;goto 400;endif;
      if(j0<j1.and.j1<jk) then;do j2=mget(j1),j; iw(j2)=iw(j2)+1;enddo;endif;j1=jpb(j1); kf=kf+1;enddo
400   enddo;endif
500   deallocate(sp,niz);if(allocated(iw))deallocate(iw,wp);return;end subroutine Grad_Draw_Mult;integer(4) function MCutGet(m,n,yi)
      integer(4) m,n; real(8) yi(0:n,0:*);logical sp_out; integer(4) i,mcut;call SpMatrixAddrs(yi,yi,m,n, sp_out,i);call SpMatrixKcu
     lt(m,mcut);MCutGet=mcut;
      end function MCutGet
