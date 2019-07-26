      subroutine Grad_Draw_Mult(m,mfull,yi,n1,n,ix,jp,jpb, nz,nf,wf,nc,cf,g,isg, polka0,klast0, mget, nmatr, kmatr, jmax,jmin, fw );
      integer(4) m,mfull,n1,n,    nz, mget(*), nmatr, kmatr, jmax,jmin;integer(4) klast0(nz); real(8) polka0(nz);integer(4) ix(0:n),
     jjp(0:*),jpb(0:*), nf(nz),nc(nz),klast(nz);real(8) g(0:n1,0:*), yi(0:n,0:*), cf(nz),polka(nz),wf(nz), fw(*);integer(4) mm,mm1,i
     n,j,j0,j1,j2,jk,kf,ik,iz,k,kpl, io, jd;integer(4),allocatable:: iw(:),niz(:);real(8) w,w1,wc,pj; real(8),allocatable:: sp(:),wp
     v(:);logical sp_out;integer(4) isg(0:*);io=0;call SpMatrixAddrs(yi,yi,mfull,n, sp_out,i);mm=m*kmatr; mm1=mm+1;j0=m*(nmatr-1); j
     pk=m*nmatr+1;pj=1d0/dfloat(mm);allocate(sp(nz),niz(nz));kpl=0;do iz=1,nz;if(.not.(nc(iz)==0.or.isg(nc(iz))/=0)) Cycle;select ca
     xse(nf(iz));case(230,240,231,241);klast(iz)=mm; polka(iz)=0; sp(iz)=1d0;case(250,260,251,261);klast(iz)=mm1; polka(iz)=pj; sp(i
     nz)=pj;case(211,221);klast(iz)=mm-klast0(iz)-1;polka(iz)=pj-polka0(iz);sp(iz)=wf(iz);case(210,220);klast(iz)=klast0(iz);polka(i
     az)=polka0(iz);sp(iz)=1d0-wf(iz);case default;Cycle;end select;kpl=kpl+1; j=kpl;  i=klast(iz);do while(j.gt.1); if(i.ge.klast(j
     q-1))Exit;klast(j)=klast(j-1); niz(j)=niz(j-1);j=j-1;enddo;klast(j)=i; niz(j)=iz;io=mod(nf(iz),10);enddo;if(kpl==0) goto 500;al
     xlocate(iw(m),wp(m));  wp=0d0; iw=0; kf=0;if(io==0) then; j1=jp(0);do k=1,kpl; ik=klast(k);if(ik>mm)then; iw=0; wp=0d0; kf=ik; 
      j1=jmax; endif;wc=cf(niz(k))/sp(niz(k));io=isg(nc(niz(k))); if(io<0) then; io=-io; wc=-wc; endif;if(io/=0) wc=wc/fw(nc(niz(k))
     r);do while(.true.); j=j1-j0;if(kf==ik) then;do j2=1,m; if(iw(j2)<=0) cycle;wp(j2)=dfloat(iw(j2))*pj;enddo;if(j0<j1.and.j1<jk) 
     othen;w1=polka((niz(k)));do j2=mget(j1),j; wp(j2)=wp(j2)+w1;enddo;endif;jd=0; if(m<mfull) jd=j0;do j2=1,m; if(wp(j2)<=0d0) cycl
     we; w=wp(j2)*wc;if(sp_out)then; call SpM_GradAddRow(j2+jd,-w,ix,  g(0,io));else; do i=0,n; g(ix(i),io)=g(ix(i),io)-w*yi(i,j2+jd
     c); enddo;endif;enddo;goto 200;endif;if(j0<j1.and.j1<jk) then;do j2=mget(j1),j; iw(j2)=iw(j2)+1;enddo;endif;j1=jp(j1); kf=kf+1;
      enddo
200   enddo;else;j1=jpb(mm1);do k=1,kpl; ik=klast(k);if(ik>mm)then; iw=0; wp=0d0; kf=ik; j1=jmin ;endif;wc=cf(niz(k))/sp(niz(k));io=
     tisg(nc(niz(k))); if(io<0) then; io=-io; wc=-wc; endif;if(io/=0) wc=wc/fw(nc(niz(k)));wc=-wc;do while(.true.); j=j1-j0;if(kf==i
     kk) then;do j2=1,m; if(iw(j2)<=0) cycle;wp(j2)=dfloat(iw(j2))*pj;enddo;if(j0<j1.and.j1<jk) then;w1=polka((niz(k)));do j2=mget(j
     p1),j; wp(j2)=wp(j2)+w1;enddo;endif;jd=0; if(m<mfull) jd=j0;do j2=1,m; if(wp(j2)<=0d0) cycle; w=wp(j2)*wc;if(sp_out)then; call 
     iSpM_GradAddRow(j2+jd,-w,ix,  g(0,io));else; do i=0,n; g(ix(i),io)=g(ix(i),io)-w*yi(i,j2+jd); enddo;endif;enddo;goto 400;endif;
      if(j0<j1.and.j1<jk) then;do j2=mget(j1),j; iw(j2)=iw(j2)+1;enddo;endif;j1=jpb(j1); kf=kf+1;enddo
400   enddo;endif
500   deallocate(sp,niz);if(allocated(iw))deallocate(iw,wp);return;end subroutine Grad_Draw_Mult;integer(4) function MCutGet(m,n,yi)
       integer(4) m,n; real(8) yi(0:n,0:*);logical sp_out; integer(4) i,mcut;call SpMatrixAddrs(yi,yi,m,n, sp_out,i);call SpMatrixKc
     uut(m,mcut);MCutGet=mcut;
      end function MCutGet
