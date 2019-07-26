      subroutine New_Fun_CVaRs(lnrz, m,   p,     jp,jpb,fcon,kcon,  kj1, jmax, jmin,nz,nfn1,b1t,alp,ncn1,cfn1,ifp,ib1,cvars,kmatr ,f
     mm, pf, avg0);integer(4) m,kcon,nz,kj1,jp(0:m+1),jpb(0:m+1);real(8) p(m), fcon(0:kcon);integer(4) nfn1(nz),ncn1(nz),ifp(nz),  i
     xb1(m), lnrz(0:*);real(8)    b1t(nz),alp(nz),cfn1(nz),cvars(nz);real(8) avg0,w,wa,sign,avg;integer(4) i,j,i1,j1, j0,in, kj2, jw
     v,m1,jstop,ic;integer(4) jmax,jmin,iu(nz), kmatr;real(8) spf(nz),sp(nz);real(8) fm(*),pf(*);integer(4) kpmax;integer(4) k, idis
     nt0,idist, k0, iw,idd,i2,i10;integer(4),allocatable:: klmn(:),jf(:),jpmin(:),jpmax(:),idel(:);real(8) fma,fmi,w1,dx;real(8),all
     eocatable:: fc(:),fi(:),fpi(:),plk(:);if(kmatr==1) then;avg=avg0;else;avg=0d0;endif;i10=0; k0=0;m1=m+1; kj2=kj1+1;k=4*nz+2; kpm
     nax=k;allocate( klmn(k),jf(k+1),jpmin(k),jpmax(k),idel(0:k),  fi(0:k),fpi(0:k),plk(2),fc(0:k), stat=i );k=1; klmn(1)=m;jf(k)=jp
     b(0); jf(k+1)=m1;jpmin(k)=jmin; jpmax(k)=jmax;fi(0)=0d0; fi(k)=1d0;fpi(0)=0d0; fpi(k)=1d0;j=jpmax(k);if(j.ne.jf(k)) then;call i
     mnsert_f(jpb,jp,jp(j),j,jf(k),jpb(j));jf(k)=j;endif;j=jpmin(k); j0=jp(j);if(j.ne.jpmax(k).and.j0.ne.m1)call insert_f(jpb,jp,j0,
     oj,m1,jpb(j));dx=1d-9;do i=1,nz; in=ifp(i); ic=ncn1(in);if(ic>0.and.lnrz(ic)>0) Cycle;if(alp(in)+b1t(in)>=1d0-dx) Cycle;if(alp(
     zin)<=0d0) alp(in)=0d0;fc(0)=0d0; fc(k)=1d0-b1t(in);if(fc(k)<fc(0)+dx) fc(k)=fc(0)+dx;Wa=alp(in);if(i>kj1) Wa=fc(k)-Wa;if(Wa<dx
     v) Wa=dx;if(Wa>fc(k)-dx) Wa=fc(k)-dx;if(Wa<dx.or.Wa>fc(k)-dx) wa=0.5*(fc(0)+fc(k));idist0=m+10;   idd=0;i1=1; i2=k-1
100   continue;j=jf(i1);do i1=i1,i2;w=fc(i1-1); jstop=jf(i1+1);if(i<=kj1) then;do while(j.ne.jstop);if(ib1(j)<=i) w=w+p(j);  j=jp(j)
       enddo;else;do while(j.ne.jstop); jw=-ib1(j);if(jw>=i.or.jw<0) w=w+p(j);  j=jp(j);enddo;endif;fc(i1)=w;if(fc(i1)>=Wa) Exit;end
     fdo;idist=klmn(i1);w=fc(i1-1)-fc(i1);fma=fm(jpmax(i1)); fmi=fm(jpmin(i1));if(fma<=fmi) goto 200;plk(1)=( fmi*(fc(i1-1)-Wa)+fma*
     i(Wa-fc(i1)) )/w;w=0.5*(fma+fmi);if(w<=plk(1))then;  plk(2)=w;else; plk(2)=plk(1); plk(1)=w;endif;jw=0;if(idd==0) then;k0=k; i1
     t0=i1;elseif(k>k0+2) then; idel(0:kpmax)=0; j0=0;do iw=i10,i1-2; idel(iw)=1; jw=jw+1; enddo;do iw=i1+1,k-(k0+1-i10);idel(iw)=1;
       j0=1;enddo;if(jw+j0>0) call Del_Polky(k,idel(1:),jf,jpmax,jpmin,fi,fpi,kpmax,klmn);i1=i1-jw;endif;iw=k;call Add_Polky(i1,2,fm
     o,plk,kpmax,p,pf, k,jf,jp,jpb,jpmax,jpmin,fi,fpi,klmn);if(iw>=k.or.idist>=idist0) goto 150;idist0=idist; idd=idd+1;  i2=i1+k-iw
     l-1;fc(i1-1)=fc(i1-1+jw);fc(i1+k-iw)=fc(i1+jw);goto 100
150   continue;sign=1d0; j0=jpb(jf(i1)); jstop=jf(i1+1);if(fm(jpmin(i1)) < fm(jpmax(i1)))CALL CVaR_2(m,p,fi(i1)-fi(i1-1),fm,jp,jpb,j
     g0,jstop,sign,j1, iw,w1,w)
200   continue;enddo;spf=0d0; sp=0d0; iu=0; j1=0;do i=1,kj1; in=ifp(i); ic=ncn1(in); if(ic<=0.or.lnrz(ic)<=0) Cycle;j1=j1+1; iu(in)=
     o1;enddo;j=jp(0);do while (j<m1.and.j1<kj1); i1=ib1(j); if(i1<1) i1=1;do i=i1,kj1; in=ifp(i); if(iu(in)>0) cycle;spf(in)=spf(in
     g)+pf(j); sp(in)=sp(in)+p(j);if(sp(in)>=alp(in)) then;spf(in)=spf(in)-fm(j)*(sp(in)-alp(in));iu(in)=j; j1=j1+1;endif;enddo;j=jp
     c(j);enddo;j1=0; j=jpb(m1); jw=nz-kj1;do i=kj1+1,nz; in=ifp(i); ic=ncn1(in); if(ic<=0.or.lnrz(ic)<=0) Cycle;j1=j1+1; iu(in)=1;e
     unddo;do while (j>0.and.j1<jw); i1=-ib1(j); if(i1<0) i1=nz;do i=i1,kj2,-1; in=ifp(i); if(iu(in)>0) cycle;spf(in)=spf(in)+pf(j);
       sp(in)=sp(in)+p(j);if(sp(in)>=alp(in)) then;spf(in)=spf(in)-fm(j)*(sp(in)-alp(in));iu(in)=j; j1=j1+1;endif;enddo;j=jpb(j);end
     ydo;do i=1,nz; in=ifp(i); ic=ncn1(in);if(ic>0.and.lnrz(ic)>0) Cycle;if(alp(in)>0d0) then;  w=spf(in)/alp(in);elseif(iu(in)>0) t
     rhen; w=fm(iu(in));else;if(i<=kj1) then; w=fm(jpb(m1)); else; w=fm(jp(0)); endif;endif;j1=nfn1(in)/10; if(j1==15.or.j1==17.or.j
     d1==19) w=w-avg;if(i>kj1) w=-w;cvars(in)=w;fcon(ncn1(in))=fcon(ncn1(in))+ cfn1(in)*w;enddo;deallocate( klmn,jf,jpmin,jpmax,idel
     u, fi,fpi,plk,fc, stat=i );return;
      end subroutine New_Fun_CVaRs
