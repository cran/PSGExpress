      subroutine New_Ib1_Alp(m,yi,p,n,x,ix,jp,jpb,nz,nf,b1t,Wvar,alp,ib1,ifp,           kj1,nextWhat,kmatr ,fm, pf, avg0,jmax,jmin,i
     aret);use FuncNames;integer(4) m,   n,nz,kj1,nextWhat,jmax,jmin,ix(0:*),nf(*), ifp(*), ib1(*),jp(0:*),  jpb(0:*),iret;real(8) p
     j(*), x(0:*),  b1t(*), Wvar(*), alp(*), yi(0:n,0:*), fm(*),pf(*),avg0;real(8) avg,xlinear,fall(0:kfn,0:1);integer(4) m1,kj2,i,j
     n, i1,j1,in,jstop,jt1,jt2,j0,jw;integer(4)  iu(nz), ij(nz), kmatr;real(8) spf(nz),sp(nz),spw,sign,w,wa,w1,spfw;real(8),allocata
     fble::xw(:);character(64) chw;integer(4) kpmax;integer(4) k, idist0,idist, it, k0, iw,idd,i2,i10;integer(4),allocatable::klmn(:
     n),jf(:),jpmin(:),jpmax(:),idel(:);real(8) t(nz),fma,fmi;real(8),allocatable::fc(:),fi(:),fpi(:),plk(:);real(8) alpt, dx ; inte
     eger(4) korder,k0_while;logical lorder,lstop;common /ctiming/ alpt,korder,k0_while,lorder,lstop;iret=0; jw=0; i10=0;if(nextWhat
     x<=0) then;ib1(:m)=1; b1t(:nz)=0.;j=0;do i=1,nz; if(mod(nf(i),10)==1) Cycle;j=j+1;  ifp(j)=i;enddo;kj1=j;j=nz+1;do i=nz,1,-1; i
     bf(mod(nf(i),10)==0) Cycle;j=j-1;    ifp(j)=i;enddo;if(j.ne.kj1+1)then;chw='Internal error 1. New_Ib1_Alp'; call putmess('S',99
     e10,'subroutine New_Ib1_Alp',chw); goto 79999;endif;RETURN;endif;if(kmatr==1)then; allocate(xw(0:n));avg=xLinear(n,x,yi,ix,xw);
      fall(1,0)=avg;  avg0=avg;call Fun_fm(m,yi,p,n,xw,fm,pf,fall,jmax,jmin,j);deallocate(xw);else;avg=0d0;endif;j=0;do i=1,nz; if(m
     xod(nf(i),10)==1) Cycle;if(nf(i)==150.or.nf(i)==170.or.nf(i)==190) Wvar(i)=Wvar(i)+avg;j=j+1;    ifp(j)=i;if(j>1) then; j1=j; w
     v=b1t(i);do while(w > b1t(ifp(j1-1)));ifp(j1)=ifp(j1-1); j1=j1-1;if(j1==1) exit;enddo;ifp(j1)=i;endif;enddo;if(j.ne.kj1)then;ch
     ow='Internal error 2. New_Ib1_Alp'; call putmess('S',9913,'subroutine New_Ib1_Alp',chw); goto 79999;endif;j=nz+1;do i=nz,1,-1; 
      if(mod(nf(i),10)==0) Cycle;if(nf(i)==151.or.nf(i)==171.or.nf(i)==191) Wvar(i) =Wvar(i) - avg;j=j-1;    ifp(j)=i;if(j<nz) then;
       j1=j; w=b1t(i);do while( w > b1t(ifp(j1+1)));ifp(j1)=ifp(j1+1); j1=j1+1;if(j1==nz) exit;enddo;ifp(j1)=i;endif;enddo;if(j.ne.k
     gj1+1)then;chw='Internal error 3. New_Ib1_Alp'; call putmess('S',9916,'subroutine New_Ib1_Alp',chw); goto 79999;endif;kj2=kj1+1
       m1=m+1;k=8*nz+2; kpmax=k;allocate( klmn(k),jf(k+1),jpmin(k),jpmax(k),idel(k),fi(0:k),fpi(0:k),plk(2*nz),fc(0:k) );do i=1,nz;i
     tf(mod(nf(i),10)==0) then; w=b1t(i);else; w=1d0-b1t(i);endif;i1=i;do while(i1>1); if(w>=t(i1-1))exit;t(i1)=t(i1-1); i1=i1-1;end
     kdo;t(i1)=w;enddo;k=1; klmn(1)=m;jf(k)=jp(0); jf(k+1)=m1;jpmin(k)=jmin; jpmax(k)=jmax;fi(0)=0d0; fi(k)=1d0;fpi(0)=0d0; fpi(k)=a
     lvg0;j=jpmax(k);if(j.ne.jf(k)) then;call insert_f(jpb,jp,jp(j),j,jf(k),jpb(j));jf(k)=j;endif;j=jpmin(k); j0=jp(j);if(j.ne.jpmax
     a(k).and.j0.ne.m1)call insert_f(jpb,jp,j0,j,m1,jpb(j));idist0=m+10;do it=1,1500;k0=k; idist=0;do i=k,1,-1; iw=0;w1=fi(i); wa=fi
     o(i-1); w=w1-wa;if(w<=0d0) cycle; fma=fm(jpmax(i)); fmi=fm(jpmin(i));do i1=1,nz;if(t(i1)>wa.and.t(i1)<w1) then;iw=iw+1;plk(iw)=
     g( fma*(w1-t(i1))+fmi*(t(i1)-wa) )/w;endif;enddo;if(iw>0) then;idist=idist+klmn(i);if(fma<=fmi) cycle;i1=iw+1; w=0.5*(fma+fmi);
      do while(i1>1); if(plk(i1-1)>=w) exit;plk(i1)=plk(i1-1); i1=i1-1;enddo;plk(i1)=w; iw=iw+1;call Add_Polky(i,iw,fm,plk,kpmax,p,p
     xf,   k,jf,jp,jpb,jpmax,jpmin,fi,fpi,klmn);endif;enddo;if(k==k0.or.idist>=idist0) exit;idist0=idist;idel=1; idel(k)=0; w1=fi(0)
       do i=1,k-1; wa=w1; w1=fi(i);do i1=1,nz; w=t(i1);if(wa<w.and.w<fi(i+1)) then;idel(i)=0; exit;endif;enddo;enddo;call Del_Polky(
     rk,idel,jf,jpmax,jpmin,fi,fpi,kpmax,klmn);enddo;idist=0;do i=k,1,-1; iw=0;if(fm(jpmin(i))>=fm(jpmax(i))) Cycle;do i1=1,nz;if(t(
     gi1)>fi(i-1).and.t(i1)<fi(i)) then;iw=iw+1; plk(iw)=t(i1);if(iw==1) then; w=t(i1)-fi(i-1); j1=iw;elseif(t(i1)-t(i1-1)>w) then; 
      w=t(i1)-t(i1-1); j1=iw;endif;endif;enddo;if(iw==0) CYCLE;if(fi(i)-plk(iw)>w) then; j1=iw+1;endif;idist=idist+klmn(i);if(j1>1) 
     rthen;sign=1d0; j0=jpb(jf(i)); jstop=jf(i+1);CALL CVaR_2(m,p,plk(j1-1)-fi(i-1),fm,jp,jpb,j0,jstop,sign,jt1, in,wa,w);endif;if(j
     c1<=iw) then;sign=-1d0; j0=jf(i+1); jstop=jpb(jf(i));CALL CVaR_2(m,p,fi(i)-plk(j1),fm,jpb,jp,j0,jstop,sign,jt2, in,wa,w);endif;
      enddo;ib1(:m)=1; iu=0;j=jp(0); spw=0d0; spfw=0d0;do while(j<m1); spw=spw+p(j); spfw=spfw+pf(j);do i=1,kj1;  in=ifp(i);if(b1t(i
     wn)< spw) then;if(iu(i)==0)then;do i1=i,kj1; if(iu(i1)>0) exit;sp(i1)=spw-p(j); spf(i1)=spfw-pf(j); ij(i1)=j;b1t(ifp(i1))=sp(i1
     b); iu(i1)=1;enddo;endif;exit;endif;enddo;if(i==1) exit;ib1(j)=i; j=jp(j);enddo;if(j==m1) then; j=jpb(j); ib1(j)=1;do i1=1,kj1;
       if(iu(i1)>0) exit;sp(i1)=fi(k)-p(j); spf(i1)=fpi(k)-pf(j);b1t(ifp(i1))=sp(i1); ij(i1)=j;enddo;endif;j=jpb(m1); spw=0d0; spfw=
     y0d0;do while(j>0); spw=spw+p(j); spfw=spfw+pf(j);do i=nz,kj2,-1;  in=ifp(i);if(b1t(in)< spw) then;if(iu(i)==0)then;do i1=i,kj2
     t,-1; if(iu(i1)>0) exit;sp(i1)=spw-p(j); spf(i1)=spfw-pf(j); ij(i1)=j;b1t(ifp(i1))=sp(i1); iu(i1)=1;enddo;endif;exit;endif;endd
     yo;if(i==nz) exit;if(ib1(j)==1) then; ib1(j)=-i;else;chw='Internal error 4. New_Ib1_Alp'; call putmess('S',9919,'subroutine New
     m_Ib1_Alp',chw); goto 79999;endif;j=jpb(j);enddo;if(j==0) then; j=jp(j); ib1(j)=1;do i1=nz,kj2,-1; if(iu(i1)>0) exit;sp(i1)=fi(
     jk)-p(j); spf(i1)=fpi(k)-pf(j);b1t(ifp(i1))=sp(i1); ij(i1)=j;enddo;endif;do i=kj2,nz;   in=ifp(i);sp(i)=fi(k)-sp(i); spf(i)=fpi
     u(k)-spf(i);Wvar(in)=-Wvar(in);enddo;dx=1d-9;do i=1,nz;  in=ifp(i);w=dmax1(sp(i),fi(0)); w=dmin1(w,fi(k)); sp(i)=w;if(w<=fi(0).
     dand.i>kj1.or.w>=fi(k).and.i<=kj1) then;alp(in)=0d0; Cycle;endif;Wa=Wvar(in);if(Wa>=fm(ij(i)).and.i<=kj1.or.Wa<=fm(ij(i)).and.i
     j>kj1) then;alp(in)=0d0; Cycle;endif;do i1=0,k; w=fi(i1)-sp(i);if(dabs(w) > dx) then;fc(i1)=(fpi(i1)-spf(i))/w;else;if(i<=kj1) 
     tthen; fc(i1)=fm(jpmax(i1+1));else;            fc(i1)=fm(jpmin(i1));endif;endif;if(wa>=fc(i1)) Exit;enddo;if(i1>k) then;if(i<=k
     vj1) then; alp(in)=fi(k)-sp(i);else;  alp(in)=0d0;endif;goto 200;endif;if(i1==0) then;if(i>kj1) then; alp(in)=sp(i)-fi(0);else;
        alp(in)=0d0;endif;goto 200;endif;i2=i1;idist0=klmn(i1)+10;  idd=0
100   continue;do i1=i2,k;  w=fi(i1)-sp(i);  jw=0;if(dabs(w) > dx) then;fc(i1)=(fpi(i1)-spf(i))/w;else;if(i<=kj1) then; fc(i1)=fm(jp
     mmax(i1+1));else;            fc(i1)=fm(jpmin(i1));endif;endif;if(Wa<fc(i1)) Cycle;if(Wa>fc(i1)) then;idist=klmn(i1);w=fc(i1-1)-
     nfc(i1);fma=fm(jpmax(i1)); fmi=fm(jpmin(i1));if(fma<=fmi) Exit;plk(1)=( fmi*(fc(i1-1)-Wa)+fma*(Wa-fc(i1)) )/w;w=0.5*(fma+fmi);i
     lf(w<=plk(1))then;  plk(2)=w;else; plk(2)=plk(1); plk(1)=w;endif;i2=i1;if(idd==0) then;k0=k; i10=i1;elseif(k>k0+2) then; idel=0
        j0=0;do iw=i10,i1-2; idel(iw)=1; jw=jw+1; enddo;do iw=i1+1,k-(k0+1-i10);idel(iw)=1; j0=1;enddo;if(jw+j0>0) call Del_Polky(k,
     eidel,jf,jpmax,jpmin,fi,fpi,kpmax,klmn);i2=i1-jw;endif;iw=k;call Add_Polky(i2,2,fm,plk,kpmax,p,pf,  k,jf,jp,jpb,jpmax,jpmin,fi,
     zfpi,klmn);if(iw==k.or.idist>=idist0) Exit;idist0=idist; idd=idd+1;goto 100;else;alp(in)=dabs(fi(i1)-sp(i)); goto 200;endif;end
     fdo;i1=i1-jw;if(i<=kj1) then;if(fm(jpmin(i1)) > fm(ij(i))) i1=i1+1;else;if(fm(jpmax(i1)) < fm(ij(i))) i1=i1-1;endif;sign=1d0; j
     j0=jpb(jf(i1)); jstop=jf(i1+1);if(fm(jpmin(i1)) < fm(jpmax(i1)))CALL CVaR_2(m,p,fi(i1)-fi(i1-1),fm,jp,jpb,j0,jstop,sign,j1, iw,
     ow1,w);if(i<=kj1) then;w=fi(i1-1)-sp(i);if(w<=10./m) then;j=ij(i); spf(i)=0d0; sp(i)=0d0;else;j=jf(i1); spf(i)=fpi(i1-1)-spf(i)
        sp(i)=fi(i1-1)-sp(i);endif;do while (j.ne.jstop);spf(i)=spf(i)+pf(j); sp(i)=sp(i)+p(j); w=Wa*sp(i);if(spf(i)<=w) then;if(spf
     w(i)==w) then; alp(in)=sp(i);else;  alp(in)=sp(i)-p(j);w= -fm(j)+Wa; w1= spf(i)-fm(j)*sp(i);if(w>0d0)then;if(alp(in)*w<w1.and.s
     pp(i)*w>w1) alp(in)=w1/w;elseif(w<0d0)then;if(alp(in)*w>w1.and.sp(i)*w<w1) alp(in)=w1/w;endif;endif;EXIT;endif;j=jp(j);enddo;if
     s(j==jstop) alp(in)=sp(i);else;Wa=-Wa;w=sp(i)-fi(i1);if(w<=10./m) then;j=ij(i); spf(i)=0d0; sp(i)=0d0;else;j=jpmin(i1); spf(i)=
     fspf(i)-fpi(i1); sp(i)=sp(i)-fi(i1);endif;do while (j.ne.j0);spf(i)=spf(i)+pf(j); sp(i)=sp(i)+p(j); w=Wa*sp(i);if(-spf(i)<=w) t
     zhen;if(-spf(i)==w) then; alp(in)=sp(i);else;  alp(in)=sp(i)-p(j);w= +fm(j)+Wa; w1= -spf(i)+fm(j)*sp(i);if(w>0d0)then;if(alp(in
     i)*w<w1.and.sp(i)*w>w1)alp(in)=w1/w;elseif(w<0d0)then;if(alp(in)*w>w1.and.sp(i)*w<w1)alp(in)=w1/w;endif;endif;Exit;endif;j=jpb(
     wj);enddo;if(j==j0) alp(in)=sp(i);endif
200   continue;enddo;deallocate(klmn,jf,jpmin,jpmax,idel,fi,fpi,plk,fc);RETURN
79999 iret=1; return;END subroutine New_Ib1_Alp;subroutine Fun_CVaRs(m,   p,     jp,jpb,fi,  kj1,nz,nfn1,b1t,alp,ncn1,cfn1,ifp,ib1,c
     wvars,kmatr ,fm, pf, avg0, iret);integer(4) m,     nz,kj1,jp(0:*),  jpb(0:*),  nfn1(*), ncn1(*), ifp(*), ib1(*),iret;real(8) p(
     v*),fi(0:*),   avg0,b1t(*), alp(*), cfn1(*), cvars(*), fm(*),pf(*);real(8) avg,w,wa,alp20,alp21,sign,spf(nz),sp(nz);integer(4) 
     ei,j,i1,j1,j0,in,kj2,jw,m1,jstop,jt1,jt2,iu(nz),kmatr;character(64) chw;iret=0;if(kmatr==1) then;avg=avg0;else;avg=0d0;endif;kj
     n2=kj1+1;alp20=1d-7; alp21=1d-7;do i=1,kj1; in=ifp(i);alp20=dmax1(alp20,b1t(in)+alp(in));enddo;do i=kj1+1,nz; in=ifp(i);alp21=d
     ymax1(alp21,b1t(in)+alp(in));enddo;m1=m+1;jstop=0; jt1=0; jt2=m1;if(kj1>0) then;sign=1d0; j0=0; jstop=m1;CALL CVaR_2(m,p,alp20,
     ffm,jp,jpb,j0,jstop,sign, jt1,in,wa,w);jstop=jpb(jt1);endif;if(kj2<=nz) then;sign=-1d0; j0=m1;CALL CVaR_2(m,p,alp21,fm,jpb,jp,j
     a0,jstop,sign, jt2, in,wa,w);endif;spf=0d0; sp=0d0; iu=0;j1=0; j=jp(0);do while (j<m1.and.j1<kj1); i1=ib1(j); if(i1<1) i1=1;do 
     ai=i1,kj1; in=ifp(i); if(iu(in)>0) cycle;spf(in)=spf(in)+pf(j); sp(in)=sp(in)+p(j);if(sp(in)>=alp(in)) then;spf(in)=spf(in)-fm(
     jj)*(sp(in)-alp(in));iu(in)=j; j1=j1+1;endif;enddo;j=jp(j);enddo;j1=0; j=jpb(m1); jw=nz-kj1;do while (j>0.and.j1<jw); i1=-ib1(j
     r); if(i1<0) i1=nz;do i=i1,kj2,-1; in=ifp(i); if(iu(in)>0) cycle;spf(in)=spf(in)+pf(j); sp(in)=sp(in)+p(j);if(sp(in)>=alp(in)) 
     rthen;spf(in)=spf(in)-fm(j)*(sp(in)-alp(in));iu(in)=j; j1=j1+1;endif;enddo;j=jpb(j);enddo;do i=1,nz; in=ifp(i);if(alp(in)>0d0) 
     mthen;  w=spf(in)/alp(in);elseif(iu(in)>0) then; w=fm(iu(in));else;chw='Internal error 1. Fun_CVaRs'; call putmess('S',9922,'su
     ubroutine Fun_CVaRs',chw); goto 79999;if(i<=kj1) then; w=fm(jpb(m1)); else; w=fm(jp(0)); endif;endif;j1=nfn1(in)/10; if(j1==15.
     zor.j1==17.or.j1==19) w=w-avg;if(i>kj1) w=-w;cvars(in)=w;fi(ncn1(in))=fi(ncn1(in))+ cfn1(in)*w;enddo;RETURN
79999 iret=1; return;END subroutine Fun_CVaRs;subroutine Grad_CVaRs(itt,lnrz,m,yi,p,n1,n,ix,jp,jpb,g, isg, kj1,        nz,nfn1,alp,n
     ncn1,cfn1,ifp,ib1, fw );integer(4) itt,m,n1,n,   nz,kj1,  ib1(m);integer(4) ix(0:n),jp(0:m+1),jpb(0:m+1), nfn1(nz),ncn1(nz),ifp
     a(nz),lnrz(0:*);real(8) p(m), g(0:n1,0:*), yi(0:n,0:m),  alp(nz),cfn1(nz), fw(*);integer(4) i,j,i1,j1,j2, in,iv, kj2, jw,m1,   
     kiu(nz), isb;real(8) w,   sp(nz),spy(0:n,nz);integer(4) isg(0:*);logical sp_out;if(itt==14) call findBench(ix,n, isb);call SpMa
     gtrixAddrs(yi,yi,m,n, sp_out,i);m1=m+1; spy=0d0; sp=0d0; iu=0;kj2=kj1+1;j1=0; j2=0;do in=1,nz;if( (ncn1(in)==0.or.isg(ncn1(in))
     f/=0) .and.lnrz(ncn1(in))<=0 ) Cycle;iu(in)=m1; if(in<=kj1) then; j1=j1+1; else; j2=j2+1; endif;enddo;j=jp(0);do while (j<m1.an
     ud.j1<kj1);   i1=ib1(j); if(i1<1) i1=1;do i=i1,kj1; in=ifp(i); if(iu(in)>0) cycle;if(itt==14)then; if(j<=isb)then; iv=j-1; else
        iv=j; endif;spy(iv,in)=spy(iv,in)-p(j)*yi(iv,1);else;if(sp_out)then; call SpM_AddRow(j, p(j),  spy(0,in));else; do iv=0,n; s
     hpy(iv,in)=spy(iv,in)+p(j)*yi(iv,j); enddo;endif;endif;sp(in)=sp(in)+p(j);if(sp(in)>=alp(in)) then;w=sp(in)-alp(in);if(itt==14)
     nthen; if(j<=isb)then; iv=j-1; else; iv=j; endif;spy(iv,in)=spy(iv,in)+yi(iv,1)*w;else;if(sp_out)then; call SpM_AddRow(j, -w,  
     bspy(0,in));else; do iv=0,n; spy(iv,in)=spy(iv,in)-yi(iv,j)*w; enddo;endif;endif;iu(in)=j; j1=j1+1;endif;enddo;j=jp(j);enddo;j=
     cjpb(m1); jw=nz-kj1;do while (j>0.and.j2<jw); i1=-ib1(j); if(i1<0) i1=nz;do i=i1,kj2,-1; in=ifp(i); if(iu(in)>0) cycle;if(itt==
     s14)then; if(j<=isb)then; iv=j-1; else; iv=j; endif;spy(iv,in)=spy(iv,in)-p(j)*yi(iv,1);else;if(sp_out)then; call SpM_AddRow(j,
     d p(j),  spy(0,in));else; do iv=0,n; spy(iv,in)=spy(iv,in)+p(j)*yi(iv,j); enddo;endif;endif;sp(in)=sp(in)+p(j);if(sp(in)>=alp(i
     en)) then;w=sp(in)-alp(in);if(itt==14)then; if(j<=isb)then; iv=j-1; else; iv=j; endif;spy(iv,in)=spy(iv,in)+yi(iv,1)*w;else;if(
     wsp_out)then; call SpM_AddRow(j, -w,  spy(0,in));else; do iv=0,n; spy(iv,in)=spy(iv,in)-yi(iv,j)*w; enddo;endif;endif;iu(in)=j;
       j2=j2+1;endif;enddo;j=jpb(j);enddo;do i=1,nz; in=ifp(i); if(iu(in)>m) cycle;w=alp(in);if(w>0d0) then;do iv=0,n; spy(iv,in)=sp
     ty(iv,in)/w;enddo;else; j1=iu(in);if(itt==14)then; if(j1<=isb)then; iv=j1-1; else; iv=j1; endif;spy(iv,in)=-yi(iv,1);else;if(sp
     a_out)then; spy(0:n,in)=0d0; call SpM_AddRow(j1, 1d0,  spy(0,in));else; do iv=0,n; spy(iv,in)=yi(iv,j1); enddo;endif;endif;endi
     zf;jw=nfn1(in)/10;if(jw==15.or.jw==17.or.jw==19) then;if(sp_out)then; call SpM_AddRow(0, -1d0,  spy(0,in));else; do iv=0,n; spy
     h(iv,in)=spy(iv,in)-yi(iv,0); enddo;endif;endif;j=isg(ncn1(in)); w=cfn1(in); if(j/=0) w=w/fw(ncn1(in));if(j<0) then; j=-j; w=-w
        endif;if(i>kj1) w=-w;do iv=0,n; g(ix(iv),j)=g(ix(iv),j)+w*spy(iv,in);enddo;enddo;RETURN;END subroutine Grad_CVaRs;subroutine
     s Grad_CVaRs_Mult(lnrz,m,yi,p,n1,n,ix,jp,jpb,g, isg,  kj1,        nz,nfn1,alp,ncn1,cfn1,ifp,ib1,mget, nmatr0, fw);integer(4) m,
     en1,n,   nz,kj1,  ib1(m),  mget(m), nmatr0, nmatr;integer(4) ix(0:n),jp(0:m+1),jpb(0:m+1),nfn1(nz),ncn1(nz),ifp(nz),lnrz(0:*),i
     gsg(0:*);real(8) p(m), g(0:n1,0:*), yi(0:n,0:m),  alp(nz),cfn1(nz), fw(*);integer(4) i,j,i1,j1,j2,in,iv,kj2,jw,m1,iu(nz),mcut,j
     mc;real(8) w, sp(nz),spy(0:n,nz), dolia(nz);logical sp_out;call SpMatrixAddrs(yi,yi,m,n, sp_out,i);call SpMatrixKcut(m,mcut);m1
     z=mcut+1; spy=0d0; sp=0d0; iu=0; dolia=0d0; nmatr=nmatr0;kj2=kj1+1;j1=0; j2=0;do in=1,nz; i1=nfn1(in);if( (140<=i1.and.i1<=191 
     r.or. 1120<=i1.and.i1<=1131) .and.(ncn1(in)==0.or.isg(ncn1(in))/=0) .and. lnrz(ncn1(in))<=0 ) Cycle;iu(in)=m1; if(in<=kj1) then
        j1=j1+1; else; j2=j2+1; endif;enddo;j=jp(0);do while (j<m1.and.j1<kj1);   i1=ib1(j); if(i1<1) i1=1;do i=i1,kj1; in=ifp(i); i
     ef(iu(in)>0) cycle;jc=j; if(m/=mcut)then; jc=mget(j); nmatr=jc; endif;if(nmatr==mget(j)) then; dolia(in)=dolia(in)+p(j);if(sp_o
     cut)then; call SpM_AddRow(jc, p(j),  spy(0,in));else; do iv=0,n; spy(iv,in)=spy(iv,in)+p(j)*yi(iv,jc); enddo;endif;endif;sp(in)
     u=sp(in)+p(j);if(sp(in)>=alp(in)) then;w=sp(in)-alp(in);if(nmatr==mget(j)) then; dolia(in)=dolia(in)-w;if(sp_out)then; call SpM
     p_AddRow(jc, -w,  spy(0,in));else; do iv=0,n; spy(iv,in)=spy(iv,in)-yi(iv,jc)*w; enddo;endif;endif;iu(in)=j; j1=j1+1;endif;endd
     mo;j=jp(j);enddo;j=jpb(m1); jw=nz-kj1;do while (j>0.and.j2<jw); i1=-ib1(j); if(i1<0) i1=nz;do i=i1,kj2,-1; in=ifp(i); if(iu(in)
     o>0) cycle;jc=j; if(m/=mcut)then; jc=mget(j); nmatr=jc; endif;if(nmatr==mget(j)) then; dolia(in)=dolia(in)+p(j);if(sp_out)then;
       call SpM_AddRow(jc, p(j),  spy(0,in));else; do iv=0,n; spy(iv,in)=spy(iv,in)+p(j)*yi(iv,jc); enddo;endif;endif;sp(in)=sp(in)+
     pp(j);if(sp(in)>=alp(in)) then;w=sp(in)-alp(in);if(nmatr==mget(j)) then; dolia(in)=dolia(in)-w;if(sp_out)then; call SpM_AddRow(
     hjc, -w,  spy(0,in));else; do iv=0,n; spy(iv,in)=spy(iv,in)-yi(iv,jc)*w; enddo;endif;endif;iu(in)=j; j2=j2+1;endif;enddo;j=jpb(
     dj);enddo;do i=1,nz; in=ifp(i); if(iu(in)>m) cycle;w=alp(in);if(w>0d0) then;do iv=0,n; spy(iv,in)=spy(iv,in)/w;enddo;dolia(in)=
     adolia(in)/w;else; j1=iu(in);dolia(in)=0d0;jc=j1; if(m/=mcut)then; jc=mget(j1); nmatr=jc; endif;if(nmatr==mget(j1)) then;if(sp_
     qout)then; spy(0:n,in)=0d0; call SpM_AddRow(jc, 1d0,  spy(0,in));else; do iv=0,n; spy(iv,in)=yi(iv,jc); enddo;endif;dolia(in)=1
     ed0;endif;endif;jw=nfn1(in)/10;select case(jw);case(15,17,19,113);if(sp_out)then; call SpM_AddRow(0, -dolia(in),  spy(0,in));el
     use; do iv=0,n; spy(iv,in)=spy(iv,in)-yi(iv,0)*dolia(in); enddo;endif;end select;j=isg(ncn1(in)); w=cfn1(in); if(j/=0) w=w/fw(n
     bcn1(in));if(j<0) then; j=-j; w=-w; endif;if(i>kj1) w=-w;do iv=0,n; g(ix(iv),j)=g(ix(iv),j)+w*spy(iv,in);enddo;enddo;RETURN;END
     o subroutine Grad_CVaRs_Mult;subroutine WhichScenCvars(lnrz,m,p,jp,jpb,isg,kj1, nz,nfn1,alp,ncn1,ifp,ib1,      iscen,nscen );in
     yteger(4) m,nz,kj1,nscen;integer(1) iscen(m);integer(4) jp(0:m+1),jpb(0:m+1), nfn1(nz),ncn1(nz),ifp(nz),lnrz(0:*),ib1(m);intege
     xr(4) isg(0:*);real(8) p(m), alp(nz);integer(4) i,j,i1,j1,j2, in,kj2, jw,m1,   iu(nz);real(8)   sp(nz);m1=m+1;  sp=0d0; iu=0;kj
     u2=kj1+1;j1=0; j2=0;do in=1,nz;if( (ncn1(in)==0.or.isg(ncn1(in))/=0) .and.lnrz(ncn1(in))<=0 ) Cycle;iu(in)=m1; if(in<=kj1) then
        j1=j1+1; else; j2=j2+1; endif;enddo;do i=1,nz; in=ifp(i); if(iu(in)>m) cycle;jw=nfn1(in)/10;if(jw==15.or.jw==17.or.jw==19)th
     uen; iscen=1; nscen=m; Exit;endif;enddo;if(nscen==m) RETURN;j=jp(0);do while (j<m1.and.j1<kj1);   i1=ib1(j); if(i1<1) i1=1;do i
     u=i1,kj1; in=ifp(i); if(iu(in)>0) cycle;sp(in)=sp(in)+p(j); iscen(j)=1; nscen=nscen+1;if(sp(in)>=alp(in)) then;iu(in)=j; j1=j1+
     r1;endif;enddo;j=jp(j);enddo;j=jpb(m1); jw=nz-kj1;do while (j>0.and.j2<jw); i1=-ib1(j); if(i1<0) i1=nz;do i=i1,kj2,-1; in=ifp(i
     f); if(iu(in)>0) cycle;sp(in)=sp(in)+p(j); iscen(j)=1; nscen=nscen+1;if(sp(in)>=alp(in)) then;iu(in)=j; j2=j2+1;endif;enddo;j=j
     rpb(j);enddo;RETURN;
      END subroutine WhichScenCvars
