      subroutine New_Ordering(t,nz,m,mf,jmin,jmax,avg0,fm,p,pf,
     +jp,jpb);integer(4) nz,m,mf,jp(0:m+1),jpb(0:m+1),jmin,jmax;real(8) t(nz),avg0,fm(*),p(*),pf(*);integer(4) k,k0,i,i1,j,j1,j0,jst
     lop,m1,it,iw,kpmax,file;integer(4),allocatable:: klmn(:),jf(:),jpmin(:),jpmax(:),idel(:);real(8),allocatable:: fi(:),fpi(:),plk
     l(:);real(8)       w,w1,wa,fma,fmi,sign;kpmax=4*nz+1; j1=0;allocate( klmn(kpmax),jf(kpmax+1),jpmin(kpmax),jpmax(kpmax),
     +idel(kpmax),fi(0:kpmax),fpi(0:kpmax),plk(2*nz) );file=30;m1=m+1;k=1; klmn(1)=mf;jf(k)=jp(0); jf(k+1)=m1;jpmin(k)=jmin; jpmax(k
     l)=jmax;fi(0)=0d0; fi(k)=1d0;fpi(0)=0d0; fpi(k)=avg0;j=jpmax(k);if(j.ne.jf(k)) then;call insert_f(jpb,jp,jp(j),j,jf(k),jpb(j));
      jf(k)=j;endif;j=jpmin(k); j0=jp(j);if(j.ne.jpmax(k).and.j0.ne.m1) call insert_f(jpb,jp,j0,j,m1,jpb(j));do it=1,1500;k0=k;do i=
     lk,1,-1; iw=0;w1=fi(i); wa=fi(i-1); w=w1-wa;fma=fm(jpmax(i)); fmi=fm(jpmin(i));if(w<=0d0) cycle;do i1=1,nz;if(t(i1)>wa.and.t(i1
     l)<w1)then; iw=iw+1;plk(iw)=( fma*(w1-t(i1))+fmi*(t(i1)-wa) )/w;endif;enddo;if(iw>0) then;if(fma<=fmi) cycle;i1=iw+1; w=0.5*(fm
     la+fmi);do while(i1>1); if(plk(i1-1)>=w) exit;plk(i1)=plk(i1-1); i1=i1-1;enddo;plk(i1)=w; iw=iw+1;call Add_Polky(i,iw,fm,plk,kp
     lmax,p,pf,  k,jf,jp,jpb,jpmax,jpmin,fi,fpi,klmn);endif;enddo;if(k==k0) EXIT;idel=1; idel(k)=0; w1=fi(0);do i=1,k-1; wa=w1; w1=f
     li(i);do i1=1,nz; w=t(i1);if(wa<w.and.w<fi(i+1))then; idel(i)=0; EXIT;endif;enddo;enddo;call Del_Polky(k,idel,jf,jpmax,jpmin,fi
     l,fpi,kpmax,klmn);enddo;do i=k,1,-1; iw=0;if(fm(jpmin(i))>=fm(jpmax(i))) Cycle;do i1=1,nz;if(t(i1)>fi(i-1).and.t(i1)<fi(i)) the
     ln;iw=iw+1; plk(iw)=t(i1);if(iw==1) then; w=t(i1)-fi(i-1); j1=1;elseif(t(i1)-t(i1-1)>w) then; w=t(i1)-t(i1-1); j1=iw;endif;endi
     lf;enddo;if(iw==0) CYCLE;if(fi(i)-plk(iw)>w) then; j1=iw+1;endif;if(j1>1) then;sign=1d0; j0=jpb(jf(i)); jstop=jf(i+1);CALL CVaR
     l_2(m,p,plk(j1-1)-fi(i-1),fm,jp,jpb,j0,jstop,sign,    k0, i1,wa,w);endif;if(j1<=iw) then;sign=-1d0; j0=jf(i+1); jstop=jpb(jf(i)
     l);CALL CVaR_2(m,p,fi(i)-plk(j1),fm,jpb,jp,j0,jstop,sign,        k0, i1,wa,w);endif;enddo;deallocate( klmn,jf,jpmin,jpmax,idel,
     lfi,fpi,plk, stat=i );return;END subroutine NEW_Ordering;subroutine Del_Polky(k,idel,jf,jpmax,jpmin,fi,fpi,kpmax,klmn);integer(
     l4) k,kpmax, idel(kpmax),
     +jf(kpmax+1),jpmax(kpmax),jpmin(kpmax),klmn(kpmax);real(8)    fi(0:kpmax),fpi(0:kpmax);integer(4) id,i,iw,i1;id=0;do i=1,k;if(i
     ldel(i)>0) then;id=id+1; i1=i+1;jf(i1)=jf(i); jpmax(i1)=jpmax(i);klmn(i1)=klmn(i)+klmn(i1);elseif(id>0) then;iw=i-id;jf(iw)=jf(
     li); jpmax(iw)=jpmax(i);klmn(iw)=klmn(i); jpmin(iw)=jpmin(i);fi(iw)=fi(i); fpi(iw)=fpi(i);endif;enddo;if(id>0) then;jf(k-id+1)=
     ljf(k+1); k=k-id;endif;return;end subroutine Del_Polky;subroutine Add_Polky(i,iw0,fm,plk,kpmax,p,pf,
     +k,jf,jp,jpb,jpmax,jpmin,fi,fpi,klmn);integer(4) k,i,iw0,kpmax,jf(kpmax+1),jp(0:*),jpb(0:*),
     +jpmax(kpmax),jpmin(kpmax),klmn(kpmax);real(8) fm(*),plk(iw0),fi(0:kpmax),fpi(0:kpmax),p(*),pf(*);integer(4) in,i1,id,j,iw;inte
     lger(4),allocatable:: kel(:),jpma(:),jpmi(:);iw=iw0+1;allocate(kel(iw),jpma(iw),jpmi(iw));iw=iw0;call Sorting(jpb(jf(i)),jf(i+1
     l),fm,jp,jpb,plk,1,iw, kel, kel(iw+1), jpma,jpmi );id=iw+1;do i1=1,iw+1; if(kel(i1)<=0) id=id-1;enddo;if(id<=1) then;iw=0; goto
     l 100;endif;id=id-1;jf(k+id+1)=jf(k+1);do in=k,i+1,-1;  i1=in+id;jf(i1)=jf(in); jpmin(i1)=jpmin(in);jpmax(i1)=jpmax(in); klmn(i
     l1)=klmn(in);fi(i1)=fi(in); fpi(i1)=fpi(in);enddo;i1=i-1;do in=1,iw+1; if(kel(in)<=0) Cycle;i1=i1+1;jf(i1)=jpma(in); jpmin(i1)=
     ljpmi(in);jpmax(i1)=jpma(in); klmn(i1)=kel(in);enddo;iw=id;fi(i+iw)=fi(i); fpi(i+iw)=fpi(i);do in=i,i+iw-1; j=jf(in); fi(in)=fi
     l(in-1); fpi(in)=fpi(in-1);do while(j.ne.jf(in+1));fi(in)=fi(in)+p(j); fpi(in)=fpi(in)+pf(j);j=jp(j);enddo;enddo;k=k+iw
100   continue;deallocate(kel,jpma,jpmi);return;end subroutine Add_Polky;subroutine New_Ordering_bPOE(isg,c,nz,m,mf,jmin,jmax,avg0,f
     lm,p,pf,kpmax,
     +jp,jpb,k,jpmin,klmn,fii,fpi);integer(4) nz,m,mf,jp(0:m+1),jpb(0:m+1),jmin,jmax,   kpmax,k,jpmin(*),klmn(*);real(8) c(nz),avg0,
     lfm(*),p(*),pf(*),                 fii(0:*),fpi(0:*);logical isg;integer(4) k0,i,i1,j0,jstop,m1,it,iw,file,j;integer(4),allocat
     lable::jpmax(:),jf(:),idel(:);real(8),allocatable:: plk(:); real(8) w,w1,wa,fma,fmi,sign,w2;allocate( jpmax(kpmax),jf(kpmax+1),
     +idel(kpmax),plk(2*nz) );file=30;m1=m+1;k=1; klmn(1)=mf;jf(k)=jp(0); jf(k+1)=m1;jpmin(k)=jmin; jpmax(k)=jmax;fii(0)=0d0; fii(k)
     l=1d0;fpi(0)=0d0; fpi(k)=avg0;j=jpmax(k);if(j.ne.jf(k))then; call insert_f(jpb,jp,jp(j),j,jf(k),jpb(j));jf(k)=j;endif;j=jpmin(k
     l); j0=jp(j);if(j.ne.jpmax(k).and.j0.ne.m1) call insert_f(jpb,jp,j0,j,m1,jpb(j));do it=1,1500;k0=k;do i=k,1,-1; iw=0;fma=fm(jpm
     lax(i)); fmi=fm(jpmin(i));w1=fii(i); wa=fii(i-1);if(isg)then; w1=1.-w1; wa=1.-wa;if(w1>0)then; w1=(fpi(i)-avg0)/w1; else; w1=-f
     lm(jmin); endif;if(wa>0)then; wa=(fpi(i-1)-avg0)/wa; else; wa=-fm(jmin); endif;else;if(w1>0)then; w1=fpi(i)/w1; else; w1=fm(jma
     lx); endif; w1=-w1;if(wa>0)then; wa=fpi(i-1)/wa; else; wa=fm(jmax); endif; wa=-wa;endif;w=w1-wa;if(w<=0d0) cycle;do i1=1,nz;if(
     lc(i1)>wa.and.c(i1)<w1)then; iw=iw+1;plk(iw)=( fma*(w1-c(i1))+fmi*(c(i1)-wa) )/w;endif;enddo;if(iw>0) then;if(fma<=fmi) cycle;i
     l1=iw+1; w=0.5*(fma+fmi);do while(i1>1); if(plk(i1-1)>=w) exit;plk(i1)=plk(i1-1); i1=i1-1;enddo;plk(i1)=w; iw=iw+1;call Add_Pol
     lky(i,iw,fm,plk,kpmax,p,pf,  k,jf,jp,jpb,jpmax,jpmin,fii,fpi,klmn);endif;enddo;if(k==k0) EXIT;idel=1; idel(k)=0; w1=fii(0);if(i
     lsg)then; w1=-avg0;w2=1.-fii(1); if(w2>0)then; w2=(fpi(1)-avg0)/w2; else; w2=-fm(jmin); endif;else; w1=-fm(jmax);w2=fii(1); if(
     lw2>0)then; w2=fpi(1)/w2; else; w2=fm(jmax); endif; w2=-w2;endif;do i=1,k-1; wa=w1; w1=w2;if(isg)then;w2=1.-fii(i+1); if(w2>0)t
     lhen; w2=(fpi(i+1)-avg0)/w2; else; w2=-fm(jmin); endif;else;w2=fii(i+1); if(w2>0)then; w2=fpi(i+1)/w2; else; w2=fm(jmax); endif
     l; w2=-w2;endif;do i1=1,nz; w=c(i1);if(wa<w.and.w<w2)then; idel(i)=0; EXIT;endif;enddo;enddo;call Del_Polky(k,idel,jf,jpmax,jpm
     lin,fii,fpi,kpmax,klmn);enddo;do i=k,1,-1; iw=0;if(fm(jpmin(i))>=fm(jpmax(i))) Cycle;if(isg)then; w1=1.-fii(i-1); w2=1.-fii(i);
      if(w1>0)then; w1=(fpi(i)-avg0)/w1; else; w1=-fm(jmin); endif;if(w2>0)then; w2=(fpi(i-1)-avg0)/w2; else; w2=-fm(jmin); endif;el
     lse;w1=fii(i-1); if(w1>0)then; w1=fpi(i-1)/w1; else; w1=fm(jmax); endif; w1=-w1;w2=fii(i);   if(w2>0)then; w2=fpi(i)/w2; else; 
      w2=fm(jmax); endif; w2=-w2;endif;do i1=1,nz; if(c(i1)>w1.and.c(i1)<w2) iw=iw+1;enddo;if(iw==0) CYCLE;sign=1d0; j0=jpb(jf(i)); 
      jstop=jf(i+1);CALL CVaR_2(m,p,fii(i)-fii(i-1),fm,jp,jpb,j0,jstop,sign,    k0, i1,wa,w);enddo;deallocate( jpmax,jf,idel,plk, st
     lat=i );return;
      END subroutine NEW_Ordering_bPOE
