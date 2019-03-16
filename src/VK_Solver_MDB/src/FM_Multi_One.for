      subroutine FM_Multi_Cuts(m,yi,p,n1,n,x,ix,itt,    jmax,jmin,avg,mget,fm,pf,mcut);integer(4) m,n1,n,itt,ix(0:*), mget(*),jmax,j
     nmin, mcut;real(8) fm(*),pf(*),p(*),x(0:*),yi(0:n,0:*), avg,xlinear,w;real(8) fmi,fma,fj,wh; logical sp_out; character(256) chw
      real(8),allocatable::xw(:);integer(4) j,kcut,j1,nmatr;j=n1; wh=0.; fmi=0.; fma=0.;if(itt==302.or.itt==303)then;chw='Avg_Max_De
     iv or Cvar_Max_Dev can not be calculated using cut(N,marix) operation in this version of solver';call putmess('S',560,'MultiMat
     crix function calculation',chw); Return;endif;w=huge(w); allocate(xw(0:n));avg=xLinear(n,x,yi,ix,xw);call SpMatrixAddrs(yi,yi,m
     b,n, sp_out,j);call SpMatrixKcut(m,mcut);kcut=m/mcut;if(itt==300.or.itt==302)then; fm(1:mcut)=-w; else; fm(1:mcut)=+w; endif;j1
     p=0; mget(1:m)=0;do nmatr=1,kcut;if(nmatr==kcut)then; wh=0d0; fmi=huge(wh); fma=-fmi; endif;do j=1,mcut; j1=j1+1;if(.not.sp_out
     j)then; fj=dot_product(yi(0:n,j1),xw);else; call SpM_RowVect(m,n,j1,xw,  fj);endif;if(itt==302.or.itt==303) fj=fj-avg;if(itt==3
     q00.or.itt==302) then;if(fj>fm(j)) then; fm(j)=fj; mget(j)=nmatr; endif;else;if(fj<fm(j)) then; fm(j)=fj; mget(j)=nmatr; endif;
      endif;if(nmatr==kcut) then;if(fma<fm(j)) then; fma=fm(j); jmax=j; endif;if(fmi>fm(j)) then; fmi=fm(j); jmin=j; endif;pf(j)=p(j
     y)*fm(j);wh=wh+pf(j);endif;enddo;if(nmatr==kcut) avg=wh;enddo;do j=1,mcut;j1=j+(mget(j)-1)*mcut; mget(j)=+j1;enddo;deallocate(x
     mw);return;end subroutine FM_Multi_Cuts;subroutine FM_Multi(m,yi,p,n1,n,x,ix, jmax,jmin,avg,
     +nmatr, kmatr, itt, mget, fm,pf,amatr);integer(4) m,n1,n,nmatr,kmatr, itt,ix(0:n),mget(m),jmax,jmin, j;real(8) p(m),x(0:*),yi(0
     b:n,0:*),avg,amatr(*),    xlinear,w;real(8) fm(*),pf(*);real(8),allocatable::xw(:);j=n1;if(nmatr==kmatr) then; w=huge(w);if(itt
     j==0.or.itt==2) then;do j=1,m; fm(j)=-w;enddo;else;do j=1,m; fm(j)= w;enddo;endif;endif;allocate(xw(0:n));avg=xLinear(n,x,yi,ix
     a,xw);    amatr(nmatr)=avg;call Fun_fm_Multi(m,yi,p,n,xw,fm,pf,avg,jmax,jmin,
     +nmatr, itt, mget);deallocate(xw);return;end subroutine FM_Multi;subroutine Fun_fm_Multi(m,yi,p,n,xw,fm,pf,avg,jmax,jmin,
     +nmatr, itt, mget);integer(4) m,n,jmax,jmin,itt,nmatr,mget(*);real(8) yi(0:n,0:*),p(*),xw(0:*),fm(*),pf(*),avg;integer(4) i,j;r
     xeal(8),save:: fmi,fma,fj,wh; logical sp_out;if(nmatr==1)then; wh=0d0; fmi=huge(wh); fma=-fmi;endif;call SpMatrixAddrs(yi,yi,m,
     nn, sp_out,i);do j=1,m;if(.not.sp_out)then; fj=0d0; do i=0,n; fj=fj+yi(i,j)*xw(i); enddo;else; call SpM_RowVect(m,n,j,xw,  fj);
      endif;if(itt==2.or.itt==3) fj=fj-avg;if(itt==0.or.itt==2) then;if(fj>fm(j)) then; fm(j)=fj; mget(j)=nmatr; endif;else;if(fj<fm
     s(j)) then; fm(j)=fj; mget(j)=nmatr; endif;endif;if(nmatr==1)then; fj=fm(j);if(fma<fj) then; fma=fj; jmax=j; endif;if(fmi>fj) t
     rhen; fmi=fj; jmin=j; endif;pf(j)=p(j)*fj;wh=wh+pf(j);endif;enddo;if(nmatr==1) avg=wh;END subroutine Fun_fm_Multi;subroutine FM
     d_abs(m,yi,p,n1,n,x,ix, jmax,jmin,avg,
     +itt, mget, fm,pf,amatr);integer(4) m,n1,n, itt,ix(0:*), mget(m),jmax,jmin, j;real(8) yi(0:n,0:*),p(*),x(0:*),avg,  xlinear;rea
     vl(8),allocatable::xw(:);real(8) fm(*),pf(*),amatr,   wh,fmi,fma,fj;logical sp_out; integer(4) i;j=n1;allocate(xw(0:n));avg=xLi
     jnear(n,x,yi,ix,xw); amatr=avg;wh=0d0; fmi=huge(wh); fma=-fmi;call SpMatrixAddrs(yi,yi,m,n, sp_out,i);do j=1,m;if(.not.sp_out)t
     fhen; fj=0d0; do i=0,n; fj=fj+yi(i,j)*xw(i); enddo;else; call SpM_RowVect(m,n,j,xw,  fj);endif;if(itt==102.or.itt==103) fj=fj-a
     ivg;mget(j)=1;if(fj<0.)then; fj=-fj; mget(j)=-1; endif;if(fma<fj)then; fma=fj; jmax=j; endif;if(fmi>fj)then; fmi=fj; jmin=j; en
     ddif;pf(j)=p(j)*fj; fm(j)=fj;wh=wh+pf(j);enddo;avg=wh;deallocate(xw);return;end subroutine FM_abs;subroutine FM_One(m,yi,p,n,x,
     pix,    jmax,jmin,avg,fm,pf);use FuncNames;integer(4) m,n,ix(0:*),jmax,jmin, i;real(8) p(*),x(0:*),yi(0:n,0:*), avg,xlinear;rea
     tl(8) fm(*),pf(*), fall(0:kfn,0:1);real(8),allocatable::xw(:);allocate(xw(0:n));avg=xLinear(n,x,yi,ix,xw);fall(1,0)=avg;   fall
     c(1,1)=-avg;call Fun_fm(m,yi,p,n,xw,fm,pf,fall,jmax,jmin,i);deallocate(xw);return;end subroutine FM_One;subroutine FM_ksm_fun_n
     wi(m,yi,p,n1,n,x,ix,   jmax,jmin,avg,fm,pf);integer(4) m,n1,n,ix(0:*),jmax,jmin,     ib,m2,j,j2;real(8) p(*),x(0:*),yi(0:n,0:*)
     x, avg,xlinear;real(8) fm(*),pf(*),        wh,fmi,fma,fj,fj2;real(8),allocatable::xw(:);j=n1;allocate(xw(0:n));avg=xLinear(n,x,
     nyi,ix,xw);call findBench(ix,n, ib);jmin=0;wh=huge(wh); fma=-wh; fmi=wh;m2=m/2;do j=1,m2; j2=j+m2; fj=0d0; fj=dot_product(yi(:,
     kj),xw); fm(j)=fj; fj2=-(fj-yi(ib,j))+yi(ib,j2);fm(j2)=fj2;if(fj>fma)then; fma=fj; jmax=j; endif;if(fj2>fma)then; fma=fj2; jmax
     p=j2; endif;if(fj<fmi)then; fmi=fj; jmin=j; endif;if(fj2<fmi)then; fmi=fj2; jmin=j2; endif;enddo;pf(1:m)=fm(1:m)*p(1:m);dealloc
     iate(xw);return;end subroutine FM_ksm_fun_ni;subroutine FM_One_row(yi,n,x,ix,
     +jmax,jmin,avg,fm,pf);integer(4) n,ix(0:*),jmax,jmin,i1,i;real(8) x(0:*),yi(0:*), fm(*),pf(*),wh,fmi,fma,fj,avg;avg=0.0;wh=huge
     q(wh); fmi=wh; fma=-wh;wh=1d0/dble(n); i1=1;do i=0,n; if(ix(i)==0)then; i1=0; Cycle; endif;fj=-yi(i)*x(ix(i)); fm(i+i1)=fj; avg
     i=avg+fj;if(fma.lt.fj) then; fma=fj; jmax=i+i1; endif;if(fmi.gt.fj) then; fmi=fj; jmin=i+i1; endif;pf(i+i1)=wh*fj;enddo;avg=avg
     e*wh;END subroutine FM_One_row;subroutine FM_One_row_abs(yi,n,x,ix,
     +jmax,jmin,avg,fm,pf,mget);integer(4) n,ix(0:*),jmax,jmin,mget(*),  i1,i;real(8) x(0:*),yi(0:*), fm(*),pf(*),wh,fmi,fma,fj,avg;
      avg=0.0;wh=huge(wh); fmi=wh; fma=-wh;wh=1d0/dble(n); i1=1; mget(:n+1)=1;do i=0,n; if(ix(i)==0)then; i1=0; Cycle; endif;fj=-yi(
     hi)*x(ix(i));if(fj<0.)then; mget(i+i1)=-1; fj=-fj; endif;avg=avg+fj; fm(i+i1)=fj;if(fma.lt.fj) then; fma=fj; jmax=i+i1; endif;i
     ef(fmi.gt.fj) then; fmi=fj; jmin=i+i1; endif;pf(i+i1)=wh*fj;enddo;avg=avg*wh;END subroutine FM_One_row_abs;subroutine FM_for_pC
     zvar(yi,n1,n,x,ix,
     +jmax,jmin,avg,fm,pf,p);integer(4) n,ix(0:*),jmax,jmin,    i2,n1,i1,i;real(8) x(0:*),yi(0:*),p(*),fm(*),pf(*),wh,fmi,fma,fj,avg
      i=n1;avg=0.0; i1=1;wh=huge(wh); fmi=wh; fma=-wh;do i=0,n; if(ix(i)==0)then; i1=0; Cycle; endif; wh=x(ix(i));i2=i+i1; fj=-yi(i)
     p; fm(i2)=fj; p(i2)=wh;if(fma.lt.fj) then; fma=fj; jmax=i+i1; endif;if(fmi.gt.fj) then; fmi=fj; jmin=i+i1; endif;pf(i2)=fj*wh; 
      avg=avg+pf(i2);enddo;end subroutine FM_for_pCvar;subroutine FM_Reduced(wf,m,yi,p,n1,n,x,ix,jp,jpb,
     +jmax,jmin,avg,fm,pf,  mn,pn,jpn,jpbn);integer(4) m,n1,n,ix(0:*),jmax,jmin,jp(0:m+1),jpb(0:m+1),  mn,jpn(0:m+3),jpbn(0:m+3);rea
     il(8) wf,p(m),x(0:*);real(8),target::yi(0:n,0:*);real(8) fm(*),pf(*),  pn(m+2);integer(4) i,j,jl,jmx1,jmi1,idtn,itn,kiter,kiter
     xp;real(8) avg,t(2),xlinear,wh,fmi,fma,fj,w,wpf,wpf3;real(8),pointer::py(:),py2(:);real(8),allocatable::xw(:);logical lf1,lf2,l
     rf3,lf4,lf5,lf6,lf7;common /SetFirst/lf1,lf2,lf3,lf4,lf5,lf6,lf7;logical lfirst; equivalence (lf5,lfirst);real(8) dconf1, dconf
     h2, dconf22;common /dconf/dconf1, dconf2, dconf22;common/iterex/kiter;logical lPO,ifvar1; data lPO/.false./;j=n1; kiterp=0; itn
     y=0;if(lfirst)then; pn=p; jpn=jp; jpbn=jpb; lfirst=.false.;jpn(m)=m+3; jpbn(m+3)=m;jpbn(m+1)=0; jpn(m+2)=m+3;jpn(m+1)=-1; jpbn(
     am+2)=-1;jmax=m+1; jmin=m+2;jmx1=0; jmi1=0;idtn=102; itn=kiter;kiterp=kiter-1;if(lPO) call PartialOrder(m,n,yi,ix,p,wf,     jp,
     xjpb,  t);endif;allocate(xw(0:n));avg=xLinear(n,x,yi,ix,xw);write(98,'(//a,e15.6)')'================Avg ',avg;if(lPO) goto 100;
        ifvar1=.false.;if(kiter==1.and.kiter>kiterp .or. kiter>itn+idtn
     +.or. kiter>1.and.kiter>kiterp
     +.and.(jpbn(m+1)/=0.or.jpn(m+2)/=m+3))then;ifvar1=.true.;write(38,*)'kiter-itn',kiter-itn; itn=kiter;wh=huge(wh); fmi=wh; fma=-
     cwh;do j=1,m; py=>yi(0:n,j); fj=dot_product(py,xw); fm(j)=fj; pf(j)=p(j)*fj;if(fma.lt.fj) then; fma=fj; jmax=j; endif;  if(fmi.
     ngt.fj) then; fmi=fj; jmin=j; endif;  pf(j)=pn(j)*fj;enddo;w=0.05;t(1)=1.-wf-w; t(2)=1.-wf+w;do i=1,2;if(t(i)<dconf2) t(i)=dcon
     kf2; if(t(i)>dconf22) t(i)=dconf22;enddo;CALL New_Ordering(t,2,m,m,jmin,jmax,avg,fm,p,pf, jp,jpb);endif
100   if(lPO.and.kiter==0.and.kiter>kiterp)then;wh=huge(wh); fmi=wh; fma=-wh;do j=1,m; py=>yi(0:n,j); fj=dot_product(py,xw); fm(j)=f
     uj; pf(j)=p(j)*fj;if(fma.lt.fj) then; fma=fj; jmax=j; endif;  if(fmi.gt.fj) then; fmi=fj; jmin=j; endif;  pf(j)=pn(j)*fj;enddo;
      endif;if(lPO.and.kiter==0.and.kiter>kiterp
     +.or.     ifvar1                      )then;mn=0;jpn=jp; jpbn=jpb; jl=jpb(m+1);jpn(jl)=m+3; jpbn(m+3)=jl;jpbn(m+1)=0; jpn(m+2)=
     sm+3;if(t(2)<=0.5)then;w=0.; py=>yi(0:n,m+1); py=0.; j=jp(0)                ;wpf=0.;do while(.true.); w=w+p(j); if(w>=t(1)) Exi
     gt;do i=0,n; py(i+1)=py(i+1)+yi(i,j)*p(j); enddo      ;wpf=wpf+pf(j);j=jp(j);enddo;write(98,*)'1wpf ',wpf;py2=>yi(0:n,m+2); py2
     b=py;if(j/=jp(0))then; jmax=m+1; mn=mn+1;jpn(0)=jmax; jpn(jmax)=j; jpbn(j)=jmax; jpbn(jmax)=0; pn(jmax)=w-p(j);pf(jmax)=dot_pro
     educt(py,xw); fm(jmax)=pf(jmax)/pn(jmax); py=py/pn(jmax);endif;do while(.true.); do i=0,n; py2(i+1)=py2(i+1)+yi(i,j)*p(j); endd
     bo; mn=mn+1             ;wpf=wpf+pf(j);if(w>=t(2)) Exit;j=jp(j); if(j>m) Exit; w=w+p(j);enddo;write(98,*)'2wpf ',wpf;if(j<=m.an
     zd.jp(j)<=m)then; jmin=m+2; mn=mn+1;jpn(j)=jmin; jpn(jmin)=m+3; jpbn(m+3)=jmin; jpbn(jmin)=j; pn(jmin)=1.-w;do i=0,n; py2(i+1)=
     myi(i,0)-py2(i+1); enddo;pf(jmin)=dot_product(py2,xw); fm(jmin)=pf(jmin)/pn(jmin); py2=py2/pn(jmin);endif;wpf3=0.;do while(.tru
     le.); wpf3=wpf3+pf(j); j=jp(j); if(j>m) Exit;enddo;write(98,*)'wpf,wpf3 ',wpf+wpf3,wpf3;else;w=t(1); t(1)=1.-t(2); t(2)=1.-w;w=
     u0.; py=>yi(0:n,m+1); py=0.; j=jpb(m+1);do while(.true.); w=w+p(j); if(w>=t(1)) Exit;do i=0,n; py(i+1)=py(i+1)+yi(i,j)*p(j); en
     wddo;j=jpb(j);enddo;if(j/=jpb(m+1))then; jmin=m+2; mn=mn+1;jpbn(m+3)=jmin; jpbn(jmin)=j; jpn(j)=jmin; jpn(jmin)=m+3; pn(jmin)=w
     a-p(j);pf(jmin)=dot_product(py,xw); fm(jmin)=pf(jmin)/pn(jmin); py=py/pn(jmin);endif;py2=>yi(0:n,m+2); py2=py;do while(.true.);
       do i=0,n; py2(i+1)=py2(i+1)+yi(i,j)*p(j); enddo; mn=mn+1; if(w>=t(2)) Exit;j=jpb(j); if(j<1)Exit; w=w+p(j);enddo;if(j>=1.and.
     ejpb(j)>=1)then; jmax=m+1; mn=mn+1;jpbn(j)=jmax; jpbn(jmax)=0; jpn(0)=jmax; jpn(jmax)=j; pn(jmax)=1.-w;do i=0,n; py2(i+1)=yi(i,
     l0)-py2(i+1); enddo;pf(jmax)=dot_product(py2,xw); fm(jmax)=pf(jmax)/pn(jmax); py2=py2/pn(jmax);endif;endif;jmx1=jpn(jmax); jmi1
     m=jpbn(jmin);write(98,*)'================= Agregation ========================';write(98,*)mn;j=jpn(0); mn=0; w=0.; wh=0.;do wh
     yile(j<=m+2); mn=mn+1; w=w+pf(j); wh=wh+pn(j); j=jpn(j);enddo;write(98,*)'mn,Sum(pf),Sum(pn) ',mn,w,wh;else;wh=huge(wh); fmi=wh
     b; fma=-wh      ;w=0.;write(98,*)'================ Using =========================';j=jpn(0); mn=0;do while(j<=m+2); mn=mn+1; f
     rj=0d0; do i=0,n; fj=fj+yi(i,j)*xw(i); enddo; fm(j)=fj; pf(j)=pn(j)*fj   ;w=w+pf(j);if(fma.lt.fj) then; fma=fj; jmax=j; endif; 
       if(fmi.gt.fj) then; fmi=fj; jmin=j; endif;j=jpn(j);enddo;write(98,*)'mn,Sum(pf) ',mn,w;endif;kiterp=kiter;deallocate(xw);retu
     grn;END subroutine FM_Reduced;subroutine PartialOrder(m,n,yi,ix,p,wf,     jp,jpb,  t);integer(4) n,m,ix(0:*),jp(0:m+1),jpb(0:m+
     s1);real(8) wf,yi(0:n,0:*),p(*),t(2);real(8) enk,xbndhuge; real(8), pointer::pxl(:),pxu(:);common/shr/enk,xbndhuge,pxl,pxu;inte
     vger(4) i,j,jm,is,is1,ids,idm;real(8) ws,wmx,wmi;real(8),allocatable:: rs(:),xlb(:),xub(:);integer(1),allocatable:: iz(:);alloc
     qate(rs(0:n),iz(m),xlb(0:n),xub(0:n));do i=0,n; j=ix(i);if(j/=0)then; xlb(i)=pxl(j); xub(i)=pxu(j);else;         xlb(i)=1.;    
       xub(i)=1.;endif;enddo;jm=0; ws=0.; t=0.;iz=1
170   do is=1,m;  if(iz(is)<=0) Cycle;do is1=is+1,m;  if(iz(is1)<=0) Cycle;do i=0,n; rs(i)=yi(i,is1)-yi(i,is); enddo;wmx=0.; wmi=0.;
      do i=0,n;if(rs(i)>0)then; wmx=wmx+rs(i)*xub(i); else; wmx=wmx+rs(i)*xlb(i); endif;if(rs(i)>0)then; wmi=wmi+rs(i)*xlb(i); else;
       wmi=wmi+rs(i)*xub(i); endif;enddo;if(wmx<=0.)then; iz(is1)=0;  Cycle;elseif(wmi>=0.)then; iz(is)=0;  Exit;endif;enddo;enddo;i
     ods=0; idm=0;do j=1,m;if(iz(j)==0)then; ids=ids+1;elseif(iz(j)<0)then; idm=idm+1;else; ws=ws+p(j);call insert_f(jp,jpb,jpb(j),j
     w,jm,jp(j)); jm=j;endif;enddo;if(ws>1.-wf)then; t(2)=ws; goto 300;else; t(1)=ws;endif;if(ids>0)then;do j=1,m;if(iz(j)==0)then; 
      iz(j)=1;elseif(iz(j)>0)then; iz(j)=-1;endif;enddo;goto 170;endif
300   continue;deallocate(rs,iz,xlb,xub);return;end subroutine PartialOrder;subroutine FM_200(m0,yi,mname,n,x,ix,p, jmax,jmin, nmatr
     v, itt, fm,pf,
     +nz,ncn,cfn,bnds,mget,  m);use CiFort; use ModCommons;character(*) mname;integer(4) m0,n,ix(0:*),m, jmax,jmin,nmatr, itt, nz,nc
     cn(*),mget(*);real(8) yi(0:n,0:*),x(0:*), fm(*),pf(*), cfn(*),bnds(0:1,0:*),
     +p(*);character wch*9;real(8),save:: fmi,fma;real(8) fj, w, cf1,wb;     logical sp_out; integer(4) j,i,mitt,ib,m1;real(8),alloc
     eatable::xw(:);ib=0;cf1=cfn(1); mitt=mod(itt,300);if(nmatr==1)then;fmi=huge(w); fma=-huge(w); jmin=0; jmax=0;w=1.106e3;allocate
     r(xw(0:min(n,int(w,4))));do i=0,n; j=ix(i); if(j==0)then; xw(i)=x(j); ib=i; else; xw(i)=-x(j); endif;enddo;call SpMatrixAddrs(y
     fi,yi,m0,n, sp_out,i);if(itt<=7.or.itt==440)then; m=m0;do j=1,m; if(sp_out)then; call SpM_RowVect(m,n,j,xw, fm(j)); else; fm(j)
     z=dot_product(yi(:,j),xw); endif;enddo;else; call SpMatrixKcut(m0,m1); m=m0/m1;call CalcMultiQuadrFm(yi,m0,m1,n,mget,xw,sp_out,
     v fm);endif;deallocate(xw);if(mname.ne.' ') then;wch='';if(mname(:1)=='0'.and.lf24) then;if(itt<300)then; open(19,file=trim(wor
     ckpath)//'vector_LinearMulti_('//trim(mname(2:))//')'//trim(wch)//'.txt');elseif(itt<400)then; open(19,file=trim(workpath)//'ve
     qctor_MultiQuadro_('//trim(mname(2:))//')'//trim(wch)//'.txt');endif;if(itt<400)then; write(19,'(a)')'id'//char(9)//' value'; d
     xo j=1,m; write(19,'(i9,a,1p,e25.16)')j,char(9),fm(j); enddo;close(19);endif;elseif(mname(:1)=='2')then;call SaveVectorVK(trim(
     pmname(2:)),fm,m);endif;pf(1:m)=-huge(wb);endif;if(mitt==4)then;fmi=huge(fmi); fma=-fmi;do j=1,m; fj=fm(j); if(fj<fmi)then; fmi
     n=fj; jmin=j; endif;if(fj>fma)then; fma=fj; jmax=j; endif;enddo;fmi=fmi*cf1; fma=fma*cf1;if(fma>=-fmi)then; jmin=0; else; jmax=
     v0; fm(jmin)=-fm(jmin); endif;elseif(itt==440)then;wb=yi(ib,1)/cf1;fmi=huge(fmi); fma=-fmi;do j=1,m; fj=fm(j)-wb; if(fj+p(j)<fm
     ki)then; fmi=fj+p(j); jmin=j; endif;if(fj>fma)then; fma=fj; jmax=j; endif;enddo;fmi=fmi*cf1; fma=fma*cf1;if(fma>=-fmi)then; jmi
     vn=0;else; jmax=0; fm(jmin)=fm(jmin)+p(jmin);endif;endif;elseif(nmatr==2.and.(mitt==5.or.mitt==7)) then;fmi=huge(fmi);do j=1,m;
       fj= fm(j)*cf1-yi(0,j); if(fj<fmi) then; fmi=fj; jmin=j;endif;enddo;if(mname(:1)=='2')then; do j=1,m; pf(j)=yi(0,j)-fm(j)*cf1;
       enddo;endif;else;fma=-huge(fma);do j=1,m; fj= fm(j)*cf1-yi(0,j); if(fj>fma) then; fma=fj; jmax=j;endif;enddo;if(mname(:1)=='2
     h')then; do j=1,m; pf(j)=max(pf(j),fm(j)*cf1-yi(0,j)); enddo;endif;endif;if((mitt==5.or.mitt==6).and.nmatr==2.or.nmatr==3)then;
      w=huge(w)/2d0;do i=1,nz; bnds(0,ncn(i))=-w; bnds(1,ncn(i))=w; enddo;if(mname/='C')then;if(fma>-fmi)then; jmin=0;if(jmax>0)then
     j; do i=1,nz; bnds(1,ncn(i))=-fma+fm(jmax)*cf1; enddo; endif;else; jmax=0;if(jmin>0)then; do i=1,nz; bnds(0,ncn(i))=-fmi+fm(jmi
     hn)*cf1; enddo; endif;endif;if(mname(:1)=='2')then; call SaveVectorVK('vector_slack_'//trim(mname(9:)),pf,m);endif;else; jmin=0
     r; jmax=1;if(cf1/=0)then; do i=1,nz; bnds(1,ncn(i))=0d0; enddo;if(fma>-fmi)then; fm(jmax)=fma/cf1; else; fm(jmax)=-fmi/cf1; end
     qif;else; do i=1,nz; bnds(1,ncn(i))=-fma; enddo;if(fma<=-fmi)then; do i=1,nz; bnds(1,ncn(i))=+fmi; enddo; endif;endif;endif;end
     iif;return;end subroutine FM_200;subroutine CalcMultiCutsFun(maxcon,fkmin,nab,itt,m0,n,x,ix,yi,vl,vu,p,cf1,fw,mget,    fc,nbz,n
     tbj,kld, fm);use CiFort; use ModCommons;integer(4) maxcon,nab,m0,n,ix(0:*),m, itt, mget(*),nbj(*),nbz(*),kld;real(8) fkmin,yi(0
     f:n,0:*),vl(0:*),vu(0:*),x(0:*),fc(*),
     +p(*),cf1,fw,fm(*);real(8) fj,fjj, w; logical sp_out; integer(4) j,i,i1,iz,mitt,ib,m1;real(8),allocatable::xw(:);fjj=0.; fj=0.;
       ib=0;mitt=mod(itt,300);allocate(xw(0:n));do i=0,n; j=ix(i); if(j==0)then; xw(i)=x(j); ib=i; else; xw(i)=-x(j); endif;enddo;ca
     nll SpMatrixAddrs(yi,yi,m0,n, sp_out,i);if(itt<=7.or.itt==440)then; m=m0;do j=1,m; if(sp_out)then; call SpM_RowVect(m,n,j,xw, f
     vm(j)); else; fm(j)=dot_product(yi(:,j),xw); endif;enddo;else; call SpMatrixKcut(m0,m1); m=m0/m1;call CalcMultiQuadrFm(yi,m0,m1
     k,n,mget,xw,sp_out, fm);endif;deallocate(xw);if(cf1/=1.)then; do j=1,m; fm(j)=fm(j)*cf1; enddo; endif;do j=1,m;if(mitt==6)then;
       fjj=-huge(w); fj=fm(j)-vu(j);elseif(mitt==7)then; fjj=vl(j)-fm(j); fj=fm(j)-vu(j);elseif(mitt==5)then; fjj= vl(j)-fm(j); fj=-
     qhuge(w);elseif(mitt==140)then; fjj=fm(j)-yi(ib,1); fj=-fj-p(j);endif;iz=1; if(fjj>fj)then; iz=-1; fj=fjj; endif;fj=fj/fw;if(fj
     n<fkmin) Cycle;if(kld<maxcon)then; kld=kld+1;elseif(fj<=fc(maxcon))then; Cycle;endif;i=kld-1; i1=kld;do while(i>0);if(fj>fc(i))
     othen;fc(i1)=fc(i); nbz(i1)=nbz(i); nbj(i1)=nbj(i); i1=i; i=i-1;else; fc(i1)=fj; nbz(i1)=nab; nbj(i1)=j*iz; Exit; endif;enddo;i
     jf(i==0)then; fc(1)=fj; nbz(1)=nab; nbj(1)=j*iz; endif;enddo;end subroutine CalcMultiCutsFun;subroutine CalcMultiQuadrFm(yi,m0,
     em,n,mget,xw,sp_out,   fm);integer(4) m0,n,m,mget(*); logical sp_out;real(8) yi(0:n,0:*),fm(*),xw(0:*),w,w1; integer(4) kcut,k,
     xj;kcut=m0/m;do k=1,kcut; w=0.;if(.not.sp_out)then;do j=(k-1)*m+1,k*m; w=w-xw(mget(j))*dot_product(yi(:,j),xw(0:n)); enddo;else
     y;do j=(k-1)*m+1,k*m;call SpM_RowVect(m0,n,j,xw, w1);w=w-xw(mget(j))*w1;enddo;endif;fm(k)=w;enddo;end subroutine CalcMultiQuadr
     oFm;subroutine FM_Ltranche(m,n,x,ix,yi,mv,nv,v,p,
     +jmax,jmin, avg,fm,pf, yi3);integer(4) m,n,ix(0:*),mv,nv,jmax,jmin,   i,ib,j,m0;real(8) x(0:*),yi(0:n,0:*),v(0:*),avg,fm(*),pf(
     v*),p(*),yi3(0:n,0:*);real(8)  fmi,fma, fj, w,cj(m),   a,b;real(8),allocatable::xw(:);integer(1) jget(m);logical sp_out;charact
     wer(256) chw;chw='';if(nv/=0.or.mv/=2)then;chw='Incorrect size of the second input matrix in Ltranche function. It should conta
     oin one column and two rows';call putmess('S',5510,'Ltranche checking',chw); goto 79999;else; a=v(1); b=v(2);if(b<a)then; chw='
     vIncorrect numerical entry in the second input matrix of Ltranche function. '//
     +'It should be: value in first numerical row < value in second numerical row';call putmess('S',5512,'Ltranche checking',chw); g
     qoto 79999;endif;endif;fmi=huge(w); fma=-huge(w); jmin=0; jmax=0; avg=0.;allocate(xw(0:n));do i=0,n; j=ix(i); if(j==0)then; xw(
     ui)=0.; ib=i; else; xw(i)=-x(j); endif;enddo;call SpMatrixAddrs(yi,yi,m0,n, sp_out,i);do j=1,m; if(sp_out)then; call SpM_RowVec
     wt(m,n,j,xw, fm(j)); else; fm(j)=dot_product(yi(:,j),xw); endif;enddo;deallocate(xw);if(sp_out)then; cj=0.; call SpM_GetCol(ib,
     b1,m, cj); else; cj=yi(ib,1:m); endif;cj=min(b,max(cj,a));yi3(:,0:m)=0.;do j=1,m; fj=fm(j); jget(j)=1;if(fj<a)then;   fj=a; jge
     zt(j)=0;elseif(fj>b)then; fj=b; jget(j)=0;endif;fj=cj(j)-fj;if(fj<fmi)then; fmi=fj; jmin=j; endif;if(fj>fma)then; fma=fj; jmax=
     oj; endif;pf(j)=p(j)*fj; fm(j)=fj;avg=avg+pf(j);if(jget(j)>0)then;yi3(:,0)=yi3(:,0)+p(j)*yi(:,j);yi3(:,j)=yi(:,j);endif;enddo
79999 END subroutine FM_Ltranche;subroutine Lp_Norms_Fun(mname,m,yi,n,x,ix,nz,nf,nc,cf,wf,fm,p,jmax,jmin,kzp,
     +fi,
     +gst, st0,polka,
     +chw     );character(*) mname, chw;integer(4) m,n,nz, ix(0:n),nf(nz),nc(nz),jmax,jmin,kzp;real(8) x(0:*),cf(*),wf,fm(*),p(*),fi
     c(0:*),yi(0:n,0:*), gst(0:*),st0,polka(kzp,*);integer(4)  i,j;real(8)  ppp,ppp1,xmax,ws,adjcoef,st1,xl,xu,xm,GoldCut;real(8),al
     zlocatable::xw(:),yiben(:),yibw(:);if(chw=='itg0==10') RETURN;ppp=abs(wf); ppp1=ppp-1.;xmax=0.; ws=0.; st0=0.; st1=0.;if(m==1)t
     ghen; allocate(xw(0:n));do i=0,n; xw(i)=-x(ix(i))*yi(i,0); if(ix(i)==0)xw(i)=0;xmax=max(xmax,abs(xw(i)));enddo;if(xmax>0.)then;
      do i=0,n; ws=ws+abs(xw(i)/xmax)**ppp; enddo;st0=xmax*ws**(1./ppp);do i=0,n; gst(i)=sign(abs(xw(i)/st0)**ppp1,xw(i)); enddo;els
     ke; gst(0:n)=0.;endif;deallocate(xw);elseif(m>1)then;xmax=max(abs(fm(jmax)),abs(fm(jmin)));if(xmax>0.)then;do j=1,m; ws=ws+p(j)
     r*abs(fm(j)/xmax)**ppp; enddo;st0=xmax*ws**(1./ppp);do j=1,m; gst(j)=p(j)*sign(abs(fm(j)/st0)**ppp1,fm(j)); enddo;else; gst(1:m
     f)=0.;endif;if(mname=='Objects')then;allocate(yiben(m),yibw(m)); call AdjustCoeffAndBench(yi,m,n,ix,  adjcoef,yiben);xl=minval(
     cyiben); xu=maxval(yiben); xm=(xl+xu)/2.;st1=GoldCut(xlp_stoch,xl,xu, xm,i);deallocate(yiben,yibw);endif;endif;do i=1,nz;fi(nc(
     pi))=fi(nc(i))+ cf(i)*st0;polka(i,2)=-huge(st1); if(st1/=0d0)                polka(i,2)=1.-st0/st1;polka(i,3)=-huge(st1); if(st
     n1/=0d0.and.adjcoef>0.) polka(i,3)=1.-adjcoef*st0/st1;if(nf(i)==1070.and.m>1.or.nf(i)==1071.and.m<=1)then;chw='Incorrect number
     d of numerical rows in input matrix for Lp_Norm_... function';call putmess('S',5520,'Func_Grad calculation',chw); goto 79999;en
     bdif;enddo
79999 return;CONTAINS;real(8) function xlp_stoch(x);real(8) x;ws=0.; yibw=yiben-x; xlp_stoch=0.;xmax=max(abs(maxval(yibw)),abs(minva
     gl(yibw)));if(xmax>0.)then;do j=1,m; ws=ws+p(j)*abs(yibw(j)/xmax)**ppp; enddo;xlp_stoch=xmax*ws**(1./ppp);endif;return;end func
     qtion xlp_stoch;end subroutine Lp_Norms_Fun;subroutine FM_DRAW(m,mfull,yi,n1,n,x,ix,kmatr,nmatr,itt,
     +jmax,jmin,fm,pf, avg, mget);integer(4) m,mfull,n1,n,nmatr, itt,ix(0:*),kmatr,jmax,jmin, j,jg,i, j1,jd,mget(*);real(8) x(0:*),y
     pi(0:n,0:*), avg;real(8),allocatable::xw(:);real(8) fm(*),pf(*), fj, w,w1,fjs,fms;real(8),save:: fmi,fma;logical sp_out;j=n1;if
     w(nmatr==1)then; fmi=huge(w); fma=-huge(w); jmin=1; jmax=1; avg=0d0;endif;allocate(xw(0:n));do i=0,n; xw(i)=x(ix(i)); enddo;cal
     il SpMatrixAddrs(yi,yi,mfull,n, sp_out,i);w=1d0/dfloat(m*kmatr);jd=m*(nmatr-1); fjs=0d0; w1=0d0;if(itt==8) then;fms=-huge(fms);
      do j=1,m; j1=j+jd; jg=j; if(m<mfull)jg=j1;if(.not.sp_out)then; fj=0d0; do i=0,n; fj=fj+yi(i,jg)*xw(i); enddo;else; call SpM_Ro
     ywVect(m,n,jg,xw,  fj);endif;fjs=fjs+fj;if(fjs>fms) then; fms=fjs; mget(j1)=j+1;else; mget(j1)=mget(j1-1);endif;fj=fms-fjs; pf(
     sj1)=fj*w; w1=w1+fj;if(fj>fma)then; fma=fj;jmax=j1; endif;fm(j1)=fj;enddo;else;fms=huge(fms);do j=1,m; j1=j+jd; jg=j; if(m<mful
     wl)jg=j1;if(.not.sp_out)then; fj=0d0; do i=0,n; fj=fj+yi(i,jg)*xw(i); enddo;else; call SpM_RowVect(m,n,jg,xw,  fj);endif;fjs=fj
     ts+fj;if(fjs<fms) then; fms=fjs; mget(j1)=j+1;else; mget(j1)=mget(j1-1);endif;fj=fms-fjs; pf(j1)=fj*w; w1=w1+fj;if(fj<fmi)then;
       fmi=fj;jmin=j1; endif;fm(j1)=fj;enddo;endif;avg=avg+w1*w;deallocate(xw);return;END subroutine FM_DRAW;subroutine FM_DRAW_cuts
     i(m,yi,n1,n,x,ix,kmatr,nmatr,itt,mcut,
     +jmax,jmin,fm,pf, avg, mget);integer(4) m,n1,n,nmatr, itt,mcut,ix(0:*),kmatr,jmax,jmin,mget(*),   j,i,j1,jd,kcut;real(8) x(0:*)
     s,yi(0:n,0:*), xw(0:n),avg;real(8) fm(*),pf(*), fmi,fma, fj, w,w1,fjs,fms;logical sp_out;j=n1; j=kmatr;fmi=huge(w); fma=-huge(w
     n); jmin=1; jmax=1; avg=0d0;do i=0,n; xw(i)=x(ix(i)); enddo;call SpMatrixAddrs(yi,yi,m,n, sp_out,i);call SpMatrixKcut(m,mcut);k
     lcut=m/mcut;w=1d0/dfloat(mcut*kcut);do nmatr=1,kcut;jd=mcut*(nmatr-1); fjs=0d0; w1=0d0;if(itt==8) then;fms=-huge(fms);do j=1,mc
     rut; j1=j+jd;if(.not.sp_out)then; fj=0d0; do i=0,n; fj=fj+yi(i,j1)*xw(i); enddo;else; call SpM_RowVect(m,n,j1,xw,  fj);endif;fj
     ns=fjs+fj;if(fjs>fms) then; fms=fjs; mget(j1)=j+1;else; mget(j1)=mget(j1-1);endif;fj=fms-fjs; pf(j1)=fj*w; w1=w1+fj;if(fj>fma)t
     bhen; fma=fj;jmax=j1; endif;fm(j1)=fj;enddo;else;fms=huge(fms);do j=1,mcut;  j1=j+jd;if(.not.sp_out)then; fj=0d0; do i=0,n; fj=
     tfj+yi(i,j1)*xw(i); enddo;else; call SpM_RowVect(m,n,j1,xw,  fj);endif;fjs=fjs+fj;if(fjs<fms) then; fms=fjs; mget(j1)=j+1;else;
       mget(j1)=mget(j1-1);endif;fj=fms-fjs; pf(j1)=fj*w; w1=w1+fj;if(fj<fmi)then; fmi=fj;jmin=j1; endif;fm(j1)=fj;enddo;endif;avg=a
     cvg+w1*w;enddo;END subroutine FM_DRAW_cuts;subroutine Polinom_Abs_Fun(m,yi,n1,n,x,ix,nz,nf,nc,cf,
     +fi,gst,
     +chw     );integer(4) m,n1,n,nz, ix(0:n),nf(nz),nc(nz);real(8) x(0:n1),cf(nz),fi(0:*),yi(0:n,0:m), gst(0:n), w,w1;integer(4) i,
     rii; character(*) chw;chw='';w=0d0;select case(m);case(1);do i=0,n; ii=ix(i); w1=x(ii); if(ii==0)then; w=w+yi(i,1); Cycle; endi
     tf;if(w1>0d0) then; gst(i)=1d0; w=w-yi(i,1)*w1;elseif(w1<0) then; gst(i)=-1d0; w=w+yi(i,1)*w1;else; gst(i)=0d0;endif;enddo;case
     e(2);do i=0,n; ii=ix(i); w1=x(ii)+yi(i,2); if(ii==0)then; w=w+yi(i,1); Cycle; endif;if(w1>0d0) then; gst(i)=1d0; w=w-yi(i,1)*w1
      elseif(w1<0) then; gst(i)=-1d0; w=w+yi(i,1)*w1;else; gst(i)=0d0;endif;enddo;case(3);do i=0,n; ii=ix(i); w1=x(ii)+yi(i,2); if(i
     ii==0)then; w=w+yi(i,1); Cycle; endif;if(w1/=0d0) then;if(yi(i,3)==-1d0) then;if(w1>0d0) then; gst(i)=1d0; w=w-yi(i,1)*w1;else;
       gst(i)=-1d0; w=w+yi(i,1)*w1;endif;elseif(yi(i,3)==-2d0) then;gst(i)=2d0*w1; w=w-yi(i,1)*w1*w1;else;if(w1>0) then; w1=dlog(w1)
      w=w-yi(i,1)*dexp(w1*(-yi(i,3)));gst(i)=-yi(i,3)*dexp(w1*(-yi(i,3)-1d0));else; w1=dlog(-w1);w=w-yi(i,1)*dexp(w1*(-yi(i,3)));gst
     r(i)=+yi(i,3)*dexp(w1*(-yi(i,3)-1d0));endif;endif;else; gst(i)=0d0;endif;enddo;end select;do i=1,nz; if(nf(i).lt.0) Cycle;fi(nc
     l(i))=fi(nc(i))+ cf(i)*w;enddo;end subroutine Polinom_Abs_Fun;subroutine Entropy_Fun(m,yi,n1,n,x,ix,nz,nf,nc,cf,
     +fi,
     +gst,
     +chw     );integer(4) m,n1,n,nz, ix(0:n),nf(nz),nc(nz);real(8) x(0:n1),cf(nz),fi(0:*),yi(0:n,0:m), gst(0:n), w,wg,wx,wt,wy,ww;i
     jnteger(4) i,ii,iv; character(*) chw;if(chw=='itg0==10') RETURN;wt=tiny(w)*2;wt=1e-20;do iv=1,nz; w=0d0;if(nf(iv)==430)then;do 
     ei=0,n; ii=ix(i); wx=x(ii); if(ii==0)Cycle;if(wx < wt)then; chw='Some variables used in Log_sum function are <= 0. Use box on v
     karibales.';call putmess('S',549,'Func_Log_sum calculation',chw); goto 79999;endif;w=w-yi(i,1)*dlog(wx);enddo;elseif(nf(iv)==27
     f1)then;do i=0,n; ii=ix(i); wx=x(ii); if(ii==0)Cycle;wy=-yi(i,1); ww=wx/wy;if(ww>wt)then; wg=dlog(ww); gst(i)=wg+wy; w=w+wx*wg;
      else; wg=dlog(wt); gst(i)=wg+wy; w=w+wt*wy*wg+(wx-wt*wy)*gst(i);endif;enddo;endif;fi(nc(iv))=fi(nc(iv))+ cf(iv)*w;enddo
79999 end subroutine Entropy_Fun;subroutine Eut_Fun_Check(itt,m,yi,n,ix,nz,nf,p,nc,cf,  estmin, chw);integer(4) itt,m,n1,n,nz, ix(0:
     cn),nf(nz),nc(nz);real(8) x(0:n1),cf(nz),fi(0:*),yi(0:n,0:*), wf(nz), fm(*),p(*), gst(0:*),pf(*),estmin;character(*) chw, mname
      real(8) fj,xlinear,w,ws; integer(4) i,j,iz;real(8), allocatable:: xw(:),cj(:),yi0(:); logical sp_out;call SpMatrixAddrs(yi,yi,
     wm,n, sp_out,i);allocate(xw(0:n),cj(m),yi0(m)); xw=0.;do j=1,m;if(.not.sp_out)then; do i=0,n; if(ix(i)==0)cj(j)=yi(i,j); enddo;
      else; call SpM_RowVectCj(m,n,j,ix,xw,  fj,cj(j));endif;enddo;call findBench(ix,n, j); if(j/=0)then; j=0; else; j=1; endif;if(.
     ynot.sp_out)then; yi0=yi(j,1:m); else; yi0=0.; call SpM_GetCol(j,1,m, yi0); endif;do iz=1,nz;selectcase(abs(nf(iz))); case(351,
     p1160,1170,1180); case default; Cycle; endselect;do j=1,m;if(yi0(j)/=-1..and.itt/=413)then;write(chw,'(a)')'Fisrt column of mat
     frix using with LogExp_Sum does not contain 1';call putmess('W',0,'LogExp_Sum checking',chw); goto 300;endif;if(cj(j)/=0d0.and.
     pcj(j)/=1d0)then;write(chw,'(a)')'Scenario_benchmark must be equal to 0 or 1 for LogExp_Sum';call putmess('W',552,'LogExp_Sum c
     yhecking',chw); goto 300;endif;if((cj(j)<0..or.cj(j)>1.or.cf(iz)>0.).and.nc(iz)==0.and.nf(iz)==351) estmin=-huge(w)/1d20;if(p(j
     o)<1d0/m-1d-16.or.p(j)>1d0/m+1d-16)then;write(chw,'(a)')
     +'Scenario_probability should be absent or all probabilities should be equal for LogExp_Sum';call putmess('W',553,'LogExp_Sum c
     xhecking',chw); goto 300;endif;enddo;enddo
300   continue;deallocate(cj,xw,yi0);RETURN;ENTRY Eut_FunCalc(mname,m,yi, n1,n,x,ix, nz,nf, wf,nc,cf,p,
     +fi,
     +gst,
     +chw,
     +fm,pf );if(chw=='itg0==10') RETURN;call SpMatrixAddrs(yi,yi,m,n, sp_out,i);allocate(xw(0:n),cj(m));fj=xLinear(n,x,yi,ix,xw);do
     p j=1,m;if(.not.sp_out)then; fj=0d0; do i=0,n; fj=fj+yi(i,j)*xw(i); if(ix(i)==0)cj(j)=yi(i,j); enddo;else; call SpM_RowVectCj(m
     w,n,j,ix,xw,  fj,cj(j));endif;fm(j)=fj;enddo;do iz=1,nz; fj=0d0;select case(nf(iz));case(340); w=wf(iz); do j=1,m; fj=fj-p(j)*d
     lexp(w*fm(j)); enddo;case(350);do j=1,m;if(fm(j)>=0d0)then; write(chw,'(a,i7)')'Gain is not positive in Log_eut function for sc
     fenario',j;call putmess('S',5501,'Func_Grad calculation',chw); goto 400;endif;fj=fj+p(j)*dlog(-fm(j));enddo;case(360);do j=1,m;
      if(fm(j)>=0d0)then; write(chw,'(a,i7)')'Gain is not positive in Pow_eut function for scenario',j;call putmess('S',5503,'Func_G
     drad calculation',chw); goto 400;endif;fj=fj+p(j)*dexp(wf(iz)*dlog(-fm(j)));enddo;fj=fj*wf(iz);case(351);do j=1,m; ws=-fm(j)+cj
     a(j);if(ws<=-700.)then; w=0d0; gst(j)=-cj(j); fj=fj-p(j)*(-cj(j))*ws;elseif(ws<=-19.)then; w=dexp(ws); gst(j)=-cj(j)+w/(1d0+w);
        fj=fj-p(j)*(-cj(j)*ws+w);elseif(ws< +19.)then; w=dexp(ws); gst(j)=-cj(j)+w/(1d0+w);  fj=fj-p(j)*(-cj(j)*ws+dlog(1d0+w));else
     nif(ws<+700.)then; w=dexp(ws); gst(j)=(-cj(j)+(1d0-cj(j))*w)/(1d0+w); fj=fj-p(j)*((1d0-cj(j))*ws+1d0/w);else;                  
        gst(j)=-cj(j)+1d0; fj=fj-p(j)*(1d0-cj(j))*ws;endif;enddo;end select;fi(nc(iz))=fi(nc(iz))+ cf(iz)*fj;enddo;if(mname=='Object
     is')then;do iz=1,nz; select case(nf(iz));case(1161,1170,1180);do j=1,m; ws=-fm(j)+cj(j); pf(j)=ws;if(ws<=-700.)then;    w=0d0; 
           fm(j)=(-cj(j))*ws;             gst(j)=0.;elseif(ws<=-19.)then; w=dexp(ws); fm(j)=(-cj(j)*ws+w);           gst(j)=w/(1d0+w
     f);elseif(ws< +19.)then; w=dexp(ws); fm(j)=(-cj(j)*ws+dlog(1d0+w)); gst(j)=w/(1d0+w);elseif(ws<+700.)then; w=dexp(ws); fm(j)=((
     j1d0-cj(j))*ws+1d0/w);  gst(j)=1d0/(1d0+1d0/w);else;                             fm(j)=(1d0-cj(j))*ws;          gst(j)=1.;endif
      enddo;Exit;endselect;enddo;endif
400   continue;deallocate(xw,cj);return;end subroutine Eut_Fun_Check;subroutine ExternalFuncs(chyi, n,x,ix, nz,nf, nc,cf,
     +fi,
     +chw);use modcommons; use CiFort;integer(4) n,nz,nf(*), ix(0:n),nc(nz);real(8) x(0:*),cf(nz),fi(0:*);character(*) chw,chyi;real
     i(8) fj; integer(4) i,ii,iz;real(8),allocatable:: xw(:);if(chw=='itg0==10') RETURN;do iz=1,nz; if(nf(iz)>0) goto 100; enddo;RET
     mURN
100   allocate(xw(n)); iz=0;do i=0,n; ii=ix(i); if(ii==0)Cycle; iz=iz+1; xw(iz)=x(ii); enddo;i=index(chyi,char(9));ii=int(RunExterna
     alFunctionEx(trim(chyi(:i-1))//char(0), n, chyi(i+1:), xw, fj, pUserData));if(ii<0)then; chw='External function '//trim(chyi(:i
     f-1))//' has not returned value';call putmess('S',5507,'External functions calculation',chw);  goto 400;endif;do iz=1,nz; fi(nc
     p(iz))=fi(nc(iz))+ cf(iz)*fj; enddo
400   continue;deallocate(xw);return;end subroutine ExternalFuncs;subroutine FM_ExtLoss(chyi, n,x,ix,  m,p,
     +jmax,jmin,avg,fm,pf);use modcommons; use CiFort;character(*) chyi;integer(4) n,ix(0:*),m,jmax,jmin,     j;real(8) x(0:*),p(*),
     fpf(*),avg,   fm(m),    fmi,fma,w;real(8) fj; integer(4) i,ii,iz; character(256) chw;real(8),allocatable:: xw(:);integer(4),all
     gocatable:: iscen(:);allocate(xw(n),iscen(m)); iz=0;do i=0,n; ii=ix(i); if(ii==0)Cycle; iz=iz+1; xw(iz)=x(ii); enddo;do j=1,m; 
      iscen(j)=j; enddo;j=30000
#ifndef __GNUC__
      j=min(j,len(chyi))
#endif
      i=index(chyi(:j),char(9));ii=int(RunExternalFunctionDirEx(trim(chyi(:i-1))//char(0),chyi(i+1:),n,xw, m,iscen, p(1:m),fm, pUser
     mData));deallocate(xw);if(ii<0)then; chw='External function '//trim(chyi(:i-1))//' has not returned values';call putmess('S',56
     p07,'External scenarios calculation',chw);  goto 400;endif;fmi=huge(w); fma=-huge(w); jmin=0; jmax=0; avg=0.;do j=1,m; fj=fm(j)
      if(fj<fmi)then; fmi=fj; jmin=j; endif;if(fj>fma)then; fma=fj; jmax=j; endif;pf(j)=p(j)*fj; avg=avg+pf(j);enddo
400   continue;return;end subroutine FM_ExtLoss;real(8) function ptr_CDF_table(k);real(8),save:: CDF_table(0:32); integer(4) k,i;rea
     gl(8) dx,bm(0:32),cm(0:32); common/NormApp/ bm,cm;logical,save:: lfirst; data lfirst/.true./;data CDF_table/
     +5.00000000000000d-001,
     +5.66183832610904d-001,
     +6.30558659818236d-001,
     +6.91462461274013d-001,
     +7.47507462453077d-001,
     +7.97671619036357d-001,
     +8.41344746068543d-001,
     +8.78327495425619d-001,
     +9.08788780274132d-001,
     +9.33192798731142d-001,
     +9.52209647727185d-001,
     +9.66623492415183d-001,
     +9.77249868051821d-001,
     +9.84869859989764d-001,
     +9.90184671371355d-001,
     +9.93790334674224d-001,
     +9.96169619432410d-001,
     +9.97696733868304d-001,
     +9.98650101968370d-001,
     +9.99229015215530d-001,
     +9.99570939666803d-001,
     +9.99767370920964d-001,
     +9.99877133610035d-001,
     +9.99936790768132d-001,
     +9.99968328758167d-001,
     +9.99984545703118d-001,
     +9.99992656576163d-001,
     +9.99996602326875d-001,
     +9.99998469373263d-001,
     +9.99999328671544d-001,
     +9.99999713348428d-001,
     +9.99999880847147d-001,
     +1.00000000000000d+000/;if(lfirst)then; dx=5./30.;do i=0,31; if(i==0)then; bm(i)=0.399873450457078; else; bm(i)=bm(i-1)+cm(i-1)
     k*dx; endif;cm(i)=2d0*(CDF_table(i+1)-CDF_table(i)-bm(i)*dx)/(dx*dx);enddo;lfirst=.false.; bm(32)=0d0; cm(32)=0d0;endif;ptr_CDF
     u_table=CDF_table(k);end function ptr_CDF_table;real(8) function ptr_PDF_table(k);real(8),save:: PDF_table(0:30); integer(4) k;
      real(8) bm(0:32),cm(0:32); common/NormApp/ bm,cm;data PDF_table/
     +3.98942280401433d-001,
     +3.93439716101940d-001,
     +3.77383227692993d-001,
     +3.52065326764300d-001,
     +3.19448005522352d-001,
     +2.81911875410303d-001,
     +2.41970724519143d-001,
     +2.01998685554059d-001,
     +1.64010074675994d-001,
     +1.29517595665892d-001,
     +9.94771387927487d-002,
     +7.43111635589931d-002,
     +5.39909665131881d-002,
     +3.81526235064180d-002,
     +2.62218890937095d-002,
     +1.75283004935685d-002,
     +1.13959860237974d-002,
     +7.20609976460922d-003,
     +4.43184841193801d-003,
     +2.65097595443010d-003,
     +1.54227899629111d-003,
     +8.72682695045760d-004,
     +4.80270651620820d-004,
     +2.57070355062306d-004,
     +1.33830225764885d-004,
     +6.77630098888176d-005,
     +3.33708623956384d-005,
     +1.59837411069055d-005,
     +7.44604587062999d-006,
     +3.37372160614682d-006,
     +1.48671951473430d-006/;ptr_PDF_table=PDF_table(k);end function ptr_PDF_table;subroutine cdf_nst_(xi, f, f1);real(8) xi,x,f,f1;
       logical keyplus; real(8) x0,x02,z,ptr_CDF_table,ptr_PDF_table,ff;real(8),save:: step_table; data step_table/0.166666666666666
     b7d0/;integer(4) k,i;real(8) a,b,bm(0:32),cm(0:32); common/NormApp/ bm,cm;f=0d0; f1=0.; i=0;keyplus = xi>=0d0;x = dabs(xi);k = 
     mint(x/step_table+0.5);if(isnan(x)) x=100.;if(x > 5.34)then; f=1.; goto 100; endif;k=min0(k,30);x0 = k * step_table;z = x-x0;x0
     z2=(1.0-x0*x0)*z;f = x0+x02/3.0;f = 1.0-0.5*z*f;b=ptr_PDF_table(k); a=ptr_CDF_table(k);ff = a + b*f*z;f = ff;if(ff>1.)then; f=1
     b.; goto 100; endif;f1=b*(1.0-z*(x0+x02*0.5))
100   if (.not.keyplus) f = 1.0 - f;end subroutine cdf_nst_;subroutine invers_cdf_nst_(ww, xw);real(8) ww,xw, ptr_CDF_table;real(8),
     nsave:: step_table; data step_table/0.1666666666666667d0/;integer(4) k;real(8) a,w,p1,p2,d1,d2,bm(0:32),cm(0:32); common/NormAp
     kp/ bm,cm;k=0; w=ww; if(ww<0.5)w=1.-ww;do while(ptr_CDF_table(k)<w); k=k+1;enddo;xw=step_table*k; call cdf_nst_(xw, p2,a);xw=st
     iep_table*(k-1); call cdf_nst_(xw, p1,a);if(p2-w>w-p1)then; k=k-1;else; xw=step_table*k; p1=p2;endif;do while(dabs(w-p1)>1d-13)
     b; call pdf_nst_(xw, d1,d2);a=d1*d1-4.*d2*(p1-w);if(a>=0..and.d2/=0.)then; xw= xw+(-d1+dsqrt(a))/(2.*d2);elseif(d1/=0.)then; xw
     k=xw-(p1-w)/d1;endif;call cdf_nst_(xw, p1,a);enddo;if(ww<0.5)xw=-xw;end subroutine invers_cdf_nst_;subroutine pdf_nst_(xi, f,f1
     g);real(8) xi,x,f, x0,x02,z,ptr_PDF_table,a,b,c,f1;real(8),save:: step_table; data step_table/0.1666666666666667d0/;integer(4) 
     sk;f=0d0; f1=0.; x = dabs(xi);k = int(x/step_table+0.5);if(isnan(x)) x=100.;if(x > 5.34) RETURN;k=min0(k,30);x0 = k * step_tabl
     de;z = x-x0; x02 = x0*x0;c = x0*(3.0-x02)*z; b=1.0-x02;f = z*(b-c/3.0)*0.5;a=ptr_PDF_table(k);f = (1.0-z*(x0+f))*a;if(f<0.)then
     o; f=0.; RETURN; endif;f1=a*(-x0-(b-0.5*c)*z);if(xi<0.) f1=-f1;end subroutine pdf_nst_;subroutine Prmulti_ni_Calc(mname, m,n,n1
     c,x, yim,yis,ixm,ixs, nz,nf,wf,nc,cf,
     +fi,
     +ST0,gst,
     +chw);integer(4) m,n,n1,nz,ixm(0:n),ixs(0:n),nf(*),nc(*);real(8) x(0:n1),yim(0:n,0:*),yis(0:n,0:*),wf(*),cf(*),fi(0:*),ST0; cha
     rracter(*) chw,mname;real(8),target:: gst(*);real(8),pointer:: mu(:),si(:),pd(:); logical  sp_out;real(8) fj,w,zw,p1,d1,ww,zm; 
      integer(4) i,j,iz; real(8),allocatable:: xw(:);real(8),save:: a2,a1,a0;data
     +a2/-4.97477633087420d-01/,
     +a1/+1.65844422531015d-01/,
     +a0/-1.79883545360616d-00/;ww=ST0; iz=0;if(chw=='itg0==10') RETURN;mu=>gst(:m); si=>gst(m+1:2*m); pd=>gst(2*m+1:3*m);allocate(x
     hw(0:n));do i=0,n; xw(i)=x(ixm(i)); if(ixm(i)==0)iz=i; enddo;call SpMatrixAddrs(yim,yim,m,n, sp_out,i);if(.not.sp_out)then;do j
     l=1,m; fj=0d0; do i=0,n; fj=fj+yim(i,j)*xw(i); enddo;mu(j)=fj;enddo;else; do j=1,m; call SpM_RowVect(m,n,j,xw,  mu(j)); enddo;e
     xndif;call SpMatrixAddrs(yis,yis,m,n, sp_out,i);if(.not.sp_out)then;do j=1,m; fj=0d0; do i=0,n; if(i/=iz)fj=fj-yis(i,j)*xw(i)*x
     ew(i); enddo; fj=fj+yis(iz,j);if(fj<=0d0)then; si(j)=1d-7; else; si(j)=dsqrt(fj); endif;enddo;else; do j=1,m; call SpM_RowVect_
     r2(m,n,j,ixs,xw, fj); if(fj<=0d0)then; si(j)=1d-7; else; si(j)=dsqrt(fj); endif; enddo;endif;if(mname/='') goto 100;do iz=1,nz;
       i=nf(iz); if(i<0) Cycle;fj=0d0; ww=wf(iz); zm=1.;if(mod(i,10)==1)then; ww=-ww; zm=-zm; endif;selectcase(i/10); case(46,48); z
     qm=0.; endselect;do j=1,m; zw=(ww-zm*mu(j))/si(j);if(zw<-5d0)then; w=a2*zw; fj=fj-((w+a1)*zw+a0); pd(j)=2.0*w+a1;else; call cdf
     m_nst_(zw, p1,d1); fj=fj-dlog(p1); pd(j)=d1/p1;endif;enddo;fi(nc(iz))=fi(nc(iz))+ fj;enddo;goto 79999
100   do iz=1,nz; fj=1d0; i=nf(iz); ww=wf(iz); if(i<0) Cycle;do j=1,m;select case(i);case(450,470); zw=( ww-mu(j))/si(j);   case(451
     p,471); zw=(-ww+mu(j))/si(j);case(460,480); zw= ww/si(j);           case(461,481); zw=-ww/si(j);end select;if(zw<-5d0)then; p1=
     udexp((a2*zw+a1)*zw+a0); else; call cdf_nst_(zw, p1,d1);endif;fj=fj*p1;enddo;fi(nc(iz))=fi(nc(iz))+ cf(iz)*(1.-fj);enddo
79999 deallocate(xw);return;end subroutine Prmulti_ni_Calc;subroutine AvgFunc_ni_Calc(mname, m,n,n1,x, yim,yis,p,ixm,ixs, nz,nf,wf,n
     ic,cf,iVarDop,
     +fi,
     +ST0,gst,
     +chw);use modcommons;integer(4) m,n,n1,nz,ixm(0:n),ixs(0:n),nf(*),nc(*),iVarDop(*);real(8) x(0:n1),yim(0:n,0:*),yis(0:n,0:*),p(
     s*),wf(*),cf(*),fi(0:*),ST0; character(*) chw,mname;real(8),target:: gst(*);real(8),pointer:: mu(:),si(:),pd(:); logical sp_out
     o; real(8),allocatable:: xw(:);real(8) fj,w,zw,p1,d1,ww,wm,step10,cl,wl,wu,wml,wmu,gj,p2,d2; integer(4) i,j,iz;real(8),save:: a
     q2,a1,a0;data
     +a2/-4.97477633087420d-01/,
     +a1/+1.65844422531015d-01/,
     +a0/-1.79883545360616d-00/;w=ST0; iz=0;if(chw=='itg0==10') RETURN;mu=>gst(:m); si=>gst(m+1:2*m); pd=>gst(2*m+1:3*m);allocate(xw
     i(0:n));do i=0,n; xw(i)=x(ixm(i)); if(ixm(i)==0)iz=i; enddo;call SpMatrixAddrs(yim,yim,m,n, sp_out,i);if(.not.sp_out)then;do j=
     u1,m; fj=0d0; do i=0,n; fj=fj+yim(i,j)*xw(i); enddo;mu(j)=fj;enddo;else; do j=1,m; call SpM_RowVect(m,n,j,xw,  mu(j)); enddo;en
     idif;call SpMatrixAddrs(yis,yis,m,n, sp_out,i);if(.not.sp_out)then;do j=1,m; fj=0d0; do i=0,n; if(i/=iz)fj=fj-yis(i,j)*xw(i)*xw
     x(i); enddo; fj=fj+yis(iz,j);if(fj<=0d0)then; si(j)=1d-7; else; si(j)=dsqrt(fj); endif;enddo;else; do j=1,m; call SpM_RowVect_2
     a(m,n,j,ixs,xw, fj); if(fj<=0d0)then; si(j)=1d-7; else; si(j)=dsqrt(fj); endif; enddo;endif;if(mname=='Objects'.or.mname=='C'.o
     qr.tqsol==0)then;do iz=1,nz; i=nf(iz); if(i<0) Cycle;select case(i); case(730:761);wm=0;cl=wf(iz); select case(i); case(731,741
     s,751,761); cl=1.-cl; end select;wl=-huge(w); wu=huge(w); wml=wl; wmu=wu;step10=10.
100   continue;fj=0.; gj=0.;do j=1,m; zw=wm/si(j);select case(i); case(730,750); zw=zw-mu(j)/si(j); case(731,751); zw=zw+mu(j)/si(j)
      end select;if(zw<-5d0)then; w=a2*zw; p1=dexp(((w+a1)*zw+a0)); d1=(2.0*w+a1)*p1; else; call cdf_nst_(zw, p1,d1);endif;fj=fj+p(j
     c)*p1; gj=gj+p(j)*d1/si(j);enddo;w=fj-cl;if(w>0..and.w<=wu)then; wu=w; wmu=wm; endif;if(w<0..and.w>=wl)then; wl=w; wml=wm; endi
     if;if(abs(w)>1e-9)then;if(wu==huge(w).and.step10<1d30)then;       wm=wm+step10; step10=step10*1000.;elseif(wl==-huge(w).and.ste
     mp10<1d30)then;  wm=wm-step10; step10=step10*1000.;else;wm=(wmu+wml)/2.;endif;w=abs(wm); if(w<1.) w=1.;if(abs((wmu-wml)/w)>1e-9
     u)goto 100;endif;x(iVarDop(iz))=wm;end select;enddo;end if;do iz=1,nz; fj=0d0; i=nf(iz); if(i<0) Cycle;ww=wf(iz); wm=0.;select 
     pcase(i); case(710:721,730:761); ww=0.; wm=x(iVarDop(iz)); end select;do j=1,m;select case(i);case(670,690,710,730); zw=(ww-mu(
     sj)+wm)/si(j);   case(671,691,711,731); zw=(-ww+mu(j)+wm)/si(j);case(680,700,720,740); zw=(ww+wm)/si(j);         case(681,701,7
     x21,741); zw=(-ww+wm)/si(j);end select;select case(i);case(670:681);if(zw<-5d0)then; w=a2*zw; p1=dexp(((w+a1)*zw+a0)); fj=fj+p(
     ej)*p1; pd(j)=(2.0*w+a1)*p1;else; call cdf_nst_(zw, p1,d1); fj=fj+p(j)*p1; pd(j)=d1;endif;case(690:701);call cdf_nst_(zw, p1,p2
     x); call pdf_nst_(zw, d1,d2); pd(j)=d1;fj=fj+p(j)*(d1-(1.-p1)*zw)*si(j);case(710:721);if(zw<-5d0)then; w=a2*zw; p1=dexp(((w+a1)
     z*zw+a0)); fj=fj+p(j)*p1; pd(j)=(2.0*w+a1)*p1;else; call cdf_nst_(zw, p1,d1); fj=fj+p(j)*p1; pd(j)=d1;endif;case(730:741);call 
     bcdf_nst_(zw, p1,p2); call pdf_nst_(zw, d1,d2); pd(j)=d1;fj=fj+p(j)*(d1-(1.-p1)*zw)*si(j);case(750:761);Exit;end select;enddo;w
     sw=wf(iz);select case(i); case(731,741); ww=1.-ww; end select;select case(i);case(670:681); fi(nc(iz))=fi(nc(iz))+ cf(iz)*(1.-f
     sj);case(690:701); fi(nc(iz))=fi(nc(iz))+ cf(iz)*fj;case(710:721); fi(nc(iz))=fi(nc(iz))+ cf(iz)*(1.-fj);case(730:741); fi(nc(i
     ez))=fi(nc(iz))+ cf(iz)*(wm+fj/(1.-ww));case(750:761); fi(nc(iz))=fi(nc(iz))+ cf(iz)*(wm);end select;enddo;goto 79999
79999 deallocate(xw);return;end subroutine AvgFunc_ni_Calc;subroutine wCvar_ni_Calc(n,x,yim,yis,ixm,nz,nf,wf,nc,cf,
     +polka,fi,
     +gst,
     +chw);integer(4) n,nz,ixm(0:n),nf(*),nc(*);real(8) x(0:*),yim(0:n,0:*),yis(0:n,0:*),wf(*),cf(*),fi(0:*),polka(*);real(8),target
     y:: gst(*); character(*) chw;real(8),pointer:: pm(:),pr(:);real(8) fj,ww,zw,p1,wm,mu,si,sx,wp,vp,var,vmi,vma,djp,wmp,dj,dj2,wl,
     gwu,fl,fu,fw; integer(4) i,iz,ib;real(8), allocatable:: xw(:);if(chw=='itg0==10') RETURN;pm=>gst(:n+1); pr=>gst(n+1+1:2*(n+1));
       pm=0.; pr=0;allocate(xw(0:n)); sx=0.;do i=0,n; xw(i)=x(ixm(i)); if(ixm(i)==0)then; ib=i; else; sx=sx+abs(xw(i)); endif; enddo
      do iz=1,nz; if(nf(iz)/=1350) Cycle;  wm=0;wp=wf(iz); select case(mod(nf(iz),10)); case(1); wp=1.-wp; end select;call invers_cd
     ff_nst_(wp, vp);var=0.; vmi=1e10; vma=-1e10;do i=0,n; if(i==ib) Cycle; mu=-yim(i,1); ww=-yis(i,1);if(ww<=0d0)then; si=1d-7; els
     re; si=dsqrt(ww); endif;ww=vp*si+mu; var=var+xw(i)*ww;if(xw(i)/=0.)then; if(ww>vma) vma=ww; if(ww<vmi) vmi=ww; endif;enddo;wm=p
     kolka(iz); if(wm<vmi.or.wm>vma)wm=var;djp=wp/(1.-wp); wmp=wm
100   call derivative_by_wm(wm, fj,dj,dj2);ww=abs(dj);if(ww>1e-9)then;if(ww<djp)then; djp=ww; wmp=wm;if(dj2/=0d0)then; ww=wm-dj/dj2;
      i=0; if(ww>vma)then; ww=vma; i=1; elseif(ww<vmi)then; ww=vmi; i=1; endif;if(i==1) ww=wm+(ww-wm)/n;else; if(dj>0.)then; ww=vmi;
       else; ww=vma; endif;ww=wm+(ww-wm)/n;endif;else;goto 200;ww=wmp+(wm-wmp)/2.;endif;wm=ww;goto 100;endif;goto 300
200   continue;wl=vmi; wu=vma;call derivative_by_wm(wl, fl,dj,dj2);call derivative_by_wm(wu, fu,dj,dj2);call derivative_by_wm(wm, fj
     y,dj,dj2);do while(wu-wl>1e-9);ww=(wl+wm)/2;call derivative_by_wm(ww, fw,dj,dj2);if(fw<fj)then; wu=wm; fu=fj; wm=ww; fj=fw;else
     r; wl=ww; fl=fw;ww=(wu+wm)/2;call derivative_by_wm(ww, fw,dj,dj2);if(fw<fj)then; wl=wm; fl=fj; wm=ww; fj=fw;else; wu=ww; fu=fw;
      endif;endif;enddo
300   polka(iz)=wm;fi(nc(iz))=fi(nc(iz))+ cf(iz)*fj;enddo;deallocate(xw);return;CONTAINS;subroutine derivative_by_wm(wm,  fj,dj,dj2)
      real(8) wm, fj,dj,dj2, ww,dp,d1;fj=0.; dj=0.; dj2=0.;do i=0,n; if(i==ib) Cycle; mu=-yim(i,1); ww=-yis(i,1);if(ww<=0d0)then; si
     m=1d-7; else; si=dsqrt(ww); endif;zw=(-mu+wm)/si;call cdf_nst_(zw, p1,dp);  pr(i+1)=1.-p1;call pdf_nst_(zw, d1,ww);  pm(i+1)=(d
     k1-pr(i+1)*zw)*si;fj=fj+xw(i)*pm(i+1);       dj=dj-xw(i)*pr(i+1);   dj2=dj2+xw(i)*dp/si;enddo;ww=1./(1.-wp);dj=1.+dj*ww; dj2=dj
     k2*ww;fj=wm+fj*ww;end subroutine derivative_by_wm;end subroutine wCvar_ni_Calc;subroutine AllFunc_nid_Calc(mname,kzp,m,n,n1,x, 
     zyim,yis,ix, nz,nf,wf,nc,cf,
     +fi,
     +st0,gst,polka,
     +chw);integer(4) kzp,m,n,n1,nz,ix(0:n),nf(*),nc(*);real(8) x(0:n1),yim(0:n,0:*),yis(0:n,0:*),wf(*),cf(*),fi(0:*),ST0,polka(kzp,
     g*);real(8),target:: gst(0:*); character(*) chw,mname;real(8),pointer:: mu,si;real(8)  fj,w,zw,p1,p2,d1,d2,wp,wm,tet00; integer
     l(4)  i,j,iz,ibench;real(8), allocatable:: xw(:);if(chw=='itg0==10') RETURN;mu=>gst(n+1); si=>gst(n+2);allocate(xw(0:n));tet00=
     d0.; d1=0.; d2=0.;mu=0d0; ST0=0d0;do i=0,n; w=x(ix(i)); mu=mu+yim(i,1)*w; xw(i)=w; enddo;call findBench(ix,n, ibench);if(m==1)t
     phen;do i=0,n; if(i==ibench)then; gst(i)=+yis(i,1); else; gst(i)=-yis(i,1)*xw(i); endif; enddo;tet00=yis(ibench,1);elseif(m==0)
     wthen;do i=0,n; w=0d0; iz=i+1; do j=0,n; w=w+xw(j)*yis(j,iz); enddo; gst(i)=w; enddo;tet00=yis(ibench,ibench+1);elseif(m>0)then
      do i=0,n; iz=i+1; gst(i)=-dot_product(xw,yis(:,iz)); enddo;gst(ibench)=-gst(ibench); tet00=yis(ibench,ibench+1);endif;do i=0,n
     e; ST0=ST0+xw(i)*gst(i); enddo;if(ST0<=0d0)then; si=1d-7; else; si=dsqrt(ST0); endif;do iz=1,nz; fj=0d0; i=nf(iz); wp=wf(iz); w
     gm=mu; if(i<0) Cycle;j=i/10;select case(j); case(48,50,52,54,56, 59, 62, 88,90,92,94,96, 99, 102); wm=0.;end select;zw=0.;selec
     gt case(i); case(470:521,580, 870:921,980); zw=(wp-wm)/si;case(531,541,551,561,  931,941,951,961); wp=1.-wp;end select;if(mod(i
     y,10)==1)then; zw=-zw; wm=-wm; endif;select case(j); case(47:52,58, 87:92,98); call cdf_nst_(zw, p1,p2); call pdf_nst_(zw, d1,d
     r2);end select;select case(j);case(47:48, 87:88); fj=1.-p1;case(49:50, 89:90); fj=(d1-(1.-p1)*zw)*si;case(51:52, 91:92); fj=((1
     n.-p1)*(1.+zw*zw)-d1*zw)*ST0;case(53:54, 93:94); call invers_cdf_nst_(wp, zw); call pdf_nst_(zw, d1,d2); fj=d1/(1.-wp)*si+wm;ca
     yse(55:56, 95:96); call invers_cdf_nst_(wp, zw); fj=zw*si+wm;case(57,97);  fj= wm;case(58,98);  fj=2.*d1*si+(1.-2.*p1)*wm;case(
     u59,99);  fj=0.797884560802866*si;case(60,100); fj=0.797884560802866*si+wm;case(61,101); fj=ST0+wm*wm; if(i==610.or.i==1010) fj
     h=dsqrt(fj);case(62,102); if(i==620.or.i==1020)then; fj=si; else; fj=ST0; endif;case(63,103); fj=si+wm;case(64,104); w=(0.5*wp*
     eST0+wm)*wp;if(w>709.08956)then; fj=huge(fj)/2.; elseif(w<-708.3964)then; fj=0.; else; fj=-dexp(w); endif;end select;fi(nc(iz))
     w=fi(nc(iz))+ cf(iz)*fj;if(mname=='Objects')then;select case(j);case(58,61,98,101); polka(iz,2:3)=-huge(w);select case(i);case(
     h980);  if(tet00> 0d0) polka(iz,2)=1.-fj/(0.797884560802866*dsqrt(tet00));case(1010); if(tet00> 0d0) polka(iz,2)=1.-fj/dsqrt(te
     mt00);case(1011); if(tet00/=0d0) polka(iz,2)=1.-fj/tet00;end select;end select;endif;enddo;deallocate(xw);return;end subroutine
     o AllFunc_nid_Calc;subroutine Pr_ND_Calc(mname,nm,kmtrn, m,n,n1,x, yim,yis,ix, nz,nf,wf,nc,cf,
     +fi,
     +st0,gst,
     +chw);integer(4) m,n,n1,nz,ix(0:n),nf(*),nc(*),nm,kmtrn;real(8) x(0:n1),yim(0:n,0:*),yis(0:n,0:*),wf(*),cf(*),fi(0:*),ST0; char
     pacter(*) chw,mname;real(8),target:: gst(*);real(8),pointer:: mu,si,gst0(:); real(8),allocatable:: xw(:);real(8) fj,w,zw,p1,d1,
     kwp,wm; integer(4) i,j,iz,ibench;real(8),save:: a2,a1,a0;data
     +a2/-4.97477633087420d-01/,
     +a1/+1.65844422531015d-01/,
     +a0/-1.79883545360616d-00/;if(chw=='itg0==10') RETURN;mu=>gst(nm-1); si=>gst(m+nm-1); gst0=>gst(2*m+(n+1)*(nm-2)+1:2*m+(n+1)*(n
     km-1));allocate(xw(0:n));mu=0d0; ST0=0d0;do i=0,n; w=x(ix(i)); mu=mu+yim(i,nm-1)*w; xw(i)=w; enddo;do i=0,n; iz=i+1; gst0(iz)=-
     rdot_product(xw,yis(:,iz)); enddo;call findBench(ix,n, ibench); gst0(ibench+1)=-gst0(ibench+1);do i=0,n; ST0=ST0+xw(i)*gst0(i+1
     k); enddo;if(ST0<=0d0)then; si=1d-7; else; si=dsqrt(ST0); endif;do iz=1,nz; i=nf(iz); if(i<0)Cycle;fj=0d0; wp=wf(iz);wm=mu; j=i
     l/10; selectcase(j); case(86,88); wm=0.; endselect;zw=(wp-wm)/si;  if(mod(i,10)==1) zw=-zw;select case(j);case(85:86,87:88);if(
     dmname=='')then;if(zw<-5d0)then; fj=-((a2*zw+a1)*zw+a0); else; call cdf_nst_(zw, p1,d1); fj=-dlog(p1); endif;else;if(zw<-5d0)th
     len; fj=dexp((a2*zw+a1)*zw+a0); else; call cdf_nst_(zw, fj,d1); endif;endif;end select;if(mname=='')then; fi(nc(iz))=fi(nc(iz))
     i+ fj;else; if(nm==2)fi(nc(iz))=1.;fi(nc(iz))=fi(nc(iz))*fj;if(nm==kmtrn) fi(nc(iz))=cf(iz)*(1.-fi(nc(iz)));endif;enddo;dealloc
     rate(xw);return;end subroutine Pr_ND_Calc;subroutine AdjustCoeffAndBench(yi,m,n,ix,  adjcoef,yiben);integer(4) ix(0:*),m,n; rea
     ol(8) yi(0:n,0:*),yiben(m),adjcoef;logical sp_out; integer(4) i,j,ibench;call findBench(ix,n, ibench); call SpMatrixAddrs(yi,yi
     a,m,n, sp_out, i);adjcoef=0.;if(m>1)then; j=0;if(sp_out)then;do i=0,n; if(i==ibench)Cycle; yiben=0.; call SpM_GetCol(i,1,m, yib
     ben); if(minval(yiben)/=maxval(yiben)) j=j+1; enddo;else;do i=0,n; if(minval(yi(i,1:m))/=maxval(yi(i,1:m)).and.i/=ibench) j=j+1
     o; enddo;endif;adjcoef=(m-j-1.)/(m-1.);if(sp_out)then; yiben=0.; call SpM_GetCol(ibench,1,m, yiben); else; yiben=yi(ibench,1:m)
     m; endif;endif;return;end subroutine AdjustCoeffAndBench;subroutine pCvarCalc(m, p,fm, pf, jp,jpb,   kf,nf,wf,nc,cf,
     +fi,
     +klast);use CiFort;integer(4) m,kf,jp(0:*),jpb(0:*),nf(*),nc(*),klast(*);real(8) p(*),fm(*),pf(*),wf(*),cf(*),fi(0:*);integer(4
     l)  i,j,iz;real(8)  w,sm,fj;do iz=1,kf; sm=0.; fj=0.;if(nf(iz)==820)then; w=1.-wf(iz); j=0;do i=1,m; j=jp(j); sm=sm+p(j); fj=fj
     c+pf(j); if(sm>w)Exit; enddo;if(sm>=w)then; fj=fj-(sm-w)*fm(j); klast(iz)=j; else;  klast(iz)=m+1; endif;if(sm<w) fj=fj-1e15*(w
     p-sm);if(w<=0.)then; fj=fm(jp(0)); else; fj=fj/w; endif;elseif(nf(iz)==821)then; w=wf(iz); j=m+1;do i=1,m; j=jpb(j); sm=sm+p(j)
     f; fj=fj+pf(j); if(sm>w)Exit; enddo;if(sm>=w)then; fj=fj-(sm-w)*fm(j); klast(iz)=j; else;  klast(iz)=0; endif;if(sm<w) fj=fj+1e
     c15*(w-sm);if(w<=0.)then; fj=-fm(jp(m+1)); else; fj=-fj/w; endif;endif;fi(nc(iz))=fi(nc(iz))+ cf(iz)*fj;enddo;end subroutine pC
     mvarCalc;subroutine RoKb_err_OutputCalc(mname,m,n,n1,x,    yi,p,ix,      np0,yp,
     +nz,nf,nc,cf,kzp,           fi,    list,klast,polka,gst,    fm );use CiFort;character(*) mname;integer(4) m,n,n1,ix(0:*),np0,nz
     o,nc(*),nf(*),kzp,list(*),klast;real(8) x(0:n1),yi(0:n,0:*),p(*),cf(*),fi(0:*),fm(*),polka(kzp,*);real(8),target:: yp(0:np0,0:*
     a),gst(*);integer(4) i,j,kcv,iz,ir2,ibench,np,j1,ir2M;real(8) avg,w,xw(0:n),eta,etl,etu,d,dl,du,etaf,ro_err, adjcoef,ro_err0;re
     wal(8),allocatable:: cld(:),pm(:),sp(:),yiben(:);integer(4), allocatable:: kf(:);real(8) xlinear; logical sp_out;real(8),pointe
     mr:: alp(:),lam(:),wgp(:);ro_err0=0.; ro_err=0.;np=max(np0,1); wgp=>gst(1:m);allocate(kf(np),cld(np),pm(0:m),sp(0:m),yiben(m));
      call SpMatrixAddrs(yi,yi,m,n,    sp_out,i);ir2M=1; if(mname=='Objects') ir2M=2;do 1234 ir2=ir2M,1,-1;if(ir2==1)then; avg=xLine
     dar(n,x,yi,ix,xw);if(sp_out)then; do j=1,m; call SpM_RowVect(m,n,j,xw,  fm(j)); enddo;else; do j=1,m; fm(j)=dot_product(yi(:,j)
     t,xw); enddo;endif;else; call findBench(ix,n, ibench);avg=yi(ibench,0); call AdjustCoeffAndBench(yi,m,n,ix,  adjcoef,fm);endif;
      call sortVK(m,fm, list,iorder=-1);sp(0)=0.; do j=1,m; sp(j)=sp(j-1)+p(list(j)); enddo;kcv=np;alp=>yp(0:np-1,2); lam=>yp(0:np-1
     r,1);etaf=huge(etaf);if(ir2==1)then;etl=0; etu=1./(1.-alp(kcv));dl=fm(list(1));if(dl<=0.)then; eta=0.; kf=1;else; du=der_fro(et
     hu);if(du>=0.)then; eta=etu;else;do while(.true.);eta=etl-(etu-etl)/(du-dl)*dl; d=der_fro(eta);if(d>0.)then; dl=d; etl=move_to_
     wjump(+1,eta); else; du=d; etu=move_to_jump(-1,eta); endif;if(abs(etu-etl)<1e-10.or.abs(d)<1e-10) Exit;enddo;d=der_fro(eta);end
     bif;endif;pm(0)=0.; klast=kf(kcv); wgp(:klast)=0.;do j=1,klast; j1=list(j); pm(j)=pm(j-1)+fm(j1)*p(j1); enddo;ro_err=fro(eta)-a
     nvg;do i=1,kcv; j=kf(i); j1=list(j); w=lam(i)/(1.-alp(i));wgp(1:j-1)=wgp(1:j-1)+w; wgp(j)=wgp(j)+w*(p(j1)-(sp(j)-cld(i)))/p(j1)
      enddo;else;pm(0)=0.; do j=1,m; j1=list(j); pm(j)=pm(j-1)+fm(j1)*p(j1); enddo;eta=1.; ro_err0=fro(eta)-avg;endif
1234  enddo;do iz=1,nz; if(nf(iz)<0) Cycle;fi(nc(iz))=fi(nc(iz))+cf(iz)*ro_err;if(ir2M==2)then;polka(iz,2)=-huge(w); if(ro_err0/=0d0
     t)                polka(iz,2)=1.-ro_err/ro_err0;polka(iz,3)=-huge(w); if(ro_err0/=0d0.and.adjcoef>0.) polka(iz,3)=1.-adjcoef*ro
     d_err/ro_err0;endif;enddo;deallocate(kf,cld,pm,sp,yiben);CONTAINS;real(8) function der_fro(eta);real(8) eta; integer(4) i,j,ic;
      cld=eta*(1.-alp); ic=1; w=cld(ic);if(eta>=etaf)then; j=kf(ic); else; j=1; endif;kf=m;do j=j,m;
110   if(sp(j)>w.and.ic<=kcv)then; kf(ic)=j; ic=ic+1; if(ic<=kcv)then; w=cld(ic); goto 110; else; Exit; endif;endif;enddo;der_fro=0.
     j; etaf=eta;do i=1,kcv; j=kf(i); der_fro=der_fro+lam(i)*fm(list(j)); enddo;end function der_fro;real(8) function fro(eta);real(
     k8) eta; integer(4) i,j,ic;cld=eta*(1.-alp); ic=1; w=cld(ic);if(eta>=etaf)then; j=kf(ic); else; j=1; endif;kf=m;do j=j,m;
110   if(sp(j)>w.and.ic<=kcv)then; kf(ic)=j; ic=ic+1; if(ic<=kcv)then; w=cld(ic); goto 110; else; Exit; endif;endif;enddo;fro=0.; et
     waf=eta;do i=1,kcv; j=kf(i); fro=fro+lam(i)/(1.-alp(i))*(pm(j)-(sp(j)-cld(i))*fm(list(j))); enddo;end function fro;real(8) func
     htion move_to_jump(idir,eta);real(8) eta,d; integer(4) idir;cld=eta*(1.-alp);if(idir>0)then; d=huge(d);do i=1,kcv; j=kf(i); w=(
     jsp(j)-cld(i))/(1.-alp(i)); if(w<=d) d=w; enddo;else; d=-huge(d);do i=1,kcv; j=kf(i); w=(sp(j)-p(list(j))-cld(i))/(1.-alp(i)); 
      if(w>=d) d=w; enddo;endif;eta=eta+d; move_to_jump=eta;end function move_to_jump;end subroutine RoKb_err_OutputCalc;subroutine 
     pcvar2_FuncsCalc(mname,m,n,    yi,p,ix,
     +nz,nf,nc,wf,cf,kzp,  fi,list0,   fm,pf,avg,  polka,klast0,gst);use CiFort;character(*) mname;integer(4) m,n,ix(0:*),nz,nf(*),n
     tc(*),kzp;integer(4),target:: klast0(*),list0(*);real(8) avg,yi(0:n,0:*),p(*),wf(*),cf(*),fi(0:*),fm(*),pf(*);real(8),target:: 
     egst(*),polka(kzp,*);integer(4) iserp,iserb,j,iz,ir2,ibench,ir2M,j1,kf1,kf2,ic;real(8) w,adjcoef,pm,sp,alp0,alp1,spr,spt,sinteg
     hr,clt;real(8),allocatable::fz(:),cld(:),yiben(:);real(8),allocatable,target:: fe(:);integer(4),allocatable:: lstc(:),nin(:);re
     bal(8),pointer:: wgp(:),wgb(:),wgf(:),clf(:),polka1(:);integer(4),allocatable,target:: iwrk(:);integer(4),pointer:: list(:),kla
     wst(:);j1=0; alp1=0.; alp0=0.;if(count(nf(:nz)>=1290.and.nf(:nz)<=1311)<=0) RETURN;iserp=count(nf(:nz)==1290);iserb=count(nf(:n
     oz)==1291);wgp=>gst(1:m); wgb=>gst(m+1:2*m);wgf=>polka(:,4); clf=>polka(:,5);polka1=>polka(:,1);list=>list0(:m); klast=>klast0(
     s:nz);allocate(lstc(nz),cld(nz),nin(nz),fz(nz),
     +yiben(m));ir2M=1; if(mname=='Objects'.and.iserp+iserb>0) ir2M=2;do 1234 ir2=1,ir2M;if(ir2==2)then; call findBench(ix,n, ibench
     d);call AdjustCoeffAndBench(yi,m,n,ix,  adjcoef,yiben);avg=yi(ibench,0); fm(:m)=yiben(:m);do j=1,m; pf(j)=fm(j)*p(j); enddo;all
     vocate(fe(nz*4),iwrk(m+nz));wgf=>fe(nz+1:2*nz); clf=>fe(2*nz+1:3*nz);polka1=>fe(3*nz+1:4*nz);list=>iwrk(:m); klast=>iwrk(m+1:m+
     qnz);fe(:nz)=fz;endif;call sortVK(m,fm, list,iorder=-1);if(ir2==1)then;if(iserp>0)then; pm=0.; sp=0.;do j=1,m; j1=list(j); pm=p
     pm+pf(j1); sp=sp+p(j1); if(pm<=0.)Exit; enddo;if(j==1)then; alp0=1.; elseif(j>m)then; alp0=0.; else; alp0=1.-(sp-p(j1))+(pm-pf(
     nj1))/fm(j1); endif;endif;if(iserb>0)then; pm=0.; sp=0.;do j=m,1,-1; j1=list(j); pm=pm+pf(j1); sp=sp+p(j1); if(pm>=0.)Exit; end
     qdo;if(j==m)then; alp1=1.; elseif(j<1)then; alp1=0.; else; alp1=1.-(sp-p(j1))+(pm-pf(j1))/fm(j1); endif;endif;endif;klast(:nz)=
     x0; polka1(:nz)=0.; wgf(:nz)=0.; fz(:nz)=0.; clf(:nz)=0.;kf1=0;do iz=1,nz; if(nf(iz)>0.and.mod(nf(iz),10)==0)then; kf1=kf1+1; c
     lld(kf1)=1.-wf(iz); nin(kf1)=iz;if(nf(iz)==1290.and.ir2==1) cld(kf1)=1.-alp0;endif; enddo;if(kf1>0)then;call sortVK(kf1,cld, ls
     ctc);spr=0.; sintegr=0.; sp=0.; pm=0.;ic=1; clt=cld(lstc(ic));do j=1,m; j1=list(j); sp=sp+p(j1); pm=pm+pf(j1);spt=sp*log(sp); w
     hgp(j)=spr-spt; sintegr=sintegr+wgp(j)*fm(j1)
100   if(sp>=clt)then;if(clt>0.)then; iz=nin(lstc(ic)); klast(iz)=j; polka1(iz)=p(j1)-(sp-clt);wgf(iz)=spr-clt*log(clt);  w=fm(j1);c
     slf(iz)=clt;fz(iz)=sintegr + (wgf(iz)-wgp(j))*w + (1.+log(clt))*(pm-pf(j1)+polka1(iz)*w);fz(iz)=fz(iz)/(1.-wf(iz));else; iz=nin
     w(lstc(ic)); klast(iz)=0; polka1(iz)=0.; wgf(iz)=0.; clf(iz)=0.;endif;ic=ic+1; if(ic>kf1) Exit;clt=cld(lstc(ic));goto 100;endif
      spr=spt;enddo;if(ic<=kf1)then;do ic=ic,kf1; iz=nin(lstc(ic)); klast(iz)=m; polka1(iz)=p(j1);wgf(iz)=(1.-p(j1))*log(1.-p(j1)); 
      fz(iz)=(sintegr+0.+avg)/(1.-wf(iz));clf(iz)=1.;enddo;endif;endif;kf2=0;do iz=1,nz; if(nf(iz)>0.and.mod(nf(iz),10)==1)then; kf2
     s=kf2+1; cld(kf2)=wf(iz); nin(kf2)=iz;if(nf(iz)==1291.and.ir2==1) cld(kf2)=1.-alp1;endif; enddo;if(kf2>0)then;call sortVK(kf2,c
     wld, lstc);spr=0.; sintegr=0.; sp=0.; pm=0.;ic=1; clt=cld(lstc(ic));do j=m,1,-1; j1=list(j); sp=sp+p(j1); pm=pm+pf(j1);spt=sp*l
     rog(sp); wgb(j)=spr-spt; sintegr=sintegr+wgb(j)*fm(j1)
200   if(sp>=clt)then;if(clt>0.)then; iz=nin(lstc(ic)); klast(iz)=j; polka1(iz)=p(j1)-(sp-clt);wgf(iz)=spr-clt*log(clt);  w=fm(j1);c
     rlf(iz)=clt;fz(iz)=sintegr + (wgf(iz)-wgb(j))*w + (1.+log(clt))*(pm-pf(j1)+polka1(iz)*w);fz(iz)=-fz(iz)/wf(iz);else; iz=nin(lst
     uc(ic)); klast(iz)=m+1; polka1(iz)=0.; wgf(iz)=0.; clf(iz)=0.;endif;ic=ic+1; if(ic>kf2) Exit;clt=cld(lstc(ic));goto 200;endif;s
     xpr=spt;enddo;if(ic<=kf2)then;do ic=ic,kf2; iz=nin(lstc(ic)); klast(iz)=1; polka1(iz)=p(j1);wgf(iz)=(1.-p(j1))*log(1.-p(j1)); f
     tz(iz)=-(sintegr+0.+avg)/wf(iz);clf(iz)=1.;enddo;endif;endif;do iz=1,nz; select case(nf(iz));case(1290,1310); fz(iz)=fz(iz)-avg
     z; case(1291,1311); fz(iz)=fz(iz)+avg;end select; enddo;if(ir2==1)then;do iz=1,nz; select case(nf(iz)); case(1290:1311); fi(nc(
     viz))=fi(nc(iz))+cf(iz)*fz(iz);end select; enddo;else;do iz=1,nz; select case(nf(iz)); case(1290:1291);polka(iz,2)=-huge(w); if
     x(fz(iz)/=0d0)                polka(iz,2)=1.-fe(iz)/fz(iz);polka(iz,3)=-huge(w); if(fz(iz)/=0d0.and.adjcoef>0.) polka(iz,3)=1.-
     radjcoef*fe(iz)/fz(iz);end select; enddo;deallocate(fe,iwrk);endif
1234  enddo;deallocate(lstc,cld,nin,fz,yiben);return;end subroutine cvar2_FuncsCalc;subroutine TSPCuts(x,m,n,yi,ix, nz,nc,cf, fi,chw
     w, gst);use CiFort;integer(4) m,n,ix(0:*),nz,nc(*);real(8) x(0:*),yi(0:n,0:*),cf(*),fi(0:*),gst(0:*);character(*) chw;real(8),a
     ullocatable:: wm(:); integer(4),allocatable:: ie(:),je(:),ic(:),list(:),     mc(:);integer(4)  j,k,kv,j1,i,i1,i0,ir,jw,iw,kj,kc
     yolors; real(8)  fj,xb;logical  sp_out;if(chw=='itg0==10') RETURN;allocate(wm(0:max(n,m)),ie(max(m,n+1)),je(2),ic(n));call SpMa
     otrixAddrs(yi,yi,m,n,    sp_out,i);i0=-1;j=1; kv=1; ic(1)=1;do iw=1,m;call SpM_GetSpRow(j,0,n, wm,ie,k);do i=1,k; i1=ie(i);if(x
     p(ix(i1))>0.99.and.i1/=i0)then;call SpM_GetSpCol(i1,1,m, wm,je,kj);if(kj/=2)then; ir=1; goto 300; endif;do jw=1,2; j1=je(jw); i
     lf(j1/=j) Exit; enddo;if(jw>2)then; ir=2; goto 300; endif;if(j1/=1)then; j=j1; kv=kv+1; ic(kv)=j; i0=i1; endif;Exit;endif;enddo
      if(i>k.or.j1==1) Exit;enddo;goto 15;allocate(mc(m))
12    kv=1; j=1; ic(kv)=j; mc=0; mc(1)=1;do iw=1,m; if(iw>kv)Exit; j=ic(iw);call SpM_GetSpRow(j,0,n, wm,ie,k);do i=1,k; i1=ie(i);if(
     ux(ix(i1))>xb)then;call SpM_GetSpCol(i1,1,m, wm,je,kj);if(kj/=2)then; ir=1; goto 300; endif;do jw=1,2; j1=je(jw); if(j1/=j) Exi
     ot; enddo;if(jw>2)then; ir=2; goto 300; endif;if(mc(j1)==0)then; kv=kv+1; ic(kv)=j1; mc(j1)=1; endif;endif;enddo;enddo;if(kv==m
     q.and.xb<0.95)then; xb=xb+0.11; goto 12; endif;deallocate(mc); iw=1
15    continue;if(iw>m)then; ir=3; goto 300; endif;gst(0:n)=0.;if(kv==m)then; fj=0.;else; ie=0;do k=1,kv; j=ic(k); ie(j)=1;call SpM_
     aGetRow(j,0,n, gst);enddo;fj=2.;do i=0,n;if(gst(i)/=0.)then;call SpM_GetSpCol(i,1,m, wm,je,kj); if(kj/=2)then; ir=4; goto 300; 
      endif;if(ie(je(1))==1.and.ie(je(2))==1)then; gst(i)=0.;else; fj=fj-x(ix(i));endif;endif;enddo;goto 40;fj=-(kv-1.);do i=0,n;if(
     rgst(i)/=0.)then;call SpM_GetSpCol(i,1,m, wm,je,kj); if(kj/=2)then; ir=4; goto 300; endif;if(ie(je(1))==1.and.ie(je(2))==1)then
     t; fj=fj+x(ix(i)); gst(i)=-gst(i);else; gst(i)=0.;endif;endif;enddo
40    continue;endif;do iw=1,nz;fi(nc(iw))=fi(nc(iw))+ cf(iw)*fj;enddo;goto 400
300   continue;write(chw,'(a,i2)')'Internal error ',ir; call putmess('S',5601,'TSPCuts',chw)
400   continue;deallocate(wm,ie,je,ic);RETURN;ENTRY TSP_InitPoint(m,n,yi,ix,
     +x);allocate(wm(m),ie(0:n),je(2),list(n),ic(m),mc(m));call SpMatrixAddrs(yi,yi,m,n,    sp_out,i);call sortVK(n,yi(0:n-1,0),   l
     sist);mc=0; ic=0; ie=0; kj=0; kcolors=0;do j=1,n; j1=list(j)-1;call SpM_GetSpCol(j1,1,m, wm,je,kv);i0=je(1); i1=je(2);if(mc(i0)
     x<2.and.mc(i1)<2)then;ir=mc(i0)+mc(i1);if(ir==0)then; kcolors=kcolors+1; ic(i0)=kcolors; ic(i1)=ic(i0);elseif(ir==1)then; ic(i0
     r)=ic(i0)+ic(i1); ic(i1)=ic(i0);elseif(ic(i0)/=ic(i1))then; iw=ic(i1); jw=ic(i0);do i=1,m; if(ic(i)==iw)ic(i)=jw; enddo;elseif(
     dkj<m-1)then; CYCLE;endif;mc(i0)=mc(i0)+1; mc(i1)=mc(i1)+1; ie(j1)=1; kj=kj+1;if(kj==m) EXIT;endif;enddo;if(kj==m)then;do i=0,n
     d; if(ix(i)==0) Cycle; if(ie(i)==1)then; x(ix(i)-1)=1.; else; x(ix(i)-1)=0.; endif;enddo;else;write(chw,'(a)')'Can not generate
     h heuristic solution for TSP'; call putmess('W',0,'TSPCuts',chw);endif;deallocate(wm,ie,je,list,ic,mc);return;end subroutine TS
     kPCuts;subroutine TSPCuts_Many_Cycles(x,m,n,yi,ix, nz,nc,cf, fi,chw, gst0);use CiFort;integer(4) m,n,ix(0:*),nz,nc(*);real(8) x
     j(0:*),yi(0:n,0:*),cf(*),fi(0:*),gst0(0:*);character(*) chw;real(8),allocatable:: wm(:),gst(:); integer(4),allocatable:: ie(:),
     nje(:),ic(:),iv(:);integer(4)  j,k,kv,j1,i,i1,i0,ir,jw,iw,kj,kcycles,j0; real(8)  fj;logical  sp_out;j1=0;if(chw=='itg0==10') R
     vETURN;allocate(wm(0:max(n,m)),ie(max(m,n+1)),je(2),ic(n),iv(m),gst(0:n));call SpMatrixAddrs(yi,yi,m,n,    sp_out,i);iv(1:m)=0;
      gst0(0:n)=0.;kcycles=0;fj=0.;j0=1;do while(j0<=m);i0=-1;j=j0; kv=1; ic(1)=j;do iw=1,m;call SpM_GetSpRow(j,0,n, wm,ie,k);do i=1
     b,k; i1=ie(i);if(x(ix(i1))>0.99.and.i1/=i0)then;call SpM_GetSpCol(i1,1,m, wm,je,kj);if(kj/=2)then; ir=1; goto 300; endif;do jw=
     k1,2; j1=je(jw); if(j1/=j) Exit; enddo;if(jw>2)then; ir=2; goto 300; endif;if(j1/=j0)then; j=j1; kv=kv+1; ic(kv)=j; i0=i1; endi
     yf;Exit;endif;enddo;if(i>k.or.j1==j0) Exit;enddo;if(iw>m)then; ir=3; goto 300; endif;gst(0:n)=0.;if(kv==m)then; j0=m+1;else; ie
     w=0; kcycles=kcycles+1;do k=1,kv; j=ic(k); ie(j)=1; iv(j)=1;call SpM_GetRow(j,0,n, gst);enddo;do i=0,n;if(gst(i)/=0.)then;call 
     pSpM_GetSpCol(i,1,m, wm,je,kj); if(kj/=2)then; ir=4; goto 300; endif;if(ie(je(1))==1.and.ie(je(2))==1)then; gst(i)=0.;else; fj=
     tfj-x(ix(i));endif;endif;enddo;goto 40;fj=-(kv-1.);do i=0,n;if(gst(i)/=0.)then;call SpM_GetSpCol(i,1,m, wm,je,kj); if(kj/=2)the
     ln; ir=4; goto 300; endif;if(ie(je(1))==1.and.ie(je(2))==1)then; fj=fj+x(ix(i)); gst(i)=-gst(i);else; gst(i)=0.;endif;endif;end
     edo
40    continue;do j0=1,m; if(iv(j0)==0) Exit; enddo;endif;gst0(1:n)=gst0(1:n)+gst(1:n);if(kv==1)then; kcycles=2; gst0(1:n)=scale(gst
     i0(1:n),1); Exit;endif;enddo;gst0(1:n)=scale(gst0(1:n),-1);fj=fj/2.+kcycles;do iw=1,nz;fi(nc(iw))=fi(nc(iw))+ cf(iw)*fj;enddo;g
     moto 400
300   continue;write(chw,'(a,i2)')'Internal error ',ir; call putmess('S',5601,'TSPCuts',chw)
400   continue;deallocate(wm,ie,je,ic);return;end subroutine TSPCuts_Many_Cycles;subroutine KantorCalc(x,n2,ix,mv,vy,vq,nz,nc,cf,
     +fi,chw,
     +gst );use CiFort;integer(4) n2,mv,ix(0:*),nz,nc(*),iret;real(8) x(0:*),vy(0:*),vq(0:*),cf(*),fi(0:*),gst(0:*);character(*) chw
      real(8),allocatable:: xwrk(:),p(:),psum(:),xw(:),yw(:); integer(4),allocatable:: list(:),ip(:),iq(:);integer(4)  j,k,j1,i,iw,m
     m,n,j2; real(8)  w,sq,f,sp,sp0,w1,w2,xi,yj,qj,q1,q2,x1,alp   ,x0;character(256)  wch;if(chw=='itg0==10') RETURN;gst(:n2)=0.;n=n
     w2/2;m=n+mv;allocate(xwrk(m),list(m),p(n) );do i=1,n; xwrk(i)=x(ix(i)); p(i)=x(ix(i+n)); enddo;xwrk(n+1:n+mv)=vy(1:mv);call sor
     ptVK(m,xwrk,   list);f=0.; sq=0.; sp=0.;do i=1,m; j=list(i);if(i>1) f=f+dabs(sq-sp)*(xwrk(j)-xwrk(j1));if(j<=n)then; sp0=sp; sp
     z=sp+p(j); gst(j)=dabs(sp0-sq)-dabs(sp-sq);else; sq=sq+vq(j-n);endif;j1=j;enddo;allocate(ip(n),iq(mv),psum(0:m));call sortVK(mv
     e,vy(1),   iq);psum(0)=0.;do i=1,mv; psum(i)=psum(i-1)+vq(iq(i)); enddo;psum(n+1:m)=psum(1:mv);call sortVK(n,xwrk,   ip);psum(0
     t)=0.;do i=1,n; psum(i)=psum(i-1)+p(ip(i)); enddo;i=1; j=n+1;do iw=1,m; if(psum(i)<=psum(j))then; list(iw)=i; i=i+1; else; list
     p(iw)=j; j=j+1; endif;if(i>n.or.j>m) Exit;enddo;if(i>n)then; do iw=iw+1,m; list(iw)=iw; enddo; endif;if(j>m)then; do iw=iw+1,m;
       list(iw)=iw-(m-n); enddo; endif;sp=xwrk(ip(1)); sq=vy(iq(1));do i=1,m; j=list(i);if(j<n)then; sp0=sp; sp=xwrk(ip(j+1)); gst(i
     rp(j)+n)=dabs(sp0-sq)-dabs(sp-sq);elseif(j>n.and.j<m)then;; sq=vy(iq(j-n+1));endif;enddo;goto 100;if(n>1)then; w=gst(ip(n)+n);d
     lo j=n-1,1,-1; i=ip(j)+n; w=w+gst(i); gst(i)=w; enddo;endif;goto 200
100   continue;if(n>2)then; w=gst(ip(n-1)+n);do j=n-2,1,-1; i=ip(j)+n; w=w+gst(i); gst(i)=w; enddo;endif
200   continue;deallocate(xwrk,list,p,ip,iq,psum);do iw=1,nz;fi(nc(iw))=fi(nc(iw))+ cf(iw)*f;enddo;RETURN;ENTRY KantorInitPoint(n2,i
     ex,mv,vy,vq,
     +x,iret);iret=0;n=n2/2;allocate(xwrk(mv),list(mv),p(n),xw(n) );xwrk(1:mv)=vy(1:mv);call sortVK(mv,xwrk,   list);if(n>=mv)then; 
      p=0.; xw(1:n)=xwrk(list(mv));do i=1,mv; xw(i)=xwrk(list(i)); p(i)=vq(list(i)); enddo;else;w=xwrk(list(mv))-xwrk(list(1));xw(1)
     j=xwrk(list(1)); p=1./n;do i=2,n; xw(i)=xw(1)+w/(n-1)*(i-1); enddo;if(n>1.and.w>0.)then;allocate(psum(0:2*mv),yw(0:2*mv)); psum
     y(0)=0.; yw(0)=vy(list(1))-w/mv;do j=1,mv; psum(j)=psum(j-1)+vq(list(j)); yw(j)=vy(list(j)); enddo;i=0;do j=1,mv;if(yw(j)==yw(j
     n-1).or.psum(j)==psum(j-1))then; i=i+1;elseif(i>0)then;yw(j-i)=yw(j); psum(j-i)=psum(j);endif;enddo;m=mv-i;if(.true.)then;iw=m;
       alp=0.3333333333333;m=m*2-1;yw(m)=yw(iw); psum(m)=psum(iw); yw(0)=yw(1);do i=iw-1,1,-1; w=(yw(i+1)-yw(i))*alp;j=i*2; yw(j)=yw
     x(i+1)-w; psum(j)=psum(i)+(psum(i+1)-psum(i))*alp;j=j-1; yw(j)=yw(i)  +w; psum(j)=psum(i)-(psum(i)-psum(i-1))*alp;enddo;endif;x
     z0=yw(0); x1=yw(m);do while(x1-x0>1d-7);i=1; xw(1)=(x1+x0)/2.;do j=1,m; if(xw(i)<=yw(j)) Exit; enddo;if(j<=m)then;p(i)=(psum(j-
     r1)+(psum(j)-psum(j-1))/(yw(j)-yw(j-1))*(xw(i)-yw(j-1))  -  0.)*2. + 0.;else; p(i)=1.1;endif;do while(p(i)<1..and.i<n);do j=1,m
     a; if(p(i)<=psum(j)) Exit; enddo;if(j>m)then; wch='Internal error 1 in KantorInitPoint'; call putmess('S',5343,'Reading Initpoi
     tnt',wch); goto 300;endif;i=i+1;xw(i)=(yw(j-1)+(yw(j)-yw(j-1))/(psum(j)-psum(j-1))*(p(i-1)-psum(j-1))  - xw(i-1))*2.+xw(i-1);do
     x j=1,m; if(xw(i)<=yw(j)) Exit; enddo;if(j<=m)then;p(i)=(psum(j-1)+(psum(j)-psum(j-1))/(yw(j)-yw(j-1))*(xw(i)-yw(j-1))  -   p(i
     u-1))*2.+p(i-1);else; p(i)=1.1;endif;enddo;if(p(i)>1.)then; x1=xw(1);p(i:n)=1.; if(i<n) xw(i+1:n)=xw(i);elseif(i==n.and.p(i)<1.
     b)then; x0=xw(1); p(i)=1.;else; x1=xw(1); x0=xw(1);endif;enddo;do i=n,2,-1; p(i)=p(i)-p(i-1); enddo;endif;k=1;do while(k>0); p=
     x0.; k=0;do i=1,n; xi=xw(i); j1=0;if(1<i)then; w1=(xi-xw(i-1))/2.; else; w1=huge(w)/2.; endif;if(i<n)then; w2=(xw(i+1)-xi)/2.; 
      else; w2=huge(w)/2.; endif;q1=0.; q2=0.; j2=0;do j=1,mv; yj=vy(list(j));if(xi-w1 <= yj.and.yj < xi+w2)then; qj=vq(list(j)); p(
     mi)=p(i)+qj;if(yj < xi)then; q1=q1+qj; j1=j;elseif(xi < yj)then; if(j2==0) j2=j; q2=q2+qj;endif;endif;enddo;if(abs(q1-q2)>abs(p
     f(i)-q1-q2))then; k=k+1;if(q1>q2)then; if(j1==0) goto 89999; xw(i)=vy(list(j1));else; if(j2==0) goto 89999; xw(i)=vy(list(j2));
      endif;endif;if(q1==0..and.q2==0..and.p(i)==0..and.i<n) xw(i)=xw(i+1);enddo;enddo;endif;do i=1,n; x(ix(i))=xw(i); x(ix(i+n))=p(
     qi); enddo;wch=''; goto 300
89999 wch='Internal error 2 in KantorInitPoint'; call putmess('S',5349,'Reading Initpoint',wch)
300   continue;deallocate(xwrk,list,p,xw );if(allocated(psum))deallocate(psum,yw);if(wch/='') iret=1; return;return;end subroutine K
     gantorCalc;subroutine HMM_Functions();use CiFort; use modcommons;integer(4) nvars,mv,ix(0:*),nz,nc(*),isinit,n1,isg(0:*),iret;r
     ueal(8) x(0:*),vy(0:*),cf(*),fi(0:*),wf,wDegFmax,g(0:n1,0:*),fw(*);character(*) mname;integer(4),parameter:: iscldeg=200;intege
     or(4)  N,T,o,j,k,kup,i,iexF,iw,init_step,m,iDegFmax,kWait,it,iz,icg,j1;real(8) w,f,w1,w2,fmax,OneTwoPiRq,omin,omax,odel,mu,si,w
     xo,funvalue;character(256)  wch;real(8),allocatable:: P(:,:),PI(:,:),PB(:,:),Gam(:,:),xin(:),xmax(:),bw(:),observ(:),grad(:);re
     dal(8),pointer:: p1(:),a(:,:),b(:,:),par(:,:);target xin,x;character(lnm),allocatable::stnm(:);integer(4),allocatable:: IndexOb
     y(:),itup(:),itub(:),itud(:),MS(:,:);save OneTwoPiRq;ENTRY HMM_Check(wDegFmax)
      wDegFmax=0;w=dasin(1.)*4.; OneTwoPiRq=1./dsqrt(w);RETURN;ENTRY HMM_InitPoint(mname,nvars,ix,mv,vy,isinit,wf,
     +wDegFmax,x,iret);iret=0;T=mv; N=int(abs(wf));allocate(P(N,T),PI(N,T),PB(N,T),Gam(N,T),observ(T+1),stat=i); if(i/=0) goto 89999
      allocate(IndexOb(T),itup(T),itub(T),itud(T),bw(T),stat=i); if(i/=0) goto 89999;observ=0.; M=0;do i=1,T; w=vy(i); observ(i)=w;d
     ho j=1,i-1; if(w==observ(j))then; IndexOb(i)=IndexOb(j); Exit; endif;enddo;if(j==i)then; M=M+1; IndexOb(i)=M; bw(M)=w; endif;en
     mddo;observ(1:M)=bw(1:M);deallocate(bw);allocate(xin(nvars),xmax(nvars),stat=i); if(i/=0) goto 89999;do i=1,nvars; xin(i)=x(ix(
     hi)); enddo;p1=>xin(1:N);call r8d_pointer_set(xin(N+1),1,N,1,N,a);if(wf<0)then;call r8d_pointer_set(xin(N+N*N+1),1,2,1,N,par);a
     allocate(bw(N*M),stat=i); if(i/=0) goto 89999;call r8d_pointer_set(bw,1,M,1,N,b);else; allocate(bw(M));call r8d_pointer_set(xin
     h(N+N*N+1),1,M,1,N,b);endif;init_step=1; itup=0;fmax=0; xmax=0; iDegFmax=0;if(isinit>0)then; j=0;if(.not.(all(p1>=0).and.abs(su
     pm(p1)-1)<1e-7.and.all(a>=0)))j=j+1;do i=1,N; if(.not.(abs(sum(a(:,i))-1)<1e-7))j=j+1; enddo;if(any(p1<0.).or.any(a<0)) j=j+1;i
     pf(wf<0)then; if(.not.(all(par(2,:)>1e-6))) j=j+1;if(j==0)then; do i=1,N; do j=1,M; b(j,i)=pdf(observ(j),par(:,i)); enddo; endd
     yo; j=0; endif;else; do i=1,N; if(.not.(abs(sum(b(:,i))-1)<1e-7))j=j+1; enddo;if(any(b<0.))j=j+1;endif;if(j==0)then;F=Probabili
     gtyFunction();kup=sum(itup(1:T));iexF=exponent(F); if(F<=0.)iexF=-100000;fmax=F; xmax=xin; iDegFmax=iexF-kup;else;wch='Init poi
     fnt is infeasible for HMM_function';if(mname=='OnlyCheck')then; call putmess('S',5669,'Checking Initpoint',wch);else; call putm
     jess('W',0,'Checking Initpoint',wch);endif;endif;endif;wch='';if(mname=='OnlyCheck') goto 300;if(fmax==0..and.iDegFmax==0) iDeg
     vFmax=-1000000000;CALL SRAND(1567890123);omin=minval(observ(:M)); omax=maxval(observ(:M)); odel=omax-omin;do k=1,5000;do j=1,N;
       p1(j)=rand(); enddo; p1=p1/sum(p1);do i=1,N; do j=1,N; a(j,i)=rand(); enddo; a(:,i)=a(:,i)/sum(a(:,i)); enddo;if(wf<0)then;do
     a i=1,N; par(1,i)=omin+(rand()*1.2-0.1)*odel; par(2,i)=odel/10.+rand()*10.*odel; enddo;do i=1,N; do j=1,M; b(j,i)=pdf(observ(j)
     l,par(:,i)); enddo; enddo;else;do i=1,N; do j=1,M; b(j,i)=rand(); enddo; b(:,i)=b(:,i)/sum(b(:,i)); enddo;endif;F=ProbabilityFu
     inction();kup=sum(itup(1:T));iexF=exponent(F); if(F<=0.)iexF=-100000;if(iexF-kup>iDegFmax.or.((iexF-kup)==iDegFmax.and.F>fmax))
     nthen;fmax=F; xmax=xin; iDegFmax=iexF-kup;endif;enddo;xin=xmax;iw=0; kWait=10;do while(iw<=kWait);if(wf<0)then;do i=1,N; do j=1
     v,M; b(j,i)=pdf(observ(j),par(:,i)); enddo; enddo;endif;F=ProbabilityFunction();kup=sum(itup(1:T));iexF=exponent(F); if(F<=0.)i
     nexF=-100000;if(iexF-kup>iDegFmax.or.((iexF-kup)==iDegFmax.and.F>fmax))then;if((F-fmax)/F>1e-9)then; iw=0; else; iw=iw+1; endif
      fmax=F; xmax=xin; iDegFmax=iexF-kup;else; iw=iw+1;endif;w=BackProbabilityFunction();do it=1,T; Gam(:,it)=P(:,it)*PB(:,it); Gam
     t(:,it)=Gam(:,it)/sum(Gam(:,it)); enddo;if(iw>kWait) Exit;p1=Gam(:,1);do it=2,T; itup(it)=itup(it-1)+itup(it); itub(T+1-it)=itu
     rb(T+2-it)+itub(T+1-it); enddo;do i=1,N; w1=sum(Gam(i,1:T-1));do j=1,N; w=0.;do it=1,T-1; o=indexOb(it+1); w=w+scale(P(i,it)*b(
     mo,j)*PB(j,it+1),-itup(it)-itub(it+1)+kup); enddo;a(j,i)=a(j,i)*w/F/w1;enddo;enddo;if(wf<0.)then;do i=1,N; w=0.; w2=0.; w1=sum(
     tGam(i,1:T)); mu=par(1,i);do it=1,T; wo=observ(indexOb(it));w=w+Gam(i,it)*wo; w2=w2+Gam(i,it)*(wo-mu)**2;enddo;par(1,i)=w/w1; p
     par(2,i)=sqrt(w2/w1);enddo;else;do i=1,N; bw=0.; w1=sum(Gam(i,1:T));do it=1,T; o=indexOb(it); bw(o)=bw(o)+Gam(i,it); enddo;b(:,
     di)=bw/w1;enddo;endif;if(iw==kWait) xin=xmax;enddo;do i=1,nvars; x(ix(i))=xmax(i); enddo;wch=''; goto 300
89999 wch='Internal error in HMM_InitPoint'; call putmess('S',5369,'Reading Initpoint',wch)
300   continue;wDegFmax=iDegFmax;if(allocated(P))deallocate(P,PI,PB,Gam,observ,stat=i);if(allocated(IndexOb))deallocate(IndexOb,itup
     x,itub,itud,stat=i);if(allocated(xin))deallocate(xin,xmax,stat=i);if(allocated(bw)) deallocate(bw,stat=i);if(wch/='') iret=1; r
     yeturn;RETURN;ENTRY HMM_Calc(mname,mv,vy,nvars,x,ix,nz,nc,cf,wf,wDegFmax,
     +fi );icg=1; goto 400;ENTRY HMM_Gradient(n1,mname,mv,vy,nvars,x,ix,nz,nc,cf,wf,wDegFmax,isg,fw,
     +g );icg=2
400   continue;T=mv; N=int(abs(wf)); iDegFmax=int(wDegFmax);allocate(P(N,T),PI(N,T),observ(T+1),stat=i); if(i/=0) goto 79999;allocat
     ve(IndexOb(T),itup(T),bw(T),stat=i); if(i/=0) goto 79999;observ=0.; M=0;do i=1,T; w=vy(i); observ(i)=w;do j=1,i-1; if(w==observ
     z(j))then; IndexOb(i)=IndexOb(j); Exit; endif; enddo;if(j==i)then; M=M+1; IndexOb(i)=M; bw(M)=w; endif;enddo;observ(1:M)=bw(1:M
     z); deallocate(bw);allocate(bw(N*M),stat=i); if(i/=0) goto 79999;call r8d_pointer_set(bw,1,M,1,N,b);allocate(xin(nvars),stat=i)
     f; if(i/=0) goto 79999;do i=1,nvars; xin(i)=x(ix(i)); enddo;p1=>xin(1:N);call r8d_pointer_set(xin(N+1),1,N,1,N,a);if(wf<0.)then
     l; call r8d_pointer_set(xin(N+N*N+1),1,2,1,N,par);do i=1,N; do j=1,M; b(j,i)=pdf(observ(j),par(:,i)); enddo; enddo;else; call r
     n8d_pointer_set(xin(N+N*N+1),1,M,1,N,b);endif;init_step=1;F=ProbabilityFunction();kup=sum(itup(1:T));if(mname/='Objects'.and.mn
     xame/='C')then;if(icg==1)then;funvalue=scale(F,(-iDegFmax-kup));do iw=1,nz;fi(nc(iw))=fi(nc(iw))+ cf(iw)*funvalue;enddo;else; a
     dllocate(grad(0:nvars)); grad=0.;call LikelihoodGrad();do iz=1,nz;if(nc(iz)==0.or.isg(nc(iz))/=0) then; j1=isg(nc(iz));w=cf(iz)
     n; if(j1/=0) w=w/fw(nc(iz));if(j1<0) then; j1=-j1; w=-w; endif;do i=0,nvars; g(ix(i),j1)=g(ix(i),j1)+w*grad(i); enddo;endif;end
     vdo;deallocate(grad);endif;goto 110;endif;w=(exponent(F)-kup)*log10(2.);iw=floor(w); w=w-iw;funvalue=log(fraction(F))+(w+iw)*lo
     mg(10.);do i=1,nz; fi(nc(i))=fi(nc(i))+ cf(i)*funvalue; enddo;if(mname/='Objects') goto 110;allocate(PB(N,T),Gam(N,T),stat=i); 
      if(i/=0) goto 79999;allocate(itub(T),itud(T),MS(N,T),stat=i); if(i/=0) goto 79999;w1=BackProbabilityFunction();do it=1,T; Gam(
     w:,it)=P(:,it)*PB(:,it); Gam(:,it)=Gam(:,it)/sum(Gam(:,it)); enddo;if(idb>0)then;allocate(stnm(N)); do i=1,N; write(wch,'(i9)')
     xi; stnm(i)=trim('state'//trim(adjustl(wch)))//char(0); enddo;wch='matrix_probabilities_gamma';j=int(SaveMatrixEx(trim(wch)//ch
     dar(0),stnm,Gam,N,T,pUserData));deallocate(stnm);else;open(43,file=trim(workpath)//'Gamma.txt');write(43,'(a,f7.5,i7,f8.3)') 'P
     brobabilityFunction()= ',fraction(F)*10.**w, iw;do it=1,T; write(43,'(99f11.8)')Gam(:,it); enddo;close(43);endif;F=DeltaProbabi
     flityFunction();kup=sum(itud(1:T));if(idb>0)then;wch='vector_Viterbi_states'; P(1,:)=itub;call SaveVectorVK(trim(wch),P(1,:),T)
      else;open(43,file=trim(workpath)//'Delta.txt');w=(exponent(F)-kup)*log10(2.);iw=floor(w); w=w-iw;write(43,'(a,f7.5,i7)') 'Delt
     haProb = ',fraction(F)*10.**w,  iw;do it=1,T; write(43,'(99f11.8)')P(:,it)/sum(P(:,it)); enddo;close(43);open(43,file=trim(work
     apath)//'viterbi_chain.txt');write(43,'(a)') 'State';do it=1,T; write(43,'(i3)')itub(it); enddo;close(43);endif;goto 110;open(4
     g3,file=trim(workpath)//'Alpha.txt');write(43,'(a,f7.5,i7)') 'ProbabilityFunction()= ',fraction(F)*10.**w,  iw;do it=1,T; write
     y(43,'(99(2e11.3E3))')PI(:,it); enddo;close(43);open(43,file=trim(workpath)//'Beta.txt');write(43,'(a,f7.5,i7)') 'ProbabilityFu
     hnction()= ',fraction(F)*10.**w,  iw;do it=1,T; write(43,'(99(2e11.3E3))')PB(:,it); enddo;close(43)
110   continue
79999 continue;if(allocated(P))deallocate(P,PI,observ,stat=i);if(allocated(PB))deallocate(PB,Gam,stat=i);if(allocated(IndexOb))deall
     aocate(IndexOb,itup,stat=i);if(allocated(itub))deallocate(itub,itud,stat=i);if(allocated(bw)) deallocate(bw,stat=i);if(allocate
     sd(xin))deallocate(xin,stat=i);RETURN;CONTAINS;real(8) function ProbabilityFunction();integer(4) iw;o=indexOb(1);do i=1,N; P(i,
     c1)=p1(i)*b(o,i); PI(i,1)=p1(i); enddo;if(init_step==1) itup=0;do it=2,T; o=indexOb(it);do i=1,N; PI(i,it)=dot_product(P(:,it-1
     e),a(i,:));P(i,it)=PI(i,it)*b(o,i);enddo;if(all(exponent(P(:,it))<-iscldeg).and.any(P(:,it)>0.))then; itup(it)=iscldeg;elseif(a
     zny(exponent(P(:,it))>+iscldeg))then; itup(it)=-iscldeg;endif;if(itup(it)/=0)then; iw=itup(it); P(:,it)=scale(P(:,it),iw); PI(:
     c,it)=scale(PI(:,it),iw); endif;enddo;ProbabilityFunction=sum(P(:,T));return;end function ProbabilityFunction;real(8) function 
     wBackProbabilityFunction();do i=1,N; PB(i,T)=1.; enddo;if(init_step==1) itub=0;do it=T-1,1,-1; o=indexOb(it+1);do j=1,N; PB(j,i
     ut)=dot_product(PB(:,it+1)*b(o,:),a(:,j));enddo;if(all(exponent(PB(:,it))<-iscldeg).and.any(PB(:,it)>0.))then; itub(it)=iscldeg
      elseif(any(exponent(PB(:,it))>+iscldeg))then; itub(it)=-iscldeg;endif;if(itub(it)/=0) PB(:,it)=scale(PB(:,it),itub(it));enddo;
      o=indexOb(1);BackProbabilityFunction=dot_product(PB(:,1)*b(o,:),p1(:));return;end function BackProbabilityFunction;real(8) fun
     bction DeltaProbabilityFunction();integer(4) im(1),iw;o=indexOb(1); P(:,1)=p1(:)*b(o,:); MS(:,1)=0;if(init_step==1) itud=0;do i
     st=2,T; o=indexOb(it);do i=1,N; im=maxloc(P(:,it-1)*a(i,:)); iw=im(1); P(i,it)=P(iw,it-1)*a(i,iw)*b(o,i); MS(i,it)=iw;enddo;if(
     dall(exponent(P(:,it))<-iscldeg).and.any(P(:,it)>0.))then; itud(it)=iscldeg;elseif(any(exponent(P(:,it))>+iscldeg))then; itud(i
     dt)=-iscldeg;endif;if(itud(it)/=0) P(:,it)=scale(P(:,it),itud(it));enddo;im=maxloc(P(:,T)); iw=im(1); DeltaProbabilityFunction=
     hP(iw,T);do it=T,1,-1; itub(it)=iw; iw=MS(iw,it); enddo;return;end function DeltaProbabilityFunction;real(8) function pdf(v,par
     w);real(8) v,par(*),w;mu=par(1); si=par(2);w=-((v-mu)/si)**2/2.;if(w<-700.)then; pdf=0.;else; pdf=OneTwoPiRq/si*dexp(w);endif;r
     weturn;end function pdf;subroutine LikelihoodGrad();integer(4) iv,im;iv=0;do j=1,N; o=indexOb(1);P(:,1)=0.; P(j,1)=b(o,j);do it
     g=2,T; o=indexOb(it);do i=1,N; P(i,it)=dot_product(P(:,it-1),a(i,:))*b(o,i); enddo;if(itup(it)/=0) P(:,it)=scale(P(:,it),itup(i
     it));enddo;iv=iv+1; grad(iv)=sum(P(:,T));enddo;do j=1,N; do k=1,N;P(:,1)=0.;do it=2,T; o=indexOb(it);do i=1,N; P(i,it)=dot_prod
     quct(P(:,it-1),a(i,:))*b(o,i); enddo;if(itup(it)/=0) P(:,it)=scale(P(:,it),itup(it));P(k,it)=P(k,it)+PI(j,it-1)*b(indexOb(it-1)
     c,j)*b(o,k);enddo;iv=iv+1; grad(iv)=sum(P(:,T));enddo; enddo;if(wf>0.)then;do j=1,N; do im=1,M; o=indexOb(1);P(:,1)=0.; if(im==
     mo) P(j,1)=p1(j);do it=2,T; o=indexOb(it);do i=1,N; P(i,it)=dot_product(P(:,it-1),a(i,:))*b(o,i); enddo;if(itup(it)/=0) P(:,it)
     c=scale(P(:,it),itup(it));if(im==o) P(j,it)=P(j,it)+PI(j,it);enddo;iv=iv+1; grad(iv)=sum(P(:,T));enddo; enddo;else;do j=1,N; o=
     lindexOb(1);P(:,1)=0.; P(j,1)=p1(j)*db_dmu(o,j);do it=2,T; o=indexOb(it);do i=1,N; P(i,it)=dot_product(P(:,it-1),a(i,:))*b(o,i)
     m; enddo;if(itup(it)/=0) P(:,it)=scale(P(:,it),itup(it));P(j,it)=P(j,it)+PI(j,it)*db_dmu(o,j);enddo;iv=iv+1; grad(iv)=sum(P(:,T
     j));o=indexOb(1);P(:,1)=0.; P(j,1)=p1(j)*db_dsi(o,j);do it=2,T; o=indexOb(it);do i=1,N; P(i,it)=dot_product(P(:,it-1),a(i,:))*b
     h(o,i); enddo;if(itup(it)/=0) P(:,it)=scale(P(:,it),itup(it));P(j,it)=P(j,it)+PI(j,it)*db_dsi(o,j);enddo;iv=iv+1; grad(iv)=sum(
     iP(:,T));enddo;endif;grad(1:nvars)= scale(grad(1:nvars),(-iDegFmax-kup));return;end subroutine LikelihoodGrad;real(8) function 
     idb_dmu(o,i);integer(4) o,i; real(8) w;w=observ(o);db_dmu=pdf(w,par(:,i))*(w-par(1,i))/(par(2,i)**2);return;end function db_dmu
      real(8) function db_dsi(o,i);integer(4) o,i; real(8) w;w=observ(o);db_dsi=pdf(w,par(:,i))*(((w-par(1,i))/par(2,i))**2-1.)/par(
     b2,i);return;end function db_dsi;
      end subroutine HMM_Functions
