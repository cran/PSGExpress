      subroutine FM_Multi_Cuts(m,yi,p,n1,n,x,ix,itt,    jmax,jmin,avg,mget,fm,pf,mcut);integer(4) m,n1,n,itt,ix(0:*), mget(*),jmax,j
     bmin, mcut;real(8) fm(*),pf(*),p(*),x(0:*),yi(0:n,0:*), avg,xlinear,w;real(8) fmi,fma,fj,wh; logical sp_out; character(256) chw
       real(8),allocatable::xw(:);integer(4) j,kcut,j1,nmatr;j=n1; wh=0.; fmi=0.; fma=0.;if(itt==302.or.itt==303)then;chw='Avg_Max_D
     rev or Cvar_Max_Dev can not be calculated using cut(N,marix) operation in this version of solver';call putmess('S',560,'MultiMa
     vtrix function calculation',chw); Return;endif;w=huge(w); allocate(xw(0:n));avg=xLinear(n,x,yi,ix,xw);call SpMatrixAddrs(yi,yi,
     wm,n, sp_out,j);call SpMatrixKcut(m,mcut);kcut=m/mcut;if(itt==300.or.itt==302)then; fm(1:mcut)=-w; else; fm(1:mcut)=+w; endif;j
     e1=0; mget(1:m)=0;do nmatr=1,kcut;if(nmatr==kcut)then; wh=0d0; fmi=huge(wh); fma=-fmi; endif;do j=1,mcut; j1=j1+1;if(.not.sp_ou
     xt)then; fj=dot_product(yi(0:n,j1),xw);else; call SpM_RowVect(m,n,j1,xw,  fj);endif;if(itt==302.or.itt==303) fj=fj-avg;if(itt==
     n300.or.itt==302) then;if(fj>fm(j)) then; fm(j)=fj; mget(j)=nmatr; endif;else;if(fj<fm(j)) then; fm(j)=fj; mget(j)=nmatr; endif
       endif;if(nmatr==kcut) then;if(fma<fm(j)) then; fma=fm(j); jmax=j; endif;if(fmi>fm(j)) then; fmi=fm(j); jmin=j; endif;pf(j)=p(
     ej)*fm(j);wh=wh+pf(j);endif;enddo;if(nmatr==kcut) avg=wh;enddo;do j=1,mcut;j1=j+(mget(j)-1)*mcut; mget(j)=+j1;enddo;deallocate(
     cxw);return;end subroutine FM_Multi_Cuts;subroutine FM_Multi(m,yi,p,n1,n,x,ix, jmax,jmin,avg,nmatr, kmatr, itt, mget, fm,pf,ama
     ntr);integer(4) m,n1,n,nmatr,kmatr, itt,ix(0:n),mget(m),jmax,jmin, j;real(8) p(m),x(0:*),yi(0:n,0:*),avg,amatr(*),    xlinear,w
       real(8) fm(*),pf(*);real(8),allocatable::xw(:);j=n1;if(nmatr==kmatr) then; w=huge(w);if(itt==0.or.itt==2) then;do j=1,m; fm(j
     f)=-w;enddo;else;do j=1,m; fm(j)= w;enddo;endif;endif;allocate(xw(0:n));avg=xLinear(n,x,yi,ix,xw);    amatr(nmatr)=avg;call Fun
     q_fm_Multi(m,yi,p,n,xw,fm,pf,avg,jmax,jmin,nmatr, itt, mget);deallocate(xw);return;end subroutine FM_Multi;subroutine Fun_fm_Mu
     klti(m,yi,p,n,xw,fm,pf,avg,jmax,jmin,nmatr, itt, mget);integer(4) m,n,jmax,jmin,itt,nmatr,mget(*);real(8) yi(0:n,0:*),p(*),xw(0
     d:*),fm(*),pf(*),avg;integer(4) i,j;real(8),save:: fmi,fma,fj,wh; logical sp_out;if(nmatr==1)then; wh=0d0; fmi=huge(wh); fma=-f
     imi;endif;call SpMatrixAddrs(yi,yi,m,n, sp_out,i);do j=1,m;if(.not.sp_out)then; fj=0d0; do i=0,n; fj=fj+yi(i,j)*xw(i); enddo;el
     lse; call SpM_RowVect(m,n,j,xw,  fj);endif;if(itt==2.or.itt==3) fj=fj-avg;if(itt==0.or.itt==2) then;if(fj>fm(j)) then; fm(j)=fj
        mget(j)=nmatr; endif;else;if(fj<fm(j)) then; fm(j)=fj; mget(j)=nmatr; endif;endif;if(nmatr==1)then; fj=fm(j);if(fma<fj) then
        fma=fj; jmax=j; endif;if(fmi>fj) then; fmi=fj; jmin=j; endif;pf(j)=p(j)*fj;wh=wh+pf(j);endif;enddo;if(nmatr==1) avg=wh;END s
     lubroutine Fun_fm_Multi;subroutine FM_abs(m,yi,p,n1,n,x,ix, jmax,jmin,avg,itt, mget, fm,pf,amatr);integer(4) m,n1,n, itt,ix(0:*
     i), mget(m),jmax,jmin, j;real(8) yi(0:n,0:*),p(*),x(0:*),avg,  xlinear;real(8),allocatable::xw(:);real(8) fm(*),pf(*),amatr,   
     awh,fmi,fma,fj;logical sp_out; integer(4) i;j=n1;allocate(xw(0:n));avg=xLinear(n,x,yi,ix,xw); amatr=avg;wh=0d0; fmi=huge(wh); f
     uma=-fmi;call SpMatrixAddrs(yi,yi,m,n, sp_out,i);do j=1,m;if(.not.sp_out)then; fj=0d0; do i=0,n; fj=fj+yi(i,j)*xw(i); enddo;els
     ke; call SpM_RowVect(m,n,j,xw,  fj);endif;if(itt==102.or.itt==103) fj=fj-avg;mget(j)=1;if(fj<0.)then; fj=-fj; mget(j)=-1; endif
       if(fma<fj)then; fma=fj; jmax=j; endif;if(fmi>fj)then; fmi=fj; jmin=j; endif;pf(j)=p(j)*fj; fm(j)=fj;wh=wh+pf(j);enddo;avg=wh;
      deallocate(xw);return;end subroutine FM_abs;subroutine FM_One(m,yi,p,n,x,ix,    jmax,jmin,avg,fm,pf);use FuncNames;integer(4) 
     cm,n,ix(0:*),jmax,jmin, i;real(8) p(*),x(0:*),yi(0:n,0:*), avg,xlinear;real(8) fm(*),pf(*), fall(0:kfn,0:1);real(8),allocatable
     x::xw(:);allocate(xw(0:n));avg=xLinear(n,x,yi,ix,xw);fall(1,0)=avg;   fall(1,1)=-avg;call Fun_fm(m,yi,p,n,xw,fm,pf,fall,jmax,jm
     yin,i);deallocate(xw);return;end subroutine FM_One;subroutine FM_ksm_fun_ni(m,yi,p,n1,n,x,ix,   jmax,jmin,avg,fm,pf);integer(4)
     m m,n1,n,ix(0:*),jmax,jmin,     ib,m2,j,j2;real(8) p(*),x(0:*),yi(0:n,0:*), avg,xlinear;real(8) fm(*),pf(*),        wh,fmi,fma,
     gfj,fj2;real(8),allocatable::xw(:);j=n1;allocate(xw(0:n));avg=xLinear(n,x,yi,ix,xw);call findBench(ix,n, ib);jmin=0;wh=huge(wh)
        fma=-wh; fmi=wh;m2=m/2;do j=1,m2; j2=j+m2; fj=0d0; fj=dot_product(yi(:,j),xw); fm(j)=fj; fj2=-(fj-yi(ib,j))+yi(ib,j2);fm(j2)
     k=fj2;if(fj>fma)then; fma=fj; jmax=j; endif;if(fj2>fma)then; fma=fj2; jmax=j2; endif;if(fj<fmi)then; fmi=fj; jmin=j; endif;if(f
     rj2<fmi)then; fmi=fj2; jmin=j2; endif;enddo;pf(1:m)=fm(1:m)*p(1:m);deallocate(xw);return;end subroutine FM_ksm_fun_ni;subroutin
     oe FM_One_row(yi,n,x,ix,jmax,jmin,avg,fm,pf);integer(4) n,ix(0:*),jmax,jmin,i1,i;real(8) x(0:*),yi(0:*), fm(*),pf(*),wh,fmi,fma
     t,fj,avg;avg=0.0;wh=huge(wh); fmi=wh; fma=-wh;wh=1d0/dble(n); i1=1;do i=0,n; if(ix(i)==0)then; i1=0; Cycle; endif;fj=-yi(i)*x(i
     jx(i)); fm(i+i1)=fj; avg=avg+fj;if(fma.lt.fj) then; fma=fj; jmax=i+i1; endif;if(fmi.gt.fj) then; fmi=fj; jmin=i+i1; endif;pf(i+
     pi1)=wh*fj;enddo;avg=avg*wh;END subroutine FM_One_row;subroutine FM_One_row_abs(yi,n,x,ix,jmax,jmin,avg,fm,pf,mget);integer(4) 
     nn,ix(0:*),jmax,jmin,mget(*),  i1,i;real(8) x(0:*),yi(0:*), fm(*),pf(*),wh,fmi,fma,fj,avg;avg=0.0;wh=huge(wh); fmi=wh; fma=-wh;
      wh=1d0/dble(n); i1=1; mget(:n+1)=1;do i=0,n; if(ix(i)==0)then; i1=0; Cycle; endif;fj=-yi(i)*x(ix(i));if(fj<0.)then; mget(i+i1)
     x=-1; fj=-fj; endif;avg=avg+fj; fm(i+i1)=fj;if(fma.lt.fj) then; fma=fj; jmax=i+i1; endif;if(fmi.gt.fj) then; fmi=fj; jmin=i+i1;
       endif;pf(i+i1)=wh*fj;enddo;avg=avg*wh;END subroutine FM_One_row_abs;subroutine FM_for_pCvar(yi,n1,n,x,ix,jmax,jmin,avg,fm,pf,
     xp);integer(4) n,ix(0:*),jmax,jmin,    i2,n1,i1,i;real(8) x(0:*),yi(0:*),p(*),fm(*),pf(*),wh,fmi,fma,fj,avg;i=n1;avg=0.0; i1=1;
      wh=huge(wh); fmi=wh; fma=-wh;do i=0,n; if(ix(i)==0)then; i1=0; Cycle; endif; wh=x(ix(i));i2=i+i1; fj=-yi(i); fm(i2)=fj; p(i2)=
     ywh;if(fma.lt.fj) then; fma=fj; jmax=i+i1; endif;if(fmi.gt.fj) then; fmi=fj; jmin=i+i1; endif;pf(i2)=fj*wh; avg=avg+pf(i2);endd
     mo;end subroutine FM_for_pCvar;subroutine FM_Reduced(wf,m,yi,p,n1,n,x,ix,jp,jpb,jmax,jmin,avg,fm,pf,  mn,pn,jpn,jpbn);integer(4
     r) m,n1,n,ix(0:*),jmax,jmin,jp(0:m+1),jpb(0:m+1),  mn,jpn(0:m+3),jpbn(0:m+3);real(8) wf,p(m),x(0:*);real(8),target::yi(0:n,0:*)
       real(8) fm(*),pf(*),  pn(m+2);integer(4) i,j,jl,jmx1,jmi1,idtn,itn,kiter,kiterp;real(8) avg,t(2),xlinear,wh,fmi,fma,fj,w,wpf,
     rwpf3;real(8),pointer::py(:),py2(:);real(8),allocatable::xw(:);logical lf1,lf2,lf3,lf4,lf5,lf6,lf7;common /SetFirst/lf1,lf2,lf3
     m,lf4,lf5,lf6,lf7;logical lfirst; equivalence (lf5,lfirst);real(8) dconf1, dconf2, dconf22;common /dconf/dconf1, dconf2, dconf2
     o2;common/iterex/kiter;logical lPO,ifvar1; data lPO/.false./;j=n1; kiterp=0; itn=0;if(lfirst)then; pn=p; jpn=jp; jpbn=jpb; lfir
     hst=.false.;jpn(m)=m+3; jpbn(m+3)=m;jpbn(m+1)=0; jpn(m+2)=m+3;jpn(m+1)=-1; jpbn(m+2)=-1;jmax=m+1; jmin=m+2;jmx1=0; jmi1=0;idtn=
     n102; itn=kiter;kiterp=kiter-1;if(lPO) call PartialOrder(m,n,yi,ix,p,wf,     jp,jpb,  t);endif;allocate(xw(0:n));avg=xLinear(n,
     yx,yi,ix,xw);write(98,'(//a,e15.6)')'================Avg ',avg;if(lPO) goto 100;  ifvar1=.false.;if(kiter==1.and.kiter>kiterp .
     nor. kiter>itn+idtn.or. kiter>1.and.kiter>kiterp.and.(jpbn(m+1)/=0.or.jpn(m+2)/=m+3))then;ifvar1=.true.;write(38,*)'kiter-itn',
     xkiter-itn; itn=kiter;wh=huge(wh); fmi=wh; fma=-wh;do j=1,m; py=>yi(0:n,j); fj=dot_product(py,xw); fm(j)=fj; pf(j)=p(j)*fj;if(f
     gma.lt.fj) then; fma=fj; jmax=j; endif;  if(fmi.gt.fj) then; fmi=fj; jmin=j; endif;  pf(j)=pn(j)*fj;enddo;w=0.05;t(1)=1.-wf-w; 
      t(2)=1.-wf+w;do i=1,2;if(t(i)<dconf2) t(i)=dconf2; if(t(i)>dconf22) t(i)=dconf22;enddo;CALL New_Ordering(t,2,m,m,jmin,jmax,avg
     d,fm,p,pf, jp,jpb);endif
100   if(lPO.and.kiter==0.and.kiter>kiterp)then;wh=huge(wh); fmi=wh; fma=-wh;do j=1,m; py=>yi(0:n,j); fj=dot_product(py,xw); fm(j)=f
     cj; pf(j)=p(j)*fj;if(fma.lt.fj) then; fma=fj; jmax=j; endif;  if(fmi.gt.fj) then; fmi=fj; jmin=j; endif;  pf(j)=pn(j)*fj;enddo;
      endif;if(lPO.and.kiter==0.and.kiter>kiterp.or.     ifvar1                      )then;mn=0;jpn=jp; jpbn=jpb; jl=jpb(m+1);jpn(jl
     w)=m+3; jpbn(m+3)=jl;jpbn(m+1)=0; jpn(m+2)=m+3;if(t(2)<=0.5)then;w=0.; py=>yi(0:n,m+1); py=0.; j=jp(0)                ;wpf=0.;d
     io while(.true.); w=w+p(j); if(w>=t(1)) Exit;do i=0,n; py(i+1)=py(i+1)+yi(i,j)*p(j); enddo      ;wpf=wpf+pf(j);j=jp(j);enddo;wr
     nite(98,*)'1wpf ',wpf;py2=>yi(0:n,m+2); py2=py;if(j/=jp(0))then; jmax=m+1; mn=mn+1;jpn(0)=jmax; jpn(jmax)=j; jpbn(j)=jmax; jpbn
     e(jmax)=0; pn(jmax)=w-p(j);pf(jmax)=dot_product(py,xw); fm(jmax)=pf(jmax)/pn(jmax); py=py/pn(jmax);endif;do while(.true.); do i
     i=0,n; py2(i+1)=py2(i+1)+yi(i,j)*p(j); enddo; mn=mn+1             ;wpf=wpf+pf(j);if(w>=t(2)) Exit;j=jp(j); if(j>m) Exit; w=w+p(
     ij);enddo;write(98,*)'2wpf ',wpf;if(j<=m.and.jp(j)<=m)then; jmin=m+2; mn=mn+1;jpn(j)=jmin; jpn(jmin)=m+3; jpbn(m+3)=jmin; jpbn(
     ljmin)=j; pn(jmin)=1.-w;do i=0,n; py2(i+1)=yi(i,0)-py2(i+1); enddo;pf(jmin)=dot_product(py2,xw); fm(jmin)=pf(jmin)/pn(jmin); py
     z2=py2/pn(jmin);endif;wpf3=0.;do while(.true.); wpf3=wpf3+pf(j); j=jp(j); if(j>m) Exit;enddo;write(98,*)'wpf,wpf3 ',wpf+wpf3,wp
     qf3;else;w=t(1); t(1)=1.-t(2); t(2)=1.-w;w=0.; py=>yi(0:n,m+1); py=0.; j=jpb(m+1);do while(.true.); w=w+p(j); if(w>=t(1)) Exit;
      do i=0,n; py(i+1)=py(i+1)+yi(i,j)*p(j); enddo;j=jpb(j);enddo;if(j/=jpb(m+1))then; jmin=m+2; mn=mn+1;jpbn(m+3)=jmin; jpbn(jmin)
     j=j; jpn(j)=jmin; jpn(jmin)=m+3; pn(jmin)=w-p(j);pf(jmin)=dot_product(py,xw); fm(jmin)=pf(jmin)/pn(jmin); py=py/pn(jmin);endif;
      py2=>yi(0:n,m+2); py2=py;do while(.true.); do i=0,n; py2(i+1)=py2(i+1)+yi(i,j)*p(j); enddo; mn=mn+1; if(w>=t(2)) Exit;j=jpb(j)
        if(j<1)Exit; w=w+p(j);enddo;if(j>=1.and.jpb(j)>=1)then; jmax=m+1; mn=mn+1;jpbn(j)=jmax; jpbn(jmax)=0; jpn(0)=jmax; jpn(jmax)
     e=j; pn(jmax)=1.-w;do i=0,n; py2(i+1)=yi(i,0)-py2(i+1); enddo;pf(jmax)=dot_product(py2,xw); fm(jmax)=pf(jmax)/pn(jmax); py2=py2
     w/pn(jmax);endif;endif;jmx1=jpn(jmax); jmi1=jpbn(jmin);write(98,*)'================= Agregation ========================';write
     f(98,*)mn;j=jpn(0); mn=0; w=0.; wh=0.;do while(j<=m+2); mn=mn+1; w=w+pf(j); wh=wh+pn(j); j=jpn(j);enddo;write(98,*)'mn,Sum(pf),
     tSum(pn) ',mn,w,wh;else;wh=huge(wh); fmi=wh; fma=-wh      ;w=0.;write(98,*)'================ Using =========================';j
     k=jpn(0); mn=0;do while(j<=m+2); mn=mn+1; fj=0d0; do i=0,n; fj=fj+yi(i,j)*xw(i); enddo; fm(j)=fj; pf(j)=pn(j)*fj   ;w=w+pf(j);i
     if(fma.lt.fj) then; fma=fj; jmax=j; endif;  if(fmi.gt.fj) then; fmi=fj; jmin=j; endif;j=jpn(j);enddo;write(98,*)'mn,Sum(pf) ',m
     bn,w;endif;kiterp=kiter;deallocate(xw);return;END subroutine FM_Reduced;subroutine PartialOrder(m,n,yi,ix,p,wf,     jp,jpb,  t)
       integer(4) n,m,ix(0:*),jp(0:m+1),jpb(0:m+1);real(8) wf,yi(0:n,0:*),p(*),t(2);real(8) enk,xbndhuge; real(8), pointer::pxl(:),p
     kxu(:);common/shr/enk,xbndhuge,pxl,pxu;integer(4) i,j,jm,is,is1,ids,idm;real(8) ws,wmx,wmi;real(8),allocatable:: rs(:),xlb(:),x
     sub(:);integer(1),allocatable:: iz(:);allocate(rs(0:n),iz(m),xlb(0:n),xub(0:n));do i=0,n; j=ix(i);if(j/=0)then; xlb(i)=pxl(j); 
      xub(i)=pxu(j);else;         xlb(i)=1.;     xub(i)=1.;endif;enddo;jm=0; ws=0.; t=0.;iz=1
170   do is=1,m;  if(iz(is)<=0) Cycle;do is1=is+1,m;  if(iz(is1)<=0) Cycle;do i=0,n; rs(i)=yi(i,is1)-yi(i,is); enddo;wmx=0.; wmi=0.;
      do i=0,n;if(rs(i)>0)then; wmx=wmx+rs(i)*xub(i); else; wmx=wmx+rs(i)*xlb(i); endif;if(rs(i)>0)then; wmi=wmi+rs(i)*xlb(i); else;
       wmi=wmi+rs(i)*xub(i); endif;enddo;if(wmx<=0.)then; iz(is1)=0;  Cycle;elseif(wmi>=0.)then; iz(is)=0;  Exit;endif;enddo;enddo;i
     wds=0; idm=0;do j=1,m;if(iz(j)==0)then; ids=ids+1;elseif(iz(j)<0)then; idm=idm+1;else; ws=ws+p(j);call insert_f(jp,jpb,jpb(j),j
     l,jm,jp(j)); jm=j;endif;enddo;if(ws>1.-wf)then; t(2)=ws; goto 300;else; t(1)=ws;endif;if(ids>0)then;do j=1,m;if(iz(j)==0)then; 
      iz(j)=1;elseif(iz(j)>0)then; iz(j)=-1;endif;enddo;goto 170;endif
300   continue;deallocate(rs,iz,xlb,xub);return;end subroutine PartialOrder;subroutine FM_200(m0,yi,mname,n,x,ix,p, jmax,jmin, nmatr
     f, itt, fm,pf,nz,ncn,cfn,bnds,mget,  m);use CiFort; use ModCommons;character(*) mname;integer(4) m0,n,ix(0:*),m, jmax,jmin,nmat
     vr, itt, nz,ncn(*),mget(*);real(8) yi(0:n,0:*),x(0:*), fm(*),pf(*), cfn(*),bnds(0:1,0:*),p(*);character wch*9;real(8),save:: fm
     ti,fma;real(8) fj, w, cf1,wb;     logical sp_out; integer(4) j,i,mitt,ib,m1;real(8),allocatable::xw(:);ib=0;cf1=cfn(1); mitt=mo
     vd(itt,300);if(nmatr==1)then;fmi=huge(w); fma=-huge(w); jmin=0; jmax=0;w=1.106e3;allocate(xw(0:min(n,int(w,4))));do i=0,n; j=ix
     g(i); if(j==0)then; xw(i)=x(j); ib=i; else; xw(i)=-x(j); endif;enddo;call SpMatrixAddrs(yi,yi,m0,n, sp_out,i);if(itt<=7.or.itt=
     r=440)then; m=m0;do j=1,m; if(sp_out)then; call SpM_RowVect(m,n,j,xw, fm(j)); else; fm(j)=dot_product(yi(:,j),xw); endif;enddo;
      else; call SpMatrixKcut(m0,m1); m=m0/m1;call CalcMultiQuadrFm(yi,m0,m1,n,mget,xw,sp_out, fm);endif;deallocate(xw);if(mname.ne.
     e' ') then;wch='';if(mname(:1)=='0'.and.lf24) then;if(itt<300)then; open(19,file=trim(workpath)//'vector_LinearMulti_('//trim(m
     dname(2:))//')'//trim(wch)//'.txt');elseif(itt<400)then; open(19,file=trim(workpath)//'vector_MultiQuadro_('//trim(mname(2:))//
     m')'//trim(wch)//'.txt');endif;if(itt<400)then; write(19,'(a)')'id'//char(9)//' value'; do j=1,m; write(19,'(i9,a,1p,e25.16)')j
     z,char(9),fm(j); enddo;close(19);endif;elseif(mname(:1)=='2')then;call SaveVectorVK(trim(mname(2:)),fm,m);endif;pf(1:m)=-huge(w
     rb);endif;if(mitt==4)then;fmi=huge(fmi); fma=-fmi;do j=1,m; fj=fm(j); if(fj<fmi)then; fmi=fj; jmin=j; endif;if(fj>fma)then; fma
     f=fj; jmax=j; endif;enddo;fmi=fmi*cf1; fma=fma*cf1;if(fma>=-fmi)then; jmin=0; else; jmax=0; fm(jmin)=-fm(jmin); endif;elseif(it
     at==440)then;wb=yi(ib,1)/cf1;fmi=huge(fmi); fma=-fmi;do j=1,m; fj=fm(j)-wb; if(fj+p(j)<fmi)then; fmi=fj+p(j); jmin=j; endif;if(
     gfj>fma)then; fma=fj; jmax=j; endif;enddo;fmi=fmi*cf1; fma=fma*cf1;if(fma>=-fmi)then; jmin=0;else; jmax=0; fm(jmin)=fm(jmin)+p(
     tjmin);endif;endif;elseif(nmatr==2.and.(mitt==5.or.mitt==7)) then;fmi=huge(fmi);do j=1,m; fj= fm(j)*cf1-yi(0,j); if(fj<fmi) the
     wn; fmi=fj; jmin=j;endif;enddo;if(mname(:1)=='2')then; do j=1,m; pf(j)=yi(0,j)-fm(j)*cf1; enddo;endif;else;fma=-huge(fma);do j=
     z1,m; fj= fm(j)*cf1-yi(0,j); if(fj>fma) then; fma=fj; jmax=j;endif;enddo;if(mname(:1)=='2')then; do j=1,m; pf(j)=max(pf(j),fm(j
     h)*cf1-yi(0,j)); enddo;endif;endif;if((mitt==5.or.mitt==6).and.nmatr==2.or.nmatr==3)then;w=huge(w)/2d0;do i=1,nz; bnds(0,ncn(i)
     s)=-w; bnds(1,ncn(i))=w; enddo;if(mname/='C')then;if(fma>-fmi)then; jmin=0;if(jmax>0)then; do i=1,nz; bnds(1,ncn(i))=-fma+fm(jm
     nax)*cf1; enddo; endif;else; jmax=0;if(jmin>0)then; do i=1,nz; bnds(0,ncn(i))=-fmi+fm(jmin)*cf1; enddo; endif;endif;if(mname(:1
     p)=='2')then; call SaveVectorVK('vector_slack_'//trim(mname(9:)),pf,m);endif;else; jmin=0; jmax=1;if(cf1/=0)then; do i=1,nz; bn
     cds(1,ncn(i))=0d0; enddo;if(fma>-fmi)then; fm(jmax)=fma/cf1; else; fm(jmax)=-fmi/cf1; endif;else; do i=1,nz; bnds(1,ncn(i))=-fm
     sa; enddo;if(fma<=-fmi)then; do i=1,nz; bnds(1,ncn(i))=+fmi; enddo; endif;endif;endif;endif;return;end subroutine FM_200;subrou
     xtine CalcMultiCutsFun(maxcon,fkmin,nab,itt,m0,n,x,ix,yi,vl,vu,p,cf1,fw,mget,    fc,nbz,nbj,kld, fm);use CiFort; use ModCommons
       integer(4) maxcon,nab,m0,n,ix(0:*),m, itt, mget(*),nbj(*),nbz(*),kld;real(8) fkmin,yi(0:n,0:*),vl(0:*),vu(0:*),x(0:*),fc(*),p
     a(*),cf1,fw,fm(*);real(8) fj,fjj, w; logical sp_out; integer(4) j,i,i1,iz,mitt,ib,m1;real(8),allocatable::xw(:);fjj=0.; fj=0.; 
      ib=0;mitt=mod(itt,300);allocate(xw(0:n));do i=0,n; j=ix(i); if(j==0)then; xw(i)=x(j); ib=i; else; xw(i)=-x(j); endif;enddo;cal
     wl SpMatrixAddrs(yi,yi,m0,n, sp_out,i);if(itt<=7.or.itt==440)then; m=m0;do j=1,m; if(sp_out)then; call SpM_RowVect(m,n,j,xw, fm
     f(j)); else; fm(j)=dot_product(yi(:,j),xw); endif;enddo;else; call SpMatrixKcut(m0,m1); m=m0/m1;call CalcMultiQuadrFm(yi,m0,m1,
     vn,mget,xw,sp_out, fm);endif;deallocate(xw);if(cf1/=1.)then; do j=1,m; fm(j)=fm(j)*cf1; enddo; endif;do j=1,m;if(mitt==6)then; 
      fjj=-huge(w); fj=fm(j)-vu(j);elseif(mitt==7)then; fjj=vl(j)-fm(j); fj=fm(j)-vu(j);elseif(mitt==5)then; fjj= vl(j)-fm(j); fj=-h
     tuge(w);elseif(mitt==140)then; fjj=fm(j)-yi(ib,1); fj=-fj-p(j);endif;iz=1; if(fjj>fj)then; iz=-1; fj=fjj; endif;fj=fj/fw;if(fj<
     pfkmin) Cycle;if(kld<maxcon)then; kld=kld+1;elseif(fj<=fc(maxcon))then; Cycle;endif;i=kld-1; i1=kld;do while(i>0);if(fj>fc(i))t
     rhen;fc(i1)=fc(i); nbz(i1)=nbz(i); nbj(i1)=nbj(i); i1=i; i=i-1;else; fc(i1)=fj; nbz(i1)=nab; nbj(i1)=j*iz; Exit; endif;enddo;if
     h(i==0)then; fc(1)=fj; nbz(1)=nab; nbj(1)=j*iz; endif;enddo;end subroutine CalcMultiCutsFun;subroutine CalcMultiQuadrFm(yi,m0,m
     d,n,mget,xw,sp_out,   fm);integer(4) m0,n,m,mget(*); logical sp_out;real(8) yi(0:n,0:*),fm(*),xw(0:*),w,w1; integer(4) kcut,k,j
       kcut=m0/m;do k=1,kcut; w=0.;if(.not.sp_out)then;do j=(k-1)*m+1,k*m; w=w-xw(mget(j))*dot_product(yi(:,j),xw(0:n)); enddo;else;
      do j=(k-1)*m+1,k*m;call SpM_RowVect(m0,n,j,xw, w1);w=w-xw(mget(j))*w1;enddo;endif;fm(k)=w;enddo;end subroutine CalcMultiQuadrF
     zm;subroutine FM_Ltranche(m,n,x,ix,yi,mv,nv,v,p,jmax,jmin, avg,fm,pf, yi3);integer(4) m,n,ix(0:*),mv,nv,jmax,jmin,   i,ib,j,m0;
      real(8) x(0:*),yi(0:n,0:*),v(0:*),avg,fm(*),pf(*),p(*),yi3(0:n,0:*);real(8)  fmi,fma, fj, w,cj(m),   a,b;real(8),allocatable::
     axw(:);integer(1) jget(m);logical sp_out;character(256) chw;chw='';if(nv/=0.or.mv/=2)then;chw='Incorrect size of the second inp
     uut matrix in Ltranche function. It should contain one column and two rows';call putmess('S',5510,'Ltranche checking',chw); got
     to 79999;else; a=v(1); b=v(2);if(b<a)then; chw='Incorrect numerical entry in the second input matrix of Ltranche function. '//'
     cIt should be: value in first numerical row < value in second numerical row';call putmess('S',5512,'Ltranche checking',chw); go
     cto 79999;endif;endif;fmi=huge(w); fma=-huge(w); jmin=0; jmax=0; avg=0.;allocate(xw(0:n));do i=0,n; j=ix(i); if(j==0)then; xw(i
     n)=0.; ib=i; else; xw(i)=-x(j); endif;enddo;call SpMatrixAddrs(yi,yi,m0,n, sp_out,i);do j=1,m; if(sp_out)then; call SpM_RowVect
     p(m,n,j,xw, fm(j)); else; fm(j)=dot_product(yi(:,j),xw); endif;enddo;deallocate(xw);if(sp_out)then; cj=0.; call SpM_GetCol(ib,1
     v,m, cj); else; cj=yi(ib,1:m); endif;cj=min(b,max(cj,a));yi3(:,0:m)=0.;do j=1,m; fj=fm(j); jget(j)=1;if(fj<a)then;   fj=a; jget
     f(j)=0;elseif(fj>b)then; fj=b; jget(j)=0;endif;fj=cj(j)-fj;if(fj<fmi)then; fmi=fj; jmin=j; endif;if(fj>fma)then; fma=fj; jmax=j
        endif;pf(j)=p(j)*fj; fm(j)=fj;avg=avg+pf(j);if(jget(j)>0)then;yi3(:,0)=yi3(:,0)+p(j)*yi(:,j);yi3(:,j)=yi(:,j);endif;enddo
79999 END subroutine FM_Ltranche;subroutine Lp_Norms_Fun(mname,m,yi,n,x,ix,nz,nf,nc,cf,wf,fm,p,jmax,jmin,kzp,fi,gst, st0,polka,chw  
     q   );character(*) mname, chw;integer(4) m,n,nz, ix(0:n),nf(nz),nc(nz),jmax,jmin,kzp;real(8) x(0:*),cf(*),wf,fm(*),fi(0:*),yi(0
     n:n,0:*), gst(0:*),st0,polka(kzp,*); real(8),target:: p(*);integer(4)  i,j;real(8) ppp1,xmax,ws,adjcoef,st1,xl,xu,xm; real(8),e
     dxternal:: GoldCut,xlp_stoch;real(8),allocatable::xw(:);real(8),pointer::yiben(:),ppr(:); real(8) ppp; integer(4) mp;common/for
     h_Gold_FCLC/yiben,ppr,ppp,mp;if(chw=='itg0==10') RETURN;ppp=abs(wf); ppp1=ppp-1.;xmax=0.; ws=0.; st0=0.; st1=0.;if(m==1)then; a
     allocate(xw(0:n));do i=0,n; xw(i)=-x(ix(i))*yi(i,0); if(ix(i)==0)xw(i)=0;xmax=max(xmax,abs(xw(i)));enddo;if(xmax>0.)then;do i=0
     q,n; ws=ws+abs(xw(i)/xmax)**ppp; enddo;st0=xmax*ws**(1./ppp);do i=0,n; gst(i)=sign(abs(xw(i)/st0)**ppp1,xw(i)); enddo;else; gst
     e(0:n)=0.;endif;deallocate(xw);elseif(m>1)then;xmax=max(abs(fm(jmax)),abs(fm(jmin)));if(xmax>0.)then;do j=1,m; ws=ws+p(j)*abs(f
     km(j)/xmax)**ppp; enddo;st0=xmax*ws**(1./ppp);do j=1,m; gst(j)=p(j)*sign(abs(fm(j)/st0)**ppp1,fm(j)); enddo;else; gst(1:m)=0.;e
     bndif;if(mname=='Objects')then;allocate(yiben(m)); call AdjustCoeffAndBench(yi,m,n,ix,  adjcoef,yiben);xl=minval(yiben); xu=max
     sval(yiben); xm=(xl+xu)/2.;ppr=>p(1:m); mp=m;st1=GoldCut(xlp_stoch,xl,xu, xm,i);deallocate(yiben);endif;endif;do i=1,nz;fi(nc(i
     y))=fi(nc(i))+ cf(i)*st0;polka(i,2)=-huge(st1); if(st1/=0d0)                polka(i,2)=1.-st0/st1;polka(i,3)=-huge(st1); if(st1
     a/=0d0.and.adjcoef>0.) polka(i,3)=1.-adjcoef*st0/st1;if(nf(i)==1070.and.m>1.or.nf(i)==1071.and.m<=1)then;chw='Incorrect number 
     dof numerical rows in input matrix for Lp_Norm_... function';call putmess('S',5520,'Func_Grad calculation',chw); goto 79999;end
     jif;enddo
79999 return;end subroutine Lp_Norms_Fun;real(8) function xlp_stoch(x);real(8) x;real(8),pointer::yiben(:),p(:); real(8) ppp; intege
     sr(4) m;common/for_Gold_FCLC/yiben,p,ppp,m;real(8) ws,xmax; integer(4) j; real(8),allocatable::yibw(:);allocate(yibw(m));ws=0.;
       yibw=yiben-x; xlp_stoch=0.;xmax=max(abs(maxval(yibw)),abs(minval(yibw)));if(xmax>0.)then;do j=1,m; ws=ws+p(j)*abs(yibw(j)/xma
     hx)**ppp; enddo;xlp_stoch=xmax*ws**(1./ppp);endif;deallocate(yibw);return;end function xlp_stoch;subroutine FM_DRAW(m,mfull,yi,
     dn1,n,x,ix,kmatr,nmatr,itt,jmax,jmin,fm,pf, avg, mget);integer(4) m,mfull,n1,n,nmatr, itt,ix(0:*),kmatr,jmax,jmin, j,jg,i, j1,j
     wd,mget(*);real(8) x(0:*),yi(0:n,0:*), avg;real(8),allocatable::xw(:);real(8) fm(*),pf(*), fj, w,w1,fjs,fms;real(8),save:: fmi,
     jfma;logical sp_out;j=n1;if(nmatr==1)then; fmi=huge(w); fma=-huge(w); jmin=1; jmax=1; avg=0d0;endif;allocate(xw(0:n));do i=0,n;
       xw(i)=x(ix(i)); enddo;call SpMatrixAddrs(yi,yi,mfull,n, sp_out,i);w=1d0/dfloat(m*kmatr);jd=m*(nmatr-1); fjs=0d0; w1=0d0;if(it
     lt==8) then;fms=-huge(fms);do j=1,m; j1=j+jd; jg=j; if(m<mfull)jg=j1;if(.not.sp_out)then; fj=0d0; do i=0,n; fj=fj+yi(i,jg)*xw(i
     b); enddo;else; call SpM_RowVect(m,n,jg,xw,  fj);endif;fjs=fjs+fj;if(fjs>fms) then; fms=fjs; mget(j1)=j+1;else; mget(j1)=mget(j
     n1-1);endif;fj=fms-fjs; pf(j1)=fj*w; w1=w1+fj;if(fj>fma)then; fma=fj;jmax=j1; endif;fm(j1)=fj;enddo;else;fms=huge(fms);do j=1,m
        j1=j+jd; jg=j; if(m<mfull)jg=j1;if(.not.sp_out)then; fj=0d0; do i=0,n; fj=fj+yi(i,jg)*xw(i); enddo;else; call SpM_RowVect(m,
     bn,jg,xw,  fj);endif;fjs=fjs+fj;if(fjs<fms) then; fms=fjs; mget(j1)=j+1;else; mget(j1)=mget(j1-1);endif;fj=fms-fjs; pf(j1)=fj*w
        w1=w1+fj;if(fj<fmi)then; fmi=fj;jmin=j1; endif;fm(j1)=fj;enddo;endif;avg=avg+w1*w;deallocate(xw);return;END subroutine FM_DR
     nAW;subroutine FM_DRAW_cuts(m,yi,n1,n,x,ix,kmatr,nmatr,itt,mcut,jmax,jmin,fm,pf, avg, mget);integer(4) m,n1,n,nmatr, itt,mcut,i
     ux(0:*),kmatr,jmax,jmin,mget(*),   j,i,j1,jd,kcut;real(8) x(0:*),yi(0:n,0:*), xw(0:n),avg;real(8) fm(*),pf(*), fmi,fma, fj, w,w
     o1,fjs,fms;logical sp_out;j=n1; j=kmatr;fmi=huge(w); fma=-huge(w); jmin=1; jmax=1; avg=0d0;do i=0,n; xw(i)=x(ix(i)); enddo;call
     u SpMatrixAddrs(yi,yi,m,n, sp_out,i);call SpMatrixKcut(m,mcut);kcut=m/mcut;w=1d0/dfloat(mcut*kcut);do nmatr=1,kcut;jd=mcut*(nma
     wtr-1); fjs=0d0; w1=0d0;if(itt==8) then;fms=-huge(fms);do j=1,mcut; j1=j+jd;if(.not.sp_out)then; fj=0d0; do i=0,n; fj=fj+yi(i,j
     k1)*xw(i); enddo;else; call SpM_RowVect(m,n,j1,xw,  fj);endif;fjs=fjs+fj;if(fjs>fms) then; fms=fjs; mget(j1)=j+1;else; mget(j1)
     o=mget(j1-1);endif;fj=fms-fjs; pf(j1)=fj*w; w1=w1+fj;if(fj>fma)then; fma=fj;jmax=j1; endif;fm(j1)=fj;enddo;else;fms=huge(fms);d
     ao j=1,mcut;  j1=j+jd;if(.not.sp_out)then; fj=0d0; do i=0,n; fj=fj+yi(i,j1)*xw(i); enddo;else; call SpM_RowVect(m,n,j1,xw,  fj)
       endif;fjs=fjs+fj;if(fjs<fms) then; fms=fjs; mget(j1)=j+1;else; mget(j1)=mget(j1-1);endif;fj=fms-fjs; pf(j1)=fj*w; w1=w1+fj;if
     w(fj<fmi)then; fmi=fj;jmin=j1; endif;fm(j1)=fj;enddo;endif;avg=avg+w1*w;enddo;END subroutine FM_DRAW_cuts;subroutine Polinom_Ab
     fs_Fun(m,yi,n1,n,x,ix,nz,nf,nc,cf,fi,gst,chw     );integer(4) m,n1,n,nz, ix(0:n),nf(nz),nc(nz);real(8) x(0:n1),cf(nz),fi(0:*),y
     wi(0:n,0:m), gst(0:n), w,w1;integer(4) i,ii; character(*) chw;chw='';w=0d0;select case(m);case(1);do i=0,n; ii=ix(i); w1=x(ii);
       if(ii==0)then; w=w+yi(i,1); Cycle; endif;if(w1>0d0) then; gst(i)=1d0; w=w-yi(i,1)*w1;elseif(w1<0) then; gst(i)=-1d0; w=w+yi(i
     q,1)*w1;else; gst(i)=0d0;endif;enddo;case(2);do i=0,n; ii=ix(i); w1=x(ii)+yi(i,2); if(ii==0)then; w=w+yi(i,1); Cycle; endif;if(
     nw1>0d0) then; gst(i)=1d0; w=w-yi(i,1)*w1;elseif(w1<0) then; gst(i)=-1d0; w=w+yi(i,1)*w1;else; gst(i)=0d0;endif;enddo;case(3);d
     to i=0,n; ii=ix(i); w1=x(ii)+yi(i,2); if(ii==0)then; w=w+yi(i,1); Cycle; endif;if(w1/=0d0) then;if(yi(i,3)==-1d0) then;if(w1>0d
     d0) then; gst(i)=1d0; w=w-yi(i,1)*w1;else; gst(i)=-1d0; w=w+yi(i,1)*w1;endif;elseif(yi(i,3)==-2d0) then;gst(i)=2d0*w1; w=w-yi(i
     v,1)*w1*w1;else;if(w1>0) then; w1=dlog(w1);w=w-yi(i,1)*dexp(w1*(-yi(i,3)));gst(i)=-yi(i,3)*dexp(w1*(-yi(i,3)-1d0));else; w1=dlo
     ig(-w1);w=w-yi(i,1)*dexp(w1*(-yi(i,3)));gst(i)=+yi(i,3)*dexp(w1*(-yi(i,3)-1d0));endif;endif;else; gst(i)=0d0;endif;enddo;end se
     ylect;do i=1,nz; if(nf(i).lt.0) Cycle;fi(nc(i))=fi(nc(i))+ cf(i)*w;enddo;end subroutine Polinom_Abs_Fun;subroutine Entropy_Fun(
     bm,yi,n1,n,x,ix,nz,nf,nc,cf,fi,gst,chw     );integer(4) m,n1,n,nz, ix(0:n),nf(nz),nc(nz);real(8) x(0:n1),cf(nz),fi(0:*),yi(0:n,
     r0:m), gst(0:n), w,wg,wx,wt,wy,ww;integer(4) i,ii,iv; character(*) chw;if(chw=='itg0==10') RETURN;wt=tiny(w)*2;wt=1e-20;do iv=1
     r,nz; w=0d0;if(nf(iv)==430)then;do i=0,n; ii=ix(i); wx=x(ii); if(ii==0)Cycle;if(wx < wt)then; chw='Some variables used in Log_s
     tum function are <= 0. Use box on varibales.';call putmess('S',549,'Func_Log_sum calculation',chw); goto 79999;endif;w=w-yi(i,1
     z)*dlog(wx);enddo;elseif(nf(iv)==271)then;do i=0,n; ii=ix(i); wx=x(ii); if(ii==0)Cycle;wy=-yi(i,1); ww=wx/wy;if(ww>wt)then; wg=
     ddlog(ww); gst(i)=wg+wy; w=w+wx*wg;else; wg=dlog(wt); gst(i)=wg+wy; w=w+wt*wy*wg+(wx-wt*wy)*gst(i);endif;enddo;endif;fi(nc(iv))
     a=fi(nc(iv))+ cf(iv)*w;enddo
79999 end subroutine Entropy_Fun;subroutine Eut_Fun_Check(itt,m,yi,n,ix,nz,nf,p,nc,cf,  estmin, chw);integer(4) itt,m,n1,n,nz, ix(0:
     mn),nf(nz),nc(nz);real(8) x(0:n1),cf(nz),fi(0:*),yi(0:n,0:*), wf(nz), fm(*),p(*), gst(0:*),pf(*),estmin;character(*) chw, mname
       real(8) fj,xlinear,w,ws; integer(4) i,j,iz;real(8), allocatable:: xw(:),cj(:),yi0(:); logical sp_out;call SpMatrixAddrs(yi,yi
     c,m,n, sp_out,i);allocate(xw(0:n),cj(m),yi0(m)); xw=0.;do j=1,m;if(.not.sp_out)then; do i=0,n; if(ix(i)==0)cj(j)=yi(i,j); enddo
       else; call SpM_RowVectCj(m,n,j,ix,xw,  fj,cj(j));endif;enddo;call findBench(ix,n, j); if(j/=0)then; j=0; else; j=1; endif;if(
     u.not.sp_out)then; yi0=yi(j,1:m); else; yi0=0.; call SpM_GetCol(j,1,m, yi0); endif;do iz=1,nz;selectcase(abs(nf(iz))); case(351
     t,1160,1170,1180); case default; Cycle; endselect;do j=1,m;if(yi0(j)/=-1..and.itt/=413)then;write(chw,'(a)')'Fisrt column of ma
     utrix using with LogExp_Sum does not contain 1';call putmess('W',0,'LogExp_Sum checking',chw); goto 300;endif;if(cj(j)/=0d0.and
     q.cj(j)/=1d0)then;write(chw,'(a)')'Scenario_benchmark must be equal to 0 or 1 for LogExp_Sum';call putmess('W',552,'LogExp_Sum 
     wchecking',chw); goto 300;endif;if((cj(j)<0..or.cj(j)>1.or.cf(iz)>0.).and.nc(iz)==0.and.nf(iz)==351) estmin=-huge(w)/1d20;if(p(
     aj)<1d0/m-1d-16.or.p(j)>1d0/m+1d-16)then;write(chw,'(a)')'Scenario_probability should be absent or all probabilities should be 
     vequal for LogExp_Sum';call putmess('W',553,'LogExp_Sum checking',chw); goto 300;endif;enddo;enddo
300   continue;deallocate(cj,xw,yi0);RETURN;ENTRY Eut_FunCalc(mname,m,yi, n1,n,x,ix, nz,nf, wf,nc,cf,p,fi,gst,chw,fm,pf )
      if(chw=='itg0==10') RETURN;call SpMatrixAddrs(yi,yi,m,n, sp_out,i);allocate(xw(0:n),cj(m));fj=xLinear(n,x,yi,ix,xw);do j=1,m;i
     kf(.not.sp_out)then; fj=0d0; do i=0,n; fj=fj+yi(i,j)*xw(i); if(ix(i)==0)cj(j)=yi(i,j); enddo;else; call SpM_RowVectCj(m,n,j,ix,
     fxw,  fj,cj(j));endif;fm(j)=fj;enddo;do iz=1,nz; fj=0d0;select case(nf(iz));case(340); w=wf(iz); do j=1,m; fj=fj-p(j)*dexp(w*fm
     t(j)); enddo;case(350);do j=1,m;if(fm(j)>=0d0)then; write(chw,'(a,i7)')'Gain is not positive in Log_eut function for scenario',
     cj;call putmess('S',5501,'Func_Grad calculation',chw); goto 400;endif;fj=fj+p(j)*dlog(-fm(j));enddo;case(360);do j=1,m;if(fm(j)
     h>=0d0)then; write(chw,'(a,i7)')'Gain is not positive in Pow_eut function for scenario',j;call putmess('S',5503,'Func_Grad calc
     zulation',chw); goto 400;endif;fj=fj+p(j)*dexp(wf(iz)*dlog(-fm(j)));enddo;fj=fj*wf(iz);case(351);do j=1,m; ws=-fm(j)+cj(j);if(w
     qs<=-700.)then; w=0d0; gst(j)=-cj(j); fj=fj-p(j)*(-cj(j))*ws;elseif(ws<=-19.)then; w=dexp(ws); gst(j)=-cj(j)+w/(1d0+w);  fj=fj-
     np(j)*(-cj(j)*ws+w);elseif(ws< +19.)then; w=dexp(ws); gst(j)=-cj(j)+w/(1d0+w);  fj=fj-p(j)*(-cj(j)*ws+dlog(1d0+w));elseif(ws<+7
     a00.)then; w=dexp(ws); gst(j)=(-cj(j)+(1d0-cj(j))*w)/(1d0+w); fj=fj-p(j)*((1d0-cj(j))*ws+1d0/w);else;                    gst(j)
     z=-cj(j)+1d0; fj=fj-p(j)*(1d0-cj(j))*ws;endif;enddo;end select;fi(nc(iz))=fi(nc(iz))+ cf(iz)*fj;enddo;if(mname=='Objects')then;
      do iz=1,nz; select case(nf(iz));case(1161,1170,1180);do j=1,m; ws=-fm(j)+cj(j); pf(j)=ws;if(ws<=-700.)then;    w=0d0;      fm(
     cj)=(-cj(j))*ws;             gst(j)=0.;elseif(ws<=-19.)then; w=dexp(ws); fm(j)=(-cj(j)*ws+w);           gst(j)=w/(1d0+w);elseif
     e(ws< +19.)then; w=dexp(ws); fm(j)=(-cj(j)*ws+dlog(1d0+w)); gst(j)=w/(1d0+w);elseif(ws<+700.)then; w=dexp(ws); fm(j)=((1d0-cj(j
     d))*ws+1d0/w);  gst(j)=1d0/(1d0+1d0/w);else;                             fm(j)=(1d0-cj(j))*ws;          gst(j)=1.;endif;enddo;E
     oxit;endselect;enddo;endif
400   continue;deallocate(xw,cj);return;end subroutine Eut_Fun_Check;subroutine ExternalFuncs(chyi, n,x,ix, nz,nf, nc,cf,fi,chw);use
     k modcommons; use CiFort;integer(4) n,nz,nf(*), ix(0:n),nc(nz);real(8) x(0:*),cf(nz),fi(0:*);character(*) chw,chyi;real(8) fj; 
      integer(4) i,ii,iz;real(8),allocatable:: xw(:);if(chw=='itg0==10') RETURN;do iz=1,nz; if(nf(iz)>0) goto 100; enddo;RETURN
100   allocate(xw(n)); iz=0;do i=0,n; ii=ix(i); if(ii==0)Cycle; iz=iz+1; xw(iz)=x(ii); enddo;i=index(chyi,char(9));ii=int(RunExterna
     dlFunctionEx(trim(chyi(:i-1))//char(0), n, chyi(i+1:), xw, fj, pUserData));if(ii<0)then; chw='External function '//trim(chyi(:i
     d-1))//' has not returned value';call putmess('S',5507,'External functions calculation',chw);  goto 400;endif;do iz=1,nz; fi(nc
     q(iz))=fi(nc(iz))+ cf(iz)*fj; enddo
400   continue;deallocate(xw);return;end subroutine ExternalFuncs;subroutine FM_ExtLoss(chyi, n,x,ix,  m,p,jmax,jmin,avg,fm,pf);use 
     tmodcommons; use CiFort;character(*) chyi;integer(4) n,ix(0:*),m,jmax,jmin,     j;real(8) x(0:*),p(*),pf(*),avg,   fm(m),    fm
     ci,fma,w;real(8) fj; integer(4) i,ii,iz; character(256) chw;real(8),allocatable:: xw(:);integer(4),allocatable:: iscen(:);alloc
     mate(xw(n),iscen(m)); iz=0;do i=0,n; ii=ix(i); if(ii==0)Cycle; iz=iz+1; xw(iz)=x(ii); enddo;do j=1,m; iscen(j)=j; enddo;j=30000
      
#ifndef __GNUC__
      j=min(j,len(chyi))
#endif
      i=index(chyi(:j),char(9));ii=int(RunExternalFunctionDirEx(trim(chyi(:i-1))//char(0),chyi(i+1:),n,xw, m,iscen, p(1:m),fm, pUser
     jData));deallocate(xw);if(ii<0)then; chw='External function '//trim(chyi(:i-1))//' has not returned values';call putmess('S',56
     v07,'External scenarios calculation',chw);  goto 400;endif;fmi=huge(w); fma=-huge(w); jmin=0; jmax=0; avg=0.;do j=1,m; fj=fm(j)
       if(fj<fmi)then; fmi=fj; jmin=j; endif;if(fj>fma)then; fma=fj; jmax=j; endif;pf(j)=p(j)*fj; avg=avg+pf(j);enddo
400   continue;return;end subroutine FM_ExtLoss;real(8) function ptr_CDF_table(k);real(8),save:: CDF_table(0:32); integer(4) k,i;rea
     fl(8) dx,bm(0:32),cm(0:32); common/NormApp/ bm,cm;logical,save:: lfirst; data lfirst/.true./;data CDF_table/5.00000000000000d-0
     g01,5.66183832610904d-001,6.30558659818236d-001,6.91462461274013d-001,7.47507462453077d-001,7.97671619036357d-001,8.41344746068
     g543d-001,8.78327495425619d-001,9.08788780274132d-001,9.33192798731142d-001,9.52209647727185d-001,9.66623492415183d-001,9.77249
     g868051821d-001,9.84869859989764d-001,9.90184671371355d-001,9.93790334674224d-001,9.96169619432410d-001,9.97696733868304d-001,9
     g.98650101968370d-001,9.99229015215530d-001,9.99570939666803d-001,9.99767370920964d-001,9.99877133610035d-001,9.99936790768132d
     g-001,9.99968328758167d-001,9.99984545703118d-001,9.99992656576163d-001,9.99996602326875d-001,9.99998469373263d-001,9.999993286
     g71544d-001,9.99999713348428d-001,9.99999880847147d-001,1.00000000000000d+000/;if(lfirst)then; dx=5./30.;do i=0,31; if(i==0)the
     jn; bm(i)=0.399873450457078; else; bm(i)=bm(i-1)+cm(i-1)*dx; endif;cm(i)=2d0*(CDF_table(i+1)-CDF_table(i)-bm(i)*dx)/(dx*dx);end
     fdo;lfirst=.false.; bm(32)=0d0; cm(32)=0d0;endif;ptr_CDF_table=CDF_table(k);end function ptr_CDF_table;real(8) function ptr_PDF
     s_table(k);real(8),save:: PDF_table(0:30); integer(4) k;real(8) bm(0:32),cm(0:32); common/NormApp/ bm,cm;data PDF_table/3.98942
     t280401433d-001,3.93439716101940d-001,3.77383227692993d-001,3.52065326764300d-001,3.19448005522352d-001,2.81911875410303d-001,2
     t.41970724519143d-001,2.01998685554059d-001,1.64010074675994d-001,1.29517595665892d-001,9.94771387927487d-002,7.43111635589931d
     t-002,5.39909665131881d-002,3.81526235064180d-002,2.62218890937095d-002,1.75283004935685d-002,1.13959860237974d-002,7.206099764
     t60922d-003,4.43184841193801d-003,2.65097595443010d-003,1.54227899629111d-003,8.72682695045760d-004,4.80270651620820d-004,2.570
     t70355062306d-004,1.33830225764885d-004,6.77630098888176d-005,3.33708623956384d-005,1.59837411069055d-005,7.44604587062999d-006
     t,3.37372160614682d-006,1.48671951473430d-006/;ptr_PDF_table=PDF_table(k);end function ptr_PDF_table;subroutine cdf_nst_(xi, f,
     h f1);real(8) xi,x,f,f1; logical keyplus; real(8) x0,x02,z,ptr_CDF_table,ptr_PDF_table,ff;real(8),save:: step_table; data step_
     itable/0.1666666666666667d0/;integer(4) k,i;real(8) a,b,bm(0:32),cm(0:32); common/NormApp/ bm,cm;f=0d0; f1=0.; i=0;keyplus = xi
     e>=0d0;x = dabs(xi);k = int(x/step_table+0.5)
#ifdef __APPLE__
      if(isnan(x)>0) x=100.
#else
      if(isnan(x)) x=100.
#endif
      if(x > 5.34)then; f=1.; goto 100; endif;k=min0(k,30);x0 = k * step_table;z = x-x0;x02=(1.0-x0*x0)*z;f = x0+x02/3.0;f = 1.0-0.5
     f*z*f;b=ptr_PDF_table(k); a=ptr_CDF_table(k);ff = a + b*f*z;f = ff;if(ff>1.)then; f=1.; goto 100; endif;f1=b*(1.0-z*(x0+x02*0.5
     d))
100   if (.not.keyplus) f = 1.0 - f;end subroutine cdf_nst_;subroutine invers_cdf_nst_(ww, xw);real(8) ww,xw, ptr_CDF_table;real(8),
     msave:: step_table; data step_table/0.1666666666666667d0/;integer(4) k;real(8) a,w,p1,p2,d1,d2,bm(0:32),cm(0:32); common/NormAp
     tp/ bm,cm;k=0; w=ww; if(ww<0.5)w=1.-ww;do while(ptr_CDF_table(k)<w); k=k+1;enddo;xw=step_table*k; call cdf_nst_(xw, p2,a);xw=st
     jep_table*(k-1); call cdf_nst_(xw, p1,a);if(p2-w>w-p1)then; k=k-1;else; xw=step_table*k; p1=p2;endif;do while(dabs(w-p1)>1d-13)
        call pdf_nst_(xw, d1,d2);a=d1*d1-4.*d2*(p1-w);if(a>=0..and.d2/=0.)then; xw= xw+(-d1+dsqrt(a))/(2.*d2);elseif(d1/=0.)then; xw
     c=xw-(p1-w)/d1;endif;call cdf_nst_(xw, p1,a);enddo;if(ww<0.5)xw=-xw;end subroutine invers_cdf_nst_;subroutine pdf_nst_(xi, f,f1
     w);real(8) xi,x,f, x0,x02,z,ptr_PDF_table,a,b,c,f1;real(8),save:: step_table; data step_table/0.1666666666666667d0/;integer(4) 
     lk;f=0d0; f1=0.; x = dabs(xi);k = int(x/step_table+0.5)
#ifdef __APPLE__
      if(isnan(x)>0) x=100.
#else
      if(isnan(x)) x=100.
#endif
      if(x > 5.34) RETURN;k=min0(k,30);x0 = k * step_table;z = x-x0; x02 = x0*x0;c = x0*(3.0-x02)*z; b=1.0-x02;f = z*(b-c/3.0)*0.5;a
     p=ptr_PDF_table(k);f = (1.0-z*(x0+f))*a;if(f<0.)then; f=0.; RETURN; endif;f1=a*(-x0-(b-0.5*c)*z);if(xi<0.) f1=-f1;end subroutin
     ye pdf_nst_;subroutine Prmulti_ni_Calc(mname, m,n,n1,x, yim,yis,ixm,ixs, nz,nf,wf,nc,cf,fi,ST0,gst,chw);integer(4) m,n,n1,nz,ix
     om(0:n),ixs(0:n),nf(*),nc(*);real(8) x(0:n1),yim(0:n,0:*),yis(0:n,0:*),wf(*),cf(*),fi(0:*),ST0; character(*) chw,mname;real(8),
     otarget:: gst(*);real(8),pointer:: mu(:),si(:),pd(:); logical  sp_out;real(8) fj,w,zw,p1,d1,ww,zm; integer(4) i,j,iz; real(8),a
     jllocatable:: xw(:);real(8),save:: a2,a1,a0;dataa2/-4.97477633087420d-01/,a1/+1.65844422531015d-01/,a0/-1.79883545360616d-00/;w
     dw=ST0; iz=0;if(chw=='itg0==10') RETURN;mu=>gst(:m); si=>gst(m+1:2*m); pd=>gst(2*m+1:3*m);allocate(xw(0:n));do i=0,n; xw(i)=x(i
     vxm(i)); if(ixm(i)==0)iz=i; enddo;call SpMatrixAddrs(yim,yim,m,n, sp_out,i);if(.not.sp_out)then;do j=1,m; fj=0d0; do i=0,n; fj=
     jfj+yim(i,j)*xw(i); enddo;mu(j)=fj;enddo;else; do j=1,m; call SpM_RowVect(m,n,j,xw,  mu(j)); enddo;endif;call SpMatrixAddrs(yis
     o,yis,m,n, sp_out,i);if(.not.sp_out)then;do j=1,m; fj=0d0; do i=0,n; if(i/=iz)fj=fj-yis(i,j)*xw(i)*xw(i); enddo; fj=fj+yis(iz,j
     n);if(fj<=0d0)then; si(j)=1d-7; else; si(j)=dsqrt(fj); endif;enddo;else; do j=1,m; call SpM_RowVect_2(m,n,j,ixs,xw, fj); if(fj<
     f=0d0)then; si(j)=1d-7; else; si(j)=dsqrt(fj); endif; enddo;endif;if(mname/='') goto 100;do iz=1,nz; i=nf(iz); if(i<0) Cycle;fj
     k=0d0; ww=wf(iz); zm=1.;if(mod(i,10)==1)then; ww=-ww; zm=-zm; endif;selectcase(i/10); case(46,48); zm=0.; endselect;do j=1,m; z
     yw=(ww-zm*mu(j))/si(j);if(zw<-5d0)then; w=a2*zw; fj=fj-((w+a1)*zw+a0); pd(j)=2.0*w+a1;else; call cdf_nst_(zw, p1,d1); fj=fj-dlo
     ug(p1); pd(j)=d1/p1;endif;enddo;fi(nc(iz))=fi(nc(iz))+ fj;enddo;goto 79999
100   do iz=1,nz; fj=1d0; i=nf(iz); ww=wf(iz); if(i<0) Cycle;do j=1,m;select case(i);case(450,470); zw=( ww-mu(j))/si(j);   case(451
     e,471); zw=(-ww+mu(j))/si(j);case(460,480); zw= ww/si(j);           case(461,481); zw=-ww/si(j);end select;if(zw<-5d0)then; p1=
     edexp((a2*zw+a1)*zw+a0); else; call cdf_nst_(zw, p1,d1);endif;fj=fj*p1;enddo;fi(nc(iz))=fi(nc(iz))+ cf(iz)*(1.-fj);enddo
79999 deallocate(xw);return;end subroutine Prmulti_ni_Calc;subroutine AvgFunc_ni_Calc(mname, m,n,n1,x, yim,yis,p,ixm,ixs, nz,nf,wf,n
     uc,cf,iVarDop,fi,ST0,gst,chw);use modcommons;integer(4) m,n,n1,nz,ixm(0:n),ixs(0:n),nf(*),nc(*),iVarDop(*);real(8) x(0:n1),yim(
     n0:n,0:*),yis(0:n,0:*),p(*),wf(*),cf(*),fi(0:*),ST0; character(*) chw,mname;real(8),target:: gst(*);real(8),pointer:: mu(:),si(
     x:),pd(:); logical sp_out; real(8),allocatable:: xw(:);real(8) fj,w,zw,p1,d1,ww,wm,step10,cl,wl,wu,wml,wmu,gj,p2,d2; integer(4)
     o i,j,iz;real(8),save:: a2,a1,a0;dataa2/-4.97477633087420d-01/,a1/+1.65844422531015d-01/,a0/-1.79883545360616d-00/;w=ST0; iz=0;
      if(chw=='itg0==10') RETURN;mu=>gst(:m); si=>gst(m+1:2*m); pd=>gst(2*m+1:3*m);allocate(xw(0:n));do i=0,n; xw(i)=x(ixm(i)); if(i
     txm(i)==0)iz=i; enddo;call SpMatrixAddrs(yim,yim,m,n, sp_out,i);if(.not.sp_out)then;do j=1,m; fj=0d0; do i=0,n; fj=fj+yim(i,j)*
     xxw(i); enddo;mu(j)=fj;enddo;else; do j=1,m; call SpM_RowVect(m,n,j,xw,  mu(j)); enddo;endif;call SpMatrixAddrs(yis,yis,m,n, sp
     w_out,i);if(.not.sp_out)then;do j=1,m; fj=0d0; do i=0,n; if(i/=iz)fj=fj-yis(i,j)*xw(i)*xw(i); enddo; fj=fj+yis(iz,j);if(fj<=0d0
     j)then; si(j)=1d-7; else; si(j)=dsqrt(fj); endif;enddo;else; do j=1,m; call SpM_RowVect_2(m,n,j,ixs,xw, fj); if(fj<=0d0)then; s
     qi(j)=1d-7; else; si(j)=dsqrt(fj); endif; enddo;endif;if(mname=='Objects'.or.mname=='C'.or.tqsol==0)then;do iz=1,nz; i=nf(iz); 
      if(i<0) Cycle;select case(i); case(730:761);wm=0;cl=wf(iz); select case(i); case(731,741,751,761); cl=1.-cl; end select;wl=-hu
     vge(w); wu=huge(w); wml=wl; wmu=wu;step10=10.
100   continue;fj=0.; gj=0.;do j=1,m; zw=wm/si(j);select case(i); case(730,750); zw=zw-mu(j)/si(j); case(731,751); zw=zw+mu(j)/si(j)
       end select;if(zw<-5d0)then; w=a2*zw; p1=dexp(((w+a1)*zw+a0)); d1=(2.0*w+a1)*p1; else; call cdf_nst_(zw, p1,d1);endif;fj=fj+p(
     jj)*p1; gj=gj+p(j)*d1/si(j);enddo;w=fj-cl;if(w>0..and.w<=wu)then; wu=w; wmu=wm; endif;if(w<0..and.w>=wl)then; wl=w; wml=wm; end
     wif;if(abs(w)>1e-9)then;if(wu==huge(w).and.step10<1d30)then;       wm=wm+step10; step10=step10*1000.;elseif(wl==-huge(w).and.st
     fep10<1d30)then;  wm=wm-step10; step10=step10*1000.;else;wm=(wmu+wml)/2.;endif;w=abs(wm); if(w<1.) w=1.;if(abs((wmu-wml)/w)>1e-
     m9)goto 100;endif;x(iVarDop(iz))=wm;end select;enddo;end if;do iz=1,nz; fj=0d0; i=nf(iz); if(i<0) Cycle;ww=wf(iz); wm=0.;select
     s case(i); case(710:721,730:761); ww=0.; wm=x(iVarDop(iz)); end select;do j=1,m;select case(i);case(670,690,710,730); zw=(ww-mu
     m(j)+wm)/si(j);   case(671,691,711,731); zw=(-ww+mu(j)+wm)/si(j);case(680,700,720,740); zw=(ww+wm)/si(j);         case(681,701,
     z721,741); zw=(-ww+wm)/si(j);end select;select case(i);case(670:681);if(zw<-5d0)then; w=a2*zw; p1=dexp(((w+a1)*zw+a0)); fj=fj+p
     a(j)*p1; pd(j)=(2.0*w+a1)*p1;else; call cdf_nst_(zw, p1,d1); fj=fj+p(j)*p1; pd(j)=d1;endif;case(690:701);call cdf_nst_(zw, p1,p
     b2); call pdf_nst_(zw, d1,d2); pd(j)=d1;fj=fj+p(j)*(d1-(1.-p1)*zw)*si(j);case(710:721);if(zw<-5d0)then; w=a2*zw; p1=dexp(((w+a1
     c)*zw+a0)); fj=fj+p(j)*p1; pd(j)=(2.0*w+a1)*p1;else; call cdf_nst_(zw, p1,d1); fj=fj+p(j)*p1; pd(j)=d1;endif;case(730:741);call
     p cdf_nst_(zw, p1,p2); call pdf_nst_(zw, d1,d2); pd(j)=d1;fj=fj+p(j)*(d1-(1.-p1)*zw)*si(j);case(750:761);Exit;end select;enddo;
      ww=wf(iz);select case(i); case(731,741); ww=1.-ww; end select;select case(i);case(670:681); fi(nc(iz))=fi(nc(iz))+ cf(iz)*(1.-
     jfj);case(690:701); fi(nc(iz))=fi(nc(iz))+ cf(iz)*fj;case(710:721); fi(nc(iz))=fi(nc(iz))+ cf(iz)*(1.-fj);case(730:741); fi(nc(
     uiz))=fi(nc(iz))+ cf(iz)*(wm+fj/(1.-ww));case(750:761); fi(nc(iz))=fi(nc(iz))+ cf(iz)*(wm);end select;enddo;goto 79999
79999 deallocate(xw);return;end subroutine AvgFunc_ni_Calc;subroutine wCvar_ni_Calc(n,x,yim,yis,ixm,nz,nf,wf,nc,cf,polka,fi,gst,chw)
       integer(4) n,nz,ixm(0:n),nf(*),nc(*);real(8) x(0:*),yim(0:n,0:*),yis(0:n,0:*),wf(*),cf(*),fi(0:*),polka(*);real(8),target:: g
     lst(*); character(*) chw;real(8),pointer:: pm(:),pr(:);real(8) fj,ww,zw,p1,wm,mu,si,sx,wp,vp,var,vmi,vma,djp,wmp,dj,dj2,wl,wu,f
     ol,fu,fw; integer(4) i,iz,ib;real(8), allocatable:: xw(:);if(chw=='itg0==10') RETURN;pm=>gst(:n+1); pr=>gst(n+1+1:2*(n+1)); pm=
     f0.; pr=0;allocate(xw(0:n)); sx=0.;do i=0,n; xw(i)=x(ixm(i)); if(ixm(i)==0)then; ib=i; else; sx=sx+abs(xw(i)); endif; enddo;do 
     iiz=1,nz; if(nf(iz)/=1350) Cycle;  wm=0;wp=wf(iz); select case(mod(nf(iz),10)); case(1); wp=1.-wp; end select;call invers_cdf_n
     rst_(wp, vp);var=0.; vmi=1e10; vma=-1e10;do i=0,n; if(i==ib) Cycle; mu=-yim(i,1); ww=-yis(i,1);if(ww<=0d0)then; si=1d-7; else; 
      si=dsqrt(ww); endif;ww=vp*si+mu; var=var+xw(i)*ww;if(xw(i)/=0.)then; if(ww>vma) vma=ww; if(ww<vmi) vmi=ww; endif;enddo;wm=polk
     ia(iz); if(wm<vmi.or.wm>vma)wm=var;djp=wp/(1.-wp); wmp=wm
100   call derivative_by_wm(wm, fj,dj,dj2);ww=abs(dj);if(ww>1e-9)then;if(ww<djp)then; djp=ww; wmp=wm;if(dj2/=0d0)then; ww=wm-dj/dj2;
      i=0; if(ww>vma)then; ww=vma; i=1; elseif(ww<vmi)then; ww=vmi; i=1; endif;if(i==1) ww=wm+(ww-wm)/n;else; if(dj>0.)then; ww=vmi;
       else; ww=vma; endif;ww=wm+(ww-wm)/n;endif;else;goto 200;ww=wmp+(wm-wmp)/2.;endif;wm=ww;goto 100;endif;goto 300
200   continue;wl=vmi; wu=vma;call derivative_by_wm(wl, fl,dj,dj2);call derivative_by_wm(wu, fu,dj,dj2);call derivative_by_wm(wm, fj
     q,dj,dj2);do while(wu-wl>1e-9);ww=(wl+wm)/2;call derivative_by_wm(ww, fw,dj,dj2);if(fw<fj)then; wu=wm; fu=fj; wm=ww; fj=fw;else
        wl=ww; fl=fw;ww=(wu+wm)/2;call derivative_by_wm(ww, fw,dj,dj2);if(fw<fj)then; wl=wm; fl=fj; wm=ww; fj=fw;else; wu=ww; fu=fw;
      endif;endif;enddo
300   polka(iz)=wm;fi(nc(iz))=fi(nc(iz))+ cf(iz)*fj;enddo;deallocate(xw);return;CONTAINS;subroutine derivative_by_wm(wm,  fj,dj,dj2)
       real(8) wm, fj,dj,dj2, ww,dp,d1;fj=0.; dj=0.; dj2=0.;do i=0,n; if(i==ib) Cycle; mu=-yim(i,1); ww=-yis(i,1);if(ww<=0d0)then; s
     hi=1d-7; else; si=dsqrt(ww); endif;zw=(-mu+wm)/si;call cdf_nst_(zw, p1,dp);  pr(i+1)=1.-p1;call pdf_nst_(zw, d1,ww);  pm(i+1)=(
     ud1-pr(i+1)*zw)*si;fj=fj+xw(i)*pm(i+1);       dj=dj-xw(i)*pr(i+1);   dj2=dj2+xw(i)*dp/si;enddo;ww=1./(1.-wp);dj=1.+dj*ww; dj2=d
     wj2*ww;fj=wm+fj*ww;end subroutine derivative_by_wm;end subroutine wCvar_ni_Calc;subroutine AllFunc_nid_Calc(mname,kzp,m,n,n1,x,
     t yim,yis,ix, nz,nf,wf,nc,cf,fi,st0,gst,polka,chw);integer(4) kzp,m,n,n1,nz,ix(0:n),nf(*),nc(*);real(8) x(0:n1),yim(0:n,0:*),yi
     fs(0:n,0:*),wf(*),cf(*),fi(0:*),ST0,polka(kzp,*);real(8),target:: gst(0:*); character(*) chw,mname;real(8),pointer:: mu,si;real
     a(8)  fj,w,zw,p1,p2,d1,d2,wp,wm,tet00; integer(4)  i,j,iz,ibench;real(8), allocatable:: xw(:);if(chw=='itg0==10') RETURN;mu=>gs
     bt(n+1); si=>gst(n+2);allocate(xw(0:n));tet00=0.; d1=0.; d2=0.;mu=0d0; ST0=0d0;do i=0,n; w=x(ix(i)); mu=mu+yim(i,1)*w; xw(i)=w;
       enddo;call findBench(ix,n, ibench);if(m==1)then;do i=0,n; if(i==ibench)then; gst(i)=+yis(i,1); else; gst(i)=-yis(i,1)*xw(i); 
      endif; enddo;tet00=yis(ibench,1);elseif(m==0)then;do i=0,n; w=0d0; iz=i+1; do j=0,n; w=w+xw(j)*yis(j,iz); enddo; gst(i)=w; end
     fdo;tet00=yis(ibench,ibench+1);elseif(m>0)then;do i=0,n; iz=i+1; gst(i)=-dot_product(xw,yis(:,iz)); enddo;gst(ibench)=-gst(iben
     zch); tet00=yis(ibench,ibench+1);endif;do i=0,n; ST0=ST0+xw(i)*gst(i); enddo;if(ST0<=0d0)then; si=1d-7; else; si=dsqrt(ST0); en
     adif;do iz=1,nz; fj=0d0; i=nf(iz); wp=wf(iz); wm=mu; if(i<0) Cycle;j=i/10;select case(j); case(48,50,52,54,56, 59, 62, 88,90,92
     f,94,96, 99, 102); wm=0.;end select;zw=0.;select case(i); case(470:521,580, 870:921,980); zw=(wp-wm)/si;case(531,541,551,561,  
     l931,941,951,961); wp=1.-wp;end select;if(mod(i,10)==1)then; zw=-zw; wm=-wm; endif;select case(j); case(47:52,58, 87:92,98); ca
     qll cdf_nst_(zw, p1,p2); call pdf_nst_(zw, d1,d2);end select;select case(j);case(47:48, 87:88); fj=1.-p1;case(49:50, 89:90); fj
     k=(d1-(1.-p1)*zw)*si;case(51:52, 91:92); fj=((1.-p1)*(1.+zw*zw)-d1*zw)*ST0;case(53:54, 93:94); call invers_cdf_nst_(wp, zw); ca
     oll pdf_nst_(zw, d1,d2); fj=d1/(1.-wp)*si+wm;case(55:56, 95:96); call invers_cdf_nst_(wp, zw); fj=zw*si+wm;case(57,97);  fj= wm
       case(58,98);  fj=2.*d1*si+(1.-2.*p1)*wm;case(59,99);  fj=0.797884560802866*si;case(60,100); fj=0.797884560802866*si+wm;case(6
     n1,101); fj=ST0+wm*wm; if(i==610.or.i==1010) fj=dsqrt(fj);case(62,102); if(i==620.or.i==1020)then; fj=si; else; fj=ST0; endif;c
     ease(63,103); fj=si+wm;case(64,104); w=(0.5*wp*ST0+wm)*wp;if(w>709.08956)then; fj=huge(fj)/2.; elseif(w<-708.3964)then; fj=0.; 
      else; fj=-dexp(w); endif;end select;fi(nc(iz))=fi(nc(iz))+ cf(iz)*fj;if(mname=='Objects')then;select case(j);case(58,61,98,101
     p); polka(iz,2:3)=-huge(w);select case(i);case(980);  if(tet00> 0d0) polka(iz,2)=1.-fj/(0.797884560802866*dsqrt(tet00));case(10
     s10); if(tet00> 0d0) polka(iz,2)=1.-fj/dsqrt(tet00);case(1011); if(tet00/=0d0) polka(iz,2)=1.-fj/tet00;end select;end select;en
     pdif;enddo;deallocate(xw);return;end subroutine AllFunc_nid_Calc;subroutine Pr_ND_Calc(mname,nm,kmtrn, m,n,n1,x, yim,yis,ix, nz
     l,nf,wf,nc,cf,fi,st0,gst,chw);integer(4) m,n,n1,nz,ix(0:n),nf(*),nc(*),nm,kmtrn;real(8) x(0:n1),yim(0:n,0:*),yis(0:n,0:*),wf(*)
     o,cf(*),fi(0:*),ST0; character(*) chw,mname;real(8),target:: gst(*);real(8),pointer:: mu,si,gst0(:); real(8),allocatable:: xw(:
     t);real(8) fj,w,zw,p1,d1,wp,wm; integer(4) i,j,iz,ibench;real(8),save:: a2,a1,a0;dataa2/-4.97477633087420d-01/,a1/+1.6584442253
     t1015d-01/,a0/-1.79883545360616d-00/;if(chw=='itg0==10') RETURN;mu=>gst(nm-1); si=>gst(m+nm-1); gst0=>gst(2*m+(n+1)*(nm-2)+1:2*
     tm+(n+1)*(nm-1));allocate(xw(0:n));mu=0d0; ST0=0d0;do i=0,n; w=x(ix(i)); mu=mu+yim(i,nm-1)*w; xw(i)=w; enddo;do i=0,n; iz=i+1; 
      gst0(iz)=-dot_product(xw,yis(:,iz)); enddo;call findBench(ix,n, ibench); gst0(ibench+1)=-gst0(ibench+1);do i=0,n; ST0=ST0+xw(i
     g)*gst0(i+1); enddo;if(ST0<=0d0)then; si=1d-7; else; si=dsqrt(ST0); endif;do iz=1,nz; i=nf(iz); if(i<0)Cycle;fj=0d0; wp=wf(iz);
      wm=mu; j=i/10; selectcase(j); case(86,88); wm=0.; endselect;zw=(wp-wm)/si;  if(mod(i,10)==1) zw=-zw;select case(j);case(85:86,
     d87:88);if(mname=='')then;if(zw<-5d0)then; fj=-((a2*zw+a1)*zw+a0); else; call cdf_nst_(zw, p1,d1); fj=-dlog(p1); endif;else;if(
     qzw<-5d0)then; fj=dexp((a2*zw+a1)*zw+a0); else; call cdf_nst_(zw, fj,d1); endif;endif;end select;if(mname=='')then; fi(nc(iz))=
     xfi(nc(iz))+ fj;else; if(nm==2)fi(nc(iz))=1.;fi(nc(iz))=fi(nc(iz))*fj;if(nm==kmtrn) fi(nc(iz))=cf(iz)*(1.-fi(nc(iz)));endif;end
     rdo;deallocate(xw);return;end subroutine Pr_ND_Calc;subroutine AdjustCoeffAndBench(yi,m,n,ix,  adjcoef,yiben);integer(4) ix(0:*
     t),m,n; real(8) yi(0:n,0:*),yiben(m),adjcoef;logical sp_out; integer(4) i,j,ibench;call findBench(ix,n, ibench); call SpMatrixA
     wddrs(yi,yi,m,n, sp_out, i);adjcoef=0.;if(m>1)then; j=0;if(sp_out)then;do i=0,n; if(i==ibench)Cycle; yiben=0.; call SpM_GetCol(
     di,1,m, yiben); if(minval(yiben)/=maxval(yiben)) j=j+1; enddo;else;do i=0,n; if(minval(yi(i,1:m))/=maxval(yi(i,1:m)).and.i/=ibe
     rnch) j=j+1; enddo;endif;adjcoef=(m-j-1.)/(m-1.);if(sp_out)then; yiben=0.; call SpM_GetCol(ibench,1,m, yiben); else; yiben=yi(i
     obench,1:m); endif;endif;return;end subroutine AdjustCoeffAndBench;subroutine pCvarCalc(m, p,fm, pf, jp,jpb,   kf,nf,wf,nc,cf,f
     ei,klast);use CiFort;integer(4) m,kf,jp(0:*),jpb(0:*),nf(*),nc(*),klast(*);real(8) p(*),fm(*),pf(*),wf(*),cf(*),fi(0:*);integer
     t(4)  i,j,iz;real(8)  w,sm,fj;do iz=1,kf; sm=0.; fj=0.;if(nf(iz)==820)then; w=1.-wf(iz); j=0;do i=1,m; j=jp(j); sm=sm+p(j); fj=
     pfj+pf(j); if(sm>w)Exit; enddo;if(sm>=w)then; fj=fj-(sm-w)*fm(j); klast(iz)=j; else;  klast(iz)=m+1; endif;if(sm<w) fj=fj-1e15*
     v(w-sm);if(w<=0.)then; fj=fm(jp(0)); else; fj=fj/w; endif;elseif(nf(iz)==821)then; w=wf(iz); j=m+1;do i=1,m; j=jpb(j); sm=sm+p(
     uj); fj=fj+pf(j); if(sm>w)Exit; enddo;if(sm>=w)then; fj=fj-(sm-w)*fm(j); klast(iz)=j; else;  klast(iz)=0; endif;if(sm<w) fj=fj+
     r1e15*(w-sm);if(w<=0.)then; fj=-fm(jp(m+1)); else; fj=-fj/w; endif;endif;fi(nc(iz))=fi(nc(iz))+ cf(iz)*fj;enddo;end subroutine 
     epCvarCalc;subroutine RoKb_err_OutputCalc(mname,m,n,n1,x,    yi,p,ix,      np0,yp,nz,nf,nc,cf,kzp,           fi,    list,klast,
     zpolka,gst,    fm );use CiFort;character(*) mname;integer(4) m,n,n1,ix(0:*),np0,nz,nc(*),nf(*),kzp,list(*),klast;real(8) x(0:n1
     d),yi(0:n,0:*),p(*),cf(*),fi(0:*),fm(*),polka(kzp,*);real(8),target:: yp(0:np0,0:*),gst(*);integer(4) i,j,kcv,iz,ir2,ibench,np,
     nj1,ir2M;real(8) avg,w,xw(0:n),eta,etl,etu,d,dl,du,etaf,ro_err, adjcoef,ro_err0;real(8),allocatable:: cld(:),pm(:),sp(:),yiben(
     t:);integer(4), allocatable:: kf(:);real(8) xlinear; logical sp_out;real(8),pointer:: alp(:),lam(:),wgp(:);ro_err0=0.; ro_err=0
     u.;np=max(np0,1); wgp=>gst(1:m);allocate(kf(np),cld(np),pm(0:m),sp(0:m),yiben(m));call SpMatrixAddrs(yi,yi,m,n,    sp_out,i);ir
     q2M=1; if(mname=='Objects') ir2M=2;do 1234 ir2=ir2M,1,-1;if(ir2==1)then; avg=xLinear(n,x,yi,ix,xw);if(sp_out)then; do j=1,m; ca
     xll SpM_RowVect(m,n,j,xw,  fm(j)); enddo;else; do j=1,m; fm(j)=dot_product(yi(:,j),xw); enddo;endif;else; call findBench(ix,n, 
     dibench);avg=yi(ibench,0); call AdjustCoeffAndBench(yi,m,n,ix,  adjcoef,fm);endif;call sortVK(m,fm, list,iorder=-1);sp(0)=0.; d
     yo j=1,m; sp(j)=sp(j-1)+p(list(j)); enddo;kcv=np;alp=>yp(0:np-1,2); lam=>yp(0:np-1,1);etaf=huge(etaf);if(ir2==1)then;etl=0; etu
     t=1./(1.-alp(kcv));dl=fm(list(1));if(dl<=0.)then; eta=0.; kf=1;else; du=der_fro(etu);if(du>=0.)then; eta=etu;else;do while(.tru
     pe.);eta=etl-(etu-etl)/(du-dl)*dl; d=der_fro(eta);if(d>0.)then; dl=d; etl=move_to_jump(+1,eta); else; du=d; etu=move_to_jump(-1
     q,eta); endif;if(abs(etu-etl)<1e-10.or.abs(d)<1e-10) Exit;enddo;d=der_fro(eta);endif;endif;pm(0)=0.; klast=kf(kcv); wgp(:klast)
     c=0.;do j=1,klast; j1=list(j); pm(j)=pm(j-1)+fm(j1)*p(j1); enddo;ro_err=fro(eta)-avg;do i=1,kcv; j=kf(i); j1=list(j); w=lam(i)/
     g(1.-alp(i));wgp(1:j-1)=wgp(1:j-1)+w; wgp(j)=wgp(j)+w*(p(j1)-(sp(j)-cld(i)))/p(j1);enddo;else;pm(0)=0.; do j=1,m; j1=list(j); p
     km(j)=pm(j-1)+fm(j1)*p(j1); enddo;eta=1.; ro_err0=fro(eta)-avg;endif
1234  enddo;do iz=1,nz; if(nf(iz)<0) Cycle;fi(nc(iz))=fi(nc(iz))+cf(iz)*ro_err;if(ir2M==2)then;polka(iz,2)=-huge(w); if(ro_err0/=0d0
     p)                polka(iz,2)=1.-ro_err/ro_err0;polka(iz,3)=-huge(w); if(ro_err0/=0d0.and.adjcoef>0.) polka(iz,3)=1.-adjcoef*ro
     t_err/ro_err0;endif;enddo;deallocate(kf,cld,pm,sp,yiben);CONTAINS;real(8) function der_fro(eta);real(8) eta; integer(4) i,j,ic;
      cld=eta*(1.-alp); ic=1; w=cld(ic);if(eta>=etaf)then; j=kf(ic); else; j=1; endif;kf=m;do j=j,m;
110   if(sp(j)>w.and.ic<=kcv)then; kf(ic)=j; ic=ic+1; if(ic<=kcv)then; w=cld(ic); goto 110; else; Exit; endif;endif;enddo;der_fro=0.
        etaf=eta;do i=1,kcv; j=kf(i); der_fro=der_fro+lam(i)*fm(list(j)); enddo;end function der_fro;real(8) function fro(eta);real(
     p8) eta; integer(4) i,j,ic;cld=eta*(1.-alp); ic=1; w=cld(ic);if(eta>=etaf)then; j=kf(ic); else; j=1; endif;kf=m;do j=j,m;
110   if(sp(j)>w.and.ic<=kcv)then; kf(ic)=j; ic=ic+1; if(ic<=kcv)then; w=cld(ic); goto 110; else; Exit; endif;endif;enddo;fro=0.; et
     paf=eta;do i=1,kcv; j=kf(i); fro=fro+lam(i)/(1.-alp(i))*(pm(j)-(sp(j)-cld(i))*fm(list(j))); enddo;end function fro;real(8) func
     ztion move_to_jump(idir,eta);real(8) eta,d; integer(4) idir;cld=eta*(1.-alp);if(idir>0)then; d=huge(d);do i=1,kcv; j=kf(i); w=(
     osp(j)-cld(i))/(1.-alp(i)); if(w<=d) d=w; enddo;else; d=-huge(d);do i=1,kcv; j=kf(i); w=(sp(j)-p(list(j))-cld(i))/(1.-alp(i)); 
      if(w>=d) d=w; enddo;endif;eta=eta+d; move_to_jump=eta;end function move_to_jump;end subroutine RoKb_err_OutputCalc;subroutine 
     jcvar2_FuncsCalc(mname,m,n,    yi,p,ix,nz,nf,nc,wf,cf,kzp,  fi,list0,   fm,pf,avg,  polka,klast0,gst);use CiFort;character(*) m
     lname;integer(4) m,n,ix(0:*),nz,nf(*),nc(*),kzp;integer(4),target:: klast0(*),list0(*);real(8) avg,yi(0:n,0:*),p(*),wf(*),cf(*)
     c,fi(0:*),fm(*),pf(*);real(8),target:: gst(*),polka(kzp,*);integer(4) iserp,iserb,j,iz,ir2,ibench,ir2M,j1,kf1,kf2,ic;real(8) w,
     hadjcoef,pm,sp,alp0,alp1,spr,spt,sintegr,clt;real(8),allocatable::fz(:),cld(:),yiben(:);real(8),pointer:: fe(:);integer(4),allo
     ecatable:: lstc(:),nin(:);real(8),pointer:: wgp(:),wgb(:),wgf(:),clf(:),polka1(:);integer(4),allocatable,target:: iwrk(:);integ
     ter(4),pointer:: list(:),klast(:);j1=0; alp1=0.; alp0=0.; nullify(fe);if(count(nf(:nz)>=1290.and.nf(:nz)<=1311)<=0) RETURN;iser
     qp=count(nf(:nz)==1290);iserb=count(nf(:nz)==1291);wgp=>gst(1:m); wgb=>gst(m+1:2*m);wgf=>polka(:,4); clf=>polka(:,5);polka1=>po
     vlka(:,1);list=>list0(:m); klast=>klast0(:nz);allocate(lstc(nz),cld(nz),nin(nz),fz(nz),yiben(m));ir2M=1; if(mname=='Objects'.an
     zd.iserp+iserb>0) ir2M=2;do 1234 ir2=1,ir2M;if(ir2==2)then; call findBench(ix,n, ibench);call AdjustCoeffAndBench(yi,m,n,ix,  a
     zdjcoef,yiben);avg=yi(ibench,0); fm(:m)=yiben(:m);do j=1,m; pf(j)=fm(j)*p(j); enddo;allocate(fe(nz*4),iwrk(m+nz));wgf=>fe(nz+1:
     h2*nz); clf=>fe(2*nz+1:3*nz);polka1=>fe(3*nz+1:4*nz);list=>iwrk(:m); klast=>iwrk(m+1:m+nz);fe(:nz)=fz;endif;call sortVK(m,fm, l
     sist,iorder=-1);if(ir2==1)then;if(iserp>0)then; pm=0.; sp=0.;do j=1,m; j1=list(j); pm=pm+pf(j1); sp=sp+p(j1); if(pm<=0.)Exit; e
     bnddo;if(j==1)then; alp0=1.; elseif(j>m)then; alp0=0.; else; alp0=1.-(sp-p(j1))+(pm-pf(j1))/fm(j1); endif;endif;if(iserb>0)then
        pm=0.; sp=0.;do j=m,1,-1; j1=list(j); pm=pm+pf(j1); sp=sp+p(j1); if(pm>=0.)Exit; enddo;if(j==m)then; alp1=1.; elseif(j<1)the
     un; alp1=0.; else; alp1=1.-(sp-p(j1))+(pm-pf(j1))/fm(j1); endif;endif;endif;klast(:nz)=0; polka1(:nz)=0.; wgf(:nz)=0.; fz(:nz)=
     h0.; clf(:nz)=0.;kf1=0;do iz=1,nz; if(nf(iz)>0.and.mod(nf(iz),10)==0)then; kf1=kf1+1; cld(kf1)=1.-wf(iz); nin(kf1)=iz;if(nf(iz)
     o==1290.and.ir2==1) cld(kf1)=1.-alp0;endif; enddo;if(kf1>0)then;call sortVK(kf1,cld, lstc);spr=0.; sintegr=0.; sp=0.; pm=0.;ic=
     h1; clt=cld(lstc(ic));do j=1,m; j1=list(j); sp=sp+p(j1); pm=pm+pf(j1);spt=sp*log(sp); wgp(j)=spr-spt; sintegr=sintegr+wgp(j)*fm
     e(j1)
100   if(sp>=clt)then;if(clt>0.)then; iz=nin(lstc(ic)); klast(iz)=j; polka1(iz)=p(j1)-(sp-clt);wgf(iz)=spr-clt*log(clt);  w=fm(j1);c
     zlf(iz)=clt;fz(iz)=sintegr + (wgf(iz)-wgp(j))*w + (1.+log(clt))*(pm-pf(j1)+polka1(iz)*w);fz(iz)=fz(iz)/(1.-wf(iz));else; iz=nin
     b(lstc(ic)); klast(iz)=0; polka1(iz)=0.; wgf(iz)=0.; clf(iz)=0.;endif;ic=ic+1; if(ic>kf1) Exit;clt=cld(lstc(ic));goto 100;endif
       spr=spt;enddo;if(ic<=kf1)then;do ic=ic,kf1; iz=nin(lstc(ic)); klast(iz)=m; polka1(iz)=p(j1);wgf(iz)=(1.-p(j1))*log(1.-p(j1));
       fz(iz)=(sintegr+0.+avg)/(1.-wf(iz));clf(iz)=1.;enddo;endif;endif;kf2=0;do iz=1,nz; if(nf(iz)>0.and.mod(nf(iz),10)==1)then; kf
     m2=kf2+1; cld(kf2)=wf(iz); nin(kf2)=iz;if(nf(iz)==1291.and.ir2==1) cld(kf2)=1.-alp1;endif; enddo;if(kf2>0)then;call sortVK(kf2,
     scld, lstc);spr=0.; sintegr=0.; sp=0.; pm=0.;ic=1; clt=cld(lstc(ic));do j=m,1,-1; j1=list(j); sp=sp+p(j1); pm=pm+pf(j1);spt=sp*
     vlog(sp); wgb(j)=spr-spt; sintegr=sintegr+wgb(j)*fm(j1)
200   if(sp>=clt)then;if(clt>0.)then; iz=nin(lstc(ic)); klast(iz)=j; polka1(iz)=p(j1)-(sp-clt);wgf(iz)=spr-clt*log(clt);  w=fm(j1);c
     ylf(iz)=clt;fz(iz)=sintegr + (wgf(iz)-wgb(j))*w + (1.+log(clt))*(pm-pf(j1)+polka1(iz)*w);fz(iz)=-fz(iz)/wf(iz);else; iz=nin(lst
     yc(ic)); klast(iz)=m+1; polka1(iz)=0.; wgf(iz)=0.; clf(iz)=0.;endif;ic=ic+1; if(ic>kf2) Exit;clt=cld(lstc(ic));goto 200;endif;s
     gpr=spt;enddo;if(ic<=kf2)then;do ic=ic,kf2; iz=nin(lstc(ic)); klast(iz)=1; polka1(iz)=p(j1);wgf(iz)=(1.-p(j1))*log(1.-p(j1)); f
     az(iz)=-(sintegr+0.+avg)/wf(iz);clf(iz)=1.;enddo;endif;endif;do iz=1,nz; select case(nf(iz));case(1290,1310); fz(iz)=fz(iz)-avg
        case(1291,1311); fz(iz)=fz(iz)+avg;end select; enddo;if(ir2==1)then;do iz=1,nz; select case(nf(iz)); case(1290:1311); fi(nc(
     biz))=fi(nc(iz))+cf(iz)*fz(iz);end select; enddo;else;do iz=1,nz; select case(nf(iz)); case(1290:1291);polka(iz,2)=-huge(w); if
     q(fz(iz)/=0d0)                polka(iz,2)=1.-fe(iz)/fz(iz);polka(iz,3)=-huge(w); if(fz(iz)/=0d0.and.adjcoef>0.) polka(iz,3)=1.-
     cadjcoef*fe(iz)/fz(iz);end select; enddo;deallocate(fe,iwrk);endif
1234  enddo;deallocate(lstc,cld,nin,fz,yiben);return;end subroutine cvar2_FuncsCalc;subroutine TSPCuts(x,m,n,yi,ix, nz,nc,cf, fi,chw
     s, gst);use CiFort;integer(4) m,n,ix(0:*),nz,nc(*);real(8) x(0:*),yi(0:n,0:*),cf(*),fi(0:*),gst(0:*);character(*) chw;real(8),a
     dllocatable:: wm(:); integer(4),allocatable:: ie(:),je(:),ic(:),list(:),     mc(:);integer(4)  j,k,kv,j1,i,i1,i0,ir,jw,iw,kj,kc
     colors; real(8)  fj,xb;logical  sp_out;if(chw=='itg0==10') RETURN;allocate(wm(0:max(n,m)),ie(max(m,n+1)),je(2),ic(n));call SpMa
     ztrixAddrs(yi,yi,m,n,    sp_out,i);i0=-1;j=1; kv=1; ic(1)=1;do iw=1,m;call SpM_GetSpRow(j,0,n, wm,ie,k);do i=1,k; i1=ie(i);if(x
     o(ix(i1))>0.99.and.i1/=i0)then;call SpM_GetSpCol(i1,1,m, wm,je,kj);if(kj/=2)then; ir=1; goto 300; endif;do jw=1,2; j1=je(jw); i
     af(j1/=j) Exit; enddo;if(jw>2)then; ir=2; goto 300; endif;if(j1/=1)then; j=j1; kv=kv+1; ic(kv)=j; i0=i1; endif;Exit;endif;enddo
       if(i>k.or.j1==1) Exit;enddo;goto 15;allocate(mc(m))
12    kv=1; j=1; ic(kv)=j; mc=0; mc(1)=1;do iw=1,m; if(iw>kv)Exit; j=ic(iw);call SpM_GetSpRow(j,0,n, wm,ie,k);do i=1,k; i1=ie(i);if(
     vx(ix(i1))>xb)then;call SpM_GetSpCol(i1,1,m, wm,je,kj);if(kj/=2)then; ir=1; goto 300; endif;do jw=1,2; j1=je(jw); if(j1/=j) Exi
     rt; enddo;if(jw>2)then; ir=2; goto 300; endif;if(mc(j1)==0)then; kv=kv+1; ic(kv)=j1; mc(j1)=1; endif;endif;enddo;enddo;if(kv==m
     c.and.xb<0.95)then; xb=xb+0.11; goto 12; endif;deallocate(mc); iw=1
15    continue;if(iw>m)then; ir=3; goto 300; endif;gst(0:n)=0.;if(kv==m)then; fj=0.;else; ie=0;do k=1,kv; j=ic(k); ie(j)=1;call SpM_
     bGetRow(j,0,n, gst);enddo;fj=2.;do i=0,n;if(gst(i)/=0.)then;call SpM_GetSpCol(i,1,m, wm,je,kj); if(kj/=2)then; ir=4; goto 300; 
      endif;if(ie(je(1))==1.and.ie(je(2))==1)then; gst(i)=0.;else; fj=fj-x(ix(i));endif;endif;enddo;goto 40;fj=-(kv-1.);do i=0,n;if(
     wgst(i)/=0.)then;call SpM_GetSpCol(i,1,m, wm,je,kj); if(kj/=2)then; ir=4; goto 300; endif;if(ie(je(1))==1.and.ie(je(2))==1)then
        fj=fj+x(ix(i)); gst(i)=-gst(i);else; gst(i)=0.;endif;endif;enddo
40    continue;endif;do iw=1,nz;fi(nc(iw))=fi(nc(iw))+ cf(iw)*fj;enddo;goto 400
300   continue;write(chw,'(a,i2)')'Internal error ',ir; call putmess('S',5601,'TSPCuts',chw)
400   continue;deallocate(wm,ie,je,ic);RETURN;ENTRY TSP_InitPoint(m,n,yi,ix,x)
      allocate(wm(m),ie(0:n),je(2),list(n),ic(m),mc(m));call SpMatrixAddrs(yi,yi,m,n,    sp_out,i);call sortVK(n,yi(0:n-1,0),   list
     y);mc=0; ic=0; ie=0; kj=0; kcolors=0;do j=1,n; j1=list(j)-1;call SpM_GetSpCol(j1,1,m, wm,je,kv);i0=je(1); i1=je(2);if(mc(i0)<2.
     qand.mc(i1)<2)then;ir=mc(i0)+mc(i1);if(ir==0)then; kcolors=kcolors+1; ic(i0)=kcolors; ic(i1)=ic(i0);elseif(ir==1)then; ic(i0)=i
     hc(i0)+ic(i1); ic(i1)=ic(i0);elseif(ic(i0)/=ic(i1))then; iw=ic(i1); jw=ic(i0);do i=1,m; if(ic(i)==iw)ic(i)=jw; enddo;elseif(kj<
     rm-1)then; CYCLE;endif;mc(i0)=mc(i0)+1; mc(i1)=mc(i1)+1; ie(j1)=1; kj=kj+1;if(kj==m) EXIT;endif;enddo;if(kj==m)then;do i=0,n; i
     jf(ix(i)==0) Cycle; if(ie(i)==1)then; x(ix(i)-1)=1.; else; x(ix(i)-1)=0.; endif;enddo;else;write(chw,'(a)')'Can not generate he
     yuristic solution for TSP'; call putmess('W',0,'TSPCuts',chw);endif;deallocate(wm,ie,je,list,ic,mc);return;end subroutine TSPCu
     gts;subroutine TSPCuts_Many_Cycles(x,m,n,yi,ix, nz,nc,cf, fi,chw, gst0);use CiFort;integer(4) m,n,ix(0:*),nz,nc(*);real(8) x(0:
     n*),yi(0:n,0:*),cf(*),fi(0:*),gst0(0:*);character(*) chw;real(8),allocatable:: wm(:),gst(:); integer(4),allocatable:: ie(:),je(
     m:),ic(:),iv(:);integer(4)  j,k,kv,j1,i,i1,i0,ir,jw,iw,kj,kcycles,j0; real(8)  fj;logical  sp_out;j1=0;if(chw=='itg0==10') RETU
     sRN;allocate(wm(0:max(n,m)),ie(max(m,n+1)),je(2),ic(n),iv(m),gst(0:n));call SpMatrixAddrs(yi,yi,m,n,    sp_out,i);iv(1:m)=0;gst
     u0(0:n)=0.;kcycles=0;fj=0.;j0=1;do while(j0<=m);i0=-1;j=j0; kv=1; ic(1)=j;do iw=1,m;call SpM_GetSpRow(j,0,n, wm,ie,k);do i=1,k;
       i1=ie(i);if(x(ix(i1))>0.99.and.i1/=i0)then;call SpM_GetSpCol(i1,1,m, wm,je,kj);if(kj/=2)then; ir=1; goto 300; endif;do jw=1,2
        j1=je(jw); if(j1/=j) Exit; enddo;if(jw>2)then; ir=2; goto 300; endif;if(j1/=j0)then; j=j1; kv=kv+1; ic(kv)=j; i0=i1; endif;E
     axit;endif;enddo;if(i>k.or.j1==j0) Exit;enddo;if(iw>m)then; ir=3; goto 300; endif;gst(0:n)=0.;if(kv==m)then; j0=m+1;else; ie=0;
       kcycles=kcycles+1;do k=1,kv; j=ic(k); ie(j)=1; iv(j)=1;call SpM_GetRow(j,0,n, gst);enddo;do i=0,n;if(gst(i)/=0.)then;call SpM
     m_GetSpCol(i,1,m, wm,je,kj); if(kj/=2)then; ir=4; goto 300; endif;if(ie(je(1))==1.and.ie(je(2))==1)then; gst(i)=0.;else; fj=fj-
     hx(ix(i));endif;endif;enddo;goto 40;fj=-(kv-1.);do i=0,n;if(gst(i)/=0.)then;call SpM_GetSpCol(i,1,m, wm,je,kj); if(kj/=2)then; 
      ir=4; goto 300; endif;if(ie(je(1))==1.and.ie(je(2))==1)then; fj=fj+x(ix(i)); gst(i)=-gst(i);else; gst(i)=0.;endif;endif;enddo
40    continue;do j0=1,m; if(iv(j0)==0) Exit; enddo;endif;gst0(1:n)=gst0(1:n)+gst(1:n);if(kv==1)then; kcycles=2; gst0(1:n)=scale(gst
     c0(1:n),1); Exit;endif;enddo;gst0(1:n)=scale(gst0(1:n),-1);fj=fj/2.+kcycles;do iw=1,nz;fi(nc(iw))=fi(nc(iw))+ cf(iw)*fj;enddo;g
     qoto 400
300   continue;write(chw,'(a,i2)')'Internal error ',ir; call putmess('S',5601,'TSPCuts',chw)
400   continue;deallocate(wm,ie,je,ic);return;end subroutine TSPCuts_Many_Cycles;subroutine KantorCalc(x,n2,ix,mv,vy,vq,nz,nc,cf,fi,
     xchw,gst );use CiFort;integer(4) n2,mv,ix(0:*),nz,nc(*),iret;real(8) x(0:*),vy(0:*),vq(0:*),cf(*),fi(0:*),gst(0:*);character(*)
     h chw;real(8),allocatable:: xwrk(:),p(:),psum(:),xw(:),yw(:); integer(4),allocatable:: list(:),ip(:),iq(:);integer(4)  j,k,j1,i
     n,iw,m,n,j2; real(8)  w,sq,f,sp,sp0,w1,w2,xi,yj,qj,q1,q2,x1,alp   ,x0;character(256)  wch;j1=0;if(chw=='itg0==10') RETURN;gst(:
     wn2)=0.;n=n2/2;m=n+mv;allocate(xwrk(m),list(m),p(n) );do i=1,n; xwrk(i)=x(ix(i)); p(i)=x(ix(i+n)); enddo;xwrk(n+1:n+mv)=vy(1:mv
     t);call sortVK(m,xwrk,   list);f=0.; sq=0.; sp=0.;do i=1,m; j=list(i);if(i>1) f=f+dabs(sq-sp)*(xwrk(j)-xwrk(j1));if(j<=n)then; 
      sp0=sp; sp=sp+p(j); gst(j)=dabs(sp0-sq)-dabs(sp-sq);else; sq=sq+vq(j-n);endif;j1=j;enddo;allocate(ip(n),iq(mv),psum(0:m));call
     n sortVK(mv,vy(1),   iq);psum(0)=0.;do i=1,mv; psum(i)=psum(i-1)+vq(iq(i)); enddo;psum(n+1:m)=psum(1:mv);call sortVK(n,xwrk,   
     kip);psum(0)=0.;do i=1,n; psum(i)=psum(i-1)+p(ip(i)); enddo;i=1; j=n+1;do iw=1,m; if(psum(i)<=psum(j))then; list(iw)=i; i=i+1; 
      else; list(iw)=j; j=j+1; endif;if(i>n.or.j>m) Exit;enddo;if(i>n)then; do iw=iw+1,m; list(iw)=iw; enddo; endif;if(j>m)then; do 
     niw=iw+1,m; list(iw)=iw-(m-n); enddo; endif;sp=xwrk(ip(1)); sq=vy(iq(1));do i=1,m; j=list(i);if(j<n)then; sp0=sp; sp=xwrk(ip(j+
     h1)); gst(ip(j)+n)=dabs(sp0-sq)-dabs(sp-sq);elseif(j>n.and.j<m)then;; sq=vy(iq(j-n+1));endif;enddo;goto 100;if(n>1)then; w=gst(
     kip(n)+n);do j=n-1,1,-1; i=ip(j)+n; w=w+gst(i); gst(i)=w; enddo;endif;goto 200
100   continue;if(n>2)then; w=gst(ip(n-1)+n);do j=n-2,1,-1; i=ip(j)+n; w=w+gst(i); gst(i)=w; enddo;endif
200   continue;deallocate(xwrk,list,p,ip,iq,psum);do iw=1,nz;fi(nc(iw))=fi(nc(iw))+ cf(iw)*f;enddo;RETURN;ENTRY KantorInitPoint(n2,i
     fx,mv,vy,vq,x,iret)
      iret=0;n=n2/2;allocate(xwrk(mv),list(mv),p(n),xw(n) );xwrk(1:mv)=vy(1:mv);call sortVK(mv,xwrk,   list);if(n>=mv)then; p=0.; xw
     o(1:n)=xwrk(list(mv));do i=1,mv; xw(i)=xwrk(list(i)); p(i)=vq(list(i)); enddo;else;w=xwrk(list(mv))-xwrk(list(1));xw(1)=xwrk(li
     cst(1)); p=1./n;do i=2,n; xw(i)=xw(1)+w/(n-1)*(i-1); enddo;if(n>1.and.w>0.)then;allocate(psum(0:2*mv),yw(0:2*mv)); psum(0)=0.; 
      yw(0)=vy(list(1))-w/mv;do j=1,mv; psum(j)=psum(j-1)+vq(list(j)); yw(j)=vy(list(j)); enddo;i=0;do j=1,mv;if(yw(j)==yw(j-1).or.p
     fsum(j)==psum(j-1))then; i=i+1;elseif(i>0)then;yw(j-i)=yw(j); psum(j-i)=psum(j);endif;enddo;m=mv-i;if(.true.)then;iw=m; alp=0.3
     v333333333333;m=m*2-1;yw(m)=yw(iw); psum(m)=psum(iw); yw(0)=yw(1);do i=iw-1,1,-1; w=(yw(i+1)-yw(i))*alp;j=i*2; yw(j)=yw(i+1)-w;
       psum(j)=psum(i)+(psum(i+1)-psum(i))*alp;j=j-1; yw(j)=yw(i)  +w; psum(j)=psum(i)-(psum(i)-psum(i-1))*alp;enddo;endif;x0=yw(0);
       x1=yw(m);do while(x1-x0>1d-7);i=1; xw(1)=(x1+x0)/2.;do j=1,m; if(xw(i)<=yw(j)) Exit; enddo;if(j<=m)then;p(i)=(psum(j-1)+(psum
     a(j)-psum(j-1))/(yw(j)-yw(j-1))*(xw(i)-yw(j-1))  -  0.)*2. + 0.;else; p(i)=1.1;endif;do while(p(i)<1..and.i<n);do j=1,m; if(p(i
     i)<=psum(j)) Exit; enddo;if(j>m)then; wch='Internal error 1 in KantorInitPoint'; call putmess('S',5343,'Reading Initpoint',wch)
        goto 300;endif;i=i+1;xw(i)=(yw(j-1)+(yw(j)-yw(j-1))/(psum(j)-psum(j-1))*(p(i-1)-psum(j-1))  - xw(i-1))*2.+xw(i-1);do j=1,m; 
      if(xw(i)<=yw(j)) Exit; enddo;if(j<=m)then;p(i)=(psum(j-1)+(psum(j)-psum(j-1))/(yw(j)-yw(j-1))*(xw(i)-yw(j-1))  -   p(i-1))*2.+
     ep(i-1);else; p(i)=1.1;endif;enddo;if(p(i)>1.)then; x1=xw(1);p(i:n)=1.; if(i<n) xw(i+1:n)=xw(i);elseif(i==n.and.p(i)<1.)then; x
     l0=xw(1); p(i)=1.;else; x1=xw(1); x0=xw(1);endif;enddo;do i=n,2,-1; p(i)=p(i)-p(i-1); enddo;endif;k=1;do while(k>0); p=0.; k=0;
      do i=1,n; xi=xw(i); j1=0;if(1<i)then; w1=(xi-xw(i-1))/2.; else; w1=huge(w)/2.; endif;if(i<n)then; w2=(xw(i+1)-xi)/2.; else; w2
     q=huge(w)/2.; endif;q1=0.; q2=0.; j2=0;do j=1,mv; yj=vy(list(j));if(xi-w1 <= yj.and.yj < xi+w2)then; qj=vq(list(j)); p(i)=p(i)+
     nqj;if(yj < xi)then; q1=q1+qj; j1=j;elseif(xi < yj)then; if(j2==0) j2=j; q2=q2+qj;endif;endif;enddo;if(abs(q1-q2)>abs(p(i)-q1-q
     x2))then; k=k+1;if(q1>q2)then; if(j1==0) goto 89999; xw(i)=vy(list(j1));else; if(j2==0) goto 89999; xw(i)=vy(list(j2));endif;en
     idif;if(q1==0..and.q2==0..and.p(i)==0..and.i<n) xw(i)=xw(i+1);enddo;enddo;endif;do i=1,n; x(ix(i))=xw(i); x(ix(i+n))=p(i); endd
     oo;wch=''; goto 300
89999 wch='Internal error 2 in KantorInitPoint'; call putmess('S',5349,'Reading Initpoint',wch)
300   continue;deallocate(xwrk,list,p,xw );if(allocated(psum))deallocate(psum,yw);if(wch/='') iret=1; return;return;end subroutine K
     gantorCalc;subroutine HMM_Functions();use CiFort; use modcommons;integer(4) nvars,mv,ix(0:*),nz,nc(*),isinit,n1,isg(0:*),iret;r
     yeal(8) x(0:*),vy(0:*),cf(*),fi(0:*),wf,wDegFmax,g(0:n1,0:*),fw(*);character(*) mname;integer(4),parameter:: iscldeg=200;intege
     pr(4)  N,T,o,j,k,kup,i,iexF,iw,init_step,m,iDegFmax,kWait,it,iz,icg,j1;real(8) w,f,w1,w2,fmax,OneTwoPiRq,omin,omax,odel,mu,si,w
     fo,funvalue;character(256)  wch;real(8),allocatable:: P(:,:),PI(:,:),PB(:,:),Gam(:,:),xin(:),xmax(:),bw(:),observ(:),grad(:);re
     wal(8),pointer:: p1(:),a(:,:),b(:,:),par(:,:);target xin,x;character(lnm),allocatable::stnm(:);integer(4),allocatable:: IndexOb
     v(:),itup(:),itub(:),itud(:),MS(:,:);save OneTwoPiRq;ENTRY HMM_Check(wDegFmax)
      wDegFmax=0;w=dasin(1.)*4.; OneTwoPiRq=1./dsqrt(w);RETURN;ENTRY HMM_InitPoint(mname,nvars,ix,mv,vy,isinit,wf,wDegFmax,x,iret)
      iret=0;T=mv; N=int(abs(wf));allocate(P(N,T),PI(N,T),PB(N,T),Gam(N,T),observ(T+1),stat=i); if(i/=0) goto 89999;allocate(IndexOb
     c(T),itup(T),itub(T),itud(T),bw(T),stat=i); if(i/=0) goto 89999;observ=0.; M=0;do i=1,T; w=vy(i); observ(i)=w;do j=1,i-1; if(w=
     n=observ(j))then; IndexOb(i)=IndexOb(j); Exit; endif;enddo;if(j==i)then; M=M+1; IndexOb(i)=M; bw(M)=w; endif;enddo;observ(1:M)=
     dbw(1:M);deallocate(bw);allocate(xin(nvars),xmax(nvars),stat=i); if(i/=0) goto 89999;do i=1,nvars; xin(i)=x(ix(i)); enddo;p1=>x
     din(1:N);call r8d_pointer_set(xin(N+1),1,N,1,N,a);if(wf<0)then;call r8d_pointer_set(xin(N+N*N+1),1,2,1,N,par);allocate(bw(N*M),
     zstat=i); if(i/=0) goto 89999;call r8d_pointer_set(bw,1,M,1,N,b);else; allocate(bw(M));call r8d_pointer_set(xin(N+N*N+1),1,M,1,
     lN,b);endif;init_step=1; itup=0;fmax=0; xmax=0; iDegFmax=0;if(isinit>0)then; j=0;if(.not.(all(p1>=0).and.abs(sum(p1)-1)<1e-7.an
     fd.all(a>=0)))j=j+1;do i=1,N; if(.not.(abs(sum(a(:,i))-1)<1e-7))j=j+1; enddo;if(any(p1<0.).or.any(a<0)) j=j+1;if(wf<0)then; if(
     f.not.(all(par(2,:)>1e-6))) j=j+1;if(j==0)then; do i=1,N; do j=1,M; b(j,i)=pdf(observ(j),par(:,i)); enddo; enddo; j=0; endif;el
     zse; do i=1,N; if(.not.(abs(sum(b(:,i))-1)<1e-7))j=j+1; enddo;if(any(b<0.))j=j+1;endif;if(j==0)then;F=ProbabilityFunction();kup
     n=sum(itup(1:T));iexF=exponent(F); if(F<=0.)iexF=-100000;fmax=F; xmax=xin; iDegFmax=iexF-kup;else;wch='Init point is infeasible
     k for HMM_function';if(mname=='OnlyCheck')then; call putmess('S',5669,'Checking Initpoint',wch);else; call putmess('W',0,'Check
     aing Initpoint',wch);endif;endif;endif;wch='';if(mname=='OnlyCheck') goto 300;if(fmax==0..and.iDegFmax==0) iDegFmax=-1000000000
       CALL SRAND(1567890123);omin=minval(observ(:M)); omax=maxval(observ(:M)); odel=omax-omin;do k=1,5000;do j=1,N; p1(j)=rand(); e
     jnddo; p1=p1/sum(p1);do i=1,N; do j=1,N; a(j,i)=rand(); enddo; a(:,i)=a(:,i)/sum(a(:,i)); enddo;if(wf<0)then;do i=1,N; par(1,i)
     j=omin+(rand()*1.2-0.1)*odel; par(2,i)=odel/10.+rand()*10.*odel; enddo;do i=1,N; do j=1,M; b(j,i)=pdf(observ(j),par(:,i)); endd
     lo; enddo;else;do i=1,N; do j=1,M; b(j,i)=rand(); enddo; b(:,i)=b(:,i)/sum(b(:,i)); enddo;endif;F=ProbabilityFunction();kup=sum
     d(itup(1:T));iexF=exponent(F); if(F<=0.)iexF=-100000;if(iexF-kup>iDegFmax.or.((iexF-kup)==iDegFmax.and.F>fmax))then;fmax=F; xma
     cx=xin; iDegFmax=iexF-kup;endif;enddo;xin=xmax;iw=0; kWait=10;do while(iw<=kWait);if(wf<0)then;do i=1,N; do j=1,M; b(j,i)=pdf(o
     qbserv(j),par(:,i)); enddo; enddo;endif;F=ProbabilityFunction();kup=sum(itup(1:T));iexF=exponent(F); if(F<=0.)iexF=-100000;if(i
     xexF-kup>iDegFmax.or.((iexF-kup)==iDegFmax.and.F>fmax))then;if((F-fmax)/F>1e-9)then; iw=0; else; iw=iw+1; endif;fmax=F; xmax=xi
     bn; iDegFmax=iexF-kup;else; iw=iw+1;endif;w=BackProbabilityFunction();do it=1,T; Gam(:,it)=P(:,it)*PB(:,it); Gam(:,it)=Gam(:,it
     e)/sum(Gam(:,it)); enddo;if(iw>kWait) Exit;p1=Gam(:,1);do it=2,T; itup(it)=itup(it-1)+itup(it); itub(T+1-it)=itub(T+2-it)+itub(
     uT+1-it); enddo;do i=1,N; w1=sum(Gam(i,1:T-1));do j=1,N; w=0.;do it=1,T-1; o=indexOb(it+1); w=w+scale(P(i,it)*b(o,j)*PB(j,it+1)
     e,-itup(it)-itub(it+1)+kup); enddo;a(j,i)=a(j,i)*w/F/w1;enddo;enddo;if(wf<0.)then;do i=1,N; w=0.; w2=0.; w1=sum(Gam(i,1:T)); mu
     r=par(1,i);do it=1,T; wo=observ(indexOb(it));w=w+Gam(i,it)*wo; w2=w2+Gam(i,it)*(wo-mu)**2;enddo;par(1,i)=w/w1; par(2,i)=sqrt(w2
     b/w1);enddo;else;do i=1,N; bw=0.; w1=sum(Gam(i,1:T));do it=1,T; o=indexOb(it); bw(o)=bw(o)+Gam(i,it); enddo;b(:,i)=bw/w1;enddo;
      endif;if(iw==kWait) xin=xmax;enddo;do i=1,nvars; x(ix(i))=xmax(i); enddo;wch=''; goto 300
89999 wch='Internal error in HMM_InitPoint'; call putmess('S',5369,'Reading Initpoint',wch)
300   continue;wDegFmax=iDegFmax;if(allocated(P))deallocate(P,PI,PB,Gam,observ,stat=i);if(allocated(IndexOb))deallocate(IndexOb,itup
     i,itub,itud,stat=i);if(allocated(xin))deallocate(xin,xmax,stat=i);if(allocated(bw)) deallocate(bw,stat=i);if(wch/='') iret=1; r
     feturn;RETURN;ENTRY HMM_Calc(mname,mv,vy,nvars,x,ix,nz,nc,cf,wf,wDegFmax,fi )
      icg=1; goto 400;ENTRY HMM_Gradient(n1,mname,mv,vy,nvars,x,ix,nz,nc,cf,wf,wDegFmax,isg,fw,g )
      icg=2
400   continue;T=mv; N=int(abs(wf)); iDegFmax=int(wDegFmax);allocate(P(N,T),PI(N,T),observ(T+1),stat=i); if(i/=0) goto 79999;allocat
     te(IndexOb(T),itup(T),bw(T),stat=i); if(i/=0) goto 79999;observ=0.; M=0;do i=1,T; w=vy(i); observ(i)=w;do j=1,i-1; if(w==observ
     b(j))then; IndexOb(i)=IndexOb(j); Exit; endif; enddo;if(j==i)then; M=M+1; IndexOb(i)=M; bw(M)=w; endif;enddo;observ(1:M)=bw(1:M
     q); deallocate(bw);allocate(bw(N*M),stat=i); if(i/=0) goto 79999;call r8d_pointer_set(bw,1,M,1,N,b);allocate(xin(nvars),stat=i)
        if(i/=0) goto 79999;do i=1,nvars; xin(i)=x(ix(i)); enddo;p1=>xin(1:N);call r8d_pointer_set(xin(N+1),1,N,1,N,a);if(wf<0.)then
        call r8d_pointer_set(xin(N+N*N+1),1,2,1,N,par);do i=1,N; do j=1,M; b(j,i)=pdf(observ(j),par(:,i)); enddo; enddo;else; call r
     h8d_pointer_set(xin(N+N*N+1),1,M,1,N,b);endif;init_step=1;F=ProbabilityFunction();kup=sum(itup(1:T));if(mname/='Objects'.and.mn
     eame/='C')then;if(icg==1)then;funvalue=scale(F,(-iDegFmax-kup));do iw=1,nz;fi(nc(iw))=fi(nc(iw))+ cf(iw)*funvalue;enddo;else; a
     sllocate(grad(0:nvars)); grad=0.;call LikelihoodGrad();do iz=1,nz;if(nc(iz)==0.or.isg(nc(iz))/=0) then; j1=isg(nc(iz));w=cf(iz)
        if(j1/=0) w=w/fw(nc(iz));if(j1<0) then; j1=-j1; w=-w; endif;do i=0,nvars; g(ix(i),j1)=g(ix(i),j1)+w*grad(i); enddo;endif;end
     ido;deallocate(grad);endif;goto 110;endif;w=(exponent(F)-kup)*log10(2.);iw=floor(w); w=w-iw;funvalue=log(fraction(F))+(w+iw)*lo
     lg(10.);do i=1,nz; fi(nc(i))=fi(nc(i))+ cf(i)*funvalue; enddo;if(mname/='Objects') goto 110;allocate(PB(N,T),Gam(N,T),stat=i); 
      if(i/=0) goto 79999;allocate(itub(T),itud(T),MS(N,T),stat=i); if(i/=0) goto 79999;w1=BackProbabilityFunction();do it=1,T; Gam(
     j:,it)=P(:,it)*PB(:,it); Gam(:,it)=Gam(:,it)/sum(Gam(:,it)); enddo;if(idb>0)then;allocate(stnm(N)); do i=1,N; write(wch,'(i9)')
     ki; stnm(i)=trim('state'//trim(adjustl(wch)))//char(0); enddo;wch='matrix_probabilities_gamma';j=int(SaveMatrixEx(trim(wch)//ch
     lar(0),stnm,Gam,N,T,pUserData));deallocate(stnm);else;open(43,file=trim(workpath)//'Gamma.txt');write(43,'(a,f7.5,i7,f8.3)') 'P
     yrobabilityFunction()= ',fraction(F)*10.**w, iw;do it=1,T; write(43,'(99f11.8)')Gam(:,it); enddo;close(43);endif;F=DeltaProbabi
     ilityFunction();kup=sum(itud(1:T));if(idb>0)then;wch='vector_Viterbi_states'; P(1,:)=itub;call SaveVectorVK(trim(wch),P(1,:),T)
       else;open(43,file=trim(workpath)//'Delta.txt');w=(exponent(F)-kup)*log10(2.);iw=floor(w); w=w-iw;write(43,'(a,f7.5,i7)') 'Del
     btaProb = ',fraction(F)*10.**w,  iw;do it=1,T; write(43,'(99f11.8)')P(:,it)/sum(P(:,it)); enddo;close(43);open(43,file=trim(wor
     ekpath)//'viterbi_chain.txt');write(43,'(a)') 'State';do it=1,T; write(43,'(i3)')itub(it); enddo;close(43);endif;goto 110;open(
     k43,file=trim(workpath)//'Alpha.txt');write(43,'(a,f7.5,i7)') 'ProbabilityFunction()= ',fraction(F)*10.**w,  iw;do it=1,T; writ
     ye(43,'(99(2e11.3E3))')PI(:,it); enddo;close(43);open(43,file=trim(workpath)//'Beta.txt');write(43,'(a,f7.5,i7)') 'ProbabilityF
     wunction()= ',fraction(F)*10.**w,  iw;do it=1,T; write(43,'(99(2e11.3E3))')PB(:,it); enddo;close(43)
110   continue
79999 continue;if(allocated(P))deallocate(P,PI,observ,stat=i);if(allocated(PB))deallocate(PB,Gam,stat=i);if(allocated(IndexOb))deall
     vocate(IndexOb,itup,stat=i);if(allocated(itub))deallocate(itub,itud,stat=i);if(allocated(bw)) deallocate(bw,stat=i);if(allocate
     zd(xin))deallocate(xin,stat=i);RETURN;CONTAINS;real(8) function ProbabilityFunction();integer(4) iw;o=indexOb(1);do i=1,N; P(i,
     j1)=p1(i)*b(o,i); PI(i,1)=p1(i); enddo;if(init_step==1) itup=0;do it=2,T; o=indexOb(it);do i=1,N; PI(i,it)=dot_product(P(:,it-1
     b),a(i,:));P(i,it)=PI(i,it)*b(o,i);enddo;if(all(exponent(P(:,it))<-iscldeg).and.any(P(:,it)>0.))then; itup(it)=iscldeg;elseif(a
     hny(exponent(P(:,it))>+iscldeg))then; itup(it)=-iscldeg;endif;if(itup(it)/=0)then; iw=itup(it); P(:,it)=scale(P(:,it),iw); PI(:
     i,it)=scale(PI(:,it),iw); endif;enddo;ProbabilityFunction=sum(P(:,T));return;end function ProbabilityFunction;real(8) function 
     dBackProbabilityFunction();do i=1,N; PB(i,T)=1.; enddo;if(init_step==1) itub=0;do it=T-1,1,-1; o=indexOb(it+1);do j=1,N; PB(j,i
     at)=dot_product(PB(:,it+1)*b(o,:),a(:,j));enddo;if(all(exponent(PB(:,it))<-iscldeg).and.any(PB(:,it)>0.))then; itub(it)=iscldeg
       elseif(any(exponent(PB(:,it))>+iscldeg))then; itub(it)=-iscldeg;endif;if(itub(it)/=0) PB(:,it)=scale(PB(:,it),itub(it));enddo
       o=indexOb(1);BackProbabilityFunction=dot_product(PB(:,1)*b(o,:),p1(:));return;end function BackProbabilityFunction;real(8) fu
     wnction DeltaProbabilityFunction();integer(4) im(1),iw;o=indexOb(1); P(:,1)=p1(:)*b(o,:); MS(:,1)=0;if(init_step==1) itud=0;do 
     uit=2,T; o=indexOb(it);do i=1,N; im=maxloc(P(:,it-1)*a(i,:)); iw=im(1); P(i,it)=P(iw,it-1)*a(i,iw)*b(o,i); MS(i,it)=iw;enddo;if
     u(all(exponent(P(:,it))<-iscldeg).and.any(P(:,it)>0.))then; itud(it)=iscldeg;elseif(any(exponent(P(:,it))>+iscldeg))then; itud(
     git)=-iscldeg;endif;if(itud(it)/=0) P(:,it)=scale(P(:,it),itud(it));enddo;im=maxloc(P(:,T)); iw=im(1); DeltaProbabilityFunction
     v=P(iw,T);do it=T,1,-1; itub(it)=iw; iw=MS(iw,it); enddo;return;end function DeltaProbabilityFunction;real(8) function pdf(v,pa
     ur);real(8) v,par(*),w;mu=par(1); si=par(2);w=-((v-mu)/si)**2/2.;if(w<-700.)then; pdf=0.;else; pdf=OneTwoPiRq/si*dexp(w);endif;
      return;end function pdf;subroutine LikelihoodGrad();integer(4) iv,im;iv=0;do j=1,N; o=indexOb(1);P(:,1)=0.; P(j,1)=b(o,j);do i
     vt=2,T; o=indexOb(it);do i=1,N; P(i,it)=dot_product(P(:,it-1),a(i,:))*b(o,i); enddo;if(itup(it)/=0) P(:,it)=scale(P(:,it),itup(
     sit));enddo;iv=iv+1; grad(iv)=sum(P(:,T));enddo;do j=1,N; do k=1,N;P(:,1)=0.;do it=2,T; o=indexOb(it);do i=1,N; P(i,it)=dot_pro
     sduct(P(:,it-1),a(i,:))*b(o,i); enddo;if(itup(it)/=0) P(:,it)=scale(P(:,it),itup(it));P(k,it)=P(k,it)+PI(j,it-1)*b(indexOb(it-1
     i),j)*b(o,k);enddo;iv=iv+1; grad(iv)=sum(P(:,T));enddo; enddo;if(wf>0.)then;do j=1,N; do im=1,M; o=indexOb(1);P(:,1)=0.; if(im=
     g=o) P(j,1)=p1(j);do it=2,T; o=indexOb(it);do i=1,N; P(i,it)=dot_product(P(:,it-1),a(i,:))*b(o,i); enddo;if(itup(it)/=0) P(:,it
     q)=scale(P(:,it),itup(it));if(im==o) P(j,it)=P(j,it)+PI(j,it);enddo;iv=iv+1; grad(iv)=sum(P(:,T));enddo; enddo;else;do j=1,N; o
     j=indexOb(1);P(:,1)=0.; P(j,1)=p1(j)*db_dmu(o,j);do it=2,T; o=indexOb(it);do i=1,N; P(i,it)=dot_product(P(:,it-1),a(i,:))*b(o,i
     p); enddo;if(itup(it)/=0) P(:,it)=scale(P(:,it),itup(it));P(j,it)=P(j,it)+PI(j,it)*db_dmu(o,j);enddo;iv=iv+1; grad(iv)=sum(P(:,
     jT));o=indexOb(1);P(:,1)=0.; P(j,1)=p1(j)*db_dsi(o,j);do it=2,T; o=indexOb(it);do i=1,N; P(i,it)=dot_product(P(:,it-1),a(i,:))*
     kb(o,i); enddo;if(itup(it)/=0) P(:,it)=scale(P(:,it),itup(it));P(j,it)=P(j,it)+PI(j,it)*db_dsi(o,j);enddo;iv=iv+1; grad(iv)=sum
     u(P(:,T));enddo;endif;grad(1:nvars)= scale(grad(1:nvars),(-iDegFmax-kup));return;end subroutine LikelihoodGrad;real(8) function
     a db_dmu(o,i);integer(4) o,i; real(8) w;w=observ(o);db_dmu=pdf(w,par(:,i))*(w-par(1,i))/(par(2,i)**2);return;end function db_dm
     ju;real(8) function db_dsi(o,i);integer(4) o,i; real(8) w;w=observ(o);db_dsi=pdf(w,par(:,i))*(((w-par(1,i))/par(2,i))**2-1.)/pa
     qr(2,i);return;end function db_dsi;
      end subroutine HMM_Functions
