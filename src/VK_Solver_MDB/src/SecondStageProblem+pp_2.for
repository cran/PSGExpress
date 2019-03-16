      subroutine SecondStageProblem();use ModCommons; use CiFort;integer(4) kmtrn,nfmatr,nmatr(*),mnb(*),nnb1,nnb3,nnb4,ix(*),nfix(*
     l),rn1,ix1(0:*),m4,jmax,jmin;real(8) p(*),yi1(0:nnb1,0:*),yi3(0:nnb3,0:*),yi4(0:nnb4,0:*),avg,fm(*),pf(*),x(0:*);character(lnm)
     l mname(*),rxname(0:*),xname(0:*);integer(4), allocatable::jss13(:),jss23(:),jss43(:),iss43(:),iwrk(:,:);logical lerr, sp_out;i
     lnteger(4) nd,IFAIL;integer(4) mx, mxmem;integer(4),pointer:: relcode(:),mark(:), kac(:), id(:), marks(:),markp(:);real(8),poin
     lter :: xd(:),rhs(:),rng(:),dual(:);real(8),pointer::dparr(:);  integer(4),pointer:: intarr(:);real(8),pointer::realall(:); int
     leger(4),pointer:: iintall(:);integer(plen) prall,piall;real(8) dpwork(*); integer(4) intwork(*);pointer (pwrl,dpwork); pointer
     l (piwrl,intwork);integer(4),parameter:: mapsize=30;integer(4) mapi(mapsize);integer(4) mapdp(mapsize);integer(1) iun;real(8),a
     lllocatable,target::ylb(:),dylb(:),dclb(:),xw(:);real(8),pointer::clb(:),dvl(:),dvu(:);real(8),pointer::vd(:);real(8),allocatab
     lle:: as(:),wrk(:), ays(:),zk(:);real(8),pointer:: ax(:,:),axs(:),vds(:),vdp(:),am(:,:),ay(:,:);integer(4),allocatable:: is(:),
     ljs(:),fjy(:),jy(:),jw(:);integer(4),pointer:: jx(:), fjx(:), fej(:), iel(:), nel(:);character(lrow) chw,wch; logical lnot_Null
     l,lscale;real(8) w,xhuge,dvuh,dvuh0,dl,cx,f,fmi,fma,timelimit,sinf,ws,wp,xdamax;integer(4) i,i1,i2,i3,i4,iw,j,j1,k,el,maxndx,nx
     l,ny,kscen,mt1,mt3,mt4,mm3,mm4,nxy,ml,nlb,nub,nsb,kel3,mxmax,
     +ionlysb,isb,ilb,iub,idy,igetmark,iscen,kex,key,kdc,kdv,mk,kel,kel0,ipfree,memfree,memwork,iam,ibs,ibst,ialgtyp,imarks,
     +i4bm,ln,    kdv0,kdc0,iret;real(8),external:: vectdotmatr_ish;integer(4),save::lnmax;save rhs,xd,rng,markp,marks,ay,vdp;Entry 
     lPrepSecondStage(kmtrn,nfmatr,mname,nmatr,mnb,nfix,ix,
     +nnb1,nnb3,nnb4,  yi3,yi4,
     +xname,   rn1,rxname);mk=0; kel0=0; kdv0=0; nsb=0; kscen=0; piwrl=0; dl=0.; kdc0=0; nx=0; ml=0;nullify(relcode,mark,kac,id,mark
     ls,markp,xd,rhs,rng,dual,dparr,intarr,clb,dvl,dvu,ax,axs,vd,vds,vdp,jx,fjx,fej,iel,nel,am,ay);ifail=0; maxndx=0; ln=0; lnmax=0;
      xhuge=1d36;if(lf20)then; iun=20; else; iun=-2; endif;lerr=.false.;do i=1,kmtrn; mt1=nmatr(nfmatr-1+i);select case(i);case(1); 
      if(mname(mt1)(:13)== 'matrix_xfirst')then; nx=nnb1; else; lerr=.true.; endif;case(2); if(mname(mt1)(:17)== 'matrix_subproblem'
     l.or.mname(mt1)(:18)== 'pmatrix_subproblem')then;else; lerr=.true.;endif;case(3); if(mname(mt1)(:19)== 'matrix_of_scenarios')th
     len; kscen=mnb(mt1); else; lerr=.true.; endif;end select;enddo;if(lerr)then; wch='Problem Statement: incorrect name of matrix i
     ln Recourse function';call putmess('S',803,'Two Stage Problem',wch); goto 19999;endif;if(kscen<=0)then; wch='Scenarios are not 
     ldefined in Recourse function';call putmess('S',806,'Two Stage Problem',wch); goto 19999;endif;mt1=nmatr(nfmatr-1+1);mt4=nmatr(
     lnfmatr-1+3); mt3=nmatr(nfmatr-1+2);mm3=mnb(mt3); mm4=mnb(mt4); nxy=nnb3;ml=mm3-3;if(ml<0)then; wch='Number of rows is less tha
     ln three in '//trim(mname(mt3))//' in Recourse function';call putmess('S',809,'Two Stage Problem',wch); goto 19999;endif;nlb=-1
     l; nub=-1;do i=1,rn1; if(rxname(i)(:13)=='lower_bounds ')then; nlb=i; nxy=nxy-1; endif;if(rxname(i)(:13)=='upper_bounds ')then;
       nub=i; nxy=nxy-1; endif;enddo;ny=nxy-nx;if(nlb>0)then; do i=0,nnb3; if(ix(nfix(mt3)+i)==nlb)then; nlb=i; Exit; endif; enddo;i
     lf(i>nnb3)then; wch=trim(mname(mt3))//' does not contain column Lower_bounds';call putmess('S',815,'Two Stage Problem',wch); go
     lto 19999;endif;endif;if(nub>0)then; do j=0,nnb3; if(ix(nfix(mt3)+j)==nub)then; nub=j; Exit; endif; enddo;if(j>nnb3)then; wch=t
     lrim(mname(mt3))//' does not contain column Upper_bounds';call putmess('S',818,'Two Stage Problem',wch); goto 19999;endif;endif
      do i=0,nnb3; if(ix(nfix(mt3)+i)==0)then; nsb=i; Exit; endif; enddo;allocate(jss13(0:nnb1),jss23(0:ny),jss43(0:nnb4),iss43(0:nn
     lb4),stat=i);if(i/=0)then; write(wch,'(a)')"Allocation_1 failed"; call putmess('S',8210,'Two Stage Problem',wch); goto 19999;en
     ldif;do i=0,nnb1; i1=ix(nfix(mt1)+i);do j=0,nnb3; if(xname(i1)==rxname(ix(nfix(mt3)+j)))then; jss13(i)=j; Exit; endif; enddo;if
     l(j>nnb3)then; wch=trim(mname(mt1))//' contains variable name which is not present in '//trim(mname(mt3));call putmess('S',824,
     l'Two Stage Problem',wch); goto 19999;endif;enddo;yi3(0:nnb3,0)=0d0; if(nlb>=0) yi3(nlb,0)=-1.; if(nub>=0) yi3(nub,0)=-1.;do i=
     l0,nnb1; yi3(jss13(i),0)=-1.; enddo;i=0;do j=0,nnb3; if(yi3(j,0)==0d0)then; i=i+1; jss23(i)=j; endif;enddo;if(i/=ny)then; write
     l(wch,'(a)')"Internal error: "//trim(mname(mt3))//" contains unknown variable";call putmess('S',827,'Two Stage Problem',wch); g
     loto 19999;endif;do i=0,nnb4; i4=ix(nfix(mt4)+i); if(i4<=0)then; jss43(i)=-1; i4bm=i; Cycle; endif;i1=scan(rxname(i4),'('); i2=
     lscan(rxname(i4),')',.true.);if(i1<=0.or.i2<=0.or.i1+1>i2-1)then;wch=trim(mname(mt4))//' contains incorrect entry (pointing to 
     lan element in matrix_subproblem) in header row';call putmess('S',830,'Two Stage Problem',wch); goto 19999;endif;read(rxname(i4
     l)(i1+1:i2-1),*,err=19999)iw;if(iw<1.or.iw>mm3)then; wch=trim(mname(mt4))//' contains incorrect row number in some element in h
     leader row';call putmess('S',833,'Two Stage Problem',wch); goto 19999;endif;do j=0,nnb3; if(rxname(i4)(:i1-1)==rxname(ix(nfix(m
     lt3)+j)))then; iss43(i)=iw; jss43(i)=j; Exit; endif; enddo;if(j>nnb3)then; wch=trim(mname(mt4))//' contains incorrect name of v
     lariable in header row';call putmess('S',836,'Two Stage Problem',wch); goto 19999;endif;enddo;call SpMatrixAddrs(yi3,yi3,mm3,nn
     lb3, sp_out,i);if(.not.sp_out)then; kel3=mm3*(nnb3+1)+1;else; call SpMatrixKelm(kel3); kel3=kel3+1;endif;call findBench(ix(nfix
     l(mt3)),nnb3, isb);if(sp_out)then;call SpM_MatrSign(mm3); call SpM_ColSign(mm3,isb);else; yi3(0:nnb3,1:mm3)=-yi3(0:nnb3,1:mm3);
      yi3(isb,1:mm3)=-yi3(isb,1:mm3);endif;yi4(0:nnb4,1:mm4)=-yi4(0:nnb4,1:mm4);allocate(xw(0:nx),ylb(ny),dylb(ny),clb(ml),dclb(ml),
     lzk(ml),stat=iw);if(iw/=0)then; write(wch,'(a)')"Allocation_2 failed"; call putmess('S',821,'Two Stage Problem',wch); goto 1999
     l9; endif;if(.not.sp_out)then; allocate(ax(0:nx,0:ml),ay(ny,0:ml+2),stat=iw);else; i=kel3+ny*2; j=max0(mm3,nnb3)+1;allocate(axs
     l(kel3),fjx(0:mm3+1),jx(kel3),   ays(i),fjy(0:mm3+1),jy(i),  wrk(j),jw(j),
     +fej(0:nx+1), iel(kel3), nel(kel3), stat=iw);endif;if(iw/=0)then; write(wch,'(a)')"Allocation_3 failed"; call putmess('S',821,'
     lTwo Stage Problem',wch); goto 19999; endif;call copybuff(loc(i4bm),4,loc(yi4),4);wch='@TSP';call copybuff(loc(wch),4,loc(yi4)+
     l4,4);yi4(i4bm,:mm4)=0d0;mxmax=0;ionlysb=-1;isb=0; ilb=0; iub=0; idy=0;allocate(iwrk(2,mm3)); iwrk=-1;do i4=0,nnb4;if(jss43(i4)
     l>=0)then; i=iss43(i4); j=jss43(i4);if(i>1.and.i<mm3-1)then;if(j==nlb)then; ilb=1; iwrk(1,i)=1; if(iwrk(2,i)/=-1) Exit;if(nub>=
     l0)then; if(yi3(nub,i)<1d20) Exit; endif;   Cycle;endif;if(j==nub)then; iub=1; iwrk(2,i)=1; if(iwrk(1,i)/=-1) Exit;if(nlb>=0)th
     len; if(yi3(nlb,i)>-1d20) Exit; endif;  Cycle;endif;if(j==nsb)then; isb=1; Cycle; endif;Exit;else; Exit;endif;endif;enddo;deall
     locate(iwrk);w=maxval(abs(yi4(:,1:mm4)));if(w>=1d20) i4=0;if(i4>nnb4)then;do i=0,nnb1; if(jss13(i)==nsb) Exit; enddo;ionlysb=i;
      else; isb=0; ilb=0; iub=0; idy=0;endif;dvuh=1d5;igetmark=1;do 200 iscen=1,mm4;if(ionlysb>=0.and.iscen>1) then;if(.not.sp_out)t
     lhen;if(isb>0)then;do i4=0,nnb4; j=jss43(i4); if(j<0.or.j/=nsb)Cycle;i=iss43(i4)-1; ax(ionlysb,i)=ax(ionlysb,i)+(yi4(i4,iscen)-
     lyi4(i4,iscen-1))*zk(i);enddo;endif;if(ilb+iub>0)then;do i4=0,nnb4; j=jss43(i4); if(j<0.or.j==nsb)Cycle;i=iss43(i4)-1; w=yi4(i4
     l,iscen)-yi4(i4,iscen-1);if(zk(i)<0.)w=-w; ax(ionlysb,i)=ax(ionlysb,i)-w;enddo;endif;else;if(isb>0)then;do i4=0,nnb4; j=jss43(i
     l4); if(j<0.or.j/=nsb)Cycle; i=iss43(i4)-1;do el=fjx(i),fjx(i+1)-1;if(jx(el)==ionlysb)then;axs(el)=axs(el)+(yi4(i4,iscen)-yi4(i
     l4,iscen-1))*zk(i); Exit;endif;enddo;enddo;endif;if(ilb+iub>0)then;do i4=0,nnb4; j=jss43(i4); if(j<0.or.j==nsb)Cycle; i=iss43(i
     l4)-1;do el=fjx(i),fjx(i+1)-1;if(jx(el)==ionlysb)then; w=yi4(i4,iscen)-yi4(i4,iscen-1);if(zk(i)<0.)w=-w; axs(el)=axs(el)-w; Exi
     lt;endif;enddo;enddo;endif;endif;goto 300;endif;ylb=0d0; dylb=0d0;clb=0d0; dclb=0d0;do i4=0,nnb4; if(jss43(i4)<0)Cycle;if(sp_ou
     lt)then; call SpM_AssElem(mm3,jss43(i4),iss43(i4),yi4(i4,iscen),  iw);if(iw==0)then; wch='Element in header row of '//trim(mnam
     le(mt4))//
     +' is not explicitly present in '//trim(mname(mt3))//' in Recourse function';call putmess('S',851,'Two Stage Problem',wch); got
     lo 19999;endif;else; yi3(jss43(i4),iss43(i4))=yi4(i4,iscen);endif;enddo;kex=0; key=0;if(sp_out)then;fjx(0)=1; fjy(0)=1; iw=mm3-
     l2;do i=1,mm3; call SpM_GetSpRow(i,0,nnb3, wrk,jw,k);do 100 el=1,k; j=jw(el);do j1=0,ny;if(j==jss23(j1))then; key=key+1; ays(ke
     ly)=wrk(el); jy(key)=j1;goto 100;endif;enddo;do j1=0,nnb1;if(j==jss13(j1))then; kex=kex+1; axs(kex)=wrk(el); jx(kex)=j1; Exit; 
      endif;enddo
100   enddo;fjx(i)=kex+1;fjy(i)=key+1;enddo;fej(0)=1; kel3=0;do j1=0,nnb1;do i=0,ml;do el=fjx(i),fjx(i+1)-1;if(jx(el)==j1)then; kel3
     l=kel3+1; iel(kel3)=i; nel(kel3)=el; goto 150; endif;enddo
150   enddo;fej(j1+1)=kel3+1;enddo;if(nlb>=0) call SpM_GetCol(nlb,2,mm3-2, clb );if(nub>=0) call SpM_GetCol(nub,2,mm3-2, dclb );do e
     ll=fjy(ml+1),fjy(ml+2)-1;  ylb(jy(el))=ays(el); enddo;do el=fjy(ml+2),fjy(ml+3)-1; dylb(jy(el))=ays(el); enddo;else;do i=0,nnb1
     l; j=jss13(i); ax(i,0:ml)=yi3(j,1:mm3-2); enddo;do i2=1,ny; j=jss23(i2); ay(i2,0:mm3-1)=yi3(j,1:mm3); enddo;if(nlb>=0)clb=yi3(n
     llb,2:mm3-2);  if(nub>=0) dclb=yi3(nub,2:mm3-2);ylb(1:ny)=ay(1:ny,ml+1); dylb(1:ny)=ay(1:ny,ml+2);endif;i=0;do i=1,ny;if(ylb(i)
     l<=-1d13.and.dylb(i)<1d13)then;if(.not.sp_out)then; ay(i,0:ml)=-ay(i,0:ml);else; do el=1,key; if(jy(el)==i) ays(el)=-ays(el); e
     lnddo;endif;w=-ylb(i); ylb(i)=-dylb(i); dylb(i)=w;endif;if(ylb(i)<=-1d13)ylb(i)=-xhuge; if(dylb(i)>=1d13)dylb(i)=xhuge;dylb(i)=
     ldylb(i)-ylb(i);if(dylb(i)<0d0)then; wch='Lower bound > Upper bound for some variable in '//trim(mname(mt3))//' in Recourse fun
     lction';call putmess('S',839,'Two Stage Problem',wch); goto 19999;endif;if(ylb(i)==-xhuge)ylb(i)=0d0;enddo;iw=0; zk=1d0;do i=1,
     lml;if(clb(i)<=-1d20.and.dclb(i)<1d20)then; zk(i)=-1d0;if(.not.sp_out)then; ay(1:ny,i)=-ay(1:ny,i); ax(0:nx,i)=-ax(0:nx,i);else
      do el=fjy(i),fjy(i+1)-1; ays(el)=-ays(el); enddo;do el=fjx(i),fjx(i+1)-1; axs(el)=-axs(el); enddo;endif;w=-clb(i); clb(i)=-dcl
     lb(i); dclb(i)=w;endif;if(clb(i)<=-1d20)clb(i)=-xhuge; if(dclb(i)>=1d20)dclb(i)=xhuge;dclb(i)=dclb(i)-clb(i);if(dclb(i)>=scale(
     lxhuge,1)) iw=iw+1;if(dclb(i)<0d0)then; wch='Lower bound > Upper bound for some constraint in '//trim(mname(mt3))//' in Recours
     le function';call putmess('S',842,'Two Stage Problem',wch); goto 19999;endif;enddo;kdc=count(0d0<dclb.and.dclb<xhuge);kdv=kdc+c
     lount(0d0<dylb.and.dylb<xhuge);nd=ml+kdv;mk=ny+kdc;mx=mk+1;mxmem=1000000;if(nd+mx>maxndx) maxndx=nd+mx;if(.not.sp_out)then; w=m
     laxval(abs(ay(1:ny,1:ml)))*1d-13;kel=count(abs(ay(1:ny,1:ml))>w);else; w=maxval(abs(ays(fjy(1):fjy(ml+1)-1)))*1d-13;kel=count(a
     lbs(ays(fjy(1):fjy(ml+1)-1))>w);endif;kel=kel+kdv+kdc+nd;ipfree=kel+1;call ssx10(mx,nd,mxmem,  workpath,iun,
     +mapi, mapdp );memfree=min(mapi(13),mapdp(7))-ipfree;mxmem=mxmem-memfree;mapi=mapi-memfree; mapdp=mapdp-memfree;i=(8+4)*mxmem; 
      j=i/8; if(j*8<i) j=j+1;j=3+j+mapsize+1+ml+3*nd;if(.not.sp_out)then;ln=(j + (1+nx)*(1+ml));else;ln=(j + kex + kex/2+1 + (ml+2)/
     l2+1 + (nx+2)/2+1 + kex/2+1  + kex/2+1);endif;prall=malloc(8*ln); piall=prall; lnmax=max(lnmax,ln);call setRealArrPointer(prall
     l,ln, realall);call setIntArrPointer(piall,2*ln, iintall);iintall(1)=mxmem; iintall(2)=mx; iintall(3)=nd; iintall(4)=ml; iintal
     ll(5)=nx; iintall(6)=kex;i=3+1; j=3+mxmem; dparr=>realall(i:j);i=2*j+1; j=2*j+mxmem; intarr=>iintall(i:j);i=j+1; j=j+mapsize; i
     lintall(i:j)=mapdp;i=j+1; j=j+mapsize; iintall(i:j)=mapi;call copybuff(loc(prall),plen,loc(yi4(i4bm,iscen)),plen);if(iscen==1)t
     lhen; memwork=(nd+mx)*nd*10; if(memwork<=0.or.memwork>40e6) memwork=int(40e6);j=int(1.5*memwork+1); pwrl=malloc(8*(1+j)); piwrl
     l=pwrl; intwork(1)=memwork;pwrl=pwrl+8; piwrl=pwrl+8*memwork;endif;dparr(:mxmem)=0d0; intarr(:mxmem)=0;rhs=> dparr(mapdp(14):ma
     lpdp(14)+mx-1);rng=> dparr(mapdp(15):mapdp(15)+mx-1);xd => dparr(mapdp(2):mapdp(2)+nd+mx-1);relcode=>intarr(mapi(12):mapi(12)+m
     lx-1);if(iscen==1) then; mark=>intarr(mapi(9):mapi(9)+nd+mx-1);kdv0=kdv; kdc0=kdc;elseif(ionlysb<0.and.igetmark>=0)then;if(kdv0
     l/=kdv.or.kdc0/=kdc)then;igetmark=-1;else;intarr(mapi(9):mapi(9)+nd+mx-1)=markp(:nd+mx);endif;endif;markp=>intarr(mapi(9):mapi(
     l9)+nd+mx-1);kac=> intarr(mxmem+1-nd-1:mxmem);xd(1:nd+mx)=0d0;kac=1;rhs=0d0;if(.not.sp_out)then; rhs(1:ny)=ay(1:ny,0);else; do 
     lel=fjy(0),fjy(1)-1; rhs(jy(el))=ays(el); enddo;endif;rng=xhuge;relcode=1; relcode(mx)=3;do j=1,ny; if(dylb(j)==0d0)then; relco
     lde(j)=3; elseif(dylb(j)>=scale(xhuge,1))then; relcode(j)=0; rng(j)=0d0; endif;enddo;if(lnot_Null(loc(dvl)))deallocate(dvl); if
     l(lnot_Null(loc(dvu)))deallocate(dvu);allocate(dvl(nd),dvu(nd));dvl=-xhuge; dvu=xhuge;i1=ml;do i=1,ml;if(dclb(i)==xhuge)then; d
     lvl(i)=0d0; dvu(i)=dvuh;elseif(0d0==dclb(i))then; dvl(i)=-dvuh; dvu(i)=dvuh;elseif(0d0<dclb(i).and.dclb(i)<xhuge)then; i1=i1+1;
       dvl(i1)=0d0; dvu(i1)=dvuh;else; dvl(i)=0d0; dvu(i)=0d0;endif;enddo;do i=1,ny;if(0d0<dylb(i).and.dylb(i)<xhuge)then; i1=i1+1; 
      dvl(i1)=0d0; endif;enddo;if(associated(am))deallocate(am); if(allocated(as))deallocate(as);if(allocated(js))deallocate(js); if
     l(allocated(is))deallocate(is);if(.not.sp_out)then; kel=0; iam=1;allocate(am(nd,mx),as(kel+1),js(kel+1),is(0:kel+1),stat=i);els
     le; kel0=kel; iam=0;allocate(am(nd,mx:mx),as(kel+1),js(kel+1),is(0:kel+1),stat=i);endif;am=0d0; is(1:kel+1)=nd+1; is(0)=0;if(.n
     lot.sp_out)then;am(1:ml,1:ny)=TRANSPOSE(ay(1:ny,1:ml));i=ny; j=ml;do i3=1,ml;if(0d0<dclb(i3).and.dclb(i3)<xhuge)then; i=i+1; j=
     lj+1; am(i3,i)=-1d0; am(j,i)=-1d0; rng(i)=dvuh; endif;enddo;do i2=1,ny;if(0d0<dylb(i2).and.dylb(i2)<xhuge)then; j=j+1; am(j,i2)
     l=-1d0; endif;enddo;else; i=ny; kel=0;do i3=1,ml;do el=fjy(i3),fjy(i3+1)-1; kel=kel+1; as(kel)=ays(el); is(kel)=i3; js(kel)=jy(
     lel); enddo;if(0d0<dclb(i3).and.dclb(i3)<xhuge)then; i=i+1; rng(i)=dvuh;kel=kel+1; as(kel)=-1d0;   is(kel)=i3; js(kel)=i;endif;
      enddo;i=ny; j=ml;do i3=1,ml;if(0d0<dclb(i3).and.dclb(i3)<xhuge)then; j=j+1; i=i+1;kel=kel+1; as(kel)=-1d0;   is(kel)=j;  js(ke
     ll)=i;endif;enddo;do i2=1,ny;if(0d0<dylb(i2).and.dylb(i2)<xhuge)then; j=j+1;kel=kel+1; as(kel)=-1d0;   is(kel)=j;  js(kel)=i2;e
     lndif;enddo;if(kel/=kel0-nd)then; write(wch,'(a)')"Internal error 2: kel/=kel0";call putmess('S',844,'Two Stage Problem',wch); 
      goto 19999;endif;endif;if(i/=mx-1.or.j/=nd)then; write(wch,'(a)')"Internal error 1";call putmess('S',845,'Two Stage Problem',w
     lch); goto 19999;endif;call insert_M_constr(mxmem,nd,nd,mx-1,iam,am,kel,as,is,js,1,intarr,dparr,kac,iret);if(iret==1) goto 1999
     l9;xw=1.;do i=0,nnb1; if(ix(nfix(mt1)+i)==0)then; xw(i)=1.; Exit; endif; enddo;if(.not.sp_out)then; dl=dot_product(ay(1:ny,0),y
     llb);else;dl=0d0; do el=fjy(0),fjy(1)-1; dl=dl+ays(el)*ylb(jy(el)); enddo;endif;if(.not.sp_out)then; do i=1,ml; clb(i)=clb(i)-d
     lot_product(ay(1:ny,i),ylb); enddo;else;do i=1,ml; w=0d0; do el=fjy(i),fjy(i+1)-1; w=w+ays(el)*ylb(jy(el)); enddo;clb(i)=clb(i)
     l-w;enddo;endif;j=ml;do i3=1,ml; if(0d0<dclb(i3).and.dclb(i3)<xhuge)then; j=j+1; am(j,mx)=+dclb(i3); endif;enddo;do i2=1,ny; if
     l(0d0<dylb(i2).and.dylb(i2)<xhuge)then; j=j+1; am(j,mx)=+dylb(i2); endif;enddo
300   continue;if(.not.sp_out)then; cx=dot_product(ax(0:nx,0),xw);else; cx=0d0;do j=fjx(0),fjx(1)-1; cx=cx+axs(j)*xw(jx(j)); enddo;e
     lndif;if(.not.sp_out)then;do i3=1,ml; am(i3,mx)=-(clb(i3)-dot_product(ax(0:nx,i3),xw)); enddo;else;do i3=1,ml; w=0d0; do j=fjx(
     li3),fjx(i3+1)-1; w=w+axs(j)*xw(jx(j)); enddo;am(i3,mx)=-(clb(i3)-w);enddo;endif;call insert_constr(nd,am(1:nd,mx),mx, intarr,d
     lparr,kac);i=0; if(allocated(wrk)) i=size(wrk);if(i<mx)then; if(i>0)deallocate(wrk); allocate(wrk(mx)); endif;if(mx>mxmax) mxma
     lx=mx;wrk(:mx)=rhs;ibs=1; if(iscen==1.or.(ionlysb<0.and.igetmark<0).or.ifail>2)ibs=0
400   timelimit=100000;ibst=1;ialgtyp=1;lscale=.false.;call ssx11(0,mx,nd,memwork, dvl,dvu, mapi,mapdp, ialgtyp,lscale,
     +ibs,ibst,timelimit,
     +dparr, intarr, dpwork, intwork,
     +ifail,sinf );rhs=wrk(:mx);if(ifail>1)then;if(ifail==12.and.dvuh<=1d9)then;ibs=1; w=dvuh; dvuh=dvuh*10.;do i=1,nd; if(dvl(i)==-
     lw) dvl(i)=-dvuh; if(dvu(i)==w) dvu(i)=dvuh; enddo;do i=ny+1,mk; if(rng(i)==w) rng(i)=dvuh; enddo;goto 400;elseif(ifail==2)then
     l;write(wch,'(a)')'Internal error 22. Subproblem undounded';call putmess('S',852,'Two Stage Problem',wch); goto 19999;else;if(i
     lbs==0)then; write(wch,'(a,i5)')'Internal error 2. Subproblem failed ',ifail;call putmess('S',848,'Two Stage Problem',wch); got
     lo 19999;else; ibs=0; goto 400;endif;endif;endif;if(ioutk>=istop-1) goto 19999;call delete_constr(nd,mx,  intarr,dparr,kac);f=d
     ll+cx-(-xd(nd+mx));if(ionlysb>=0.and.iscen>1)then;j=nd;j=j+(nd+mx)/2+1;iw=(1+nx)*(1+ml); if(sp_out) iw=kex;ln=(iw+j); prall=mal
     lloc(8*ln); piall=prall; lnmax=max(lnmax,ln);call setRealArrPointer(prall,ln, realall);call setIntArrPointer(piall,2*ln, iintal
     ll);call copybuff(loc(prall),plen,loc(yi4(i4bm,iscen)),plen);if(sp_out)then; realall(1:iw)=axs;else; call copybuff(loc(ax),8*iw
     l,loc(realall),8*iw);endif;realall(iw+1:iw+nd)=am(1:nd,mx); iintall(2*(iw+nd)+1:2*(iw+nd)+nd+mx)=mark;else;j=6+3*mxmem;i=j+1; j
     l=j+mapsize; i=j+1; j=j+mapsize;i=j/2; if(i*2<j)i=(j+1)/2;j=i+1;            realall(j)=dl;if(.not.sp_out)then;i=j+1; j=j+(1+nx)
     l*(1+ml); call copybuff(loc(ax),8*(j-i+1), loc(realall(i)),8*(j-i+1));else;i=j+1; j=j+kex; call copybuff(loc(axs),8*(j-i+1), lo
     lc(realall(i)),8*(j-i+1));i=j+1; j=j+kex/2+1; call copybuff(loc(jx),8*(j-i+1), loc(realall(i)),8*(j-i+1));i=j+1; j=j+(ml+2)/2+1
     l; call copybuff(loc(fjx),8*(j-i+1), loc(realall(i)),8*(j-i+1));i=j+1; j=j+(nx+2)/2+1; call copybuff(loc(fej),8*(j-i+1), loc(re
     lalall(i)),8*(j-i+1));i=j+1; j=j+kex/2+1; call copybuff(loc(iel),8*(j-i+1), loc(realall(i)),8*(j-i+1));i=j+1; j=j+kex/2+1; call
     l copybuff(loc(nel),8*(j-i+1), loc(realall(i)),8*(j-i+1));endif;i=j+1; j=j+ml;           realall(i:j)=clb;i=j+1; j=j+nd;       
          realall(i:j)=dvl;i=j+1; j=j+nd;           realall(i:j)=dvu;i=j+1; j=j+nd;           realall(i:j)=am(1:nd,mx);endif
200   enddo;mnb(mt1)=mm4;call free(pwrl-8); yi4(i4bm,0)=0.;pwrl=malloc(2*8+5*4); call copybuff(loc(pwrl),plen,loc(yi4(i4bm,0)),plen)
      call copybuff(loc(xhuge),8,pwrl,8); pwrl=pwrl+8;call copybuff(loc(dvuh),8,pwrl,8); pwrl=pwrl+8;call copybuff(loc(mxmax),4,pwrl
     l,4); pwrl=pwrl+4;call copybuff(loc(ionlysb),4,pwrl,4); pwrl=pwrl+4;call copybuff(loc(igetmark),4,pwrl,4); pwrl=pwrl+4;call cop
     lybuff(loc(ny),4,pwrl,4); pwrl=pwrl+4;call copybuff(loc(sp_out),4,pwrl,4)
19999 deallocate(jss13,jss23,jss43,iss43, stat=i);deallocate(xw,ylb,dylb,clb,dclb,zk, stat=i);if(associated(ax)) deallocate(ax,ay, s
     ltat=i);if(associated(axs)) deallocate(axs,fjx,jx,ays,fjy,jy,jw,fej,iel,nel, stat=iw);deallocate(iwrk, stat=iw);if(associated(d
     lvl))deallocate(dvl,dvu);if(associated(am))deallocate(am); if(allocated(as))deallocate(as);if(allocated(js))deallocate(js); if(
     lallocated(is))deallocate(is);if(allocated(wrk))deallocate(wrk);RETURN;ENTRY FM_Recourse(mname,x,ix1,nnb1,m4,nnb4,yi4,p,     yi
     l1,jmax,jmin,avg,fm,pf)
      piwrl=0; dl=0.; f=0.; nx=0; ml=0; nullify(rhs,xd);call copybuff(loc(yi4),4,loc(i4bm),4);call copybuff(loc(yi4(i4bm,0)),plen,lo
     lc(pwrl),plen);call copybuff(pwrl,8,loc(xhuge),8); pwrl=pwrl+8;call copybuff(pwrl,8,loc(dvuh),8); pwrl=pwrl+8;call copybuff(pwr
     ll,4,loc(mxmax),4); pwrl=pwrl+4;call copybuff(pwrl,4,loc(ionlysb),4); pwrl=pwrl+4;call copybuff(pwrl,4,loc(igetmark),4); pwrl=p
     lwrl+4;call copybuff(pwrl,4,loc(ny),4); pwrl=pwrl+4;call copybuff(pwrl,4,loc(sp_out),4);dvuh0=dvuh;imarks=0; ws=0.; wp=0.;alloc
     late(xw(0:nnb1),wrk(mxmax));do i=0,nnb1; xw(i)=x(ix1(i)); enddo;fmi=huge(fmi); fma=-fmi; avg=0d0;yi1(0:nnb1,0)=0d0;do 333 iscen
     l=1,m4;ifail=0
343   call copybuff(loc(yi4(i4bm,iscen)),plen,loc(prall),plen);piall=prall;call setRealArrPointer(prall,lnmax, realall);call setIntA
     lrrPointer(piall,2*lnmax, iintall);if(.not.(ionlysb>=0.and.iscen>1))then;mxmem=iintall(1);mx=iintall(2);nd=iintall(3);ml=iintal
     ll(4);nx=iintall(5);kex=iintall(6);dparr=>realall(4:3+mxmem);j=6+3*mxmem; intarr=>iintall(6+2*mxmem+1:6+3*mxmem);i=j+1; j=j+map
     lsize; mapdp=iintall(i:j);i=j+1; j=j+mapsize; mapi=iintall(i:j);i=j/2; if(i*2<j)i=(j+1)/2;j=i+1;            dl=realall(j);sp_ou
     lt=kex>0;if(.not.sp_out)then;i=j+1; j=j+(1+nx)*(1+ml);call AX1_set(realall(i:j),nx,ml,ax);else;i=j+1; j=j+kex;        axs=>real
     lall(i:j);i=j+1; j=j+kex/2+1;    call i4_pointer_set(iintall(2*i-1:2*j-1),1,kex,jx);i=j+1; j=j+(ml+2)/2+1; call i4_pointer_set(
     liintall(2*i-1:2*j-1),0,ml+1,fjx);i=j+1; j=j+(nx+2)/2+1; call i4_pointer_set(iintall(2*i-1:2*j-1),0,nx+1,fej);i=j+1; j=j+kex/2+
     l1;    call i4_pointer_set(iintall(2*i-1:2*j-1),1,kex, iel);i=j+1; j=j+kex/2+1;    call i4_pointer_set(iintall(2*i-1:2*j-1),1,k
     lex, nel);endif;i=j+1; j=j+ml;           clb=>realall(i:j);i=j+1; j=j+nd;           dvl=>realall(i:j);i=j+1; j=j+nd;           
      dvu=>realall(i:j);i=j+1; j=j+nd;           vds=>realall(i:j);rhs=> dparr(mapdp(14):mapdp(14)+mx-1);rng=> dparr(mapdp(15):mapdp
     l(15)+mx-1);xd=>  dparr(mapdp(2):mapdp(2)+nd+mx-1);relcode=>intarr(mapi(12):mapi(12)+mx-1);mark=>intarr(mapi(9):mapi(9)+nd+mx-1
     l);kac=> intarr(mxmem+1-nd-1:mxmem);else;if(sp_out)then; iw=kex; axs=>realall(1:iw);else; iw=(1+nx)*(1+ml); call AX1_set(realal
     ll(1:iw),nx,ml,ax);endif;vds=>realall(iw+1:iw+nd);marks=>iintall(2*(iw+nd)+1:2*(iw+nd)+nd+mx);endif;allocate(vd(nd));vd(ml+1:nd
     l)=vds(ml+1:nd);if(.not.sp_out)then;do i3=1,ml; vd(i3)=-(clb(i3)-dot_product(ax(0:nx,i3),xw));enddo;else;do i3=1,ml; w=0d0; do 
     lj=fjx(i3),fjx(i3+1)-1; w=w+axs(j)*xw(jx(j)); enddo;vd(i3)=-(clb(i3)-w);enddo;endif;if(associated(vdp))then;if(ionlysb<0.and.ig
     letmark>=0)then;if(iscen>1)then; ws=sum(abs(vd-vds)); wp=sum(abs(vd-vdp)); imarks=1;if(wp<=ws*10.or.igetmark>0)then; mark=markp
     l; imarks=0;endif;endif;markp=>mark;elseif(ionlysb>=0.and.igetmark<=0)then;if(iscen>1)then; ws=sum(abs(vd-vds)); wp=sum(abs(vd-
     lvdp)); imarks=0;if(ws*10<wp.or.igetmark<0)then; mark=marks; imarks=1;endif;else; imarks=1;endif;endif;vds=vd; vdp=>vds;endif;c
     lall insert_constr(nd,vd,mx, intarr,dparr,kac);if(associated(rhs)) wrk(:mx)=rhs;call Check_stop_whatch(1,w); timelimit=w+600;if
     l(ifail==0)then; ibs=1; else; ibs=0; endif;ibst=1;ialgtyp=1;lscale=.false.;if(iscen==1)then;memwork=(nd+mx)*nd*10; if(memwork<=
     l0.or.memwork>40e6) memwork=int(40e6);j=int(1.5*memwork+1); pwrl=malloc(8*(1+j)); piwrl=pwrl; intwork(1)=memwork;pwrl=pwrl+8; p
     liwrl=pwrl+8*memwork;endif;call ssx11(-1,mx,nd,memwork, dvl,dvu, mapi,mapdp, ialgtyp,lscale,
     +ibs,ibst,timelimit,
     +dparr, intarr, dpwork, intwork,
     +ifail,sinf );if(ioutk>=istop-1) goto 79999;if(associated(rhs)) rhs(1:mx)=wrk(:mx);call delete_constr(nd,mx,  intarr,dparr,kac)
      if(ionlysb>=0) marks=>mark;if(ifail>1)then;if(ifail==12.and.dvuh0<=1d9)then;ibs=0; w=dvuh0;j=count(-1d9<=dvl.and.dvl<=-w)+coun
     lt(w<=dvu.and.dvu<=1d9);if(j>0)then;do i=1,nd; if(-1d9<=dvl(i).and.dvl(i)<=-w) dvl(i)=-w*10.; if(w<=dvu(i).and.dvu(i)<=1d9) dvu
     l(i)=w*10.; enddo;do i=ny+1,mx-1; if(w<=rng(i).and.rng(i)<=1d9) rng(i)=w*10.; enddo;if(dvuh==w)dvuh=w*10.;else; dvuh=1d10;endif
      goto 343;elseif(ifail==2)then;write(wch,'(a,i5)')'Internal error 23. Unbounded subproblem ',iscen;call putmess('S',853,'Two St
     lage Problem',wch); goto 79999;else;if(ibs==0)then; write(wch,'(a,i5)')'Internal error 3. Subproblem failed ',ifail;call putmes
     ls('S',849,'Two Stage Problem',wch); goto 79999;else; ibs=0; goto 343;endif;endif;endif;if(.not.sp_out)then; cx=dot_product(ax(
     l0:nx,0),xw);else; cx=0.; do j=fjx(0),fjx(1)-1; cx=cx+axs(j)*xw(jx(j)); enddo;endif;if(associated(xd)) f=dl+cx-(-xd(nd+mx));if(
     lfma<f) then; fma=f; jmax=iscen; endif;if(fmi>f) then; fmi=f; jmin=iscen; endif;pf(iscen)=p(iscen)*f;avg=avg+pf(iscen);fm(iscen
     l)=f;xdamax=0;do j=1,nd; if(dvl(j)>-xhuge) xd(j)=xd(j)+dvl(j);w=abs(xd(j)); if(w>xdamax)xdamax=w;enddo;if(mname(1)=='Objects')t
     lhen;w=dvuh0; j=0;dual=>dparr(mapdp(17):mapdp(17)+mx-2);do i=1,nd;if(dvl(i)<=-w.and.xd(i)<=dvl(i)*(1.-1d-7).or.w<=dvu(i).and.xd
     l(i)>=dvu(i)*(1.-1d-7))then;if(abs(vd(i)-VectDotMatr_Ish(dual,i, intarr,dparr,kac))>1d-7) j=j+1;endif;enddo;do i=ny+1,mx-1; if(
     lrng(i)==w.and.xd(nd+i)==0..and.dual(i)/=0.) j=j+1;enddo;if(j>0.and.dvuh==w)then; dvuh=w*10.;write(wch,'(a,i7)')'SecondStage Pr
     loblem is infeasible for scenario ',iscen; call putmess('W',0,'TwoStage Problem',wch);endif;endif;if(.not.sp_out)then;do j=0,nn
     lb1; yi1(j,iscen)=ax(j,0)-dot_product(xd(1:ml),ax(j,1:ml));enddo;else; yi1(0:nnb1,iscen)=0d0;do el=fjx(0),fjx(1)-1; yi1(jx(el),
     liscen)=axs(el); enddo;do j=0,nnb1;w=0d0; do el=fej(j),fej(j+1)-1; if(iel(el)<=0) Cycle; w=w+xd(iel(el))*axs(nel(el)); enddo;yi
     l1(j,iscen)=yi1(j,iscen)-w;enddo;endif;w=p(iscen);  yi1(0:nnb1,0)=yi1(0:nnb1,0)+w*yi1(0:nnb1,iscen)
333   enddo
79999 deallocate(xw,wrk);if(associated(vd))deallocate(vd);call free(pwrl-8);RETURN;ENTRY Free_Recourse(m4,nnb4,yi4)
      call copybuff(loc(yi4)+4,4,loc(chw),4);if(chw(:4)/='@TSP') RETURN;call copybuff(loc(yi4),4,loc(i4bm),4);do iscen=0,m4; if(yi4(
     li4bm,iscen)==0d0)Cycle;call copybuff(loc(yi4(i4bm,iscen)),plen,loc(prall),plen);call free(prall);enddo;nullify(dvl,dvu,mark,ma
     lrks);RETURN;end subroutine SecondStageProblem;subroutine AX1_set(rarr,nx,ml,ax);integer(4) nx,ml;real(8),pointer::ax(:,:); rea
     ll(8),target::rarr(0:nx,0:ml);ax=>rarr;
      end subroutine AX1_set
