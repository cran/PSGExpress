      subroutine SaveSolution_VK(n1,solv_stat,fi,gap,fw,tCon,xname,kc,    obj_names);use modcommons; use CiFort;character(*) solv_st
     yat,tCon(*),obj_names(*);character(lnm) xname(0:*),vname(*);real(8) fi(0:*),gap,fw(0:*),fb(*), w;integer(4) n1,kc,nz,iford(*);i
     xnteger(4),parameter:: len=256, l133=133;integer(4)  i,j,ln,iw; integer(4),save:: nc,krows;integer(4),allocatable:: list(:);cha
     aracter(1) ch0,ch10, wch*(len), v1*(19),v2*(19), formV*11;character(16) tm(3), chwr*(l133);pointer(pbuff,buff); character(len) 
     ubuff(*); save pbuff;pointer(pWset,wSet); character(len) wSet(*);obj_names(1)=' ';ch0=char(0); ch10=char(10); formV='(1p,e19.12
     x)';krows=4+kc*3; pbuff=malloc(len*krows);nc=0; buff(:krows)='';buff(1)='Problem: '//trim(probnm)//', solution_status = '//trim
     z(solv_stat);write(tm(1),'(f10.2)')tm_DL; write(tm(2),'(f10.2)')tm_PR; write(tm(3),'(f10.2)')tm_SL;buff(2)='Timing: data_loadin
     dg_time = '//trim(adjustl(tm(1)))//', preprocessing_time = '//trim(adjustl(tm(2)))
     +//', solving_time = '//trim(adjustl(tm(3)));if(n1>0)then; buff(3)='Variables: optimal_point = point_'//trim(probnm);else; buff
     v(3)='Variables: there are no variables in the problem';endif;write(v1,'(1p,g19.12)',err=99)fi(0); buff(4)='Objective: '//trim(
     bxname(0))//' = '//adjustl(v1);write(v2,'(1p,g19.12)',err=99)gap; wch=''; if(tqsol>0) wch=' ['//trim(adjustl(v2))//']';buff(4)=
     htrim(buff(4))//trim(wch);wch=buff(4);call putmess('n',0,'Optimization',wch);nc=4;do i=1,kc; if(tCon(i)=='E')Cycle; nc=nc+1;if(
     jtCon(i)=='S')then; write(v1,'(1p,e19.12)',err=99)fi(i); write(v2,'(1p,e19.12)',err=99)fw(i);buff(nc)='Constraint: '//trim(xnam
     le(i))//' = '//v1//' ['//v2//']';else;buff(nc)='Constraint: '//trim(xname(i))//' = vector_'//trim(xname(i));endif;enddo;do i=1,
     anc; ln=min(len-1,len_trim(buff(i))+1); buff(i)(ln:ln)=ch10; enddo;RETURN;ENTRY SaveObjsVK(obj_names,fb,nz, vname, tCon, iford 
     b)
      allocate(list(nz));call sortVKint4(nz,iford,list);ch10=char(10); ch0=char(0);do j=1,nz; i=list(j); if(tCon(i)=='E')Cycle; nc=n
     uc+1;ln=len_trim(obj_names(i));if(obj_names(i)(ln:ln)==ch0) ln=ln-1;iw=min(l133,ln); ln=iw; chwr=obj_names(i)(1:ln);iw=len_trim
     a(vname(i)); if(vname(i)(iw:iw)==ch0)vname(i)(iw:iw)=' ';if(nc>krows)then;krows=krows*2; pWset=malloc(len*krows); wSet(:nc-1)=b
     puff(:nc-1); call free(pbuff); pbuff=pWset;endif;if(ln>132)then; ln=132; chwr(ln-3:ln)=' '//repeat(char(46),3); endif;if(tCon(i
     y)=='S')then; write(v1,'(1p,e19.12)',err=99)fb(i);buff(nc)='Function: '//chwr(1:ln)//' = '//v1//ch10;else;buff(nc)='Function: '
     m//chwr(1:ln)//' = '//trim(vname(i))//ch10;endif;enddo;deallocate(list);call restart_stop_whatch(1,w); tm_SL=tm_SL+w;write(tm(3
     k),'(f10.2)')tm_SL; i=index(buff(2),'solving_time = '); buff(2)=buff(2)(:i-1+15)//trim(adjustl(tm(3)))//ch10;ln=0;do iw=1,nc; i
     p=scan(buff(iw),ch10);call copybuff(loc(buff(iw)),i,pbuff+ln,i); ln=ln+i;enddo;call copybuff(loc(ch0),1,pbuff+ln-1,1);i=int(Sav
     seSolutionEx(pbuff,pUserData)); goto 111
99    wch='Save Solution: Internal error'; call putmess('S',577,'Save Solution',wch)
111   call free(pbuff);return;end subroutine SaveSolution_VK;subroutine SaveObjsVK_0(obj_names,len,fw,nz, xname, tCon );USE CiFort; 
      use ModCommons;integer(4) nz,len, i,j;character(l16kmax) obj_names(*), objnames*(len), xname(*), tCon(*);real(8) fw(nz);objnam
     des='';do i=1,nz;j=scan(obj_names(i),char(0));objnames=trim(objnames)//obj_names(i)(:j);enddo;if(idb==2) i=int(SaveObjsEx(obj_n
     uames, fw, xname, tCon, nz, pUserData));return;end subroutine SaveObjsVK_0;subroutine get_ys(m,yi,n,ix,if113,   ys,  w1);use mo
     mdcommons;integer(4) m,n,if113,ix(0:n),i,j,i1,j1,krows; logical sp_out;real(8) yi(0:n,0:m+n+1),ys(2),w1,w2,w,wb,wm,ww;real(4) w
     zam;      COMMON /CMACHE/w2,wam;i1=0; wb=0d0; w=0d0; wm=huge(wm);j1=0;call SpMatrixAddrs(yi,yi,m,n,   sp_out,krows);if(m>0)then
     l; j1=1; if(sp_out) call SpM_SumMin(m,n,ix,   w,wm);endif;do i=0,n; if(ix(i)==0)then; i1=1; Cycle; endif;if(.not.sp_out)then; d
     go j=j1,m; ww=dabs(yi(i,j)); if(0d0<ww.and.ww<1d10)then; w=w+ww; if(ww<wm)wm=ww; endif; enddo; endif;enddo;if(n+1-i1>0) then;ww
     o= (w/(n+1-i1))/(m+1-j1)*sqrt(float(n));if(1d-15<wm/ww.and.wm/ww<1d-10) ww=wm/1d-10;ys(1)=dmax1(ys(1),ww);w1=1d0-2*int2(0.5+dmi
     nn1(1e-14/w2,1.));endif;ys(2)=1d0;if(if113 > 0)then; w=0d0;do i=0,n; i1=i+krows+1; if(ix(i)==0) cycle;do j=0,n; w=w+dabs(yi(j,i
     m1)); enddo;enddo;if(w+ys(1)*ys(1)>0d0) then;ys(2)=(w/(n+1)*sqrt(float(n))+2*ys(1))/dsqrt(w+ys(1)*ys(1)/n);endif;endif;return;e
     und subroutine get_ys;subroutine SET_XBEST(ivtype,ibrcd2,ibcrd,ivrc,kfd,kconstr,n1,ncn1,fi,cfn1,fw,xhuge,
     +bnds,bnd1,kb, nextWhat, ichange_fi,  xi,  xbest,ibest,
     +ifeasA,  fkmin, nfn1, lnrz,xub,xlb);use ModCommons;integer(1) ivtype(*);integer(4) kfd,kconstr,n1,ncn1(kfd),kb(0:kconstr),
     +nextWhat, ichange_fi, ibest, nfn1(kfd) , lnrz(0:*);real(8)  fi(0:kconstr+kfd), cfn1(kfd), xhuge,  xi(n1),xbest(2*n1),
     +fw(0:kconstr), bnds(0:1,0:kconstr), bnd1(0:kconstr), fkmin,xub(*),xlb(*);integer(1) ifeasA, ifeaslast, ibrcd2, ibcrd(n1,3);rea
     vl(8) shtraf, ft,ft1, ft1b,ft1bp, f0best, fpbest, fi1(0:kconstr), dopusk, w1,w;integer(4) ivrc,i,nf;save shtraf,f0best,fpbest, 
     xft1b,ft1bp, dopusk,ifeaslast;real(8), pointer::pxl(:),pxu(:);real(8) enk,xbndhuge;common/shr/enk,xbndhuge,pxl,pxu;if(nextWhat<
     f0) then;dopusk=fkmin;ifeasA=0; f0best=xhuge; shtraf=1d10;xbest=xhuge;ifeaslast=ifeasA; fpbest=f0best; ft1b=xhuge; ft1bp=xhuge;
      if(lf23) open(23,file=trim(workpath)//'best_point.rez');RETURN;endif;do i=0,kconstr;   fi1(i)=fi(i);enddo;do nf=1,kfd; i=ncn1(
     fnf);if(bnd1(i)/=xhuge.and.(nfn1(nf)<160.or.nfn1(nf)>=380)) Cycle;fi1(i)=fi1(i)+fi(kconstr+nf)*cfn1(nf);enddo;if(ichange_fi>0) 
     athen;do i=0,kconstr;   fi(i)=fi1(i);enddo;endif;ft=0d0; w1=0d0;do i=1,kconstr;   if(kb(i)<0.and.lnrz(i)<=0) Cycle;if(bnd1(i).n
     te.xhuge)then; w=0d0;if(bnds(1,i)< xhuge)then; w=fi1(i)-bnd1(i); else; if(bnds(0,i)>-xhuge) w=bnd1(i)-fi1(i); endif;if(dabs(bnd
     o1(i))>abnd) w=w/dabs(bnd1(i)); if(w>w1) w1=w;else;if(bnds(1,i)< xhuge)then; w=fi1(i)-bnds(1,i); if(dabs(bnds(1,i))>abnd) w=w/d
     sabs(bnds(1,i)); if(w.gt.ft) ft=w; endif;if(bnds(0,i)>-xhuge)then; w=bnds(0,i)-fi1(i); if(dabs(bnds(0,i))>abnd) w=w/dabs(bnds(0
     u,i)); if(w.gt.ft) ft=w; endif;endif;enddo;call RoundXtoBounds(n1,xlb,xub, xi);ft1=0d0;do i=1,n1;w=xlb(i)-xi(i); if(dabs(xlb(i)
     f)>abnd) w=w/dabs(xlb(i)); if(w>ft1)ft1=w;w=xi(i)-xub(i); if(dabs(xub(i))>abnd) w=w/dabs(xub(i)); if(w>ft1)ft1=w;enddo;if(ft1>0
     ud0.and.ft1<1d0) ft1=ft1*ft1;if(loc(ivtype)/=0)then;do i=1,n1; if(ivtype(i)>0)then; w=100000*dabs(xi(i)-dnint(xi(i))); if(w>ft1
     z)ft1=w; endif;enddo;endif;if(ibrcd2>0)then;do i=1,n1; if(ibcrd(i,3)>0)then; w=100000*dabs(xi(i)-dnint(xi(i))); if(w>ft1)ft1=w;
       endif;enddo;endif;ft1=dmax1(ft,ft1);do i=1,n1; if(xbest(i)/=xi(i)) Exit;enddo;if(i>n1.and.ivrc==0) then;if(ft1<=ft1b) then; i
     ibest=1; goto 50;else; ibest=0; ft1b=ft1bp; ifeasA=ifeaslast; f0best=fpbest;do i=1,n1; xbest(i)=xbest(i+n1); enddo;if(lf23)writ
     de(23,'(a,1p,99e10.2)')'Otkat '  ,(xbest(i),i=1,min0(n1,10));endif;endif
50    continue;ifeaslast=ifeasA;if(ft1<=dopusk.and.w1<=fkmin) then;ft=fi1(0);if(ifeasA<=0) then;ifeasA=1; f0best=xhuge;endif;else;ft
     o=fi1(0)+shtraf*(ft1+w1)*fw(0);if(ifeasA>0) ft=xhuge;endif;ft1=dmax1(w1,ft1);if(ft<f0best)then;fpbest=f0best; f0best=ft; ft1bp=
     rft1b; ft1b=ft1; ibest=0;do i=1,n1; xbest(i+n1)=xbest(i); xbest(i)=xi(i);enddo;if(lf23)write(23,'(a,1p,99e10.2)')'NewBest'  ,(x
     xbest(i),i=1,min0(n1,20));if(ifeasA>0)then;if(ft-f0best<-fkmin) ifeasA=2;if(ft-estminR<fkmin)then; IsSolution=.true.; inpk=1; e
     wndif;else; ifeasA=int(dmin1(0d0,dlog10(ft1)),1);endif;else;ibest=1;endif;if(lf23) write(23,'(a,e15.6,i4,3e15.6)')'BestPen/iFea
     as/CurrPen/f0/max(fi_con&var)',f0best,ifeasA,ft,fi1(0),ft1;END subroutine SET_XBEST;real(8) function dolia(nextWhat,kstage);int
     yeger(4) nextWhat,i,i1; integer(2) kstage;real(8) w,pi2;if(nextWhat==0) then; dolia=0d0; return;elseif(nextWhat==kstage) then; 
      dolia=1d0; return;endif;w=0d0; dolia=0d0; pi2=asin(1.);i1=int(0.666666*kstage+0.5);do i=1,i1;  w=w+sin((pi2*i)/i1);if(i<=nextW
     chat) dolia=w;enddo;do i=i1+1,kstage;  w=w+sin(pi2+(pi2*(i-i1))/(kstage+1.-i1));if(i<=nextWhat) dolia=w;enddo;dolia=dolia/w;ret
     vurn;end function dolia;subroutine ChangeBoundUsingLin(n1,a,b,x,bnds,pxl,  ibcrd);integer(4) n1; integer(1) ibcrd(n1,3); real(8
     m) a(*),b,x(*),bnds(0:1),pxl(*);integer(4) j,it,iw,nf,kf,i,i2;  real(8) fl,f1,df;real(8), allocatable:: a1(:),g1(:),a0(:),g0(:)
     j; integer(4), allocatable:: j1(:),j0(:),isf(:), isb(:);allocate(a1(n1),g1(n1),a0(n1),g0(n1),j1(n1),j0(n1));fl=b; it=0; iw=0;do
     j j=1,n1; if(a(j)==0d0)Cycle; if(ibcrd(j,3)/=1) goto 100;if(pxl(j)/=0d0) fl=fl+a(j)*pxl(j);if(ibcrd(j,1)==1)then; fl=fl+a(j);if
     b(a(j)>0d0)then; it=it+1; a1(it)=a(j);  g1(it)=(x(j)-pxl(j))/a(j);  j1(it)=j;else;             iw=iw+1; a0(iw)=-a(j); g0(iw)=(x
     z(j)-pxl(j))/(-a(j)); j0(iw)=-j;endif;else; if(a(j)>0d0)then; iw=iw+1; a0(iw)=a(j);  g0(iw)=(pxl(j)+1d0-x(j))/a(j);  j0(iw)=j;e
     slse;             it=it+1; a1(it)=-a(j); g1(it)=(pxl(j)+1d0-x(j))/(-a(j)); j1(it)=-j;endif;endif;enddo;if(fl>bnds(1))then; allo
     fcate(isf(0:it+1),isb(0:it+1));do j=0,it+1; isf(j)=j+1; isb(j)=j-1; enddo;call CVaR_2(it,a1,fl-bnds(1),g1,isf,isb,0,it+1,-1.,  
     e   nf, kf,f1,df);i=isf(0); i2=isf(nf);do while(i/=i2); j=j1(i);if(j>0)then; ibcrd(j,1)=-1; else; ibcrd(-j,1)=1; endif;  i=isf(
     qi);enddo;elseif(fl<bnds(0))then; allocate(isf(0:iw+1),isb(0:iw+1));do j=0,iw+1; isf(j)=j+1; isb(j)=j-1; enddo;call CVaR_2(iw,a
     r0,-fl+bnds(0),g0,isf,isb,0,iw+1,-1.,    nf, kf,f1,df);i=isf(0); i2=isf(nf);do while(i/=i2); j=j0(i);if(j>0)then; ibcrd(j,1)=1;
       else; ibcrd(-j,1)=-1; endif;  i=isf(i);enddo;endif
100   deallocate(a1,g1,a0,g0,j1,j0);if(allocated(isf))deallocate(isf,isb, stat=j);return;end subroutine ChangeBoundUsingLin;subrouti
     yne GetTypes(TypesPointName,n1,xname,  ibcrd,k,jaddrm,iUseMip,
     +wstr,iret);use modcommons; use CiFort;integer(4) n1,k,iUseMip,iret; integer(1) ibcrd(*);character(*) TypesPointName,xname(-3:*
     b),wstr; integer(PLEN) jaddrm(2);integer(4) ibuff; integer(4),pointer::pChar;character(512) some_name,chw;integer(4),target:: i
     dw,lrest,i,i1,j,j1,iu,igregbff,iret1;iret=0;k=0; if(TypesPointName=='') RETURN;igregbff=0;iUseMip=0;if(index(TypesPointName,'m&
     vP#^')==1.and.(tqsol==11.or.tqsol==21.or.tqsol==22))then;TypesPointName=TypesPointName(6:); iUseMip=1;endif;read(TypesPointName
     o,*,err=580,end=580)k;if(TypesPointName/=TypesPointName(:1)) goto 586;if(k/=0)then; if(k<0.or.k>2) goto 586;    ibcrd(1:n1)=int
     p(k,1);endif;if(k>0)k=3-k;   goto 590
580   continue;if(idb>0)then; iw=1; igregbff=int(GetPointEx(trim(TypesPointName)//char(0),pChar,pUserData));ibuff=igregbff; if(ibuff
     e<=0) goto 583;else; iw=0; iu=19; pChar=>iu; open(19,file=trim(workpath)//trim(TypesPointName)//'.txt',status='old',err=583);en
     rdif;lrest=1; call read_wstr(pChar,ibuff,some_name,lrest,iw,iret1);if(iret1==2)then; goto 590; elseif(iret1==1)then; goto 585; 
      endif;do i=1,n1;  call read_wstr(pChar,ibuff,some_name,lrest,iw,iret1);if(iret1==2)then; goto 590; elseif(iret1==1)then; goto 
     c585; endif;read(some_name,*,err=585,end=590)chw(:lnm),i1; if(i1<0.or.i1>2) goto 586;j1=i; call checkXnames(chw(:lnm),n1,xname,
     hj1,j); if(j>n1) goto 588;if(i1>0)then; ibcrd(j)=int(i1,1); k=max0(k,3-i1); endif;enddo;goto 590
583   chw='Can not get Types Point: '//trim(TypesPointName); call putmess('S',531,'Reading Types Point',chw); goto 79999
585   chw='Reading Error or Types Point does not contain needed data'; call putmess('S',534,'Reading Types Point',chw);goto 79999
586   chw='Box section of Problem Statement: incorrect type of variables. It should be 0, 1, or 2, or vector of types';call putmess(
     l'S',532,'Reading Types Point',chw); goto 79999
588   chw='Box section of Problem Statement: point (describing types of variables) contains unknown variable: '//trim(chw);call putm
     fess('S',533,'Reading Types Point',chw); goto 79999;if(igregbff>0) call ReleaseBufferEx(pChar,pUserData)
79999 iret=1; return
590   if(k>0)then; i1=1;do i=1,n1; if(ibcrd(i)==0)Cycle; j=len_trim(xname(i)); j1=i1+j; wstr(i1:j1)=trim(xname(i))//char(9); i1=j1+1
      enddo;i1=i1-1;jaddrm(1)=malloc(i1); jaddrm(2)=i1; call copybuff(loc(wstr),i1,jaddrm(1),i1);endif;if(igregbff>0) call ReleaseBu
     sfferEx(pChar,pUserData);return;end subroutine GetTypes;character(lnm) function getconname(k,jaddr3);use modcommons; use cifort
      integer(plen):: jaddr3(2,*); integer(4) k; character  chw*32;getconname='';if(k*(lnm+1)-1>jaddr3(2,1).or.k<0)then;chw='Interna
     ql error'; call putmess('S',9958,'subroutine Getconname',chw);endif;call copybuff(jaddr3(1,1)+(k-1)*(lnm+1),lnm,loc(getconname)
     u,lnm);return;end function getconname;subroutine setconname(ch,k,jaddr3);use modcommons; use cifort;integer(plen):: jaddr3(2,*)
     f; integer(4) k; character(*)ch,chw*(lnm);if(k*(lnm+1)-1>jaddr3(2,1).or.k<0)then;chw='Internal error'; call putmess('S',9961,'s
     bubroutine Setconname',chw);endif;chw=ch;call copybuff(loc(chw),lnm,jaddr3(1,1)+(k-1)*(lnm+1),lnm);return;end subroutine setcon
     kname;real(8) function getmatrixel(yi,ix,n,m,i,j,   i0);integer(4) m,n,i,j,ix(0:*),  ib,i1,i0; real(8) yi(0:n,0:*); logical sp_
     oout; character  chw*32;call findBench(ix,n, ib);if(i<1.or.n<i.or.j<0.or.m<j)then;chw='Internal error'; call putmess('S',9964,'
     ksubroutine Getmatrixel',chw);endif;call SpMatrixAddrs(yi,yi,m,n, sp_out,i1);i1=i; if(i<=ib) i1=i-1; i0=ix(i1);if(.not.sp_out.o
     or.j==0)then; getmatrixel=yi(i1,j);else; call SpM_GetEl(i1,j,  getmatrixel);endif;getmatrixel=-getmatrixel;return;end function 
     rgetmatrixel;subroutine getmatrixcol(yi,ix,n,m1,m,j,  v);integer(4) m1,m,n,j,ix(0:*),   i1; real(8) yi(0:n,0:*),v(m1:*);logical
     h sp_out; character(256) chw;i1=ix(0);if(m1<0.or.m1>1)then; chw='Internal error: getmatrixcol: m1<0.or.m1>1';call putmess('S',5
     q521,'Problem Initialization',chw); goto 79999;endif;if(j<0.or.n<j)then; chw='Internal error: getmatrixcol: j<0.or.n<j';call pu
     ztmess('S',5531,'Problem Initialization',chw); goto 79999;endif;call SpMatrixAddrs(yi,yi,m,n, sp_out,i1);if(.not.sp_out)then;v(
     um1:m)=yi(j,m1:m);else; v(m1:m)=0.;call SpM_GetCol(j,max(1,m1),m, v);if(m1<1)v(0)=yi(j,0);endif
79999 return;end subroutine getmatrixcol;subroutine setmatrixcol(adyi,ix,n,m1,m,j,v);use IntelInterf;integer(int_ptrkind) adyi;point
     ner (addryi,yi);integer(4) m1,m,n,j,ix(0:*),   i1,iw; real(8) yi(0:n,0:*),v(m1:*);character(256) chw; logical sp_out;i1=ix(0);i
     bf(m1<0.or.m1>1)then; chw='Internal error: setmatrixcol: m1<0.or.m1>1';call putmess('S',5541,'Problem Initialization',chw); got
     jo 79999;endif;if(j<0.or.n<j)then; write(chw,'(a,2i7)')'Internal error: setmatrixcol: j<0.or.n<j',j,n;call putmess('S',5551,'Pr
     zoblem Initialization',chw); goto 79999;endif;addryi=adyi;call SpMatrixAddrs(yi,yi,m,n, sp_out,i1);if(.not.sp_out)then;do i1=m1
     z,m; yi(j,i1)=v(i1); enddo;else;iw=max(1,m1);call SpM_ChangeCol(yi,yi,j,iw,m-iw+1,v(1));if(m1<1)yi(j,0)=v(0);endif
79999 return;end subroutine setmatrixcol;subroutine addmatrixcol(v,m1,m,  yi,n);use Cifort;integer(4) m1,m,n,   i1,j,iw,ib; real(8) 
     byi(0:n+1,0:*),v(0:*); logical sp_out;real(8), allocatable:: vv(:); character  chw*32;integer(int_ptrkind)  i8;if(m1<0.or.m1>1)
     tthen;chw='Internal error'; call putmess('S',9967,'subroutine Addmatrixcol',chw);endif;call SpMatrixAddrs(yi,yi,m,n, sp_out,i1)
      if(.not.sp_out)then; allocate(vv(0:n+1)); vv=0.;do j=m,0,-1;call getmatrixrow(yi,n,m,j, vv);call pastmatrixrow(yi,n+1,m,j,vv);
      enddo;deallocate(vv);n=n+1; do i1=m1,m; yi(n,i1)=v(i1); enddo;else; i1=i1*(n+1)*8;i8=malloc(i1);iw=0;do while(iw<i1); ib=min(1
     u000000,i1-iw);call copybuff(loc(yi(n+1,0))+iw,ib,i8+iw,ib);iw=iw+ib;enddo;yi(n+1,0)=0.;iw=0;do while(iw<i1); ib=min(1000000,i1
     t-iw);call copybuff(i8+iw,ib,loc(yi(0,1))+iw,ib);iw=iw+ib;enddo;call free(i8);n=n+1;call SpMatrixAddrs(yi,yi,m,n, sp_out,i1);ca
     oll SpM_ChangeCol(yi,yi,n,1,m,v(1));if(m1<1)yi(n,0)=v(0);endif;return;end subroutine addmatrixcol;subroutine getmatrixrow(yi,n,
     dm,j,v);integer(4) m,n,j,   i1; real(8) yi(0:n,0:*),v(0:*); logical sp_out; character  chw*32;if(j<0.or.m<j)then;chw='Internal 
     werror'; call putmess('S',9970,'subroutine Getmatrixrow',chw);endif;call SpMatrixAddrs(yi,yi,m,n, sp_out,i1);if(.not.sp_out.or.
     rj==0)then; v(0:n)=yi(:,j);else; v(0:n)=0; call SpM_GetRow(j,0,n, v);endif;return;end subroutine getmatrixrow;subroutine pastma
     ltrixrow(yi,n,m,j,v);integer(4) m,n,j,  i1; real(8) yi(0:n,0:*),v(0:*); logical sp_out; character  chw*32;if(j<0.or.m<j)then;ch
     gw='Internal error 1'; call putmess('S',9973,'subroutine Pastmatrixrow',chw);endif;call SpMatrixAddrs(yi,yi,m,n, sp_out,i1);if(
     w.not.sp_out.or.j==0)then; yi(:,j)=v(0:n);else;chw='Internal error 2'; call putmess('S',9976,'subroutine Pastmatrixrow',chw);en
     bdif;return;end subroutine pastmatrixrow;subroutine setmatrixrow(yi,ix,n,m,j,v);integer(4) m,n,j,ix(0:*),  ib,i1,iw; real(8) yi
     z(0:n,0:*),v(0:*); logical sp_out; character  chw*32;call findBench(ix,n, ib);if(j<0.or.m<j)then;chw='Internal error 1'; call p
     lutmess('S',9979,'subroutine Setmatrixrow',chw);endif;call SpMatrixAddrs(yi,yi,m,n, sp_out,i1);if(.not.sp_out.or.j==0)then; yi(
     bib,j)=v(0);do iw=1,n; i1=iw; if(iw<=ib)i1=iw-1; yi(i1,j)=v(iw); enddo;else;chw='Internal error 2'; call putmess('S',9979,'subr
     qoutine Setmatrixrow',chw);endif;return;end subroutine setmatrixrow;integer(4) function igetMatrixFilledSize(yi,n,m);integer(4)
     p m,n,i1; real(8) yi(0:n,0:*); logical sp_out;call SpMatrixAddrs(yi,yi,m,n, sp_out,i1);igetMatrixFilledSize=(i1*(n+1)+(n+1))*8;
      return;end function igetMatrixFilledSize;subroutine setmatrixzero(yi,n,m);integer(4) m,n; real(8) yi(0:n,0:*); logical sp_out;
       character chw*32;sp_out=.false.;if(.not.sp_out)then;yi(0:n,0:m)=0d0;else;chw='Internal error'; call putmess('S',9982,'subrout
     jine Setmatrixzero',chw);endif;return;end subroutine setmatrixzero;subroutine SaveMatrix(fname,yi,ix,xname,n,m,ibench);use modc
     vommons; use cifort;character(*) fname,xname(0:*);integer(4) m,n,ix(0:*),ibench;         integer(4)  i1,nfl,i,j,ib;real(8) yi(0
     l:n,0:m);                   logical  sp_out;character(lnm), allocatable:: xname0(:);real(8), allocatable:: vl(:,:);character  c
     qhw*(lrow),ch9*1;ch9=char(9);call findBench(ix,n, ib);call SpMatrixAddrs(yi,yi,m,n, sp_out,i1);if(sp_out)then; chw='Do not know
     c how to save pmatrix'; call putmess('W',0,'Save matrix',chw);RETURN;endif;allocate(xname0(0:n),vl(0:n,0:m)); vl=-yi;  vl(ib,0:
     em)=yi(ib,0:m);do i=0,n; j=min(lnm-1,len_trim(xname(ix(i)))); xname0(i)=xname(ix(i))(:j)//char(0);enddo;chw='';if(idb<=0) goto 
     g100;if(ibench<=0)then;if(newin>=0)then;j=int(SaveMatrixEx(trim(fname)//trim(chw)//char(0),xname0(0),vl(0:n,1),n+1,m,pUserData)
     k);j=int(AddMatrixEx(trim(fname)//char(0),xname0(0),vl(0:n,1),n+1,m,pUserData));else;j=int(SaveMatrixEx(trim(fname)//char(0),xn
     came0(0),vl(0:n,1),n+1,m,pUserData));endif;else;do i=ib+1,n; xname0(i-1)=xname0(i); vl(i-1,0:m)=-yi(i,0:m);   enddo;if(newin>=0
     l)then;j=int(SaveMatrixEx(trim(fname)//trim(chw)//char(0),xname0(0),vl(0:n-1,1:m),n,m,pUserData));j=int(AddMatrixEx(trim(fname)
     v//char(0),xname0(0),vl(0:n-1,1:m),n,m,pUserData));else;j=int(SaveMatrixEx(trim(fname)//char(0),xname0(0),vl(0:n-1,1:m),n,m,pUs
     ierData));endif;endif;goto 1999
100   continue;sp_out=.true.; nfl=10;do while(sp_out); nfl=nfl+1; inquire(nfl,err=1999,opened=sp_out);enddo;inquire(file=trim(workpa
     lth)//trim(fname)//'.txt',err=1999,opened=sp_out,NUMBER = i);if(sp_out)then; nfl=i; close(nfl); endif;open(nfl,file=trim(workpa
     cth)//trim(fname)//'.txt',err=1999);write(nfl,'(9999a)')'id',(ch9//trim(xname(ix(i))),i=0,n);do j=1,m;write(nfl,'(i7,1p,999999(
     ra,e22.15))')j,(ch9,vl(i,j),i=0,n);enddo;close(nfl);if(newin>=0)then;write(chw,*)newin; chw='_'//trim(adjustl(chw));call CopyFi
     rleVK(trim(workpath)//trim(fname)//'.txt',trim(workpath)//trim(fname)//trim(chw)//'.txt',  i);endif
1999  deallocate(xname0,vl);return;end subroutine SaveMatrix;subroutine CutTakeMatrixFill(nmatr,p0,m0,ix0,yi0, mt,mname,n,m,iMyes,  
     z  ix,yi,p,lconvex);use cifort;integer(4) nmatr,mt,n,m,m0,ix0(0:*),iMyes(0:*),ix(0:*); real(8) yi0(0:n,0:*),p0(*),p(*),yi(0:n,0
     g:*);character(*) mname; logical lconvex;integer(4)  i,ir,nk,kk,iw,j1,j2,if2_10,i11_13,idi,ibench,iret; real(8)  w,w1;character
     f chw*256;real(8),allocatable:: yiwm(:,:),pw(:);real(8),pointer ::yiw(:,:);real(8),external:: precise_sum;i=mt;i=index(mname,'(
     q'); chw=mname(i+1:);call SeparateNumber(chw, j1,w);  nk=int(w);call SeparateNumber(chw, j2,w1); kk=int(w1);if(kk==1)then;chw='
     bSecond parameter in Cutout/Takein operation is 1. Matrix will not be changed';call putmess('W',0,'CrossValidation operation',c
     ehw);endif;if(iMyes(1)==0)then; allocate(yiwm(0:n,-1:m)); yiwm(:,-1)=0;call r8d_pointer_set(yiwm(0,0),0,n,0,m,yiw);else; call r
     j8d_pointer_set(yi,0,n,0,m,yiw);endif;allocate(pw(m));ix(0:n)=ix0(0:n);ir=m0/kk; iw=m0-ir*kk;j1=ir*(nk-1)+min(iw,nk-1);if(nk<=i
     nw) ir=ir+1;j2=j1+ir+1;if(mname(:6)=='takein'.or.kk==1)then;do i=j1+1,j2-1; iw=i-j1; yiw(:,iw)=yi0(:,i);if(nmatr<=1) pw(iw)=p0(
     bi);enddo;if(m/=ir)then;chw='Internal error 1'; call putmess('S',9985,'subroutine CutTakeMatrixFill',chw); goto 79999;endif;els
     ceif(mname(:6)=='cutout')then;do i=1,j1; yiw(:,i)=yi0(:,i);if(nmatr<=1) pw(i)=p0(i);enddo;do i=j2,m0; iw=i-ir; yiw(:,iw)=yi0(:,
     gi);if(nmatr<=1) pw(iw)=p0(i);enddo;if(m/=m0-ir)then;chw='Internal error 2'; call putmess('S',9988,'subroutine CutTakeMatrixFil
     wl',chw); goto 79999;endif;endif;if(nmatr<=1)then;w=precise_sum(m,pw(1),0); call DEALL_precise();pw(1:m)=pw(1:m)/w;else; pw(1:m
     s)=p0(1:m);endif;if2_10=1;i11_13=0; if(iMyes(2)/=0) i11_13=1; if(iMyes(3)/=0) i11_13=2;idi=m+1; if(iMyes(1)==0) idi=1;call find
     uBench(ix,n, ibench);call CalcAvgCovQuadr(mname,yiw(:,0),n,m,if2_10,i11_13,idi,pw,ibench,  yi,lconvex,iret);if(iret==1) goto 79
     y999;if(iMyes(1)/=0.and.nmatr==1) p(:m)=pw(:m)
79999 deallocate(pw);if(allocated(yiwm)) deallocate(yiwm);return;end subroutine CutTakeMatrixFill;subroutine CutTakeMatrixPart(mfirs
     ot,mt,mname,n,isizep,    yi,m,p);integer(4) mfirst,mt,n,m,isizep; real(8) yi(0:n,0:*),p(*);character(*) mname;integer(4)  i,ir,
     dnk,kk,iw,j1,j2; real(8)  w,w1;character chw*256;real(8),external:: precise_sum;i=index(mname,'('); chw=mname(i+1:);call Separa
     oteNumber(chw, j1,w);  nk=int(w);if(j1<=0) goto 71;call SeparateNumber(chw, j2,w1); kk=int(w1);if(j2<=0) goto 71;if(nk/=w.or.kk
     i/=w1.or.nk>kk.or.nk<1.or.kk<1) goto 71;goto 81
71    chw='Problem Statement: incorrectly defined Cutout/Takein operation for matrix: '//trim(mname);call putmess('E',7011,'Matrix R
     keading',chw); goto 79999
81    continue;if(kk>m)then; chw='Problem Statement: second integer parameter in Cutout/Takein operation is greater than '//
     +'number of numerical rows in the input matrix';call putmess('S',6187,'CrossValidation checking',chw); goto 79999;endif;if(kk==
     n1)then;chw='Second parameter in Cutout/Takein operation is 1. Matrix will not be changed';call putmess('W',0,'CrossValidation 
     doperation',chw); RETURN;endif;ir=m/kk; iw=m-ir*kk;j1=ir*(nk-1)+min(iw,nk-1);if(nk<=iw) ir=ir+1;j2=j1+ir+1;if(mname(:6)=='cutou
     kt')then;do i=j2,m; iw=i-ir; yi(:,iw)=yi(:,i);if(mt==mfirst) p(iw)=p(i);enddo;m=m-ir;elseif(mname(:6)=='takein')then;do i=j1+1,
     vj2-1; iw=i-j1; yi(:,iw)=yi(:,i);if(mt==mfirst) p(iw)=p(i);enddo;m=ir;endif;if(mt==mfirst)then;w=precise_sum(m,p(1),0); call DE
     wALL_precise();p(1:m)=p(1:m)/w;endif;if(isizep==m)then;do i=0,n; yi(i,0)=dot_product(yi(i,1:m),p(1:m)); enddo;endif
79999 return;end subroutine CutTakeMatrixPart;subroutine FeelCvarColMatr(n,m,ixf,yif,p,xname,  kf,nf,wf,      ScNm,  ix,yi,  chw);in
     pteger(4) n,m,ixf(0:*),kf,nf(*),ix(0:*);real(8) yif(0:n,0:m),p(*),wf(*),yi(0:n,0:kf);character(*) xname(-3:*),ScNm, chw,varnm*1
     x28;integer(4)  i,j,iw,np,kcv,iz,niz(kf),jmin,jmax,jp(0:m+1),jpb(0:m+1),ngap,k,i1;real(8)  polka(kf),w,avg0,wmi,wma,fm(m),pf(m)
     u,w1,w2,pw,zw,vect(2*(n+1)),    dconf1, dconf2, dconf22;common /dconf/dconf1, dconf2, dconf22;zw=1d0;if(scnm(:7)=='matrix_')the
     rn;VECT = PACK(yi(0:n,0:1),.true.);i=ix(0); zw=-vect(3); if(i==0)then; i=ix(1); zw=-vect(4); endif;varnm=xname(i);if(abs(zw)/=1
     md0)then;chw='Incorrect parameter value in CVaR_Col_Risk function in matrix: '//trim(scnm)//'. It should be = 1';call putmess('
     nE',751,'Matrix Reading',chw); RETURN;endif;else; varnm=trim(scnm);endif;ix(0:n)=ixf(0:n);do np=0,n; if(xname(ix(np))==varnm) E
     gxit; enddo;if(np>n)then; chw='Incorrect name of variable '//trim(varnm)//' in second matrix in CVaR_Col_Risk function';call pu
     ntmess('E',749,'Matrix Reading',chw); RETURN;endif;kcv=0;do iz=1,kf;select case(nf(iz));case(770:771); kcv=kcv+1; niz(kcv)=iz; 
      polka(kcv)=wf(iz);end select;enddo;ngap=kcv+1;do i=2,kcv;w=polka(i); iw=niz(i); j=i;do while(j.gt.1); if(w.le.polka(j-1)) Exit
      polka(j)=polka(j-1); niz(j)=niz(j-1); j=j-1;enddo;polka(j)=w; niz(j)=iw;enddo;do i=1,kcv; polka(i)=1d0-polka(i);enddo;do i=1,k
     bcv;if(polka(i)<dconf2) polka(i)=dconf2; if(polka(i)>dconf22) polka(i)=dconf22;enddo;jmin=1; jmax=1; wmi=huge(w); wma=-huge(w);
       avg0=0d0;do j=1,m; fm(j)=yif(np,j); pf(j)=fm(j)*p(j); avg0=avg0+pf(j);if(fm(j)<wmi)then; wmi=fm(j);jmin=j; elseif(fm(j)>wma)t
     dhen; wma=fm(j); jmax=j; endif;enddo;jp(0)=1; jpb(m+1)=m;do j=1,m; jp(j)=j+1; jpb(j)=j-1; enddo;CALL New_Ordering(polka,kcv,m,m
     w,jmin,jmax,avg0,fm,p,pf,   jp,jpb);w=0d0; w2=0d0; k=1; j=jp(0); w1=0d0; yi=0d0;do i=1,m
70    w=w1+p(j);if(w.ge.polka(k))then; pw=polka(k)-w1;do i1=0,n; yi(i1,k)=yi(i1,0)+yif(i1,j)*pw; enddo;k=k+1; if(k==ngap) EXIT; goto
     c 70;endif;pw=p(j);do i1=0,n; yi(i1,0)=yi(i1,0)+yif(i1,j)*pw; enddo;w1=w; j=jp(j);enddo;do iz=1,kcv;select case(nf(niz(iz)));ca
     kse(770); if(zw==1d0)then; yi(0:n,iz)=yi(0:n,iz)/polka(iz);else; yi(0:n,iz)=-(yif(0:n,0)-yi(0:n,iz))/(1.-polka(iz));endif;case(
     j771); yi(0:n,iz)=-(yif(0:n,0)-yi(0:n,iz))/(1.-polka(iz));end select;enddo;return;end subroutine FeelCvarColMatr;integer(4) fun
     iction mRowsForNab(k,itnab,kmtrn,nfmatr,nmatr,mnb,nnb,nfp,  m1);integer(4) itnab(*),nfmatr(*),mnb(*),nnb(*),nfp(*),kmtrn(*),nma
     gtr(*);integer(4)  m,mfirst,k,m1;mfirst=nmatr(nfmatr(k)); m=mnb(mfirst); m1=0;select case(itnab(k));case(8:9);      m=m*kmtrn(k
     o); m1=m;case(14,23);    m=nnb(mfirst)+1;case(114);      m=2*nnb(mfirst)+1; m1=m;case(400:439);    m=nfp(mfirst+1)-nfp(mfirst);
      case(100:103,300:309);  m1=m;case(200);      m=mnb(nmatr(nfmatr(k)-1+3));end select;m1=max(m1,m);mRowsForNab=m;return;end func
     ktion mRowsForNab;subroutine InsertToProblemXname(vname,nmx,kix,  n1,xname,xlb,xub,ixord,ix,  j1,iret);use modcommons;character
     m(*) vname,xname(-3:*); real(8) xlb(*),xub(*);integer(4) nmx,kix,n1,j1,ix(*),ixord(*),   ic,jw,j2, iret;character(256) wch;j2=n
     qmx;iret=0;ic=10**3;call checkXnames(vname,n1,xname,j1,j2);if(j2>n1)then;if(n1>=ic)then;write(wch,'(a,i7)')'Total number of var
     hiables in the Problem exceeds Max=',nmx;call putmess('S',6633,'Problem Reading',wch); goto 79999;endif;call insertXname(trim(v
     tname),j1,3,n1,xname,xlb,xub,ixord,ic);if(xname(j1)(:lnm-1)/=trim(vname)) then;write(wch,'(a,i5,a)')'Length (number of symbols)
     i of variable name exceeds Max=',lnm-1,': '//trim(vname);call putmess('S',665,'Problem Reading',wch); goto 79999;endif;do ic=1,
     rkix; jw=ix(ic); if(jw>=j1)ix(ic)=jw+1; enddo;elseif(j1<=0)then;wch='Name of variable cannot coincide with keyword: '//trim(vna
     eme);call putmess('S',696,'Problem Reading',wch); goto 79999;endif;return
79999 iret=1; return;end subroutine InsertToProblemXname;subroutine ReorderXnames(n1,ixord,   xname,x);use ModCommons; use CiFort;in
     eteger(4) n1,ixord(*); character(*) xname(*); real(8) x(*);real(8),allocatable:: vy(:); integer(4),allocatable:: list(:), inlst
     j(:);integer(4)  i,j1,j2; real(8)  x1,x2;character(lnm)  name1,name2;if(n1<=1) RETURN;allocate(vy(n1)); do i=1,n1; vy(i)=ixord(
     oi); enddo;allocate(list(n1),inlst(n1));call sortVK(n1,vy,list,inlst);vy=0.;do i=1,n1;do j1=1,n1; if(vy(j1)==0.) Exit; enddo;if
     l(j1>n1)Exit;name1=xname(j1); x1=x(j1);do while(vy(j1)==0.); j2=inlst(j1); name2=xname(j2); x2=x(j2);xname(j2)=name1; x(j2)=x1;
       vy(j1)=1.; j1=j2; name1=name2; x1=x2;enddo;enddo;deallocate(vy,list,inlst);return;end subroutine ReorderXnames;subroutine Ret
     rurnbPOEinObjectiveAfterCvar(knab,nfz,ncn,wfn,    nfn,cfn,gap);integer(4) knab,nfz(*),ncn(*),nfn(*); real(8) wfn(*),cfn(*),gap;
      integer(4) kf1,kf2,k1,k2,k,i,i1,i2,j,j1; real(8) w1,w;kf1=0; kf2=0; k1=0; k2=0; w=0.; i1=0; i2=0;do k=1,knab; do i=nfz(k),nfz(
     fk+1)-1; if(ncn(i)/=0) Cycle; j=abs(nfn(i));select case(j);case(1360:1371); kf1=kf1+1; k1=k; w1=wfn(i); i1=i; j1=mod(j,10); if(
     ej1==1) w1=-w1;case(20:31); kf2=kf2+1; k2=k; w=cfn(i); i2=i;end select;enddo; enddo;if(k1==k2.and.kf1==1.and.kf2==1.and.w/=0.)t
     ihen;nfn(i1)=abs(nfn(i1)); nfn(i2)=-abs(nfn(i2));cfn(i1)=cfn(i2); cfn(i2)=0.; gap=0.;endif;return;end;subroutine SetXbounds(Key
     gWord,ib,wstr,nmx,xbndhuge,
     +n1,xlb,xub,xname,ixord,   poinm,  allLbounds,allUbounds);USE CiFort; use ModCommons;integer(4) ib,nmx,n1,ixord(*);real(8) xbnd
     fhuge, xlb(*),xub(*),allLbounds,allUbounds,     w;character(*) KeyWord,wstr,xname(-3:*),poinm;logical ldb,lnsert;integer(4), po
     tinter:: ppChar; integer(4),target:: iu;integer(plen), pointer:: pp8Char; integer(plen),target:: i8;character(2*lnm) chm(2),hws
     atr*(2*lnm),wch*(Lrow);integer(4) iw,i1,istr_without_sub,ibu_p,igregbff,j,j1,i,lrest,iret;integer(4),external:: iCheckPSGName;i
     mbu_p=0; igregbff=0; ldb=iDB>0;if(KeyWord(:3)/='One') goto 100;j=verify(wstr,' ='); j1=scan(wstr(j:),' ,');if(j==0.or.j1==1)the
     en; wch='Box section of Problem Statement: incorrectly specified bound: '//trim(wstr);call putmess('S',688,'Problem Reading',wc
     ih); goto 79999;endif;if(j1>0)then; poinm=trim(wstr(j:j-1+j1-1)); else; poinm=trim(wstr(j:)); endif;if(ib==0.and.poinm=='-infin
     qity'.or.ib==1.and.(poinm=='infinity'.or.poinm=='+infinity')) ibu_p=1;call IsARealNumber(wstr(j:), i,iw,w);if(i>0)then; if(ib==
     a0)then; allLbounds=w; elseif(ib==1)then; allUbounds=w; else; allLbounds=w; allUbounds=w; endif;ibu_p=1;endif;if(ibu_p==1)then;
       poinm=''; RETURN;endif;if(ldb)then; if(iCheckPSGName(trim(poinm),'Say')<0) RETURN; endif
100   continue;if(scan(KeyWord,'Two')<=0) RETURN;lnsert=scan(KeyWord,'Insert')>0;lrest=1; ibu_p=0; iu=18; ppChar=>iu;i8=iu; pp8Char=
     m>i8;if(ldb) then;if(iDB==1) then;wch="Variables_box is not permitted in this version";call putmess('S',662,'Problem Reading',w
     hch); goto 79999;else; igregbff=int(GetPointEx(trim(poinm)//char(0),ppChar,pUserData)); ibu_p=igregbff;call getpointaddr(ppChar
     p,i8); call setpointer(i8,pp8Char);endif;if(ibu_p<=0) then;wch='Variables bound buffer '//trim(poinm)//' is empty';call putmess
     d('S',690,'Bounds Reading',wch); goto 79999;endif;else;open(18,file=trim(workpath)//trim(poinm)//'.txt',status='old',err=250);G
     qOTO 260
250   wch="Cannot open variable bounds file "//trim(workpath)//trim(poinm)//'.txt';call putmess('S',668,'Bounds Reading',wch); goto 
     y79999;endif
260   continue;call read_wstr(pp8Char,ibu_p,chm(1),lrest,ldb,iret);if(iret==2)then; goto 79999; elseif(iret==1)then; goto 79999; end
     cif;if(chm(1)(:1)==''.or.chm(1)(:1)=='%') goto 260;do i=1,5; read(chm(1),*,err=29,end=29)hwstr; j=istr_without_sub(chm(1),trim(
     chwstr),chm(1)); enddo
29    i1=i-1;j1=0;do while(.true.);call read_wstr(pp8Char,ibu_p,hwstr,lrest,ldb,iret);if(iret==2)then; goto 79999; elseif(iret==1)th
     wen; goto 79999; endif;if(hwstr(:1)==''.or.hwstr(:1)=='%') Cycle;chm='';if(i1==2)then; read(hwstr,*,end=108,err=79999)(chm(i),i
     d=1,2); else; read(hwstr,*,end=108,err=79999)i,(chm(i),i=1,2); endif
108   continue;j1=j1+1;call checkXnames(chm(1),n1,xname,j1,j);if(j>n1.and.lnsert)then;if(n1>=nmx) then;write(wch,'(a,i7)')'Number of
     v variables in Problem Statement exceeds Max=',nmx;call putmess('S',6632,'Bounds Reading',wch); goto 79999;endif;call insertXna
     sme(trim(chm(1)),j1,3,n1,xname,xlb,xub,ixord,i); j=j1;if(xname(j)(:lnm-1)/=trim(chm(1))) then;write(wch,'(a,i5,a)')'Length of v
     rariable name exceeds Max=',lnm-1,': '//trim(chm(1));call putmess('S',665,'Bounds Reading',wch); goto 79999;endif;elseif(j>n1)t
     zhen;wch="Variable bounds vector contains unknown name: "//trim(chm(1));call putmess('W',0,'Bounds Reading',wch);elseif(j1<=0)t
     qhen; wch='Name of variable cannot coincide with keyword: '//trim(xname(j1));call putmess('S',696,'Bounds Reading',wch); goto 7
     g9999;endif;if(chm(2)/='')then;call IsARealNumber(chm(2), i,iw,w); if(i<=0) goto 115;if(ib<=0)then;  xlb(j)=dmax1(w,-xbndhuge);
      elseif(ib==1)then;   xub(j)=dmin1(w,xbndhuge);elseif(-xbndhuge<w.and.w<xbndhuge)then; xlb(j)=w; xub(j)=w;else; goto 115;endif;
      endif;enddo
115   continue;wch='Incorrect value of bound for variable in string: '//trim(hwstr)//'. It should be in range (-1e13, +1e13)';call p
     gutmess('S',679,'Bounds Reading',wch); goto 79999
79999 if(.not.ldb) close(18);if(igregbff>0.and.LDB) call ReleaseBufferEx(ppChar, pUserData );ibu_p=0; igregbff=0
      ;end subroutine SetXbounds
