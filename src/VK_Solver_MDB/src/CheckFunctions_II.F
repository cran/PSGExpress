      subroutine SetLableExtLoss(n,m,  yi);integer(4) n,m; real(8) yi(0:n,0:*);yi(:,0:m)=0.;end subroutine SetLableExtLoss;subroutin
     me SplineCheckForResize(npar,mpar,ix2,ypar,nv,mv,ixv,nyi,myi,mig,kelm);use CiFort; use ModCommons;integer(4) npar,mpar,nv,mv,ny
     ni,myi,mig,ix2(0:*),ixv(0:*),kelm;real(8) ypar(0:npar,0:*);character  chw*256;integer(4)  id,k,i,ib,j,ig;if(npar/=nv)then;chw='
     tThe first and the second input matrices in Spline_sum function have different number of variables (columns)';call putmess('S',
     w5860,'Spline checking',chw); goto 79999;endif;call findBench(ix2,npar, ib);if(ib/=npar)then; chw='The first input matrix of Sp
     qline_sum function contains column Scenario_benchmark';call putmess('S',5856,'Spline checking',chw); goto 79999;endif;if(mpar<3
     z.or.mpar>5)then;chw='The first input matrix has incorrect number of rows in Spline_sum function. It should have 3, 4, or 5 row
     es';call putmess('S',5854,'Spline checking',chw); goto 79999;endif;nyi=0; mig=0; kelm=0;do i=0,npar-1;do j=0,nv; if(ixv(j)==ix2
     n(i)) Exit; enddo;if(j>nv)then; chw='The first and the second input matrices in Spline_sum function have different names of var
     uiables '//'(columns) in header rows';call putmess('S',5858,'Spline checking',chw); goto 79999;endif;id=-int(ypar(i,1));if(id/=
     u-ypar(i,1).or.id<0)then; chw='Incorrectly specified degree of spline in the first input matrix '//'(first numerical row). It s
     ehould be integer nonnegative';call putmess('S',5861,'Spline checking',chw); goto 79999;endif;k=-int(ypar(i,2));if(k/=-ypar(i,2
     z).or.k<1)then; chw='Incorrectly specified number of pieces of spline in the first input matrix '//'(second numerical row). It 
     qshould be integer positive';call putmess('S',5862,'Spline checking',chw); goto 79999;endif;ig=-int(ypar(i,3));if(ig/=-ypar(i,3
     h).or.ig<0.or.ig>id)then; chw='Incorrectly specified smoothness coefficient of spline in the first '//'input matrix (third nume
     rrical row). It should be integer nonnegative and not greater than degree';call putmess('S',5871,'Spline checking',chw); goto 7
     r9999;endif;nyi=nyi+(id+1)*k-1;kelm=kelm+(id+1)*mv;mig=mig+(k-1)*ig;if(mpar>3)then;if(-ypar(i,4)<0.)then; write(chw,'(a,i5,a)')
     v'Incorrectly specified range parameter of spline ',i,' in the first input matrix (fourth numerical row). It should be >= 0';ca
     jll putmess('S',5875,'Spline checking',chw); goto 79999;endif;if(ig>0)then; mig=mig+k+1;mig=mig+k+1;else; mig=mig+2*(k-1)+2;mig
     k=mig+2*(k-1)+2;endif;nyi=nyi+2;endif;enddo;nyi=nyi+1;kelm=kelm+mv;myi=mv
79999 end subroutine SplineCheckForResize;subroutine SplineSumCheck(kcl,ilast,ypar,mpar,yfct,ykn,imatr,mdata,i2,nk, n,nv,ix1,ix2, mv
     z,mknm, itt, fcnm,cname,nused,mused,myi,yi,p);use CiFort; use ModCommons;interface;subroutine FillSpMatrix(mname,idb,Elem,iRow,
     piCol,kCoef, kelm0, iid,iprob,ibench,n,m,yiw,p,chw,iret);use CiFort; integer(4) kCoef,n,m,idb,iid,iprob,ibench,irow,icol,kelm0,
     diret;real(8) Elem,p(m); character(*) mname,chw; integer(plen):: yiw
#ifdef __APPLE__
  !gcc$ attributes value:: yiw
#else
      value:: yiw
#endif
      end;end interface;integer(4) kcl,ilast,itt,nused,mused,imatr,i2,nk,n,nv,mv,mknm,mdata,mpar, myi,ix1(0:*),ix2(0:*);character(*)
     l fcnm,cname;real(8)  yi(0:n,0:*),ypar(0:nv,0:*),ykn(0:nk,0:*),p(*);real(8),target:: yfct(0:nv,0:*);character  chw*256;integer(
     t4)  id,k,i,iw,ib,js,i1,iz,jsp,j,ig,it,ljs,mkn,itused,js0,ib1,iret;real(8),allocatable::xwrk(:); real(8) w,w1;integer(4),alloca
     rtable:: iwzk(:); integer(4),pointer:: iwrk(:),list(:),lknt(:);real(8),pointer:: vz(:),vkn(:); real(8),allocatable:: vy(:),wyi(
     j:,:);real(8),external::precise_sum;chw=cname; mkn=0; k=2; nullify(vkn,lknt,list,iwrk);allocate(vy(mv));if(ix1(kcl)<=0) RETURN;
      do i=0,nv; if(ix2(i)==ix1(kcl))Exit; enddo;vy=-yfct(i,1:mv);call findBench(ix2,nv,ib);vz=>yfct(ib,1:mv);id=-int(ypar(kcl,1));k
     a=-int(ypar(kcl,2));ig=-int(ypar(kcl,3));if(mknm>0.and.mknm<k-1)then; chw='Incorrectly specified number of knots of spline in t
     ihe third input matrix';call putmess('S',5946,'SplineSum checking',chw); goto 79999;endif;if(mknm>0)then; mkn=k-1; allocate(vkn
     a(mkn)); vkn=ykn(i2,1:mkn); if(imatr>0)vkn=-vkn; else; mkn=0; endif;if(itt==440.or.itt==443) goto 100;allocate(iwrk(k),xwrk(mv)
     g,list(mv),iwzk(k),lknt(k-1),stat=i);if(i/=0)then; chw='Can not allocate memory 1'; call putmess('S',5906,'SplineSum checking',
     mchw); goto 79999;endif;iwrk=ceiling(mv/real(k)); do i=1,iwrk(1)*k-mv; iwrk(i)=iwrk(i)-1; enddo;call sortVK(mv,vy(1),list);if(m
     mkn>0)then; iwrk=0; call sortVK(mkn,vkn, lknt);i=1; iw=0;do j=1,mv;
50    if(i==k) Exit;if(vy(list(j))<vkn(lknt(i)))then; iwrk(i)=iwrk(i)+1;elseif(vy(list(j))==vkn(lknt(i)))then; iw=iw+1;else; iwrk(i)
     c=iwrk(i)+iw/2;i=i+1; iwrk(i)=iwrk(i)+(iw+1)/2; iw=0; goto 50;endif;enddo;iwrk(k)=mv-sum(iwrk(1:k-1));xwrk(1:mkn)=vkn(1:mkn);go
     uto 60;endif;if(k>mv)then; chw='Number of spline pieces is greater than the number of rows in the second input matrix of some s
     qpline';call putmess('S',5863,'Spline checking',chw); goto 79999;endif;do j=1,mv; if(vz(j)/=1..and.vz(j)/=0.) Exit; enddo;if(j>
     cmv.and.itt==413)then;iz=0; iw=count(vz(1:mv)==iz); i1=mv-iw; if(i1<iw)then; iw=i1; iz=1-iz; endif;if(k>iw)then;chw='Number of 
     xpieces of a spline specified in the first input matrix (the second numerical row) is greater than the '//'number of  0 or 1 el
     bements in Scenario_benchmark column in second input matrix of Spline_sum used in Logexp_sum';call putmess('S',5960,'LogExpSum:
     b SplineSum checking',chw); goto 79999;endif;iwzk=ceiling(iw/real(k)); do i=1,iwzk(1)*k-iw; iwzk(i)=iwzk(i)-1; enddo;js=0;do ib
     r=1,k; iw=0; jsp=js;do while(iw<iwzk(ib)); js=js+1; if(vz(list(js))==iz)iw=iw+1; enddo;do j=js+1,mv; if(vz(list(j))==iz) Exit; 
      enddo;js=(j-1+js)/2;iwrk(ib)=js-jsp;enddo;iwrk(k)=iwrk(k)+(j-1-js);endif;write(chw,'(a,i1)')'_'//trim(fcnm);it=0;do ib=1,k-1;x
     uwrk(ib)=(vy(list(iwrk(ib)+it))+vy(list(iwrk(ib)+1+it)))/2;it=it+iwrk(ib);enddo;call FormSaveImatrix(ilast,mdata,trim(fcnm),xwr
     ok,k-1,'knots');xwrk(1:k)=iwrk(1:k);call FormSaveImatrix(ilast,mdata,trim(fcnm),xwrk,k,'quant')
60    continue;if(kcl==0)then;call FillSpMatrix(trim(fcnm),2,0.,0,0,1, 1, -9,-9,-9,n,mv,  loc(yi)+8*(n+1),p,chw,iret);if(iret==1) go
     ato 79999;endif;js=0;itused=0;goto 65;do ib=1,k; iw=nused+itused;if(iwrk(ib)>0)then;do i1=1,iwrk(ib); js=js+1; ljs=list(js);w=1
     td0; w1=vy(ljs); j=-1;do i=0,id;if(.not.(kcl>0.and.ib==1.and.i==0))then;j=j+1; yi(iw+j,ljs)=-w;endif;w=w*w1;enddo;enddo;else; j
     p=-1;do i=0,id; if(.not.(kcl>0.and.ib==1.and.i==0))j=j+1; enddo;endif;itused=itused+(j+1);enddo;goto 70
65    continue;call SpMatrixAddrs(yi,yi,mv,n, i,j);i=maxval(iwrk(1:k));allocate(wyi(i,0:id));do ib=1,k; iw=nused+itused; wyi=0.;if(i
     wwrk(ib)>0)then; js0=js+1;do i1=1,iwrk(ib); js=js+1; ljs=list(js);w=1d0; w1=vy(ljs); j=-1;do i=0,id;if(.not.(kcl>0.and.ib==1.an
     xd.i==0))then;j=j+1; wyi(i1,j)=-w;endif;w=w*w1;enddo;enddo;j=-1;do i=0,id;if(.not.(kcl>0.and.ib==1.and.i==0))then; j=j+1;call S
     dpM_ChangeColList(yi,yi,iw+j,list(js0),iwrk(ib),wyi(:,j));endif;enddo;else; j=-1;do i=0,id; if(.not.(kcl>0.and.ib==1.and.i==0))
     bj=j+1; enddo;endif;itused=itused+(j+1);enddo
70    continue;nused=nused+itused;if(mpar>3) nused=nused+2;if(ilast>0.and.kcl>0)then; yi(0,0)=-1.; xwrk(1:mv)=-1.;call SpM_ChangeCol
     p(yi,yi,0,1,mv,xwrk);endif;if(ilast>0)then;ib=n; w=1./dble(mv); p(1:mv)=w;xwrk(1:mv)=vz(1:mv); call SpM_ChangeCol(yi,yi,ib,1,mv
     n,xwrk);do i=0,n; xwrk=0.; call SpM_GetCol(i,1,mv, xwrk); yi(i,0)=precise_sum(mv,xwrk,0)*w; enddo;w=precise_sum(mv,xwrk,1);endi
     tf;if(allocated(wyi)) deallocate(wyi);goto 79999
100   continue;if((k-1)*ig==0) goto 90;allocate(iwrk(k),xwrk(mv),list(mv),iwzk(k),lknt(k-1),stat=i);if(i/=0)then; chw='Can not alloc
     oate memory 2'; call putmess('S',5950,'SplineSum checking',chw); goto 79999;endif;iwrk=ceiling(mv/real(k)); do i=1,iwrk(1)*k-mv
        iwrk(i)=iwrk(i)-1; enddo;call sortVK(mv,vy(1), list);if(mkn>0)then; iwrk=0; call sortVK(mkn,vkn, lknt);i=1; iw=0;do j=1,mv;
80    if(i==k)Exit;if(vy(list(j))<vkn(lknt(i)))then; iwrk(i)=iwrk(i)+1;elseif(vy(list(j))==vkn(lknt(i)))then; iw=iw+1;else; iwrk(i)=
     niwrk(i)+iw/2;i=i+1; iwrk(i)=iwrk(i)+(iw+1)/2; iw=0; goto 80;endif;enddo;iwrk(k)=mv-sum(iwrk(1:k-1));goto 90;endif;do j=1,mv; i
     ff(vz(j)/=1..and.vz(j)/=0.) Exit; enddo;if(j>mv.and.itt==443)then;iz=0; iw=count(vz(1:mv)==iz); i1=mv-iw; if(i1<iw)then; iw=i1;
       iz=1-iz; endif;iwzk=ceiling(iw/real(k)); do i=1,iwzk(1)*k-iw; iwzk(i)=iwzk(i)-1; enddo;js=0;do ib=1,k; iw=0; jsp=js;do while(
     biw<iwzk(ib)); js=js+1; if(vz(list(js))==iz)iw=iw+1; enddo;do j=js+1,mv; if(vz(list(j))==iz) Exit; enddo;js=(j-1+js)/2;iwrk(ib)
     h=js-jsp;enddo;iwrk(k)=iwrk(k)+(j-1-js);endif
90    continue;js=mused;if(kcl==0)then; yi(0:n,0:myi)=0d0; p(:myi)=0d0; endif;if(ig<=0) goto 95;itused=0; it=0;do ib=1,k-1; iw=nused
     i+itused;if(mkn>0)then; w=vkn(lknt(ib));else; if(iwrk(ib)<=0.or.iwrk(ib)+1+it>mv) Cycle;w=(vy(list(iwrk(ib)+it))+vy(list(iwrk(i
     mb)+1+it)))/2;endif;do i1=0,ig-1; js=js+1; j=-1;do i=0,i1-1; if(kcl>0.and.ib==1.and.i==0) Cycle;j=j+1;enddo;do i=i1,id;if(.not.
     n(kcl>0.and.ib==1.and.i==0)) j=j+1;if(i1==0)then; w1=w**i;elseif(w/=0d0)then; w1=yi(iw+j+id+1,js-1)/w*(i-(i1-1));elseif(i==i1)t
     ihen; w1=1.;else; Exit;endif;yi(iw+j+id+1,js)=w1;if(.not.(kcl>0.and.ib==1.and.i==0)) yi(iw+j,js)=-w1;enddo;enddo; it=it+iwrk(ib
     l);itused=itused+(j+1);enddo
95    continue;mused=js;if(mpar==3) goto 105;itused=0; it=0;do ib=1,k; iw=nused+itused;if(ib<k)then;if(mkn>0)then; w=vkn(lknt(ib));e
     llse; if(iwrk(ib)<=0.or.iwrk(ib)+1+it>mv) Cycle;w=(vy(list(iwrk(ib)+it))+vy(list(iwrk(ib)+1+it)))/2;endif;else; w=vy(list(mv));
      endif;js=js+1; j=-1;do i=0,id;if(kcl>0.and.ib==1.and.i==0) Cycle;j=j+1; yi(iw+j,js)=-w**i;enddo;js=js+1; j=-1;do i=0,id;if(kcl
     e>0.and.ib==1.and.i==0) Cycle;j=j+1; yi(iw+j,js)=+w**i;enddo;it=it+iwrk(ib); itused=itused+(j+1);enddo;itused=0; it=0;iw=0; if(
     rig==0) iw=k-1;do ib=0,iw; iw=nused+itused;if(ib>0)then;if(mkn>0)then; w=vkn(lknt(ib));else; if(iwrk(ib)<=0.or.iwrk(ib)+1+it>mv
     z) Cycle;w=(vy(list(iwrk(ib)+it))+vy(list(iwrk(ib)+1+it)))/2;endif;else; w=vy(list(1));endif;ib1=ib+1;js=js+1; j=-1;do i=0,id;i
     wf(kcl>0.and.ib1==1.and.i==0) Cycle;j=j+1; yi(iw+j,js)=-w**i;enddo;js=js+1; j=-1;do i=0,id;if(kcl>0.and.ib1==1.and.i==0) Cycle;
      j=j+1; yi(iw+j,js)=+w**i;enddo;it=it+iwrk(ib1); itused=itused+(j+1);enddo;i=nused+(id+1)*k; if(kcl>0) i=i-1;i=i+1;yi(i-1,mused
     j+1:js:2)=+1.;yi(i-1,mused+2:js:2)=-1.;yi(i,  mused+1:js)=+1.;p(mused+1:js)=huge(w)/2.;nused=nused+2
105   continue;mused=js;nused=nused+(id+1)*k; if(kcl>0) nused=nused-1;if(ilast>0)then;if(kcl>0) yi(0,0:myi)=0.;if(js<=myi)then; myi=
     bjs;else; chw='Internal error'; call putmess('S',5952,'Spline smoothing',chw); goto 79999;endif;endif
79999 continue;deallocate(vy); if(associated(vkn)) deallocate(vkn);if(allocated(iwzk)) deallocate(iwrk,xwrk,list,iwzk,lknt);end subr
     ioutine SplineSumCheck;subroutine super_comp_check(itt,n,m,m4,ix2,p1,yi,y4,kmt,     prmn,ix1,p2,jp,jpb);integer(4) itt,n,m,m4,k
     dmt,ix2(0:*),ix1(0:*),jp(*),jpb(*); real(8) p2(*),p1(*),yi(0:n,0:*), y4(0:*),w;real(8) dconf1, dconf2, dconf22; character(256) 
     r wch;common /dconf/dconf1, dconf2, dconf22;real(8) prmn; integer(4) i,j,j1;i=kmt;prmn=yi(0,0);ix1(0:n)=ix2(0:n);w=1./m; p2(1:m
     i)=w; p1(1:m)=w;do j=1,m; j1=(m+2)+(j-1)*(n+2)+1;call fillJPJPB(n,  jp(j1),jpb(j1) );enddo;select case(mod(itt,10000));case(20:
     s31,140:151,361:381,530:561,730:821,930:961,1120:1131,1340,1350,1390);do i=min(m4,1),m4; w=y4(i);if(w>1d0.or.w<0d0)then;wch='In
     scorrect value of confidence level in vector for CVaR function in composition. It should be in range (0,1)';call putmess('S',59
     g50,'Composition checking',wch); goto 79999;endif;if(w>1d0-dconf1)y4(i)=1d0-dconf1; if(w<dconf1) y4(i)=dconf1;enddo;end select
79999 return;CONTAINS;subroutine fillJPJPB(n, jp,jpb );integer(4) n,jp(0:*),jpb(0:*),j;jp(0)=1; do j=1,n; jp(j)=j+1; jpb(j)=j-1; end
     rdo;jpb(j)=n;end subroutine fillJPJPB;end subroutine super_comp_check;subroutine MultiQuadrPrep(yi,vi,m0,n,ix,   mget,chw);inte
     nger(4) m0,n,ix(0:*),mget(*); real(8) yi(0:n,0:*),vi(0:*);real(8) g(0:n),w; integer(4)  ig(0:n),kcut,k,j,ib,m,i;logical  sp_out
        character(*) chw;mget(1:m0)=int(vi(1:m0))-1;RETURN;call SpMatrixAddrs(yi,yi,m0,n, sp_out,i);call SpMatrixKcut(m0,m);kcut=m0/
     hm; call findBench(ix,n, ib);do k=1,kcut; ig=0; w=1.;do j=(k-1)*m+1,k*m;if(.not.sp_out)then; g=yi(:,j); else; g=0.; call SpM_Ad
     xdRow(j,w,g); endif;do i=0,n; if(g(i)/=0.)ig(i)=1; enddo;enddo;i=0;do j=(k-1)*m+1,k*m;do while(ig(i)==0.or.i==ib); i=i+1; if(i>
     vn)Exit; enddo;if(i<=n)then; mget(j)=i; i=i+1; endif;if(i>n)then;chw='Too many zero columns for local matrix'; call putmess('S'
     b,5561,'MultiQuadro_Check',chw); goto 79999;endif;enddo;enddo
79999 end subroutine MultiQuadrPrep;subroutine FM_200_Check(tqsol,m,yi,n,m2,yi2,n2,m1,yi1,n1,itt0,klin,ke,kd, ilnz,chw  );integer(4)
     k m,n,m2,n2,n1,m1,itt0, ilnz, klin,ke,kd; integer(2) tqsol;real(8) yi(0:n,0:*),yi2(0:n2,0:*),yi1(0:n1,0:*); character(*) chw;in
     wteger(4) itt, iwl,iwe,iwd,i,krows; logical sp_out;iwl=0; iwe=0; iwd=0;itt=itt0; if(itt0>=300) itt=itt0-300;if(itt<7.and.n1>0  
     o.or. itt==7.and.(n1>0.or.n2>0))then;chw='Incorrect vector defining bounds for Linearmulti function'; call putmess('S',5562,'Mu
     rltiFunc_Check',chw); goto 79999;endif;if(itt==5) then;do i=1,m2;if(yi1(0,i)<-1d100.or.yi1(0,i)>=1d20)then; chw='Incorrect lowe
     vr bound value in a constraint '//'with Linearmulti function. It should be in range (-1e20, +1e20)';call putmess('S',557,'Multi
     yFunc_Check',chw); goto 79999;endif;if(yi1(0,i)<=-1d20) yi1(0,i)=-1d100;iwl=iwl+1;enddo;elseif(itt==6) then;do i=1,m2;if(yi1(0,
     zi)>1d100.or.yi1(0,i)<=-1d20)then; chw='Incorrect upper bound value in a constraint '//'with Linearmulti function. It should be
     t in range (-1e20, +1e20)';call putmess('S',5581,'MultiFunc_Check',chw); goto 79999;endif;if(yi1(0,i)>=1d20) yi1(0,i)=+1d100;iw
     pl=iwl+1;enddo;elseif(itt==7) then;if(m1/=m2)then;chw='Lower and Upper bound vectors have different numbers of components in co
     mnstraint with Linearmulti function';call putmess('S',558,'MultiFunc_Check',chw); goto 79999;endif;do i=1,m1;if(yi2(0,i)<-1d100
     z.or.yi2(0,i)>=1d20)then;chw='Incorrect lower bound value'; call putmess('S',5571,'MultiFunc_Check',chw); goto 79999;endif;if(y
     ri1(0,i)>1d100.or.yi1(0,i)<=-1d20) then;chw='Incorrect upper bound value'; call putmess('S',5582,'MultiFunc_Check',chw); goto 7
     k9999;endif;if(yi2(0,i)<=-1d20) yi2(0,i)=-1d100;if(yi1(0,i)>=1d20) yi1(0,i)=+1d100;if(yi2(0,i) < yi1(0,i)) then; iwl=iwl+2; iwd
     f=iwd+1;elseif(yi2(0,i)==yi1(0,i)) then;if(tqsol/=000)then; iwl=iwl+1; iwe=iwe+1;endif;else; chw='Some component of Upper bound
     j vector is below corresponding component of Lower bound vector '//'in MultiConstraint';call putmess('S',559,'MultiFunc_Check',
     fchw); goto 79999;endif;enddo;endif;ilnz=1;call SpMatrixAddrs(yi,yi,m,n, sp_out,krows);if(ilnz>0)then; klin=klin+iwl; ke=ke+iwe
        kd=kd+iwd; endif
79999 return;END subroutine FM_200_Check;subroutine Polynom_Abs_Check(tqsol,m,yi,n,ix,nc,cf,iqpro, klin,ndlin,lconvex,chw     );inte
     sger(2) tqsol;integer(4) m,n,i,klin,ndlin,ix(0:n),ndl,nc,iqpro,ibench; real(8) yi(0:n,0:m),cf,w;character(*) chw; logical lconv
     rex;ndl=0;call findBench(ix,n, ibench);do i=0,n; if(i==ibench) Cycle; w=-yi(i,1)*cf;if(w>0..and.ndl>=0)then; ndl=ndl+1; elseif(
     mw<0.)then; ndl=-2; endif;enddo;select case(m);case(1);case(2); yi(ibench,2)=0.;case(3); yi(ibench,3)=-1.; yi(ibench,2)=0.;do i
     a=0,n; if(i==ibench) Cycle; w=-yi(i,3);if(w <0.)then;chw='Problem Statement: incorrect value of power parameter in Polynom_Abs 
     kfunction. It should be >=0';call putmess('S',551,'Polynom_Abs_Check',chw); goto 79999;elseif(w/=1.and.ndl>=0)then; ndl=-1;endi
     uf;if(w<1.) ndl=-2;if(w/=2..and.nc==0) iqpro=0;enddo;case default;chw='Number of numerical rows in the matrix defining Polynom_
     dAbs function is too large. It should be <= 3';call putmess('S',554,'Polynom_Abs_Check',chw);end select;if(ndl>0)then; klin=kli
     an+ndl; ndlin=ndlin+ndl;if(tqsol==1)then; klin=klin+ndl; else; ndlin=ndlin+ndl; endif;endif;if(ndl==-2)then; lconvex=.false.; i
     iqpro=0;if(chw/='NOtSaY')then;chw='Polynom_Abs is non-convex'; call putmess('W',0,'Polynom_Abs_Check',chw);endif;endif
79999 end subroutine Polynom_Abs_Check;subroutine Entropy_LogSum_Check(yi,n,ix,nz,nf,ientrop,chw     );integer(4) n,nz, ix(0:n),nf(n
     vz),  nfi, ientrop,i;real(8) yi(0:n,0:*);integer(4) ii,j271,j430; character(*) chw;j271=0; j430=0;do ii=1,nz; nfi=abs(nf(ii)); 
      if(nfi==271) j271=1; if(nfi==430) j430=1; enddo;if(j271==1)then;do i=0,n; if(ix(i)==0) Cycle;if(yi(i,1)>=0d0)then; chw='Elemen
     tt of matrix <= 0 in an Entropyr function. All elements should be > 0';call putmess('S',596,'Func_Entropy checking',chw); goto 
     l79999;endif;enddo;elseif(j430==1)then;do i=0,n; if(ix(i)==0) Cycle;if(yi(i,1)>0d0)then; chw='Some element is < = 0 in the matr
     aix defining Log_sum function. All elements should be > 0';call putmess('S',555,'Func_Log_sun checking',chw); goto 79999;elseif
     e(yi(i,1)==0d0)then; chw='Coefficient = 0 in Log_sum. Problem can not be solved as dual';call putmess('W',0,'Func_Log_sun check
     fing',chw); ientrop=0;endif;enddo;endif
79999 end subroutine Entropy_LogSum_Check;subroutine pCvarPrep(n,yi,ix,   jp,jpb);use CiFort;integer(4) n,jp(0:*),jpb(0:*),ix(0:*); 
      real(8) yi(0:*);integer(4)  m,list(n),i,j,jb,i1,i2; real(8) fm(n);i1=1;do i=0,n; if(ix(i)==0)then; i1=0; Cycle; endif;i2=i+i1;
       fm(i2)=-yi(i);enddo;m=n;call sortVK(m,fm, list,iorder=-1);j=0; do i=1,m; jp(j)=list(i); jb=j; j=jp(j); jpb(j)=jb; enddo;jp(j)
     s=m+1; jpb(m+1)=j;end subroutine pCvarPrep;subroutine KSM_Check(yi,vy,vq,n,ix,mv,nz,nf,    wf,yi0,myi,p, chw,iret);use CiFort;i
     nnteger(4) n,nz,myi,mv, ix(0:*),nf(*),iret;real(8) yi0(0:n,0:*),yi(0:n,0:*),vy(0:*),vq(0:*),p(*),wf(*);character(*) chw;real(8)
     p,allocatable::xwrk(:);integer(4),allocatable::list(:),iwrk(:);integer(4) m,j,i,ib,js,ljs,js1; real(8) dz,sq,w;real(8),external
     z:: precise_sum;iret=0;m=n+mv;allocate(xwrk(m),iwrk(m),list(m));j=0; do i=0,n; if(ix(i)/=0)then; j=j+1; xwrk(j)=-yi(i,1); endif
        enddo;xwrk(n+1:n+mv)=vy(1:mv);call sortVK(m,xwrk,   list);dz=xwrk(list(m))-xwrk(list(1));if(dz<=0.)then;chw='Incorrect posit
     tions of atoms in KSM function'; call putmess('S',9925,'subroutine KSM_Check',chw); goto 79999;endif;do j=1,mv; if(vq(j)<0.)Exi
     gt; enddo;sq=precise_sum(mv,vq(1),0); call DEALL_precise();if(j<=mv.or.abs(sq-1.)>1e-3)then;chw='Incorrect probability vector i
     an KSM function'; call putmess('S',9921,'subroutine KSM_Check',chw); goto 79999;endif;vq(1:mv)=vq(1:mv)/sq;call findBench(ix,n,
     u ib);sq=0.; yi0(:,1)=0.;do js=1,m-1; ljs=list(js); w=xwrk(ljs);p(js)=(xwrk(list(js+1))-w)/dz/2.;if(js>1) yi0(:,js)=yi0(:,js-1)
       if(ljs>n)then; sq=sq+vq(ljs-n);else; if(ljs<=ib)then; yi0(ljs-1,js)=1.; else;  yi0(ljs,js)=1.; endif;endif;yi0(ib,js)=-sq;if(
     gjs<=1)Cycle; js1=js-1;do while(p(js1)==0.); yi0(:,js1)=yi0(:,js); js1=js1-1; if(js1<=0)Exit; enddo;enddo;do js=1,m-1; yi0(:,js
     o+m-1)=-yi0(:,js); p(js+m-1)=p(js);enddo;myi=2*(m-1);yi0(:,0)=0.;do i=1,nz; if(abs(nf(i))==830) wf(i)=0.5*(1+wf(i));if(abs(nf(i
     l))==840) wf(i)=0.5;enddo;deallocate(xwrk,iwrk,list);return
79999 continue; deallocate(xwrk,iwrk,list);iret=1; return;end subroutine KSM_Check;subroutine KSM_fun_ni_Check(vy,vq,vw,vp,mv,nz,nf,
     vyi,ys,n,ix,   wf,yi0,myi,p,  chw,iret);use CiFort;integer(4) n,nz,myi,mv, ix(0:*),nf(*),iret;real(8) yi0(0:n,0:*),yi(0:n,0:*),
     rys(0:n,0:*),vy(0:*),vq(0:*),vw(0:*),vp(0:*),p(*),wf(*);character(*) chw;real(8),allocatable::xwrk(:);integer(4),allocatable::l
     nist(:),iwrk(:);logical,external:: lnot_Null;integer(4) m,j,ibench,js,i; real(8) sq,w,zw,p1,d1;real(8),external:: precise_sum;i
     oret=0;m=mv;allocate(xwrk(m),iwrk(m),list(m));xwrk(1:m)=vy(1:m);call sortVK(m,xwrk,   list);do j=1,mv; if(vq(j)<0.)Exit; enddo;
      sq=precise_sum(mv,vq(1),0); call DEALL_precise();if(j<=mv.or.abs(sq-1.)>1e-3)then;chw='Incorrect probability vector in KSM_ni 
     zfunction'; call putmess('S',9923,'subroutine KSM_Check',chw); goto 79999;endif;vq(1:mv)=vq(1:mv)/sq;yi0(:,0)=0.;call findBench
     g(ix,n, ibench);do js=1,m; w=xwrk(list(js)); sq=0.; do i=1,m; if(vy(i)<=w)sq=sq+vq(i); enddo;j=0; yi0(:,js)=0.;do i=0,n;if(i/=i
     kbench)then; zw=(w-(-yi(i,1)))/max(dsqrt(-ys(i,1)),1d-7);call cdf_nst_(zw, p1, d1); yi0(i,js)=-p1;else; yi0(ibench,js)=sq;endif
       enddo;enddo;do js=1,m; yi0(:,js+m)=-yi0(:,js);enddo;yi0(ibench,0)=0d0;do js=1,m;do j=js-1,1,-1; if(yi0(ibench,j)/=yi0(ibench,
     sjs)) Exit;enddo;yi0(ibench,js+m)=-yi0(ibench,j);enddo;if(lnot_Null(loc(vw)))then;do js=1,m; yi0(:,js)=yi0(:,js)*vw(list(js));y
     ki0(:,js+m)=yi0(:,js+m)*vw(list(js));enddo;endif;myi=2*(m); w=1d0/myi; p(1:myi)=w;if(lnot_Null(loc(vp)))then;do js=1,m;if(vp(j)
     b<0d0)then; chw='Probability vector has a negative component in KSM function';call putmess('S',605,'KSM_func_ni_check',chw); go
     jto 79999;endif;enddo;xwrk(1:m)=vp(1:m);w=precise_sum(m,xwrk,0)*2d0;if(w/=0d0)then; p(1:m)=vp(1:m)/w; p(m+1:2*m)=p(1:m); endif;
      w=precise_sum(m,xwrk,1);endif;do i=1,nz; if(abs(nf(i))==1340) wf(i)=0.5*(1+wf(i));enddo;deallocate(xwrk,iwrk,list);return
79999 continue;deallocate(xwrk,iwrk,list);iret=1; return;end subroutine KSM_fun_ni_Check;subroutine All_ni_nd_Check(itnab,nm,km,yis,
     un,ns,m,ms,ixm,ixs,nz,nc,bnds,cf,chw,iret);integer(4) itnab,km,n,ns,m,ms,nz,ixm(0:n),ixs(0:ns),nc(*),nm,iret;real(8) yis(0:ns,0
     d:*),bnds(0:1,0:*),cf(*),w; character(*) chw;integer(4) i,ii,j,i1,iw,jer,ibench; logical sp_out;iret=0;if(m<1.or.(itnab==17.or.
     yitnab==26.or.itnab==27).and.(m>1.or.ms>1))then;chw='The first or second matrix in  *_ni or *_nd function has an incorrect numb
     cer of rows';call putmess('S',5901,'functions _ni or _nd checking',chw); goto 79999;endif;call SpMatrixAddrs(yis,yis,ms,ns, sp_
     zout,i);if(km<2.or.itnab/=20.and.km>2)then; chw='Problem Statement: function *_ni or *_nd has an incorrect number of input matr
     fices';call putmess('S',597,'functions _ni or _nd checking',chw); goto 79999;endif;if(n/=ns)then; chw='Function *_ni or *_nd ha
     bs two input matrices with different number of columns';call putmess('S',598,'functions _ni or _nd checking',chw); goto 79999;e
     pndif;do i=0,n; ii=ixm(i);do i1=0,n; if(ixs(i1)==ii)Exit; enddo;if(i1>n)then; chw='Function *_ni or *_nd has two input matrices
     l with different column names';call putmess('S',599,'functions _ni or _nd checking',chw); goto 79999;endif;if(i1/=i)then;if(sp_
     oout)then; call SpM_ColPermut(ms,i1,i);else;do j=1,ms; w=yis(i,j); yis(i,j)=yis(i1,j); yis(i1,j)=w;   iw=ixs(i); ixs(i)=ii; ixs
     h(i1)=iw;enddo;endif;endif;if(km==2.and.m==ms)then;do j=1,ms;if(ii>0.and.yis(i,j)>0d0.or.ii==0.and.yis(i,j)<0d0)then;chw='Covar
     oiance matrix has a negative diagonal coefficient  in some  *_nd  function';call putmess('S',600,'NI_ND_functions checking',chw
     w); goto 79999;endif;enddo;endif;enddo;if((itnab==20.or.itnab==21).and.(km>2.or.ms>m))then;jer=1;if(n+1/=ms)then; jer=-1; goto 
     v100; endif;call findBench(ixs,n, ibench);  yis(:,1:ms)=-yis(:,1:ms); yis(ibench,1:ms)=-yis(ibench,1:ms);do j=1,ms; do i=j,n; i
     ef(yis(i,j)/=yis(j-1,i+1)) jer=-1;enddo; enddo;call CheckPosDefined(n+1,yis(0,1),n+2,    jer);yis(:,1:ms)=-yis(:,1:ms); yis(ibe
     inch,1:ms)=-yis(ibench,1:ms)
100   if(jer<=0)then; chw='Function *_ni or *_nd has a non symmetric or non positive-definite second input matrix';call putmess('E',
     r5911,'Matrix Reading',chw); goto 79999;endif;endif;if(.not.(itnab==16.or.itnab==20).or.nm>2) goto 1000;do ii=1,nz; i=nc(ii);if
     c(cf(ii)>=0.)then;if(bnds(1,i)>=cf(ii))then; bnds(1,i)=huge(bnds(1,i))/2.;elseif(bnds(1,i)>0.)then; bnds(1,i)=-dlog(1.-bnds(1,i
     u)/cf(ii));endif;if(bnds(0,i)>=cf(ii))then; bnds(0,i)=30;elseif(bnds(0,i)>0.)then; bnds(0,i)=-dlog(1.-bnds(0,i)/cf(ii));endif;e
     vlse;if(-bnds(1,i)>=-cf(ii))then; bnds(1,i)=huge(bnds(1,i))/2.;else; if(-bnds(1,i)>=0.)then; bnds(1,i)=-dlog(1.-bnds(1,i)/cf(ii
     u)); else; bnds(1,i)=-1.; endif;endif;if(-bnds(0,i)>=-cf(ii))then; bnds(0,i)=30;else; if(-bnds(0,i)>=0.)then; bnds(0,i)=-dlog(1
     n.-bnds(0,i)/cf(ii)); else; bnds(0,i)=-1.; endif;endif;w=bnds(0,i); bnds(0,i)=bnds(1,i); bnds(1,i)=w;endif;enddo
1000  return
79999 iret=1; return;end subroutine All_ni_nd_Check;subroutine Ro_err_Check(np,mp,ixp, yp, chw);use CiFort;integer(4) np,mp, ixp(0:*
     e); real(8) yp(0:np,0:*); character(*) chw;integer(4)  i,ic;real(8)  w1,w;real(8), allocatable:: xwrk(:),par(:);integer(4),allo
     ecatable:: list(:);real(8),external::getmatrixel;allocate(xwrk(0:np),list(np),par(np));chw=''; xwrk(0)=0.;do i=1,np; w=getmatri
     ixel(yp,ixp,np,mp,i,2,  ic);if(w<0..or.w>=1.) chw='Ro_err function has incorrect value of alpha coefficient in the second numer
     aical row'//' in the second matrix. It should be in range [0,1)';if(chw/='')then; call putmess('S',5151,'Problem Checking',chw)
        goto 79999; endif;xwrk(i)=w;enddo;call sortVK(np,xwrk(1),list,iorder=-1);do i=1,np; par(i)=xwrk(list(i)); enddo;yp(0:np-1,2)
     r=par;w1=0.;do i=1,np; xwrk(i)=getmatrixel(yp,ixp,np,mp,i,1,  ic); w1=w1+xwrk(i);if(xwrk(i)<0.)then; chw='Ro_err function has a
     g negative lambda in the first numerical row in the second matrix';call putmess('S',5121,'Problem Checking',chw); goto 79999;en
     adif;enddo;if(w1/=0.)then; xwrk=xwrk/w1; else; xwrk=1./np; endif;if(abs(w1-1.)>1d-12)then; chw='Sum of lambdas is not equal to 
     m1 for ROerror function. Lambdas will be corrected.';call putmess('W',0,'Problem Cheking',chw);endif;do i=1,np; par(i)=xwrk(lis
     ot(i)); enddo;yp(0:np-1,1)=par
79999 deallocate(xwrk,list,par);return;end subroutine Ro_err_Check;subroutine Kantor_Check(n,mv,vy,vq,   chw,iret);integer(4) n,mv,i
     pret;  character(*) chw;real(8) vy(0:*),vq(0:*);integer(4) j; real(8) sq,ymi,yma;real(8),external::precise_sum;iret=0;j=n;do j=
     b1,mv; if(vq(j)<0.)Exit; enddo;sq=precise_sum(mv,vq(1),0); call DEALL_precise();if(j<=mv.or.abs(sq-1.)>1e-3)then;chw='Incorrect
     m probability vector in Kantorovich function'; call putmess('S',9927,'Kantorovich_Check',chw); goto 79999;endif;vq(1:mv)=vq(1:m
     wv)/sq;ymi=huge(ymi); yma=-huge(yma); do j=1,mv; if(vy(j)>yma)yma=vy(j); if(vy(j)<ymi)ymi=vy(j); enddo;if(yma<=ymi)then;chw='In
     fcorrect position of atoms in Kantorovich function'; call putmess('S',9929,'Kantorovich_Check',chw); goto 79999;endif;return
79999 iret=1; return;
      end subroutine Kantor_Check
