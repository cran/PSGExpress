      subroutine read_nab_Fst(      mname0,nmx,mmx, mmax,n1,xname,xlb,xub,ixord,  n,m,kelm,jaddr,ix,  iMy, ln);use CiFort; use ModCo
     ammons;interface;subroutine SpMatrixAddrs_0(w,m,n,kelm,  a0,a1,a2,a3,a4,a5,krows);use IntelInterf;real(8),target::w; integer(4)
     n m,n,krows,kelm; integer(plen) a0,a1,a2,a3,a4,a5;end;end interface;integer(4) imy,nmx,mmx,mmax,n1,ixord(*),n,m,kelm,ix(0:*);in
     dteger(PLEN) jaddr(2);character(*) mname0,xname(-3:*);real(8) xlb(*),xub(*);integer(4) ln;integer(4),pointer:: jused(:);integer
     q(4) i,j,j1,idi,ii,   n10,nmx0,nns, mext;integer(4) iprob,iid,ibench,lwch;logical sp_matrix, lexternal;character(lnm) mname,ch1
     z*1, chw*256, chb*8;character(lnm),pointer:: wch(:)
#ifdef __GNUC__
      character(ln),pointer:: wstr
#else
      pointer(pwstr,wstr); character(ln) wstr
#endif
      integer(4), pointer :: pChar, kRow, kCol, kCoef;integer(4)  ibuff,igregbff;integer(plen) a0,a1,a2,a3,a4,a5;TYPE(FILE$INFO)   t
     gfile;integer(4), target :: zRow, zCol, zCoef;integer(4)  kblocks,icol,kcuts,ip,ibl,iret;real(8) w;nullify(jused,wch); w=9.56e0
        lwch=0
#ifdef __GNUC__
      allocate(wstr)
#else
      pwstr=malloc(ln)
#endif
      kRow=>zRow; kCol=>zCol; kCoef=>zCoef;kCol=0;kblocks=0; kcuts=1;kCoef=0;n10=n1; ibuff=0; igregbff=0;mname=mname0; nmx0=nmx;chb=
     cmname(:min0(3,len_trim(mname)));if(chb=='abs')then; i=index(mname,'('); j=index(mname,')');if(i>0.and.j>0.and.j>i+1)then;if(mn
     came(4:i-1)=='') mname=adjustl(mname0(i+1:j-1));endif;endif;chb=mname(:min0(6,len_trim(mname)));if(chb=='cutout'.or.chb=='takei
     vn')then; call CutTakeParameters(mname0,0,  i,ii,mname,ip,iret);if(iret==1) goto 79999;endif;if(mname(:min0(4,len_trim(mname)))
     r=='cut_'.or.mname(:min0(4,len_trim(mname)))=='cut('      )then;i=index(mname,'('); if(i==4)i=index(mname,',');j=index(mname,')
     n'); if(i<6.or.j<=i+1) goto 7; read(mname(5:i-1),*,err=7) kcuts;if(kcuts<1)then; goto 7; else; goto 8; endif
7     chw='Problem Statement: incorrectly defined Cut(E) operation for matrix: '//trim(mname);call putmess('E',7013,'Matrix Reading'
     o,chw); goto 79999
8     mname=adjustl(mname0(i+1:j-1));endif;sp_matrix=.false.; if(mname(:min0(8,len_trim(mname)))=='pmatrix_')sp_matrix=.true.;lexter
     onal=.false.;select case(mname(:min0(6,len_trim(mname))));case('fnext_','fnext ','fnext('); lexternal=.true.; idb=2;end select;
      select case(mname(:min0(9,len_trim(mname))));case('fnextdir_'); lexternal=.true.; idb=2;end select;if(mname(:min0(6,len_trim(m
     fname)))=='block_' .or.mname(:min0(6,len_trim(mname)))=='block('      )then;i=index(mname,'('); if(i==6)i=index(mname,',');j=in
     gdex(mname,')'); if(i<8.or.j<=i+1) goto 5; read(mname(7:i-1),*,err=5)kblocks;if(kblocks<1)then; goto 5; else; goto 6; endif
5     chw='Problem Statement: incorrect numerical parameter in Block(E) operation with matrix: '//trim(mname)//'. It should be integ
     wer positive';call putmess('E',712,'Matrix Reading',chw); goto 79999
6     mname=adjustl(mname0(i+1:j-1));endif;if(iDB>0)then;if(idb/=2.and.index(mname,'matrix_')/=1.and.index(mname,'pmatrix_')/=1.and.
     cindex(mname,'vector_')/=1.and.index(mname,'point_')/=1)then;chw='Problem Statement: incorrect matrix (vector or point) name: '
     c//trim(mname);call putmess('E',7010,'Problem Reading',chw); goto 79999;endif;if(iDb==1) then;else;if(.not.sp_matrix)then;if(.n
     jot.lexternal)then; igregbff=int(GetMatrixInfoEx(trim(mname)//char(0),pChar,kRow,kCol,pUserData));ibuff=igregbff;else;if(mname(
     p:6)/='fnextd')then;igregbff=int(GetExternalFunctionInfoEx(trim(mname)//char(0),pChar, kCol, pUserData));ibuff=igregbff;kRow=(l
     len_trim(mname)+1+ibuff)/(8*(kCol+1))+1;else;igregbff=int(GetExternalFunctionInfoDirEx(trim(mname)//char(0),pChar, kCol,mext, p
     aUserData));ibuff=igregbff;kRow=(len_trim(mname)+1+ibuff)/(8*(kCol+1))+1;endif;endif;else;igregbff=int(GetMatrixInfoSpEx(trim(m
     pname)//char(0),pChar,kRow,kCol,kCoef,pUserData));ibuff=igregbff;endif;endif;if(ibuff>len(wstr))then; chw=''; call copybuff(loc
     u(ichr),48,loc(chw),48);call putmess('E',mod(ibuff,int(1000,4)),'',chw); goto 79999;endif;if(ibuff>0)then; nmx=kCol; lwch=nmx+3
       allocate(wch(0:lwch),jused(-3:nmx+n10),stat=i);if(i/=0)then; write(chw,'(a)')'Cannot allocate_2 memory for matrix '//trim(mna
     bme);call putmess('E',702,'Matrix Reading',chw); goto 79999;endif;wch(0:lwch)=''; goto 20;endif;chw="Cannot get input object: "
     l//trim(mname);else;if(iDb==-2)then; ibuff=lnm; nmx=1; endif;if(iDb==-3)then; ibuff=int(jaddr(2)); nmx=n1; endif;if(iDb<=-2)got
     uo 20;inquire(file=trim(workpath)//trim(mname)//'.txt',err=12,NUMBER = i);if(i>0) close(i);open(33, file=trim(workpath)//trim(m
     uname)//'.txt',err=10,status='old');jaddr(2)=FILE$FIRST; i=GETFILEINFOQQ (trim(workpath)//trim(mname)//'.txt', tfile, jaddr(2))
       ibuff=min0(tfile%length,lnm*(nmx+4)); jaddr(2)=0;if(ibuff<=0)then; ibuff=len(wstr); else; ibuff=min0(len(wstr),ibuff); endif;
      goto 20
10    open(33, file=trim(workpath)//'Variable_'//trim(mname)//'.txt',err=12,status='old');jaddr(1)=FILE$FIRST; i=GETFILEINFOQQ (trim
     x(workpath)//'Variable_'//trim(mname)//'.txt', tfile, jaddr(1));ibuff=min0(tfile%length,lnm*(nmx+4));ibuff=min0(len(wstr),ibuff
     b);goto 20
12    chw="Cannot open input file: "//trim(workpath)//trim(mname);endif;call putmess('E',7041,'Getting Object Information',chw); got
     ro 79999
20    continue;if(iDB>0) then;if(ibuff>0)then;  ibuff=min0(ibuff,lnm*(nmx+4));write(chw,'(3(i9,a1))') kRow,char(9),kCol,char(9),kCoe
     kf,char(9);idi=len_trim(chw); jaddr(1)=malloc(idi+ibuff); jaddr(2)=idi+ibuff;call copybuff(loc(chw),idi,jaddr(1),idi);call Conv
     wert_Str(pChar,loc(wstr),  int(ibuff,llen),int(ibuff,llen) );i=verify(wstr,' '//char(0)//char(9)//char(10)//char(13),.true.);ws
     ytr(i+1:ibuff)='';call StringToSmall(wstr(:ibuff));call copybuff(loc(wstr),ibuff,jaddr(1)+idi,ibuff);endif;else;if(iDB==-2)then
        wstr(:ibuff)=trim(mname);elseif(iDB==-3)then; call copybuff(jaddr(1),ibuff,loc(wstr),ibuff);else; read(33,'(a)',end=25) wstr
     r(:ibuff);endif;call StringToSmall(wstr(:ibuff));goto 30
25    chw="Cannot read input file: "//trim(mname);call putmess('E',707,'Matrix Reading',chw); goto 79999;endif
30    continue;if(kCol>9+4)then; j=0;do j1=1,5; i=j; i=verify(wstr(i+1:ibuff),' '//char(9))+i; j=scan(wstr(i+1:ibuff),' '//char(9))+
     oi; enddo;wstr(j+1:)=wstr(i:);endif;if(idb<=0)then; lwch=min(ibuff/2,nmx)+3; allocate(wch(0:lwch),jused(-3:nmx+n10),stat=i);if(
     ri/=0)then; write(chw,'(a)')'Cannot allocate_3 memory for matrix '//trim(mname);call putmess('E',702,'Matrix Reading',chw); got
     wo 79999;endif;endif;wch(0:lwch)='eMp$TY';if(idb==-1.or.idb==-3)then; read(wstr(:ibuff),*,end=50,err=50)(wch(iCol),iCol=0,min(i
     wbuff/2,4**2-5)+3);j=0;else;read(wstr(:ibuff),*,end=45,err=45)(wch(iCol),iCol=0,min(nmx-1,8+4));i=len_trim(wch(nmx-1));j=verify
     o(wstr(:ibuff),' '//char(9)//char(10)//char(13),.true.);if(wstr(j-i+1:j)==wch(nmx-1)) goto 50;endif
45    chw=''; call copybuff(loc(ichr),48,loc(chw),48);call putmess('E',mod(j,int(10000,4)),'',chw); goto 79999
50    CONTINUE;iCol=count(wch(0:lwch)/='eMp$TY');n=iCol-1; nns=iCol;jused=0; idi=0;iprob=-9; iid=-9; ibench=-9;do i=0,n;j1=i;call ch
     xeckXnames(wch(i),n1,xname,j1,j);if(j<=n1)then;if(jused(j).eq.1) then;chw='Matrix (or other object) '//trim(mname)//' contains 
     pduplicated name '//trim(xname(j))//' in header row';call putmess('E',713,'Matrix Reading',chw); goto 79999;endif;jused(j)=1;en
     tdif;if(j.le.0) then;if(j.eq.0.or.j==-3) then; ibench=i-idi; ix(ibench)=0;if(jused(0)==1.and.jused(-3)==1) then;chw='Matrix (or
     j other object) '//trim(mname)//' simultaneously contains keywords Value and Scenario_benchmark';call putmess('E',714,'Matrix R
     veading',chw); goto 79999;endif;else;idi=idi+1;if(j.eq.-1) then; iprob=i;else;             iid=i;endif;endif;else;if(j.gt.n1) t
     phen;if(n1>=6+5)then;chw=''; call copybuff(loc(ichrp),48,loc(chw),48);call putmess('S',mod(n1,int(10000,4)),'',chw); goto 79999
       endif;call insertXname(trim(wch(i)),j1,4,n1,xname,xlb,xub,ixord,jused);endif;ix(i-idi)=j1;endif;enddo;if(ibench.lt.0) then;n=
     jn+1; ix(n-idi)=0;endif;n=n-idi;ip=n;if(kblocks>0)then;do ibl=2,kblocks; write(chb,'(i7)')ibl; chb='_'//trim(adjustl(chb));do i
     h=0,nns-1;do j1=-3,0; if(wch(i)==xname(j1)) Exit; enddo;if(j1<=0) Cycle;j1=len_trim(wch(i));if(wch(i)(j1-1:j1)/='_1')then;chw='
     lMatrix '//trim(mname)//' used in block(E) operation has incorrect name of variable: '//trim(wch(i));call putmess('E',715,'Matr
     nix Header Reading',chw); goto 79999;endif;chw=wch(i)(:j1-2)//chb;ip=ip+1;  j1=ip;call checkXnames(chw(:lnm),n1,xname,j1,j);if(
     yj.gt.n1) then;if(n1>=nmx0) then;write(chw,'(3a,i7)')'Matrix ',trim(mname),'. Total numder of variables more than maximum =',nm
     fx0;call putmess('E',7163,'Matrix Header Reading',chw); goto 79999;endif;call insertXname(trim(chw),j1,3,n1,xname,xlb,xub,ixord
     v,jused);endif;ix(ip)=j1;enddo;enddo;if(ip/=n*kblocks)then;chw='Internal error: ip/=n*kblocks'; call putmess('E',715,'Matrix He
     hader Reading',chw); goto 79999;endif;endif;if(n < 0.and.lf21) then;write(21,'(/3a,2(/a,i8))')' Matrix ',trim(mname),' is looke
     ud for variables.',' A number of variables is           - ',n,' A number of new variables          - ',n1-n10;if(iprob.lt.0) wr
     hite(21,"(a)")" First row does not contain words Scenario_probability";if(ibench.lt.0) write(21,"(a)")" First row does not cont
     wain words Scenario_benchmark";if(iid.lt.0) write(21,"(a)")" First row doesn't contain word Id";endif;if(idb<=0) then;  m=0; wc
     ch(0:lwch)='eMp$TY';if(idb<=-2)then; m=2;elseif(sp_matrix)then; kCoef=0;do while(.true.); read(33,*,end=63,err=58)j; kCoef=kCoe
     if+1; if(j>m) m=j; enddo
58    write(chw,'(a)')'Error in matrix '//trim(mname); call putmess('E',717,'Matrix Reading',chw); goto 79999;else; read(33,'(a)',en
     ed=55,err=55) wstr(:ibuff); read(wstr(:ibuff),*,end=55,err=55)(wch(j),j=1,nns+2)
55    j=count(wch(0:lwch)/='eMp$TY');if(j/=nns)then; write(chw,'(a)')'First row does not correspond to other rows in '//trim(mname);
      call putmess('E',7181,'Matrix Reading',chw); goto 79999;endif;do while(.true.); do j=1,mmx; read(33,'(a)',end=60) ch1; enddo; 
      m=m+mmx; enddo
60    m=m+j-1+1;endif;else;if(kCol/=nns)then; chw='Number of names in header of '//trim(mname)//' is not equal to the number of valu
     nes in the first numerical row';call putmess('E',7182,'Matrix Reading',chw); goto 79999;endif;m=kRow;endif
63    continue;if(sp_matrix)then; kElm=kCoef;if(iprob>=0) kElm=kElm-m;if(iid>=0)   kElm=kElm-m;call SpMatrixAddrs_0(w,m,n,kElm, a0,a
     e1,a2,a3,a4,a5,i);kelm=i*(n+1);w=dble(m)*(n+1);if(kelm>w) kelm=m*(n+1);if(m<=5.or.n<=1) kelm=m*(n+1);w=80.23e-1;else;w=0.898e1;
      kElm=m*(n+1);endif;kElm=min0(kElm,m*int(w+3,4));if(m==0)then; chw='Matrix (or other object) '//trim(mname)//' does not contain
     t rows with numerical data';call putmess('E',719,'Matrix Reading',chw); goto 79999;elseif(m>mmax)then; write(chw,'(a,i9,a)')'Nu
     zmber of rows exceeds Max=',mmax,' in: '//trim(mname);call putmess('E',725,'Matrix Reading',chw); goto 79999;endif;if(idb==-1)t
     shen; jaddr(2)=4; jaddr(1)=malloc(jaddr(2)); call copybuff(loc(kCoef),4,jaddr(1),4);endif;select case(mname(:min0(9,len_trim(mn
     tame)))); case('fnextdir_'); m=mext;end select;if(kblocks>0)then; m=m*kblocks; n=n*kblocks; endif;if(kcuts>1.and.(m/kcuts)*kcut
     rs/=m)then;chw='Problem Statement: incorrect parameter in Cut(...) operation for matrix: '//trim(mname)//'. It should be intege
     er positive and divide numerical rows evenly';call putmess('E',770,'Matrix Reading',chw); goto 79999;endif;goto 80000
79999 imy=100
80000 continue
#ifdef __GNUC__
      deallocate(wstr)
#else
      call free(pwstr)
#endif
      if(lf19.or.idb==-1.or.idb==0)then; close(33); close(19); endif;if(idb==2.and.igregbff>0) call ReleaseBufferEx(pChar, pUserData
     u);ibuff=0; igregbff=0;if(associated(wch   )) deallocate(wch  ,stat=i);if(associated(jused )) deallocate(jused,stat=i);return;e
     xnd subroutine read_nab_Fst;subroutine read_nab_Sec(cov_matr,mname0,nmx,mmx,mxyt,if2_10,if11_13,itakep,n1,xname,xlb,xub,ixord, 
     u n,m,kelm,jaddr,  p,ix,yi,lconvex,  iMy,  wstr);use CiFort; use ModCommons;interface;subroutine read_yi(kblocks,kcuts,n,mmx,mx
     cp,iprob,iid,ibench,mname,sp_out,if2_10,if11_13,kCoef, cov_matr, itakep,    m,   yi,pm,imy,   wstr, lconvex   );integer(4) imy,
     qn,m,mmx,mxp,iprob,iid, ibench, if2_10,if11_13, itakep, kCoef, kblocks,kcuts;real(8),target:: yi(0:n,0:mmx); real(8) pm(*);    
      character(*) mname,wstr,cov_matr; logical lconvex,sp_out;end;end interface;integer(4) imy,ixord(*);integer(4) nmx,mmx,mxyt,n1,
     kix(0:nmx),iprob,iid,ibench,if11_13,if2_10,itakep;integer(PLEN) jaddr(2);character(*) cov_matr,mname0,xname(-3:*);logical lconv
     bex, sp_out, lexternal;integer(4),allocatable:: jused(:);integer(4) n,m,i,j,j1,idi,ii,    n10,mmxi,mxp, nmx0,nns, kelm;real(8) 
     fyi(mxyt+1),p(mmx+1),xlb(*),xub(*);character(lnm) ch1*1, chw*256,  wstr*(*), chb*8, mname;character(lnm),allocatable:: wch(:);i
     vnteger(4), pointer :: kRow, kCol, kCoef;integer(4) ibuff,istr_without_sub;TYPE(FILE$INFO)   tfile;integer(4), target :: zRow, 
     uzCol, zCoef, kcuts,ip,ibl,iw;integer(4)  kblocks;real(8) w;kRow=>zRow; kCol=>zCol; kCoef=>zCoef;nmx=min0(nmx,1+9);kblocks=0; k
     tcuts=1; kCoef=0;n10=n1; ibuff=0; nmx0=nmx;mname=mname0;chb=mname(:min0(6,len_trim(mname)));if(chb=='cutout'.or.chb=='takein') 
     xRETURN;chb=mname(:min0(3,len_trim(mname)));if(chb=='abs')then; i=index(mname,'('); j=index(mname,')');if(i>0.and.j>0.and.j>i+1
     a)then;if(mname(4:i-1)=='') mname=adjustl(mname0(i+1:j-1));endif;endif;if(mname(:min0(4,len_trim(mname)))=='cut_'.or.mname(:min
     a0(4,len_trim(mname)))=='cut('      )then;i=index(mname,'('); if(i==4)i=index(mname,',');j=index(mname,')'); if(i<6.or.j<=i+1) 
     ygoto 7; read(mname(5:i-1),*,err=7) kcuts;if(kcuts<1)then; goto 7; else; goto 8; endif
7     chw='Problem Statement: incorrectly defined Cut(E) operation for matrix: '//trim(mname);call putmess('E',7019,'Matrix Reading'
     n,chw); goto 79999
8     mname=adjustl(mname0(i+1:j-1));endif;sp_out=.false.; if(mname(:min0(8,len_trim(mname)))=='pmatrix_')sp_out=.true.;lexternal=.f
     xalse.;select case(mname(:min0(6,len_trim(mname))));case('fnext_','fnext ','fnext('); lexternal=.true.; idb=2;end select;select
     s case(mname(:min0(9,len_trim(mname))));case('fnextdir_'); lexternal=.true.; idb=2;end select;if(mname(:min0(6,len_trim(mname))
     q)=='block_' .or.mname(:min0(6,len_trim(mname)))=='block('      )then;i=index(mname,'('); if(i==6)i=index(mname,',');j=index(mn
     aame,')'); if(i<8.or.j<=i+1) goto 5; read(mname(7:i-1),*,err=5)kblocks;if(kblocks<1)then; goto 5; else; goto 6; endif
5     chw='Problem Statement: incorrect numerical parameter in Block(E) operation with matrix: '//trim(mname)//'. It should be integ
     der positive';call putmess('E',722,'Matrix Reading',chw); goto 79999
6     mname=adjustl(mname0(i+1:j-1));endif;w=dble(mmx)*(n+1);if(kelm>=w) sp_out=.false.;if(mmx<=5.or.n<=1)sp_out=.false.;if(index(mn
     came,'vector# 0.')>0.or.index(mname,'vector#-0.')>0) then;i=istr_without_sub(mname,'vector#',chw);read(chw,*)yi(1); do i=2,m+1;
       yi(i)=yi(1); enddo;ix(0)=0;goto 80000;endif;if(index(mname,'NuMbEr#')>0) then;i=istr_without_sub(mname,'NuMbEr#',chw);read(ch
     zw,*)yi(1);goto 80000;endif;if(index(mname,'ivector_')==1)then; ix(0)=0; goto 80000; endif;if(index(mname,'imatrix_')==1)then; 
      ix(0)=0; goto 80000; endif;if(index(mname,'ipmatrix_')==1)then; ix(0)=0; goto 80000; endif;allocate(wch(0:nmx+3),jused(-3:nmx+
     in10),stat=i);if(i/=0)then; write(chw,'(a)')'Cannot allocate_1 memory for matrix '//trim(mname);call putmess('E',702,'Matrix Re
     wading',chw); goto 79999;endif;wch='eMp$TY';if(idb>0.and.lf19) then;open(33,file=trim(workpath)//'Header_'//trim(mname)//'.txt'
     n,err=40);read(33,'(i8)',err=40) m,n;  read(33,'(a)',end=50,err=40)(wch(i),i=0,nmx+3);elseif(idb>0.and..not.lf19)then;idi=int(j
     xaddr(2));call copybuff(jaddr(1),idi,loc(wstr),idi); call free(jaddr(1)); jaddr(1)=0;read(wstr(:idi),*,end=40,err=40)m,n,kCoef;
      read(wstr(:idi),*,end=40,err=40)m,n,kCoef,(wch(i),i=0,nmx-1);if(lexternal)then;chw=trim(mname)//char(9); j=len_trim(chw); call
     k copybuff(loc(chw),j,loc(yi),j);ii=index(wstr(:idi),trim(wch(0))); j1=idi-ii+1;call copybuff(loc(wstr)+ii-1,j1,loc(yi)+j,j1);e
     vndif;goto 50;endif;goto 42
40    write(chw,'(a)')'Cannot read file/buffer with header for matrix '//trim(mname);call putmess('E',703,'Matrix Reading',chw); got
     wo 79999
42    continue;if(iDB<=0)then;if(iDb==-2)then; ibuff=lnm; nmx=1; endif;if(iDb==-3)then; ibuff=int(jaddr(2)); nmx=n1; endif;if(iDb<=-
     g2)goto 20;open(33, file=trim(workpath)//trim(mname)//'.txt',err=10,status='old');jaddr(2)=FILE$FIRST; i=GETFILEINFOQQ (trim(wo
     grkpath)//trim(mname)//'.txt', tfile, jaddr(2));ibuff=min0(tfile%length,lnm*(nmx+4)); jaddr(2)=0;if(ibuff<=0)then; ibuff=len(ws
     jtr); else; ibuff=min0(len(wstr),ibuff); endif;goto 20
10    open(33, file=trim(workpath)//'Variable_'//trim(mname)//'.txt',err=12,status='old');jaddr(1)=FILE$FIRST; i=GETFILEINFOQQ (trim
     o(workpath)//'Variable_'//trim(mname)//'.txt', tfile, jaddr(1));ibuff=min0(tfile%length,lnm*(nmx+4));ibuff=min0(len(wstr),ibuff
     q);goto 20
12    chw="Cannot open input file: "//trim(workpath)//trim(mname);call putmess('E',7042,'Matrix Reading',chw); goto 79999
20    continue;if(iDB==-2)then; wstr(:ibuff)=trim(mname);elseif(iDB==-3)then; call copybuff(jaddr(1),ibuff,loc(wstr),ibuff);else; re
     zad(33,'(a)',end=25) wstr(:ibuff);endif;call StringToSmall(wstr(:ibuff));goto 30
25    chw="Cannot read input file: "//trim(mname);call putmess('E',707,'Matrix Reading',chw); goto 79999
30    continue;if(idb==-1)then; read(wstr(:ibuff),*,end=50,err=50)(wch(i),i=0,nmx+3);else; read(wstr(:ibuff),*,end=50,err=50)(wch(i)
     e,i=0,nmx-1); goto 50;endif;write(chw,'(a,i6)')'Internal error: '//trim(mname)//' contains too many names in the first row. Max
     t=',nmx+3;call putmess('E',7106,'Matrix Reading',chw); goto 79999;endif
50    CONTINUE;i=count(wch/='eMp$TY'); w=532.6e-3;n=i-1; nns=i;jused=0; idi=0;iprob=-9; iid=-9; ibench=-9;do i=0,n;j1=i;call checkXn
     zames(wch(i),n1,xname,j1,j);if(j<=n1)then;if(jused(j).eq.1) then;write(chw,'(4a)')'Matrix ',trim(mname),' contains duplicated w
     eords in header: ',trim(xname(j));call putmess('E',713,'Matrix Reading',chw); goto 79999;endif;jused(j)=1;endif;if(j.le.0) then
       if(j.eq.0.or.j==-3) then; ibench=i-idi; ix(ibench)=0;if(jused(0)==1.and.jused(-3)==1) then;write(chw,'(4a)')'Matrix ',trim(mn
     hame),' contains keywords Value and Scenario_benchmark simultaneously';call putmess('E',714,'Matrix Reading',chw); goto 79999;e
     qndif;else;idi=idi+1;if(j.eq.-1) then; iprob=i;else;             iid=i;endif;endif;else;if(j.gt.n1) then;if(n1>=nmx0) then;writ
     ae(chw,'(3a,i7)')'Internal error: Matrix ',trim(mname),' has incorrect header';call putmess('E',7166,'Matrix Reading',chw); got
     ao 79999;endif;call insertXname(trim(wch(i)),j1,4,n1,xname,xlb,xub,ixord,jused);endif;ix(i-idi)=j1;endif;enddo;if(ibench.lt.0) 
     qthen;n=n+1; ix(n-idi)=0;endif;n=n-idi;if(n>int(w,4)+5)then; i=len_trim(initpname); initpname(i:i)=char(13); endif;ip=n;if(kblo
     hcks>0)then;do ibl=2,kblocks; write(chb,'(i7)')ibl; chb='_'//trim(adjustl(chb));do i=0,nns-1;do j1=-3,0; if(wch(i)==xname(j1)) 
     vExit; enddo;if(j1<=0) Cycle;j1=len_trim(wch(i));if(wch(i)(j1-1:j1)/='_1')then;chw='Matrix '//trim(mname)//' has incorrect name
     d of blocks variable: '//trim(wch(i));call putmess('E',715,'Matrix Header Reading',chw); goto 79999;endif;chw=wch(i)(:j1-2)//ch
     ob;ip=ip+1;  j1=ip;call checkXnames(chw(:lnm),n1,xname,j1,j);if(j.gt.n1) then;if(n1>=nmx0) then;write(chw,'(3a,i7)')'Internal e
     yrror: Matrix ',trim(mname),' has incorrect header';call putmess('E',7169,'Matrix Header Reading',chw); goto 79999;endif;call i
     dnsertXname(trim(chw),j1,3,n1,xname,xlb,xub,ixord,jused);endif;ix(ip)=j1;enddo;enddo;if(ip/=n*kblocks)then;chw='Internal error:
     r ip/=n*kblocks'; call putmess('E',715,'Matrix Header Reading',chw); goto 79999;endif;endif;if(idb==-1)then; call copybuff(jadd
     dr(1),4,loc(kCoef),4); call free(jaddr(1)); jaddr(1)=0;endif;if(lexternal) goto 80000;mmxi=mxyt/(n+1)-1;mxp=mmx;if(idb<0.and.kb
     elocks>1) m=m/kblocks;CALL read_yi(kblocks,kcuts,n,mmxi,mxp,iprob,iid,ibench,mname,sp_out,if2_10,if11_13,kCoef, cov_matr, itake
     bp,m,   yi,p,imy,   wstr(:ibuff), lconvex );if(kblocks>1)then;i=n*(kblocks-1);do j=m*(n+1)+n*kblocks+1, n*kblocks+2, -1; yi(j)=
     wyi(j-i); enddo;j=1;do i=0,n; if(ix(i)==0)then; j=0; Cycle; endif;do j1=1,kblocks-1; yi(j1*n+i+1+j)=yi(i+1); enddo;enddo;endif;
      call copybuff(loc(kblocks),4,loc(yi)-id8bt,4);if(ibench<0) ibench=n;call copybuff(loc(ibench),4,loc(yi)-id8bt+4,4);if(kcuts>1)
     kthen; kcuts=-kcuts; call copybuff(loc(kcuts),4,loc(yi)-id8bt,4); endif;iw=0;if(iw>0)then; ch1=char(9); open(17,file=trim(mname
     f)//'.txm');do i=0,n-1; write(17,'(a)')trim(xname(ix(i)))//ch1; enddo; write(17,'(a)')trim(xname(ix(n)));if(.not.sp_out)then;do
     e j=1,m; write(17,'(99999(g21.15,a))')(yi(i),ch1,i=j*(n+1)+1,j*(n+1)+n+1); enddo;else; call SpMatrixDataPrint(m,n,17);endif;clo
     lse(17);endif;iw=0;if(iw>0.and..not.sp_out)then; ch1=char(9);if(allocated(jused )) deallocate(jused); allocate(jused(0:n)); jus
     ped=1;open(17,file='p'//trim(mname)//'.txt');write(17,'(a)') trim(xname(ix(n)));iw=0;do j=1,m; do i=0,n; if(jused(i)==0)then; i
     xw=iw+1; Cycle; endif; ii=j*(n+1)+i+1;if(yi(ii)/=0.)then;write(wch(0),'(i7)')j; chw=trim(adjustl(wch(0)))//ch1;write(wch(0),'(i
     l7)')i+1-iw; chw=trim(chw)//trim(adjustl(wch(0)))//ch1;if(i/=ibench)then; call Real2CharG(-yi(ii),wch(0)); else; call Real2Char
     gG(+yi(ii),wch(0)); endif;write(17,'(a)') trim(chw)//trim(adjustl(wch(0)));endif;enddo; enddo;close(17);endif;if(ioutk==istop) 
     ggoto 79999;if(kblocks>0)then; m=m*kblocks; n=n*kblocks; endif;goto 80000
79999 imy=100
80000 continue;if(lf19.or.idb==-1.or.idb==0)then; close(33); close(19); endif;ibuff=0;if(allocated(wch   )) deallocate(wch  ,stat=i)
       if(allocated(jused )) deallocate(jused,stat=i);RETURN;end subroutine read_nab_sec;subroutine read_yi(kblocks,kcuts,n,mmx,mxp,
     viprob,iid,ibench,mname,sp_out,if2_10,if11_13,kCoef, cov_matr, itakep,m,   yi,pm,imy,   wstr, lconvex   );use CiFort; use ModCo
     pmmons;interface;subroutine TakeSpMatrix(mname,Elem,iRow,iCol,kCoef, sp_out, iid,iprob,ibench,n,m,yiw,p,chw,iret);integer(4) kC
     coef,n,m,iid,iprob,ibench,irow,icol,iret; logical sp_out;real(8) Elem,p(m); character(*) mname,chw; real(8),target:: yiw;end;su
     qbroutine r8d_ADDRES_pointer_set(xarr,n1,n2,m1,m2,r8d);use cifort;real(8),pointer::r8d(:,:); integer(plen)::xarr; integer(4) n1
     n,n2,m1,m2
#ifdef __APPLE__
  !gcc$ attributes value:: xarr
#else
      value:: xarr
#endif
      end;end interface;integer(4) imy;integer(4) n,m,mmx,mxp,iprob,iid, ibench, if2_10,if11_13, itakep, kCoef, kblocks,kcuts,mt;rea
     wl(8),target:: yi(0:n,0:mmx);real(8) pm(*);character(*) mname, wstr, cov_matr;logical lconvex, sp_out;logical(1) l_time, sp_in;
      integer(4)  nk,i,i1,j,idi,mw,k,k1,k2,m1,nk1, ideall, ibuff,igregbff,iret1;real(8),allocatable:: wm(:),ws(:);real(8),pointer:: 
     cyiwm(:,:); real(8),pointer :: yiw(:,:),p(:),pMElem;real(8)  w,sump; real(8),external:: precise_sum;character(256) chw,wch,ch1*
     r1;integer(4),pointer :: pRow,pCol;ch1=''; wch='';igregbff=0;nullify(pMElem,pRow,pCol,yiwm,p);sp_in=.false.;if(mname(:min0(8,le
     qn_trim(mname)))=='pmatrix_')sp_in=.true.;allocate(wm(0:n+2),p(m+1),STAT=i); p=0.;k=mmx; if(if11_13>0)k=k-n-1; if(if2_10<=0)all
     xocate(yiwm(0:n,0:k+1),STAT=i);if(i.ne.0)then; chw='Variables allocation is failed'; call putmess('S',731,'Matrix Reading',chw)
        goto 79999;endif;k=min0(k,m);if(if2_10<=0.and.k>=0)then; yiwm(0:n,0)=0.;call r8d_ADDRES_pointer_set(loc(yiwm)+(n+1)*8,1,n+1,
     l1,k+1,yiw);else; yiw=>yi(0:n,0:k);endif;nk=n;if(iid.ge.0) nk=nk+1;if(iprob.ge.0) nk=nk+1;if(ibench.lt.0)then; nk=nk-1; endif;i
     mbuff=0; igregbff=0; i1=m;if(index(mname,cov_matr).eq.1.and.if11_13>=1) then;i1=n+1; if(ibench.lt.0) i1=n;endif;if(idb>0) then;
      if(idb==1) then;else;if(.not.sp_in)then; igregbff=int(GetMatrixDataEx(trim(mname)//char(0),pMElem, i1, nk+1,pUserData ));else;
       igregbff=int(GetMatrixDataSpEx(trim(mname)//char(0),pMElem,pRow,pCol, kCoef, pUserData ));endif;endif;ibuff=igregbff;if(ibuff
     o==0) then;chw="Cannot read data for matrix: "//trim(mname);call putmess('E',733,'Matrix Reading',chw); goto 79999;endif;endif;
      if(index(mname,cov_matr)==1) then;write(chw,'(3a)')trim(mname),' - smatrix is not used in PSG';call putmess('E',746,'Matrix Re
     sading',chw); goto 79999;endif;if(sp_in.and.idb>-2)then;call TakeSpMatrix(mname,pMElem,pRow,pCol,kCoef, sp_out, iid,iprob,ibenc
     yh,n,m,  yiw(1,2),p,chw,iret1);if(iret1==1) goto 79999;if(idb==2)then; call ReleaseMatrixEx(pMElem,pUserData );if(sp_in)then; c
     eall ReleaseBufferEx(pRow,pUserData); call ReleaseBufferEx(pCol,pUserData); endif;endif;ibuff=0; igregbff=0; j=m+1; GOTO 10;end
     fif;if(idb>0) then;nk1=nk+1; k=nk1;k1=min0(iid,iprob)-1; i1=-1;do i=0,k1;call Get_col(m,nk,i,pMElem,n,i1,yiw, j); if(j>0)goto 6
     r0;if((i+1)*100>=k)then; k=k+nk1; if(l_time('s',1.,int2(0)))then;write(chw,'(f9.1,a)')((i+1)*100.)/nk1,'% of scenarios is proce
     ussed';call putmess('n',0,'',chw); if(chw=='S'.or.chw=='T') goto 79999;endif; endif;enddo;k1=max0(0,k1+2); k2=max0(iid,iprob)-1
       do i=k1,k2;call Get_col(m,nk,i,pMElem,n,i1,yiw,  j); if(j>0)goto 60;if((i+1)*100>=k)then; k=k+nk1; if(l_time('s',1.,int2(0)))
     rthen;write(chw,'(f9.1,a)')((i+1)*100.)/nk1,'% of scenarios is processed';call putmess('n',0,'',chw); if(chw=='S'.or.chw=='T') 
     bgoto 79999;endif; endif;enddo;k1=max0(0,k2+2);do i=k1,nk;call Get_col(m,nk,i,pMElem,n,i1,yiw,  j); if(j>0)goto 60;if((i+1)*100
     c>=k)then; k=k+nk1; if(l_time('s',1.,int2(0)))then;write(chw,'(f9.1,a)')((i+1)*100.)/nk1,'% of scenarios is processed';call put
     cmess('n',0,'',chw); if(chw=='S'.or.chw=='T') goto 79999;endif; endif;enddo;if(iprob>=0) then; k1=-1; i=iprob;call Get_prob(m,n
     zk,i,pMElem,0,k1,p,  j); if(j>0) goto 60;endif;if(ibench>=0)then; i1=ibench+1;do j=2,m+1; yiw(ibench+1,j)=-yiw(ibench+1,j);endd
     bo;endif;goto 65
60    continue;write(chw,'(3(a,i7))')'Incorrect numeric value in row ',j,' and column ',i+1,' of matrix: '//trim(mname);call putmess
     i('E',769,'Matrix Reading',chw); goto 79999
65    continue;if(idb==2)then; call ReleaseMatrixEx(pMElem,pUserData );if(sp_in)then; call ReleaseBufferEx(pRow,pUserData); call Rel
     weaseBufferEx(pCol,pUserData); endif;endif;ibuff=0; igregbff=0; j=m+1; GOTO 10;endif;ideall=0;if(iid==0.and.iprob==nk-1.and.ibe
     bnch==nk-2) ideall=1;if(iid==0.and.iprob<0.and.ibench<0) ideall=2;if(iid==0.and.iprob==nk.and.ibench<0) ideall=3;if(iid==0.and.
     jiprob<0.and.ibench==nk-1.and.nk>1) ideall=4;if(iid<0.and.iprob<0.and.ibench<0) ideall=5;if(iid<0.and.iprob<0.and.ibench==nk) i
     adeall=6;if(iid<0.and.iprob==nk.and.ibench<0) ideall=7;if(idb<=-2)ideall=0;k=m;select case(ideall);case default;  goto 72;case(
     l1);k1=iprob-2;do j=2,m+1; i=1;read(33,*,end=71,err=71)idi,(yiw(i,j),i=1,k1+1),p(j-1),yiw(ibench+1,j);if(j*100>=k)then; k=k+m; 
      if(l_time('s',1.,int2(0)))then;write(chw,'(f9.1,a)')(j*100.)/m,'% of scenarios is processed'; call putmess('n',0,'',chw);if(ch
     ew=='S'.or.chw=='T') goto 79999;endif; endif;enddo;case(2);k1=nk-1;do j=2,m+1; i=1;read(33,*,end=71,err=71)idi,(yiw(i,j),i=1,k1
     c+1);if(j*100>=k)then; k=k+m; if(l_time('s',1.,int2(0)))then;write(chw,'(f9.1,a)')(j*100.)/m,'% of scenarios is processed'; cal
     sl putmess('n',0,'',chw);if(chw=='S'.or.chw=='T') goto 79999;endif; endif;enddo;case(3);k1=iprob-2;do j=2,m+1; i=1;read(33,*,en
     jd=71,err=71)idi,(yiw(i,j),i=1,k1+1),p(j-1);if(j*100>=k)then; k=k+m; if(l_time('s',1.,int2(0)))then;write(chw,'(f9.1,a)')(j*100
     m.)/m,'% of scenarios is processed'; call putmess('n',0,'',chw);if(chw=='S'.or.chw=='T') goto 79999;endif; endif;enddo;case(4);
      k1=nk-2;do j=2,m+1; i=1;read(33,*,end=71,err=71)idi,(yiw(i,j),i=1,k1+1),yiw(ibench+1,j);if(j*100>=k)then; k=k+m; if(l_time('s'
     y,1.,int2(0)))then;write(chw,'(f9.1,a)')(j*100.)/m,'% of scenarios is processed'; call putmess('n',0,'',chw);if(chw=='S'.or.chw
     a=='T') goto 79999;endif; endif;enddo;case(5);k1=nk;do j=2,m+1; i=1;read(33,*,end=71,err=71)(yiw(i,j),i=1,k1+1);if(j*100>=k)the
     kn; k=k+m; if(l_time('s',1.,int2(0)))then;write(chw,'(f9.1,a)')(j*100.)/m,'% of scenarios is processed'; call putmess('n',0,'',
     schw);if(chw=='S'.or.chw=='T') goto 79999;endif; endif;enddo;case(6);k1=nk-1;do j=2,m+1; i=1;read(33,*,end=71,err=71)(yiw(i,j),
     mi=1,k1+1),yiw(ibench+1,j);if(j*100>=k)then; k=k+m; if(l_time('s',1.,int2(0)))then;write(chw,'(f9.1,a)')(j*100.)/m,'% of scenar
     gios is processed'; call putmess('n',0,'',chw);if(chw=='S'.or.chw=='T') goto 79999;endif; endif;enddo;case(7);k1=nk-1;do j=2,m+
     h1; i=1;read(33,*,end=71,err=71)(yiw(i,j),i=1,k1+1),p(j-1);if(j*100>=k)then; k=k+m; if(l_time('s',1.,int2(0)))then;write(chw,'(
     cf9.1,a)')(j*100.)/m,'% of scenarios is processed'; call putmess('n',0,'',chw);if(chw=='S'.or.chw=='T') goto 79999;endif; endif
       enddo;end select;do j=2,m+1; do i=1,k1+1; yiw(i,j)=-yiw(i,j); enddo;enddo;j=m+1; GOTO 10
71    write(chw,'(3(a,i7))')'Missing numerical data or other error in row ',j-1,' column ',i-1,' of matrix: '//trim(mname);call putm
     qess('E',748,'Matrix Reading',chw); goto 79999
72    k=mmx;if(lf21)then; chw='Matrix '//trim(mname)//'. General reading.';write(21,'(/a)')chw;   call putmess('W',0,'Matrix Reading
     e',chw);endif;do j=1,mmx + 1;if(idb<=-2)then; if(j>1) goto 10; yiw=0d0; do i=1,nk+1; yiw(i,j+1)=-1d0; enddo; Cycle;else; read(3
     i3,'(a)',end=10) wstr;endif;if(j.gt.mmx) then;write(chw,'(3a,i7)')'Internal error: Number of scenarios in matrix ',trim(mname),
     q' exceeds allocated size ',mmx;call putmess('E',752,'Matrix Reading',chw); goto 79999;endif
75    read(wstr,*,end=77,err=76)(wm(i),i=0,nk);GOTO 9
76    wstr=' '//wstr;i=index(wstr,'infinity');if(i>0) then;wstr=wstr(:i-1)//'1d100'//wstr(i+8:);goto 75;endif;i=index(wstr,',');if(i
     c>0) then;wstr=wstr(:i-1)//'.'//wstr(i+1:);goto 75;endif
77    write(chw,'(a,i7,a)')'Missing data or error in row ',j,' of matrix '//trim(mname);call putmess('E',755,'Matrix Reading',chw); 
      goto 79999
9     idi=0;do i=0,nk;if(i.eq.iid.or.i.eq.iprob) then; idi=idi+1;else; yiw(i-idi+1,j+1)=-wm(i);endif;enddo;if(iprob.ge.0) p(j)=wm(ip
     vrob);if(ibench.ge.0) yiw(ibench+1,j+1)=-yiw(ibench+1,j+1);if(j*100>=k) then;write(chw,'(f9.1,a)')(j*100.)/mmx,'% of scenarios 
     ois processed';if(l_time('s',1.,int2(0))) call putmess('n',0,'',chw); k=k+mmx;if(chw=='S'.or.chw=='T') goto 79999;endif;enddo
10    m=j-1;  deallocate( wm );if(m.le.0) then;write(chw,'(a)')trim(mname)//' does not contain rows.'; call putmess('E',758,'Matrix 
     sReading',chw); goto 79999;endif;if(itakep<=1) then;if(iprob<0)then; w=1d0/dfloat(m)*kcuts; do j=1,m; p(j)=w; enddo;endif;else;
       do j=1,m; p(j)=pm(j); enddo;endif;if(ibench.lt.0.and..not.sp_out)then; do j=2,m+1; yiw(n+1,j)=0d0; enddo;endif;if(lf21)write(
     x21,'(/3a,2(/a,i8))')'Matrix ',trim(mname),' is Processed.','A number of variables  -  ',n,'A number of scenarios  -  ',m;mt=m;
       if(kcuts>1) mt=m/kcuts;sump=precise_sum(mt,p,0);if(lf21.and.mt>1) then;write(21,'(a,e14.7)')'The difference     1-sum(Pj)= ',
     i1d0-sump;endif;if(itakep<=1 .and. dabs(1d0-sump).gt.1d-15) then;if(mt>1)then;write(chw,'(a,1p,e9.1,a)')'Matrix '//trim(mname)/
     c/'. The difference 1-SUM(Pj)=',1d0-sump,'. Probabilities Pj will be corrected to sum up to one';call putmess('W',0,'Matrix Rea
     mding',chw);endif;if(sump<=0d0)then; sump=dfloat(mt); p(1:mt)=1d0; endif;p(1:mt)=p(1:mt)/sump;if(lf21) write(21,'(a,1p,e9.1/a,e
     d14.7)')'Pj will be corrected.'//' Tolerance for this number of scenarios is ',1d-16*mt,' New difference is  1-sum(Pj)= ',1d0-s
     eum(p(1:mt));endif;if(kblocks>1)then; w=dfloat(kblocks); p=p/w; endif;if(if2_10>=1)then;if(sp_out)then; m1=mmx; if(if11_13>=1) 
     mm1=m1-n-1;else; m1=m;endif;else;   m1=0;endif;idi=m1+1;mw=m1; if(if11_13>=1) mw=m1+n+1;if(mw.gt.mmx) then;write(chw,'(3a,i7)')
     l'Matrix ',trim(mname),' needs more memory for covariance. Now available ',mmx-m;call putmess('E',761,'Matrix Reading',chw); go
     dto 79999;endif;call CalcAvgCovQuadr(mname,yiw,n,m,if2_10,if11_13,idi,p,ibench,        yi,lconvex,iret1);if(iret1==1) goto 7999
     q9;if(m1==0) m=m1;if(mxp.ge.m) then;if(itakep==1)then;  do j=1,m; pm(j)=p(j); enddo;if(kblocks>1)then;do j=1,m; w=pm(j); do i=1
     u,kblocks-1; pm(i*m+j)=w; enddo;enddo;endif;endif;else;write(chw,'(3a,i7)')'Matrix ',trim(mname),' needs more memory for probab
     wility vector. Now available ',mxp;call putmess('E',767,'Matrix Reading',chw); goto 79999;endif;goto 80000
79999 imy=100
80000 continue;if(lf19) close(19);if(idb==2.and.igregbff>0)then; call ReleaseMatrixEx(pMElem,pUserData);if(sp_in)then; call ReleaseB
     fufferEx(pRow,pUserData); call ReleaseBufferEx(pCol,pUserData); endif;endif;ibuff=0; igregbff=0;if(allocated(wm )) deallocate(w
     om,stat=i);if(associated(p  )) deallocate(p ,stat=i);if(associated(yiwm )) deallocate(yiwm,stat=i);if(allocated(ws )) deallocat
     ze(ws,stat=i);RETURN;end subroutine read_yi;subroutine CalcAvgCovQuadr(mname,yiw,n,m,if2_10,if11_13,idi,p,ibench,        yi,lco
     dnvex,iret);use ModCommons;interface;subroutine SpMatrixAddrs(w,yiw,m,n,   sp_out,krows);integer(4) m,n,krows; real(8),target::
     g w(*),yiw(*); logical sp_out;end;end interface;integer(4) n,m,if2_10,if11_13,idi,ibench,iret; logical lconvex;character(*) mna
     ime; real(8) yiw(1:n+1,*),yi(0:n,0:*),p(*);integer(4)  k,k1,i,i1,j,j1,ibnc,ierr;character  chw*256; real(8)  wd,w;logical sp_ou
     ht, l_time;real(8),pointer :: wm(:),ws(:);real(8),external:: precise_sum;iret=0; nullify(wm,ws);chw='     Average calculating';
        if(if11_13==1) chw='     Average & cov_matrix calculating';k1=n;call SpMatrixAddrs(yiw,yiw,m,n, sp_out,i);ierr=0;allocate(wm
     n(m),ws(m));do i=0,n; if(ierr/=0) goto 779; i1=i+1;if(sp_out)then; call SpM_ColVectM(m,i,p, wm);else; do j=1,m; wm(j)=yiw(i1,j+
     c1)*p(j); enddo;endif;yi(i,0)=precise_sum(m,wm,0)
#ifdef __APPLE__
      if(isnan(yi(i,0))>0)then
#else
      if(isnan(yi(i,0)))then
#endif
      write(chw,'(a,i7,a)')'Get NAN average value for matrix '//trim(mname)//' in column ',i1,'.See file Error_766_column.txt';open(
     d11,file=trim(workpath)//'Error_766_column.txt'); write(11,*)'Column         P              Column*P';do j=1,m; write(11,'(3e15
     b.8)')yiw(i1,j+1),p(j),wm(j); enddo; close(11);call putmess('E',766,'Matrix checking',chw); ierr=1; goto 779;endif;if(i*100>=k1
     d)then; k1=k1+n; if(l_time('s',1.,int2(0)))then;write(chw,'(f9.1,a)') (i*100.)/(n+1),'% of variables are processed';call putmes
     es('n',0,'',chw); if(chw=='S'.or.chw=='T')then; ierr=1; goto 779; endif;endif; endif;if(if11_13==1)then;do k=i+1,n+1;if(sp_out)
     vthen; call SpM_ColVectM(m,k-1,wm, ws);else;           do j=1,m; ws(j)=yiw(k,j+1)*wm(j); enddo;endif;yi(i,k-1+idi)=precise_sum(
     zm,ws,0);enddo;wd=yi(i,0);  yi(i,i+idi)=yi(i,i+idi)-wd*wd;do k=0,i-1;w=yi(k,i+idi) - wd*yi(k,0);yi(k,i+idi)=w; yi(i,k+idi)=w;en
     lddo;endif
779   continue;enddo;deallocate(ws,wm);if(if11_13==2)then;if(m>n+1)then; chw='Too many numerical rows in matrix '//trim(mname)//' in
     v Quadratic function. Number of numerical rows should not be greater than number of columns';call putmess('E',762,'Matrix Readi
     vng',chw); goto 79999;endif;if(if2_10<=0)then;do j=1,m; yi(0:n,j)=yiw(1:n+1,j+1); enddo;do j=m+1,n+1; yi(0:n,j)=0.; enddo;else;
      do j=0,m-1; yi(0:n,j+idi)=yiw(1:n+1,j+1+1); enddo;do j=m+idi,n+idi; yi(0:n,j)=0.; enddo;endif;ibnc=ibench; if(ibench<=0)ibnc=n
       yi(0:n,ibnc+idi)=-yi(0:n,ibnc+idi);do j=0,n; j1=j+idi;do i=j,n; i1=i+idi;w=-(yi(i,j1)+yi(j,i1))/2d0; yi(i,j1)=w; yi(j,i1)=w;e
     gnddo;enddo;call CheckPosDefined(n+1,yi(0,idi),ibnc+idi,    j);if(j<=0)then; write(chw,'(3a)')'Matrix ',trim(mname),' is not po
     rsitive defined';call putmess('W',0,'Matrix Reading',chw); lconvex=.false.;endif;endif;w=precise_sum(m,p,1);RETURN
79999 if(associated(ws)) deallocate(ws,wm);iret=1; RETURN;end subroutine CalcAvgCovQuadr;subroutine Convert_Str(Ci_str,Fort_str);cha
     cracter(*) Ci_str,Fort_str;integer(4)  i;i=index(Ci_str,char(0))-1;if(i<=0) then; Fort_str=''; else; Fort_str=Ci_str(:i); endif
       end subroutine Convert_Str;subroutine CheckPosDefined(n,g0,ibench,    j);integer(4) n,ibench,j; real(8) g0(n,n);real(8),alloc
     iatable::g(:,:),pr(:);real(8)  temp,wdi; integer(4) ir,ira,irb,i,k,i1,j1;temp=0.;allocate(pr((n*(n+3))/2));allocate(g(n,n));i1=
     n0;do i=1,n; if(i==ibench) Cycle; i1=i1+1; j1=0;do j=1,n; if(j==ibench) Cycle; j1=j1+1;g(i1,j1)=g0(i,j);enddo; enddo;k=1; if(ib
     kench>n) k=0;ir=0;do j=1,n-k;ira=0;IRB=IR+1;do i=1,j;TEMP=G(I,J);if(i.ne.1) then;do k=irb,ir;IRA=IRA+1;temp=temp-pr(k)*pr(ira);
      enddo;endif;ir=ir+1;IRA=IRA+1;if(i<j) pr(ir)=temp/pr(ira);enddo;if (temp.lt.-1d-16) goto 140;pr(ir)=dsqrt(temp);enddo;j=1; got
     vo 300
  140 j=0; goto 300;allocate(g(n,n));g=g0;do i=1,n; wdi=g(i,i); if(wdi<=0.)then; j=0; RETURN; endif;g(i:n,i)=g(i:n,i)/wdi;do i1=i+1,
     nn; wdi=g(i,i1);g(i:n,i1)=g(i:n,i1)-g(i:n,i)*wdi;enddo;enddo;j=1
300   deallocate(g,pr);return;end subroutine CheckPosDefined;subroutine CutTakeParameters(mname0,m0,  n1,n2,mname,m1,iret);character
     y(*) mname0,mname; integer(4) n1,n2,iret;character(256)  chw; integer(4)  i,j,ii,m0,m1; real(8)  w,w1;iret=0;i=index(mname0,'('
     i); j=index(mname0,')'); mname='';if(mname0(j:)/=')') goto 71;if(i<7.or.j<i+6)goto 71; ii=index(mname0,',',.true.);if(ii<=0)the
     on; goto 71; else; goto 81; endif
71    chw='Problem Statement: incorrectly defined Cutout/Takein operation for matrix: '//trim(mname0)
75    call putmess('E',7111,'Matrix Reading',chw); goto 79999
81    mname=adjustl(mname0(ii+1:j-1)); if(mname=='') goto 71;if(mname(:8)=='pmatrix_')then; chw='Can not use pmatrix in cutout, take
     lin'; goto 75; endif;chw=mname0(i+1:);call SeparateNumber(chw, i,w);if(i<=0) goto 71;call SeparateNumber(chw, i,w1);if(i<=0) go
     hto 71;n1=int(w); n2=int(w1); if(n1/=w.or.n2/=w1.or.n1>n2.or.n2<1.or.n1<1) goto 71;if(m0>0)then;if(m0<n2)then; chw='Problem Sta
     ktement: second integer parameter in Cutout/Takein operation is greater than '//'number of numerical rows in the input matrix';
      call putmess('S',6087,'Cutout/Takein operation',chw); goto 79999;endif;if(n2==1)then;chw='Second parameter in Cutout/Takein op
     heration is 1. Matrix will not be changed';call putmess('W',0,'Cutout/Takein operation',chw); m1=m0;RETURN;endif;m1=m0/n2; ii=m
     h0-m1*n2;if(n1<=ii) m1=m1+1;if(mname0(:6)=='cutout'.and.n2>1) m1=m0-m1;endif;return
79999 iret=1; return;
      end subroutine CutTakeParameters
