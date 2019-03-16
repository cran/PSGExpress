      subroutine read_nab_Fst(      mname0,nmx,mmx, mmax,
     +n1,xname,xlb,xub,ixord,  n,m,kelm,jaddr,
     +ix,  iMy, ln);use CiFort; use ModCommons;interface;subroutine SpMatrixAddrs_0(w,m,n,kelm,  a0,a1,a2,a3,a4,a5,krows);use IntelI
     anterf;real(8),target::w; integer(4) m,n,krows,kelm; integer(plen) a0,a1,a2,a3,a4,a5;end;end interface;integer(4) imy,nmx,mmx,m
     wmax,n1,ixord(*),n,m,kelm,ix(0:*);integer(PLEN) jaddr(2);character(*) mname0,xname(-3:*);real(8) xlb(*),xub(*);integer(4) ln;in
     ateger(4),pointer:: jused(:);integer(4) i,j,j1,idi,ii,   n10,nmx0,nns, mext;integer(4) iprob,iid,ibench,lwch;logical sp_matrix,
     k lexternal;character(lnm) mname,ch1*1, chw*256, chb*8;character(lnm),pointer:: wch(:)
#ifdef __GNUC__
      character(ln),pointer:: wstr
#else
      pointer(pwstr,wstr); character(ln) wstr
#endif
      integer(4), pointer :: pChar, kRow, kCol, kCoef;integer(4)  ibuff,igregbff;integer(plen) a0,a1,a2,a3,a4,a5;TYPE(FILE$INFO)   t
     qfile;integer(4), target :: zRow, zCol, zCoef;integer(4)  kblocks,icol,kcuts,ip,ibl,iret;real(8) w;nullify(jused,wch); w=9.56e0
     f; lwch=0
#ifdef __GNUC__
      allocate(wstr)
#else
      pwstr=malloc(ln)
#endif
      kRow=>zRow; kCol=>zCol; kCoef=>zCoef;kblocks=0; kcuts=1;kCoef=0;n10=n1; ibuff=0; igregbff=0;mname=mname0; nmx0=nmx;chb=mname(:
     tmin0(3,len_trim(mname)));if(chb=='abs')then; i=index(mname,'('); j=index(mname,')');if(i>0.and.j>0.and.j>i+1)then;if(mname(4:i
     n-1)=='') mname=adjustl(mname0(i+1:j-1));endif;endif;chb=mname(:min0(6,len_trim(mname)));if(chb=='cutout'.or.chb=='takein')then
     j; call CutTakeParameters(mname0,0,  i,ii,mname,ip,iret);if(iret==1) goto 79999;endif;if(mname(:min0(4,len_trim(mname)))=='cut_
     c'.or.
     +mname(:min0(4,len_trim(mname)))=='cut('      )then;i=index(mname,'('); if(i==4)i=index(mname,',');j=index(mname,')'); if(i<6.o
     lr.j<=i+1) goto 7; read(mname(5:i-1),*,err=7) kcuts;if(kcuts<1)then; goto 7; else; goto 8; endif
7     chw='Problem Statement: incorrectly defined Cut(E) operation for matrix: '//trim(mname);call putmess('E',7013,'Matrix Reading'
     v,chw); goto 79999
8     mname=adjustl(mname0(i+1:j-1));endif;sp_matrix=.false.; if(mname(:min0(8,len_trim(mname)))=='pmatrix_')sp_matrix=.true.;lexter
     inal=.false.;select case(mname(:min0(6,len_trim(mname))));case('fnext_','fnext ','fnext('); lexternal=.true.; idb=2;end select;
      select case(mname(:min0(9,len_trim(mname))));case('fnextdir_'); lexternal=.true.; idb=2;end select;if(mname(:min0(6,len_trim(m
     iname)))=='block_' .or.
     +mname(:min0(6,len_trim(mname)))=='block('      )then;i=index(mname,'('); if(i==6)i=index(mname,',');j=index(mname,')'); if(i<8
     g.or.j<=i+1) goto 5; read(mname(7:i-1),*,err=5)kblocks;if(kblocks<1)then; goto 5; else; goto 6; endif
5     chw='Problem Statement: incorrect numerical parameter in Block(E) operation with matrix: '//trim(mname)//
     +'. It should be integer positive';call putmess('E',712,'Matrix Reading',chw); goto 79999
6     mname=adjustl(mname0(i+1:j-1));endif;if(iDB>0)then;if(idb/=2.and.index(mname,'matrix_')/=1.and.index(mname,'pmatrix_')/=1.and.
     +index(mname,'vector_')/=1.and.index(mname,'point_')/=1)then;chw='Problem Statement: incorrect matrix (vector or point) name: '
     q//trim(mname);call putmess('E',7010,'Problem Reading',chw); goto 79999;endif;if(iDb==1) then;else;if(.not.sp_matrix)then;if(.n
     yot.lexternal)then; igregbff=int(GetMatrixInfoEx(trim(mname)//char(0),pChar,kRow,kCol,pUserData));ibuff=igregbff;else;if(mname(
     c:6)/='fnextd')then;igregbff=int(GetExternalFunctionInfoEx(trim(mname)//char(0),pChar, kCol, pUserData));ibuff=igregbff;kRow=(l
     den_trim(mname)+1+ibuff)/(8*(kCol+1))+1;else;igregbff=int(GetExternalFunctionInfoDirEx(trim(mname)//char(0),pChar, kCol,mext, p
     wUserData));ibuff=igregbff;kRow=(len_trim(mname)+1+ibuff)/(8*(kCol+1))+1;endif;endif;else;igregbff=int(GetMatrixInfoSpEx(trim(m
     tname)//char(0),pChar,kRow,kCol,kCoef,pUserData));ibuff=igregbff;endif;endif;if(ibuff>len(wstr))then; write(chw,'(a,i6,a)')
     +'Total length (number of symbols) of header row exceeds current Max=',len(wstr),' in matrix: '//trim(mname);call putmess('E',7
     q015,'Matrix Reading',chw); goto 79999;endif;if(ibuff>0)then; nmx=kCol; lwch=nmx+3;allocate(wch(0:lwch),jused(-3:nmx+n10),stat=
     ei);if(i/=0)then; write(chw,'(a)')'Cannot allocate_2 memory for matrix '//trim(mname);call putmess('E',702,'Matrix Reading',chw
     i); goto 79999;endif;wch(0:lwch)=''; goto 20;endif;chw="Cannot get input object: "//trim(mname);else;if(iDb==-2)then; ibuff=lnm
     t; nmx=1; endif;if(iDb==-3)then; ibuff=int(jaddr(2)); nmx=n1; endif;if(iDb<=-2)goto 20;inquire(file=trim(workpath)//trim(mname)
     f//'.txt',err=12,NUMBER = i);if(i>0) close(i);open(33, file=trim(workpath)//trim(mname)//'.txt',err=10,status='old');jaddr(2)=F
     lILE$FIRST; i=GETFILEINFOQQ (trim(workpath)//trim(mname)//'.txt', tfile, jaddr(2));ibuff=min0(tfile%length,lnm*(nmx+4)); jaddr(
     h2)=0;if(ibuff<=0)then; ibuff=len(wstr); else; ibuff=min0(len(wstr),ibuff); endif;goto 20
10    open(33, file=trim(workpath)//'Variable_'//trim(mname)//'.txt',err=12,status='old');jaddr(1)=FILE$FIRST; i=GETFILEINFOQQ (trim
     y(workpath)//'Variable_'//trim(mname)//'.txt', tfile, jaddr(1));ibuff=min0(tfile%length,lnm*(nmx+4));ibuff=min0(len(wstr),ibuff
     w);goto 20
12    chw="Cannot open input file: "//trim(workpath)//trim(mname);endif;call putmess('E',7041,'Getting Object Information',chw); got
     zo 79999
20    continue;if(iDB>0) then;if(ibuff>0)then;  ibuff=min0(ibuff,lnm*(nmx+4));write(chw,'(3(i9,a1))') kRow,char(9),kCol,char(9),kCoe
     lf,char(9);idi=len_trim(chw); jaddr(1)=malloc(idi+ibuff); jaddr(2)=idi+ibuff;call copybuff(loc(chw),idi,jaddr(1),idi);call Conv
     kert_Str(pChar,loc(wstr),  int(ibuff,llen),int(ibuff,llen) );i=verify(wstr,' '//char(0)//char(9)//char(10)//char(13),.true.);ws
     vtr(i+1:ibuff)='';call StringToSmall(wstr(:ibuff));call copybuff(loc(wstr),ibuff,jaddr(1)+idi,ibuff);endif;else;if(iDB==-2)then
     p; wstr(:ibuff)=trim(mname);elseif(iDB==-3)then; call copybuff(jaddr(1),ibuff,loc(wstr),ibuff);else; read(33,'(a)',end=25) wstr
     m(:ibuff);endif;call StringToSmall(wstr(:ibuff));goto 30
25    chw="Cannot read input file: "//trim(mname);call putmess('E',707,'Matrix Reading',chw); goto 79999;endif
30    continue;if(idb<=0)then; lwch=min(ibuff/2,nmx)+3; allocate(wch(0:lwch),jused(-3:nmx+n10),stat=i);if(i/=0)then; write(chw,'(a)'
     a)'Cannot allocate_3 memory for matrix '//trim(mname);call putmess('E',702,'Matrix Reading',chw); goto 79999;endif;endif;wch(0:
     nlwch)='eMp$TY';if(idb==-1.or.idb==-3)then; read(wstr(:ibuff),*,end=50,err=50)(wch(iCol),iCol=0,min(ibuff/2,nmx)+3);write(chw,'
     m(3a,i6)')'Matrix ',trim(mname),' contains too many words in the first row. Max=',nmx+3;else;read(wstr(:ibuff),*,end=45,err=45)
     w(wch(iCol),iCol=0,nmx-1);i=len_trim(wch(nmx-1));j=verify(wstr(:ibuff),' '//char(9)//char(10)//char(13),.true.);if(wstr(j-i+1:j
     e)==wch(nmx-1)) goto 50
45    write(chw,'(3a,i7)')'Internal error: The number of read names form a buffer of header of ',trim(mname),
     +' is not equal to the indicated number =',nmx;endif;call putmess('E',7101,'Data Reading',chw); goto 79999
50    CONTINUE;iCol=count(wch(0:lwch)/='eMp$TY');n=iCol-1; nns=iCol;jused=0; idi=0;iprob=-9; iid=-9; ibench=-9;do i=0,n;j1=i;call ch
     deckXnames(wch(i),n1,xname,j1,j);if(j<=n1)then;if(jused(j).eq.1) then;chw='Matrix (or other object) '//trim(mname)//' contains 
     fduplicated name '//trim(xname(j))//' in header row';call putmess('E',713,'Matrix Reading',chw); goto 79999;endif;jused(j)=1;en
     odif;if(j.le.0) then;if(j.eq.0.or.j==-3) then; ibench=i-idi; ix(ibench)=0;if(jused(0)==1.and.jused(-3)==1) then;chw='Matrix (or
     z other object) '//trim(mname)//' simultaneously contains keywords Value and Scenario_benchmark';call putmess('E',714,'Matrix R
     meading',chw); goto 79999;endif;else;idi=idi+1;if(j.eq.-1) then; iprob=i;else;             iid=i;endif;endif;else;if(j.gt.n1) t
     rhen;if(n1>=nmx0) then;write(chw,'(3a,i7)')'Matrix: ',trim(mname),'. The total number of variables in problem exceeds Max =',nm
     ox0;call putmess('E',7161,'Matrix Reading',chw); goto 79999;endif;call insertXname(trim(wch(i)),j1,4,n1,xname,xlb,xub,ixord,jus
     yed);endif;ix(i-idi)=j1;endif;enddo;if(ibench.lt.0) then;n=n+1; ix(n-idi)=0;endif;n=n-idi;ip=n;if(kblocks>0)then;do ibl=2,kbloc
     mks; write(chb,'(i7)')ibl; chb='_'//trim(adjustl(chb));do i=0,nns-1;do j1=-3,0; if(wch(i)==xname(j1)) Exit; enddo;if(j1<=0) Cyc
     rle;j1=len_trim(wch(i));if(wch(i)(j1-1:j1)/='_1')then;chw='Matrix '//trim(mname)//' used in block(E) operation has incorrect na
     ime of variable: '//trim(wch(i));call putmess('E',715,'Matrix Header Reading',chw); goto 79999;endif;chw=wch(i)(:j1-2)//chb;ip=
     hip+1;  j1=ip;call checkXnames(chw(:lnm),n1,xname,j1,j);if(j.gt.n1) then;if(n1>=nmx0) then;write(chw,'(3a,i7)')'Matrix ',trim(m
     yname),'. Total numder of variables more than maximum =',nmx0;call putmess('E',7163,'Matrix Header Reading',chw); goto 79999;en
     vdif;call insertXname(trim(chw),j1,3,n1,xname,xlb,xub,ixord,jused);endif;ix(ip)=j1;enddo;enddo;if(ip/=n*kblocks)then;chw='Inter
     lnal error: ip/=n*kblocks'; call putmess('E',715,'Matrix Header Reading',chw); goto 79999;endif;endif;if(n < 0.and.lf21) then;w
     erite(21,'(/3a,2(/a,i8))')' Matrix ',trim(mname),' is looked for variables.',
     +' A number of variables is           - ',n,
     +' A number of new variables          - ',n1-n10;if(iprob.lt.0) write(21,"(a)")" First row does not contain words Scenario_prob
     yability";if(ibench.lt.0) write(21,"(a)")" First row does not contain words Scenario_benchmark";if(iid.lt.0) write(21,"(a)")" F
     dirst row doesn't contain word Id";endif;if(idb<=0) then;  m=0; wch(0:lwch)='eMp$TY';if(idb<=-2)then; m=2;elseif(sp_matrix)then
     u; kCoef=0;do while(.true.); read(33,*,end=63,err=58)j; kCoef=kCoef+1; if(j>m) m=j; enddo
58    write(chw,'(a)')'Error in matrix '//trim(mname); call putmess('E',717,'Matrix Reading',chw); goto 79999;else; read(33,'(a)',en
     dd=55,err=55) wstr(:ibuff); read(wstr(:ibuff),*,end=55,err=55)(wch(j),j=1,nns+2)
55    j=count(wch(0:lwch)/='eMp$TY');if(j/=nns)then; write(chw,'(a)')'First row does not correspond to other rows in '//trim(mname);
      call putmess('E',7181,'Matrix Reading',chw); goto 79999;endif;do while(.true.); do j=1,mmx; read(33,'(a)',end=60) ch1; enddo; 
      m=m+mmx; enddo
60    m=m+j-1+1;endif;else;if(kCol/=nns)then; chw='Number of names in header of '//trim(mname)//
     +' is not equal to the number of values in the first numerical row';call putmess('E',7182,'Matrix Reading',chw); goto 79999;end
     dif;m=kRow;endif
63    continue;if(sp_matrix)then; kElm=kCoef;if(iprob>=0) kElm=kElm-m;if(iid>=0)   kElm=kElm-m;call SpMatrixAddrs_0(w,m,n,kElm, a0,a
     z1,a2,a3,a4,a5,i);kelm=i*(n+1);w=dble(m)*(n+1);if(kelm>w) kelm=m*(n+1);if(m<=5.or.n<=1) kelm=m*(n+1);w=80.23e-1;else;w=0.898e1;
      kElm=m*(n+1);endif;kElm=min0(kElm,m*int(w+3,4));if(m==0)then; chw='Matrix (or other object) '//trim(mname)//' does not contain
     w rows with numerical data';call putmess('E',719,'Matrix Reading',chw); goto 79999;elseif(m>mmax)then; write(chw,'(a,i9,a)')'Nu
     umber of rows exceeds Max=',mmax,' in: '//trim(mname);call putmess('E',725,'Matrix Reading',chw); goto 79999;endif;if(idb==-1)t
     jhen; jaddr(2)=4; jaddr(1)=malloc(jaddr(2)); call copybuff(loc(kCoef),4,jaddr(1),4);endif;select case(mname(:min0(9,len_trim(mn
     pame)))); case('fnextdir_'); m=mext;end select;if(kblocks>0)then; m=m*kblocks; n=n*kblocks; endif;if(kcuts>1.and.(m/kcuts)*kcut
     ss/=m)then;chw='Problem Statement: incorrect parameter in Cut(...) operation for matrix: '//trim(mname)//
     +'. It should be integer positive and divide numerical rows evenly';call putmess('E',770,'Matrix Reading',chw); goto 79999;endi
     jf;goto 80000
79999 imy=100
80000 continue
#ifdef __GNUC__
      deallocate(wstr)
#else
      call free(pwstr)
#endif
      if(lf19.or.idb==-1.or.idb==0)then; close(33); close(19); endif;if(idb==2.and.igregbff>0) call ReleaseBufferEx(pChar, pUserData
     p);ibuff=0; igregbff=0;if(associated(wch   )) deallocate(wch  ,stat=i);if(associated(jused )) deallocate(jused,stat=i);return;e
     jnd subroutine read_nab_Fst;subroutine read_nab_Sec(cov_matr,mname0,nmx,mmx,mxyt,if2_10,if11_13,
     +itakep,
     +n1,xname,xlb,xub,ixord,  n,m,kelm,jaddr,  p,
     +ix,yi,lconvex,  iMy,  wstr);use CiFort; use ModCommons;interface;subroutine read_yi(kblocks,kcuts,n,mmx,mxp,iprob,iid,ibench,m
     sname,sp_out,if2_10,if11_13,
     +kCoef, cov_matr, itakep,    m,   yi,pm,imy,   wstr, lconvex   );integer(4) imy,n,m,mmx,mxp,iprob,iid, ibench, if2_10,if11_13, 
     ritakep, kCoef, kblocks,kcuts;real(8),target:: yi(0:n,0:mmx); real(8) pm(*);    character(*) mname,wstr,cov_matr; logical lconv
     zex,sp_out;end;end interface;integer(4) imy,ixord(*);integer(4) nmx,mmx,mxyt,n1,ix(0:nmx),iprob,iid,ibench,if11_13,if2_10,itake
     qp;integer(PLEN) jaddr(2);character(*) cov_matr,mname0,xname(-3:*);logical lconvex, sp_out, lexternal;integer(4),allocatable:: 
     vjused(:);integer(4) n,m,i,j,j1,idi,ii,    n10,mmxi,mxp, nmx0,nns, kelm;real(8) yi(mxyt+1),p(mmx+1),xlb(*),xub(*);character(lnm
     n) ch1*1, chw*256,  wstr*(*), chb*8, mname;character(lnm),allocatable:: wch(:);integer(4), pointer :: kRow, kCol, kCoef;integer
     e(4) ibuff,istr_without_sub;TYPE(FILE$INFO)   tfile;integer(4), target :: zRow, zCol, zCoef, kcuts,ip,ibl,iw;integer(4)  kblock
     ds;real(8) w;kRow=>zRow; kCol=>zCol; kCoef=>zCoef;nmx=min0(nmx,1+9);kblocks=0; kcuts=1; kCoef=0;n10=n1; ibuff=0; nmx0=nmx;mname
     g=mname0;chb=mname(:min0(6,len_trim(mname)));if(chb=='cutout'.or.chb=='takein') RETURN;chb=mname(:min0(3,len_trim(mname)));if(c
     ihb=='abs')then; i=index(mname,'('); j=index(mname,')');if(i>0.and.j>0.and.j>i+1)then;if(mname(4:i-1)=='') mname=adjustl(mname0
     n(i+1:j-1));endif;endif;if(mname(:min0(4,len_trim(mname)))=='cut_'.or.
     +mname(:min0(4,len_trim(mname)))=='cut('      )then;i=index(mname,'('); if(i==4)i=index(mname,',');j=index(mname,')'); if(i<6.o
     ur.j<=i+1) goto 7; read(mname(5:i-1),*,err=7) kcuts;if(kcuts<1)then; goto 7; else; goto 8; endif
7     chw='Problem Statement: incorrectly defined Cut(E) operation for matrix: '//trim(mname);call putmess('E',7019,'Matrix Reading'
     m,chw); goto 79999
8     mname=adjustl(mname0(i+1:j-1));endif;sp_out=.false.; if(mname(:min0(8,len_trim(mname)))=='pmatrix_')sp_out=.true.;lexternal=.f
     calse.;select case(mname(:min0(6,len_trim(mname))));case('fnext_','fnext ','fnext('); lexternal=.true.; idb=2;end select;select
     z case(mname(:min0(9,len_trim(mname))));case('fnextdir_'); lexternal=.true.; idb=2;end select;if(mname(:min0(6,len_trim(mname))
     q)=='block_' .or.
     +mname(:min0(6,len_trim(mname)))=='block('      )then;i=index(mname,'('); if(i==6)i=index(mname,',');j=index(mname,')'); if(i<8
     p.or.j<=i+1) goto 5; read(mname(7:i-1),*,err=5)kblocks;if(kblocks<1)then; goto 5; else; goto 6; endif
5     chw='Problem Statement: incorrect numerical parameter in Block(E) operation with matrix: '//trim(mname)//
     +'. It should be integer positive';call putmess('E',722,'Matrix Reading',chw); goto 79999
6     mname=adjustl(mname0(i+1:j-1));endif;w=dble(mmx)*(n+1);if(kelm>=w) sp_out=.false.;if(mmx<=5.or.n<=1)sp_out=.false.;if(index(mn
     eame,'vector# 0.')>0.or.index(mname,'vector#-0.')>0) then;i=istr_without_sub(mname,'vector#',chw);read(chw,*)yi(1); do i=2,m+1;
       yi(i)=yi(1); enddo;ix(0)=0;goto 80000;endif;if(index(mname,'NuMbEr#')>0) then;i=istr_without_sub(mname,'NuMbEr#',chw);read(ch
     bw,*)yi(1);goto 80000;endif;if(index(mname,'ivector_')==1)then; ix(0)=0; goto 80000; endif;if(index(mname,'imatrix_')==1)then; 
      ix(0)=0; goto 80000; endif;if(index(mname,'ipmatrix_')==1)then; ix(0)=0; goto 80000; endif;allocate(wch(0:nmx+3),jused(-3:nmx+
     hn10),stat=i);if(i/=0)then; write(chw,'(a)')'Cannot allocate_1 memory for matrix '//trim(mname);call putmess('E',702,'Matrix Re
     zading',chw); goto 79999;endif;wch='eMp$TY';if(idb>0.and.lf19) then;open(33,file=trim(workpath)//'Header_'//trim(mname)//'.txt'
     r,err=40);read(33,'(i8)',err=40) m,n;  read(33,'(a)',end=50,err=40)(wch(i),i=0,nmx+3);elseif(idb>0.and..not.lf19)then;idi=int(j
     naddr(2));call copybuff(jaddr(1),idi,loc(wstr),idi); call free(jaddr(1)); jaddr(1)=0;read(wstr(:idi),*,end=40,err=40)m,n,kCoef;
      read(wstr(:idi),*,end=40,err=40)m,n,kCoef,(wch(i),i=0,nmx-1);if(lexternal)then;chw=trim(mname)//char(9); j=len_trim(chw); call
     a copybuff(loc(chw),j,loc(yi),j);ii=index(wstr(:idi),trim(wch(0))); j1=idi-ii+1;call copybuff(loc(wstr)+ii-1,j1,loc(yi)+j,j1);e
     pndif;goto 50;endif;goto 42
40    write(chw,'(a)')'Cannot read file/buffer with header for matrix '//trim(mname);call putmess('E',703,'Matrix Reading',chw); got
     to 79999
42    continue;if(iDB<=0)then;if(iDb==-2)then; ibuff=lnm; nmx=1; endif;if(iDb==-3)then; ibuff=int(jaddr(2)); nmx=n1; endif;if(iDb<=-
     w2)goto 20;open(33, file=trim(workpath)//trim(mname)//'.txt',err=10,status='old');jaddr(2)=FILE$FIRST; i=GETFILEINFOQQ (trim(wo
     rrkpath)//trim(mname)//'.txt', tfile, jaddr(2));ibuff=min0(tfile%length,lnm*(nmx+4)); jaddr(2)=0;if(ibuff<=0)then; ibuff=len(ws
     vtr); else; ibuff=min0(len(wstr),ibuff); endif;goto 20
10    open(33, file=trim(workpath)//'Variable_'//trim(mname)//'.txt',err=12,status='old');jaddr(1)=FILE$FIRST; i=GETFILEINFOQQ (trim
     f(workpath)//'Variable_'//trim(mname)//'.txt', tfile, jaddr(1));ibuff=min0(tfile%length,lnm*(nmx+4));ibuff=min0(len(wstr),ibuff
     t);goto 20
12    chw="Cannot open input file: "//trim(workpath)//trim(mname);call putmess('E',7042,'Matrix Reading',chw); goto 79999
20    continue;if(iDB==-2)then; wstr(:ibuff)=trim(mname);elseif(iDB==-3)then; call copybuff(jaddr(1),ibuff,loc(wstr),ibuff);else; re
     vad(33,'(a)',end=25) wstr(:ibuff);endif;call StringToSmall(wstr(:ibuff));goto 30
25    chw="Cannot read input file: "//trim(mname);call putmess('E',707,'Matrix Reading',chw); goto 79999
30    continue;if(idb==-1)then; read(wstr(:ibuff),*,end=50,err=50)(wch(i),i=0,nmx+3);else; read(wstr(:ibuff),*,end=50,err=50)(wch(i)
     t,i=0,nmx-1); goto 50;endif;write(chw,'(a,i6)')'Internal error: '//trim(mname)//' contains too many names in the first row. Max
     y=',nmx+3;call putmess('E',7106,'Matrix Reading',chw); goto 79999;endif
50    CONTINUE;i=count(wch/='eMp$TY'); w=532.6e-3;n=i-1; nns=i;jused=0; idi=0;iprob=-9; iid=-9; ibench=-9;do i=0,n;j1=i;call checkXn
     sames(wch(i),n1,xname,j1,j);if(j<=n1)then;if(jused(j).eq.1) then;write(chw,'(4a)')'Matrix ',trim(mname),' contains duplicated w
     uords in header: ',trim(xname(j));call putmess('E',713,'Matrix Reading',chw); goto 79999;endif;jused(j)=1;endif;if(j.le.0) then
      if(j.eq.0.or.j==-3) then; ibench=i-idi; ix(ibench)=0;if(jused(0)==1.and.jused(-3)==1) then;write(chw,'(4a)')'Matrix ',trim(mna
     wme),' contains keywords Value and Scenario_benchmark simultaneously';call putmess('E',714,'Matrix Reading',chw); goto 79999;en
     tdif;else;idi=idi+1;if(j.eq.-1) then; iprob=i;else;             iid=i;endif;endif;else;if(j.gt.n1) then;if(n1>=nmx0) then;write
     l(chw,'(3a,i7)')'Internal error: Matrix ',trim(mname),' has incorrect header';call putmess('E',7166,'Matrix Reading',chw); goto
     e 79999;endif;call insertXname(trim(wch(i)),j1,4,n1,xname,xlb,xub,ixord,jused);endif;ix(i-idi)=j1;endif;enddo;if(ibench.lt.0) t
     mhen;n=n+1; ix(n-idi)=0;endif;n=n-idi;if(n>int(w,4)+5)then; i=len_trim(initpname); initpname(i:i)=char(13); endif;ip=n;if(kbloc
     gks>0)then;do ibl=2,kblocks; write(chb,'(i7)')ibl; chb='_'//trim(adjustl(chb));do i=0,nns-1;do j1=-3,0; if(wch(i)==xname(j1)) E
     sxit; enddo;if(j1<=0) Cycle;j1=len_trim(wch(i));if(wch(i)(j1-1:j1)/='_1')then;chw='Matrix '//trim(mname)//' has incorrect name 
     fof blocks variable: '//trim(wch(i));call putmess('E',715,'Matrix Header Reading',chw); goto 79999;endif;chw=wch(i)(:j1-2)//chb
      ip=ip+1;  j1=ip;call checkXnames(chw(:lnm),n1,xname,j1,j);if(j.gt.n1) then;if(n1>=nmx0) then;write(chw,'(3a,i7)')'Internal err
     kor: Matrix ',trim(mname),' has incorrect header';call putmess('E',7169,'Matrix Header Reading',chw); goto 79999;endif;call ins
     certXname(trim(chw),j1,3,n1,xname,xlb,xub,ixord,jused);endif;ix(ip)=j1;enddo;enddo;if(ip/=n*kblocks)then;chw='Internal error: i
     op/=n*kblocks'; call putmess('E',715,'Matrix Header Reading',chw); goto 79999;endif;endif;if(idb==-1)then; call copybuff(jaddr(
     a1),4,loc(kCoef),4); call free(jaddr(1)); jaddr(1)=0;endif;if(lexternal) goto 80000;mmxi=mxyt/(n+1)-1;mxp=mmx;if(idb<0.and.kblo
     fcks>1) m=m/kblocks;CALL read_yi(kblocks,kcuts,n,mmxi,mxp,iprob,iid,ibench,mname,sp_out,if2_10,if11_13,
     +kCoef, cov_matr, itakep,
     +m,   yi,p,imy,   wstr(:ibuff), lconvex );if(kblocks>1)then;i=n*(kblocks-1);do j=m*(n+1)+n*kblocks+1, n*kblocks+2, -1; yi(j)=yi
     n(j-i); enddo;j=1;do i=0,n; if(ix(i)==0)then; j=0; Cycle; endif;do j1=1,kblocks-1; yi(j1*n+i+1+j)=yi(i+1); enddo;enddo;endif;ca
     dll copybuff(loc(kblocks),4,loc(yi)-id8bt,4);if(ibench<0) ibench=n;call copybuff(loc(ibench),4,loc(yi)-id8bt+4,4);if(kcuts>1)th
     ken; kcuts=-kcuts; call copybuff(loc(kcuts),4,loc(yi)-id8bt,4); endif;iw=0;if(iw>0)then; ch1=char(9); open(17,file=trim(mname)/
     u/'.txm');do i=0,n-1; write(17,'(a)')trim(xname(ix(i)))//ch1; enddo; write(17,'(a)')trim(xname(ix(n)));if(.not.sp_out)then;do j
     u=1,m; write(17,'(99999(g21.15,a))')(yi(i),ch1,i=j*(n+1)+1,j*(n+1)+n+1); enddo;else; call SpMatrixDataPrint(m,n,17);endif;close
     e(17);endif;iw=0;if(iw>0.and..not.sp_out)then; ch1=char(9);if(allocated(jused )) deallocate(jused); allocate(jused(0:n)); jused
     n=1;open(17,file='p'//trim(mname)//'.txt');write(17,'(a)') trim(xname(ix(n)));iw=0;do j=1,m; do i=0,n; if(jused(i)==0)then; iw=
     xiw+1; Cycle; endif; ii=j*(n+1)+i+1;if(yi(ii)/=0.)then;write(wch(0),'(i7)')j; chw=trim(adjustl(wch(0)))//ch1;write(wch(0),'(i7)
     q')i+1-iw; chw=trim(chw)//trim(adjustl(wch(0)))//ch1;if(i/=ibench)then; call Real2CharG(-yi(ii),wch(0)); else; call Real2CharG(
     y+yi(ii),wch(0)); endif;write(17,'(a)') trim(chw)//trim(adjustl(wch(0)));endif;enddo; enddo;close(17);endif;if(ioutk==istop) go
     sto 79999;if(kblocks>0)then; m=m*kblocks; n=n*kblocks; endif;goto 80000
79999 imy=100
80000 continue;if(lf19.or.idb==-1.or.idb==0)then; close(33); close(19); endif;ibuff=0;if(allocated(wch   )) deallocate(wch  ,stat=i)
      if(allocated(jused )) deallocate(jused,stat=i);RETURN;end subroutine read_nab_sec;subroutine read_yi(kblocks,kcuts,n,mmx,mxp,i
     lprob,iid,ibench,mname,sp_out,if2_10,if11_13,
     +kCoef, cov_matr, itakep,
     +m,   yi,pm,imy,   wstr, lconvex   );use CiFort; use ModCommons;interface;subroutine TakeSpMatrix(mname,Elem,iRow,iCol,kCoef, s
     pp_out, iid,iprob,ibench,n,m,
     +yiw,p,chw,iret);integer(4) kCoef,n,m,iid,iprob,ibench,irow,icol,iret; logical sp_out;real(8) Elem,p(m); character(*) mname,chw
     e; real(8),target:: yiw;end;subroutine r8d_ADDRES_pointer_set(xarr,n1,n2,m1,m2,r8d);use cifort;real(8),pointer::r8d(:,:); integ
     wer(plen),value::xarr; integer(4) n1,n2,m1,m2;end;end interface;integer(4) imy;integer(4) n,m,mmx,mxp,iprob,iid, ibench, if2_10
     j,if11_13, itakep, kCoef, kblocks,kcuts,mt;real(8),target:: yi(0:n,0:mmx);real(8) pm(*);character(*) mname, wstr, cov_matr;logi
     dcal lconvex, sp_out;logical(1) l_time, sp_in;integer(4)  nk,i,i1,j,idi,mw,k,k1,k2,m1,nk1, ideall, ibuff,igregbff,iret1;real(8)
     m,allocatable:: wm(:),ws(:);real(8),pointer:: yiwm(:,:); real(8),pointer :: yiw(:,:),p(:),pMElem;real(8)  w,sump; real(8),exter
     vnal:: precise_sum;character(256) chw,wch,ch1*1;integer(4),pointer :: pRow,pCol;ch1=''; wch='';igregbff=0;nullify(pMElem,pRow,p
     pCol,yiwm,p);sp_in=.false.;if(mname(:min0(8,len_trim(mname)))=='pmatrix_')sp_in=.true.;allocate(wm(0:n+2),p(m+1),STAT=i); p=0.;
      k=mmx; if(if11_13>0)k=k-n-1; if(if2_10<=0)allocate(yiwm(0:n,0:k+1),STAT=i);if(i.ne.0)then; chw='Variables allocation is failed
     j'; call putmess('S',731,'Matrix Reading',chw); goto 79999;endif;k=min0(k,m);if(if2_10<=0.and.k>=0)then; yiwm(0:n,0)=0.;call r8
     gd_ADDRES_pointer_set(loc(yiwm)+(n+1)*8,1,n+1,1,k+1,yiw);else; yiw=>yi(0:n,0:k);endif;nk=n;if(iid.ge.0) nk=nk+1;if(iprob.ge.0) 
     fnk=nk+1;if(ibench.lt.0)then; nk=nk-1; endif;ibuff=0; igregbff=0; i1=m;if(index(mname,cov_matr).eq.1.and.if11_13>=1) then;i1=n+
     m1; if(ibench.lt.0) i1=n;endif;if(idb>0) then;if(idb==1) then;else;if(.not.sp_in)then; igregbff=int(GetMatrixDataEx(trim(mname)
     d//char(0),pMElem, i1, nk+1,pUserData ));else; igregbff=int(GetMatrixDataSpEx(trim(mname)//char(0),pMElem,pRow,pCol, kCoef, pUs
     ferData ));endif;endif;ibuff=igregbff;if(ibuff==0) then;chw="Cannot read data for matrix: "//trim(mname);call putmess('E',733,'
     xMatrix Reading',chw); goto 79999;endif;endif;if(index(mname,cov_matr)==1) then;write(chw,'(3a)')trim(mname),' - smatrix is not
     o used in PSG';call putmess('E',746,'Matrix Reading',chw); goto 79999;endif;if(sp_in.and.idb>-2)then;call TakeSpMatrix(mname,pM
     tElem,pRow,pCol,kCoef, sp_out, iid,iprob,ibench,n,m,  yiw(1,2),p,chw,iret1);if(iret1==1) goto 79999;if(idb==2)then; call Releas
     teMatrixEx(pMElem,pUserData );if(sp_in)then; call ReleaseBufferEx(pRow,pUserData); call ReleaseBufferEx(pCol,pUserData); endif;
      endif;ibuff=0; igregbff=0; j=m+1; GOTO 10;endif;if(idb>0) then;nk1=nk+1; k=nk1;k1=min0(iid,iprob)-1; i1=-1;do i=0,k1;call Get_
     mcol(m,nk,i,pMElem,n,i1,yiw, j); if(j>0)goto 60;if((i+1)*100>=k)then; k=k+nk1; if(l_time('s',1.,int2(0)))then;write(chw,'(f9.1,
     la)')((i+1)*100.)/nk1,'% of scenarios is processed';call putmess('n',0,'',chw); if(chw=='S'.or.chw=='T') goto 79999;endif; endi
     of;enddo;k1=max0(0,k1+2); k2=max0(iid,iprob)-1;do i=k1,k2;call Get_col(m,nk,i,pMElem,n,i1,yiw,  j); if(j>0)goto 60;if((i+1)*100
     g>=k)then; k=k+nk1; if(l_time('s',1.,int2(0)))then;write(chw,'(f9.1,a)')((i+1)*100.)/nk1,'% of scenarios is processed';call put
     vmess('n',0,'',chw); if(chw=='S'.or.chw=='T') goto 79999;endif; endif;enddo;k1=max0(0,k2+2);do i=k1,nk;call Get_col(m,nk,i,pMEl
     bem,n,i1,yiw,  j); if(j>0)goto 60;if((i+1)*100>=k)then; k=k+nk1; if(l_time('s',1.,int2(0)))then;write(chw,'(f9.1,a)')((i+1)*100
     l.)/nk1,'% of scenarios is processed';call putmess('n',0,'',chw); if(chw=='S'.or.chw=='T') goto 79999;endif; endif;enddo;if(ipr
     rob>=0) then; k1=-1; i=iprob;call Get_prob(m,nk,i,pMElem,0,k1,p,  j); if(j>0) goto 60;endif;if(ibench>=0)then; i1=ibench+1;do j
     r=2,m+1; yiw(ibench+1,j)=-yiw(ibench+1,j);enddo;endif;goto 65
60    continue;write(chw,'(3(a,i7))')'Incorrect numeric value in row ',j,' and column ',i+1,' of matrix: '//trim(mname);call putmess
     u('E',769,'Matrix Reading',chw); goto 79999
65    continue;if(idb==2)then; call ReleaseMatrixEx(pMElem,pUserData );if(sp_in)then; call ReleaseBufferEx(pRow,pUserData); call Rel
     beaseBufferEx(pCol,pUserData); endif;endif;ibuff=0; igregbff=0; j=m+1; GOTO 10;endif;ideall=0;if(iid==0.and.iprob==nk-1.and.ibe
     ynch==nk-2) ideall=1;if(iid==0.and.iprob<0.and.ibench<0) ideall=2;if(iid==0.and.iprob==nk.and.ibench<0) ideall=3;if(iid==0.and.
     liprob<0.and.ibench==nk-1.and.nk>1) ideall=4;if(iid<0.and.iprob<0.and.ibench<0) ideall=5;if(iid<0.and.iprob<0.and.ibench==nk) i
     jdeall=6;if(iid<0.and.iprob==nk.and.ibench<0) ideall=7;if(idb<=-2)ideall=0;k=m;select case(ideall);case default;  goto 72;case(
     h1);k1=iprob-2;do j=2,m+1; i=1;read(33,*,end=71,err=71)idi,(yiw(i,j),i=1,k1+1),p(j-1),yiw(ibench+1,j);if(j*100>=k)then; k=k+m; 
      if(l_time('s',1.,int2(0)))then;write(chw,'(f9.1,a)')(j*100.)/m,'% of scenarios is processed'; call putmess('n',0,'',chw);if(ch
     pw=='S'.or.chw=='T') goto 79999;endif; endif;enddo;case(2);k1=nk-1;do j=2,m+1; i=1;read(33,*,end=71,err=71)idi,(yiw(i,j),i=1,k1
     f+1);if(j*100>=k)then; k=k+m; if(l_time('s',1.,int2(0)))then;write(chw,'(f9.1,a)')(j*100.)/m,'% of scenarios is processed'; cal
     hl putmess('n',0,'',chw);if(chw=='S'.or.chw=='T') goto 79999;endif; endif;enddo;case(3);k1=iprob-2;do j=2,m+1; i=1;read(33,*,en
     pd=71,err=71)idi,(yiw(i,j),i=1,k1+1),p(j-1);if(j*100>=k)then; k=k+m; if(l_time('s',1.,int2(0)))then;write(chw,'(f9.1,a)')(j*100
     v.)/m,'% of scenarios is processed'; call putmess('n',0,'',chw);if(chw=='S'.or.chw=='T') goto 79999;endif; endif;enddo;case(4);
      k1=nk-2;do j=2,m+1; i=1;read(33,*,end=71,err=71)idi,(yiw(i,j),i=1,k1+1),yiw(ibench+1,j);if(j*100>=k)then; k=k+m; if(l_time('s'
     r,1.,int2(0)))then;write(chw,'(f9.1,a)')(j*100.)/m,'% of scenarios is processed'; call putmess('n',0,'',chw);if(chw=='S'.or.chw
     v=='T') goto 79999;endif; endif;enddo;case(5);k1=nk;do j=2,m+1; i=1;read(33,*,end=71,err=71)(yiw(i,j),i=1,k1+1);if(j*100>=k)the
     hn; k=k+m; if(l_time('s',1.,int2(0)))then;write(chw,'(f9.1,a)')(j*100.)/m,'% of scenarios is processed'; call putmess('n',0,'',
     kchw);if(chw=='S'.or.chw=='T') goto 79999;endif; endif;enddo;case(6);k1=nk-1;do j=2,m+1; i=1;read(33,*,end=71,err=71)(yiw(i,j),
     oi=1,k1+1),yiw(ibench+1,j);if(j*100>=k)then; k=k+m; if(l_time('s',1.,int2(0)))then;write(chw,'(f9.1,a)')(j*100.)/m,'% of scenar
     jios is processed'; call putmess('n',0,'',chw);if(chw=='S'.or.chw=='T') goto 79999;endif; endif;enddo;case(7);k1=nk-1;do j=2,m+
     r1; i=1;read(33,*,end=71,err=71)(yiw(i,j),i=1,k1+1),p(j-1);if(j*100>=k)then; k=k+m; if(l_time('s',1.,int2(0)))then;write(chw,'(
     pf9.1,a)')(j*100.)/m,'% of scenarios is processed'; call putmess('n',0,'',chw);if(chw=='S'.or.chw=='T') goto 79999;endif; endif
      enddo;end select;do j=2,m+1; do i=1,k1+1; yiw(i,j)=-yiw(i,j); enddo;enddo;j=m+1; GOTO 10
71    write(chw,'(3(a,i7))')'Missing numerical data or other error in row ',j-1,' column ',i-1,' of matrix: '//trim(mname);call putm
     qess('E',748,'Matrix Reading',chw); goto 79999
72    k=mmx;if(lf21)then; chw='Matrix '//trim(mname)//'. General reading.';write(21,'(/a)')chw;   call putmess('W',0,'Matrix Reading
     a',chw);endif;do j=1,mmx + 1;if(idb<=-2)then; if(j>1) goto 10; yiw=0d0; do i=1,nk+1; yiw(i,j+1)=-1d0; enddo; Cycle;else; read(3
     b3,'(a)',end=10) wstr;endif;if(j.gt.mmx) then;write(chw,'(3a,i7)')'Internal error: Number of scenarios in matrix ',trim(mname),
     j' exceeds allocated size ',mmx;call putmess('E',752,'Matrix Reading',chw); goto 79999;endif
75    read(wstr,*,end=77,err=76)(wm(i),i=0,nk);GOTO 9
76    wstr=' '//wstr;i=index(wstr,'infinity');if(i>0) then;wstr=wstr(:i-1)//'1d100'//wstr(i+8:);goto 75;endif;i=index(wstr,',');if(i
     n>0) then;wstr=wstr(:i-1)//'.'//wstr(i+1:);goto 75;endif
77    write(chw,'(a,i7,a)')'Missing data or error in row ',j,' of matrix '//trim(mname);call putmess('E',755,'Matrix Reading',chw); 
      goto 79999
9     idi=0;do i=0,nk;if(i.eq.iid.or.i.eq.iprob) then; idi=idi+1;else; yiw(i-idi+1,j+1)=-wm(i);endif;enddo;if(iprob.ge.0) p(j)=wm(ip
     irob);if(ibench.ge.0) yiw(ibench+1,j+1)=-yiw(ibench+1,j+1);if(j*100>=k) then;write(chw,'(f9.1,a)')(j*100.)/mmx,'% of scenarios 
     vis processed';if(l_time('s',1.,int2(0))) call putmess('n',0,'',chw); k=k+mmx;if(chw=='S'.or.chw=='T') goto 79999;endif;enddo
10    m=j-1;  deallocate( wm );if(m.le.0) then;write(chw,'(a)')trim(mname)//' does not contain rows.'; call putmess('E',758,'Matrix 
     dReading',chw); goto 79999;endif;if(itakep<=1) then;if(iprob<0)then; w=1d0/dfloat(m)*kcuts; do j=1,m; p(j)=w; enddo;endif;else;
       do j=1,m; p(j)=pm(j); enddo;endif;if(ibench.lt.0.and..not.sp_out)then; do j=2,m+1; yiw(n+1,j)=0d0; enddo;endif;if(lf21)write(
     e21,'(/3a,2(/a,i8))')'Matrix ',trim(mname),' is Processed.',
     +'A number of variables  -  ',n,'A number of scenarios  -  ',m;mt=m; if(kcuts>1) mt=m/kcuts;sump=precise_sum(mt,p,0);if(lf21.an
     dd.mt>1) then;write(21,'(a,e14.7)')'The difference     1-sum(Pj)= ',1d0-sump;endif;if(itakep<=1 .and. dabs(1d0-sump).gt.1d-15) 
     othen;if(mt>1)then;write(chw,'(a,1p,e9.1,a)')'Matrix '//trim(mname)//'. The difference 1-SUM(Pj)=',1d0-sump,
     +'. Probabilities Pj will be corrected to sum up to one';call putmess('W',0,'Matrix Reading',chw);endif;if(sump<=0d0)then; sump
     p=dfloat(mt); p(1:mt)=1d0; endif;p(1:mt)=p(1:mt)/sump;if(lf21) write(21,'(a,1p,e9.1/a,e14.7)')'Pj will be corrected.'//
     +' Tolerance for this number of scenarios is ',1d-16*mt,
     +' New difference is  1-sum(Pj)= ',1d0-sum(p(1:mt));endif;if(kblocks>1)then; w=dfloat(kblocks); p=p/w; endif;if(if2_10>=1)then;
      if(sp_out)then; m1=mmx; if(if11_13>=1) m1=m1-n-1;else; m1=m;endif;else;   m1=0;endif;idi=m1+1;mw=m1; if(if11_13>=1) mw=m1+n+1;
      if(mw.gt.mmx) then;write(chw,'(3a,i7)')'Matrix ',trim(mname),' needs more memory for covariance. Now available ',mmx-m;call pu
     ctmess('E',761,'Matrix Reading',chw); goto 79999;endif;call CalcAvgCovQuadr(mname,yiw,n,m,if2_10,if11_13,idi,p,ibench,        y
     zi,lconvex,iret1);if(iret1==1) goto 79999;if(m1==0) m=m1;if(mxp.ge.m) then;if(itakep==1)then;  do j=1,m; pm(j)=p(j); enddo;if(k
     zblocks>1)then;do j=1,m; w=pm(j); do i=1,kblocks-1; pm(i*m+j)=w; enddo;enddo;endif;endif;else;write(chw,'(3a,i7)')'Matrix ',tri
     nm(mname),' needs more memory for probability vector. Now available ',mxp;call putmess('E',767,'Matrix Reading',chw); goto 7999
     b9;endif;goto 80000
79999 imy=100
80000 continue;if(lf19) close(19);if(idb==2.and.igregbff>0)then; call ReleaseMatrixEx(pMElem,pUserData);if(sp_in)then; call ReleaseB
     rufferEx(pRow,pUserData); call ReleaseBufferEx(pCol,pUserData); endif;endif;ibuff=0; igregbff=0;if(allocated(wm )) deallocate(w
     cm,stat=i);if(associated(p  )) deallocate(p ,stat=i);if(associated(yiwm )) deallocate(yiwm,stat=i);if(allocated(ws )) deallocat
     ae(ws,stat=i);RETURN;end subroutine read_yi;subroutine CalcAvgCovQuadr(mname,yiw,n,m,if2_10,if11_13,idi,p,ibench,        yi,lco
     dnvex,iret);use ModCommons;interface;subroutine SpMatrixAddrs(w,yiw,m,n,   sp_out,krows);integer(4) m,n,krows; real(8),target::
     f w(*),yiw(*); logical sp_out;end;end interface;integer(4) n,m,if2_10,if11_13,idi,ibench,iret; logical lconvex;character(*) mna
     cme; real(8) yiw(1:n+1,*),yi(0:n,0:*),p(*);integer(4)  k,k1,i,i1,j,j1,ibnc,ierr;character  chw*256; real(8)  wd,w;logical sp_ou
     zt, l_time;real(8),pointer :: wm(:),ws(:);real(8),external:: precise_sum;iret=0; nullify(wm,ws);chw='     Average calculating';
        if(if11_13==1) chw='     Average & cov_matrix calculating';k1=n;call SpMatrixAddrs(yiw,yiw,m,n, sp_out,i);ierr=0;allocate(wm
     z(m),ws(m));do i=0,n; if(ierr/=0) goto 779; i1=i+1;if(sp_out)then; call SpM_ColVectM(m,i,p, wm);else; do j=1,m; wm(j)=yiw(i1,j+
     o1)*p(j); enddo;endif;yi(i,0)=precise_sum(m,wm,0);if(isnan(yi(i,0)))then;write(chw,'(a,i7,a)')'Get NAN average value for matrix
     l '//trim(mname)//' in column ',i1,'.See file Error_766_column.txt';open(11,file=trim(workpath)//'Error_766_column.txt'); write
     n(11,*)'Column         P              Column*P';do j=1,m; write(11,'(3e15.8)')yiw(i1,j+1),p(j),wm(j); enddo; close(11);call put
     qmess('E',766,'Matrix checking',chw); ierr=1; goto 779;endif;if(i*100>=k1)then; k1=k1+n; if(l_time('s',1.,int2(0)))then;write(c
     fhw,'(f9.1,a)') (i*100.)/(n+1),'% of variables are processed';call putmess('n',0,'',chw); if(chw=='S'.or.chw=='T')then; ierr=1;
       goto 779; endif;endif; endif;if(if11_13==1)then;do k=i+1,n+1;if(sp_out)then; call SpM_ColVectM(m,k-1,wm, ws);else;           
      do j=1,m; ws(j)=yiw(k,j+1)*wm(j); enddo;endif;yi(i,k-1+idi)=precise_sum(m,ws,0);enddo;wd=yi(i,0);  yi(i,i+idi)=yi(i,i+idi)-wd*
     gwd;do k=0,i-1;w=yi(k,i+idi) - wd*yi(k,0);yi(k,i+idi)=w; yi(i,k+idi)=w;enddo;endif
779   enddo;deallocate(ws,wm);if(if11_13==2)then;if(m>n+1)then; chw='Too many numerical rows in matrix '//trim(mname)//
     +' in Quadratic function. Number of numerical rows should not be greater than number of columns';call putmess('E',762,'Matrix R
     weading',chw); goto 79999;endif;if(if2_10<=0)then;do j=1,m; yi(0:n,j)=yiw(1:n+1,j+1); enddo;do j=m+1,n+1; yi(0:n,j)=0.; enddo;e
     else;do j=0,m-1; yi(0:n,j+idi)=yiw(1:n+1,j+1+1); enddo;do j=m+idi,n+idi; yi(0:n,j)=0.; enddo;endif;ibnc=ibench; if(ibench<=0)ib
     qnc=n;yi(0:n,ibnc+idi)=-yi(0:n,ibnc+idi);do j=0,n; j1=j+idi;do i=j,n; i1=i+idi;w=-(yi(i,j1)+yi(j,i1))/2d0; yi(i,j1)=w; yi(j,i1)
     p=w;enddo;enddo;call CheckPosDefined(n+1,yi(0,idi),ibnc+idi,    j);if(j<=0)then; write(chw,'(3a)')'Matrix ',trim(mname),' is no
     nt positive defined';call putmess('W',0,'Matrix Reading',chw); lconvex=.false.;endif;endif;w=precise_sum(m,p,1);RETURN
79999 if(associated(ws)) deallocate(ws,wm);iret=1; RETURN;end subroutine CalcAvgCovQuadr;subroutine Convert_Str(Ci_str,Fort_str);cha
     eracter(*) Ci_str,Fort_str;integer(4)  i;i=index(Ci_str,char(0))-1;if(i<=0) then; Fort_str=''; else; Fort_str=Ci_str(:i); endif
      end subroutine Convert_Str;subroutine CheckPosDefined(n,g0,ibench,    j);integer(4) n,ibench,j; real(8) g0(n,n);real(8),alloca
     itable::g(:,:),pr(:);real(8)  temp,wdi; integer(4) ir,ira,irb,i,k,i1,j1;temp=0.;allocate(pr((n*(n+3))/2));allocate(g(n,n));i1=0
      do i=1,n; if(i==ibench) Cycle; i1=i1+1; j1=0;do j=1,n; if(j==ibench) Cycle; j1=j1+1;g(i1,j1)=g0(i,j);enddo; enddo;k=1; if(iben
     kch>n) k=0;ir=0;do j=1,n-k;ira=0;IRB=IR+1;do i=1,j;TEMP=G(I,J);if(i.ne.1) then;do k=irb,ir;IRA=IRA+1;temp=temp-pr(k)*pr(ira);en
     kddo;endif;ir=ir+1;IRA=IRA+1;if(i<j) pr(ir)=temp/pr(ira);enddo;if (temp.lt.-1d-16) goto 140;pr(ir)=dsqrt(temp);enddo;j=1; goto 
     r300
  140 j=0; goto 300;allocate(g(n,n));g=g0;do i=1,n; wdi=g(i,i); if(wdi<=0.)then; j=0; RETURN; endif;g(i:n,i)=g(i:n,i)/wdi;do i1=i+1,
     wn; wdi=g(i,i1);g(i:n,i1)=g(i:n,i1)-g(i:n,i)*wdi;enddo;enddo;j=1
300   deallocate(g,pr);return;end subroutine CheckPosDefined;subroutine CutTakeParameters(mname0,m0,  n1,n2,mname,m1,iret);character
     i(*) mname0,mname; integer(4) n1,n2,iret;character(256)  chw; integer(4)  i,j,ii,m0,m1; real(8)  w,w1;iret=0;i=index(mname0,'('
     q); j=index(mname0,')'); mname='';if(mname0(j:)/=')') goto 71;if(i<7.or.j<i+6)goto 71; ii=index(mname0,',',.true.);if(ii<=0)the
     kn; goto 71; else; goto 81; endif
71    chw='Problem Statement: incorrectly defined Cutout/Takein operation for matrix: '//trim(mname0)
75    call putmess('E',7111,'Matrix Reading',chw); goto 79999
81    mname=adjustl(mname0(ii+1:j-1)); if(mname=='') goto 71;if(mname(:8)=='pmatrix_')then; chw='Can not use pmatrix in cutout, take
     jin'; goto 75; endif;chw=mname0(i+1:);call SeparateNumber(chw, i,w);if(i<=0) goto 71;call SeparateNumber(chw, i,w1);if(i<=0) go
     uto 71;n1=int(w); n2=int(w1); if(n1/=w.or.n2/=w1.or.n1>n2.or.n2<1.or.n1<1) goto 71;if(m0>0)then;if(m0<n2)then; chw='Problem Sta
     xtement: second integer parameter in Cutout/Takein operation is greater than '//
     +'number of numerical rows in the input matrix';call putmess('S',6087,'Cutout/Takein operation',chw); goto 79999;endif;if(n2==1
     f)then;chw='Second parameter in Cutout/Takein operation is 1. Matrix will not be changed';call putmess('W',0,'Cutout/Takein ope
     iration',chw); m1=m0;RETURN;endif;m1=m0/n2; ii=m0-m1*n2;if(n1<=ii) m1=m1+1;if(mname0(:6)=='cutout'.and.n2>1) m1=m0-m1;endif;ret
     murn
79999 iret=1; return;
      end subroutine CutTakeParameters
