      subroutine DivideFunRows(probaddr,   probaddr_w,  wstr, pwstr, wch );USE CiFort; use ModCommons; use FuncNames;integer(PLEN) p
     lrobaddr,probaddr_w;character(*) wstr, pwstr, wch;integer(4),external:: iBuffLen;integer(4)  i,j,i1,id,ibuff,lrest,nfg,i0,nf2,l
     lrst0,iret;real(8) w;integer(PLEN),pointer:: pChar;integer(PLEN) probaddr_t;logical lDB;character(l16kmax),external:: move_spac
     le;character(1) ch0;integer(4),external:: iFindCloseBracket;ibuff=0; ch0=char(0);call setpointer(probaddr, pChar); ibuff=iBuffL
     len(probaddr); if(ibuff<=0) RETURN;probaddr_w=malloc(ibuff*2); probaddr_t=probaddr_w;lDB=.true.;wstr=''; lrest=1
62    continue;j=len_trim(wstr)+1;if(j>1)then; wstr(j:j)=char(10); call CopySubBuff(1,loc(wstr),1,j, probaddr_t); probaddr_t=probadd
     lr_t+j;endif;wstr='';do while(wstr.eq.''.or.wstr(:1)=='%'); lrst0=lrest; call read_wstr(pChar,ibuff,wstr,lrest,ldb,iret);if(ire
     lt==2)then; goto 1001; elseif(iret==1)then; goto 79999; endif;enddo;pwstr=wstr;wstr=move_space(wstr);call IsFunctionRow(wstr,fn
     lc_name,kfn,    w,wch,nfg,id,nf2);if(nf2<0)then; i=scan(trim(wstr),'('); i=scan(wstr(:i-2),'*');if(i>0) call IsFunctionRow(wstr
     l(i+1:),fnc_name,kfn,    w,wch,nfg,id,nf2);if(nf2<0) GOTO 62;endif;i0=1;do while(i0>0); i1=scan(wstr(i0:),'('); i=i1+i0-1;if(i>
     l=i0)then; j=iFindCloseBracket('()',wstr,i+1);if(j<=0)then;wch='Problem Statement: incorrect brackets in string: '//trim(pwstr)
     l; call putmess('S',6981,'DivideFunRows',wch);goto 79999;endif;if(scan(wstr(j+1:),'(')<=0) j=0;if(j>=i+1)then; wch=wstr(i0:j)//
     lchar(10);call CopySubBuff(1,loc(wch),1,j-i0+2, probaddr_t);probaddr_t=probaddr_t+(j-i0+2); i0=j+1;endif;endif;if(i1<=0.or.j<=0
     l)then; j=len_trim(wstr);if(wstr(i0:j)/='')then; wch=wstr(i0:j)//char(10);call CopySubBuff(1,loc(wch),1,j-i0+2, probaddr_t);  p
     lrobaddr_t=probaddr_t+(j-i0+2);endif;i0=0;endif;enddo;wstr='';GOTO 62;goto 79999
1001  continue;call CopyBuff(loc(ch0),1,probaddr_t,1)
79999 continue;return;end subroutine DivideFunRows;subroutine EMConstraints(probaddr,l16p,  probaddr_w,   wstr, pwstr, wch);USE CiFo
     lrt; use ModCommons; use FuncNames;integer(PLEN) probaddr,probaddr_w;integer(4) l16p;character(*) wstr, pwstr, wch;integer(4)  
     liostat,nc,ideall,i,k,j1,ibuff,lrest,lstr,iret;real(8) w,xhuge,sign_min;integer(PLEN),pointer:: pChar;integer(PLEN) jaddr;logic
     lal  lDB,lw;character(l16kmax),external:: move_space;character(lnm) cname;integer(4),allocatable:: kf(:),lnrz(:),lrin(:,:,:);re
     lal(8),allocatable::bnds(:,:);character(lnm),allocatable::obname(:,:),vector(:,:), ch3(:);integer(4)  iw,llast,j2,lrst0,kconstr
     l,mxkf,mxconstr,imult,i1;integer(4),external:: iBuffLen,istr_without_sub;interface;integer(4) function indexInBuff(buff,str,len
     l);use CiFort; character(*) str; integer(plen),value:: buff; integer(llen),value:: len;end;end interface;lstr=l16p; l16p=l16kma
     lx;probaddr_w=0; if(probaddr<=0) RETURN;ibuff=0;call setpointer(probaddr, pChar); ibuff=iBuffLen(probaddr); if(ibuff<=0) RETURN
      lDB=.true.; jaddr=0; lrest=1;xhuge=huge(w)/2d0; sign_min=1d0;mxconstr=1;mxkf=100;allocate(
     +lnrz(1:mxconstr),
     +bnds(0:1,1:mxconstr),
     +kf(1:mxconstr),
     +obname(mxkf,1:mxconstr),
     +STAT=iostat);if(iostat.ne.0)then; wch='Arrays allocation is failed'; call putmess('S',6140,'MultiConstr Checking',wch); goto 7
     l9999; endif;allocate(vector(0:1,0:mxkf),lrin(0:1,-1:8,0:mxkf),
     +ch3(3),STAT=iostat);if(iostat.ne.0)then; wch='Arrays allocation is failed'; call putmess('S',6043,'MultiConstr Checking',wch);
       goto 79999; endif;lDB=.true.;bnds(0,:)=-xhuge; bnds(1,:)=xhuge;obname=''; lnrz=0;kconstr=0;kf=0;wstr=''; vector=''; wch='';im
     lult=0;goto 100
15555 wch='Problem Statement: incorrect row: '//trim(pwstr);call putmess('S',6083,'MultiConstr Checking',wch); goto 79999
100   continue;nc=0;vector='';lrin=0;do while(wstr.eq.''.or.wstr(:1)=='%'); lrst0=lrest; call read_wstr(pChar,ibuff,wstr,lrest,ldb,i
     lret);if(iret==2)then; goto 1001; elseif(iret==1)then; goto 79999; endif;enddo;if(wstr(:11)=='multivalue:'.or.wstr(:11)=='multi
     lvalue ')then;wstr=trim(wstr(6:));call CopySubBuff(1,loc(wstr),1,lrest-lrst0-1, probaddr+(lrst0-1));goto 100;endif;if(.not.(wst
     lr(:15)=='multiconstraint'.and.scan(wstr(16:16),' ,:_=<>')==1).and.
     +.not.(wstr(:10)=='constraint'.and.scan(wstr(11:11),' ,:_=<>')==1)            )then; wstr=''; goto 100; endif;imult=0; if(wstr(
     l:5)=='multi')then; wstr=trim(wstr(6:)); imult=1; endif;wstr=move_space(wstr);call CopySubBuff(1,loc(wstr),1,lrest-lrst0-1, pro
     lbaddr+(lrst0-1));pwstr=wstr;if(scan(wstr(11:11),':_,')==1)then; wstr=adjustl(wstr(12:));else; wstr=adjustl(wstr(11:));endif;nc
     l=1; kconstr=kconstr+1; kf(nc)=0;lrin(0,-1,0)=lrst0;cname='';wch=''; read(wstr,*,err=102,end=102)wch; i=index(trim(wch),':'); i
     lf(i>0)wch=wch(:i-1)
102   if(wch/='linearize'.and.wch(:10)/='linearize='.and.wch/='mip'.and.wch(:10)/='mip='.and.
     +wch(:1)/='<'.and.wch(:1)/='>'.and.wch(:1)/='='.and.
     +wch/='lower_bound'.and.wch/='upper_bound'.and.wch(:12)/='lower_bound='.and.wch(:12)/='upper_bound=')then;i=istr_without_sub(ws
     ltr,trim(wch),wstr); cname=wch;i=verify(wstr,' :,');  if(i>0)wstr=wstr(i:);endif;call Find3Part(wstr,1,'linearize','=','?????',
     lj1,i,i1,k,w,lw);if(lw)then; if(wstr(i1:k)=='1')then; lnrz(nc)=1; elseif(wstr(i1:k)=='0')then; lnrz(nc)=-1; else; goto 15555; e
     lndif;wstr(j1:k)='d';elseif(j1*i>0)then; goto 15555;endif;call Find3Part(wstr,1,'mip','=','?????',j1,i,i1,k,w,lw);if(lw)then; i
     lf(wstr(i1:k)=='1')then; lnrz(nc)=1; elseif(wstr(i1:k)=='0')then; lnrz(nc)=-1; else; goto 15555; endif;wstr(j1:k)='d';elseif(j1
     l*i>0)then; goto 15555;endif;call Find3Part('Equal'//wstr,1,'Equal','==','NUM',j1,i,i1,k,w,lw);if(j1*i<=0) call Find3Part('Equa
     ll'//wstr,1,'Equal','=','NUM',j1,i,i1,k,w,lw);if(j1*i<=0)then; bnds(0:1,nc)=-xhuge;else;if(i1<=0)then; goto 15555;else; i1=i1-5
     l; k=k-5;if(lw)then; bnds(0:1,nc)=w; goto 1035; endif;if(wstr(i1:k)=='-infinity'.or.wstr(i1:k)=='infinity')then;wch='Problem St
     latement: incorrectly specified equality in constraint: '//trim(pwstr)//
     +'. It should be in range (-1e20, +1e20)';call putmess('S',6621,'MultiConstr Checking',wch); goto 79999;else;bnds(0:1,nc)=0; ve
     lctor(0:1,0)=wstr(i1:k);i=indexInBuff(probaddr+(lrst0-1),wstr(i1:k),int(lstr,llen));lrin(0,0:1,0)=i; lrin(1,0:1,0)=i+(k-i1);if(
     lk-i1+1>lnm)then; write(wch,'(a,i5,a)')
     +'Problem Statement: length (number of symbols) of vector name exceeds Max=',lnm-7,': '//wstr(i1+7:k);call putmess('S',6631,'Mu
     lltiConstr Checking',wch); goto 79999;endif;endif;endif
1035  wstr(j1:k)='d'; goto 1051;endif;call Find3Part(wstr,1,'>','=','NUM',j1,i,i1,k,w,lw);if(j1>0.and.i<=0)then; call Find3Part('>='
     l//wstr(j1+1:),1,'>','=','NUM',  j2,i,i1,k,w,lw);k=k+j1-2; if(i1>0)i1=i1+j1-2;endif;if(j1*i<=0) call Find3Part(wstr,1,'lower_bo
     lund','=','NUM',j1,i,i1,k,w,lw);if(j1*i<=0)then; bnds(0,nc)=-xhuge;else;if(i1<=0)then; goto 15555;else; if(lw)then; bnds(0,nc)=
     lw; goto 103; endif;if(wstr(i1:k)=='-infinity')then; bnds(0,nc)=-xhuge;else;vector(0,0)=wstr(i1:k); bnds(0,nc)=-xhuge/2.;i=inde
     lxInBuff(probaddr+(lrst0-1),wstr(i1:k),int(lstr,llen));lrin(0,0,0)=i; lrin(1,0,0)=i+(k-i1);if(k-i1+1>lnm) then;write(wch,'(a,i5
     l)')'Length of vector name '//wstr(i1+7:k)//' exceeds Max=',lnm-7;call putmess('S',6641,'MultiConstr Checking',wch); goto 79999
      endif;endif;endif
103   wstr(j1:k)='d';endif;call Find3Part(wstr,1,'<','=','NUM',j1,i,i1,k,w,lw);if(j1>0.and.i<=0)then; call Find3Part('<='//wstr(j1+1
     l:),1,'<','=','NUM',  j2,i,i1,k,w,lw);k=k+j1-2; if(i1>0)i1=i1+j1-2;endif;if(j1*i<=0) call Find3Part(wstr,1,'upper_bound','=','N
     lUM',j1,i,i1,k,w,lw);if(j1*i<=0)then; bnds(1,nc)= xhuge;else;if(i1<=0)then; goto 15555;else; if(lw)then; bnds(1,nc)=w; goto 105
     l; endif;if(wstr(i1:k)=='infinity'.or.wstr(i1:k)=='+infinity')then; bnds(1,nc)=xhuge;else;vector(1,0)=wstr(i1:k); bnds(1,nc)=+x
     lhuge/2.;i=indexInBuff(probaddr+(lrst0-1),wstr(i1:k),int(lstr,llen));lrin(0,1,0)=i; lrin(1,1,0)=i+(k-i1);if(k-i1+1>lnm) then;wr
     lite(wch,'(a,i5)')'Length of vector name '//wstr(i1+7:k)//' exceeds Max=',lnm-7;call putmess('S',6651,'MultiConstr Checking',wc
     lh); goto 79999;endif;endif;endif
105   wstr(j1:k)='d';endif
1051  continue;if(verify(wstr,' d,'//char(9))>0) goto 15555;GOTO 62
62    continue;wstr='';do while(wstr.eq.''.or.wstr(:1)=='%'); lrst0=lrest; call read_wstr(pChar,ibuff,wstr,lrest,ldb,iret);if(iret==
     l2)then; goto 1001; elseif(iret==1)then; goto 79999; endif;enddo;if(wstr(:15)=='multiconstraint'.and.scan(wstr(16:16),' ,:_=<>'
     l)==1  .or.
     +wstr(:10)=='constraint'.and.scan(wstr(11:11),' ,:_=<>')==1  )then;if(imult==0)then;if(count(vector(:,0)/='')>0)then; wch='Prob
     llem Statement: regular constraint cannot use bound vector: '//
     +trim(vector(0,0))//'  '//trim(vector(1,0))//'. It should use number';call putmess('S',6541,'MultiConstr Checking',wch); goto 7
     l9999;endif;vector=''; lrin(:,0:,:)=0;endif;call ChangePS_forLastMulti(kconstr,probaddr,vector,lrin,0,kf(1),l16p,cname,     pro
     lbaddr_w,llast);if(ioutk>=istop-1) goto 79999;goto 100;endif;if(wstr(:4)=='box:'.or.wstr(:4)=='box '.or.wstr(:4)=='box='.or.wst
     lr(:4)=='box<'.or.wstr(:4)=='box>') goto 1001;if(wstr(:17)=='box_of_variables:'.or.wstr(:17)=='box_of_variables '.or.wstr(:17)=
     l='box of variables ') goto 1001;if(wstr(:7)=='solver:'.or.wstr(:7)=='solver ') goto 1001;if(wstr(:6)=='value:'.or.wstr(:6)=='v
     lalue ') goto 1001;if(wstr(:6)=='point:'.or.wstr(:6)=='point ') goto 1001;if(wstr(:11)=='multivalue:'.or.wstr(:11)=='multivalue
     l ') goto 1001;pwstr=wstr;if(kf(nc)==mxkf) call Resize_mxkf_arrays();call ProcessIfFucntionRow(probaddr,lrst0,lrest,mxkf,lrin,o
     lbname(:,nc),l16p,  wstr,kf(nc),  vector,iw,iret);if(iret==1) goto 79999;if(iw<=0) goto 15555;GOTO 62
1001  continue;if(imult==0)then;if(count(vector(:,0)/='')>0)then;wch='Problem Statement: can not use vector as a bound in regular co
     lnstraint: '//trim(vector(0,0))//'  '//vector(1,0);call putmess('S',6542,'MultiConstr Checking',wch); goto 79999;endif;vector='
     l'; lrin(:,0:,:)=0;endif;call ChangePS_forLastMulti(kconstr,probaddr,vector,lrin,0,kf(1),l16p,cname,     probaddr_w,llast);if(i
     loutk>=istop-1) goto 79999;if(lrest==0)goto 1030
1020  do while(wstr.eq.''.or.wstr(:1)=='%'); i=lrest; call read_wstr(pChar,ibuff,wstr,lrest,ldb,iret);if(iret==2)then; goto 1030; el
     lseif(iret==1)then; goto 79999; endif;enddo;if(wstr(:11)=='multivalue:'.or.wstr(:11)=='multivalue ')then;wstr=trim(wstr(6:)); c
     lall CopySubBuff(1,loc(wstr),1,lrest-i-1, probaddr+(i-1));endif;wstr=''; goto 1020
1030  continue;if(probaddr_w>0)then;if(lrst0>ibuff) lrst0=ibuff;call IncreaseBuffer(0,ibuff-(lrst0-1), probaddr_w, i);call CopySubBu
     lff(1,probaddr+(lrst0-1),1,ibuff-(lrst0-1),  probaddr_w+llast);endif
79999 continue;ibuff=0;if(jaddr/=0)then; call free(jaddr); jaddr=0; endif;iostat=0; i=0;if(allocated(kf   )) deallocate(kf   ,stat=i
     l); iostat=iostat+i;if(allocated(obname))deallocate(obname,stat=i); iostat=iostat+i;if(allocated(lnrz)) deallocate(lnrz,stat=i)
     l; iostat=iostat+i;if(allocated(bnds )) deallocate(bnds ,stat=i); iostat=iostat+i;if(allocated(vector))deallocate(vector,stat=i
     l); iostat=iostat+i;if(allocated(ch3  )) deallocate(ch3,  stat=i); iostat=iostat+i;ideall=iostat;if(iostat.ne.0)then; wch="Can 
     lnot deallocate all"; call putmess('W',0,'Deallocate in EMConstraints',wch);endif;RETURN;CONTAINS;subroutine Resize_mxkf_arrays
     l();integer i,j;i=int(max(sizeof(obname),sizeof(vector)*2,sizeof(lrin)*2*10))*(mxkf+1); jaddr=malloc(i); j=mxkf*2;i=int(sizeof(
     lobname))*mxkf;call copybuff(loc(obname),i,jaddr,i); deallocate(obname);allocate(obname(j,1)); call copybuff(jaddr,i,loc(obname
     l),i);i=int(sizeof(vector))*2*(mxkf+1); call copybuff(loc(vector),i,jaddr,i); deallocate(vector);allocate(vector(0:1,0:j)); cal
     ll copybuff(jaddr,i,loc(vector),i);i=4*2*10*(mxkf+1);call copybuff(loc(lrin),i,jaddr,i); deallocate(lrin); allocate(lrin(0:1,-1
     l:8,0:j)); call copybuff(jaddr,i,loc(lrin),i);call free(jaddr); jaddr=0; mxkf=j;end subroutine Resize_mxkf_arrays;END SUBROUTIN
     lE EMConstraints;subroutine ProcessIfFucntionRow(probaddr,lrst0,lrest,mxkf,lrin,obname,l16p,    wstr,kf,    vector,isfunc,iret)
      use modcommons; use FuncNames;integer(PLEN) probaddr;integer(4) lrst0,lrest,mxkf,lrin(0:1,-1:8,0:mxkf),kf,isfunc,l16p,iret;cha
     lracter(*)vector(0:1,0:mxkf),obname(*),wstr;character(l16kmax)  pwstr, wch;character(l16kmax),allocatable:: chm(:);real(8)  w;i
     lnteger(4)  nfg,id,nf2,nf1,i,j,iOpn,i0,i1,j1,k,iret1;character(l16kmax),external:: move_space;integer(4),external:: istr_withou
     lt_sub;interface;integer(4) function indexInBuff(buff,str,len);use CiFort; character(*) str; integer(plen),value:: buff; intege
     lr(llen),value:: len;end;end interface;iret=0; i0=0;isfunc=0;allocate(chm(3),STAT=i);if(i/=0)then; wch='Array chm allocation is
     l failed'; call putmess('S',6043,'ProcessFucntionRow',wch); goto 79999; endif;wstr=move_space(wstr);call CopySubBuff(1,loc(wstr
     l),1,lrest-lrst0-1, probaddr+(lrst0-1));pwstr=wstr;goto 1221;call IsFunctionRow(wstr,fnc_name,kfn,    w,chm,nfg,id,nf2);nf1=nf2
      select case(nf2);case(1180,1201,1270,1271);i=index(wstr,'('); j=index(wstr,')',.true.);if(i>0.and.i<j-1)then; wstr=wstr(i+1:j-
     l1); else; goto 15555; endif;call IsFunctionRow(wstr,fnc_name,kfn,    w,chm,nfg,id,nf2);end select;kf=kf+1;lrin(0,-1,kf)=lrst0;
      if(nf2<0)then;i=scan(trim(wstr),'('); i=scan(wstr(:i-2),'*');if(i>0)then; call IsFunctionRow(wstr(i+1:),fnc_name,kfn,    w,chm
     l,nfg,id,nf2);nf1=nf2;select case(nf2);case(1180,1270,1271);i1=i+index(wstr(i+1:),'('); j=index(wstr,')',.true.);if(i>0.and.i<j
     l-1)then; wstr=wstr(i1+1:j-1); else; goto 15555; endif;call IsFunctionRow(wstr,fnc_name,kfn,    w,chm,nfg,id,nf2);end select;en
     ldif;if(nf2<0) GOTO 62;if(nf2>=0)then; chm=''; read(pwstr(:i-1),*,end=762,err=15555)chm;
762   if(.not.(chm(1)/=''.and.chm(2)==''))goto 15555;endif;vector(0,kf)=chm(1);if(vector(0,kf)(:7)/='vector_') goto 15555;i=indexInB
     luff(probaddr+(lrst0-1),trim(chm(1)),int(l16p,llen));lrin(0,0,kf)=i; lrin(1,0,kf)=i+(len_trim(chm(1))-1);endif;isfunc=1;i=istr_
     lwithout_sub(wstr,trim(chm(1)),wstr);iOpn=index(wstr,'(');i=index(pwstr,'(');i0=index(pwstr,trim(chm(1)));if(i>1)then; obname(k
     lf)=trim(pwstr(i0:i-1));if(obname(kf)/=trim(pwstr(i0:i-1))) then;write(wch,'(a,i5,a)')'Problem Statement: length (number of sym
     lbols) of a name exceeds Max=',lnm,
     +' in: '//trim(pwstr(i0:i-1));call putmess('S',6327,'ProcessFucntionRow',wch); goto 79999;endif;if(scan(trim(pwstr(i0:i-1)),' '
     l)>0) then;wch='Problem Statement: incorrect function name in row: '//trim(pwstr); call putmess('S',6325,'ProcessFucntionRow',w
     lch);goto 79999;endif;else; wch='';if(i<=0)then; wch='Problem Statement: missing ( in row: '//trim(pwstr);endif;if(wch/='') the
     ln; call putmess('S',6323,'ProcessFucntionRow',wch); goto 79999; endif;endif;i=iOpn;goto 1234
1221  continue;kf=kf+1; lrin(0,-1,kf)=lrst0;call IsFRwithVector(fnc_name,kfn,  wstr,  w,chm(1),nfg,id,nf2,vector(0,kf));if(nf2<0) GO
     lTO 62;nf1=nf2;select case(nf1);case(1180,1270,1271);i=index(wstr,'('); j=index(wstr,')',.true.);call IsFunctionRow(wstr(i+1:j-
     l1),fnc_name,kfn, w,chm,nfg,id,nf2);if(nf2<0)then; nf2=nf1; else; wstr=wstr(i+1:j-1); endif;end select;if(vector(0,kf)/='')then
      i=indexInBuff(probaddr+(lrst0-1),trim(vector(0,kf)),int(l16p,llen));lrin(0,0,kf)=i; lrin(1,0,kf)=i+(len_trim(vector(0,kf))-1);
      endif;isfunc=1;obname(kf)=chm(1);if(obname(kf)/=trim(chm(1)))then;write(wch,'(a,i5,a)')'Problem Statement: length (number of s
     lymbols) of a name exceeds Max=',lnm,
     +' in: '//trim(pwstr(i0:i-1));call putmess('S',6327,'ProcessFucntionRow',wch); goto 79999;endif;if(scan(trim(chm(1)),' ')>0) th
     len;wch='Problem Statement: incorrect function name in row: '//trim(pwstr); call putmess('S',6375,'ProcessFucntionRow',wch);got
     lo 79999;endif;iOpn=index(wstr,'('); i=iOpn
1234  continue;select case(nfg);case(2:3,7:8,14:19,21:22,28:33,34,36,37,38,40,41,45:56,64,67:70,73:81,82:83,85:96,
     +104,105,107,112:115,123:124,129:131,132,134,135,136:137,140);i1=index(wstr,',');if(i1==0.or.i+1>i1-1) goto 15555;call IsARealN
     lumber(wstr(i+1:), k,j1,w);i1=index(wstr(i+1+j1-1:),',')-1+(i+1+j1-1);if(i1<(i+1+j1-1).or.i+1>i1-1) goto 15555;if(k<=0)then;chm
     l='';read(wstr(i+1+j1-1:i1-1),*,end=771,err=77)chm;
771   if(.not.(chm(1)/=''.and.chm(2)==''))goto 77;vector(1,kf)=chm(1);if(vector(1,kf)(:7)/='vector_') goto 15555;i=1+indexInBuff(pro
     lbaddr+(lrst0-1),'(',int(l16p,llen));if(nf2/=nf1) i=i+indexInBuff(probaddr+(lrst0-1)+i-1,'(',int(l16p-i+1,llen));lrin(0,1,kf)=i
     l; lrin(1,1,kf)=i+(indexInBuff(probaddr+(lrst0-1)+i-1,',',int(l16p-i+1,llen))  -1)-1;endif;case default;i1=iOpn; w=0d0;end sele
     lct;GOTO 80
77    continue;wch='Problem Statement: incorrectly specified parameter or vector of parameters in function in row:'//trim(pwstr);cal
     ll putmess('S',6410,'ProcessFucntionRow',wch); goto 79999
80    continue;i=index(wstr,')',.true.);if(verify(wstr(i+1:),' d,'//char(9))>0) goto 15555;chm(1)=wstr(i1+1:i-1);if(chm(1).eq.'')the
     ln;wch='Empty argument list in '//trim(pwstr); call putmess('W',0,'ProcessFucntionRow',wch);endif;call AnalyzeUpperestBracket(n
     lf2,nfg,pwstr,kf,mxkf, chm, lrin,vector, iret1);if(iret1==1) goto 79999;goto 62
15555 wch="Problem Statement: incorrect row: "//trim(pwstr); call putmess('S',6321,'ProcessFucntionRow',wch)
79999 isfunc=-1
62    if(allocated(chm  )) deallocate(chm);if(isfunc<0) iret=1;return;end subroutine ProcessIfFucntionRow;subroutine AnalyzeUpperest
     lBracket(nf2,nfg,pwstr,kf,mxkf,    chm,   lrin,vector,iret);integer(4) nf2,nfg,kf,mxkf,lrin(0:1,-1:8,0:mxkf),iret;character(*)p
     lwstr,chm(*),vector(0:1,0:mxkf);integer(4)  km,i0,i,ln,ic,j,j1,i1,iret1; character(256)  wch;integer(4),external:: iFindCloseBr
     lacket;iret=0;km=1;select case(nf2);case(180:191,220:221,240:241,260:261,440,780:811,850:861,1100:1160);case default;i0=index(p
     lwstr,trim(chm(1))//')')-1;i=1; ln=len_trim(chm(1)); call RemoveInsideContent('()','X',chm(1),chm(2),iret1);if(iret1==1) goto 7
     l9999;do while(i<ln);ic=index(chm(2)(i:),'cut(')+i-1;if(ic>=i)then; j=ic-2; else; j=ln; ic=0; endif;j1=1;do while(j1>0); call F
     lindFirstThreePointsBlock(chm(2)(i:j),i1,j1);if(i1>0)then; km=km+1; lrin(0,km,kf)=i0+i-1+i1; lrin(1,km,kf)=i0+i-1+j1;i=i-1+j1+2
      elseif(i1<0)then; wch='Problem Statement: wrong using ,..., in row: '//trim(pwstr);call putmess('S',6440,'MultiConstr Checking
     l',wch); goto 79999;endif;enddo;if(ic>0)then; i=iFindCloseBracket('()',chm(2),ic+4);if(i<ic+4)then; wch='Problem Statement: inc
     lorrect brackets in function: '//trim(pwstr);call putmess('S',6450,'MultiConstr Checking',wch); goto 79999;endif;km=km+1; lrin(
     l0,km,kf)=i0+ic; lrin(1,km,kf)=i0+i;else; i=ln;endif;i=i+2;enddo;if(km==1)then; j1=0; i1=1; i=1; call RemoveInsideContent('()',
     l'',chm(1),chm(2),iret1);if(iret1==1) goto 79999;do while(i>0); i=scan(chm(2)(i1:),','); if(i>1)j1=j1+1; i1=i1+i;if(i==1)then; 
      j1=0; Exit; endif;enddo;if(j1>0)then;select case(nfg);case(45:64, 67:77, 83:84, 85:104, 119:121, 132:135);case default;km=km+1
     l; lrin(0,km,kf)=i0+1; lrin(1,km,kf)=i0+len_trim(chm(1));end select;endif;endif;end select;if(nfg==20)then; vector=''; lrin(:,0
     l:1,:)=0;endif;return
79999 iret=1; return;end subroutine AnalyzeUpperestBracket;subroutine ChangePS_forLastMulti(kconstr,probaddr,vector,lrin,iff,kf,l16p
     l,cname,     probaddr_w,llast);USE CiFort; use ModCommons; use FuncNames;integer(PLEN) probaddr,probaddr_w;character(*) vector(
     l0:1,0:*),cname;integer(4) kconstr,llast,lrin(0:1,-1:8,0:*),iff,kf,l16p;character(L16Kmax)  wstr,pwstr;real(8),pointer:: pMElem
     l;integer(4),pointer :: iChar, kRow, kCol;integer(4),target :: zRow, zCol;integer(PLEN),pointer:: pChar;character(256)  wch, ch
     lw(3);integer(4),allocatable::ncol(:),nb(:),ivr(:);real(8),allocatable::vctr(:,:);integer(PLEN) ia8;logical lDB;integer(4)  ibu
     lff,iwbuff,ln,nh,nfg,id,nf2,iv,iz1,iz2,i2,mv,km,lrest,is,nz,i1,j1,kv0,ivbuff,j,i,
     +mv0,kv,iret;real(8) v1,v2,v11,v22,w;integer(4),external:: iBuffLen,iCheckPSGName;i=kconstr; j=l16p; iv=0;kRow=>zRow; kCol=>zCo
     ll;i=count(lrin(0,0:,0:kf)>0); if(i<=0.and.probaddr_w==0) RETURN;ldb=.true.;allocate(ncol(i),nb(i));call setpointer(probaddr, p
     lChar); ibuff=iBuffLen(probaddr);mv0=0; kv=0;do j=iff,kf; do i=1,0,-1; if(vector(i,j)=='') Cycle;if(idb>0)then; if(iCheckPSGNam
     le(trim(vector(i,j)),'Say')<0) goto 79999;if(index(trim(vector(i,j)),'vector_')/=1)then;chw='Problem Statement: incorrect name 
     lof vector: '//trim(vector(i,j));call putmess('E',6340,'MultiConstr/Func Checking',chw); goto 79999;else;ivbuff=int(GetMatrixIn
     lfoEx(trim(vector(i,j))//char(0),iChar,kRow,kCol,pUserData));if(ivbuff>0)then;call Convert_Str(iChar,loc(wch),  int(ivbuff,llen
     l),int(ivbuff,llen)); call StringToSmall(wch(:ivbuff));call ReleaseBufferEx(iChar, pUserData);else; wch='Internal error: Empty 
     lbuffer while reading header of '//trim(vector(i,j));call putmess('S',6332,'MultiConstr/Func Checking',wch); goto 79999;endif;e
     lndif;else; open(33, file=trim(workpath)//trim(vector(i,j))//'.txt',err=38,status='old');read(33,'(a)',end=38,err=38)wch; ivbuf
     lf=len_trim(wch); call StringToSmall(wch(:ivbuff));do kRow=1,2000000000; read(33,'(a)',end=30,err=38)chw(1); enddo
30    kRow=kRow-1;chw=''; read(wch,*,end=35,err=38)chw
35    kCol=count(chw/='');goto 39
38    wch='Problem Statement: error when solver works with file: '//trim(workpath)//trim(vector(i,j))//'.txt';call putmess('S',6337,
     l'MultiConstr/Func Checking',wch); goto 79999;endif
39    continue;if(mv0==0)then; mv0=kRow;elseif(kRow/= mv0)then; wch='Vectors in a MultiConstraint have different numbers of componen
     lts';call putmess('S',6331,'MultiConstr/Func Checking',wch); goto 79999;endif;chw=''; read(wch,*,end=40)chw
40    continue;if(count(chw=='id')==0.and.kCol/=1 .or. count(chw=='id')==1.and.kCol/=2 .or. count(chw=='id')>1 )then;wch='Vector '//
     ltrim(vector(i,j))//' has incorrect header'; call putmess('S',6333,'MultiConstr/Func Checking',wch);goto 79999;endif;kv=kv+1; n
     lcol(kv)=kCol; nb(kv)=0; if(chw(1)=='id')nb(kv)=1;enddo; enddo;allocate(vctr(0:mv0,0:kv)); kv0=kv; vctr=0.;kv=0;do j=iff,kf; do
     l i=1,0,-1; if(vector(i,j)=='')Cycle; kv=kv+1;if(idb>0)then; ivbuff=int(GetMatrixDataEx(trim(vector(i,j))//char(0),pMElem,mv0,n
     lcol(kv),pUserData));if(ivbuff/=0)then; i1=-1; call Get_col(mv0,ncol(kv)-1,nb(kv),pMElem,0,   i1,vctr(:,kv),j1);call ReleaseMat
     lrixEx(pMElem,pUserData);if(j1>0)goto 60;else; wch='Internal error: Empty buffer while reading data of '//trim(vector(i,j));cal
     ll putmess('S',6334,'MultiConstr/Func Checking',wch); goto 79999;endif;else;open(33, file=trim(workpath)//trim(vector(i,j))//'.
     ltxt',err=48,status='old'); rewind(33);read(33,*,err=48)wch;do nz=1,mv0; chw='';read(33,*,err=48)chw(:ncol(kv));if(nb(kv)==1)ch
     lw(1)=chw(2);read(chw(1),*,err=48)vctr(nz,kv);enddo;close(33); goto 49
48    wch='Error while working with file: '//trim(workpath)//trim(vector(i,j))//'.txt';call putmess('S',6339,'MultiConstr/Func Check
     ling',wch); goto 79999;endif
49    continue;enddo; enddo;if(idb>0)vctr=-vctr;do is=1,kf; if(count(lrin(0,2:,is)>0)==0)Cycle;lrest=lrin(0,-1,is); call read_wstr(p
     lChar,ibuff,wstr,lrest,ldb,iret); pwstr=wstr;if(iret==2)then; goto 1001; elseif(iret==1)then; goto 1001; endif;do km=2,8; if(lr
     lin(0,km,is)==0)Exit; i=lrin(0,km,is); j=lrin(1,km,is);if(index(wstr(i:j),'cut(')==1)then;chw=''; read(wstr(i+4:j),*,end=50,err
     l=1002)chw
50    if(chw(3)/=''.or.chw(2)=='') goto 1002; read(chw(1),*,end=1002,err=1002)mv;if(mv0>0)then;if(mv/=mv0)then; wch='Problem Stateme
     lnt: numerical parameter in Cut(...) operation does not correspond to size '//
     +'of other elements in MultiConstraint or Inner Product: '//trim(pwstr);call putmess('S',6335,'MultiConstr/Func Checking',wch);
       goto 79999;endif;else; mv0=mv;endif;elseif(index(wstr(i:j),',...,')>0)then; call CheckFirstThreePoints(wstr(i:j),i1,j1);if(mv
     l0>0)then;if(j1+2/=mv0)then; wch='Problem Statement: number of objects defined by ,..., does not correspond to the size '//
     +'of other elements in MultiConstraint or Inner Product: '//trim(pwstr);call putmess('S',6336,'MultiConstr/Func Checking',wch);
       goto 79999;endif;else; mv0=j1+2;endif;else; mv=1; i1=1; i2=1; call RemoveInsideContent('()','',pwstr(i:j),wstr(i:j),iret);if(
     liret==1) goto 79999;do while(i2>0); i2=scan(wstr(i+i1-1:j),','); if(i2>0)mv=mv+1; i1=i1+i2; enddo;if(mv0>0)then;if(mv/=mv0)the
     ln; wch='Problem Statement: numbers of matrices in the list does not correspond to size of other '//
     +'elements in MultiConstraint or Inner Product: '//trim(pwstr);call putmess('S',6347,'MultiConstr/Func Checking',wch); goto 799
     l99;endif;else; mv0=mv;endif;endif;enddo;enddo;if(probaddr_w==0)then; probaddr_w=malloc(ibuff); iwbuff=ibuff; llast=lrin(0,-1,i
     lff)-1;call copybuff(probaddr,ibuff,probaddr_w,ibuff);if(iff==1.and.vector(0,1)=='')then; lrest=lrin(0,-1,1); call read_wstr(pC
     lhar,ibuff,wstr,lrest,ldb,iret);if(iret==2)then; goto 1001; elseif(iret==1)then; goto 1001; endif;wch=' '//trim(wstr)//' is con
     lsidered as a set of similar functions';call putmess('W',0,'MultiConstr/Func Checking',wch);endif;else; iwbuff=iBuffLen(probadd
     lr_w);endif;allocate(ivr(0:mv0)); ivr=1;if(kv==2.and.count(lrin(0,0:,0:kf)>0)==2)then;if(lrin(0,0,0)==0.and.lrin(0,1,0)>0
     +.or.lrin(0,0,0)>0.and.lrin(0,1,0)==0)then;do is=1,kf; if(lrin(0,1,is)>0) Exit; enddo;if(is<=kf)then; i=lrin(0,1,is);lrest=lrin
     l(0,-1,is); call read_wstr(pChar,ibuff,wstr,lrest,ldb,iret);if(iret==2)then; goto 1001; elseif(iret==1)then; goto 1001; endif;c
     lall IsFunctionRow(wstr,fnc_name,kfn,    w,chw,nfg,id,nf2);select case(nfg);case(2:3,21:22,37,53:54,73:74,77:79,82:83,93:94,112
     l:113,129:131,134,135); iv=1;case(14:15,38,55:56,75:76,80:81,95:96); iv=1;case(7:8,40:41,49:52,69:70,89:92,114:115); iv=-1;case
     l(16:19,45:48,67:68,85:88,136:137); iv=-1;case(36); if(nf2==361) iv=1;case default; iv=0;end select;if(iv/=0)then;  iz1=1; if(l
     lrin(0,0,0)>0) iz1=-1;if(w>0.)then; iz2=iz1; elseif(w<0.)then; iz2=-iz1; else; iz2=0; endif;iz2=iz2*iv;do nz=2,mv0;if(iz1>0)the
     ln; v1=vctr(nz,1); else; v1=-vctr(nz,1); endif;if(iz2>0)then; v2=vctr(nz,2); elseif(iz2<0)then; v2=-vctr(nz,2); else; v2=0.; en
     ldif;do is=1,nz-1; if(ivr(is)==0)Cycle;if(iz1>0)then; v11=vctr(is,1); else; v11=-vctr(is,1); endif;if(iz2>0)then; v22=vctr(is,2
     l); elseif(iz2<0)then; v22=-vctr(is,2); else; v22=0.; endif;if(v11>=v1.and.v22<=v2)then; ivr(is)=0; Cycle; endif;if(v11<=v1.and
     l.v22>=v2)then; ivr(nz)=0; Exit; endif;enddo;enddo;endif;endif;endif;endif;do nz=1,mv0; if(ivr(nz)==0)Cycle; kv=0; do is=iff,kf
     l;i=lrin(0,-1,is); call read_wstr(pChar,ibuff,wstr,i,ldb,iret); pwstr=wstr;if(iret==2)then; goto 1001; elseif(iret==1)then; got
     lo 1001; endif;if(wstr=='') Cycle;if(count(lrin(0,0:,is)>0)/=0)then;do nh=8,0,-1; if(lrin(0,nh,is)==0)Cycle;i=lrin(0,nh,is); j=
     llrin(1,nh,is);if(nh<2)then; kv=kv+1; iv=kv;if(is==0.and.nh==0.and.lrin(0,0,0)==lrin(0,1,0))Cycle;call Real2CharG(vctr(nz,iv),w
     lch);wstr=wstr(:i-1)//trim(wch)//wstr(j+1:);if(is==0)then; if(wstr(:5)=='multi') wstr=trim(wstr(6:)); endif;else;if(index(wstr(
     li:j),'cut(')==1)then; write(wch,*)nz;wstr=wstr(:i-1)//'takein('//trim(adjustl(wch))//','//wstr(i+4:);elseif(index(wstr(i:j),',
     l...,')>0)then; call GetNthMatrix(wstr(i:j),nz,wch);wstr=wstr(:i-1)//trim(adjustl(wch))//wstr(j+1:);else;mv=0; i1=0; i2=1; call
     l RemoveInsideContent('()','_',pwstr(i:j),wstr(i:j),iret);if(iret==1) goto 79999;do while(i2>0.and.mv<nz); i1=i1+i2; i2=scan(ws
     ltr(i+i1-1:j),','); mv=mv+1; enddo;if(i2>0)then; i2=i+i1-1+i2-2; else; i2=j; endif; wch=pwstr(i+i1-1:i2);wstr=wstr(:i-1)//trim(
     ladjustl(wch))//wstr(j+1:);endif;endif;enddo;endif;if(is==0.and.cname/='')then; i=index(wstr,trim(cname)); j=len_trim(cname);if
     l(i>1.and.wstr(:i-1)=='constraint:')then; write(wch,*)nz; wch=adjustl(wch);wstr=wstr(:i-1)//trim(cname)//'_'//trim(wch)//wstr(i
     l+j:);endif;endif;wstr=trim(wstr)//char(10); ln=len_trim(wstr);if(ln>=iwbuff-llast)then;call IncreaseBuffer(0,ln*(mv0-nz+1), pr
     lobaddr_w, iwbuff);endif;ia8=probaddr_w+llast; call copybuff(loc(wstr),ln,ia8,ln); llast=llast+ln;enddo; enddo;if(mv0==0)then; 
      do is=iff,kf;i=lrin(0,-1,is); call read_wstr(pChar,ibuff,wstr,i,ldb,iret); pwstr=wstr;if(iret==2)then; goto 1001; elseif(iret=
     l=1)then; goto 1001; endif;wstr=trim(wstr)//char(10); ln=len_trim(wstr);if(ln>=iwbuff-llast)then;call IncreaseBuffer(0,ln*100, 
     lprobaddr_w, iwbuff);endif;ia8=probaddr_w+llast; call copybuff(loc(wstr),ln,ia8,ln); llast=llast+ln;enddo; endif;goto 79999
60    continue;chw='Incorrect component in vector: '//trim(vector(i,j));call putmess('E',7691,'MultiConstr/Func Checking',chw); goto
     l 79999
1001  continue;write(chw,'(a)')'Internal Error in ChangePS_forLastMulti';call putmess('E',7695,'MultiConstr/Func Checking',chw); got
     lo 79999
1002  continue;chw='Problem Statement: incorrectly defined Cut(E) operation for matrix: '//trim(pwstr);call putmess('E',7696,'MultiC
     lonstr/Func Checking',chw); goto 79999
79999 continue;if(allocated(ncol))deallocate(ncol);if(allocated(nb)) deallocate(nb);if(allocated(vctr)) deallocate(vctr);if(allocate
     ld(ivr)) deallocate(ivr);return;end subroutine ChangePS_forLastMulti;subroutine IncreaseBuffer(i0,ladd,   probaddr,ibuff);use M
     lodCommons; USE CiFort;integer(PLEN)probaddr; integer(4) i0,ladd,ibuff,iBuffLen;integer(PLEN) ia8; integer(4) iw,lcl,ir; charac
     lter(1) ch0;character(1) wstr(*); pointer(it8,wstr);if(i0==0)ibuff=iBuffLen(probaddr);ia8=probaddr; probaddr=malloc(ibuff+ladd)
     l;lcl=0; ir=2**15; it8=malloc(ir); wstr(1:ir)='';do while(lcl<ladd+1); iw=min(ir,ladd+1-lcl);call copybuff(it8,iw,probaddr+ibuf
     lf-1+lcl,iw); lcl=lcl+iw;enddo;call free(it8);if(i0==0)then; call copybuff(ia8,ibuff-1,probaddr,ibuff-1);else;          call co
     lpybuff(ia8,ibuff,probaddr,ibuff);endif;call free(ia8);ibuff=ibuff+ladd; ch0=char(0);if(i0==0) call copybuff(loc(ch0),1,probadd
     lr+ibuff-1,1);return;end subroutine IncreaseBuffer;subroutine EMFunctions(probaddr,l16p,     probaddr_w,   wstr,pwstr,wch);USE 
     lCiFort; use ModCommons; use FuncNames;integer(PLEN) probaddr,probaddr_w;integer(4) l16p;integer(4)  iostat,nc,ideall,i,ibuff,l
     lrest,iret;real(8) w,xhuge,sign_min;integer(PLEN),pointer:: pChar;integer(PLEN) jaddr;logical  lDB;character(*) wstr, pwstr, wc
     lh;integer(4),allocatable:: kf(:),lrin(:,:,:);character(lnm),allocatable::obname(:,:),vector(:,:);character(1) ch0;integer(4)  
     liw,llast,lrst0,mxkf,mxconstr,kfnps;integer(4),external:: iBuffLen;pwstr='';l16p=l16kmax; ch0=char(0);probaddr_w=0; if(probaddr
     l<=0) RETURN;ibuff=0;call setpointer(probaddr, pChar); ibuff=iBuffLen(probaddr); if(ibuff<=0) RETURN;lDB=.true.; jaddr=0; lrest
     l=1;xhuge=huge(w)/2d0; sign_min=1d0;mxconstr=1;mxkf=10;allocate(
     +kf(1:mxconstr),
     +obname(mxkf,1:mxconstr),
     +STAT=iostat);if(iostat.ne.0)then; wch='Arrays allocation is failed'; call putmess('S',6240,'MultiFunc Checking',wch); goto 799
     l99; endif;allocate(vector(0:1,0:mxkf),lrin(0:1,-1:8,0:mxkf),
     +STAT=iostat);if(iostat.ne.0)then; wch='Arrays allocation is failed'; call putmess('S',6043,'MultiFunc Checking',wch); goto 799
     l99; endif;lDB=.true.;obname='';kf=0;wstr=''; vector=''; wch='';goto 100;goto 79999
100   continue;kfnps=0;lrin=0
62    continue;call ChangePS_forLastMulti(kfnps,probaddr,vector,lrin,1,kf(1),l16p,'',     probaddr_w,llast);if(ioutk>=istop-1) goto 
     l79999;vector='';lrin=0;nc=1; kf(nc)=0;wstr='';do while(wstr.eq.''.or.wstr(:1)=='%'); lrst0=lrest; call read_wstr(pChar,ibuff,w
     lstr,lrest,ldb,iret);if(iret==2)then; goto 1001; elseif(iret==1)then; goto 79999; endif;enddo;if(kf(nc)==mxkf) call Resize_mxkf
     l_arrays();call ProcessIfFucntionRow(probaddr,lrst0,lrest,mxkf,lrin,obname(:,nc),l16p,  wstr,kf(nc),  vector,iw, iret);if(iret=
     l=1) goto 79999;if(iw==1) kfnps=kfnps+1;GOTO 62
1001  continue;if(probaddr_w>0)then;call CopyBuff(loc(ch0),1,probaddr_w+llast,1);endif
79999 continue;ibuff=0;if(jaddr/=0)then; call free(jaddr); jaddr=0; endif;iostat=0; i=0;if(allocated(kf   )) deallocate(kf   ,stat=i
     l); iostat=iostat+i;if(allocated(obname))deallocate(obname,stat=i); iostat=iostat+i;if(allocated(vector))deallocate(vector,stat
     l=i); iostat=iostat+i;ideall=iostat;if(iostat.ne.0)then; wch="Can not deallocate all"; call putmess('W',0,'Deallocate in EMCons
     ltraints',wch);endif;RETURN;CONTAINS;subroutine Resize_mxkf_arrays();integer i,j;i=int(max(sizeof(obname),sizeof(vector)*2,size
     lof(lrin)*2*10))*(mxkf+1); jaddr=malloc(i); j=mxkf*2;i=int(sizeof(obname))*mxkf;call copybuff(loc(obname),i,jaddr,i); deallocat
     le(obname);allocate(obname(j,1)); call copybuff(jaddr,i,loc(obname),i);i=int(sizeof(vector))*2*(mxkf+1); call copybuff(loc(vect
     lor),i,jaddr,i); deallocate(vector);allocate(vector(0:1,0:j)); call copybuff(jaddr,i,loc(vector),i);i=4*2*10*(mxkf+1);call copy
     lbuff(loc(lrin),i,jaddr,i); deallocate(lrin); allocate(lrin(0:1,-1:8,0:j)); call copybuff(jaddr,i,loc(lrin),i);call free(jaddr)
     l; jaddr=0; mxkf=j;end subroutine Resize_mxkf_arrays;END SUBROUTINE EMFunctions;subroutine ExtractThreePointsForAllFunctions(pr
     lobaddr,   probaddr_w,  wstr, pwstr, wch);USE CiFort; use ModCommons; use FuncNames;integer(PLEN) probaddr,probaddr_w;character
     l(*) wstr, pwstr, wch;integer(4),external:: iBuffLen;integer(4)  i,j,id,ibuff,lrest,nfg,nf2,lrst0,iwbuff,iret;real(8) w;integer
     l(PLEN),pointer:: pChar;integer(PLEN) probaddr_t;logical lDB;character(l16kmax),external:: move_space;character(1) ch0;ibuff=0;
       ch0=char(0); lrst0=0;call setpointer(probaddr, pChar); ibuff=iBuffLen(probaddr); if(ibuff<=0) RETURN;probaddr_w=0;lDB=.true.;
      wstr=''; lrest=1
62    continue;if(probaddr_w>0)then; j=len_trim(wstr)+1;if(probaddr_t+j>probaddr_w+iwbuff)then; i=int(probaddr_t-probaddr_w);call In
     lcreaseBuffer(0,j, probaddr_w, iwbuff); probaddr_t=probaddr_w+i;endif;if(j>1)then; wstr(j:j)=char(10); call CopySubBuff(1,loc(w
     lstr),1,j, probaddr_t); probaddr_t=probaddr_t+j;endif;endif;wstr='';do while(wstr.eq.''.or.wstr(:1)=='%'); lrst0=lrest; call re
     lad_wstr(pChar,ibuff,wstr,lrest,ldb,iret);if(iret==2)then; goto 1001; elseif(iret==1)then; goto 79999; endif;enddo;wstr=move_sp
     lace(wstr);call IsFunctionRow(wstr,fnc_name,kfn,    w,wch,nfg,id,nf2);if(nf2<0)then;i=scan(trim(wstr),'('); i=scan(wstr(:i-2),'
     l*');if(i>0) call IsFunctionRow(wstr(i+1:),fnc_name,kfn,    w,wch,nfg,id,nf2);if(nf2<0) GOTO 62;endif;call FindFirstThreePoints
     lBlock(wstr,i,j);if(i>0)then; i=len_trim(wstr); call CompleteThreePoints(wstr,pwstr);if(probaddr_w<=0)then; j=len_trim(wstr);iw
     lbuff=ibuff+(j-i)+1; probaddr_w=malloc(iwbuff);call CopySubBuff(1,probaddr,1,lrst0-1, probaddr_w);  probaddr_t=probaddr_w+lrst0
     l-1;endif;endif;GOTO 62;goto 79999
1001  continue;if(probaddr_w>0) call CopyBuff(loc(ch0),1,probaddr_t-1,1)
79999 continue;return;end subroutine ExtractThreePointsForAllFunctions;subroutine IsFRwithVector(fnc_name,kfn,
     +wstr,
     +w,fname,nfg,id,nf2,vector1);integer(4) kfn,nfg,id,nf2; real(8) w;character(*) wstr,fname,fnc_name(0:kfn,0:*),vector1;integer(4
     l)  i;character(256)  chm(2);fname=''; nf2=-1; vector1='';call IsFunctionRow(wstr,fnc_name,kfn,    w,fname,nfg,id,nf2);if(nf2<0
     l)then; i=scan(trim(wstr),'('); i=scan(wstr(:i-2),'*');if(i>1)then;call IsFunctionRow(wstr(i+1:),fnc_name,kfn,    w,fname,nfg,i
     ld,nf2);if(nf2>=0)then; vector1=adjustl(wstr(:i-1));if(len(vector1)<len_trim(adjustl(wstr(:i-1))))then;chm="Problem statement: 
     lLenght of vector name exceeds maximum (128) in string: "//trim(wstr);call putmess('S',6293,'Function row',chm); goto 79999;end
     lif;chm=''; read(vector1,*,end=762,err=15555)chm
762   if(.not.(chm(1)/=''.and.chm(2)==''))goto 15555;wstr=adjustl(wstr(i+1:));endif;endif;endif;if(nf2>0) fname=adjustl(wstr(:scan(w
     lstr,'(')-1));RETURN
15555 chm="Problem statement: Incorrect vector name in string: "//trim(wstr); call putmess('S',6294,'Function row',chm)
79999 vector1=''; nf2=-1; fname='';return;
      end subroutine IsFRwithVector
