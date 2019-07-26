      subroutine ListOfOutputObjects(probaddr,  probaddr_w,   wstr, pwstr, wch);USE CiFort; use ModCommons; use FuncNames;integer(PL
     jEN) probaddr,probaddr_w;character(*) wstr, pwstr, wch;integer(4)  i,j,j1,i1,i2,id,ibuff,lrest,nfg,nf2,nf1,lrst0,nc,iOpn,io,ko,
     miret;real(8) w;integer(PLEN),pointer:: pChar;logical lDB;character cname*256,fname*256,ch1*1, row*1024,rww*1024;character(l16k
     vmax),external:: move_space;character(256),allocatable:: objects(:);integer(4), external:: iBuffLen, istr_without_sub,iCheckPSG
     dName, iFindCloseBracket;character(lnm),external:: MnameFromCutTake;ibuff=0; probaddr_w=0;call setpointer(probaddr, pChar); ibu
     nff=iBuffLen(probaddr); if(ibuff<=0) RETURN;cname='objective'; nc=0;ko=1000; allocate(objects(ko));io=0; objects='';if(newin>=0
     h)then; io=io+1; objects(io)='point_'//probnm; endif;lDB=.true.;lrest=1
62    continue;wstr='';do while(wstr.eq.''.or.wstr(:1)=='%'); lrst0=lrest; call read_wstr(pChar,ibuff,wstr,lrest,ldb,iret);if(iret==
     t2)then; goto 1001; elseif(iret==1)then; goto 79999; endif;enddo;pwstr=wstr; wstr=move_space(wstr);if(wstr(:15)=='multiconstrai
     snt'.and.scan(wstr(16:16),' ,:_=<>')==1  .or.wstr(:10)=='constraint'.and.scan(wstr(11:11),' ,:_=<>')==1  )then;i=index(wstr,'co
     bnstraint'); wstr=wstr(i+10:); if(scan(wstr(:1),' :_,')==1)wstr=wstr(2:);wstr=adjustl(wstr);wch=''; read(wstr,*,err=102,end=102
     u)wch; i=index(trim(wch),':'); if(i>0)wch=wch(:i-1);i=scan(wch,'><='); if(i>0)wch=wch(:i-1)
102   if(wch/='linearize'.and.wch(:10)/='linearize='.and.wch(:1)/='<'.and.wch(:1)/='>'.and.wch(:1)/='='.and.wch/='lower_bound'.and.w
     bch/='upper_bound'.and.wch(:12)/='lower_bound='.and.wch(:12)/='upper_bound=')then;i=istr_without_sub(wstr,trim(wch),wstr);i=ver
     zify(wstr,' :,'); if(i>0)wstr=wstr(i:);nc=nc+1; cname=wch;endif;if(nc>0.and.cname=='')then; write(cname,*)nc; cname='constraint
     i_'//adjustl(cname(:99)); endif;endif;if(wstr(:11)=='multivalue:'.or.wstr(:11)=='multivalue ' .or. wstr(:6)=='value:'.or.wstr(:
     h6)=='value ')then;cname='value'; nc=nc+1;endif;i=index(wstr,'('); if(i<=0) goto 62;j=scan(wstr(:i),'* ',.true.); if(j>0) wstr=
     cwstr(j+1:);call IsFunctionRow(wstr,fnc_name,kfn,    w,wch,nfg,id,nf2);if(nfg==127)then; i=index(wstr,'('); j=index(wstr,')',.t
     nrue.);if(j-1>i+1)then; wstr=wstr(i+1:j-1); else; goto 15555; endif;call IsFunctionRow(wstr,fnc_name,kfn,    w,wch,nfg,id,nf2);
      endif;nf1=nf2; fname=wch;select case(nf2);case(0:1400);i=index(wstr,'('); j=index(wstr,')',.true.);if(.not.(i>0.and.i<j-1)) go
     nto 15555;i1=i+index(wstr(i+1:j-1),'('); j1=i+index(wstr(i+1:j-1),')',.true.);if(i1>0.and.i1<j1-1)then; wstr=wstr(i+1:j-1);call
     o IsFunctionRow(wstr,fnc_name,kfn,    w,wch,nfg,id,nf2);endif;end select;if(nf2<0.and.nf1<0) GOTO 62;if(io+10>ko)then; j=int(ko
     o*size(objects)*256); probaddr_w=malloc(j);call CopyBuff(loc(objects),j,probaddr_w,j); deallocate(objects);ko=ko*2+10; allocate
     w(objects(ko)); call CopyBuff(probaddr_w,j,loc(objects),j); call free(probaddr_w); probaddr_w=0;endif;iOpn=index(wstr,'('); if(
     ciOpn<=0) goto 15555;if(nf1==1190.or.nf2==1190)then;i=iOpn; i2=0; i1=index(wstr(i+1:),'matrix_');if(i1>0)i2=index(wstr(i+i1+1:)
     f,',');i=i+i1+i2; j=0; if(i2>0)then; j=iFindCloseBracket('()',wstr,i); else; i=-1; endif;row=''; if(j>i+1) row=wstr(i+1:j-1);ca
     gll RemoveInsideContent('()','',row,     rww,j); if(j/=0) goto 15555;j=index(rww,',',.true.);if(i>=0.and.j<=0)then;io=io+2; obj
     yects(io-1)=MnameFromCutTake('',row,'knots'); objects(io)=MnameFromCutTake('',row,'quants');if(iCheckPSGName(trim(objects(io)),
     w'NoSay')<0) goto 15555;endif;endif;select case(nf1);end select;GOTO 62
15555 wch='Problem Statement: incorrect string: '//trim(pwstr); call putmess('S',6183,'Functions row',wch);goto 79999
1001  continue;if(io==0) goto 79999;probaddr_w=malloc(int(io*(256*size(objects)+1),4)); lrst0=0;do i=1,io; wch=trim(objects(i));sele
     jct case(wch(:7));case('point_i','point_g');i1=len_trim(wch); j=count(wch==objects(:i-1));if(j>0)then; write(cname,*)j+1; wch(i
     p1+1:)='_'//trim(adjustl(cname));elseif(count(wch==objects(i+1:io))>0)then; wch(i1+1:)='_1';endif;case('point_p','matrix_');cas
     le default; Cycle;end select;wch=trim(wch)//char(10); j=len_trim(wch);call CopyBuff(loc(wch),j,probaddr_w+lrst0,j); lrst0=lrst0
     i+j;enddo;if(lrst0<=0) lrst0=1;ch1=char(0);call CopyBuff(loc(ch1),1,probaddr_w+lrst0-1,1)
79999 continue;if(allocated(objects)) deallocate(objects);return;end subroutine ListOfOutputObjects;integer(4) function iCheckPSGNam
     oe(name,word);use modcommons;character(*) name,word,wch*256; integer(4) i,j0,j;iCheckPSGName=1; i=1;j0=verify(name,' '); j=veri
     bfy(name,' ',back=.true.);if(j0>0)then;do i=j0,j; j=iachar(name(i:i));select case(j);case( 48:57,   59:90,   95,  97:122,  58, 
     v35, 36, 38);case default; iCheckPSGName=-1; Exit;end select;enddo;endif;if(iCheckPSGName==-1.and.word=='Say')then;wch='Problem
     s Statement: incorrect name: '//trim(name); call putmess('S',8001,'Name checking',wch); goto 79999;endif;if(i-1>Lnm)then; iChec
     ukPSGName=-1;if(word=='Say')then; wch='Problem Statement: PSG gets too long name. Make it shorter: '//trim(name);call putmess('
     qS',8001,'Name checking',wch); goto 79999;endif;endif
79999 return;
      end function iCheckPSGName
