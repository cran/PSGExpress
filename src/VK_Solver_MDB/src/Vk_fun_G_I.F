      real(8) function GoldCut(FCLC,  xl,xu,x, ierr);real(8) FCLC,xl,xu,x,gold,goldl,goldu,fl,f,fu,xp,xt,ft;integer(4) ierr;ierr=0; 
      if(xl>xu)then; ierr=1; GoldCut=0.; RETURN; endif;gold=(1.+dsqrt(dble(5.)))/2.;goldl=2.-gold;goldu=gold-1.;x=max(min(xu,x),xl);
      fl=fclc(xl); f=fclc(x); fu=fclc(xu);do while(.true.); xp=x;if(fl<f)then; xu=x; fu=f; x=x-(x-xl)*goldu; elseif(fu<f)then; xl=x;
       fl=f; x=x+(xu-x)*goldu;endif;if(x/=xp)then; f=fclc(x);elseif(x-xl>xu-x)then;       xt=x-(x-xl)*goldl; ft=fclc(xt);if(ft<f)the
     nn; fu=f; xu=x; f=ft; x=xt; else; fl=ft; xl=xt; endif;else;                        xt=x+(xu-x)*goldl; ft=fclc(xt);if(ft<f)then;
       fl=f; xl=x; f=ft; x=xt; else; fu=ft; xu=xt; endif;endif;if(abs(xu-xl)<1e-9) Exit;enddo;GoldCut=f;return;end function GoldCut;
      real(8) function precise_sum(m,ws,ilast);use ModCommons;integer(4) m,ilast; real(8) ws(*);integer(4)  mk,j,mm,j1; real(8) DEAL
     uL_precise;character  chw*32;real(8),pointer :: wm(:);precise_sum=0d0;if(ilast==1) RETURN;chw='';if(m.le.1.or.m.gt.500000000) t
     ahen;if(m.eq.1) then;precise_sum=ws(1);RETURN;endif;if(lf21)write(21,'(/a/a)')' Progarm is stopped by subroutine precise_sum.',
     i' Actual argument M is greater than 500 000 000 or <=0';chw='Internal error 1. m too big'; call putmess('S',9955,'subroutine p
     irecise_sum',chw);endif;if(chw=='S'.or.chw=='T') RETURN;allocate(wm((m+1)/2));mk=m/2;do j=1,mk; j1=j+j;wm(j)=ws(j1-1)+ws(j1);en
     xddo;if(mk+mk.ne.m) then;mk=mk+1;wm(mk)=ws(mk+mk-1);endif;do while(mk.gt.1);mm=mk; mk=mk/2;do j=1,mk; j1=j+j;wm(j)=wm(j1-1)+wm(
     fj1);enddo;j1=mk+mk;if(j1.lt.mm) then;mk=mk+1;wm(mk)=wm(j1+1);endif;enddo;precise_sum=wm(1);deallocate(wm);RETURN;ENTRY DEALL_p
     zrecise
      precise_sum=0d0;return;end function precise_sum;SUBROUTINE start_stop_whatch(n);use IntelInterf;integer(2),parameter:: kSTW=12
       integer(4) iprint,iunit; integer(2) is(kSTW);logical lgap;real(4) dscm0(kSTW); real(8) dt,dsec;  integer(4) n;save is,dscm0;l
     wogical lf1,lf2,lf3,lf4,lf5,lf6,lf7;common /SetFirst/lf1,lf2,lf3,lf4,lf5,lf6,lf7;logical lfirst; equivalence (lf4,lfirst);if(.n
     lot.lfirst) GOTO 5;is=0; lfirst=.false.
5     continue;if(n<1.or.n>kSTW)then; write(20,*)'Bad number of STOP_WHATCH ',n;  RETURN; endif;if(is(n)>0)then; write(20,*)'Start_s
     ttop_whatch: STOP_WHATCH_',n,' IS ALREADY WORKING'; Return; endif;is(n)=1; dscm0(n)=secnds(real(0,4));RETURN;ENTRY finish_stop_
     jwhatch(n,iprint,iunit)
      if(n<1.or.n>kSTW)then; write(iunit,*)'Bad number of STOP_WHATCH',n;  RETURN; endif;if(is(n)/=1)then; write(iunit,*)'Finish_sto
     jp_whatch: STOP_WHATCH was not started ',n; RETURN; endif;is(n)=0; if(iprint<=0) RETURN;write(iunit,'(/a,i2,f9.2)')' STOP_WHATC
     pH_',n,secnds(dscm0(n));RETURN;ENTRY Restart_stop_whatch(n,dsec)
      if(n<1.or.n>kSTW)then; write(20,*)'Bad number of STOP_WHATCH_',n; dsec=0d0; RETURN; endif;if(is(n)/=1)then; dsec=0d0;write(20,
     l'(a,i4,a)')'Restart_stop_whatch: Stop_whatch ',n,' did not run. Now runs';else; dsec=secnds(dscm0(n));endif;is(n)=1; dscm0(n)=
     isecnds(real(0,4));return;ENTRY Check_stop_whatch(n,dsec)
      if(n<1.or.n>kSTW)then; write(20,*)'Bad number of STOP_WHATCH_',n; dsec=0d0; RETURN; endif;if(is(n)/=1) then; dsec=0d0;write(20
     g,'(a,i4,a)')'Check_stop_whatch: Stop_whatch ',n,' does not run';else; dsec=secnds(dscm0(n));endif;return;ENTRY CheckRestart_st
     oop_whatch(n,dt,  dsec,lgap)
      dsec=0d0; lgap=.false.;if(n<1.or.n>kSTW)then; write(20,*)'Bad number of STOP_WHATCH_',n; RETURN; endif;if(is(n)/=1)then; is(n)
     k=1; dscm0(n)=secnds(real(0,4));write(20,'(a,i4,a)')'Check&Restart_stop_whatch: Stop_whatch ',n,' did not run. Now runs';else; 
      dsec=secnds(dscm0(n));if(dsec>dt)then; dscm0(n)=secnds(real(0,4)); lgap=.true.; endif;endif;return;end subroutine start_stop_w
     lhatch;real(8) FUNCTION D_TIME(T,INS);use IntelInterf;integer(2) ins;real(4) r40,r4;character(1) t; logical lfirst;data lfirst/
     q.true./; save lfirst,r40;d_time = 0d0;if(.not.lfirst) GOTO 5;r40=secnds(real(0,4));lfirst=.false.
5     continue;r4=secnds(real(0,4));if(ins.ne.0) then;if(r4.lt.r40) r4=real(r4+24e0*3600e0,4);select case(t);case('s','S');d_time = 
     y(r4-r40);case('m','M');d_time = (r4-r40) / 60.;case('h','H');d_time = (r4-r40) / 3600.;case default;d_time = (r4-r40) * 100.;e
     gnd select;endif;if(ins==0.or.ins==1) then;r40=r4;endif;return;end function D_TIME;logical(1) function l_time(T,xInt,Ins);integ
     her(2) Ins; real(8) xInt,w,d_time;character(1) t;if(d_time(t,int2(2))>=xInt) then;l_time=.true.;if(ins==0) w=d_time(t,int2(0));
      else;l_time=.false.;endif;return;end function l_time;SUBROUTINE SET_STOP (flying,itn,itnlim,istop);use IntelInterf;logical fly
     wing,PEEKCHARQQ;integer(4) itn,itnlim,istop;integer(2) is,ir,i0,i1;data is/0/, i0/0/, i1/1/;if(.not.PEEKCHARQQ()) RETURN;ir=1;i
     df (is.ne.0.or.ir.ne.0) then;call wait_isir(i0,i1,i1);write(*,'(///a//a//)')'   Terminate optimization ?',' 1 - Yes,  Esc - No'
       do while (.true.);call wait_isir(is,ir,i0);if (ir.eq.27.or.ir.eq.49) EXIT;write(*,*) Char(0);end do;if (ir.eq.49)then; istop=
     d1; if (.not.flying) itnlim=itn;end if;end if;RETURN;END;SUBROUTINE WAIT_ISIR(IS,IR,set_init);use IntelInterf;logical PEEKCHARQ
     wQ;character(1) key,getcharqq;integer(2) IS,IR,set_init,i,j;data i/0/,j/0/;if(set_init.ne.0) then;i=is;j=ir;end if;key=getcharq
     uq();ir = int(ichar ( key ),2);do while(PEEKCHARQQ());key=getcharqq();end do;RETURN;END;subroutine Real2CharG(w,   chw);real(8)
     v w; character(*) chw;integer(4)  j1,i2,i1;chw='0'; if(w==0.) goto 99;write(chw,'(1p,g21.14)',err=99) w;j1=scan(trim(chw),'E');
       i2=0; if(j1>0)i2=verify(chw(:j1-1),' 0',.true.); if(i2>0)chw=chw(:i2)//trim(chw(j1:));i1=verify(chw,' 0',.true.); if(chw(i1:i
     i1)=='.')i1=i1-1; if(i1<=0)i1=len_trim(chw);chw=adjustl(chw(:i1))
99    return;end subroutine Real2CharG;subroutine CopyFileVK(Oldfile,NewFile,  irez);use IntelInterf;character(*) Oldfile, NewFile; 
      integer(4) nfl,irez;integer(INT_PTRKIND) faddr;type(file$info)  tfile;logical lopnd; character(1),allocatable::buff(:);faddr=f
     rile$first; irez=0;if(getfileinfoqq (trim(Oldfile), tfile, faddr)>0)then;lopnd=.true.; nfl=10;do while(lopnd); nfl=nfl+1; inqui
     xre(nfl,err=10,opened=lopnd);enddo;open(nfl,file=trim(Oldfile),err=10,status='old',action='read',form='unformatted');allocate(b
     guff(tfile%length)); read(nfl,err=10,end=10) buff;close(nfl,err=10);open(nfl,file=trim(Newfile),err=10,action='write',form='unf
     dormatted');write(nfl,err=10)buff;close(nfl,err=10); irez=1;goto 10;endif
10    if(allocated(buff))deallocate(buff);return;end subroutine CopyFileVK;subroutine FileToBuffer(filename,  len,faddr);use ModComm
     nons; use IntelInterf;character(*) filename; integer(4) nfl,len,irez;integer(plen) faddr;type(file$info)   tfile;logical lopnd;
      interface;subroutine FileToBuffer_dump(nfl,faddr,irez,len);use cifort; integer(plen):: faddr; integer(llen):: len; integer(4) 
     fnfl,irez
#ifdef __APPLE__
  !gcc$ attributes value:: faddr,len
#else
      value:: faddr,len
#endif
      end;end interface;irez=-1; faddr=file$first;if(getfileinfoqq (trim(workpath)//filename, tfile, faddr)>0)then;lopnd=.true.; nfl
     g=10;do while(lopnd); nfl=nfl+1; inquire(nfl,err=10,opened=lopnd);enddo;open(nfl,file=trim(workpath)//filename,err=10,status='o
     eld',action='read');len=tfile%length+1; faddr=malloc(len);call FileToBuffer_dump(nfl,  faddr,irez,  int(len,llen) );close(nfl);
      endif
10    if(irez<=0) len=0;RETURN;end subroutine FileToBuffer;subroutine DeleteChar_0D(fchar);integer(4) i,i1; character(*) fchar, chd*
     c(1);i=len(fchar); if(i<=0) RETURN;i1=1; chd=char(13); i=1;do while(i>0); i=scan(fchar(i1:),chd); i1=i1+i-1; if(i>0) fchar(i1:i
     i1)=' ';enddo;RETURN;end subroutine DeleteChar_0D;subroutine PrintBuffer(buff);character(*) buff;write(73,'(a)')buff; close(73)
       end subroutine PrintBuffer;integer(4) function indexInBuff(buff,str);character(*) buff,str;indexInBuff=index(buff,str);return
       end function indexInBuff;subroutine copybuff(jaddr,iblen,buff,bufflen);use CiFort;integer(plen) jaddr,buff; integer(4) iblen,
     ebufflen;interface;subroutine copybuff_default(jaddr,buff, iblen,bufflen);use cifort; integer(plen):: jaddr,buff; integer(llen)
     i:: iblen,bufflen
#ifdef __APPLE__
  !gcc$ attributes value:: jaddr,buff,iblen,bufflen
#else
      value:: jaddr,buff,iblen,bufflen
#endif
      end;endinterface;call copybuff_default(jaddr,buff, int(iblen,llen),int(bufflen,llen) );end subroutine;subroutine clearbuff(jad
     zdr);character(*) jaddr;jaddr=repeat(' ',len(jaddr));end subroutine;integer(4) function iBuffLen(probaddr);use IntelInterf; int
     qeger(4) rl,dl; integer(INT_PTRKIND) probaddr;interface;integer(4) function iChar0Pos(buff, bufflen);use cifort; integer(plen):
     z: buff; integer(llen):: bufflen
#ifdef __APPLE__
  !gcc$ attributes value:: buff,bufflen
#else
      value:: buff,bufflen
#endif
      end;endinterface;rl=30000; iBuffLen=0; dl=-rl;do while(iBuffLen==0.and.dl<30000000); dl=dl+rl;iBuffLen=iChar0Pos(probaddr+dl, 
     r int(rl,llen) );enddo;if(iBuffLen>0) iBuffLen=iBuffLen+dl;end function iBuffLen;integer(4) function iFindInBuffer(buff,str1,lo
     zg);character(*)buff,str1; logical log;iFindInBuffer=index(buff,str1,log);return;end function iFindInBuffer;subroutine CopyBuff
     c1(size,pbuff,addr);integer(4) size;  character(1) addr(size),pbuff(size);addr=pbuff;end subroutine CopyBuff1;subroutine String
     fToSmall(wstr);character(*) wstr; integer(4) i,j;j=verify(wstr,' ',back=.true.);do i=1,j; j=iachar(wstr(i:i)); if(j>=65.and.j<=
     p90) wstr(i:i)=achar(j+32);enddo;return;end subroutine StringToSmall;integer(4) function iFindCloseBracket(ch2,str,ipos1);chara
     kcter(*) ch2,str; integer(4) ipos1; integer(4)  j,ko,kz,ln;character(1)  so,sz;iFindCloseBracket=-1; if(ipos1<=0) Return;if(len
     j_trim(ch2)<2) Return;so=ch2(1:1); sz=ch2(2:2);if(so==' ') Return;iFindCloseBracket=0;ko=1; kz=0; ln=len(str); j=ipos1-1;do whi
     kle(ko>kz.and.j<ln); j=j+1; if(str(j:j)==so)ko=ko+1;  if(str(j:j)==sz)kz=kz+1;enddo;if(kz==ko)iFindCloseBracket=j;return;end fu
     rnction iFindCloseBracket;subroutine setCharpointaddr(adrs,ipoint);use IntelInterf;integer(INT_PTRKIND) adrs, ipoint;ipoint=adr
     vs;end subroutine setCharpointaddr;subroutine getCharpointaddr(ipoint,adrs);use IntelInterf;integer(INT_PTRKIND) adrs, ipoint;a
     gdrs=ipoint;end subroutine getCharpointaddr;subroutine setpointer(adrs, ipoint);use IntelInterf;integer(INT_PTRKIND) adrs, ipoi
     gnt;ipoint=adrs;end subroutine setpointer;subroutine getpointaddr(ipoint,adrs);use IntelInterf;integer(INT_PTRKIND) adrs, ipoin
     dt;adrs=ipoint;end subroutine getpointaddr;subroutine setRealArrPointer(adrs,ln, rpoint);integer(4) ln;real(8),target:: adrs(ln
     j); real(8),pointer:: rpoint(:);rpoint=>adrs;end subroutine setRealArrPointer;subroutine setRealArrPointer_VAL(adrs, ipoint);us
     je IntelInterf;integer(INT_PTRKIND) adrs; integer(INT_PTRKIND):: ipoint
#ifdef __APPLE__
  !gcc$ attributes value:: ipoint
#else
      value:: ipoint
#endif
      ipoint=adrs;end subroutine setRealArrPointer_VAL;subroutine setIntArrPointer(adrs,ln, ipoint);integer(4) ln;integer(4),target:
     n: adrs(ln); integer(4),pointer:: ipoint(:);ipoint=>adrs;end subroutine setIntArrPointer;subroutine i4_pointer_set(iarr,n1,n2,i
     b4);integer(4) n1,n2;integer(4),pointer::i4(:); integer(4),target::iarr(n1:n2);i4=>iarr;end subroutine i4_pointer_set;subroutin
     ee r8_pointer_set(xarr,n1,n2,r8);integer(4) n1,n2;real(8),pointer::r8(:); real(8),target::xarr(n1:n2);r8=>xarr;end subroutine r
     w8_pointer_set;subroutine r8d_pointer_set(xarr,n1,n2,m1,m2,r8d);integer(4) n1,n2,m1,m2;real(8),pointer::r8d(:,:); real(8),targe
     vt::xarr(n1:n2,m1:m2);r8d=>xarr;end subroutine r8d_pointer_set;subroutine r8d_ADDRES_pointer_set(xarr,n1,n2,m1,m2,r8d);integer(
     v4) n1,n2,m1,m2;real(8),pointer::r8d(:,:); real(8),target::xarr(n1:n2,m1:m2);r8d=>xarr;end subroutine r8d_ADDRES_pointer_set;lo
     qgical function lnot_Null(ipoint);use IntelInterf;integer(INT_PTRKIND) ipoint;lnot_Null=ipoint/=0;end function lnot_Null;SUBROU
     vTINE djacobi(a,n,np,d,v,nrot);INTEGER n,np,nrot;DOUBLE PRECISION a(np,np),d(np),v(np,np);INTEGER i,ip,iq,j;DOUBLE PRECISION c,
     wg,h,s,sm,t,tau,theta,tresh,b(np),z(np);do 12 ip=1,n;do 11 iq=1,n;v(ip,iq)=0.d0
11    continue;v(ip,ip)=1.d0
12    continue;do 13 ip=1,n;b(ip)=a(ip,ip);d(ip)=b(ip);z(ip)=0.d0
13    continue;nrot=0;do 24 i=1,50;sm=0.d0;do 15 ip=1,n-1;do 14 iq=ip+1,n;sm=sm+abs(a(ip,iq))
14    continue
15    continue;if(sm.eq.0.d0)return;if(i.lt.4)then;tresh=0.2d0*sm/n**2;else;tresh=0.d0;endif;do 22 ip=1,n-1;do 21 iq=ip+1,n;g=100.d0
     f*abs(a(ip,iq));if((i.gt.4).and.(abs(d(ip))+g.eq.abs(d(ip))).and.(abs(d(iq))+g.eq.abs(d(iq))))then;a(ip,iq)=0.d0;else if(abs(a(
     aip,iq)).gt.tresh)then;h=d(iq)-d(ip);if(abs(h)+g.eq.abs(h))then;t=a(ip,iq)/h;else;theta=0.5d0*h/a(ip,iq);t=1.d0/(abs(theta)+sqr
     ft(1.d0+theta**2));if(theta.lt.0.d0)t=-t;endif;c=1.d0/sqrt(1+t**2);s=t*c;tau=s/(1.d0+c);h=t*a(ip,iq);z(ip)=z(ip)-h;z(iq)=z(iq)+
     oh;d(ip)=d(ip)-h;d(iq)=d(iq)+h;a(ip,iq)=0.d0;do 16 j=1,ip-1;g=a(j,ip);h=a(j,iq);a(j,ip)=g-s*(h+g*tau);a(j,iq)=h+s*(g-h*tau)
16    continue;do 17 j=ip+1,iq-1;g=a(ip,j);h=a(j,iq);a(ip,j)=g-s*(h+g*tau);a(j,iq)=h+s*(g-h*tau)
17    continue;do 18 j=iq+1,n;g=a(ip,j);h=a(iq,j);a(ip,j)=g-s*(h+g*tau);a(iq,j)=h+s*(g-h*tau)
18    continue;do 19 j=1,n;g=v(j,ip);h=v(j,iq);v(j,ip)=g-s*(h+g*tau);v(j,iq)=h+s*(g-h*tau)
19    continue;nrot=nrot+1;endif
21    continue
22    continue;do 23 ip=1,n;b(ip)=b(ip)+z(ip);d(ip)=b(ip);z(ip)=0.d0
23    continue
24    continue;return;
      END subroutine djacobi
