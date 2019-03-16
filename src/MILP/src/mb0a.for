      subroutine ssx10(mx,n1x,  mxmem,  workpath0,un0,
     +mapi, mapdp );integer(4) mx, n1x, mxmem, mapi(*),mapdp(*);integer(1) un0; character(256) workpath0
      include 'mifcomx.inc'
      integer(1) un; common/un/un;character(256) workpath;integer(4),parameter:: bchmax = 200;integer(4) mxnsub;integer(4) inpk,inpp
     l,log_pr,istop,iDB;common /contr/ inpk,inpp,log_pr,istop,iDB, workpath;workpath=workpath0; un=un0;m = mx;n1 = n1x;n = m-1 + n1;
      mxnsub = 4;mapdp(1) = 1;mapi(1) = 1;mapdp(10) = mxmem - m + 1;mapdp(11) = mapdp(10) - n1;mapdp(15) = mapdp(11) - m;mapdp(13) =
     l mapdp(15) - n1;mapdp(3) = mapdp(13);mapdp(14) = mapdp(13) - m;mapdp(12) = mapdp(14) - n1;mapdp(2) = mapdp(14) - n - 1;mapdp(1
     l6) = mapdp(2) - m;mapdp(18) = mapdp(16) - (m-1)/8 - 1;mapdp(17) = mapdp(18) - m * mxnsub - 1;mapdp(5) = mapdp(17) - n;mapdp(6)
     l = mapdp(5) - n;mapdp(7) = mapdp(6) - 4*((n-1)/8 + 1);mapi(14) = mxmem+1 - n1 - 1;mapi(12) = mapi(14) - m;mapi(19) = mapi(12) 
     l- n1;mapi(20) = mapi(19) - n1;mapi(9) = mapi(20) - (m + n1 );mapi(2) = mapi(9) - (2 * m + bchmax);goto 10;mapi(9) = mapi(14) -
     l (m + n1 + 1);mapi(12) = mapi(9) - m;mapi(19) = mapi(12) - n1;mapi(20) = mapi(19) - n1;mapi(2) = mapi(20) - (2 * m + bchmax)
10    continue;mapi(3) = mapi(2) - (2 * m + bchmax);mapi(4) = mapi(3) - m;mapi(7) = mapi(4) - m;mapi(6) = mapi(7) - m;mapi(5) = mapi
     l(6) - m;mapi(15) = mapi(5) - m - 4;mapi(16) = mapi(15) - m - 4;mapi(17) = mapi(16) - m - 4;mapi(18) = mapi(17) - 2 * ((n-1)/4 
     l+ 1);mapi(13) = mapi(18) - (n+1);RETURN;end subroutine ssx10;subroutine ssx11(ipar,mx,n1x,memwork,  lb,ub, mapi,mapdp, ialgtyp
     l,lscale,
     +idb0,idbt,timelimit,
     +dparr,intarr,   dpwork,intwork,
     +ipos,sinfeas );use IntTypes;interface;subroutine tomilp_1(m,n1,nz,mc,big, i1,d1,i14,i19,i20, i12,i9,d14, d12,d13, d18, d3, d5,
     lione);use IntTypes; integer(llen),value:: ione;integer(4) m,n1,nz,mc,i1,i14,i19,i20,i12,i9; real(8) big,d1,d14,d12,d13,d18,d3,
     ld5;end subroutine tomilp_1;subroutine tomilp_for_Crash(m,n1,nz,mc,big,i1,d1,i14,i19,i20,i12,i9,d14,d12,d13,d18,d3,d5,i5,d2,ion
     le);use IntTypes; integer(llen),value:: ione;integer(4) m,n1,nz,mc,i1,i14,i19,i20,i12,i9,i5; real(8) big,d1,d14,d12,d13,d18,d3,
     ld5,d2;end subroutine tomilp_for_Crash;subroutine tomilp_CtPl(m,n1,nz,mc,big,i1,d1,i14,i19,i20,i12,i9,d14,d12,d13,d18,d3,d5,i5,
     ld2,ione);use IntTypes; integer(llen),value::ione;integer(4) m,n1,nz,mc,i1,i14,i19,i20,i12,i9,i5; real(8) big,d1,d14,d12,d13,d1
     l8,d3,d5,d2;end subroutine tomilp_CtPl;subroutine ssx2_1(m,n1,n,bchmax,mxnsub,jas,ialgtyp,idbt,d1,i1,dw,iw,i5,i6,i7,d17,d14,d3,
     li14,i19,i20,i9,i2,i3,
     +d10,d11,d18,d16,i4,i15,i16,i17,d6,d5,d7,i18,d2,tl,i13,pname,i8a,i8b,ione);use IntTypes; integer(llen),value:: i8a,i8b,ione; ch
     laracter(*) pname;integer(4) m,n1,n,bchmax,mxnsub,jas,ialgtyp,idbt,i1,iw(*),i5,i6,i7,i14,i19,i20,i9,i2,i3,i4,i15,i16,i17,i18,i1
     l3;real(8) d1,dw(*),d17,d14,d3,d10,d11,d18,d16,d6,d5,d7,d2,tl;end subroutine ssx2_1;end interface;integer(4) ipar,mx,n1x,memwor
     lk,mapi(*),mapdp(*),ialgtyp,idb0,idbt,intarr(*),intwork(*),ipos;real(8) lb(*),ub(*),dparr(*),dpwork(*),timelimit,sinfeas;charac
     lter pname*40;integer(4) bchmax, mxnsub;integer(4) i,j,jas,k,inttop,dptop
      include 'mifcomx.inc'
      intrinsic mod, min;logical lpalloc,lscale;integer(1) un; common/un/un;character(256) workpath;integer(4) inpk,inpp,log_pr,isto
     lp,iDB;common /contr/ inpk,inpp,log_pr,istop,iDB, workpath;real(8), pointer::prnsc(:),pcnsc(:),prng(:),paij(:);common/pForScale
     l/prnsc,pcnsc,prng,paij,lpalloc;pname='ssx11';bchmax = 200;istop=0;log_pr=-1;big = 1.0d30;mxnsub = 4;m = mx;n1 = n1x;mc = mx;n 
     l= m-1 + n1;mapdp(1) = 1;mapi(1) = 1;inpk=ipar;idb=idb0;inpp=0; if(lscale) inpp=1;lpalloc=.false.;nz=intarr(mapi(14)+n1)-intarr
     l(mapi(14));inttop = memwork+nz;dptop = memwork+nz;j = inttop - nz;k = dptop - nz;if (j.le.m.or.k.le.m) then;if(un>=0)write(un,
     l*) ' Remaining work area too small:',j,k;if(un>=0)write(un,*) ' Run terminated';istop=100; GOTO 79999;endif;j = mapdp(12) - 1;
      do 130 i=1,n1;dparr(i+j) = lb(i)
130   continue;j = mapdp(13) - 1;do 140 i=1,n1;dparr(i+j) = ub(i)
140   continue;select case(idb);case(0);call tomilp_1
     \(m ,n1, nz, mc, big,
     \intarr(mapi(1)),  dparr(mapdp(1)),
     \intarr(mapi(14)), intarr(mapi(19)), intarr(mapi(20)),
     \intarr(mapi(12)), intarr(mapi(9)), dparr(mapdp(14)),
     \dparr(mapdp(12)), dparr(mapdp(13)),
     \dparr(mapdp(18)), dparr(mapdp(3)),
     \dparr(mapdp(5)),  int(1,llen)
     \);case(1);select case(idbt);case(3);call tomilp_for_Crash
     \(m ,n1, nz, mc, big,
     \intarr(mapi(1)),  dparr(mapdp(1)),
     \intarr(mapi(14)), intarr(mapi(19)), intarr(mapi(20)),
     \intarr(mapi(12)), intarr(mapi(9)), dparr(mapdp(14)),
     \dparr(mapdp(12)), dparr(mapdp(13)),
     \dparr(mapdp(18)), dparr(mapdp(3)),
     \dparr(mapdp(5)),
     +intarr(mapi(5)),  dparr(mapdp(2)),
     \int(1,llen)     );case default;call tomilp_CtPl
     \(m ,n1, nz, mc, big,
     \intarr(mapi(1)),  dparr(mapdp(1)),
     \intarr(mapi(14)), intarr(mapi(19)), intarr(mapi(20)),
     \intarr(mapi(12)), intarr(mapi(9)), dparr(mapdp(14)),
     \dparr(mapdp(12)), dparr(mapdp(13)),
     \dparr(mapdp(18)), dparr(mapdp(3)),
     \dparr(mapdp(5)),
     +intarr(mapi(5)),  dparr(mapdp(2)),
     \int(1,llen)    );end select;case default;return;end select;if(istop/=0) goto 79999;m1 = m;objind = m1;m = m - 1;jas = min(intt
     lop,dptop);iposi=0;nffr=intarr(mapi(14)+n1);nzr=nz;call ssx2_1
     \(m, n1, n, bchmax, mxnsub, jas,       ialgtyp, idbt,
     \dparr(mapdp(1)),  intarr(mapi(1)),  dpwork, intwork,
     \intarr(mapi(5)),  intarr(mapi(6)),
     \intarr(mapi(7)),  dparr(mapdp(17)),
     \dparr(mapdp(14)), dparr(mapdp(3)),
     \intarr(mapi(14)), intarr(mapi(19)), intarr(mapi(20)),
     \intarr(mapi(9)),  intarr(mapi(2)),  intarr(mapi(3)),
     \dparr(mapdp(10)),dparr(mapdp(11)), dparr(mapdp(18)),
     \dparr(mapdp(16)), intarr(mapi(4)),
     \intarr(mapi(15)), intarr(mapi(16)), intarr(mapi(17)),
     \dparr(mapdp(6)),  dparr(mapdp(5)),
     \dparr(mapdp(7)),  intarr(mapi(18)),
     \dparr(mapdp(2)),timelimit,
     \intarr(mapi(13)),  pname,  int(8,llen),int(8,llen),int(1,llen) )
     \;m=m+1;ipos = iposi;if(ialgtyp==1)then;if(ipos==1.and.ninf>0) ipos=12;else;if(ipos==2) ipos=12;if(ipos==1.and.ninf>0) ipos=2;e
     lndif;sinfeas=-sinf
79999 if(istop>0) ipos=istop;return;
      end subroutine ssx11
