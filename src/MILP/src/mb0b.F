      subroutine ssx2_1(mrow, ncol, nvar, bchmax, nsmax, memtopjs,  ialgtyp, idbt,aij0, inda0, aij, inda,kb1, kb2,kb3, at,rhs, rng,k
     qac, colend, colendx,mark, ketac, ketah,rname, cname, relk,pivots, pivrows,jc, jd, je,djrow, objrow,colw, inref,xs,  timelimit,
     qiflag, pname);use IntelInterf
      include 'mifcomx.inc'
      integer(4) mrow, ncol, nvar, bchmax, nsmax, memtopjs, ialgtyp, idbt;integer(4) inda(nffr:memtopjs), mark(nvar),               
     e                 inda0(1:nzr);integer kb1(mrow+1), kb2(mrow+1), kb3(mrow+1);integer kac(ncol+1), colend(ncol), colendx(ncol);i
     rnteger ketac(2*(mrow+1)+bchmax), ketah(2*(mrow+1)+bchmax);integer jc(mrow+5), jd(mrow+5), je(mrow+5), pivrows(mrow+1);integer 
     xiflag(0:mrow+ncol+1);integer(2) inref(nvar);double precision aij(nffr:memtopjs), at(nsmax*(mrow+1)), rhs(mrow+1),     aij0(1:n
     kzr);double precision pivots(mrow+1), djrow(nvar), objrow(ncol);double precision xs(nvar), rng(nvar), timelimit;real(4) colw(nv
     ear);character(*) rname(mrow+1), cname(ncol);character(1) relk(mrow+1);character pname*40;character workpath*256;integer(4) inp
     lk,inpp,log_pr,istop,iDB;common /contr/ inpk,inpp,log_pr,istop,iDB, workpath;real(8), pointer::prnsc(:),pcnsc(:),prng(:),paij(:
     r); logical lpalloc;common/pForScale/prnsc,pcnsc,prng,paij,lpalloc;character pn*40,dfil*40,ch,chs;character(8) objname,rhsname,
     krngname,bndname;common /names1/  pn,dfil,objname,rhsname,rngname,bndname;character ich*1;integer(4) jas,crdisk,memveg,    i,j;
      integer(4) iinda, iindael;parameter (iindael = 5000000);common /inver/  iinda(iindael);nullify(prnsc,pcnsc,prng,paij);memtop =
     p memtopjs;jas = memtop;memveg = memtop;jjas=iindael;chs='1';do 123 i=1,40; pn(i:i) = pname(i:i)
123   continue;m = mrow;n1 = ncol;n = nvar;nsub = nsmax;dfil='milp00.dmf';i=1; if(idb==0.and.(idbt==1.or.idbt==2)) i=0;call pari(i);
      nzr=kac(n1+1)-kac(1);select case(idb);case(0);do 110 i=1,m1; kb1(i)=n1+i
110   continue;do 120 j=1,n; xs(j)=0.0
120   continue;case(1);if(inpp<=0.and.idbt/=3) goto 225;if(idbt==3) then; do i=1,m1; kb1(i)=n1+i; enddo; endif;endselect;ch='2';if(i
     qnpp>0)then; ch='0'; lpalloc=.false.; i=0; j=0;if(associated(prnsc)) deallocate(prnsc,stat=j); i=i+j;if(associated(pcnsc)) deal
     ilocate(pcnsc,stat=j); i=i+j;if(associated(prng )) deallocate(prng, stat=j); i=i+j;if(associated(paij )) deallocate(paij, stat=
     tj); i=i+j;if(i==0)allocate(prnsc(m),pcnsc(n1),prng(n),paij(kac(n1+1)-1),stat=i);if(i==0) lpalloc=.true.;endif;GOTO 135
130   write(*,'(/,a)') 'Starting options (select one):';write(*,'(6(1x,a,/))')'0: Scale problem','1: Start form Lower Logical Basis'
     c,'2: Start from Upper Logical Basis','3: Start from Crash Basis','4: Dumped Basis';write(*,'(a)') 'Enter Your Choice: ';read (
     y*,'(a)') ch
135   CONTINUE;if(ch/='0') then;select case(idbt);case(1); ch='1';case(3); ch='3';case default; ch='2';end select;endif;if (ch.eq.'4
     c') then;call rdum (kb1,mark,xs,rng);elseif (ch.eq.'3') then;crdisk=0;if (2*(memveg-nzr).gt.nzr) then;if(idb/=1)then; call cras
     dh(aij0,inda0,aij,jc,jd,je,colw,kac,colend,colendx,kb1,mark,djrow(1),djrow((n+3)/2),crdisk);else; call crash_VK(aij0,inda0,aij,
     pjc,jd,je,colw,kac,colend,colendx,kb1,mark,djrow(1),djrow((n+3)/2),crdisk);endif;iduc=0;else;crdisk = 1;if(idb/=1)then; call cr
     nash(aij0,inda0,aij0,jc,jd,je,colw,kac,colend,colendx,kb1,mark,djrow(1),djrow((n+3)/2),crdisk);else; call crash_VK(aij0,inda0,a
     iij0,jc,jd,je,colw,kac,colend,colendx,kb1,mark,djrow(1),djrow((n+3)/2),crdisk);endif;endif;elseif (ch.eq.'1'.or.ch.eq.'2') then
       call lbas (mrow,kb1,kb2,mark,kac,rng,ketac,xs,rhs,ch,n);iduc=0;elseif (ch.eq.'0') then;call scal(aij0,inda0,rhs,at(1),at(m1+1
     r),at(m2+1),kac,colend,colendx,rng,mark,djrow);if(idb==1) then; call ChngMrkXs(n1,m1,rng,mark,xs);if(idbt/=3) goto 225;endif;ch
     o='2';goto 135;else;goto 130;endif;GOTO 225
220   write(*,'(/,1x,a)')'Algorithm (1:Primal, 2:Dual): ';read (*,'(a)') ich
225   continue;ich='1';if(ialgtyp/=1) ich='2';if (ich.eq.'1') then;call primal(mrow, ncol, nvar, bchmax, nsmax, memveg, pname,aij0, 
     minda0, aij, inda,kb1, kb2,kb3, at,rhs, rng,kac, colend, colendx,mark, ketac, ketah,rname, cname, relk,pivots, pivrows,jc, jd, 
     mje,djrow, objrow,colw, inref,xs, iinda, iindael,timelimit);elseif (ich.eq.'2') then;call dual(mrow, ncol, nvar, bchmax, nsmax,
     h memveg, pname,aij0, inda0, aij, inda,kb1, kb2,kb3, at,rhs, rng,kac, colend, colendx,mark, ketac, ketah,rname, cname, relk,piv
     hots, pivrows,jc, jd, je,djrow, objrow,colw, inref,xs, iinda, iindael, iflag,timelimit);else;goto 220;endif;if (iscale.gt.0)the
     en;if(lpalloc)then; deallocate(prnsc,pcnsc,prng,paij); lpalloc=.false.;else;endif;endif;RETURN;if (iscale.gt.0) then;open (33,f
     sile=trim(workpath)//'milprnsc.tmp',form='unformatted',status='unknown');close (33,status='delete');open (33,file=trim(workpath
     p)//'milpcnsc.tmp',form='unformatted',status='unknown');close (33,status='delete');open (33,file=trim(workpath)//'milprng.tmp',
     lform='unformatted',status='unknown');close (33,status='delete');open (33,file=trim(workpath)//'milpaij.tmp',form='unformatted'
     w,status='unknown');close (33,status='delete');endif;if(inpk>0)then;open (33,file=trim(workpath)//'milprhs.tmp',form='unformatt
     ted',status='unknown');close (33,status='delete');open (33,file=trim(workpath)//'milplb.tmp',form='unformatted',status='unknown
     r');close (33,status='delete');endif;return;
      end subroutine ssx2_1
