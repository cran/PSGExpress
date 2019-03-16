      subroutine IsFunctionRow(wstr,fnc_name,kfn,
     +w, chm, j,k,nf   );integer(4) kfn,k,i,j,nf,i1,iz,is,i2,i0; real(8) w;character(*) wstr,chm, fnc_name(0:kfn,0:*); character  ws
     ltr1*255;w=1d0; chm=''; j=-1; iz=-1; nf=-1;i0=verify(wstr,' '); if(i0<=0) goto 75;iz=scan(wstr(i0:),'(')+i0-1; if(iz<=1+i0-1)go
     lto 75;is=iz;iz=scan(wstr(is+1:),')'); if(iz<=0)goto 75;iz=scan(wstr(i0:is),'*')+i0-1; if(iz==1+i0-1)goto 75;i=iz; if(i<=i0-1)t
     lhen; i=len_trim(wstr); if(i<=1) goto 75; endif;call IsARealNumber(wstr(:i-1), j,i1,w);chm=wstr(:i-1);if(iz>i0-1)then; if(j<=0)
     l goto 75;if(wstr(i1:iz-1)/='') goto 75;i1=iz+1;endif;if(is<=i1) goto 75;wstr1=' '//trim(wstr(i1:is-1))//'_'; i2=is-i1+2;do j=k
     lfn,0,-1; do k=3,0,-1;i=index(wstr1(:i2),' '//trim(fnc_name(j,k)));if(i.ne.0
     +.and. index(wstr1(:i2),' '//trim(fnc_name(j,k))//'dev_').eq.0
     +.and. index(wstr1(:i2),' '//trim(fnc_name(j,k))//'pos_').eq.0
     +.and. index(wstr1(:i2),' '//trim(fnc_name(j,k))//'neg_').eq.0
     +.and. index(wstr1(:i2),' '//trim(fnc_name(j,k))//'g_').eq.0 ) then;if(wstr(i1:i1+i-2)=='')then; iz=k; if(k>1)iz=k-2; nf=j*10+i
     lz; chm=fnc_name(j,k)(:len_trim(fnc_name(j,k))-1); endif;goto 75;endif;enddo; enddo
75    k=iz;end subroutine IsFunctionRow;subroutine MaxRowBuff(buff,     krows,lrowm,kzp,km,kc);character(*) buff;  integer(4) krows,
     llrowm, kzp;character(10)  objn(6);integer(4)  i,i1,id,iaddlength,iaddmatr,j,ip,iaddlen,km,kc;integer(4),external:: icheckpsgna
     lme;kzp=0; km=0;i=1; i1=1; krows=-1; lrowm=1;do while(i1>0);i1=SCAN (buff(i:),char(10));if(i1<=0)then; i1=scan(buff(i:),char(0)
     l); if(i1<=0)i1=len_trim(buff(i:)); endif;ip=i; iaddlength=1; iaddlen=0;do while(iaddlength>0);call CheckFirstThreePoints(buff(
     lip:i+i1-1)//' ',iaddlength,iaddmatr);km=km+iaddmatr; iaddlen=iaddlen+iaddlength;ip=index(buff(ip:),',...,')+ip+4;enddo;lrowm=m
     lax0(lrowm,i1+iaddlen);i=i+i1; krows=krows+1;enddo;lrowm=lrowm+20;id=0; i1=1;do while(i1>0); i1=SCAN(buff(id+1:),','); id=id+i1
     l; kzp=kzp+1;enddo;objn(1)='matrix_'; objn(2)='smatrix_'; objn(3)='point_'; objn(4)='vector_'; objn(5)='pmatrix_'; objn(6)='var
     liable';do i=1,6; id=0; i1=1; j=len_trim(objn(i));do while(i1>0); i1=index(buff(id+1:),trim(objn(i))); id=id+i1+j-1; km=km+1;en
     lddo;enddo;kc=0; id=0; i1=1;do while(i1>0); i1=index(buff(id+1:),'constraint_'); id=id+i1+j-1; kc=kc+1;enddo;end subroutine Max
     lRowBuff;subroutine insertXname(name,jn,km,
     +n1,xname,xlb,xub,ixord,jused);integer(4) n1,jn,km,jused(-3:*),ixord(*); character(*) xname(-3:*),name; real(8) xlb(*),xub(*);i
     lnteger(4) j1,j2; real(8) w1,w2;n1=n1+1;goto 10;do j1=n1,jn+1,-1; xname(j1)=xname(j1-1); enddo; xname(jn)=name;if(km<3) RETURN;
       w1=xlb(n1); w2=xub(n1);do j1=n1,jn+1,-1; j2=j1-1; xlb(j1)=xlb(j2); xub(j1)=xub(j2); ixord(j1)=ixord(j2); enddo;xlb(jn)=w1; xu
     lb(jn)=w2; ixord(jn)=n1;if(km<4) RETURN;do j1=n1,jn+1,-1; jused(j1)=jused(j1-1); enddo
10    j1=n1;select case(km);case(:2);do j2=n1-1,jn,-1; xname(j1)=xname(j2); j1=j2; enddo;xname(jn)=name;case(3); w1=xlb(n1); w2=xub(
     ln1);do j2=n1-1,jn,-1; xname(j1)=xname(j2); xlb(j1)=xlb(j2); xub(j1)=xub(j2); ixord(j1)=ixord(j2); j1=j2; enddo;xname(jn)=name;
       xlb(jn)=w1; xub(jn)=w2; ixord(jn)=n1;case(4:); w1=xlb(n1); w2=xub(n1);do j2=n1-1,jn,-1; xname(j1)=xname(j2); xlb(j1)=xlb(j2);
       xub(j1)=xub(j2); ixord(j1)=ixord(j2); jused(j1)=jused(j2);j1=j2;enddo;xname(jn)=name; xlb(jn)=w1; xub(jn)=w2; ixord(jn)=n1; j
     lused(jn)=1;end select;return;
      end subroutine insertXname
