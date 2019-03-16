      LOGICAL(4) FUNCTION PEEKCHARQQ();PEEKCHARQQ=.false.;END FUNCTION PEEKCHARQQ;CHARACTER(LEN=1) FUNCTION GETCHARQQ();GETCHARQQ=''
     l;END FUNCTION GETCHARQQ;SUBROUTINE GETTIM(HOUR,MIN,SEC,HDTS);INTEGER(2), INTENT(OUT) :: HOUR,MIN,SEC,HDTS;integer val(8);call 
     ldate_and_time(VALUES=val);HOUR=int(val(5),2); MIN=int(val(6),2); SEC=int(val(7),2); HDTS=int(val(8)/10,2);END SUBROUTINE;SUBRO
     lUTINE GETDAT(IYEAR,IMONTH,IDAY);INTEGER(2), INTENT(OUT) :: IYEAR,IMONTH, IDAY;integer val(8);call date_and_time(VALUES=val);IY
     lEAR=int(val(1),2); IMONTH=int(val(2),2); IDAY=int(val(3),2);END SUBROUTINE;INTEGER(4) FUNCTION GETFILEINFOQQ(FILES, BUFFER,DWH
     lANDLE);use IntTypes;CHARACTER(LEN=*) FILES;TYPE(FILE$INFO) :: BUFFER;INTEGER(POINTER_LEN) DWHANDLE;integer val(13),STAT;GETFIL
     lEINFOQQ=0; DWHANDLE=0;if(STAT(trim(files),VAL)==0)then;BUFFER%creation=0;BUFFER%lastwrite=val(10);BUFFER%lastaccess=val(9);BUF
     lFER%length=val(8);BUFFER%permit=val(3);BUFFER%lastwrite=val(10);BUFFER%name='';GETFILEINFOQQ=len_trim(files);endif;END FUNCTIO
     lN GETFILEINFOQQ;SUBROUTINE SORTQQ(ADRARRAY, LENGTH, iSIZE);use CiFort;INTEGER(POINTER_LEN)::   ADRARRAY;INTEGER(SIZEOF_SIZE_T)
     l LENGTH;INTEGER(4)  iSIZE,i,j,n,iw1,iw2,list(LENGTH),invlist(LENGTH);pointer(p,intg); integer(4) intg(*);p=ADRARRAY; n=int(len
     lgth);    i=iSIZE;call sortVKint4(n,intg,list,invlist);list=0
10    do i=1,n; if(list(i)==0) Exit; enddo;if(i<=n)then; iw1=intg(i); j=i;do i=1,n; j=invlist(j); if(list(j)/=0) Exit;iw2=intg(j); i
     lntg(j)=iw1; iw1=iw2; list(j)=1;enddo;goto 10;endif;return;
      END SUBROUTINE
