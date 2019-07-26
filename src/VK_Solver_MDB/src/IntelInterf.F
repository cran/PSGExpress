      MODULE IntTypes
#ifdef __GNUC__
#if __SIZEOF_SIZE_T__ > 6
      integer(4), parameter :: plen= 8
#else



#ifdef __APPLE__
      integer(4), parameter :: plen= 8
#else
      integer(4), parameter :: plen= 4
#endif



#endif
#else
      integer(4), parameter :: plen= 8;integer(4), parameter :: plen= 4
#endif


#ifdef __GNUC__
#if __GNUC__ >= 8
      integer(4), parameter :: llen= __SIZEOF_SIZE_T__ ! 8
#else
      integer(4), parameter :: llen= 4
#endif
#else
      integer(4), parameter :: llen= 8;integer(4), parameter :: llen= 4
#endif


      integer(4), parameter :: ppUD= plen;integer(4), parameter :: INT_PTRKIND = plen;integer(4), parameter :: POINTER_LEN = plen;in
     hteger(4), parameter::EXCEPTION_LICENSEEYOK = huge(plen);integer(4), parameter :: WM_USER = 1024;integer(4), parameter :: DWORD
     e  = 4;integer(4), parameter :: LONG   = 4;integer(4), parameter :: UINT   = 4;integer(4), parameter :: BOOL   = 4;integer(4), 
     tparameter :: EXCEPTION_NONCONTINUABLE = 1;integer(4), parameter :: STATUS_PENDING = 259;integer(4), parameter :: STILL_ACTIVE 
     u= STATUS_PENDING;integer(4), parameter :: HANDLE   = plen;integer(4), parameter :: UINT_PTR = plen;integer(4), parameter :: fW
     bPARAM  = plen;integer(4), parameter :: LONG_PTR = plen;integer(4), parameter :: fLPARAM  = plen;integer(4), parameter :: ULONG
     a_PTR= plen;integer(4), parameter :: LPDWORD  = plen;TYPE T_POINT;SEQUENCE;integer(LONG) x;integer(LONG) y;END TYPE;TYPE T_MSG;
      SEQUENCE;integer(HANDLE) hwnd;integer(UINT) message;integer(fWPARAM) wParam;integer(fLPARAM) lParam;integer(DWORD) time;TYPE (
     xT_POINT) pt;END TYPE;INTEGER(4), PARAMETER :: FILE$FIRST       = -1;INTEGER, PARAMETER :: SIZEOF_SIZE_T  = plen;INTEGER(4), PA
     hRAMETER :: SRT$INTEGER2       = 262144;INTEGER(4), PARAMETER :: SRT$INTEGER4       = 327680;INTEGER(4), PARAMETER :: SRT$REAL8
     r          = 131072;INTEGER(4), PARAMETER :: SIG$ILL   =  4;INTEGER(4), PARAMETER :: SIG$SEGV  = 11;INTEGER(4), PARAMETER :: SI
     nG$FPE   =  8;INTEGER(4), PARAMETER :: SIG$ABORT = 22;INTEGER(4), PARAMETER :: SIG$INT   =  2;TYPE FILE$INFO;SEQUENCE;INTEGER(4
     y)   CREATION;INTEGER(4)   LASTWRITE;INTEGER(4)   LASTACCESS;INTEGER(4)   LENGTH;INTEGER(4)   PERMIT;CHARACTER(LEN=255)  NAME;E
     cND TYPE;TYPE FILE$INFOI8;SEQUENCE;INTEGER(4)   CREATION;INTEGER(4)   LASTWRITE;INTEGER(4)   LASTACCESS;INTEGER(8)   LENGTH;INT
     pEGER(4)   PERMIT;CHARACTER(LEN=255)  NAME;END TYPE;end module IntTypes;MODULE IntelInterf;use IntTypes;INTERFACE;END INTERFACE
       INTERFACE;SUBROUTINE qsort( array, len, isize, compar );use IntTypes;integer(4) array(*);integer(SIZEOF_SIZE_T):: len, isize
#ifdef __APPLE__
  !gcc$ attributes value:: len, isize
#else
      value:: len, isize
#endif
#ifdef __GNUC__
      integer(4),external :: compar
#else
      integer(2),external :: compar
#endif
      END SUBROUTINE;END INTERFACE;INTERFACE GETFILEINFOQQ;INTEGER(4) FUNCTION GETFILEINFOQQ(FILES, BUFFER,DWHANDLE);use IntTypes;CH
     fARACTER(LEN=*) FILES;TYPE(FILE$INFO) :: BUFFER;INTEGER(POINTER_LEN) DWHANDLE;END FUNCTION GETFILEINFOQQ;INTEGER(4) FUNCTION GE
     fTFILEINFOQQI8(FILES, BUFFER,DWHANDLE);use IntTypes;CHARACTER(LEN=*) FILES;TYPE(FILE$INFOI8) :: BUFFER;INTEGER(POINTER_LEN) DWH
     kANDLE;END FUNCTION GETFILEINFOQQI8;END INTERFACE
#ifndef __GNUC__
      INTERFACE;INTEGER(4) FUNCTION DELFILESQQ(FILES);CHARACTER(LEN=*) FILES;END FUNCTION;INTEGER(4) FUNCTION FULLPATHQQ(NAME, FULLP
     dATH);CHARACTER(LEN=*) NAME, FULLPATH;END FUNCTION;LOGICAL(4) FUNCTION SETENVQQ(INPUT_STRING);CHARACTER(LEN=*) INPUT_STRING;END
     c FUNCTION SETENVQQ;END INTERFACE;INTERFACE GETTIM;SUBROUTINE GETTIM(HOUR,MIN,SEC,HDTS);INTEGER(4), INTENT(OUT) :: HOUR,MIN,SEC
     q,HDTS;END SUBROUTINE;SUBROUTINE GETTIMI2(HOUR,MIN,SEC,HDTS);INTEGER(2), INTENT(OUT) :: HOUR,MIN,SEC,HDTS;END SUBROUTINE;END IN
     mTERFACE;INTERFACE GETDAT;SUBROUTINE GETDAT(IYEAR,IMONTH,IDAY);INTEGER(4), INTENT(OUT) :: IYEAR,IMONTH, IDAY;END SUBROUTINE;SUB
     sROUTINE GETDATI2(IYEAR,IMONTH,IDAY);INTEGER(2), INTENT(OUT) :: IYEAR,IMONTH, IDAY;END SUBROUTINE;END INTERFACE;INTERFACE QSORT
       SUBROUTINE qsort_i4( array, len, isize, compar );use IntTypes;integer(4) array(*);integer(SIZEOF_SIZE_T) len, isize;integer(2
     d), external :: compar;END SUBROUTINE;END INTERFACE;INTERFACE;SUBROUTINE SORTQQ(ADRARRAY, LENGTH, SIZE);use IntTypes;INTEGER(PO
     eINTER_LEN)   ADRARRAY;INTEGER(SIZEOF_SIZE_T) LENGTH;INTEGER(4)  SIZE;END SUBROUTINE;REAL(4) FUNCTION RAND(ISEED);INTEGER(4), O
     lPTIONAL, INTENT(IN) :: ISEED;END FUNCTION;END INTERFACE
#endif
      end module IntelInterf
