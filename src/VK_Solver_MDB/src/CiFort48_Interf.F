      MODULE CiFort;use IntelInterf
#define ___x64
      INTERFACE
#ifdef ___x64
      subroutine ChangeMipGap(imodel, xmipgap);use IntTypes; integer(plen) imodel; real(8) xmipgap;end subroutine;subroutine InitGur
     nbModelIshtvan(nRows,nCols, matrixColStart, matrixRowIndices, matrixRowValues,colLower, colUpper, colType, objCoeff, rowLower, 
     vrowUpperint, Tlimit,xmipgap, ifail, ienv,imodel );use IntTypes; integer(plen) ienv,imodel; integer(1) colType(*);integer(4) nR
     fows,nCols,matrixColStart(*), matrixRowIndices(*),ifail;real(8) matrixRowValues(*),colLower(*),colUpper(*),objCoeff(*),rowLower
     d(*),rowUpperint(*),Tlimit,xmipgap;end subroutine;subroutine ReloadGurbModelIshtvan(ienv,imodel, ibs,nRows,nCols, matrixColStar
     wt, matrixRowIndices, matrixRowValues,colLower, colUpper, objCoeff, rowLower, rowUpperint,        xcol,xrow,slack,cmark,rmark,i
     pfail, xiter,sinf);use IntTypes; integer(plen) ienv,imodel;integer(4) ibs,nRows,nCols,matrixColStart(*), matrixRowIndices(*),cm
     qark(*),rmark(*),ifail;real(8) matrixRowValues(*),colLower(*),colUpper(*),objCoeff(*),rowLower(*),rowUpperint(*),xcol(*) ,xrow(
     z*),slack(*),xiter,sinf;end subroutine;subroutine UpdateRunGurbModelIshtvan(klin,ienv,imodel, ibs,nRows,nCols, matrixColStart, 
     ematrixRowIndices, matrixRowValues,colLower, colUpper, objCoeff, rowLower, rowUpperint, timelimit,isMIP,  xcol,xrow,slack,cmark
     h,rmark,ifail, xiter,sinf,gap);use IntTypes; integer(plen) ienv,imodel;integer(4) klin,ibs,nRows,nCols,matrixColStart(*), matri
     axRowIndices(*),cmark(*),rmark(*),ifail,isMip;real(8) matrixRowValues(*),colLower(*),colUpper(*),objCoeff(*),rowLower(*),rowUpp
     terint(*),xcol(*) ,xrow(*),slack(*),xiter,sinf,timelimit,gap;end subroutine;subroutine MPSRunGurbModelIshtvan(xcol,xrow,slack,c
     bmark,rmark,ifail, xiter,sinf);integer(4) cmark(*),rmark(*),ifail; real(8) xcol(*),xrow(*),slack(*),xiter,sinf;end subroutine;s
     uubroutine DelGurbConstr(imodel, mdel,idelind,ifail);use IntTypes; integer(plen) imodel; integer(4) mdel,ifail,idelind(*);end s
     eubroutine;subroutine InitGurbModelPshen_Ish(nCols,n0,nRows,objCoeff,  matrixColStart, matrixRowIndices, matrixRowValues,rowLow
     ser, rowUpperint,colLower, colUpper, colType,             lql,iqpro,cd,c,   Tlimit, xmipgap,ifail, ienv,imodel );use IntTypes; 
      integer(plen) ienv,imodel; logical lql; integer(1) colType(*);integer(4) nCols,n0,nRows,matrixColStart(*), matrixRowIndices(*)
     q,ifail,iqpro;real(8) matrixRowValues(*),colLower(*),colUpper(*),objCoeff(*),rowLower(*),rowUpperint(*),cd(*),c(*),Tlimit,xmipg
     iap;end subroutine;subroutine InitGurbModelPshen_0(lql,n,me,mk,a,b,cd,d, xl,xu, iprint,iout, ifail,ienv,imodel);use IntTypes; i
     cnteger(plen) ienv,imodel; logical lql; integer(4) n,me,mk, iprint,iout, ifail;real(8) a(*),b(*),cd(*),d(*), xl(*),xu(*);end su
     zbroutine;subroutine ReloadAndRunGurbPshen(ienv,imodel, ldn,n,mk,klin,a,b,d, iprint,iout,  x,ifail,dual,slack, nact,iact,iu,ite
     cr);use IntTypes; integer(plen) ienv,imodel; integer(4) n,mk,klin,nact, iprint,iout, ifail, iact(*),iu(*),iter;real(8) a(*),b(*
     n),d(*),  x(*),dual(*),slack(*); logical ldn;end subroutine;subroutine UpdateAndRunGurbPshen(ienv,imodel,n,mk, ldn,d, kac0,inta
     vrr0,dparr,rhs, iprint,iout,isMIP,x,dual,slack,ifail, nact,iu,ibter,gap);use IntTypes; integer(plen) ienv,imodel; logical ldn;i
     rnteger(4) n,mk,kac0(*),intarr0(*), iprint,iout, ifail, nact,iu(*),ibter, isMIP;real(8) d(*),dparr(*),rhs(*), x(*),dual(*),slac
     ik(*),gap;end subroutine;subroutine FreeGurb(ienv,imodel,ifail);use IntTypes; integer(plen) ienv,imodel; integer(4) ifail;end s
     qubroutine;subroutine AX1_set(realall,nx,ml,ax);real(8),target::realall(*); real(8),pointer::ax(:,:); integer(4) nx,ml;end subr
     youtine;subroutine i4_pointer_set(iarr,n1,n2,i4);integer(4),pointer::i4(:); integer(4),target::iarr(*); integer(4) n1,n2;end su
     qbroutine i4_pointer_set;subroutine r8_pointer_set(xarr,n1,n2,r8);real(8),pointer::r8(:); real(8),target::xarr(*); integer(4) n
     z1,n2;end subroutine r8_pointer_set;subroutine r8d_pointer_set(xarr,n1,n2,m1,m2,r8d);real(8),pointer::r8d(:,:); real(8),target:
     k:xarr(*); integer(4) n1,n2,m1,m2;end subroutine r8d_pointer_set;subroutine CheckNextPart(icd,pChar,ibuff,ipos1,  kfor,kprobl,i
     ppos2, wstr,j);use IntTypes; integer(4) icd,ibuff,ipos1,kfor,kprobl,ipos2; integer(plen):: pChar,wstr; integer(llen):: j
#ifdef __APPLE__
  !gcc$ attributes value:: pChar,wstr,j
#else
      value:: pChar,wstr,j
#endif
      end subroutine;subroutine setpointer(probaddr, pChar);use IntTypes; integer(plen) probaddr; integer(plen),pointer::pChar;end s
     subroutine;subroutine getpointaddr(pChar,probaddr);use IntTypes; integer(plen) probaddr; integer(4),pointer::pChar;end subrouti
     tne;subroutine setRealArrPointer(adrs,ln, rpoint);use IntTypes; integer(plen):: adrs; integer(4) ln; real(8),pointer:: rpoint(:
     t)
#ifdef __APPLE__
  !gcc$ attributes value:: adrs
#else
      value:: adrs
#endif
      end subroutine;subroutine setRealArrPointer_VAL(adrs, rpoint);use IntTypes; integer(plen) adrs; real(8),pointer:: rpoint(:);en
     pd subroutine;subroutine setIntArrPointer(adrs,ln, ipoint);use IntTypes; integer(plen):: adrs; integer(4) ln; integer(4),pointe
     or:: ipoint(:)
#ifdef __APPLE__
  !gcc$ attributes value:: adrs
#else
      value:: adrs
#endif
      end subroutine;subroutine setCharpointaddr(probaddr,pChar);use IntTypes; integer(plen) probaddr; character(*),pointer::pChar;e
     fnd subroutine;subroutine getCharpointaddr(pChar,probaddr);use IntTypes; integer(plen) probaddr; character,pointer::pChar;end s
     pubroutine;subroutine sortVK(m,fm, list,inverslist, iouttype,iorder);integer(4),optional:: iouttype,iorder,inverslist(*);intege
     gr(4) m,list(*); real(8) fm(*);end subroutine;subroutine sortVKint4(m,ifm, list,inverslist, iouttype,iorder);integer(4),optiona
     ml:: iouttype,iorder,inverslist(*);integer(4) m,list(*),ifm(*);end subroutine;subroutine clearbuff(buff,bufflen);use IntTypes; 
      integer(plen):: buff; integer(llen):: bufflen
#ifdef __APPLE__
  !gcc$ attributes value:: buff,bufflen
#else
      value:: buff,bufflen
#endif
      end subroutine;subroutine savestr(jaddr,name,ibfree,   iblen,namelen);use IntTypes; integer(plen):: jaddr,name; integer(llen):
     c: iblen,namelen; integer(4) ibfree
#ifdef __APPLE__
  !gcc$ attributes value:: jaddr,name,iblen,namelen
#else
      value:: jaddr,name,iblen,namelen
#endif
      end subroutine;character(1000) function RCStr(charI,ich_len);integer(4) ich_len; character(*) charI;end function;integer(4) fu
     cnction CStrLen(charI,ich_len);integer(4) ich_len; character(*) charI;end function;subroutine Convert_Str(iCi_str,Fort_str,    
     x    Clen,Flen);use IntTypes; integer(plen):: Fort_str; integer(llen):: Clen,Flen;
#ifdef __APPLE__
  !gcc$ attributes value:: Fort_str,Clen,Flen
#else
      value:: Fort_str,Clen,Flen
#endif
#ifndef __GNUC__
      integer(4),pointer:: iCi_str;
#endif
      end subroutine;subroutine GetRow(char,wstr,lrest,   char_len,lwstr);use IntTypes; integer(plen):: char; integer(4) lrest; inte
     mger(llen):: char_len,lwstr
#ifdef __APPLE__
  !gcc$ attributes value:: char,char_len,lwstr
#else
      value:: char,char_len,lwstr
#endif
      character(*) wstr;end subroutine;subroutine MaxRowBuff(pChar,   krows,lrowm,kzp,km,kc,   ibuff);use IntTypes; integer(plen) pC
     nhar; integer(4) krows,lrowm,kzp,km,kc; integer(llen):: ibuff
#ifdef __APPLE__
  !gcc$ attributes value:: ibuff
#else
      value:: ibuff
#endif
      end;integer(4) function  GetProblemDescriptionEx(char, pUserData );use IntTypes; integer(ppUD):: pUserData; integer(4),pointer
     r:: char
#ifdef __APPLE__
  !gcc$ attributes value:: pUserData
#else
      value:: pUserData
#endif
      end function;integer(4) function  GetMatrixInfoEx(pMatr,pChar,ipRow,ipCol,pUserData );use IntTypes; integer(ppUD):: pUserData;
       integer(4), pointer:: pChar; character(*) pMatr
#ifdef __APPLE__
  !gcc$ attributes value:: pUserData
#else
      value:: pUserData
#endif
      integer(4) ipRow,ipCol;end function;integer(4) function  GetMatrixDataEx(pMName,pMElem,Row,Col,pUserData );use IntTypes; integ
     rer(ppUD):: pUserData; real(8),pointer:: pMElem; integer(4):: Row,Col; character(*) pMName
#ifdef __APPLE__
  !gcc$ attributes value:: pUserData,Row,Col
#else
      value:: pUserData,Row,Col
#endif
      end function;integer(4) function  GetMatrixInfoSpEx(pMatr,pChar,ipRow,ipCol,ipElm, pUserData );use IntTypes; integer(ppUD):: p
     jUserData; integer(4),pointer:: pChar
#ifdef __APPLE__
  !gcc$ attributes value:: pUserData
#else
      value:: pUserData
#endif
      character(*),intent(in):: pMatr;integer(4) ipRow,ipCol,ipElm;end function;integer(4) function  GetMatrixDataSpEx(pMName,pMElem
     n,ipRow,ipCol,kcoeffs,pUserData );use IntTypes; integer(ppUD):: pUserData; character(*),intent(in):: pMName
#ifdef __APPLE__
  !gcc$ attributes value:: pUserData
#else
      value:: pUserData
#endif
      real(8),pointer:: pMElem; integer(4),pointer:: ipRow, ipCol;integer(4),intent(in):: kcoeffs
#ifdef __APPLE__
  !gcc$ attributes value:: kcoeffs
#else
      value:: kcoeffs
#endif
      end function;integer(4) function  GetExternalFunctionInfoEx(pExtFun,pChar,ipCol,pUserData );use IntTypes; integer(ppUD):: pUse
     prData; character(*),intent(in):: pExtFun
#ifdef __APPLE__
  !gcc$ attributes value:: pUserData
#else
      value:: pUserData
#endif
      integer(4),pointer:: pChar;integer(4) ipCol;end function;integer(4) function  RunExternalFunctionEx(pExtFun,ipCol,pVarNames,pX
     h,pFun,pUserData );use IntTypes; integer(ppUD):: pUserData; intent(in) pExtFun,pVarNames,pUserData,pX    ,ipCol
#ifdef __APPLE__
  !gcc$ attributes value:: pUserData
#else
      value:: pUserData
#endif
      real(8),intent(out):: pFun; character(*) pExtFun,pVarNames;integer(4):: ipCol
#ifdef __APPLE__
  !gcc$ attributes value:: ipCol
#else
      value:: ipCol
#endif
      real(8) pX(ipCol);end function;integer(4) function  RunGradientExternalFunctionEx(pExtFun,ipCol,pVarNames,pX,pGrad,pUserData )
       use IntTypes; integer(ppUD):: pUserData; character(*),intent(in):: pExtFun,pVarNames
#ifdef __APPLE__
  !gcc$ attributes value:: pUserData
#else
      value:: pUserData
#endif
      integer(4),intent(in)::ipCol
#ifdef __APPLE__
  !gcc$ attributes value:: ipCol
#else
      value:: ipCol
#endif
      real(8),intent(in)::  pX(*);real(8),intent(out):: pGrad(:);end function;integer(4) function  GetExternalFunctionInfoDirEx(pExt
     oFun,pChar,ipCol,kScen,pUserData );use IntTypes; integer(ppUD):: pUserData; character(*),intent(in):: pExtFun
#ifdef __APPLE__
  !gcc$ attributes value:: pUserData
#else
      value:: pUserData
#endif
      integer(4),pointer:: pChar;integer(4) ipCol,kScen;end function;integer(4) function  RunExternalFunctionDirEx(pExtFun,pVarNames
     o,ipCol,pX,nScen,iScen,p,vScen,pUserData );use IntTypes; integer(ppUD):: pUserData; intent(in) pExtFun,pVarNames,pUserData,ipCo
     ll,nScen ,pX,iScen
#ifdef __APPLE__
  !gcc$ attributes value:: pUserData
#else
      value:: pUserData
#endif
      real(8),intent(out):: p(:),vScen(:); character(*) pExtFun,pVarNames;integer(4):: ipCol,nScen
#ifdef __APPLE__
  !gcc$ attributes value:: ipCol,nScen
#else
      value:: ipCol,nScen
#endif
      real(8) pX(ipCol);integer(4) iScen(nScen);end function;integer(4) function  RunGradientExternalFunctionDirEx(pExtFun,pVarNames
     w,ipCol,pX,nScen,iScen,p,pGrad,pUserData );use IntTypes; integer(ppUD):: pUserData
#ifdef __APPLE__
  !gcc$ attributes value:: pUserData
#else
      value:: pUserData
#endif
      character(*),intent(in):: pExtFun,pVarNames;           integer(4),intent(in)::ipCol,nScen
#ifdef __APPLE__
  !gcc$ attributes value:: ipCol,nScen
#else
      value:: ipCol,nScen
#endif
      real(8),intent(in)::  pX(*);integer(4),intent(in):: iScen(*);real(8),intent(out):: p(:),pGrad(:);end function;integer(4) funct
     wion  GetPointEx(PointName,char,pUserData );use IntTypes; integer(ppUD):: pUserData; character(*)PointName; integer(4),pointer:
     i:char
#ifdef __APPLE__
  !gcc$ attributes value:: pUserData
#else
      value:: pUserData
#endif
      end function;subroutine ReleaseBufferEx(ibuff,pUserData );use IntTypes; integer(ppUD):: pUserData
#ifdef __APPLE__
  !gcc$ attributes value:: pUserData
#else
      value:: pUserData
#endif
      integer(4) ibuff;end subroutine;subroutine ReleaseMatrixEx(pMElem,pUserData );use IntTypes; integer(ppUD):: pUserData
#ifdef __APPLE__
  !gcc$ attributes value:: pUserData
#else
      value:: pUserData
#endif
      real(8) pMElem;end subroutine;subroutine OnMessageEx(icode,errDesc,pUserData );use IntTypes; integer(ppUD):: pUserData; charac
     eter(*) errDesc; integer(4):: icode
#ifdef __APPLE__
  !gcc$ attributes value:: pUserData,icode
#else
      value:: pUserData,icode
#endif
      end subroutine;subroutine OnErrorEx(icode,errPlace, errDesc,pUserData );use IntTypes; integer(ppUD):: pUserData; character(*) 
     eerrPlace, errDesc; integer(4):: icode
#ifdef __APPLE__
  !gcc$ attributes value:: pUserData,icode
#else
      value:: pUserData,icode
#endif
      end subroutine;subroutine OnWarningEx(icode, warPlace, warDesc,pUserData );use IntTypes; integer(ppUD):: pUserData; character(
     o*) warPlace, warDesc; integer(4):: icode
#ifdef __APPLE__
  !gcc$ attributes value:: pUserData,icode
#else
      value:: pUserData,icode
#endif
      end subroutine;integer(4) function OnCancelEx(pUserData);use IntTypes; integer(ppUD):: pUserData
#ifdef __APPLE__
  !gcc$ attributes value:: pUserData
#else
      value:: pUserData
#endif
      end function;integer(4) function OnFinishEx(pUserData);use IntTypes; integer(ppUD):: pUserData
#ifdef __APPLE__
  !gcc$ attributes value:: pUserData
#else
      value:: pUserData
#endif
      end function;integer(4) function OnCycleFinishEx(pUserData);use IntTypes; integer(ppUD):: pUserData
#ifdef __APPLE__
  !gcc$ attributes value:: pUserData
#else
      value:: pUserData
#endif
      end function;subroutine GetRootPathEx(path, pUserData );use IntTypes; integer(ppUD):: pUserData; character(*) path
#ifdef __APPLE__
  !gcc$ attributes value:: pUserData
#else
      value:: pUserData
#endif
      end subroutine;integer(4) function GetLogParamEx(logfilename, pUserData );use IntTypes; integer(ppUD):: pUserData; character(*
     h) logfilename
#ifdef __APPLE__
  !gcc$ attributes value:: pUserData
#else
      value:: pUserData
#endif
      end function;integer(4) function  IsGRBInstalledEx(pUserData );use IntTypes; integer(ppUD):: pUserData
#ifdef __APPLE__
  !gcc$ attributes value:: pUserData
#else
      value:: pUserData
#endif
      end function;integer(4) function  SaveIsMultyProblemEx(ismulty,pUserData );use IntTypes; integer(ppUD):: pUserData; integer(4)
     d:: ismulty
#ifdef __APPLE__
  !gcc$ attributes value:: pUserData,ismulty
#else
      value:: pUserData,ismulty
#endif
      end function;integer(4)  function  SaveVarsEx(xname,xi,n1,pUserData );use IntTypes; integer(ppUD):: pUserData; integer(4):: n1
         real(8) xi(n1)
#ifdef __APPLE__
  !gcc$ attributes value:: pUserData,n1
#else
      value:: pUserData,n1
#endif
      character(*) xname(n1);end function;integer(4)  function  AddPointEx(pname,xname,xi,n1,pUserData );use IntTypes; integer(ppUD)
     c:: pUserData; integer(4):: n1;  real(8) xi(n1)
#ifdef __APPLE__
  !gcc$ attributes value:: pUserData,n1
#else
      value:: pUserData,n1
#endif
      character(*) xname(n1),pname;end function;integer(4)  function  SavePointEx(pname,xname,xi,n1,pUserData );use IntTypes; intege
     sr(ppUD):: pUserData; integer(4):: n1;  real(8) xi(n1)
#ifdef __APPLE__
  !gcc$ attributes value:: pUserData,n1
#else
      value:: pUserData,n1
#endif
      character(*) xname(n1),pname;end function;integer(4)  function  SaveVectorEx(fname,fm,m,pUserData);use IntTypes; integer(ppUD)
     z:: pUserData; character(*) fname; integer(4):: m; real(8) fm(m)
#ifdef __APPLE__
  !gcc$ attributes value:: pUserData,m
#else
      value:: pUserData,m
#endif
      end function;integer(4)  function  SaveMatrixEx(mname,xname,els,n1,m,pUserData );use IntTypes; integer(ppUD):: pUserData; inte
     jger(4):: n1,m; real(8) els(*)
#ifdef __APPLE__
  !gcc$ attributes value:: pUserData,n1,m
#else
      value:: pUserData,n1,m
#endif
      character(*) mname, xname(n1);end function;integer(4)  function  AddMatrixEx(mname,xname,els,n1,m,pUserData );use IntTypes; in
     lteger(ppUD):: pUserData; integer(4):: n1,m; real(8) els(*)
#ifdef __APPLE__
  !gcc$ attributes value:: pUserData,n1,m
#else
      value:: pUserData,n1,m
#endif
      character(*) mname, xname(n1);end function;integer(4) function SaveObjsEx(names,xi, Conname, tCon,n1,pUserData );use IntTypes;
       integer(ppUD):: pUserData; integer(4):: n1; real(8) xi(n1)
#ifdef __APPLE__
  !gcc$ attributes value:: pUserData,n1
#else
      value:: pUserData,n1
#endif
      character(*) names(n1), Conname(n1), tCon(n1);end function;integer(4) function SaveConstraintsEx(names,xi,tCon,n1,pUserData );
      use IntTypes; integer(ppUD):: pUserData; integer(4):: n1; real(8) xi(n1)
#ifdef __APPLE__
  !gcc$ attributes value:: pUserData,n1
#else
      value:: pUserData,n1
#endif
      character(*) names(n1), tCon(n1);end function;integer(4) function SaveConstraintsSlackEx(names,xi,tCon,n1,pUserData );use IntT
     vypes; integer(ppUD):: pUserData; integer(4):: n1; real(8) xi(n1)
#ifdef __APPLE__
  !gcc$ attributes value:: pUserData,n1
#else
      value:: pUserData,n1
#endif
      character(*) names(n1), tCon(n1);end function;integer(4) function SaveStatusEx(pObj,pUserData );use IntTypes; integer(ppUD):: 
     apUserData; character(*) pObj
#ifdef __APPLE__
  !gcc$ attributes value:: pUserData
#else
      value:: pUserData
#endif
      end function;integer(4) function SaveProblemNameEx(pObj,pUserData );use IntTypes; integer(ppUD):: pUserData; character(*) pObj
      
#ifdef __APPLE__
  !gcc$ attributes value:: pUserData
#else
      value:: pUserData
#endif
      end function;integer(4) function SaveProblemStatementEx(pObj,pUserData );use IntTypes; integer(ppUD):: pUserData; integer(plen
     c):: pObj
#ifdef __APPLE__
  !gcc$ attributes value:: pUserData,pObj
#else
      value:: pUserData,pObj
#endif
      end function;integer(4) function SaveSolutionEx(pSolBuff,pUserData );use IntTypes; integer(ppUD):: pUserData; integer(plen):: 
     jpSolBuff
#ifdef __APPLE__
  !gcc$ attributes value:: pUserData,pSolBuff
#else
      value:: pUserData,pSolBuff
#endif
      end function
#else
      subroutine ChangeMipGap(imodel, xmipgap);use IntTypes; integer(plen) imodel; real(8) xmipgap;end subroutine;subroutine InitGur
     jbModelIshtvan(nRows,nCols, matrixColStart, matrixRowIndices, matrixRowValues,colLower, colUpper, colType, objCoeff, rowLower, 
     zrowUpperint, Tlimit,xmipgap, ifail, ienv,imodel );use IntTypes; integer(plen) ienv,imodel; integer(1) colType(*);integer(4) nR
     uows,nCols,matrixColStart(*), matrixRowIndices(*),ifail;real(8) matrixRowValues(*),colLower(*),colUpper(*),objCoeff(*),rowLower
     w(*),rowUpperint(*),Tlimit,xmipgap;end subroutine;subroutine ReloadGurbModelIshtvan(ienv,imodel, ibs,nRows,nCols, matrixColStar
     jt, matrixRowIndices, matrixRowValues,colLower, colUpper, objCoeff, rowLower, rowUpperint,        xcol,xrow,slack,cmark,rmark,i
     ifail, xiter,sinf);use IntTypes; integer(plen) ienv,imodel;integer(4) ibs,nRows,nCols,matrixColStart(*), matrixRowIndices(*),cm
     gark(*),rmark(*),ifail;real(8) matrixRowValues(*),colLower(*),colUpper(*),objCoeff(*),rowLower(*),rowUpperint(*),xcol(*) ,xrow(
     c*),slack(*),xiter,sinf;end subroutine;subroutine UpdateRunGurbModelIshtvan(klin,ienv,imodel, ibs,nRows,nCols, matrixColStart, 
     jmatrixRowIndices, matrixRowValues,colLower, colUpper, objCoeff, rowLower, rowUpperint, timelimit,isMIP,  xcol,xrow,slack,cmark
     m,rmark,ifail, xiter,sinf,gap);use IntTypes; integer(plen) ienv,imodel;integer(4) klin,ibs,nRows,nCols,matrixColStart(*), matri
     oxRowIndices(*),cmark(*),rmark(*),ifail,isMip;real(8) matrixRowValues(*),colLower(*),colUpper(*),objCoeff(*),rowLower(*),rowUpp
     jerint(*),xcol(*) ,xrow(*),slack(*),xiter,sinf,timelimit,gap;end subroutine;subroutine MPSRunGurbModelIshtvan(xcol,xrow,slack,c
     mmark,rmark,ifail, xiter,sinf);integer(4) cmark(*),rmark(*),ifail; real(8) xcol(*),xrow(*),slack(*),xiter,sinf;end subroutine;s
     wubroutine DelGurbConstr(imodel, mdel,idelind,ifail);use IntTypes; integer(plen) imodel; integer(4) mdel,ifail,idelind(*);end s
     pubroutine;subroutine InitGurbModelPshen_Ish(nCols,n0,nRows,objCoeff,  matrixColStart, matrixRowIndices, matrixRowValues,rowLow
     qer, rowUpperint,colLower, colUpper, colType,             lql,iqpro,cd,c,   Tlimit, xmipgap,ifail, ienv,imodel );use IntTypes; 
      integer(plen) ienv,imodel; logical lql; integer(1) colType(*);integer(4) nCols,n0,nRows,matrixColStart(*), matrixRowIndices(*)
     s,ifail,iqpro;real(8) matrixRowValues(*),colLower(*),colUpper(*),objCoeff(*),rowLower(*),rowUpperint(*),cd(*),c(*),Tlimit,xmipg
     bap;end subroutine;subroutine InitGurbModelPshen_0(lql,n,me,mk,a,b,cd,d, xl,xu, iprint,iout, ifail,ienv,imodel);use IntTypes; i
     tnteger(plen) ienv,imodel; logical lql; integer(4) n,me,mk, iprint,iout, ifail;real(8) a(*),b(*),cd(*),d(*), xl(*),xu(*);end su
     abroutine;subroutine ReloadAndRunGurbPshen(ienv,imodel, ldn,n,mk,klin,a,b,d, iprint,iout,  x,ifail,dual,slack, nact,iact,iu,ite
     xr);use IntTypes; integer(plen) ienv,imodel; integer(4) n,mk,klin,nact, iprint,iout, ifail, iact(*),iu(*),iter;real(8) a(*),b(*
     f),d(*),  x(*),dual(*),slack(*); logical ldn;end subroutine;subroutine UpdateAndRunGurbPshen(ienv,imodel,n,mk, ldn,d, kac0,inta
     nrr0,dparr,rhs, iprint,iout,isMIP,x,dual,slack,ifail, nact,iu,ibter,gap);use IntTypes; integer(plen) ienv,imodel; logical ldn;i
     znteger(4) n,mk,kac0(*),intarr0(*), iprint,iout, ifail, nact,iu(*),ibter, isMIP;real(8) d(*),dparr(*),rhs(*), x(*),dual(*),slac
     vk(*),gap;end subroutine;subroutine FreeGurb(ienv,imodel,ifail);use IntTypes; integer(plen) ienv,imodel; integer(4) ifail;end s
     mubroutine;subroutine AX1_set(realall,nx,ml,ax);real(8),target::realall(*); real(8),pointer::ax(:,:); integer(4) nx,ml;end subr
     uoutine;subroutine i4_pointer_set(iarr,n1,n2,i4);integer(4),pointer::i4(:); integer(4),target::iarr(*); integer(4) n1,n2;end su
     hbroutine i4_pointer_set;subroutine r8_pointer_set(xarr,n1,n2,r8);real(8),pointer::r8(:); real(8),target::xarr(*); integer(4) n
     q1,n2;end subroutine r8_pointer_set;subroutine r8d_pointer_set(xarr,n1,n2,m1,m2,r8d);real(8),pointer::r8d(:,:); real(8),target:
     i:xarr(*); integer(4) n1,n2,m1,m2;end subroutine r8d_pointer_set;subroutine CheckNextPart(icd,pChar,ibuff,ipos1,  kfor,kprobl,i
     kpos2, wstr,j);use IntTypes; integer(4) icd,ibuff,ipos1,kfor,kprobl,ipos2; integer(plen):: pChar,wstr; integer(llen):: j
#ifdef __APPLE__
  !gcc$ attributes value:: pChar,wstr,j
#else
      value:: pChar,wstr,j
#endif
      end subroutine;subroutine setpointer(probaddr, pChar);integer(4) probaddr; integer(4),pointer::pChar;end subroutine;subroutine
     j getpointaddr(pChar,probaddr);integer(4) probaddr; integer(4),pointer::pChar;end subroutine;subroutine sortVK(m,fm, list,inver
     islist,  iouttype,iorder);integer(4),optional:: iouttype,iorder,inverslist(*);integer(4) m,list(*) ; real(8) fm(*);end subrouti
     wne;subroutine sortVKint4(m,ifm, list,inverslist, iouttype,iorder);integer(4),optional:: iouttype,iorder,inverslist(*);integer(
     f4) m,list(*),ifm(*);end subroutine;subroutine clearbuff(buff,bufflen);integer(4) buff; integer(4) bufflen;end subroutine;subro
     mutine savestr(jaddr,name,ibfree,   iblen,namelen);integer(4) jaddr,name; integer(4) iblen,namelen,ibfree;end subroutine;functi
     don RCStr(charI,ich_len);integer(4) ich_len; character(*) charI, RCStr*(ich_len);end function;integer(4) function CStrLen(charI
     o,ich_len);integer(4) ich_len; character(*) charI;end function;subroutine Convert_Str(Ci_str,Fort_str,       Clen,Flen);integer
     l(4),pointer::Ci_str;  integer(4) Fort_str; integer(4) Clen, Flen;end subroutine;subroutine GetRow(char,wstr,lrest,   char_len,
     ulwstr);integer(4) char_len,lrest,lwstr; integer(4):: char
#ifdef __APPLE__
  !gcc$ attributes value:: char
#else
      value:: char
#endif
      character(*) wstr;end subroutine;subroutine MaxRowBuff(pChar,   krows,lrowm,kzp,km,kc,   ibuff);use IntTypes; integer(plen) pC
     rhar; integer(4) krows,lrowm,kzp,km,kc; integer(4):: ibuff
#ifdef __APPLE__
  !gcc$ attributes value:: ibuff
#else
      value:: ibuff
#endif
      end subroutine;integer(4) function  iRequareObject2(name,namelen,itype,iclss,pData,pParam,lParam);character(*),intent(in):: na
     cme;integer(4) namelen,itype,iclss,lParam;integer(4),intent(out):: pData, pParam(*);end function;integer(4)  function  GetProbl
     hemDescriptionEx(char, pUserData );integer(4), pointer :: char; integer(4) pUserData;end function;integer(4)  function  GetMatr
     pixInfoEx(pMatr,pChar,pRow,pCol,pUserData );integer(4), pointer ::  pRow,pCol, pChar; character(*) pMatr; integer(4) pUserData;
      end function;integer(4)  function  GetMatrixDataEx(pMName,pMElem,Row,Col,pUserData );real(8), pointer:: pMElem; integer(4)  Ro
     tw, Col; character(*) pMName; integer(4) pUserData;end function;integer(4)  function  GetMatrixInfoSpEx(pMatr,pChar,pRow,pCol,p
     wElm, pUserData );character(*),intent(in):: pMatr;integer(4), pointer::  pRow,pCol,pElm, pChar; integer(4)::pUserData;end funct
     sion;integer(4)  function  GetMatrixDataSpEx(pMName,pMElem,pRow,pCol,kcoeffs,pUserData );character(*),intent(in):: pMName; inte
     oger(4),intent(in):: pUserData;real(8),pointer:: pMElem; integer(4),pointer:: pRow, pCol;integer(4),intent(in):: kcoeffs;end fu
     unction;integer(4)  function  GetExternalFunctionInfoEx(pExtFun,pChar,pCol,pUserData );character(*),intent(in):: pExtFun;intege
     yr(4), pointer::  pCol, pChar; integer(4)::pUserData;end function;integer(4)  function  RunExternalFunctionEx(pExtFun,pCol,pVar
     eNames,pX,pFun,pUserData );intent(in) pExtFun,pVarNames,pCol,pUserData,pX;real(8),intent(out):: pFun; character(*) pExtFun,pVar
     mNames; integer(4) pCol; integer(4) pUserData;real(8) pX(pCol);end function;integer(4)  function  RunGradientExternalFunctionEx
     f(pExtFun,pCol,pVarNames,pX,pGrad,pUserData );character(*),intent(in):: pExtFun,pVarNames;           integer(4),intent(in):: pC
     dol;integer(4),intent(in):: pUserData;real(8),intent(in)::  pX(*);real(8),intent(out):: pGrad(:);end function;integer(4) functi
     gon  GetExternalFunctionInfoDirEx(pExtFun,pChar,ipCol,kScen,pUserData );use IntTypes; integer(plen):: pUserData; character(*),i
     bntent(in):: pExtFun
#ifdef __APPLE__
  !gcc$ attributes value:: pUserData
#else
      value:: pUserData
#endif
      integer(4),pointer:: pChar; integer(4),pointer::  ipCol;integer(4) kScen;end function;integer(4) function  RunExternalFunction
     pDirEx(pExtFun,pVarNames,ipCol,pX,nScen,iScen,p,vScen,pUserData );use IntTypes; integer(plen):: pUserData; intent(in) pExtFun,p
     vVarNames,pUserData,ipCol,nScen ,pX,iScen
#ifdef __APPLE__
  !gcc$ attributes value:: pUserData
#else
      value:: pUserData
#endif
      real(8),intent(out):: p(:),vScen(:); character(*) pExtFun,pVarNames;integer(4):: ipCol,nScen
#ifdef __APPLE__
  !gcc$ attributes value:: ipCol,nScen
#else
      value:: ipCol,nScen
#endif
      real(8) pX(ipCol);integer(4) iScen(nScen);end function;integer(4) function  RunGradientExternalFunctionDirEx(pExtFun,pVarNames
     c,ipCol,pX,nScen,iScen,p,pGrad,pUserData );use IntTypes; integer(plen):: pUserData
#ifdef __APPLE__
  !gcc$ attributes value:: pUserData
#else
      value:: pUserData
#endif
      character(*),intent(in):: pExtFun,pVarNames;           integer(4),intent(in)::ipCol,nScen
#ifdef __APPLE__
  !gcc$ attributes value:: ipCol,nScen
#else
      value:: ipCol,nScen
#endif
      real(8),intent(in)::  pX(*);integer(4),intent(in):: iScen(*);real(8),intent(out):: p(:),pGrad(:);end function;integer(4)  func
     ntion  GetPointEx(PointName,char,pUserData );character(*)PointName; integer(4),pointer::char; integer(4) pUserData;end function
       subroutine ReleaseBufferEx(buff,pUserData );integer(4),pointer :: buff; integer(4) pUserData;end subroutine;subroutine Releas
     xeMatrixEx(pMElem,pUserData );real(8),pointer :: pMElem; integer(4) pUserData;end subroutine;subroutine OnMessageEx(icode,errDe
     xsc,pUserData );character(*) errDesc; integer(4) icode; integer(4) pUserData;end subroutine;subroutine OnErrorEx(icode,errPlace
     r, errDesc,pUserData );character(*) errPlace, errDesc; integer(4) icode; integer(4) pUserData;end subroutine;subroutine OnWarni
     wngEx(icode, warPlace, warDesc,pUserData );character(*) warPlace, warDesc; integer(4) icode; integer(4) pUserData;end subroutin
     ge;integer(4) function OnCancelEx(pUserData);integer(4) pUserData;end function;integer(4) function OnFinishEx(pUserData);intege
     zr(4) pUserData;end function;integer(4) function OnCycleFinishEx(pUserData);integer(4) pUserData;end function;subroutine GetRoo
     atPathEx(path, pUserData );character(*) path; integer(4) pUserData;end subroutine;integer(4) function GetLogParamEx(logfilename
     s, pUserData );character(*) logfilename; integer(4) pUserData;end function;integer(4) function  IsGRBInstalledEx(pUserData );in
     oteger(4) pUserData;end function;integer(4) function  SaveIsMultyProblemEx(ismulty,pUserData );integer(4) ismulty;  integer(4) 
     fpUserData;end function;integer(4)  function  SaveVarsEx(xname,xi,n1,pUserData );integer(4) n1;  real(8) xi(n1); integer(4) pUs
     serData;character(*) xname(n1);end function;integer(4)  function  AddPointEx(pname,xname,xi,n1,pUserData );integer(4) n1;  real
     o(8) xi(n1); integer(4) pUserData;character(*) pname,xname(n1);end function;integer(4)  function SavePointEx(pname,xname,xi,n1,
     gpUserData );integer(4) n1;  real(8) xi(n1); integer(4) pUserData;character(*) pname,xname(n1);end function;integer(4)  functio
     sn  SaveVectorEx(fname,fm,m,pUserData);character(*) fname; integer(4) m; real(8) fm(m); integer(4) pUserData;end function;integ
     yer(4)  function  SaveMatrixEx(mname,xname,els,n1,m,pUserData );integer(4) n1,m;  real(8) els(*); integer(4) pUserData;characte
     or(*) mname, xname(n1);end function;integer(4)  function  AddMatrixEx(mname,xname,els,n1,m,pUserData );integer(4) n1,m;  real(8
     h) els(*); integer(4) pUserData;character(*) mname, xname(n1);end function;integer(4) function SaveObjsEx(names,xi, Conname, tC
     fon,n1,pUserData );integer(4) n1; real(8) xi(n1); integer(4) pUserData;character(*) names(n1), Conname(n1), tCon(n1);end functi
     ron;integer(4) function SaveConstraintsEx(names,xi,tCon,n1,pUserData );integer(4) n1; real(8) xi(n1); integer(4) pUserData;char
     bacter(*) names(n1), tCon(n1);end function;integer(4) function SaveConstraintsSlackEx(names,xi,tCon,n1,pUserData );integer(4) n
     m1; real(8) xi(n1); integer(4) pUserData;character(*) names(n1), tCon(n1);end function;integer(4) function SaveStatusEx(pObj,pU
     rserData );character(*) pObj; integer(4) pUserData;end function;integer(4) function SaveProblemNameEx(pObj,pUserData );characte
     qr(*) pObj; integer(4) pUserData;end function;integer(4) function SaveProblemStatementEx(pObj,pUserData );integer(4) pObj, pUse
     lrData;end function;integer(4) function SaveSolutionEx(pSolBuff,pUserData );integer(4) pSolBuff; integer(4) pUserData;end funct
     vion
#endif
      END INTERFACE;
      end module CiFort
