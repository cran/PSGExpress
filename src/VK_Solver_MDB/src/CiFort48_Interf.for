      MODULE CiFort;use IntelInterf
#define ___x64
      INTERFACE
#ifdef ___x64
      subroutine ChangeMipGap(imodel, xmipgap);use IntTypes; integer(plen) imodel; real(8) xmipgap;end subroutine;subroutine InitGur
     lbModelIshtvan(nRows,nCols, matrixColStart, matrixRowIndices, matrixRowValues,
     +colLower, colUpper, colType, objCoeff, rowLower, rowUpperint, Tlimit,xmipgap, ifail, ienv,imodel );use IntTypes; integer(plen)
     l ienv,imodel; integer(1) colType(*);integer(4) nRows,nCols,matrixColStart(*), matrixRowIndices(*),ifail;real(8) matrixRowValue
     ls(*),colLower(*),colUpper(*),objCoeff(*),rowLower(*),rowUpperint(*),Tlimit,xmipgap;end subroutine;subroutine ReloadGurbModelIs
     lhtvan(ienv,imodel, ibs,nRows,nCols, matrixColStart, matrixRowIndices, matrixRowValues,
     +colLower, colUpper, objCoeff, rowLower, rowUpperint,        xcol,xrow,slack,cmark,rmark,ifail, xiter,sinf);use IntTypes; integ
     ler(plen) ienv,imodel;integer(4) ibs,nRows,nCols,matrixColStart(*), matrixRowIndices(*),cmark(*),rmark(*),ifail;real(8) matrixR
     lowValues(*),colLower(*),colUpper(*),objCoeff(*),rowLower(*),rowUpperint(*),
     +xcol(*) ,xrow(*),slack(*),xiter,sinf;end subroutine;subroutine UpdateRunGurbModelIshtvan(klin,ienv,imodel, ibs,nRows,nCols, ma
     ltrixColStart, matrixRowIndices, matrixRowValues,
     +colLower, colUpper, objCoeff, rowLower, rowUpperint, timelimit,isMIP,  xcol,xrow,slack,cmark,rmark,ifail, xiter,sinf,gap);use 
     lIntTypes; integer(plen) ienv,imodel;integer(4) klin,ibs,nRows,nCols,matrixColStart(*), matrixRowIndices(*),cmark(*),rmark(*),i
     lfail,isMip;real(8) matrixRowValues(*),colLower(*),colUpper(*),objCoeff(*),rowLower(*),rowUpperint(*),
     +xcol(*) ,xrow(*),slack(*),xiter,sinf,timelimit,gap;end subroutine;subroutine MPSRunGurbModelIshtvan(
     +xcol,xrow,slack,cmark,rmark,ifail, xiter,sinf);integer(4) cmark(*),rmark(*),ifail; real(8) xcol(*),xrow(*),slack(*),xiter,sinf
      end subroutine;subroutine DelGurbConstr(imodel, mdel,idelind,ifail);use IntTypes; integer(plen) imodel; integer(4) mdel,ifail,
     lidelind(*);end subroutine;subroutine InitGurbModelPshen_Ish(nCols,n0,nRows,objCoeff,  matrixColStart, matrixRowIndices, matrix
     lRowValues,
     +rowLower, rowUpperint,colLower, colUpper, colType,             lql,iqpro,cd,c,   Tlimit, xmipgap,
     +ifail, ienv,imodel );use IntTypes; integer(plen) ienv,imodel; logical lql; integer(1) colType(*);integer(4) nCols,n0,nRows,mat
     lrixColStart(*), matrixRowIndices(*),ifail,iqpro;real(8) matrixRowValues(*),colLower(*),colUpper(*),objCoeff(*),rowLower(*),row
     lUpperint(*),cd(*),c(*),Tlimit,xmipgap;end subroutine;subroutine InitGurbModelPshen_0(lql,n,me,mk,a,b,cd,d, xl,xu, iprint,iout,
     l ifail,ienv,imodel);use IntTypes; integer(plen) ienv,imodel; logical lql; integer(4) n,me,mk, iprint,iout, ifail;real(8) a(*),
     lb(*),cd(*),d(*), xl(*),xu(*);end subroutine;subroutine ReloadAndRunGurbPshen(ienv,imodel, ldn,n,mk,klin,a,b,d, iprint,iout,  x
     l,ifail,dual,slack, nact,iact,iu,iter);use IntTypes; integer(plen) ienv,imodel; integer(4) n,mk,klin,nact, iprint,iout, ifail, 
     liact(*),iu(*),iter;real(8) a(*),b(*),d(*),  x(*),dual(*),slack(*); logical ldn;end subroutine;subroutine UpdateAndRunGurbPshen
     l(ienv,imodel,n,mk, ldn,d, kac0,intarr0,dparr,rhs, iprint,iout,isMIP,
     +x,dual,slack,ifail, nact,iu,ibter,gap);use IntTypes; integer(plen) ienv,imodel; logical ldn;integer(4) n,mk,kac0(*),intarr0(*)
     l, iprint,iout, ifail, nact,iu(*),ibter, isMIP;real(8) d(*),dparr(*),rhs(*), x(*),dual(*),slack(*),gap;end subroutine;subroutin
     le FreeGurb(ienv,imodel,ifail);use IntTypes; integer(plen) ienv,imodel; integer(4) ifail;end subroutine;subroutine AX1_set(real
     lall,nx,ml,ax);real(8),target::realall(*); real(8),pointer::ax(:,:); integer(4) nx,ml;end subroutine;subroutine i4_pointer_set(
     liarr,n1,n2,i4);integer(4),pointer::i4(:); integer(4),target::iarr(*); integer(4) n1,n2;end subroutine i4_pointer_set;subroutin
     le r8_pointer_set(xarr,n1,n2,r8);real(8),pointer::r8(:); real(8),target::xarr(*); integer(4) n1,n2;end subroutine r8_pointer_se
     lt;subroutine r8d_pointer_set(xarr,n1,n2,m1,m2,r8d);real(8),pointer::r8d(:,:); real(8),target::xarr(*); integer(4) n1,n2,m1,m2;
      end subroutine r8d_pointer_set;subroutine CheckNextPart(icd,pChar,ibuff,ipos1,  kfor,kprobl,ipos2, wstr,j);use IntTypes; integ
     ler(4) icd,ibuff,ipos1,kfor,kprobl,ipos2; integer(plen),value:: pChar,wstr; integer(llen),value:: j;end subroutine;subroutine s
     letpointer(probaddr, pChar);use IntTypes; integer(plen) probaddr; integer(plen),pointer::pChar;end subroutine;subroutine getpoi
     lntaddr(pChar,probaddr);use IntTypes; integer(plen) probaddr; integer(4),pointer::pChar;end subroutine;subroutine setRealArrPoi
     lnter(adrs,ln, rpoint);use IntTypes; integer(plen),value:: adrs; integer(4) ln; real(8),pointer:: rpoint(:);end subroutine;subr
     loutine setRealArrPointer_VAL(adrs, rpoint);use IntTypes; integer(plen) adrs; real(8),pointer:: rpoint(:);end subroutine;subrou
     ltine setIntArrPointer(adrs,ln, ipoint);use IntTypes; integer(plen),value:: adrs; integer(4) ln; integer(4),pointer:: ipoint(:)
      end subroutine;subroutine setCharpointaddr(probaddr,pChar);use IntTypes; integer(plen) probaddr; character(*),pointer::pChar;e
     lnd subroutine;subroutine getCharpointaddr(pChar,probaddr);use IntTypes; integer(plen) probaddr; character,pointer::pChar;end s
     lubroutine;subroutine sortVK(m,fm, list,inverslist, iouttype,iorder);integer(4),optional:: iouttype,iorder,inverslist(*);intege
     lr(4) m,list(*); real(8) fm(*);end subroutine;subroutine sortVKint4(m,ifm, list,inverslist, iouttype,iorder);integer(4),optiona
     ll:: iouttype,iorder,inverslist(*);integer(4) m,list(*),ifm(*);end subroutine;subroutine clearbuff(buff,bufflen);use IntTypes; 
      integer(plen),value:: buff; integer(llen),value:: bufflen;end subroutine;subroutine savestr(jaddr,name,ibfree,   iblen,namelen
     l);use IntTypes; integer(plen),value:: jaddr,name; integer(llen),value:: iblen,namelen; integer(4) ibfree;end subroutine;charac
     lter(1000) function RCStr(charI,ich_len);integer(4) ich_len; character(*) charI;end function;integer(4) function CStrLen(charI,
     lich_len);integer(4) ich_len; character(*) charI;end function;subroutine Convert_Str(iCi_str,Fort_str,        Clen,Flen);use In
     ltTypes; integer(plen),value:: Fort_str; integer(llen),value:: Clen,Flen;
#ifndef __GNUC__
      integer(4),pointer:: iCi_str;
#endif
      end subroutine;subroutine GetRow(char,wstr,lrest,   char_len,lwstr);use IntTypes; integer(plen),value:: char; integer(4) lrest
     l; integer(llen),value:: char_len,lwstr;character(*) wstr;end subroutine;subroutine MaxRowBuff(pChar,   krows,lrowm,kzp,km,kc, 
     l  ibuff);use IntTypes; integer(plen) pChar; integer(4) krows,lrowm,kzp,km,kc; integer(llen),value:: ibuff;end;integer(4) funct
     lion  GetProblemDescriptionEx(char, pUserData );use IntTypes; integer(ppUD),value:: pUserData; integer(4),pointer:: char;end fu
     lnction;integer(4) function  GetMatrixInfoEx(pMatr,pChar,ipRow,ipCol,pUserData );use IntTypes; integer(ppUD),value:: pUserData;
       integer(4), pointer:: pChar; character(*) pMatr;integer(4) ipRow,ipCol;end function;integer(4) function  GetMatrixDataEx(pMNa
     lme,pMElem,Row,Col,pUserData );use IntTypes; integer(ppUD),value:: pUserData; real(8),pointer:: pMElem; integer(4),value:: Row,
     lCol; character(*) pMName;end function;integer(4) function  GetMatrixInfoSpEx(pMatr,pChar,ipRow,ipCol,ipElm, pUserData );use In
     ltTypes; integer(ppUD),value:: pUserData; integer(4),pointer:: pChar;character(*),intent(in):: pMatr;integer(4) ipRow,ipCol,ipE
     llm;end function;integer(4) function  GetMatrixDataSpEx(pMName,pMElem,ipRow,ipCol,kcoeffs,pUserData );use IntTypes; integer(ppU
     lD),value:: pUserData; character(*),intent(in):: pMName;real(8),pointer,intent(out):: pMElem; integer(4),pointer,intent(out):: 
     lipRow, ipCol;integer(4),intent(in),value:: kcoeffs;end function;integer(4) function  GetExternalFunctionInfoEx(pExtFun,pChar,i
     lpCol,pUserData );use IntTypes; integer(ppUD),value:: pUserData; character(*),intent(in):: pExtFun;integer(4),pointer:: pChar;i
     lnteger(4) ipCol;end function;integer(4) function  RunExternalFunctionEx(pExtFun,ipCol,pVarNames,pX,pFun,pUserData );use IntTyp
     les; integer(ppUD),value:: pUserData; intent(in) pExtFun,pVarNames,pUserData,pX    ,ipCol;real(8),intent(out):: pFun; character
     l(*) pExtFun,pVarNames;integer(4),value:: ipCol;real(8) pX(ipCol);end function;integer(4) function  RunGradientExternalFunction
     lEx(pExtFun,ipCol,pVarNames,pX,pGrad,pUserData );use IntTypes; integer(ppUD),value:: pUserData; character(*),intent(in):: pExtF
     lun,pVarNames;integer(4),intent(in),value::ipCol;real(8),intent(in)::  pX(*);real(8),intent(out):: pGrad(:);end function;intege
     lr(4) function  GetExternalFunctionInfoDirEx(pExtFun,pChar,ipCol,kScen,pUserData );use IntTypes; integer(ppUD),value:: pUserDat
     la; character(*),intent(in):: pExtFun;integer(4),pointer:: pChar;integer(4) ipCol,kScen;end function;integer(4) function  RunEx
     lternalFunctionDirEx(pExtFun,pVarNames,ipCol,pX,nScen,iScen,p,vScen,pUserData );use IntTypes; integer(ppUD),value:: pUserData; 
      intent(in) pExtFun,pVarNames,pUserData,ipCol,nScen ,pX,iScen;real(8),intent(out):: p(:),vScen(:); character(*) pExtFun,pVarNam
     les;integer(4),value:: ipCol,nScen;real(8) pX(ipCol);integer(4) iScen(nScen);end function;integer(4) function  RunGradientExter
     lnalFunctionDirEx(pExtFun,pVarNames,ipCol,pX,nScen,iScen,p,pGrad,pUserData );use IntTypes; integer(ppUD),value:: pUserData;char
     lacter(*),intent(in):: pExtFun,pVarNames;           integer(4),intent(in),value::ipCol,nScen;real(8),intent(in)::  pX(*);intege
     lr(4),intent(in):: iScen(*);real(8),intent(out):: p(:),pGrad(:);end function;integer(4) function  GetInitialPointEx(char,pUserD
     lata );use IntTypes; integer(ppUD),value:: pUserData; integer(4), pointer :: char;end function;integer(4) function  GetPointEx(
     lPointName,char,pUserData );use IntTypes; integer(ppUD),value:: pUserData; character(*)PointName; integer(4),pointer::char;end 
     lfunction;subroutine ReleaseBufferEx(ibuff,pUserData );use IntTypes; integer(ppUD),value:: pUserData;integer(4) ibuff;end subro
     lutine;subroutine ReleaseMatrixEx(pMElem,pUserData );use IntTypes; integer(ppUD),value:: pUserData;real(8) pMElem;end subroutin
     le;subroutine OnMessageEx(icode,errDesc,pUserData );use IntTypes; integer(ppUD),value:: pUserData; character(*) errDesc; intege
     lr(4),value:: icode;end subroutine;subroutine OnErrorEx(icode,errPlace, errDesc,pUserData );use IntTypes; integer(ppUD),value::
     l pUserData; character(*) errPlace, errDesc; integer(4),value:: icode;end subroutine;subroutine OnWarningEx(icode, warPlace, wa
     lrDesc,pUserData );use IntTypes; integer(ppUD),value:: pUserData; character(*) warPlace, warDesc; integer(4),value:: icode;end 
     lsubroutine;integer(4) function OnCancelEx(pUserData);use IntTypes; integer(ppUD),value:: pUserData;end function;integer(4) fun
     lction OnFinishEx(pUserData);use IntTypes; integer(ppUD),value:: pUserData;end function;integer(4) function OnCycleFinishEx(pUs
     lerData);use IntTypes; integer(ppUD),value:: pUserData;end function;subroutine GetRootPathEx(path, pUserData );use IntTypes; in
     lteger(ppUD),value:: pUserData; character(*) path;end subroutine;integer(4) function GetLogParamEx(logfilename, pUserData );use
     l IntTypes; integer(ppUD),value:: pUserData; character(*) logfilename;end function;integer(4) function  IsGRBInstalledEx(pUserD
     lata );use IntTypes; integer(ppUD),value:: pUserData;end function;integer(4) function  SaveIsMultyProblemEx(ismulty,pUserData )
      use IntTypes; integer(ppUD),value:: pUserData; integer(4),value:: ismulty;end function;integer(4)  function  SaveObjective2(xn
     lame,xi,pUserData );use IntTypes; integer(ppUD),value:: pUserData; real(8),value:: xi;character(*) xname(*);end function;intege
     lr(4)  function  SaveVarsEx(xname,xi,n1,pUserData );use IntTypes; integer(ppUD),value:: pUserData; integer(4),value:: n1;  real
     l(8) xi(n1);character(*) xname(n1);end function;integer(4)  function  AddPointEx(pname,xname,xi,n1,pUserData );use IntTypes; in
     lteger(ppUD),value:: pUserData; integer(4),value:: n1;  real(8) xi(n1);character(*) xname(n1),pname;end function;integer(4)  fu
     lnction  SavePointEx(pname,xname,xi,n1,pUserData );use IntTypes; integer(ppUD),value:: pUserData; integer(4),value:: n1;  real(
     l8) xi(n1);character(*) xname(n1),pname;end function;integer(4)  function  SaveVectorEx(fname,fm,m,pUserData);use IntTypes; int
     leger(ppUD),value:: pUserData; character(*) fname; integer(4),value:: m; real(8) fm(m);end function;integer(4)  function  SaveM
     latrixEx(mname,xname,els,n1,m,pUserData );use IntTypes; integer(ppUD),value:: pUserData; integer(4),value:: n1,m; real(8) els(*
     l);character(*) mname, xname(n1);end function;integer(4)  function  AddMatrixEx(mname,xname,els,n1,m,pUserData );use IntTypes; 
      integer(ppUD),value:: pUserData; integer(4),value:: n1,m; real(8) els(*);character(*) mname, xname(n1);end function;integer(4)
     l function SaveObjsEx(names,xi, Conname, tCon,n1,pUserData );use IntTypes; integer(ppUD),value:: pUserData; integer(4),value:: 
     ln1; real(8) xi(n1);character(*) names(n1), Conname(n1), tCon(n1);end function;integer(4) function SaveConstraintsEx(names,xi,t
     lCon,n1,pUserData );use IntTypes; integer(ppUD),value:: pUserData; integer(4),value:: n1; real(8) xi(n1);character(*) names(n1)
     l, tCon(n1);end function;integer(4) function SaveConstraintsSlackEx(names,xi,tCon,n1,pUserData );use IntTypes; integer(ppUD),va
     llue:: pUserData; integer(4),value:: n1; real(8) xi(n1);character(*) names(n1), tCon(n1);end function;integer(4) function SaveS
     ltatusEx(pObj,pUserData );use IntTypes; integer(ppUD),value:: pUserData; character(*) pObj;end function;integer(4) function Sav
     leProblemNameEx(pObj,pUserData );use IntTypes; integer(ppUD),value:: pUserData; character(*) pObj;end function;integer(4) funct
     lion SaveProblemStatementEx(pObj,pUserData );use IntTypes; integer(ppUD),value:: pUserData; integer(plen),value:: pObj;end func
     ltion;integer(4) function SaveSolutionEx(pSolBuff,pUserData );use IntTypes; integer(ppUD),value:: pUserData; integer(plen),valu
     le:: pSolBuff;end function
#else
      subroutine ChangeMipGap(imodel, xmipgap);use IntTypes; integer(plen) imodel; real(8) xmipgap;end subroutine;subroutine InitGur
     lbModelIshtvan(nRows,nCols, matrixColStart, matrixRowIndices, matrixRowValues,
     +colLower, colUpper, colType, objCoeff, rowLower, rowUpperint, Tlimit,xmipgap, ifail, ienv,imodel );use IntTypes; integer(plen)
     l ienv,imodel; integer(1) colType(*);integer(4) nRows,nCols,matrixColStart(*), matrixRowIndices(*),ifail;real(8) matrixRowValue
     ls(*),colLower(*),colUpper(*),objCoeff(*),rowLower(*),rowUpperint(*),Tlimit,xmipgap;end subroutine;subroutine ReloadGurbModelIs
     lhtvan(ienv,imodel, ibs,nRows,nCols, matrixColStart, matrixRowIndices, matrixRowValues,
     +colLower, colUpper, objCoeff, rowLower, rowUpperint,        xcol,xrow,slack,cmark,rmark,ifail, xiter,sinf);use IntTypes; integ
     ler(plen) ienv,imodel;integer(4) ibs,nRows,nCols,matrixColStart(*), matrixRowIndices(*),cmark(*),rmark(*),ifail;real(8) matrixR
     lowValues(*),colLower(*),colUpper(*),objCoeff(*),rowLower(*),rowUpperint(*),
     +xcol(*) ,xrow(*),slack(*),xiter,sinf;end subroutine;subroutine UpdateRunGurbModelIshtvan(klin,ienv,imodel, ibs,nRows,nCols, ma
     ltrixColStart, matrixRowIndices, matrixRowValues,
     +colLower, colUpper, objCoeff, rowLower, rowUpperint, timelimit,isMIP,  xcol,xrow,slack,cmark,rmark,ifail, xiter,sinf,gap);use 
     lIntTypes; integer(plen) ienv,imodel;integer(4) klin,ibs,nRows,nCols,matrixColStart(*), matrixRowIndices(*),cmark(*),rmark(*),i
     lfail,isMip;real(8) matrixRowValues(*),colLower(*),colUpper(*),objCoeff(*),rowLower(*),rowUpperint(*),
     +xcol(*) ,xrow(*),slack(*),xiter,sinf,timelimit,gap;end subroutine;subroutine MPSRunGurbModelIshtvan(
     +xcol,xrow,slack,cmark,rmark,ifail, xiter,sinf);integer(4) cmark(*),rmark(*),ifail; real(8) xcol(*),xrow(*),slack(*),xiter,sinf
      end subroutine;subroutine DelGurbConstr(imodel, mdel,idelind,ifail);use IntTypes; integer(plen) imodel; integer(4) mdel,ifail,
     lidelind(*);end subroutine;subroutine InitGurbModelPshen_Ish(nCols,n0,nRows,objCoeff,  matrixColStart, matrixRowIndices, matrix
     lRowValues,
     +rowLower, rowUpperint,colLower, colUpper, colType,             lql,iqpro,cd,c,   Tlimit, xmipgap,
     +ifail, ienv,imodel );use IntTypes; integer(plen) ienv,imodel; logical lql; integer(1) colType(*);integer(4) nCols,n0,nRows,mat
     lrixColStart(*), matrixRowIndices(*),ifail,iqpro;real(8) matrixRowValues(*),colLower(*),colUpper(*),objCoeff(*),rowLower(*),row
     lUpperint(*),cd(*),c(*),Tlimit,xmipgap;end subroutine;subroutine InitGurbModelPshen_0(lql,n,me,mk,a,b,cd,d, xl,xu, iprint,iout,
     l ifail,ienv,imodel);use IntTypes; integer(plen) ienv,imodel; logical lql; integer(4) n,me,mk, iprint,iout, ifail;real(8) a(*),
     lb(*),cd(*),d(*), xl(*),xu(*);end subroutine;subroutine ReloadAndRunGurbPshen(ienv,imodel, ldn,n,mk,klin,a,b,d, iprint,iout,  x
     l,ifail,dual,slack, nact,iact,iu,iter);use IntTypes; integer(plen) ienv,imodel; integer(4) n,mk,klin,nact, iprint,iout, ifail, 
     liact(*),iu(*),iter;real(8) a(*),b(*),d(*),  x(*),dual(*),slack(*); logical ldn;end subroutine;subroutine UpdateAndRunGurbPshen
     l(ienv,imodel,n,mk, ldn,d, kac0,intarr0,dparr,rhs, iprint,iout,isMIP,
     +x,dual,slack,ifail, nact,iu,ibter,gap);use IntTypes; integer(plen) ienv,imodel; logical ldn;integer(4) n,mk,kac0(*),intarr0(*)
     l, iprint,iout, ifail, nact,iu(*),ibter, isMIP;real(8) d(*),dparr(*),rhs(*), x(*),dual(*),slack(*),gap;end subroutine;subroutin
     le FreeGurb(ienv,imodel,ifail);use IntTypes; integer(plen) ienv,imodel; integer(4) ifail;end subroutine;subroutine AX1_set(real
     lall,nx,ml,ax);real(8),target::realall(*); real(8),pointer::ax(:,:); integer(4) nx,ml;end subroutine;subroutine i4_pointer_set(
     liarr,n1,n2,i4);integer(4),pointer::i4(:); integer(4),target::iarr(*); integer(4) n1,n2;end subroutine i4_pointer_set;subroutin
     le r8_pointer_set(xarr,n1,n2,r8);real(8),pointer::r8(:); real(8),target::xarr(*); integer(4) n1,n2;end subroutine r8_pointer_se
     lt;subroutine r8d_pointer_set(xarr,n1,n2,m1,m2,r8d);real(8),pointer::r8d(:,:); real(8),target::xarr(*); integer(4) n1,n2,m1,m2;
      end subroutine r8d_pointer_set;subroutine CheckNextPart(icd,pChar,ibuff,ipos1,  kfor,kprobl,ipos2, wstr,j);use IntTypes; integ
     ler(4) icd,ibuff,ipos1,kfor,kprobl,ipos2; integer(plen),value:: pChar,wstr; integer(llen),value:: j;end subroutine;subroutine s
     letpointer(probaddr, pChar);integer(4) probaddr; integer(4),pointer::pChar;end subroutine;subroutine getpointaddr(pChar,probadd
     lr);integer(4) probaddr; integer(4),pointer::pChar;end subroutine;subroutine sortVK(m,fm, list,inverslist,  iouttype,iorder);in
     lteger(4),optional:: iouttype,iorder,inverslist(*);integer(4) m,list(*) ; real(8) fm(*);end subroutine;subroutine sortVKint4(m,
     lifm, list,inverslist, iouttype,iorder);integer(4),optional:: iouttype,iorder,inverslist(*);integer(4) m,list(*),ifm(*);end sub
     lroutine;subroutine clearbuff(buff,bufflen);integer(4) buff; integer(4) bufflen;end subroutine;subroutine savestr(jaddr,name,ib
     lfree,   iblen,namelen);integer(4) jaddr,name; integer(4) iblen,namelen,ibfree;end subroutine;function RCStr(charI,ich_len);int
     leger(4) ich_len; character(*) charI, RCStr*(ich_len);end function;integer(4) function CStrLen(charI,ich_len);integer(4) ich_le
     ln; character(*) charI;end function;subroutine Convert_Str(Ci_str,Fort_str,       Clen,Flen);integer(4),pointer::Ci_str;  integ
     ler(4) Fort_str; integer(4) Clen, Flen;end subroutine;subroutine GetRow(char,wstr,lrest,   char_len,lwstr);integer(4) char_len,
     llrest,lwstr; integer(4),value:: char;character(*) wstr;end subroutine;subroutine MaxRowBuff(pChar,   krows,lrowm,kzp,km,kc,   
     libuff);use IntTypes; integer(plen) pChar; integer(4) krows,lrowm,kzp,km,kc; integer(4),value:: ibuff;end subroutine;integer(4)
     l function  iRequareObject2(name,namelen,itype,iclss,pData,pParam,lParam);character(*),intent(in):: name;integer(4) namelen,ity
     lpe,iclss,lParam;integer(4),intent(out):: pData, pParam(*);end function;integer(4)  function  GetProblemDescriptionEx(char, pUs
     lerData );integer(4), pointer :: char; integer(4) pUserData;end function;integer(4)  function  GetMatrixInfoEx(pMatr,pChar,pRow
     l,pCol,pUserData );integer(4), pointer ::  pRow,pCol, pChar; character(*) pMatr; integer(4) pUserData;end function;integer(4)  
     lfunction  GetMatrixDataEx(pMName,pMElem,Row,Col,pUserData );real(8), pointer:: pMElem; integer(4)  Row, Col; character(*) pMNa
     lme; integer(4) pUserData;end function;integer(4)  function  GetMatrixInfoSpEx(pMatr,pChar,pRow,pCol,pElm, pUserData );characte
     lr(*),intent(in):: pMatr;integer(4), pointer::  pRow,pCol,pElm, pChar; integer(4)::pUserData;end function;integer(4)  function 
     l GetMatrixDataSpEx(pMName,pMElem,pRow,pCol,kcoeffs,pUserData );character(*),intent(in):: pMName; integer(4),intent(in):: pUser
     lData;real(8),pointer,intent(out):: pMElem; integer(4),pointer,intent(out):: pRow, pCol;integer(4),intent(in):: kcoeffs;end fun
     lction;integer(4)  function  GetExternalFunctionInfoEx(pExtFun,pChar,pCol,pUserData );character(*),intent(in):: pExtFun;integer
     l(4), pointer::  pCol, pChar; integer(4)::pUserData;end function;integer(4)  function  RunExternalFunctionEx(pExtFun,pCol,pVarN
     lames,pX,pFun,pUserData );intent(in) pExtFun,pVarNames,pCol,pUserData,pX;real(8),intent(out):: pFun; character(*) pExtFun,pVarN
     lames; integer(4) pCol; integer(4) pUserData;real(8) pX(pCol);end function;integer(4)  function  RunGradientExternalFunctionEx(
     lpExtFun,pCol,pVarNames,pX,pGrad,pUserData );character(*),intent(in):: pExtFun,pVarNames;           integer(4),intent(in):: pCo
     ll;integer(4),intent(in):: pUserData;real(8),intent(in)::  pX(*);real(8),intent(out):: pGrad(:);end function;integer(4) functio
     ln  GetExternalFunctionInfoDirEx(pExtFun,pChar,ipCol,kScen,pUserData );use IntTypes; integer(plen),value:: pUserData; character
     l(*),intent(in):: pExtFun;integer(4),pointer:: pChar; integer(4),pointer::  ipCol;integer(4) kScen;end function;integer(4) func
     ltion  RunExternalFunctionDirEx(pExtFun,pVarNames,ipCol,pX,nScen,iScen,p,vScen,pUserData );use IntTypes; integer(plen),value:: 
     lpUserData; intent(in) pExtFun,pVarNames,pUserData,ipCol,nScen ,pX,iScen;real(8),intent(out):: p(:),vScen(:); character(*) pExt
     lFun,pVarNames;integer(4),value:: ipCol,nScen;real(8) pX(ipCol);integer(4) iScen(nScen);end function;integer(4) function  RunGr
     ladientExternalFunctionDirEx(pExtFun,pVarNames,ipCol,pX,nScen,iScen,p,pGrad,pUserData );use IntTypes; integer(plen),value:: pUs
     lerData;character(*),intent(in):: pExtFun,pVarNames;           integer(4),intent(in),value::ipCol,nScen;real(8),intent(in)::  p
     lX(*);integer(4),intent(in):: iScen(*);real(8),intent(out):: p(:),pGrad(:);end function;integer(4)  function  GetInitialPointEx
     l(char,pUserData );integer(4), pointer :: char; integer(4) pUserData;end function;integer(4)  function  GetPointEx(PointName,ch
     lar,pUserData );character(*)PointName; integer(4),pointer::char; integer(4) pUserData;end function;subroutine ReleaseBufferEx(b
     luff,pUserData );integer(4),pointer :: buff; integer(4) pUserData;end subroutine;subroutine ReleaseMatrixEx(pMElem,pUserData );
      real(8),pointer :: pMElem; integer(4) pUserData;end subroutine;subroutine OnMessageEx(icode,errDesc,pUserData );character(*) e
     lrrDesc; integer(4) icode; integer(4) pUserData;end subroutine;subroutine OnErrorEx(icode,errPlace, errDesc,pUserData );charact
     ler(*) errPlace, errDesc; integer(4) icode; integer(4) pUserData;end subroutine;subroutine OnWarningEx(icode, warPlace, warDesc
     l,pUserData );character(*) warPlace, warDesc; integer(4) icode; integer(4) pUserData;end subroutine;integer(4) function OnCance
     llEx(pUserData);integer(4) pUserData;end function;integer(4) function OnFinishEx(pUserData);integer(4) pUserData;end function;i
     lnteger(4) function OnCycleFinishEx(pUserData);integer(4) pUserData;end function;subroutine GetRootPathEx(path, pUserData );cha
     lracter(*) path; integer(4) pUserData;end subroutine;integer(4) function GetLogParamEx(logfilename, pUserData );character(*) lo
     lgfilename; integer(4) pUserData;end function;integer(4) function  IsGRBInstalledEx(pUserData );integer(4) pUserData;end functi
     lon;integer(4) function  SaveIsMultyProblemEx(ismulty,pUserData );integer(4) ismulty;  integer(4) pUserData;end function;intege
     lr(4)  function  SaveObjective2(xname,xi,pUserData );real(8) xi; integer(4) pUserData;character(*) xname(*);end function;intege
     lr(4)  function  SaveVarsEx(xname,xi,n1,pUserData );integer(4) n1;  real(8) xi(n1); integer(4) pUserData;character(*) xname(n1)
      end function;integer(4)  function  AddPointEx(pname,xname,xi,n1,pUserData );integer(4) n1;  real(8) xi(n1); integer(4) pUserDa
     lta;character(*) pname,xname(n1);end function;integer(4)  function SavePointEx(pname,xname,xi,n1,pUserData );integer(4) n1;  re
     lal(8) xi(n1); integer(4) pUserData;character(*) pname,xname(n1);end function;integer(4)  function  SaveVectorEx(fname,fm,m,pUs
     lerData);character(*) fname; integer(4) m; real(8) fm(m); integer(4) pUserData;end function;integer(4)  function  SaveMatrixEx(
     lmname,xname,els,n1,m,pUserData );integer(4) n1,m;  real(8) els(*); integer(4) pUserData;character(*) mname, xname(n1);end func
     ltion;integer(4)  function  AddMatrixEx(mname,xname,els,n1,m,pUserData );integer(4) n1,m;  real(8) els(*); integer(4) pUserData
      character(*) mname, xname(n1);end function;integer(4) function SaveObjsEx(names,xi, Conname, tCon,n1,pUserData );integer(4) n1
     l; real(8) xi(n1); integer(4) pUserData;character(*) names(n1), Conname(n1), tCon(n1);end function;integer(4) function SaveCons
     ltraintsEx(names,xi,tCon,n1,pUserData );integer(4) n1; real(8) xi(n1); integer(4) pUserData;character(*) names(n1), tCon(n1);en
     ld function;integer(4) function SaveConstraintsSlackEx(names,xi,tCon,n1,pUserData );integer(4) n1; real(8) xi(n1); integer(4) p
     lUserData;character(*) names(n1), tCon(n1);end function;integer(4) function SaveStatusEx(pObj,pUserData );character(*) pObj; in
     lteger(4) pUserData;end function;integer(4) function SaveProblemNameEx(pObj,pUserData );character(*) pObj; integer(4) pUserData
      end function;integer(4) function SaveProblemStatementEx(pObj,pUserData );integer(4) pObj, pUserData;end function;integer(4) fu
     lnction SaveSolutionEx(pSolBuff,pUserData );integer(4) pSolBuff; integer(4) pUserData;end function
#endif
      END INTERFACE;
      end module CiFort
