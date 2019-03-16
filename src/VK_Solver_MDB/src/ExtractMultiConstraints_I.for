      subroutine ExtractProblemStatementToFull(iVk_solver,  probaddr,  l16p0);use ModCommons; use FuncNames;integer(4) iVk_solver,l1
     l6p0; integer(plen) probaddr;integer(plen) probaddr_w,probaddr_in;integer(4)  mxkf,mxconstr,nabmx,l16p,km,kc; character(256)  c
     lhw;character(1),allocatable:: wstr(:), pwstr(:), wch(:);integer(4),external:: iBuffLen;interface;subroutine DivideFunRows(prob
     laddr,probaddr_w,  wstr,pwstr,wch,  l1,l2,l3  );use CiFort; integer(plen) probaddr,probaddr_w; integer(plen),value:: wstr,pwstr
     l,wch; integer(llen),value:: l1,l2,l3;end;subroutine EMConstraints(probaddr,l16p,probaddr_w, wstr,pwstr,wch, l1,l2,l3);use CiFo
     lrt; integer(plen) probaddr,probaddr_w; integer(4) l16p;integer(plen),value:: wstr,pwstr,wch; integer(llen),value:: l1,l2,l3;en
     ld;subroutine EMFunctions(probaddr,l16p,probaddr_w, wstr,pwstr,wch, l1,l2,l3);use CiFort; integer(plen) probaddr,probaddr_w; in
     lteger(4) l16p;integer(plen),value:: wstr,pwstr,wch; integer(llen),value:: l1,l2,l3;end;subroutine ExtractThreePointsForAllFunc
     ltions(probaddr,probaddr_w,  wstr,pwstr,wch,  l1,l2,l3  );use CiFort; integer(plen) probaddr,probaddr_w; integer(plen),value:: 
     lwstr,pwstr,wch; integer(llen),value:: l1,l2,l3;end;subroutine PrintBuffer(probaddr_w,iBuffLen);use CiFort; integer(plen) proba
     lddr_w; integer(llen),value:: iBuffLen;end;end interface;l16p0=0;if(probaddr==0) RETURN;  probaddr_in=probaddr;call set_func_na
     lmes(fnc_name,kfn);mxkf=1; mxconstr=1; nabmx=1;call read_task_0(probaddr,  mxkf,mxconstr,nabmx,  l16p,km,kc);allocate(wstr(l16p
     l),pwstr(l16p),wch(l16p));call DivideFunRows(probaddr,probaddr_w, loc(wstr),loc(pwstr),loc(wch), int(l16p,llen),int(l16p,llen),
     lint(l16p,llen) );deallocate(wstr,pwstr,wch);if(probaddr_w==0)then; chw='Internal error: DivideFunRows'; call putmess('S',6044,
     l'ExtractProblemStatementToFull',chw);goto 79999;endif;probaddr=probaddr_w; if(ioutk>=istop-1) goto 79999;if(iVk_solver==2) got
     lo 100;mxkf=1; mxconstr=1; nabmx=1;call read_task_0(probaddr,  mxkf,mxconstr,nabmx,  l16p,km,kc);allocate(wstr(l16p),pwstr(l16p
     l),wch(l16p));call EMConstraints(probaddr,l16p,probaddr_w,loc(wstr),loc(pwstr),loc(wch),  int(l16p,llen),int(l16p,llen),int(l16
     lp,llen) );deallocate(wstr,pwstr,wch);if(probaddr_w>0)then; call free(probaddr); probaddr=probaddr_w; endif;if(ioutk>=istop-1) 
     lgoto 79999;mxkf=1; mxconstr=1; nabmx=1;call read_task_0(probaddr,  mxkf,mxconstr,nabmx,  l16p,km,kc);allocate(wstr(l16p),pwstr
     l(l16p),wch(l16p));call EMFunctions(probaddr,l16p,probaddr_w,loc(wstr),loc(pwstr),loc(wch),  int(l16p,llen),int(l16p,llen),int(
     ll16p,llen) );deallocate(wstr,pwstr,wch);if(probaddr_w>0)then; call free(probaddr); probaddr=probaddr_w; endif;if(ioutk>=istop-
     l1) goto 79999
100   continue;if(iVk_solver/=1)then;mxkf=1; mxconstr=1; nabmx=1;call read_task_0(probaddr,  mxkf,mxconstr,nabmx,  l16p,km,kc);alloc
     late(wstr(l16p),pwstr(l16p),wch(l16p));call ExtractThreePointsForAllFunctions(probaddr, probaddr_w,loc(wstr),loc(pwstr),loc(wch
     l),
     +int(l16p,llen),int(l16p,llen),int(l16p,llen) );deallocate(wstr,pwstr,wch);if(probaddr_w>0)then;if(probaddr/=probaddr_in) call 
     lfree(probaddr);probaddr=probaddr_w;endif;if(ioutk>=istop-1) goto 79999;endif
79999 continue;l16p0=l16p;if(allocated(wstr))deallocate(wstr,pwstr,wch);return;
      end subroutine ExtractProblemStatementToFull
