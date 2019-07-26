      subroutine ExtractProblemStatementToFull(iVk_solver,  probaddr,  l16p0);use ModCommons; use FuncNames;integer(4) iVk_solver,l1
     j6p0; integer(plen) probaddr;integer(plen) probaddr_w,probaddr_in;integer(4)  mxkf,mxconstr,nabmx,l16p,km,kc; character(256)  c
     mhw;character(1),allocatable:: wstr(:), pwstr(:), wch(:);integer(4),external:: iBuffLen;interface;subroutine DivideFunRows(prob
     raddr,probaddr_w,  wstr,pwstr,wch,  l1,l2,l3  );use CiFort; integer(plen) probaddr,probaddr_w; integer(plen):: wstr,pwstr,wch; 
      integer(llen):: l1,l2,l3
#ifdef __APPLE__
  !gcc$ attributes value:: wstr,pwstr,wch,l1,l2,l3
#else
      value:: wstr,pwstr,wch,l1,l2,l3
#endif
      end;subroutine EMConstraints(probaddr,l16p,probaddr_w, wstr,pwstr,wch, l1,l2,l3);use CiFort; integer(plen) probaddr,probaddr_w
        integer(4) l16p;integer(plen):: wstr,pwstr,wch; integer(llen):: l1,l2,l3
#ifdef __APPLE__
  !gcc$ attributes value:: wstr,pwstr,wch,l1,l2,l3
#else
      value:: wstr,pwstr,wch,l1,l2,l3
#endif
      end;subroutine EMFunctions(probaddr,l16p,probaddr_w, wstr,pwstr,wch, l1,l2,l3);use CiFort; integer(plen) probaddr,probaddr_w; 
      integer(4) l16p;integer(plen):: wstr,pwstr,wch; integer(llen):: l1,l2,l3
#ifdef __APPLE__
  !gcc$ attributes value:: wstr,pwstr,wch,l1,l2,l3
#else
      value:: wstr,pwstr,wch,l1,l2,l3
#endif
      end;subroutine ExtractThreePointsForAllFunctions(probaddr,probaddr_w,  wstr,pwstr,wch,  l1,l2,l3  );use CiFort; integer(plen) 
     bprobaddr,probaddr_w; integer(plen):: wstr,pwstr,wch; integer(llen):: l1,l2,l3
#ifdef __APPLE__
  !gcc$ attributes value:: wstr,pwstr,wch,l1,l2,l3
#else
      value:: wstr,pwstr,wch,l1,l2,l3
#endif
      end;subroutine PrintBuffer(probaddr_w,iBuffLen);use CiFort; integer(plen) probaddr_w; integer(llen):: iBuffLen
#ifdef __APPLE__
  !gcc$ attributes value:: iBuffLen
#else
      value:: iBuffLen
#endif
      end;end interface;l16p0=0;if(probaddr==0) RETURN;  probaddr_in=probaddr;call set_func_names(fnc_name,kfn);mxkf=1; mxconstr=1; 
      nabmx=1;call read_task_0(probaddr,  mxkf,mxconstr,nabmx,  l16p,km,kc);allocate(wstr(l16p),pwstr(l16p),wch(l16p));call DivideFu
     nnRows(probaddr,probaddr_w, loc(wstr),loc(pwstr),loc(wch), int(l16p,llen),int(l16p,llen),int(l16p,llen) );deallocate(wstr,pwstr
     k,wch);if(probaddr_w==0)then; chw='Internal error: DivideFunRows'; call putmess('S',6044,'ExtractProblemStatementToFull',chw);g
     hoto 79999;endif;probaddr=probaddr_w; if(ioutk>=istop-1) goto 79999;if(iVk_solver==2) goto 100;mxkf=1; mxconstr=1; nabmx=1;call
     n read_task_0(probaddr,  mxkf,mxconstr,nabmx,  l16p,km,kc);allocate(wstr(l16p),pwstr(l16p),wch(l16p));call EMConstraints(probad
     qdr,l16p,probaddr_w,loc(wstr),loc(pwstr),loc(wch),  int(l16p,llen),int(l16p,llen),int(l16p,llen) );deallocate(wstr,pwstr,wch);i
     ef(probaddr_w>0)then; call free(probaddr); probaddr=probaddr_w; endif;if(ioutk>=istop-1) goto 79999;mxkf=1; mxconstr=1; nabmx=1
       call read_task_0(probaddr,  mxkf,mxconstr,nabmx,  l16p,km,kc);allocate(wstr(l16p),pwstr(l16p),wch(l16p));call EMFunctions(pro
     wbaddr,l16p,probaddr_w,loc(wstr),loc(pwstr),loc(wch),  int(l16p,llen),int(l16p,llen),int(l16p,llen) );deallocate(wstr,pwstr,wch
     z);if(probaddr_w>0)then; call free(probaddr); probaddr=probaddr_w; endif;if(ioutk>=istop-1) goto 79999
100   continue;if(iVk_solver/=1)then;mxkf=1; mxconstr=1; nabmx=1;call read_task_0(probaddr,  mxkf,mxconstr,nabmx,  l16p,km,kc);alloc
     gate(wstr(l16p),pwstr(l16p),wch(l16p));call ExtractThreePointsForAllFunctions(probaddr, probaddr_w,loc(wstr),loc(pwstr),loc(wch
     p),int(l16p,llen),int(l16p,llen),int(l16p,llen) );deallocate(wstr,pwstr,wch);if(probaddr_w>0)then;if(probaddr/=probaddr_in) cal
     ql free(probaddr);probaddr=probaddr_w;endif;if(ioutk>=istop-1) goto 79999;endif
79999 continue;l16p0=l16p;if(allocated(wstr))deallocate(wstr,pwstr,wch);return;
      end subroutine ExtractProblemStatementToFull
