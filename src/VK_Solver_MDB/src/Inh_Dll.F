      integer(4) function VK_Load_Basis(sizeg, pBuff);use ModCommons;interface;subroutine CopyBuff1(isize,addr,pbuff,   la,lp);use m
     modcommons; integer(llen):: la,lp; integer(plen):: addr,pbuff; integer(4) isize
#ifdef __APPLE__
  !gcc$ attributes value:: la,lp,addr,pbuff
#else
      value:: la,lp,addr,pbuff
#endif
      end subroutine;endinterface;integer(4):: sizeg; integer(plen):: pbuff
#ifdef __APPLE__
  !gcc$ attributes value:: sizeg,pbuff
#else
      value:: sizeg,pbuff
#endif
      integer(4) isave,iget,size; integer(plen) addr;common/SaveBas/ addr,isave,iget,size;size=sizeg; addr=0;if(size>0) addr=malloc(
     esize);if(addr>0.and.size>0)then;  call CopyBuff1(size,pbuff,addr,  int(1,llen),int(1,llen)); VK_Load_Basis=1; iget=1;else; add
     zr=0; VK_Load_Basis=0; iget=0; size=0;endif;return;end function VK_Load_Basis;integer(4) function VK_Save_Basis();use ModCommon
     ts;integer(4) isave,iget,size;integer(plen) addr;common/SaveBas/ addr,isave,iget,size;isave=1; VK_Save_Basis=1;end;integer(4) f
     munction VK_Get_Basis(sizeg, pBuff);use ModCommons;integer(4) sizeg; integer(plen) pBuff;integer(4) isave,iget,size; integer(pl
     een) addr;common/SaveBas/ addr,isave,iget,size;if(size<=0.or.addr<=0)then; sizeg=0; pbuff=0; VK_Get_Basis=0;else; sizeg=size; p
     zbuff=addr; VK_Get_Basis=1;endif;end;integer(4) function VK_Release_Basis_Buffer(pBuff);use ModCommons;integer(plen):: pbuff
#ifdef __APPLE__
  !gcc$ attributes value:: pbuff
#else
      value:: pbuff
#endif
      integer(4) isave,iget,size; integer(plen) addr;common/SaveBas/ addr,isave,iget,size;isave=0;if(pbuff>0)then; VK_Release_Basis_
     kBuffer=1; CALL FREE(pbuff);else; VK_Release_Basis_Buffer=0;endif;
      end
