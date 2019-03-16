      integer(4) function VK_Load_Basis(sizeg, pBuff);use ModCommons;interface;subroutine CopyBuff1(isize,addr,pbuff,   la,lp);use m
     lodcommons; integer(llen),value:: la,lp; integer(plen),value:: addr,pbuff; integer(4) isize;end subroutine;endinterface;integer
     l(4),value:: sizeg; integer(plen),value:: pbuff;integer(4) isave,iget,size; integer(plen) addr;common/SaveBas/ addr,isave,iget,
     lsize;size=sizeg; addr=0;if(size>0) addr=malloc(size);if(addr>0.and.size>0)then;  call CopyBuff1(size,pbuff,addr,  int(1,llen),
     lint(1,llen)); VK_Load_Basis=1; iget=1;else; addr=0; VK_Load_Basis=0; iget=0; size=0;endif;return;end function VK_Load_Basis;in
     lteger(4) function VK_Save_Basis();use ModCommons;integer(4) isave,iget,size;integer(plen) addr;common/SaveBas/ addr,isave,iget
     l,size;isave=1; VK_Save_Basis=1;end;integer(4) function VK_Get_Basis(sizeg, pBuff);use ModCommons;integer(4) sizeg; integer(ple
     ln) pBuff;integer(4) isave,iget,size; integer(plen) addr;common/SaveBas/ addr,isave,iget,size;if(size<=0.or.addr<=0)then; sizeg
     l=0; pbuff=0; VK_Get_Basis=0;else; sizeg=size; pbuff=addr; VK_Get_Basis=1;endif;end;integer(4) function VK_Release_Basis_Buffer
     l(pBuff);use ModCommons;integer(plen),value:: pbuff;integer(4) isave,iget,size; integer(plen) addr;common/SaveBas/ addr,isave,i
     lget,size;isave=0;if(pbuff>0)then; VK_Release_Basis_Buffer=1; CALL FREE(pbuff);else; VK_Release_Basis_Buffer=0;endif;
      end
