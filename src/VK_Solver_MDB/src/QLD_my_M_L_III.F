      integer(4) function iofj(n,z,j);integer(4) n,z(2*n,*),j,i,set_iofj;iofj=z(3,j);return;ENTRY set_iofj(i,n,z,j)
      z(3,j)=i;set_iofj=1;
      end function iofj
