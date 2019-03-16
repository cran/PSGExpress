      subroutine GetReal(n2k0,rsto, xi);integer(4) n2k0; real(8) rsto(*),xi(*);character(256) wch; integer(4) i;do i=1,n2k0;if(isnan
     l(rsto(i)))then; wch='NAN in storage buffer'; call putmess('W',0,'Buffer reading',wch); rsto(i)=0d0; endif;xi(i)=rsto(i);enddo;
      end subroutine GetReal
