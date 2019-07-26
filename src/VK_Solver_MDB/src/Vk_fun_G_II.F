      subroutine FileToBuffer_dump(nfl,  fchar,irez);integer(4) nfl,irez,i,i1; character(*) fchar;i=len(fchar); i1=0;do while(.true.
     a);read(nfl,'(a)',err=20,end=21) fchar(i1+1:i-1); i1=len_trim(fchar(:i-1))+1; fchar(i1:i1)=char(10);enddo
21    fchar(i1+1:i)='';call DeleteChar_0D(fchar);i=len_trim(fchar)+1; fchar(i:i)=char(0);irez=1;RETURN
20    irez=-1;RETURN;end subroutine FileToBuffer_dump;subroutine copybuff_default(jaddr,buff);character(*) jaddr,buff;buff=jaddr;end
     e subroutine;integer(4) function iChar0Pos(input_str);character(*) input_str;iChar0Pos=index(input_str,char(0));
      end function iChar0Pos
