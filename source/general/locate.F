      Subroutine Locate(keyword,ilength,ifound)
      Character*10 keyword,line*80
      integer ilength,ifound
C
      ifound = 1
      rewind(5)
 10   read(5,'(A80)',END=15,ERR=15) line
      if (line(3:(ilength+2)).eq.keyword(1:ilength)) then
         backspace(5)
         goto 20
      endif
      goto 10
 15   ifound = -1
 20   Return
      End
