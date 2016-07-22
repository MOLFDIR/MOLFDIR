      subroutine xyzsoc( n, nn, soc, xyz)
      integer n, nn
c x, y, z components of soc integrlas : real*8 n*(n+1)/2
c ---> complex*16 n*n soc matrix
      complex*16 soc(n,n)
      real*8 xyz(nn,3)
      real*8 dzero, dhalf
      data dzero/0.0d0/, dhalf/5.0d-01/

      do 1 i=1,n
      do 1 j=1,n
 1    soc(i,j)=cmplx(dzero,dzero)

      k = 0
      do i = 1, n
      do j = 1, i
         k = k + 1
         soc(j,i)=soc(j,i)  +  cmplx(dzero,xyz(k,1))
         soc(j,i)=soc(j,i)  +  cmplx(xyz(k,2),dzero)
         soc(i,j)=soc(i,j)  +  cmplx(dzero,xyz(k,3))
      enddo
      enddo

cmckim start
      print*,'SO-ints (non-zero only)'
      do 11 i=1,n
      do 11 j=1,n
         print *, i, j, soc(i,j)
 11   continue
cmckim end
      return
      end
