*deck lmnvgn
      subroutine lmnvgn(lmn1u,lmnv)
c     # generate cartesian gaussian exponent array.
c     # lmnv(*,*) = exponents of the cartesian gaussian basis functions.
c     #             s  p  d   f   g   h   i
c     #       lmn = 0  1  2   3   4   5   6
c     #    numxyz = 1, 3, 6, 10, 15  21  28      =((lmn+1)*(lmn+2))/2
      implicit logical (a-z)
      integer i, j, k, lmn, lmn1u, lmnv(3,*), ndx
c
      ndx = 0
      do 40 lmn = 0,lmn1u-1
        do 30 i = lmn,(lmn+2)/3,-1
          do 20 j = min(i,lmn-i),(lmn-i+1)/2,-1
            k = lmn - i -j
            if( i.eq.j .and. j.eq.k ) then
              ndx = ndx + 1
              lmnv(1,ndx) = i
              lmnv(2,ndx) = i
              lmnv(3,ndx) = i
            elseif( i.eq.j .and.j.gt.k ) then
              ndx = ndx + 1
              lmnv(1,ndx) = i
              lmnv(2,ndx) = i
              lmnv(3,ndx) = k
              ndx = ndx + 1
              lmnv(1,ndx) = i
              lmnv(2,ndx) = k
              lmnv(3,ndx) = i
              ndx = ndx + 1
              lmnv(1,ndx) = k
              lmnv(2,ndx) = i
              lmnv(3,ndx) = i
            elseif( i.gt.j .and. j.eq.k ) then
              ndx = ndx + 1
              lmnv(1,ndx) = i
              lmnv(2,ndx) = j
              lmnv(3,ndx) = j
              ndx = ndx + 1
              lmnv(1,ndx) = j
              lmnv(2,ndx) = i
              lmnv(3,ndx) = j
              ndx = ndx + 1
              lmnv(1,ndx) = j
              lmnv(2,ndx) = j
              lmnv(3,ndx) = i
            elseif( i.gt.j .and. j.gt.k ) then
              ndx = ndx + 1
              lmnv(1,ndx) = i
              lmnv(2,ndx) = j
              lmnv(3,ndx) = k
              ndx = ndx + 1
              lmnv(1,ndx) = i
              lmnv(2,ndx) = k
              lmnv(3,ndx) = j
              ndx = ndx + 1
              lmnv(1,ndx) = j
              lmnv(2,ndx) = i
              lmnv(3,ndx) = k
              ndx = ndx + 1
              lmnv(1,ndx) = k
              lmnv(2,ndx) = i
              lmnv(3,ndx) = j
              ndx = ndx + 1
              lmnv(1,ndx) = j
              lmnv(2,ndx) = k
              lmnv(3,ndx) = i
              ndx = ndx + 1
              lmnv(1,ndx) = k
              lmnv(2,ndx) = j
              lmnv(3,ndx) = i
            endif
   20     continue
   30   continue
   40 continue
      return
      end
