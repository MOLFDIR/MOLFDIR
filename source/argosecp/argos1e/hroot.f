*deck hroot
      subroutine hroot(x,nn,dpn,pn1,eps)
c                  improves the approximate root  x
c                in addition we also obtain
c                    dpn = derivative of h(n) at x
c                    pn1 = value of h(n-1) at x
c
      implicit real*8 (a-h,o-z)
c
c     # iter = 5 sufficient for 8-byte accuracy up to nn = 7
      do 14 iter=1,10
        call hrecur(p,dp,pn1,x,nn)
        d  = p/dp
        x  = x - d
        if( abs(d).le.eps ) go to 16
   14 continue
   16 dpn = dp
      return
      end
