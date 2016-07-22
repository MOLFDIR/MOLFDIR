*deck hrecur
      subroutine hrecur(pn,dpn,pn1,x,nn)
c
      implicit real*8 (a-h,o-z)
c
      p1 = 1.0d0
      p  = x
      dp1 = 0.0d0
      dp  = 1.0d0
      do 20 j=2,nn
        fj = (j)
        fj2 = 0.5d0*(fj-1.0d0)
        q  = x*p - fj2*p1
        dq = x*dp + p - fj2*dp1
        p1 = p
        p  = q
        dp1 = dp
        dp  = dq
   20 continue
      pn  = p
      dpn = dp
      pn1 = p1
      return
      end
