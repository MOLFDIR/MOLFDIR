*deck stvint
      subroutine stvint(hpt,hwt)
c
c  gauss-hermite quadrature using minimum-point formula
c
      implicit real*8 (a-h,o-z)
      parameter (zero=0.0d0)
      common /stv/ xint,yint,zint,t,x0,y0,z0,xi,yi,zi,xj,yj,zj,ni,nj
      dimension hpt(*), hwt(*)
c
      xint=zero
      yint=zero
      zint=zero
      npts=(ni+nj)/2
      imax=(npts*(npts+1))/2
      imin=imax-npts+1
      do 40 i=imin,imax
        dum=hpt(i)/t
        ptx=dum+x0
        pty=dum+y0
        ptz=dum+z0
        ax=ptx-xi
        ay=pty-yi
        az=ptz-zi
        bx=ptx-xj
        by=pty-yj
        bz=ptz-zj
        dum=hwt(i)
c       # take care to avoid (0.0d0)**0
        dumx = dum
        dumy = dum
        dumz = dum
        do 20 ipwr = 1,ni-1
          dumx = dumx*ax
          dumy = dumy*ay
          dumz = dumz*az
   20   continue
        do 30 ipwr = 1,nj-1
          dumx = dumx*bx
          dumy = dumy*by
          dumz = dumz*bz
   30   continue
        xint = xint + dumx
        yint = yint + dumy
        zint = zint + dumz
c       xint=xint+dum*ax**(ni-1)*bx**(nj-1)
c       yint=yint+dum*ay**(ni-1)*by**(nj-1)
c       zint=zint+dum*az**(ni-1)*bz**(nj-1)
   40 continue
      return
      end
