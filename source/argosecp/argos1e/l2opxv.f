*deck l2opxv
      subroutine l2opxv( lmn, v, itl, itu, lmnv, lenscr, scr, eval,
     & leig )
c
c  compute the matrix vector product, (L^2) * v, in an unnormalized
c  cartesian basis, and determine if v(*) is an eigenvector of the
c  total angular momentum operator.
c
c  input:
c  lmn = (l + m + n) where l, m, and n are the exponents of x, y, and z
c        respectively.  lmn=0 for cartesian "s" functions, 1 for
c        cartesian "p" functions, 2 for cartesian "d" functions, etc.
c  v(1:xyzdim) = input vector.  the elements are coefficients of the
c              cartesian basis functions (x**l * y**m * z**n).
c              where xyzdim = ((lmn + 1) * (lmn + 2))/2
c  lmnv(1:3,1:lmnvmx) = cartesian exponents of each basis function.
c  lenscr = scratch vector length.
c  scr(1:lenscr) = scratch work vector.  This must be at least as large
c                  as the cartesian subspace dimension.
c                  lenscr >= xyzdim
c
c  output:
c  eval = vector expectation value = <v| L^2 |v> / <v|v>
c  leig = eigenvector return code.
c       = -4 for vector norm error.
c       = -3 for lmnv(*,*) inconsistency.
c       = -2 for lenscr error.
c       = -1 if v(*) is not an eigenvector of L^2
c       = principal quantum number if v(*) is an eigenvector.
c         0 for s-type vectors, 1 for p-type vectors, etc.
c         the eigenvalue is given by (leig*(leig+1)) to within numerical
c         precision.
c  28-oct-96 account for nonorthogonality of cartesian gaussians (rmp)
c  11-jun-91 written by ron shepard with suggestions from r. m. pitzer.
c
      implicit logical(a-z)
c
c     # dummy:
c     integer lmn, lenv, lenscr, leig
      integer itl, itu, lmn, lenscr, leig
      integer lmnv(3,*)
      real*8 eval
      real*8 v(*), scr(*)
c
c     # local:
      integer l, m, n, i, j, it, jt, xyzdim, ll, mm, nn, p, q
      real*8 vi, rnorm, vnorm2
c
c     # small is used to determine if a vector is close enough to be
c     #       called an eigenvector.  this should be about 1000x the
c     #       machine epsilon to avoid sqrt() precision problems.
c
      real*8    zero,     one,     two,     fourth,        small
      parameter(zero=0d0, one=1d0, two=2d0, fourth=0.25d0, small=1d-10)
c
c     # bummer error types.
      integer    wrnerr,  nfterr,  faterr
      parameter (wrnerr=0,nfterr=1,faterr=2)
c
      integer  iodfac
      external iodfac
c
c     # the [l,m,n] basis function is assigned the internal index:
c     #  (lx*(lx+1))/2 + n + 1   with lx=(lmn-l)=(m+n)
c     # this convention is inconsistent with the rest of this program,
c     # but it is used anyway since it is convenient and since the
c     # computed results are local.
c
      integer llp1
      llp1(l) = (l * (l + 1)) / 2 + 1
c
      leig = -1
      eval = -one
c
c     # check to make sure scr(*) is large enough.
      xyzdim = llp1( lmn) + lmn
      if ( xyzdim .gt. lenscr ) then
         call bummer('l2opxv: need larger scr(*), (xyzdim-lenscr)=',
     &    (xyzdim-lenscr), wrnerr )
         leig = -2
         return
      endif
c
c     # initialize the scratch vector.
      call wzero(xyzdim,scr,1)
c
c     # compute the matrix-vector product and the vector norm.
c     # matrix elements of lop(*,*) are computed from the operator
c     # definition in cartesian space.
c
      vnorm2 = zero
      do 30 it = itl, itu
         i = it - (itl - 1)
         l = lmnv(1,it)
         m = lmnv(2,it)
         n = lmnv(3,it)
         vi = v(i)
         do 20 jt = itl, it-1
            j = jt - (itl - 1)
            vnorm2 = vnorm2 + (two * vi) * v(j) *
     &       iodfac(l,lmnv(1,jt),m,lmnv(2,jt),n,lmnv(3,jt))
   20    continue
         vnorm2 = vnorm2 + vi * vi * iodfac(l,l,m,m,n,n)
c
         if ( (l+m+n) .ne. lmn ) then
c           # inconsistent exponents in the basis function.
            call bummer('l2opxv: (l+m+n-lmn)=', (l+m+n-lmn), wrnerr )
            leig = -3
            return
         endif
c
c        # L^2 is sparse in this representation.  each vi contributes
c        # to, at most, only 7 elements in the matrix-vector product.
c
         if ( l .ge. 2 ) then
c           # include -l*(l-1) * ( [l-2,m,n+2] + [l-2,m+2,n] ) terms.
            ll = l * (l - 1)
            p = llp1( lmn - l + 2 ) + n
            scr(p) = scr(p) - vi * (ll)
            p = p + 2
            scr(p) = scr(p) - vi * (ll)
         endif
c
         if ( m .ge. 2 ) then
c           # include -m*(m-1) * ( [l,m-2,n+2] + [l+2,m-2,n] ) terms.
            mm = m * (m - 1)
            p = llp1( lmn - l ) + n + 2
            scr(p) = scr(p) - vi * (mm)
            p = llp1( lmn - l - 2 ) + n
            scr(p) = scr(p) - vi * (mm)
         endif
c
         if ( n .ge. 2 ) then
c           # include -n*(n-1) * ( [l,m+2,n-2] + [l+2,m,n-2] ) terms.
            nn = n * (n - 1)
            p = llp1( lmn - l ) + n - 2
            scr(p) = scr(p) - vi * (nn)
            p = llp1( lmn - l - 2 ) + n - 2
            scr(p) = scr(p) - vi * (nn)
         endif
c
c        # include the 2*(l*m+l*n+m*n+l+m+n)*[l,m,n] diagonal term.
c
         p = llp1( lmn - l ) + n
         scr(p) = scr(p) + vi * (2 * (l * (m + n) + m * n + l + m + n))
   30 continue
c
      if ( vnorm2 .le. small ) then
         call bummer('l2mxv: small vector norm, lnm=', lmn, wrnerr)
         leig = -4
         return
      endif
c
c     # compute the expectation value.
c
      eval  = zero
      do 50 it = itl, itu
         i = it - (itl - 1)
         l = lmnv(1,it)
         m = lmnv(2,it)
         n = lmnv(3,it)
         p = llp1( lmn - l ) + n
         do 40 jt = itl, itu
            j = jt - (itl - 1)
            eval = eval + v(j) * scr(p) *
     &                  iodfac(l,lmnv(1,jt),m,lmnv(2,jt),n,lmnv(3,jt))
   40    continue
   50 continue
      eval = eval / vnorm2
c
c     # compute the residual norm.
c     #
      rnorm = zero
c
      do 70 it = itl, itu
         i = it - (itl - 1)
         l = lmnv(1,it)
         m = lmnv(2,it)
         n = lmnv(3,it)
         p = llp1( lmn - l ) + n
         do 60 jt = itl, it-1
            j = jt - (itl - 1)
            q = llp1( lmn - lmnv(1,jt) ) + lmnv(3,jt)
            rnorm = rnorm + two * ( scr(p) - eval * v(i) ) *
     &                            ( scr(q) - eval * v(j) ) *
     &                iodfac(l,lmnv(1,jt),m,lmnv(2,jt),n,lmnv(3,jt))
   60    continue
        rnorm = rnorm + (scr(p) - eval * v(i))**2 * iodfac(l,l,m,m,n,n)
   70 continue
c
c     # normalize w.r.t. |v|=1.
      rnorm = rnorm / vnorm2
c
      rnorm = sqrt( rnorm )
      if ( rnorm .gt. small ) then
c
c        # v(*) is not an eigenvector.
c
         leig = -1
      else
c
c        # v(*) is an eigenvector.
c
c        # determine leig such that eval = leig*(leig+1)
c
c        # the following assignment should truncate to the
c        # next smaller integer value...
         leig = ( sqrt( eval + fourth ) )
      endif
c
      return
      end
