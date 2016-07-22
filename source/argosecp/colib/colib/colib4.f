*colib4.f
*colib part=4 of 9. general vector and math routines.
*version=4.1 last modified: 24-nov-90
c
c  RCS $Revision: 1.3 $  $Date: 2001/09/07 02:25:11 $
c
c  see colib1.f for version history info.
c
c  the routines in this file are not used often enough, or are not
c  sufficiently critical, to consider inlining.
c  such critical routines are collected into colib1.f.
c
*deck wset
      subroutine wset(n,factor,a,inca)
      implicit integer(a-z)
      real*8 factor, a(inca,*)
c
      do 10 i = 1, n
         a(1,i) = factor
10    continue
c
      return
      end
*deck izero
      subroutine izero(n,ia,inca)
c
c zero the elements of ia
c
      integer n, inca
      integer ia(inca,*)
c
      integer i
      do 10 i = 1, n
         ia(1,i) = 0
10    continue
c
      return
      end
*deck iset
      subroutine iset(n,ival,ia,inca)
c
c set each element of ia to ival
c
      integer n, ival, inca
      integer ia(inca,*)
c
      integer i
c
      do 10 i = 1, n
         ia(1,i) = ival
10    continue
      return
      end
*deck icopy
      subroutine  icopy(n,ivx,incx,ivy,incy)
c
c  blas-style integer vector copy.
c
      implicit integer(a-z)
      integer ivx(*),ivy(*)
      integer i,incx,incy,ix,iy,n
c
      if(n.le.0)return
      if(incx.eq.1.and.incy.eq.1)then
c
c        code for both increments equal to 1
c
         do 30 i = 1,n
            ivy(i) = ivx(i)
30       continue
c
      else
c
c        code for unequal increments or equal increments
c          not equal to 1
c
         ix = 1
         iy = 1
         if(incx.lt.0)ix = (-n+1)*incx + 1
         if(incy.lt.0)iy = (-n+1)*incy + 1
         do 10 i = 1, n
            ivy(iy) = ivx(ix)
            ix = ix + incx
            iy = iy + incy
10       continue
c
      endif
c
      return
      end
*deck inzero
c *** this routine is incremental ***
      subroutine inzero( len, x, indexx )
c
c  this subroutine will zero indexed locations of a vector.
c
c     len       : length of index vector. (len>0)
c     x(*)      : vector.
c     indexx(*) : list of locations to zero.
c                 the elements of indexx(*) must be distinct.
c
c  30-nov-90 written by eric stahlberg.
c
      integer len, indexx(len)
      real*8 x(*)
c
      integer i
      real*8    zero
      parameter(zero=0.0d0)
c
cdir$  ivdep
c$doit ivdep
cvd$   permutation (indexx)
*vocl loop,novrec
c
      do 10 i = 1, len
         x(indexx(i)) = zero
10    continue
c
      return
      end
*deck inaxpy
c *** this routine is incremental ***
      subroutine inaxpy( len, a,   x, indexx,   y, indexy )
c
c  this subroutine computes y = a*x + y for indexed locations.
c
c     len       : length of index vector. (len>0)
c     a         : scalar value to multiply vector x(*).
c     x(*)      : vector x(*).
c     y(*)      : vector y(*).
c     indexx(*) : list of x locations to use in computation.
c                 the elements of indexx(*) must be distinct.
c     indexy(*) : list of y locations to use in computation.
c                 the elements of indexy(*) must be distinct.
c
c  30-nov-90 written by eric stahlberg.
c
      integer len, indexx(len), indexy(len)
      real*8  a, x(*), y(*)
c
      integer i
c
cdir$  ivdep
c$doit ivdep
cvd$   permutation (indexx, indexy)
*vocl loop,novrec
c
      do 10 i = 1 , len
         y(indexy(i)) = y(indexy(i)) + a * x(indexx(i))
10    continue
c
      return
      end
*deck indot
c *** this routine is incremental ***
      function indot( len,   x, indexx,   y, indexy )
c
c  this function computes the dot product of 2 indexed vectors.
c
c     len       : length of in index vectors. (len>0)
c     x(*)      : x vector.
c     y(*)      : y vector.
c     indexx(*) : list of x locations.
c                 the elements of indexx(*) must be distinct.
c     indexy(*) : list of y locations.
c                 the elements of indexy(*) must be distinct.
c
c  30-nov-90 written by eric stahlberg.
c
      real*8 indot
      integer len, indexx(len), indexy(len)
      real*8 x(*), y(*)
c
      integer i
      real*8    zero,      dotpr
      parameter(zero=0.0d0)

c
      dotpr = zero
c
cdir$  ivdep
c$doit ivdep
cvd$   permutation (indexx, indexy)
*vocl loop,novrec
c
      do 10 i = 1, len
         dotpr = dotpr + x(indexx(i)) * y(indexy(i))
10    continue
c
      indot = dotpr
c
      return
      end
*deck ortmgs
c *** this routine is incremental ***
      subroutine ortmgs( nur, nuc, u, isflg, s, su, tfact, ierr )
c
c  use the modified schmidt procedure to produce vectors that
c  are orthonormal with respect to a metric matrix.
c
c  input:
c  nur  =  dimension of s(*) and number of rows in u(*).
c  nuc  =  number of vectors (columns) in u(*).
c  u(*) =  initial input vectors.
c  isflg = metric matrix flag.
c        = 0 don't use s(*) or su(*). assumed metric is a unit matrix.
c        = 1 use s(*) and su(*) to determine vector overlaps.
c  s(*) =  basis overlap matrix (metric) stored lower-triangular packed.
c
c  output:
c  s(*) =  unmodified metric matrix.
c  u(*) =  modified vectors such that u(transpose) * s * u = 1 (the
c          unit matrix of dimension nuc).  the transformation matrix t,
c          where u(new)=u(old)*t, is upper triangular and is not stored
c          explicitly.
c  tfact = measure of the nonorthonormality of the input vectors.
c          sqrt( sum(i) norm(t(i)-1)**2 ) where t(i) is an elementary
c          transformation matrix and t = t(1)*t(2)*...*t(i)*...
c  ierr  = 0 for normal return.
c         -1 for error in input dimensions.
c         -2 for isflg error.
c          k if the k-th vector could not be normalized.
c
c  31-oct-90 isflg added, orthos()/ortho() combined. -rls
c  12-jun-87 tfact computation added. -rls
c  01-mar-83 inline s*u replaced with spmxv() call. -rls
c  04-jul-78 written by ron shepard.
c
      implicit integer(a-z)
c
      integer nur, nuc, isflg, ierr
      real*8 u(nur,nuc), s(*), su(nur), tfact
c
      integer j, k
      real*8 term, tnorm
      real*8     zero,    one
      parameter (zero=0d0,one=1d0)
c
      real*8   ddot, dnrm2
      external ddot, dnrm2
c
      ierr=0
      tfact=zero
c
c     # check input:
c
      if ( nuc .le. 0    .or.    nuc .gt. nur ) then
         ierr=-1
         return
      endif
c
c     # sucessively normalize each vector, then subtract that normalized
c     # component from each of the remaining updated vectors.
c     #
c     # note that the classical grahm-schmidt method subtracts the
c     # components of the updated vector with each original vector.
c     # this is am important distinction with finite precision. -rls
c
      if ( isflg .eq. 1 ) then
c
c        # use s(*) such that on return, transpose(u) * s * u = 1
c
         do 140 k = 1, nuc
            call wzero( nur, su, 1 )
c
c           # form the k'th column of s*u.
c
            call spmxv( s, u(1,k), su, nur )
c
            term = ddot( nur,   u(1,k), 1,   su, 1 )
c
            if ( term .le. zero ) then
               ierr=k
               return
            endif
c
            tnorm = one / sqrt(term)
c
c           # scale the k'th vector to unit norm.
c
            call dscal( nur, tnorm, u(1,k), 1 )
c
            tfact = tfact + (tnorm-one)**2
c
c           # calculate overlap of this k'th vector with each successive
c           # j'th vector and subtract this component of the k'th vector
c           # from the j'th vector.  the dot products are scaled by
c           # tnorm since u(*,k) was scaled but not su(*).
c
            do 120 j = (k+1), nuc
               term = -tnorm * ddot( nur, u(1,j), 1, su, 1 )
               if ( term .ne. zero )
     &          call daxpy( nur, term, u(1,k), 1, u(1,j), 1 )
               tfact = tfact + term**2
120         continue
140      continue
c
      elseif ( isflg .eq. 0 ) then
c
c        # assume s(*,*) = 1. upon return transpose(u) * u = 1
c        # s(*) and su(*) are not referenced.
c
         do 240 k = 1, nuc
c
c           # normalize u(*,k).
c
            term = dnrm2( nur, u(1,k), 1 )
c
            if ( term .le. zero ) then
               ierr=k
               return
            endif
c
            tnorm = one / term
c
c           # scale the k'th vector to unit norm.
c
            call dscal( nur, tnorm, u(1,k), 1 )
c
            tfact = tfact + (tnorm-one)**2
c
c           # calculate overlap of this k'th vector with each successive
c           # j'th vector and subtract this component of the k'th vector
c           # from the j'th vector.
c
            do 220 j = (k+1), nuc
               term = -ddot( nur, u(1,j), 1, u(1,k), 1 )
               if ( term .ne. zero )
     &          call daxpy( nur, term, u(1,k), 1, u(1,j), 1 )
               tfact = tfact + term**2
220         continue
240      continue
c
      else
         ierr = -2
         return
      endif
c
      tfact = sqrt( tfact )
c
      return
      end
*deck traxd
c *** this routine is incremental ***
      function traxd( n, a, d )
c
c  compute matrix_trace( a * d ).
c
c  input:
c  n = matrix dimension.
c  a(*) = symmetric matrix stored lower-triangular packed.
c  d(*) = symmetric matrix stored lower-triangular packed.
c
c  output:
c  result = tr( a * d )
c
c  08-oct-90 (columbus day) written by ron shepard.
c
      implicit integer(a-z)
c
      integer n
      real*8 traxd
      real*8 a(*), d(*)
c
      integer pq, p, q
      real*8 tempd, tempnd
c
      real*8     zero
      parameter( zero=0d0 )
c
c     # diagonal and nondiagonal terms are accumulated separately.
c
      tempd  = a(1) * d(1)
      tempnd = zero
      pq     = 1
      do 20 p = 2, n
         do 10 q = 1, (p-1)
            tempnd = tempnd + a( pq + q ) * d( pq + q )
10       continue
         pq = pq + p
         tempd = tempd + a(pq) * d(pq)
20    continue
c
      traxd = tempd + tempnd + tempnd
c
      return
      end
*deck trsaxd
c *** this routine is incremental ***
      function trsaxd( nsym, nxpsy, a, d )
c
c  compute matrix_trace( a * d ) for symmetry blocked a(*) and d(*).
c
c  input:
c  nsym = number of symmetry blocks in the arrays a(*) and d(*).
c  nxpsy(1:nsym) = number of orbitals in each symmetry block.
c  a(*) = symmetric matrix stored lower-triangular packed by symmetry.
c  d(*) = symmetric matrix stored lower-triangular packed by symmetry.
c
c  output:
c  result = tr( a * d )
c
c  08-oct-90 (columbus day) written by ron shepard.
c
      implicit integer(a-z)
c
      integer nsym
      real*8 trsaxd
      integer nxpsy(nsym)
      real*8 a(*), d(*)
c
      integer pq, isym, n
      real*8 result
c
      real*8   traxd
      external traxd
c
      result = (0)
      pq = 1
      do 100 isym = 1, nsym
         n = nxpsy(isym)
         if ( n .ne. 0 ) then
            result = result + traxd( n, a(pq), d(pq) )
            pq = pq + ( n * ( n + 1 ) ) / 2
         endif
100   continue
c
      trsaxd = result
c
      return
      end
*deck spmxv
c *** this routine is incremental ***
      subroutine spmxv( a, b, c, n )
c
c  form the symmetric matrix-vector product, c = a*b, where a(*,*)
c  is stored lower-triangular packed.
c
c  c(i) = sum(j) a(ij)*b(j)
c
c  note: *** this routine is inefficient on some machines. ***
c  if the same a(*) matrix is used for repeated calls to this routine,
c  and if timing is cricital, then the programmer should consider
c  instead explicit expansion of a(*) to square form and the use of
c  other library routines such as mxv(), gmxv(), etc. -rls
c
c  blas version written 05-mar-85 by ron shepard.
c
      implicit integer(a-z)
c
      integer n
      real*8 a(*), b(*), c(*)
c
      integer lm, m
      real*8    zero
      parameter(zero=0d0)
c
      real*8   ddot
      external ddot

      if ( n .le. 0 ) return
c
      call wzero( n, c, 1 )
c
      c(1) = a(1) * b(1)
      lm = 2
      do 100 m = 2, n
         c(m) = ddot( m, a(lm),1, b,1 )
         if ( b(m) .ne. zero ) call daxpy( (m-1), b(m), a(lm),1, c,1 )
         lm = lm + m
100   continue
c
      return
      end
