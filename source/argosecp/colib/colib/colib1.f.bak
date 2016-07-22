*colib1.f
*colib part=1 of 9. inlinable vector and matrix routines.
*version=4.1 last modified: 14-may-92
c
c  RCS $Revision: 1.3 $  $Date: 2001/09/07 02:25:10 $
c
c  colib version history:
c  14-may-92 4.1b5 fujitsu code of Ross Nobes and Roger Edberg
c            merged. minor ftnchek-related cleanup. -rls
c  24-apr-92 4.1b4 rs6000 modifications. getime() removed from
c            colib9.f.  minor cleanup. -rls
c  24-oct-91 4.1b3 minor cleanup in ufvin()/ufvout(). -rls
c  17-oct-91 4.1b2 who2c() modified. -rls/g.gawboy
c  11-sep-91 4.1a13 stubs for tcgmsg routines added, pfname() added,
c            pend() added to bummer().  blas2+blas3 flags added for
c            convex. -rls/rjh/ms
c            promoted to 4.1b1 on 28-sep-91
c  03-sep-91 4.1a12 uname() calls added for Cray; getenv() calls
c            fixed on the Cray 2. -rls
c  01-sep-91 4.1a11 convex radlab code added. -john bentley/rls
c  20-jul-91 4.1a10 cray nuw check added in ulab*(). -galen gawboy/rls
c  08-jun-91 4.1a9 colib1 fps gmtxm() typo fixed. -rls
c  05-jun-91 colib2 readda() format change. -rls
c  31-may-91 4.1a6 version, includes SIFS support routines. -rls
c**********************************************************************
c  note:  this file should be processed and copied to $COLUMBUS/ for
c  source-level inlining if this is supported on the target machine.
c
c this currently applies to the following machines:
c
c cray x-mp, y-mp, cray 2 :
c    % $COLUMBUS/port crayinline colib1.f ;cp colib1.f $COLUMBUS/
c
c stardent titan :
c    % cp colib1.f $COLUMBUS/
c**********************************************************************
c
c  routines in the colib library are designated as:
c      incremental: new routine.  functionality and/or interface is
c                   subject to change.
c      (normal):    mature code.  functionality may be enhanced, but
c                   the interface is stable.
c      obsolete:    for various reasons, such routines are marked for
c                   removal.  calls to these should be eliminated in
c                   existing codes, and new codes should not use these
c                   routines.  obsolete routines are grouped together
c                   into ciudg9.f
c**********************************************************************
c
c  things-to-do:
c  * add leading dimension arguments to all g*() matrix routines.
c
c
*deck gmxm
      subroutine gmxm(a,nar, b,nbr, c,ncc, alpha)
c
c  generalized matrix-matrix product to compute c = c + alpha*(a * b)
c
c  input:
c  a(*),b(*) = input matrices.
c  nar = number of rows in a(*) and c(*).
c  nbr = number of columns of a(*) and rows of b(*).
c  ncc = number of columns of b(*) and c(*).
c        all dimensions must be > 0.
c  alpha = matrix product scale factor.
c
c  output:
c  c(*) = c(*) + alpha * (a * b ) updated matrix.
c
c  10-nov-89 written by ron shepard.
c
      implicit none
      integer nar, nbr, ncc
      real*8 a(nar,nbr), b(nbr,ncc), c(nar,ncc), alpha
c
      integer i, j, k
      real*8 bkj, sum
      real*8    zero,     one
      parameter(zero=0d0, one=1d0)
c
      real*8   ddot
      external ddot
c
*mdc*if fujitsu
*c     # 14-may-92 fujitsu code of Ross Nobes and Roger Edberg
*c     #           merged. -rls
*      integer    maxvl
*      parameter( maxvl = 512 )
*      integer imax, istart, kextra
*      real*8 b0, b1, b2, b3
*      kextra = mod( nbr, 4 )
*c
*         do 50 j = 1, ncc
*            do 40 istart = 1, nar, maxvl
*               imax = min( istart+maxvl-1, nar )
*               do 20 k = 1, kextra
*                     b0 = b(k,j)   * alpha
**vocl loop,repeat(maxvl)
*                  do 20 i = istart, imax
*                     c(i,j) = c(i,j) + a(i,k) * b0
*20             continue
*               do 30 k = kextra+1, nbr, 4
*                     b0 = b(k,j)   * alpha
*                     b1 = b(k+1,j) * alpha
*                     b2 = b(k+2,j) * alpha
*                     b3 = b(k+3,j) * alpha
**vocl loop,repeat(maxvl)
*                  do 30 i = istart, imax
*                     c(i,j) = c(i,j) + a(i,k)   * b0
*     &                               + a(i,k+1) * b1
*     &                               + a(i,k+2) * b2
*     &                               + a(i,k+3) * b3
*30             continue
*40          continue
*50       continue
*mdc*elseif blas3 cray (ibm and vector) essl
c     # interface to the library routine.
      call dgemm('n','n',nar,ncc,nbr, alpha, a,nar, b,nbr, one, c,nar)
*mdc*elseif alliant and fx2800
*c     # 13-mar-91 -rls
*c     # matmul() is crippled and only runs at fortran speed,
*c     # so use ddot/dgemm instead. ddot runs at .04 to 10 mflops,
*c     # dgemm runs at .04 to 23 mflops with the switchover at
*c     # 2.2 mflops at nbr=8 for square matrices.
*c
*      if ( nar*nbr .gt. 64 ) then
*         call dgemm('n','n',nar,ncc,nbr,
*     &    alpha, a,nar, b,nbr, one, c,nar)
*      else
*         do 50 j = 1, ncc
*            do 40 i = 1, nar
*               c(i,j) = c(i,j)
*     &          + alpha * ddot( nbr,    a(i,1), nar,    b(1,j), 1 )
*40          continue
*50       continue
*      endif
*mdc*elseif alliant
*      intrinsic matmul
*      c = c + alpha * matmul( a, b )
*mdc*elseif titan
*c     16-feb-90 tuned for 16Mhz R2000 single processor. -rls
*      if ( nar*nbr .ge. 400 .or. nar*nbr*ncc .ge. 4096 ) then
*c        # interface to the library gmxma().
*         call gmxma(a,1,nar, b,1,nbr, c,1,nar, nar,nbr,ncc, alpha,one)
*      else
*c$doit   vbest
*         do 50 i = 1, nar
*            do 40 j = 1, ncc
*               do 30 k = 1, nbr
*                  c(i,j) = c(i,j) + alpha * a(i,k) * b(k,j)
*30             continue
*40          continue
*50       continue
*      endif
*mdc*elseif sun
*c  23-dec-89 dot-product loop order. -rls
*      do 50 j = 1, ncc
*         do 40 i = 1, nar
*            sum = zero
*            do 30 k = 1, nbr
*               sum = sum + a(i,k) * b(k,j)
*30          continue
*            c(i,j) = c(i,j) + alpha * sum
*40       continue
*50    continue
*mdc*elseif fps
*c     # 06-mar-91 rgmmul()/saxpy loop order. -rls
*      if ( alpha .eq. (1) ) then
*         call rgmmul( 2, nar,nbr,ncc, a,1,nar, b,1,nbr, c,1,nar )
*      elseif ( alpha .eq. (-1) ) then
*         call rgmmul( 3, nar,nbr,ncc, a,1,nar, b,1,nbr, c,1,nar )
*      else
*         do 50 j = 1, ncc
*            do 40 k = 1, nbr
*               bkj = b(k,j) * alpha
*               if ( bkj .ne. zero ) then
*                  do 30 i = 1, nar
*                     c(i,j) = c(i,j) + a(i,k) * bkj
*30                continue
*               endif
*40          continue
*50       continue
*      endif
*mdc*else
*c
*c  all-fortran version...
*c  note: *** this code should not be modified ***
*c  a saxpy inner loop is used to exploit sparseness in the matrix
*c  b(*), and this results in sequential access to a(*), b(*),
*c  and c(*). -rls
*c
*      do 50 j = 1, ncc
*         do 40 k = 1, nbr
*            bkj = b(k,j) * alpha
*            if ( bkj .ne. zero ) then
*               do 30 i = 1, nar
*                  c(i,j) = c(i,j) + a(i,k) * bkj
*30             continue
*            endif
*40       continue
*50    continue
*mdc*endif
      return
      end
*deck gmtxm
      subroutine gmtxm(a,natr, b,nbr, c,ncc, alpha)
c
c  to compute c = c + alpha*( a(transpose) * b )
c
c  input:
c  a(*),b(*) = input matrices.
c  natr = number of rows in a(transopose)(*) and c(*).
c  nbr = number of columns of a(transpose)(*) and rows of b(*).
c  ncc = number of columns of b(*) and c(*).
c        all dimensions must be > 0.
c  alpha = matrix product scale factor.
c
c  output:
c  c(*) = c(*) + alpha*( a(t) * b ) updated  matrix.
c
c  10-nov-89 written by ron shepard.
c
      implicit none
      integer natr, nbr, ncc
      real*8 a(nbr,natr), b(nbr,ncc), c(natr,ncc), alpha
c
      integer i, j, k
      real*8 sum, bkj
      real*8    zero,     one
      parameter(zero=0d0, one=1d0)
c
      real*8   ddot
      external ddot
c
*mdc*if fujitsu
*c     # 14-may-92 fujitsu code of Ross Nobes and Roger Edberg
*c     #           merged. -rls
*      integer    maxvl
*      parameter( maxvl = 512 )
*      integer imax, istart, kextra
*      real*8  b0, b1, b2, b3
*      kextra = mod( nbr, 4 )
*c
*         do 50 istart = 1, natr, maxvl
*            imax = min( istart+maxvl-1, natr )
*            do 20 k = 1, kextra
*               do 20 j = 1, ncc
*                  b0 = b(k,j) * alpha
**vocl loop,repeat(maxvl)
*                  do 20 i = istart, imax
*                     c(i,j) = c(i,j) + b0 * a(k,i)
*20          continue
*           do 30 k = kextra+1, nbr, 4
*              do 30 j = 1, ncc
*                 b0 = b(k,j)   * alpha
*                 b1 = b(k+1,j) * alpha
*                 b2 = b(k+2,j) * alpha
*                 b3 = b(k+3,j) * alpha
**vocl loop,repeat(maxvl)
*                 do 30 i = istart, imax
*                    c(i,j) = c(i,j) + b0 * a(k,i)
*     &                              + b1 * a(k+1,i)
*     &                              + b2 * a(k+2,i)
*     &                              + b3 * a(k+3,i)
*30         continue
*50       continue
*c
*mdc*elseif blas3 cray (ibm and vector) essl
c     # interface to the library routine.
      call dgemm('t','n',natr,ncc,nbr,
     & alpha, a,nbr, b,nbr, one, c,natr)
*mdc*elseif alliant and fx2800
*c     # 13-mar-91 -rls
*c     # matmul() is crippled and only runs at fortran speed,
*c     # so use ddot/dgemm instead. ddot runs at .04 to 10 mflops,
*c     # dgemm runs at .04 to 29 mflops with the switchover at
*c     # 6.6 mflops at nbr=16 for square matrices.
*c
*      if ( natr*nbr .gt. 240 ) then
*         call dgemm('t','n', natr,ncc,nbr,
*     &    alpha, a,nbr, b,nbr, one, c,natr)
*      else
*         do 50 j = 1, ncc
*            do 40 i = 1, natr
*               c(i,j) = c(i,j)
*     &          + alpha * ddot( nbr,    a(1,i), 1,    b(1,j), 1 )
*40          continue
*50       continue
*      endif
*mdc*elseif alliant
*      intrinsic transpose, matmul
*      c = c + alpha * matmul( transpose(a), b )
*mdc*elseif titan
*c     16-feb-90 tuned for 16Mhz R2000 single processor. -rls
*      if ( ncc*natr*nbr .ge. 512 ) then
*c        # interface to library gmxma().
*         call gmxma(a,nbr,1, b,1,nbr, c,1,natr, natr,nbr,ncc,
*     &    alpha,one)
*      else
*         do 30 j = 1, ncc
*            do 20 i = 1, natr
*               sum = zero
*               do 10 k = 1, nbr
*                  sum = sum + a(k,i) * b(k,j)
*10             continue
*               c(i,j) = c(i,j) + sum * alpha
*20          continue
*30       continue
*      endif
*mdc*elseif fps
*c     # 08-jun-91 typo fixed in rgmmul() calls. -rls
*c     # 06-mar-91 rgmmul()/saxpy loop order. -rls
*      if ( alpha .eq. (1) ) then
*         call rgmmul( 2, natr,nbr,ncc, a,nbr,1, b,1,nbr, c,1,natr )
*      elseif ( alpha .eq. (-1) ) then
*         call rgmmul( 3, natr,nbr,ncc, a,nbr,1, b,1,nbr, c,1,natr )
*      else
*         do 30 k = 1, nbr
*            do 20 j = 1, ncc
*               bkj = b(k,j) * alpha
*               if ( bkj .ne. zero ) then
*                  do 10 i = 1, natr
*                     c(i,j) = c(i,j) + a(k,i) * bkj
*10                continue
*               endif
*20          continue
*30       continue
*      endif
*mdc*else
*c
*c  all fortran version.
*c
*c  note: *** this code should not be modified ***
*c  use dot-product loop ordering for sequential memory accesses.
*c  sparsness is not exploited.  none of the saxpy inner-loop orderings
*c  result in sequential memory access.  if saxpy loops are essential
*c  for some machine, then the matrices should be partitioned, and
*c  intermediate results accumulated into a sequentially accessed
*c  intermediate array and periodically dumped to c(*). -rls
*c
*      do 30 j = 1, ncc
*         do 20 i = 1, natr
*            sum = zero
*            do 10 k = 1, nbr
*               sum = sum + a(k,i) * b(k,j)
*10          continue
*            c(i,j) = c(i,j) + alpha * sum
*20       continue
*30    continue
*mdc*endif
      return
      end
*deck gmxmt
      subroutine gmxmt(a,nar, b,nbtr, c,ncc, alpha)
c
c  to compute c = c + alpha * ( a * b(transpose) )
c
c  input:
c  a(*),b(*) = input matrices.
c  nar = number of rows in a(*) and c(*).
c  nbtr = number of columns of a(*) and rows of b(transpose)(*).
c  ncc = number of columns of b(transpose)(*) and c(*).
c        all dimensions must be > 0.
c  alpha = matrix product scale factor.
c
c  output:
c  c(*) = c(*) + alpha*(a * b(transpose)) updated matrix.
c
c  10-nov-89 written by ron shepard.
c
      implicit none
      integer nar, nbtr, ncc
      real*8 a(nar,nbtr), b(ncc,nbtr), c(nar,ncc), alpha
c
      integer i, j, k
      real*8 bjk, sum
      real*8    zero,     one
      parameter(zero=0d0, one=1d0)
c
      real*8   ddot
      external ddot
c
*mdc*if fujitsu
*c     # 14-may-92 fujitsu code of Ross Nobes and Roger Edberg
*c     #           merged. -rls
*      integer    maxvl
*      parameter (maxvl = 512)
*      integer imax, istart, kextra
*      real*8 b0, b1, b2, b3
*      kextra = mod( nbtr, 4 )
*c
*         do 50 j = 1, ncc
*            do 40 istart = 1, nar, maxvl
*               imax = min( istart+maxvl-1, nar )
*               do 20 k = 1, kextra
*                     b0 = b(j,k)   * alpha
**vocl loop,repeat(maxvl)
*                  do 20 i = istart, imax
*                     c(i,j) = c(i,j) + a(i,k) * b0
*20             continue
*               do 30 k = kextra+1, nbtr, 4
*                     b0 = b(j,k)   * alpha
*                     b1 = b(j,k+1) * alpha
*                     b2 = b(j,k+2) * alpha
*                     b3 = b(j,k+3) * alpha
**vocl loop,repeat(maxvl)
*                  do 30 i = istart, imax
*                     c(i,j) = c(i,j) + a(i,k)   * b0
*     &                               + a(i,k+1) * b1
*     &                               + a(i,k+2) * b2
*     &                               + a(i,k+3) * b3
*30             continue
*40          continue
*50       continue
*c
*mdc*elseif blas3 cray (ibm and vector) essl
c     # interface to the library routine.
      call dgemm('n','t',nar,ncc,nbtr, alpha, a,nar, b,ncc, one, c,nar)
*mdc*elseif alliant and fx2800
*c     # 13-mar-91 -rls
*c     # matmul() is crippled and only runs at fortran speed,
*c     # so use ddot/dgemm instead. ddot runs at .04 to 10 mflops,
*c     # dgemm runs at .04 to 21 mflops with the switchover at
*c     # 2.6 mflops at nbtr=8 for square matrices.
*c
*      if ( nar*nbtr .gt. 64 ) then
*         call dgemm('n','t',nar,ncc,nbtr,
*     &    alpha, a,nar, b,ncc, one, c,nar)
*      else
*         do 50 j = 1, ncc
*            do 40 i = 1, nar
*               c(i,j) = c(i,j)
*     &          + alpha * ddot( nbtr,    a(i,1), nar,    b(j,1), ncc )
*40          continue
*50       continue
*      endif
*mdc*elseif alliant
*      intrinsic transpose, matmul
*      c = c + alpha * matmul( a, transpose(b) )
*mdc*elseif titan
*c     16-feb-90 tuned for 16Mhz R2000 single processor. -rls
*      if ( ncc*nar*nbtr .ge. 512 ) then
*c        # interface to library gmxma().
*         call gmxma(a,1,nar, b,ncc,1, c,1,nar, nar,nbtr,ncc, alpha,one)
*      else
*c$doit   vbest
*         do 50 j = 1, ncc
*            do 40 i = 1, nar
*               sum = zero
*               do 30 k = 1, nbtr
*                  sum = sum + a(i,k) * b(j,k)
*30             continue
*               c(i,j) = c(i,j) + sum * alpha
*40          continue
*50       continue
*      endif
*mdc*elseif sun
*c
*c  23-dec-89 dot-product loop order. -rls
*c
*      do 50 j = 1, ncc
*         do 40 i = 1, nar
*            sum = zero
*            do 30 k = 1, nbtr
*               sum = sum + a(i,k) * b(j,k)
*30          continue
*            c(i,j) = c(i,j) + alpha * sum
*40       continue
*50    continue
*mdc*elseif fps
*c     # 06-mar-91 rgmmul()/saxpy loop order. -rls
*      if ( alpha .eq. (1) ) then
*         call rgmmul( 2, nar,nbtr,ncc, a,1,nar, b,ncc,1, c,1,nar )
*      elseif ( alpha .eq. (-1) ) then
*         call rgmmul( 3, nar,nbtr,ncc, a,1,nar, b,ncc,1, c,1,nar )
*      else
*         do 50 k = 1, nbtr
*            do 40 j = 1, ncc
*               bjk = b(j,k) * alpha
*               if ( bjk .ne. zero ) then
*                  do 30 i = 1, nar
*                     c(i,j) = c(i,j) + a(i,k) * bjk
*30                continue
*               endif
*40          continue
*50       continue
*      endif
*mdc*else
*c
*c  all fortran version.
*c  note: *** this code should not be modified ***
*c  saxpy inner loop allows exploitation of sparseness in b(*) and
*c  results in sequential memory accesses. -rls
*c
*      do 50 k = 1, nbtr
*         do 40 j = 1, ncc
*            bjk = b(j,k) * alpha
*            if ( bjk .ne. zero ) then
*               do 30 i = 1, nar
*                  c(i,j) = c(i,j) + a(i,k) * bjk
*30             continue
*            endif
*40       continue
*50    continue
*mdc*endif
      return
      end
*deck gmtxmt
      subroutine gmtxmt(a,natr, b,nbtr, c,ncc, alpha)
c
c  to compute c = c + alpha * ( a(transpose) * b(transpose) )
c
c  input:
c  a(*),b(*) = input matrices.
c  natr = number of rows in a(transpose)(*) and c(*).
c  nbtr = number of columns of a(transpose)(*) and
c         rows of b(transpose)(*).
c  ncc = number of columns of b(transpose)(*) and c(*).
c        all dimensions must be > 0.
c  alpha = matrix product scale factor.
c
c  output:
c  c(*) = c(*) + alpha*(a(transpose) * b(transpose)) updated matrix.
c
c  10-nov-89 written by ron shepard.
c
      implicit none
      integer natr, nbtr, ncc
      real*8 a(nbtr,natr), b(ncc,nbtr), c(natr,ncc), alpha
c
      integer i, j, k
      real*8 aki, sum
      real*8    zero,     one
      parameter(zero=0d0, one=1d0)
c
      real*8   ddot
      external ddot
c
*mdc*if fujitsu
*c     # 14-may-92 fujitsu code of Ross Nobes and Roger Edberg
*c     #           merged. -rls
*      integer    maxvl
*      parameter( maxvl = 512 )
*      integer imax, istart, kextra
*      real*8  b0, b1, b2, b3
*      kextra = mod( nbtr, 4 )
*c
*         do 50 istart = 1, natr, maxvl
*            imax = min( istart+maxvl-1, natr )
*            do 20 k = 1, kextra
*               do 20 j = 1, ncc
*                  b0 = b(j,k) * alpha
**vocl loop,repeat(maxvl)
*                  do 20 i = istart, imax
*                     c(i,j) = c(i,j) + b0 * a(k,i)
*20          continue
*           do 30 k = kextra+1, nbtr, 4
*              do 30 j = 1, ncc
*                 b0 = b(j,k)   * alpha
*                 b1 = b(j,k+1) * alpha
*                 b2 = b(j,k+2) * alpha
*                 b3 = b(j,k+3) * alpha
**vocl loop,repeat(maxvl)
*                 do 30 i = istart, imax
*                    c(i,j) = c(i,j) + b0 * a(k,i)
*     &                              + b1 * a(k+1,i)
*     &                              + b2 * a(k+2,i)
*     &                              + b3 * a(k+3,i)
*30         continue
*50       continue
*c
*mdc*elseif blas3 cray (ibm and vector) essl
c     # interface to the library routine.
      call dgemm('t','t', natr,ncc,nbtr,
     & alpha, a,nbtr, b,ncc, one, c,natr)
*mdc*elseif alliant and fx2800
*c     # 13-mar-91 -rls
*c     # matmul() is crippled and only runs at fortran speed,
*c     # so use ddot/dgemm instead. ddot runs at .04 to 10 mflops,
*c     # dgemm runs at .04 to 29 mflops with the switchover at
*c     # 4.9 mflops at nbtr=8 for square matrices.
*c
*      if ( natr*nbtr .gt. 64 ) then
*         call dgemm('t','t', natr,ncc,nbtr,
*     &    alpha, a,nbtr, b,ncc, one, c,natr)
*      else
*         do 50 j = 1, ncc
*            do 40 i = 1, natr
*               c(i,j) = c(i,j)
*     &          + alpha * ddot( nbtr,    a(1,i), 1,    b(j,1), ncc )
*40          continue
*50       continue
*      endif
*mdc*elseif alliant
*      intrinsic transpose, matmul
*      c = c + alpha * matmul( transpose(a), transpose(b) )
*mdc*elseif titan
*c     16-feb-90 tuned for 16Mhz R2000 single processor. -rls
*      if ( ncc*nbtr.ge. 400 .or. ncc*natr*nbtr .ge. 4096 ) then
*c        # interface to library gmxma().
*         call gmxma(a,nbtr,1, b,ncc,1, c,1,natr, natr,nbtr,ncc,
*     &    alpha,one)
*      else
*c$doit   vbest
*         do 50 j = 1, ncc
*            do 40 i = 1, natr
*               sum = zero
*               do 30 k = 1, nbtr
*                  sum = sum + a(k,i) * b(j,k)
*30             continue
*               c(i,j) = c(i,j) + alpha * sum
*40          continue
*50       continue
*      endif
*mdc*elseif sun
*c
*c  23-dec-89 dot-product loop order. -rls
*c
*      do 50 j = 1, ncc
*         do 40 i = 1, natr
*            sum = zero
*            do 30 k = 1, nbtr
*               sum = sum + a(k,i) * b(j,k)
*30          continue
*            c(i,j) = c(i,j) + alpha * sum
*40       continue
*50    continue
*mdc*elseif fps
*c     # 06-mar-91 rgmmul()/saxpy loop order. -rls
*      if ( alpha .eq. (1) ) then
*         call rgmmul( 2, natr,nbtr,ncc, a,nbtr,1, b,ncc,1, c,1,natr )
*      elseif ( alpha .eq. (-1) ) then
*         call rgmmul( 3, natr,nbtr,ncc, a,nbtr,1, b,ncc,1, c,1,natr )
*      else
*         do 50 i = 1, natr
*            do 40 k = 1, nbtr
*               aki = a(k,i) * alpha
*               if ( aki .ne. zero ) then
*                  do 30 j = 1, ncc
*                     c(i,j) = c(i,j) + aki * b(j,k)
*30                continue
*               endif
*40          continue
*50       continue
*      endif
*mdc*else
*c
*c  all fortran version.
*c  note: *** this code should not be modified ***
*c  for this matrix operation, it is not possible to have sequential
*c  memory accesses to all three matrices.  if this is important, then
*c  this routine should partition the output matrix, and accumulate the
*c  results for each partition into a sequentially accessed intermediate
*c  array, dumping the results to c(*) as necessary.  note also that
*c  c(transpose)=b*a.  the results could also be computed using mxm()
*c  and explicitly transposed (if necessary) by the calling routine.
*c  -rls
*c
*      do 50 i = 1, natr
*         do 40 k = 1, nbtr
*            aki = a(k,i) * alpha
*            if ( aki .ne. zero ) then
*               do 30 j = 1, ncc
*                  c(i,j) = c(i,j) + aki * b(j,k)
*30             continue
*            endif
*40       continue
*50    continue
*mdc*endif
      return
      end
*deck gmxma
c
c  note:  this routine is so general that it usually cannot be
c  implemented using vendor-supplied library calls.
c  consequently, its use is discouraged in cases where efficiency
c  is of primary concern.  the user's algorithm should be restructured
c  to use one of the more efficient routines instead. -rls
c
*mdc*if titan
*c  use library version
*mdc*elseif sun rs6000
c     # 06-mar-91 dot-product code moved from mxma(). -rls
      subroutine gmxma(a,iac,iar, b,ibc,ibr, c,icc,icr, nar,nac,nbc,
     & alpha,beta)
c
      implicit none
c
      integer iac, iar, ibc, ibr, icc, icr, nar, nac, nbc
      real*8 a(*), b(*), c(*), alpha, beta
c
      integer i1j, k1j, j, ij, ik1, i, kj, ik, k
      real*8     zero,     sum
      parameter (zero=0d0)
c
      i1j = 1
      k1j = 1
      do 50 j = 1, nbc
         ij = i1j
         ik1 = 1
         do 40 i = 1, nar
            sum = zero
            kj = k1j
            ik = ik1
            do 30 k = 1, nac
               sum = sum + a(ik) * b(kj)
               kj = kj + ibc
               ik = ik + iar
30          continue
            c(ij) = beta * c(ij) + alpha * sum
            ij = ij + icc
            ik1 = ik1 + iac
40       continue
         i1j = i1j + icr
         k1j = k1j + ibr
50    continue
      return
      end
*mdc*else
*c
*c  standard fortran version.
*c  note: *** this code should not be modified ***
*      subroutine gmxma(a,iac,iar, b,ibc,ibr, c,icc,icr, nar,nac,nbc,
*     & alpha,beta)
*c
*c  fortran version of generalized mxma to calculate
*c     c = alpha * a * b + beta * c
*c
*c  arguments:
*c    a    = first input matrix with effective dimensions a(nar,nac).
*c    iac  = spacing between consecutive elements in a column of a.
*c    iar  = spacing between consecutive elements in a row of a.
*c    b    = second input matrix with effective dimensions b(nac,nbc).
*c    ibc  = spacing between consecutive elements in a column of b.
*c    ibr  = spacing between consecutive elements in a row of b.
*c    c    = output matrix with effective dimensions c(nar,nbc).
*c    icc  = spacing between consecutive elements in a column of c.
*c    icr  = spacing between consecutive elements in a row of c.
*c    nar  = number of rows in a and c.
*c    nac  = number of columns in a and rows in b.
*c    nbc  = number of columns in b and c.
*c    alpha = matrix-matrix product scale factor.
*c    beta  = output matrix scale factor.
*c
*c  see mxma() for further details.
*c
*c  written 04-jan-85 by ron shepard.
*c
*      implicit none
*c
*      integer iac, iar, ibc, ibr, icc, icr, nar, nac, nbc
*      real*8 a(*), b(*), c(*), alpha, beta
*c
*      integer i1j, j, ij, i, k1j, i1k, kj, k, ik
*      real*8     zero,     bkj
*      parameter (zero=0d0)
*c
*c  initialize the output matrix c(*).
*c
*      i1j = 1
*      do 200 j = 1, nbc
*         ij = i1j
*         do 100 i = 1, nar
*            c(ij) = c(ij) * beta
*            ij = ij + icc
*100      continue
*         i1j = i1j + icr
*200   continue
*c
*c  this version uses an outer product algorithm to take advantage of
*c  zeros in the matrix b(*); i.e. the innermost do-loop is a saxpy.
*c  the loop ordering in this version results in sequential access of
*c  all the matrices for iac=ibc=icc=1 cases.
*c
*      i1j = 1
*      k1j = 1
*      do 500 j = 1, nbc
*         i1k = 1
*         kj = k1j
*         do 400 k = 1, nac
*            bkj = b(kj) * alpha
*            kj = kj + ibc
*            if ( bkj .ne. zero ) then
*               ij = i1j
*               ik = i1k
*               do 300 i = 1, nar
*                  c(ij) = c(ij) + a(ik) * bkj
*                  ij = ij + icc
*                  ik = ik + iac
*300            continue
*            endif
*            i1k = i1k + iar
*400      continue
*         i1j = i1j + icr
*         k1j = k1j + ibr
*500   continue
*c
*      return
*      end
*mdc*endif
*deck gmxv
      subroutine gmxv(a,nar, b,nbr, c, alpha)
c
c  computes the matrix-vector product:  c = c + alpha * (a * b).
c
c  input:
c  a(*) = input matrix.
c  nar = number of rows in a(*) and c(*) (must be > 0).
c  b(*) = input vector.
c  nbr = number of rows in b(*) and columns of a(*) (must be > 0).
c  alpha = matrix-vector product scale factor.
c
c  output:
c  c(*) = updated output vector.
c
c  10-nov-89 written by ron shepard.
c
      implicit none
      integer nar, nbr
      real*8 a(nar,nbr), b(nbr), c(nar), alpha
c
      integer i, j
      real*8 bj, sum
      real*8    zero,     one
      parameter(zero=0d0, one=1d0)
c
      real*8   ddot
      external ddot
c
*mdc*if fujitsu
*c     # 14-may-92 fujitsu code of Ross Nobes and Roger Edberg
*c     #           merged. -rls
*      integer    maxvl
*      parameter( maxvl = 512 )
*      integer k, imax, istart, kextra
*      real*8 b0, b1, b2, b3
*      kextra = mod( nbr, 4 )
*c
*            do 50 istart = 1, nar, maxvl
*               imax = min( istart+maxvl-1, nar )
*               do 20 k = 1, kextra
*                     b0 = b(k)   * alpha
**vocl loop,repeat(maxvl)
*                  do 20 i = istart, imax
*                     c(i) = c(i) + a(i,k) * b0
*20             continue
*               do 30 k = kextra+1, nbr, 4
*                     b0 = b(k)   * alpha
*                     b1 = b(k+1) * alpha
*                     b2 = b(k+2) * alpha
*                     b3 = b(k+3) * alpha
**vocl loop,repeat(maxvl)
*                  do 30 i = istart, imax
*                     c(i) = c(i) + a(i,k)   * b0
*     &                           + a(i,k+1) * b1
*     &                           + a(i,k+2) * b2
*     &                           + a(i,k+3) * b3
*30             continue
*50          continue
*mdc*elseif blas2 cray
         call dgemv('n',nar,nbr, alpha, a,nar, b,1, one, c,1)
*mdc*elseif (ibm and vector) essl
*         call dgemx(nar,nbr,alpha,a,nar,b,1,c,1)
*mdc*elseif alliant and fx2800
*c     # 13-mar-91 -rls
*c     # matmul() is crippled and only runs at fortran speed,
*c     # so use ddot/dgemv instead. ddot runs at .04 to 10 mflops,
*c     # dgemv runs at .04 to 17 mflops with the switchover at
*c     # 6.2 mflops at nbr=24 for square matrices.
*c
*      if ( nar*nbr .gt. 576 ) then
*         call dgemv('n', nar,nbr, alpha, a,nar, b,1, one, c,1)
*      else
*         do 40 i = 1, nar
*            c(i) = c(i) + alpha * ddot( nbr,   a(i,1), nar,   b, 1 )
*40       continue
*      endif
*mdc*elseif alliant
*      intrinsic matmul
*      c = c + alpha * matmul( a, b )
*mdc*elseif titan
*c     7-dec-89 tuned for 16Mhz R2000 single processor. -rls
*      if ( nar*nbr .ge. 400 ) then
*c        # interface to library gmxma().
*         call gmxma(a,1,nar, b,1,1, c,1,1, nar,nbr,1, alpha,one)
*      else
*c$doit   vbest
*         do 30 i = 1, nar
*            do 20 j = 1, nbr
*               c(i) = c(i) + alpha * a(i,j) * b(j)
*20          continue
*30       continue
*      endif
*mdc*elseif sun
*c
*c  23-dec-89 dot-product loop order. -rls
*c
*      do 30 i = 1, nar
*         sum = zero
*         do 20 j = 1, nbr
*            sum = sum + a(i,j) * b(j)
*20       continue
*         c(i) = c(i) + alpha * sum
*30    continue
*mdc*elseif fps
*c     # 06-mar-91 rgmmul()/saxpy loop order. -rls
*      if ( alpha .eq. (1) ) then
*         call rgmmul( 2, nar,nbr,1, a,1,nar, b,1,1, c,1,1 )
*      elseif ( alpha .eq. (-1) ) then
*         call rgmmul( 3, nar,nbr,1, a,1,nar, b,1,1, c,1,1 )
*      else
*         do 30 j = 1, nbr
*            bj = b(j) * alpha
*            if ( bj .ne. zero ) then
*               do 20 i = 1, nar
*                  c(i) = c(i) + a(i,j) * bj
*20             continue
*            endif
*30       continue
*      endif
*mdc*else
*c
*c  fortran version...
*c  note: *** this code should not be modified ***
*c  a saxpy inner loop is used to exploit sparseness in b(*), and
*c  this loop ordering also results in sequential access to a(*)
*c  and c(*). -rls
*c
*      do 30 j = 1, nbr
*         bj = b(j) * alpha
*         if ( bj .ne. zero ) then
*            do 20 i = 1, nar
*               c(i) = c(i) + a(i,j) * bj
*20          continue
*         endif
*30    continue
*mdc*endif
      return
      end
*deck gmtxv
      subroutine gmtxv(a,natr, b,nbr, c, alpha)
c
c  compute the matrix(transpose)-vector product:
c         c = c + alpha * (a(transpose) * b).
c
c  input:
c  a(*) = input matrix.
c  natr = number of rows in a(transpose) and c(*) (must be > 0).
c  b(*) = input vector.
c  nbr = number of rows in b(*) and columns of a(transpose)
c         (must be > 0).
c  alpha = matrix-vector product scale factor.
c
c  10-nov-89 written by ron shepard.
c
      implicit none
      integer natr, nbr
      real*8 a(nbr,natr), b(nbr), c(natr), alpha
c
      integer i, j
      real*8 sum, bj
      real*8    zero,     one
      parameter(zero=0d0, one=1d0)
c
      real*8   ddot
      external ddot
c
*mdc*if blas2 cray
      call dgemv('t',nbr,natr, alpha, a,nbr, b,1, one, c,1)
*mdc*elseif (ibm and vector) or essl
*      call dgemtx(nbr,natr,alpha,a,nbr,b,1,c,1)
*mdc*elseif alliant and fx2800
*c     # 13-mar-91 -rls
*c     # matmul() is crippled and only runs at fortran speed,
*c     # so use ddot/dgemv instead. ddot runs at .04 to 10 mflops,
*c     # dgemv runs at .04 to 17 mflops with the switchover at
*c     # 9.7 mflops at nbr=32 for square matrices.
*c
*      if ( natr*nbr .gt. 1024 ) then
*         call dgemv('t', nbr,natr, alpha, a,nbr, b,1, one, c,1)
*      else
*         do 40 i = 1, natr
*            c(i) = c(i) + alpha * ddot( nbr,   a(1,i), 1,   b, 1 )
*40       continue
*      endif
*mdc*elseif alliant
*      intrinsic matmul
*      c = c + alpha * matmul( b, a )
*mdc*elseif titan
*c     7-dec-89 tuned for 16Mhz R2000 single processor. -rls
*      if ( nbr*natr .ge. 400 ) then
*c        # interface to library gmxma().
*         call gmxma(a,nbr,1, b,1,1, c,1,1, natr,nbr,1, alpha,one)
*      else
*c$doit   vbest
*         do 30 i = 1, natr
*            do 20 j = 1, nbr
*               c(i) = c(i) + alpha * a(j,i) * b(j)
*20          continue
*30       continue
*      endif
*mdc*elseif fps
*c     # 06-mar-91 rgmmul()/saxpy loop order. -rls
*      if ( alpha .eq. (1) ) then
*         call rgmmul( 2, natr,nbr,1, a,nbr,1, b,1,1, c,1,1 )
*      elseif ( alpha .eq. (-1) ) then
*         call rgmmul( 3, natr,nbr,1, a,nbr,1, b,1,1, c,1,1 )
*      else
*         do 30 j = 1, nbr
*            bj = b(j) * alpha
*            if ( bj .ne. zero ) then
*               do 20 i = 1, natr
*                  c(i) = c(i) + a(j,i) * bj
*20             continue
*            endif
*30       continue
*      endif
*mdc*else
*c
*c  standard fortran version.
*c  note: *** this code should not be modified ***
*c  this version uses a dot-product inner loop to allow sequential
*c  memory access to a(*). -rls
*c
*      do 30 i = 1, natr
*         sum = zero
*         do 20 j = 1, nbr
*            sum = sum + a(j,i) * b(j)
*20       continue
*         c(i) = c(i) + alpha * sum
*30    continue
*mdc*endif
      return
      end
*deck gmxva
c
c  note:  this routine is so general that it usually cannot be
c  implemented using vendor-supplied library calls.
c  consequently, its use is discouraged in cases where efficiency
c  is of primary concern.  the user's algorithm should be restructured
c  to use one of the more efficient routines instead. -rls
c
      subroutine gmxva(a,iac,iar, b,ibc, c,icc, nar,nac, alpha)
c
c  general matrix-vector product routine to compute
c      c = c + alpha * a * b
c
c  arguments:
c    a    = first input matrix with effective dimensions a(nar,nac).
c    iac  = spacing between consecutive elements in a column of a.
c    iar  = spacing between consecutive elements in a row of a.
c    b    = input vector with effective dimensions b(nac).
c    ibc  = spacing between consecutive elements in a column of b.
c    c    = output vector with effective dimensions c(nar).
c    icc  = spacing between consecutive elements in a column of c.
c    nar  = number of rows in a and c.
c    nac  = number of columns in a and rows in b.
c    alpha = matrix-vector product scale factor.
c
c  19-nov-89  written by ron shepard.
c
      implicit none
c
      integer iac, iar, ibc, icc, nar, nac
      real*8 a(*), b(*), c(*), alpha
c
      integer i1k, kpt, k, ik, ipt, i, ik1
      real*8 bk, sum
      real*8     zero,     one
      parameter (zero=0d0, one=1d0)
*mdc*if titan
*c     # interface to library gmxma().
*c     # for some reason, the fortran version of this routine does
*c     # not compile to optimal code, even though mxma(), which is
*c     # almost identical, does compile correctly.
*c     # until this is fixed, just punt and call gmxma(). -rls
*      call gmxma(a,iac,iar, b,ibc,1, c,icc,1, nar,nac,1, alpha,one)
*mdc*elseif sun rs6000
c  23-dec-89  dot-product loop ordering. -rls
      ik1 = 1
      ipt = 1
      do 20 i = 1, nar
         sum = zero
         kpt = 1
         ik = ik1
         do 10 k = 1, nac
            sum = sum + a(ik) * b(kpt)
            kpt = kpt + ibc
            ik = ik + iar
10       continue
         c(ipt) = c(ipt) + alpha * sum
         ipt = ipt + icc
         ik1 = ik1 + iac
20    continue
*mdc*elseif fps
*c     # 06-mar-91 rgmmul()/saxpy loop order. -rls
*      if ( alpha .eq. (1) ) then
*         call rgmmul( 2, nar,nac,1, a,iac,iar, b,ibc,1, c,icc,1 )
*      elseif ( alpha .eq. (-1) ) then
*         call rgmmul( 3, nar,nac,1, a,iac,iar, b,ibc,1, c,icc,1 )
*      else
*         i1k = 1
*         kpt = 1
*         do 400 k = 1, nac
*            bk=  b(kpt) * alpha
*            if ( bk .ne. zero ) then
*               ik  = i1k
*               ipt = 1
*               do 300 i = 1, nar
*                  c(ipt) = c(ipt) + a(ik) * bk
*                  ipt = ipt + icc
*                  ik = ik + iac
*300            continue
*            endif
*            i1k = i1k + iar
*            kpt = kpt + ibc
*400      continue
*      endif
*mdc*else
*c
*c  all fortran version.
*c  note: *** this code should not be modified ***
*c  this version uses an outer product algorithm to take advantage of
*c  zeros in the vector b(*); i.e. the innermost do-loop is a saxpy.
*c  the loop ordering in this version results in sequential access of
*c  all the matrices for iac=ibc=icc=1 cases.
*c
*      i1k = 1
*      kpt = 1
*      do 400 k = 1, nac
*         bk=  b(kpt) * alpha
*         if ( bk .ne. zero ) then
*            ik  = i1k
*            ipt = 1
*cdir$       ivdep
*cvd$        ivdep
**vocl loop,novrec
*            do 300 i = 1, nar
*               c(ipt) = c(ipt) + a(ik) * bk
*               ipt = ipt + icc
*               ik = ik + iac
*300         continue
*         endif
*         i1k = i1k + iar
*         kpt = kpt + ibc
*400   continue
*mdc*endif
      return
      end
*deck mxm
*mdc*if cray
*c  use library version.
*mdc*elseif fujitsu
*c     # 14-may-92 fujitsu code of Ross Nobes and Roger Edberg
*c     #           merged. -rls
*      subroutine mxm(a,nar,b,nbr,c,ncc)
*      implicit none
*      integer nar, nbr, ncc
*      real*8 a(nar,nbr),b(nbr,ncc),c(nar,ncc)
*      integer    maxvl
*      parameter( maxvl = 512 )
*      integer i, imax, istart, j, k, kextra
*      do 5 j = 1, ncc
*         do 5 i = 1, nar
*            c(i,j) = 0.d0
*5     continue
*      kextra = mod( nbr, 4 )
*      do 40 j = 1, ncc
*         do 30 istart = 1, nar, maxvl
*            imax = min( istart+maxvl-1, nar )
*            do 10 k = 1, kextra
**vocl loop,repeat(maxvl)
*               do 10 i = istart, imax
*                  c(i,j) = c(i,j) + a(i,k) * b(k,j)
*10          continue
*            do 20 k = kextra+1, nbr, 4
**vocl loop,repeat(maxvl)
*               do 20 i = istart, imax
*                  c(i,j) = c(i,j) + a(i,k)   * b(k,j)
*     &                            + a(i,k+1) * b(k+1,j)
*     &                            + a(i,k+2) * b(k+2,j)
*     &                            + a(i,k+3) * b(k+3,j)
*20          continue
*30       continue
*40    continue
*      return
*      end
*mdc*elseif fps
*c     # 06-mar-91 rgmmul() interface. -rls
*      subroutine mxm(a,nar,b,nbr,c,ncc)
*      implicit integer(a-z)
*      integer nar, nbr, ncc
*      real*8 a(nar,nbr), b(nbr,ncc), c(nar,ncc)
*      call rgmmul( 0, nar,nbr,ncc, a,1,nar, b,1,nbr, c,1,nar )
*      return
*      end
*mdc*elseif alliant and fx2800
*c     # 13-mar-91 -rls
*c     # matmul() is crippled and only runs at fortran speed,
*c     # so use ddot/dgemm instead. ddot runs at .04 to 10 mflops,
*c     # dgemm runs at .04 to 23 mflops with the switchover at
*c     # 2.2 mflops at nbr=8 for square matrices.
*c
*      subroutine mxm(a,nar,b,nbr,c,ncc)
*      implicit integer(a-z)
*c
*      integer nar, nbr, ncc
*      real*8 a(nar,nbr), b(nbr,ncc), c(nar,ncc)
*c
*      integer i, j
*      real*8     zero,     one
*      parameter( zero=0d0, one=1d0 )
*c
*      real*8   ddot
*      external ddot
*c
*      if ( nar*nbr .gt. 64 ) then
*         call dgemm('n','n',nar,ncc,nbr,
*     &    one, a,nar, b,nbr, zero, c,nar)
*      else
*         do 50 j = 1, ncc
*            do 40 i = 1, nar
*               c(i,j) = ddot( nbr,    a(i,1), nar,    b(1,j), 1 )
*40          continue
*50       continue
*      endif
*      return
*      end
*mdc*elseif alliant
*c     # 10-nov-89 intrinsic matmul() used. -rls
*      subroutine mxm(a,nar,b,nbr,c,ncc)
*      implicit integer(a-z)
*      integer nar, nbr, ncc
*      real*8 a(nar,nbr), b(nbr,ncc), c(nar,ncc)
*      intrinsic matmul
*      c = matmul( a, b )
*      return
*      end
*mdc*elseif titan
*c  15-jun-89 interface to library mxma(). -rls
*      subroutine mxm(a,nar,b,nbr,c,ncc)
*      implicit integer(a-z)
*      integer nar, nbr, ncc
*      real*8 a(nar,nbr), b(nbr,ncc), c(nar,ncc)
*c
*      integer i, j, k
*      real*8 bkj
*      real*8    zero
*      parameter(zero=0d0)
*c
*c     16-feb-90 tuned for 16Mhz R2000 single processor. -rls
*      if ( nar*nbr .ge. 400 .or. nar*nbr*ncc .ge. 4096 ) then
*c        # interface to the library mxma().
*         call mxma(a,1,nar, b,1,nbr, c,1,nar, nar,nbr,ncc)
*      else
*c$doit   vbest
*         do 50 i = 1, nar
*            do 40 j = 1, ncc
*               c(i,j) = zero
*               do 30 k = 1, nbr
*                  c(i,j) = c(i,j) + a(i,k) * b(k,j)
*30             continue
*40          continue
*50       continue
*      endif
*      return
*      end
*mdc*elseif sun
      subroutine mxm(a,nar,b,nbr,c,ncc)
c  23-dec-89 dot-product loop order. -rls
      implicit integer(a-z)
      integer nar, nbr, ncc
      real*8 a(nar,nbr), b(nbr,ncc), c(nar,ncc)
c
      integer i, j, k
      real*8    zero,    sum
      parameter(zero=0d0)
c
      do 50 j = 1, ncc
         do 40 i = 1, nar
            sum = zero
            do 30 k = 1, nbr
               sum = sum + a(i,k) * b(k,j)
30          continue
            c(i,j) = sum
40       continue
50    continue
      return
      end
*mdc*elseif blas3
*c  25-may-89 interface to level-3 blas. -rls
*      subroutine mxm(a,nar,b,nbr,c,ncc)
*      implicit integer(a-z)
*      integer nar, nbr, ncc
*      real*8 a(nar,nbr), b(nbr,ncc), c(nar,ncc)
*      real*8    zero,     one
*      parameter(zero=0d0, one=1d0)
*c
*      call dgemm('n','n',nar,ncc,nbr, one, a,nar, b,nbr, zero, c,nar)
*      return
*      end
*mdc*elseif (ibm .and. vector) or essl
*c  25-may-89 interface to ibm-essl dgemul(). -rls
*      subroutine mxm(a,nar,b,nbr,c,ncc)
*      implicit integer(a-z)
*      integer nar, nbr, ncc
*      real*8 a(nar,nbr), b(nbr,ncc), c(nar,ncc)
*c
*      call dgemul(a,nar,'n',  b,nbr,'n',  c,nar,  nar,nbr,ncc)
*      return
*      end
*mdc*else
*      subroutine mxm(a,nar,b,nbr,c,ncc)
*c
*c  fortran version of cray mxm() to compute c = a * b
*c  note: *** this code should not be modified ***
*c
*c  input:
*c  a(*),b(*) = input matrices.
*c  nar = number of rows in a(*) and c(*).
*c  nbr = number of columns of a(*) and rows of b(*).
*c  ncc = number of columns of b(*) and c(*).
*c        all dimensions must be > 0.
*c
*c  output:
*c  c(*) = a * b product matrix.
*c
*c  written by ron shepard.
*c
*      implicit integer(a-z)
*      integer nar, nbr, ncc
*      real*8 a(nar,nbr), b(nbr,ncc), c(nar,ncc)
*c
*      integer i, j, k
*      real*8 bkj
*      real*8    zero
*      parameter(zero=0d0)
*c
*      do 20 j = 1, ncc
*         do 10 i = 1, nar
*            c(i,j) = zero
*10       continue
*20    continue
*c
*c  a saxpy inner loop is used to exploit sparseness in the matrix
*c  b(*), and this results in sequential access to a(*), b(*),
*c  and c(*). -rls
*c
*      do 50 j = 1, ncc
*         do 40 k = 1, nbr
*            bkj = b(k,j)
*            if ( bkj .ne. zero ) then
*               do 30 i = 1, nar
*                  c(i,j) = c(i,j) + a(i,k) * bkj
*30             continue
*            endif
*40       continue
*50    continue
*      return
*      end
*mdc*endif
*deck mtxm
      subroutine mtxm(a,natr,b,nbr,c,ncc)
c
c  to compute c = a(transpose) * b
c
c  input:
c  a(*),b(*) = input matrices.
c  natr = number of rows in a(transopose)(*) and c(*).
c  nbr = number of columns of a(transpose)(*) and rows of b(*).
c  ncc = number of columns of b(*) and c(*).
c        all dimensions must be > 0.
c
c  output:
c  c(*) = a(t) * b product matrix.
c
c  25-may-89 interface to ibm-essl dgemul(). -rls
c  written by ron shepard.
c
      implicit none
      integer natr, nbr, ncc
      real*8 a(nbr,natr), b(nbr,ncc), c(natr,ncc)
c
      integer i, j, k
      real*8 sum
      real*8    zero,     one
      parameter(zero=0d0, one=1d0)
c
      real*8   ddot
      external ddot
c
*mdc*if fujitsu
*c     # 14-may-92 fujitsu code of Ross Nobes and Roger Edberg
*c     #           merged. -rls
*      integer    maxvl
*      parameter( maxvl = 512 )
*      integer imax, istart, kextra
*      real*8  b0, b1, b2, b3
*      do 5 j = 1, ncc
*         do 5 i = 1, natr
*            c(i,j) = zero
*5     continue
*      kextra = mod( nbr, 4 )
*      do 30 istart = 1, natr, maxvl
*         imax = min( istart+maxvl-1, natr )
*         do 10 k = 1, kextra
*            do 10 j = 1, ncc
*               b0 = b(k,j)
**vocl loop,repeat(maxvl)
*               do 10 i = istart, imax
*                  c(i,j) = c(i,j) + b0 * a(k,i)
*10       continue
*        do 20 k = kextra+1, nbr, 4
*           do 20 j = 1, ncc
*              b0 = b(k,j)
*              b1 = b(k+1,j)
*              b2 = b(k+2,j)
*              b3 = b(k+3,j)
**vocl loop,repeat(maxvl)
*              do 20 i = istart, imax
*                 c(i,j) = c(i,j) + b0 * a(k,i)
*     &                           + b1 * a(k+1,i)
*     &                           + b2 * a(k+2,i)
*     &                           + b3 * a(k+3,i)
*20      continue
*30    continue
*mdc*elseif cray
*c     # interface to the library mxma().
*      call mxma(a,nbr,1,  b,1,nbr,  c,1,natr,  natr,nbr,ncc)
*mdc*elseif fps
*c      # 06-mar-91 rgmmul() interface. -rls
*      call rgmmul( 0, natr,nbr,ncc, a,nbr,1, b,1,nbr, c,1,natr )
*mdc*elseif alliant and fx2800
*c     # 13-mar-91 -rls
*c     # matmul() is crippled and only runs at fortran speed,
*c     # so use ddot/dgemm instead. ddot runs at .04 to 10 mflops,
*c     # dgemm runs at .04 to 29 mflops with the switchover at
*c     # 6.6 mflops at nbr=16 for square matrices.
*c
*      if ( natr*nbr .gt. 240 ) then
*         call dgemm('t','n', natr,ncc,nbr,
*     &    one, a,nbr, b,nbr, zero, c,natr )
*      else
*         do 50 j = 1, ncc
*            do 40 i = 1, natr
*               c(i,j) = ddot( nbr,    a(1,i), 1,    b(1,j), 1 )
*40          continue
*50       continue
*      endif
*mdc*elseif alliant
*c     # 10-nov-89 intrinsic matmul() used. -rls
*      intrinsic matmul, transpose
*      c = matmul( transpose(a), b)
*mdc*elseif blas3
c  25-may-89 interface to level-3 blas. -rls
      call dgemm('t','n',natr,ncc,nbr, one, a,nbr, b,nbr, zero, c,natr)
*mdc*elseif (ibm and vector) or essl
*c  interface to the ibm-essl routine dgemul().
*      call dgemul(a,nbr,'t',  b,nbr,'n',  c,natr,  natr,nbr,ncc)
*mdc*elseif titan
*c     16-feb-90 tuned for 16Mhz R2000 single processor. -rls
*      if ( ncc*natr*nbr .ge. 512 ) then
*c        # interface to the library mxma().
*         call mxma(a,nbr,1,  b,1,nbr,  c,1,natr,  natr,nbr,ncc)
*      else
*         do 30 j = 1, ncc
*            do 20 i = 1, natr
*               sum = zero
*               do 10 k = 1, nbr
*                  sum = sum + a(k,i) * b(k,j)
*10             continue
*               c(i,j) = sum
*20          continue
*30       continue
*      endif
*mdc*else
*c
*c  all fortran version.
*c  note: *** this code should not be modified ***
*c
*c  use dot-product loop ordering for sequential memory accesses.
*c  sparsness is not exploited.  none of the saxpy inner-loop orderings
*c  result in sequential memory access.  if saxpy loops are essential
*c  for some machine, then the matrices should be partitioned, and
*c  intermediate results accumulated into a sequentially accessed
*c  intermediate array and periodically dumped to c(*). -rls
*c
*      do 30 j = 1, ncc
*         do 20 i = 1, natr
*            sum = zero
*            do 10 k = 1, nbr
*               sum = sum + a(k,i)*b(k,j)
*10          continue
*            c(i,j) = sum
*20       continue
*30    continue
*mdc*endif
      return
      end
*deck mxmt
      subroutine mxmt( a, nar, b, nbtr, c, ncc )
c
c  to compute c = a * b(transpose)
c
c  input:
c  a(*),b(*) = input matrices.
c  nar = number of rows in a(*) and c(*).
c  nbtr = number of columns of a(*) and rows of b(transpose)(*).
c  ncc = number of columns of b(transpose)(*) and c(*).
c        all dimensions must be > 0.
c
c  output:
c  c(*) = a * b(transpose) product matrix.
c
c  25-may-89 interface to ibm-essl dgemul(). -rls
c  written by ron shepard.
c
      implicit none
      integer nar, nbtr, ncc
      real*8 a(nar,nbtr), b(ncc,nbtr), c(nar,ncc)
c
      integer i, j, k
      real*8 bjk, sum
      real*8    zero,     one
      parameter(zero=0d0, one=1d0)
c
      real*8   ddot
      external ddot
c
*mdc*if fujitsu
*c     # 14-may-92 fujitsu code of Ross Nobes and Roger Edberg
*c     #           merged. -rls
*      integer    maxvl
*      parameter (maxvl = 512)
*      integer imax, istart, kextra
*      do 5 j = 1, ncc
*         do 5 i = 1, nar
*            c(i,j) = zero
*5     continue
*      kextra = mod( nbtr, 4 )
*      do 40 j = 1, ncc
*         do 30 istart = 1, nar, maxvl
*            imax = min( istart+maxvl-1, nar )
*            do 10 k = 1, kextra
**vocl loop,repeat(maxvl)
*               do 10 i = istart, imax
*                  c(i,j) = c(i,j) + a(i,k) * b(j,k)
*10          continue
*            do 20 k = kextra+1, nbtr, 4
**vocl loop,repeat(maxvl)
*               do 20 i = istart, imax
*                  c(i,j) = c(i,j) + a(i,k)   * b(j,k)
*     &                            + a(i,k+1) * b(j,k+1)
*     &                            + a(i,k+2) * b(j,k+2)
*     &                            + a(i,k+3) * b(j,k+3)
*20          continue
*30       continue
*40    continue
*mdc*elseif cray
*c     # interface to the library mxma().
*      call mxma(a,1,nar,  b,ncc,1,  c,1,nar,  nar,nbtr,ncc)
*mdc*elseif fps
*c      # 06-mar-91 rgmmul() interface. -rls
*      call rgmmul( 0, nar,nbtr,ncc, a,1,nar, b,ncc,1, c,1,nar )
*mdc*elseif alliant and fx2800
*c     # 13-mar-91 -rls
*c     # matmul() is crippled and only runs at fortran speed,
*c     # so use ddot/dgemm instead. ddot runs at .04 to 10 mflops,
*c     # dgemm runs at .04 to 21 mflops with the switchover at
*c     # 2.6 mflops at nbtr=8 for square matrices.
*c
*      if ( nar*nbtr .gt. 64 ) then
*         call dgemm( 'n', 't', nar, ncc, nbtr,
*     &    one,  a, nar,  b, ncc, zero,  c, nar )
*      else
*         do 50 j = 1, ncc
*            do 40 i = 1, nar
*               c(i,j) = ddot( nbtr,    a(i,1), nar,    b(j,1), ncc )
*40          continue
*50       continue
*      endif
*mdc*elseif alliant
*c     # 10-nov-89 intrinsic matmul() used. -rls
*      intrinsic matmul, transpose
*      c = matmul( a, transpose(b) )
*mdc*elseif blas3
c  25-may-89 interface to level-3 blas. -rls
      call dgemm('n','t',nar,ncc,nbtr, one, a,nar, b,ncc, zero, c,nar)
*mdc*elseif (ibm and vector) or essl
*c  interface to the ibm-essl routime dgemul().
*      call dgemul(a,nar,'n',  b,ncc,'t',  c,nar,  nar,nbtr,ncc)
*mdc*elseif titan
*c     16-feb-90 tuned for 16Mhz R2000 single processor. -rls
*      if ( ncc*nar*nbtr .ge. 512 ) then
*c        # interface to the library mxma().
*         call mxma(a,1,nar,  b,ncc,1,  c,1,nar,  nar,nbtr,ncc)
*      else
*c$doit   vbest
*         do 50 j = 1, ncc
*            do 40 i = 1, nar
*               sum = zero
*               do 30 k = 1, nbtr
*                  sum = sum + a(i,k) * b(j,k)
*30             continue
*               c(i,j) = sum
*40          continue
*50       continue
*      endif
*mdc*elseif sun
*c
*c  23-dec-89 dot-product loop order. -rls
*c
*      do 50 j = 1, ncc
*         do 40 i = 1, nar
*            sum = zero
*            do 30 k = 1, nbtr
*               sum = sum + a(i,k) * b(j,k)
*30          continue
*            c(i,j) = sum
*40       continue
*50    continue
*mdc*else
*c
*c  all fortran version.
*c  note: *** this code should not be modified ***
*c  saxpy inner loop allows exploitation of sparseness in b(*) and
*c  results in sequential memory accesses. -rls
*c
*      do 20 j = 1, ncc
*         do 10 i = 1, nar
*            c(i,j) = zero
*10       continue
*20    continue
*      do 50 k = 1, nbtr
*         do 40 j = 1, ncc
*            bjk = b(j,k)
*            if ( bjk .ne. zero ) then
*               do 30 i = 1, nar
*                  c(i,j) = c(i,j) + a(i,k) * bjk
*30             continue
*            endif
*40       continue
*50    continue
*mdc*endif
      return
      end
*deck mtxmt
      subroutine mtxmt(a,natr,b,nbtr,c,ncc)
c
c  to compute c = a(transpose) * b(transpose)
c
c  input:
c  a(*),b(*) = input matrices.
c  natr = number of rows in a(transpose)(*) and c(*).
c  nbtr = number of columns of a(transpose)(*) and
c         rows of b(transpose)(*).
c  ncc = number of columns of b(transpose)(*) and c(*).
c        all dimensions must be > 0.
c
c  output:
c  c(*) = a(transpose) * b(transpose) product matrix.
c
c  25-may-89 interface to ibm-essl dgemul(). -rls
c  written by ron shepard.
c
      implicit none
      integer natr, nbtr, ncc
      real*8 a(nbtr,natr), b(ncc,nbtr), c(natr,ncc)
c
      integer i, j, k
      real*8 aki, sum
      real*8    zero,     one
      parameter(zero=0d0, one=1d0)
c
      real*8   ddot
      external ddot
c
*mdc*if fujitsu
*c     # 14-may-92 fujitsu code of Ross Nobes and Roger Edberg
*c     #           merged. -rls
*      integer    maxvl
*      parameter( maxvl = 512 )
*      integer imax, istart, kextra
*      real*8  b0, b1, b2, b3
*      kextra = mod( nbtr, 4 )
*c
*      do 20 j = 1, ncc
*         do 10 i = 1, natr
*            c(i,j) = zero
*10       continue
*20    continue
*c
*         do 50 istart = 1, natr, maxvl
*            imax = min( istart+maxvl-1, natr )
*            do 30 k = 1, kextra
*               do 30 j = 1, ncc
*                  b0 = b(j,k)
**vocl loop,repeat(maxvl)
*                  do 30 i = istart, imax
*                     c(i,j) = c(i,j) + b0 * a(k,i)
*30          continue
*           do 40 k = kextra+1, nbtr, 4
*              do 40 j = 1, ncc
*                 b0 = b(j,k)
*                 b1 = b(j,k+1)
*                 b2 = b(j,k+2)
*                 b3 = b(j,k+3)
**vocl loop,repeat(maxvl)
*                 do 40 i = istart, imax
*                    c(i,j) = c(i,j) + b0 * a(k,i)
*     &                              + b1 * a(k+1,i)
*     &                              + b2 * a(k+2,i)
*     &                              + b3 * a(k+3,i)
*40         continue
*50       continue
*c
*mdc*elseif cray
*c     # interface to the library mxma().
*      call mxma(a,nbtr,1,  b,ncc,1,  c,1,natr,  natr,nbtr,ncc)
*mdc*elseif fps
*c      # 06-mar-91 rgmmul() interface. -rls
*      call rgmmul( 0, natr,nbtr,ncc, a,nbtr,1,  b,ncc,1,  c,1,natr )
*mdc*elseif alliant and fx2800
*c     # 13-mar-91 -rls
*c     # matmul() is crippled and only runs at fortran speed,
*c     # so use ddot/dgemm instead. ddot runs at .04 to 10 mflops,
*c     # dgemm runs at .04 to 29 mflops with the switchover at
*c     # 4.9 mflops at nbtr=8 for square matrices.
*c
*      if ( natr*nbtr .gt. 64 ) then
*         call dgemm('t','t',natr,ncc,nbtr,
*     &    one, a,nbtr, b,ncc, zero, c,natr)
*      else
*         do 50 j = 1, ncc
*            do 40 i = 1, natr
*               c(i,j) = ddot( nbtr,    a(1,i), 1,    b(j,1), ncc )
*40          continue
*50       continue
*      endif
*mdc*elseif alliant
*c     # 10-nov-89 intrinsic matmul() used. -rls
*      intrinsic matmul, transpose
*      c = matmul( transpose(a), transpose(b) )
*mdc*elseif blas3
c  25-may-89 interface to level-3 blas. -rls
      call dgemm('t','t',natr,ncc,nbtr,one,a,nbtr,b,ncc,zero,c,natr)
*mdc*elseif (ibm and vector) or essl
*c  interface to the ibm-essl routime dgemul().
*      call dgemul(a,nbtr,'t',  b,ncc,'t',  c,natr,  natr,nbtr,ncc)
*mdc*elseif titan
*c     16-feb-90 tuned for 16Mhz R2000 single processor. -rls
*      if ( ncc*nbtr.ge. 400 .or. ncc*natr*nbtr .ge. 4096 ) then
*c        # interface to the library mxma().
*         call mxma(a,nbtr,1,  b,ncc,1,  c,1,natr,  natr,nbtr,ncc)
*      else
*c$doit   vbest
*         do 50 j = 1, ncc
*            do 40 i = 1, natr
*               sum = zero
*               do 30 k = 1, nbtr
*                  sum = sum + a(k,i) * b(j,k)
*30             continue
*               c(i,j) = sum
*40          continue
*50       continue
*      endif
*mdc*elseif sun
*c
*c  23-dec-89 dot-product loop order. -rls
*c
*      do 50 j = 1, ncc
*         do 40 i = 1, natr
*            sum = zero
*            do 30 k = 1, nbtr
*               sum = sum + a(k,i) * b(j,k)
*30          continue
*            c(i,j) = sum
*40       continue
*50    continue
*mdc*else
*c
*c  all fortran version.
*c  note: *** this code should not be modified ***
*c
*c  for this matrix operation, it is not possible to have sequential
*c  memory accesses to all three matrices.  if this is important, then
*c  this routine should partition the output matrix, and accumulate the
*c  results for each partition into a sequentially accessed intermediate
*c  array, dumping the results to c(*) as necessary.  note also that
*c  c(transpose)=b*a.  the results could also be computed using mxm()
*c  and explicitly transposed (if necessary) by the calling routine.
*c  -rls
*c
*      do 20 j = 1, ncc
*         do 10 i = 1, natr
*            c(i,j) = zero
*10       continue
*20    continue
*      do 50 i = 1, natr
*         do 40 k = 1, nbtr
*            aki = a(k,i)
*            if ( aki .ne. zero ) then
*               do 30 j = 1, ncc
*                  c(i,j) = c(i,j) + aki * b(j,k)
*30             continue
*            endif
*40       continue
*50    continue
*mdc*endif
      return
      end
*deck mxma
c
c  note:  this routine is so general that it usually cannot be
c  implemented using vendor-supplied library calls.
c  consequently, its use is discouraged in cases where efficiency
c  is of primary concern.  the user's algorithm should be restructured
c  to use one of the more efficient routines instead. -rls
c
*mdc*if cray titan or (alliant and fx8)
*c      # use the library version.
*mdc*elseif fps
*c      # 06-mar-91 rgmmul() interface. -rls
*      subroutine mxma(a,iac,iar, b,ibc,ibr, c,icc,icr, nar,nac,nbc)
*      implicit integer(a-z)
*      integer iac, iar, ibc, ibr, icc, icr, nar, nac, nbc
*      real*8 a(*), b(*), c(*)
*      call rgmmul( 0, nar,nac,nbc, a,iac,iar, b,ibc,ibr, c,icc,icr )
*      return
*      end
*mdc*elseif sun rs6000
      subroutine mxma(a,iac,iar, b,ibc,ibr, c,icc,icr, nar,nac,nbc)
c
c     # 23-dec-89 dot-product loop order. -rls
c
      implicit integer(a-z)
      integer iac, iar, ibc, ibr, icc, icr, nar, nac, nbc
      real*8 a(*), b(*), c(*)
c
      integer i1j, k1j, j, ij, ik1, i, kj, ik, k
      real*8     zero,    sum
      parameter (zero=0d0)
c
      i1j = 1
      k1j = 1
      do 50 j = 1, nbc
         ij  = i1j
         ik1 = 1
         do 40 i = 1, nar
            sum = zero
            kj = k1j
            ik = ik1
            do 30 k = 1, nac
               sum = sum + a(ik) * b(kj)
               kj = kj + ibc
               ik = ik + iar
30          continue
            c(ij) = sum
            ij  = ij + icc
            ik1 = ik1 + iac
40       continue
         i1j = i1j + icr
         k1j = k1j + ibr
50    continue
      return
      end
*mdc*else
*      subroutine mxma(a,iac,iar, b,ibc,ibr, c,icc,icr, nar,nac,nbc)
*c
*c  fortran version of cray mxma to calculate c=a*b.
*c  note: *** this code should not be modified ***
*c
*c  arguments:
*c    a    = first input matrix with effective dimensions a(nar,nac).
*c    iac  = spacing between consecutive elements in a column of a.
*c    iar  = spacing between consecutive elements in a row of a.
*c    b    = second input matrix with effective dimensions b(nac,nbc).
*c    ibc  = spacing between consecutive elements in a column of b.
*c    ibr  = spacing between consecutive elements in a row of b.
*c    c    = output matrix with effective dimensions c(nar,nbc).
*c    icc  = spacing between consecutive elements in a column of c.
*c    icr  = spacing between consecutive elements in a row of c.
*c    nar  = number of rows in a and c.
*c    nac  = number of columns in a and rows in b.
*c    nbc  = number of columns in b and c.
*c
*c  besides allowing arbitrary sub-blocking and element spacing of
*c  any of the matrices, this routine also allows the transposition
*c  of any of the matrix arguments.
*c
*c  for example, suppose that z(*) is dimensioned in the calling
*c  program as:
*c
*c        dimension z(n,*)
*c
*c  then the following call will calculate a matrix product
*c  involving z(*):
*c
*c        call mxma(...z,1,n, ...)
*c
*c  while a matrix product involving z(transpose) may be calculated
*c  with:
*c        call mxma(...z,n,1, ...)
*c
*c  written 04-jan-85 by ron shepard.
*c
*      implicit none
*c
*      integer iac, iar, ibc, ibr, icc, icr, nar, nac, nbc
*      real*8 a(*),b(*),c(*)
*c
*      integer i1j, j, ij, i, k1j, i1k, kj, k, ik
*      real*8     zero,     bkj
*      parameter (zero=0d0)
*c
*c  initialize the output matrix c(*).
*c
*      i1j = 1
*      do 200 j = 1, nbc
*         ij = i1j
*         do 100 i = 1, nar
*            c(ij) = zero
*            ij = ij + icc
*100      continue
*         i1j = i1j + icr
*200   continue
*c
*c  this version uses an outer product algorithm to take advantage of
*c  zeros in the matrix b(*); i.e. the innermost do-loop is a saxpy.
*c  the loop ordering in this version results in sequential access of
*c  all the matrices for iac=ibc=icc=1 cases.
*c
*      i1j = 1
*      k1j = 1
*      do 500 j = 1, nbc
*         i1k = 1
*         kj = k1j
*         do 400 k = 1, nac
*            bkj = b(kj)
*            kj = kj + ibc
*            if ( bkj .ne. zero ) then
*               ij = i1j
*               ik = i1k
*               do 300 i = 1, nar
*                  c(ij) = c(ij) + a(ik) * bkj
*                  ij = ij + icc
*                  ik = ik + iac
*300            continue
*            endif
*            i1k = i1k + iar
*400      continue
*         i1j = i1j + icr
*         k1j = k1j + ibr
*500   continue
*c
*      return
*      end
*mdc*endif
*deck mxv
*mdc*if cray
*c  use library version.
*mdc*elseif fujitsu
*c     # 14-may-92 fujitsu code of Ross Nobes and Roger Edberg
*c     #           merged. -rls
*      subroutine mxv(a,nar,b,nbr,c)
*      implicit none
*      integer nar, nbr
*      real*8 a(nar,nbr), b(nbr), c(nar)
*      integer    maxvl
*      parameter( maxvl = 512 )
*      integer i, imax, istart, k, kextra
*      real*8     zero,       one
*      parameter( zero = 0d0, one = 1d0 )
*      do 5 i = 1, nar
*         c(i) = zero
*5     continue
*         kextra = mod( nbr, 4 )
*         do 30 istart = 1, nar, maxvl
*            imax = min( istart+maxvl-1, nar )
*            do 10 k = 1, kextra
**vocl loop,repeat(maxvl)
*               do 10 i = istart, imax
*                  c(i) = c(i) + a(i,k) * b(k)
*10          continue
*            do 20 k = kextra+1, nbr, 4
**vocl loop,repeat(maxvl)
*               do 20 i = istart, imax
*                  c(i) = c(i) + a(i,k)   * b(k)
*     &                        + a(i,k+1) * b(k+1)
*     &                        + a(i,k+2) * b(k+2)
*     &                        + a(i,k+3) * b(k+3)
*20          continue
*30       continue
*      return
*      end
*mdc*elseif fps
*c      # 06-mar-91 rgmmul() interface. -rls
*      subroutine mxv(a,nar,b,nbr,c)
*      implicit integer(a-z)
*      integer nar, nbr
*      real*8 a(nar,nbr), b(nbr), c(nar)
*      call rgmmul( 0, nar,nbr,1, a,1,nar, b,1,1, c,1,1 )
*      return
*      end
*mdc*elseif alliant and fx2800
*c     # 13-mar-91 -rls
*c     # matmul() is crippled and only runs at fortran speed,
*c     # so use ddot/dgemv instead. ddot runs at .04 to 10 mflops,
*c     # dgemv runs at .04 to 17 mflops with the switchover at
*c     # 6.2 mflops at nbr=24 for square matrices.
*c
*      subroutine mxv(a,nar,b,nbr,c)
*      implicit integer(a-z)
*      integer nar, nbr
*      real*8 a(nar,nbr), b(nbr), c(nar)
*c
*      integer i
*      real*8    zero,     one
*      parameter(zero=0d0, one=1d0)
*c
*      real*8   ddot
*      external ddot
*c
*      if ( nar*nbr .gt. 576 ) then
*         call dgemv('n', nar,nbr, one, a,nar, b,1, zero, c,1)
*      else
*         do 40 i = 1, nar
*            c(i) = ddot( nbr,   a(i,1), nar,   b, 1 )
*40       continue
*      endif
*      return
*      end
*mdc*elseif alliant
*c     # 10-nov-89 intrinsic matmul() used. -rls
*      subroutine mxv(a,nar,b,nbr,c)
*      implicit integer(a-z)
*      integer nar, nbr
*      real*8 a(nar,nbr),b(nbr),c(nar)
*      intrinsic matmul
*c
*c     # fx8 data:
*c     # 21-feb-90 4-cpu matmul() peak is 7.7 mflops for this operation.
*c     # 4-cpu library mxva() peak is 17.4 mflops, with the switchover
*c     # occuring at 40x40.  however, the 1-cpu matmul() is always
*c     # faster than the 1-cpu mxva(). -rls
*c
*      c = matmul( a, b )
*      return
*      end
*mdc*elseif blas2
      subroutine mxv(a,nar,b,nbr,c)
c
c  25-may-89 interface to level-2 blas. -rls
c
      implicit integer(a-z)
      integer nar, nbr
      real*8 a(nar,nbr),b(nbr),c(nar)
c
      real*8    zero,     one
      parameter(zero=0d0, one=1d0)
c
      call dgemv('n',nar,nbr,one,  a,nar,  b,1,zero,  c,1)
      return
      end
*mdc*elseif (ibm and vector) or essl
*      subroutine mxv(a,nar,b,nbr,c)
*c
*c  25-may-89 interface to ibm-essl dgemx(). -rls
*c
*      implicit integer(a-z)
*      integer nar, nbr
*      real*8 a(nar,nbr),b(nbr),c(nar)
*c
*      real*8    zero,     one
*      parameter(zero=0d0, one=1d0)
*c
*      do 10 i = 1, nar
*         c(i) = zero
*10    continue
*      call dgemx( nar, nbr, one,   a, nar,  b, 1,   c, 1 )
*      return
*      end
*mdc*elseif titan
*      subroutine mxv(a,nar,b,nbr,c)
*c
*c  15-jun-89 interface to library mxma(). -rls
*c
*      implicit integer(a-z)
*      integer nar, nbr
*      real*8 a(nar,nbr),b(nbr),c(nar)
*c
*      integer i, j
*      real*8    zero,    bj
*      parameter(zero=0d0)
*c
*c     7-dec-89 tuned for 16Mhz R2000 single processor. -rls
*      if ( nar*nbr .ge. 400 ) then
*c        # interface to library mxma().
*         call mxma(a,1,nar, b,1,1, c,1,1, nar,nbr,1)
*      else
*c$doit   vbest
*         do 30 i = 1, nar
*            c(i) = zero
*            do 20 j = 1, nbr
*               c(i) = c(i) + a(i,j) * b(j)
*20          continue
*30       continue
*      endif
*      return
*      end
*mdc*elseif sun
*      subroutine mxv(a,nar,b,nbr,c)
*c
*c  23-dec-89 dot-product loop ordering. -rls
*c
*      implicit integer(a-z)
*      integer nar, nbr
*      real*8 a(nar,nbr),b(nbr),c(nar)
*c
*      integer i, j
*      real*8    zero,    sum
*      parameter(zero=0d0)
*c
*      do 30 i = 1, nar
*         sum = zero
*         do 20 j = 1, nbr
*            sum = sum + a(i,j) * b(j)
*20       continue
*         c(i) = sum
*30    continue
*      return
*      end
*mdc*else
*      subroutine mxv(a,nar,b,nbr,c)
*c
*c  fortran version of cray mxv() library routine.
*c  note: *** this code should not be modified ***
*c  computes the matrix-vector product c = a * b.
*c
*c  input:
*c  a(*) = input matrix.
*c  nar = number of rows in a(*) and c(*) (must be > 0).
*c  b(*) = input vector.
*c  nbr = number of rows in b(*) and columns of a(*) (must be > 0).
*c
*c  output:
*c  c(*) = output vector.
*c
*c  written by ron shepard.
*c
*      implicit integer(a-z)
*      integer nar, nbr
*      real*8 a(nar,nbr), b(nbr), c(nar)
*c
*      integer i, j
*      real*8 bj
*      real*8    zero
*      parameter(zero=0d0)
*c
*      do 10 i = 1, nar
*         c(i) = zero
*10    continue
*c
*c  a saxpy inner loop is used to exploit sparseness in b(*), and
*c  this loop ordering also results in sequential access to a(*)
*c  and c(*). -rls
*c
*      do 30 j = 1, nbr
*         bj = b(j)
*         if ( bj .ne. zero ) then
*            do 20 i = 1, nar
*               c(i) = c(i) + a(i,j) * bj
*20          continue
*         endif
*30    continue
*c
*      return
*      end
*mdc*endif
*deck mtxv
      subroutine mtxv(a,natr,b,nbr,c)
c
c  compute the matrix(transpose)-vector product:
c         c = a(transpose) * b.
c
c  input:
c  a(*) = input matrix.
c  natr = number of rows in a(transpose) and c(*) (must be > 0).
c  b(*) = input vector.
c  nbr = number of rows in b(*) and columns of a(transpose)
c         (must be > 0).
c
c  25-may-89 interface to ibm-essl dgemul(). -rls
c  written by ron shepard.
c
      implicit none
      integer natr, nbr
      real*8 a(nbr,natr),b(nbr),c(natr)
c
      integer i, j
      real*8 sum
      real*8    zero,     one
      parameter(zero=0d0, one=1d0)
c
      real*8   ddot
      external ddot
c
*mdc*if cray
*c-  interface to the library mxva().
*c-  21-feb-90  mxva() has a 110 mflops peak for this operation. -rls
*c-     call mxva(a,nbr,1,  b,1,  c,1,  natr,nbr)
*c  interface to the library sxmpy().
*c  21-feb-90  sxmpy() has a 180 mflops peak. -rls
*      do 10 i=1,natr
*         c(i)=zero
*10    continue
*      call sxmpy(natr,1,c, nbr,1,b, nbr,a)
*mdc*elseif fps
*c      # 06-mar-91 rgmmul() interface. -rls
*      call rgmmul( 0, natr,nbr,1, a,nbr,1, b,1,1, c,1,1 )
*mdc*elseif alliant and fx2800
*c     # 13-mar-91 -rls
*c     # matmul() is crippled and only runs at fortran speed,
*c     # so use ddot/dgemv instead. ddot runs at .04 to 10 mflops,
*c     # dgemv runs at .04 to 17 mflops with the switchover at
*c     # 9.7 mflops at nbr=32 for square matrices.
*c
*      if ( natr*nbr .gt. 1024 ) then
*         call dgemv('t', nbr,natr, one, a,nbr, b,1, zero, c,1)
*      else
*         do 40 i = 1, natr
*            c(i) = ddot( nbr,   a(1,i), 1,   b, 1 )
*40       continue
*      endif
*mdc*elseif (alliant and fx8)
*c     # 21-feb-90 mxva() branch added. -rls
*c     # 10-nov-89 intrinsic matmul() used. -rls
*      intrinsic matmul, transpose
*c
*c-fx8 data: 4_cpu_mflops/1_cpu_mflops
*c-2.08/0.59      c = matmul( b, a)
*c-0.71/0.72      c = matmul( transpose(a), b )
*c-14.2/4.72      call mxva(a,nbr,1,  b,1,  c,1,  natr,nbr)
*c
*      if ( natr*nbr .le. 256 ) then
*         c = matmul( b, a)
*      else
*         call mxva(a,nbr,1,  b,1,  c,1,  natr,nbr)
*      endif
*mdc*elseif titan
*c     16-feb-90 tuned for 16Mhz R2000 single processor. -rls
*      if ( nbr*natr .ge. 400 ) then
*c        # interface to library mxma().
*         call mxma(a,nbr,1, b,1,1, c,1,1, natr,nbr,1)
*      else
*c$doit   vbest
*         do 30 i = 1, natr
*            c(i) = zero
*            do 20 j = 1, nbr
*               c(i) = c(i) + a(j,i) * b(j)
*20          continue
*30       continue
*      endif
*mdc*elseif blas2
c  25-may-89 interface to level-2 blas. -rls
c  note that level-2 conventions concerning transposed matrices
c  are not consistent with level-3 conventions. -rls
      call dgemv('t',nbr,natr,  one,  a,nbr,  b,1,  zero,  c,1)
*mdc*elseif (ibm and vector) or essl
*c  interface to the ibm-essl routine dgemtx().
*      call dgemtx(nbr,natr,one,a,nbr,b,1,c,1)
*mdc*else
*c
*c  all fortran version.
*c  note: *** this code should not be modified ***
*c  this version uses a dot-product inner loop to allow sequential
*c  memory access to a(*). -rls
*c
*      do 30 i = 1, natr
*         sum = zero
*         do 20 j = 1, nbr
*            sum = sum + a(j,i) * b(j)
*20       continue
*         c(i) = sum
*30    continue
*mdc*endif
c
      return
      end
*deck mxva
c
c  note:  this routine is so general that it usually cannot be
c  implemented using vendor-supplied library calls.
c  consequently, its use is discouraged in cases where efficiency
c  is of primary concern.  the user's algorithm should be restructured
c  to use one of the more efficient routines instead. -rls
c
*mdc*if cray or (alliant and fx8)
*c  use library version.
*mdc*elseif fps
*c      # 06-mar-91 rgmmul() interface. -rls
*      subroutine mxva(a,iac,iar, b,ibc, c,icc, nar,nac)
*      implicit integer(a-z)
*      integer iac, iar, ibc, icc, nar, nac
*      real*8 a(*), b(*), c(*)
*      call rgmmul( 0, nar,nac,1, a,iac,iar, b,ibc,1, c,icc,1 )
*      return
*      end
*mdc*elseif sun rs6000
      subroutine mxva(a,iac,iar, b,ibc, c,icc, nar,nac)
c
c  23-dec-89  dot-product loop ordering. -rls
c
      implicit integer(a-z)
      integer iac, iar, ibc, icc, nar, nac
      real*8 a(*), b(*), c(*)
c
      integer ik1, ipt, i, kpt, ik, k
      real*8    zero,    sum
      parameter(zero=0d0)
c
      ik1 = 1
      ipt = 1
      do 20 i = 1, nar
         sum = zero
         kpt = 1
         ik = ik1
         do 10 k = 1, nac
            sum = sum + a(ik) * b(kpt)
            kpt = kpt + ibc
            ik = ik + iar
10       continue
         c(ipt) = sum
         ipt = ipt + icc
         ik1 = ik1 + iac
20    continue
      return
      end
*mdc*elseif titan
*      subroutine mxva(a,iac,iar, b,ibc, c,icc, nar,nac)
*      implicit integer(a-z)
*      integer iac, iar, ibc, icc, nar, nac
*      real*8 a(*), b(*), c(*)
*c
*      integer ik1, ipt, i, kpt, ik, k
*      real*8    zero,    sum
*      parameter(zero=0d0)
*c
*c     16-feb-90 tuned for 16Mhz R2000 single processor. -rls
*      if ( nar*nac .ge. 400 ) then
*c        # interface to library mxma().
*         call mxma(a,iac,iar, b,ibc,1, c,icc,1, nar,nac,1)
*      else
*         ik1 = 1
*         ipt = 1
*c$doit   vbest
*         do 20 i = 1, nar
*            sum = zero
*            kpt = 1
*            ik = ik1
*            do 10 k = 1, nac
*               sum = sum + a(ik) * b(kpt)
*               kpt = kpt + ibc
*               ik = ik + iar
*10          continue
*            c(ipt) = sum
*            ipt = ipt + icc
*            ik1 = ik1 + iac
*20       continue
*      endif
*      return
*      end
*mdc*else
*      subroutine mxva(a,iac,iar, b,ibc, c,icc, nar,nac)
*c
*c  fortran version of cray mxva to calculate c=a*b.
*c  note: *** this code should not be modified ***
*c
*c  arguments:
*c    a    = first input matrix with effective dimensions a(nar,nac).
*c    iac  = spacing between consecutive elements in a column of a.
*c    iar  = spacing between consecutive elements in a row of a.
*c    b    = input vector with effective dimensions b(nac).
*c    ibc  = spacing between consecutive elements in a column of b.
*c    c    = output vector with effective dimensions c(nar).
*c    icc  = spacing between consecutive elements in a column of c.
*c    nar  = number of rows in a and c.
*c    nac  = number of columns in a and rows in b.
*c
*c  see also mxma() documentation.
*c
*c  written 04-jan-85 by ron shepard.
*c
*      implicit none
*c
*      integer iac, iar, ibc, icc, nar, nac
*      real*8 a(*), b(*), c(*)
*c
*      integer ipt, i, i1k, kpt, k, ik
*      real*8     zero,     bk
*      parameter (zero=0d0)
*c
*c  initialize the output vector c(*).
*c
*      ipt = 1
*      do 100 i = 1, nar
*         c(ipt) = zero
*         ipt = ipt + icc
*100   continue
*c
*c  this version uses an outer product algorithm to take advantage of
*c  zeros in the vector b(*); i.e. the innermost do-loop is a saxpy.
*c  the loop ordering in this version results in sequential access of
*c  all the matrices for iac=ibc=icc=1 cases.
*c
*      i1k = 1
*      kpt = 1
*      do 400 k = 1, nac
*         bk = b(kpt)
*         if ( bk .ne. zero ) then
*            ik  = i1k
*            ipt = 1
*            do 300 i = 1, nar
*               c(ipt) = c(ipt) + a(ik) * bk
*               ipt = ipt + icc
*               ik  = ik + iac
*300         continue
*         endif
*         i1k = i1k + iar
*         kpt = kpt + ibc
*400   continue
*c
*      return
*      end
*mdc*endif
*deck blas
c  the following are selected blas routines with rolled loops.
c  these are for inlining on the cray and other vector machines.
c  the standard sblas <-> dblas conversion scripts may be used with
c  these routines.
*deck daxpy
*mdc*if (cray .and. inline) or (alliant and fx8) or fujitsu
*      subroutine daxpy(n,da,dx,incx,dy,incy)
*c
*c     constant times a vector plus a vector.
*c     uses rolled loops for increments equal to one.
*c     written by ron shepard, based on daxpy written by
*c     jack dongarra, linpack, 3/11/78.
*c
*      real*8 dx(*),dy(*),da
*      integer i,incx,incy,ix,iy,m,mp1,n
*c
*      real*8    zero
*      parameter(zero=0d0)
*c
*      if ( n.le.0 ) return
*      if ( da .eq. zero ) return
*      if ( incx.eq.1.and.incy.eq.1 ) go to 20
*c
*c        code for unequal increments or equal increments
*c          not equal to 1
*c
*      ix = 1
*      iy = 1
*      if ( incx.lt.0 ) ix = (-n+1)*incx + 1
*      if ( incy.lt.0 ) iy = (-n+1)*incy + 1
*      do 10 i = 1, n
*         dy(iy) = dy(iy) + da*dx(ix)
*         ix = ix + incx
*         iy = iy + incy
*10    continue
*      return
*c
*c        code for both increments equal to 1
*c
*20    continue
*      do 50 i = 1, n
*         dy(i) = dy(i) + da*dx(i)
*50    continue
*      return
*      end
*mdc*endif
*deck dcopy
*mdc*if ( cray .and. inline) or fujitsu
*      subroutine  dcopy(n,dx,incx,dy,incy)
*c
*c     copies a vector, x, to a vector, y.
*c     uses rolled loops for increments equal to one.
*c     written by ron shepard, based on dcopy written by
*c     jack dongarra, linpack, 3/11/78.
*c
*      real*8 dx(*),dy(*)
*      integer i,incx,incy,ix,iy,m,mp1,n
*c
*      if ( n.le.0 ) return
*      if ( incx.eq.1 .and. incy.eq.1 ) go to 20
*c
*c        code for unequal increments or equal increments
*c          not equal to 1
*c
*      ix = 1
*      iy = 1
*      if ( incx.lt.0 ) ix = (-n+1)*incx + 1
*      if ( incy.lt.0 ) iy = (-n+1)*incy + 1
*      do 10 i = 1, n
*         dy(iy) = dx(ix)
*         ix = ix + incx
*         iy = iy + incy
*10    continue
*      return
*c
*c        code for both increments equal to 1
*c
*20    continue
*      do 50 i = 1, n
*         dy(i) = dx(i)
*50    continue
*      return
*      end
*mdc*endif
*deck ddot
*mdc*if (cray .and. inline) or fujitsu
*      function ddot(n,dx,incx,dy,incy)
*c
*c     forms the dot product of two vectors.
*c     uses rolled loops for increments equal to one.
*c     written by ron shepard, based on ddot written by
*c     jack dongarra, linpack, 3/11/78.
*c
*      real*8 dx(*),dy(*),dtemp,ddot
*      integer i,incx,incy,ix,iy,m,mp1,n
*c
*      ddot = (0)
*      dtemp = (0)
*      if ( n.le.0 ) return
*      if ( incx.eq.1 .and. incy.eq.1 ) go to 20
*c
*c        code for unequal increments or equal increments
*c          not equal to 1
*c
*      ix = 1
*      iy = 1
*      if ( incx.lt.0 ) ix = (-n+1)*incx + 1
*      if ( incy.lt.0 ) iy = (-n+1)*incy + 1
*      do 10 i = 1, n
*         dtemp = dtemp + dx(ix)*dy(iy)
*         ix = ix + incx
*         iy = iy + incy
*10    continue
*      ddot = dtemp
*      return
*c
*c        code for both increments equal to 1
*c
*20    continue
*      do 50 i = 1, n
*         dtemp = dtemp + dx(i)*dy(i)
*50    continue
*      ddot = dtemp
*      return
*      end
*mdc*endif
*deck dscal
*mdc*if ( cray .and. inline) or fujitsu
*      subroutine  dscal(n,da,dx,incx)
*c
*c     scales a vector by a constant.
*c     uses rolled loops for increment equal to one.
*c     written by ron shepard, based on dscal written by
*c     jack dongarra, linpack, 3/11/78.
*c
*      real*8 da,dx(*)
*      integer i,incx,m,mp1,n,nincx
*c
*      if ( n.le.0 ) return
*      if ( incx.eq.1 ) go to 20
*c
*c        code for increment not equal to 1
*c
*      nincx = n*incx
*      do 10 i = 1, nincx, incx
*         dx(i) = da*dx(i)
*10    continue
*      return
*c
*c        code for increment equal to 1
*c
*20    continue
*      do 50 i = 1, n
*         dx(i) = da*dx(i)
*50    continue
*      return
*      end
*mdc*endif
*deck wzero
      subroutine wzero(n,a,inca)
c
      implicit none
      integer n, inca
c
      integer ia, i
      real*8    zero
      parameter(zero=0.d0)
*mdc*if titan
*      real*8 a(*)
*      if ( inca .eq. 1 ) then
*         do 10 i = 1, n
*            a(i) = zero
*10       continue
*      else
*         do 20 i = 1, (n*inca), inca
*            a(i) = zero
*20       continue
*      endif
*mdc*elseif (ibmmvs .and. osu )
*      real*8 a(inca,*)
*         do 10 i = 1, n
*            a(1,i) = zero
*10       continue
*mdc*else
      real*8 a(*)
      ia = 1
      do 10 i = 1, n
         a(ia) = zero
         ia    = ia + inca
10    continue
*mdc*endif
c
      return
      end
*deck expnds
      subroutine expnds( n, a, b )
      implicit none
c
c     expand the symmetric, lower-triangular packed, matrix a(*)
c     into b(*,*).
c
      integer n
      real*8 a(*), b(n,n)
c
      integer i, j, ii
      real*8 xx
c     19-nov-89  carry-around scalar, ii, eliminated. -rls
c
      b(1,1) = a(1)
c
cdir$  ivdep
c$doit ivdep  #titan
cvd$   ivdep
*vocl loop,novrec
c
      do 20 i = 2, n
         ii = (i*(i-1))/2
c
cdir$    ivdep
c$doit   ivdep  #titan
cvd$     ivdep
*vocl loop,novrec
c
         do 10 j = 1, i
            xx     = a(ii+j)
            b(i,j) = xx
            b(j,i) = xx
10       continue
20    continue
c
      return
      end
