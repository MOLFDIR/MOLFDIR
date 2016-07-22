*deck argos
      program argos
      implicit logical(a-z)
c
c  allocate space and call the driver routine.
c
c  ifirst = first usable space in the the real*8 work array a(*).
c  lcore  = length of workspace array in working precision.
c  mem1   = additional machine-dependent memory allocation variable.
c  a(*)   = workspace array. a(ifirst : ifirst+lcore-1) is useable.
c
      integer ifirst, lcore
c
c     # mem1 and a(*) should be declared below within mdc blocks.
c
c     # local...
c     # lcored = default value for lcore.
c     # ierr   = error return code.
c
      integer lcored, ierr
c
c     # bummer error types.
      integer   wrnerr,  nfterr,  faterr
      parameter(wrnerr=0,nfterr=1,faterr=2)
c
c     # "call pbginf" should be the first executable statement.  this
c     # sets up any communications necessary for parallel execution,
c     # and does nothing in the sequential case.
c
*mdc*if unicos
*c
*c     # nonstandard unicos memory allocation.
*c     # mem1 is a pointer to the beginning of a(*).
*c
*      parameter (lcored = 1 800 000)
*      pointer (mem1, a)
*      real*8 a(2)
*c
*      call pbginf
*c
*      call getlcl( lcored, lcore, ierr )
*      if ( ierr .ne. 0 ) then
*         call bummer('argos: from getlcl: ierr=',ierr,faterr)
*      endif
*c
*      call hpalloc( mem1, lcore, ierr, 1 )
*      ifirst = 1
*mdc*elseif alliant
*c
*c     # fortran 90 style core allocation.
*c
*      integer mem1
*      parameter (lcored = 1 800 000)
*      real*8 a(:)
*c
*      call pbginf
*c
*      call getlcl( lcored, lcore, ierr )
*      if ( ierr .ne. 0 ) then
*         call bummer('argos: from getlcl: ierr=',ierr,faterr)
*      endif
*c
*      allocate( a(lcore) )
*c
*      mem1   = 0
*      ifirst = 1
*mdc*elseif sun iris convex
c
c     # nonstandard sun bsd unix memory allocation.
c     # mem1 is a pointer to the beginning of a(*).
c
      integer mem1
      parameter (lcored = 1 800 000)
      pointer (mem1, a)
      real*8 a(2)
c
c     integer  malloc
c     external malloc
c
      call pbginf
c
      call getlcl( lcored, lcore, ierr )
      if ( ierr .ne. 0 ) then
         call bummer('argos: from getlcl: ierr=',ierr,faterr)
      endif
c
c     #  malloc() argument is assumed to be in bytes.
      mem1 = malloc( 8*lcore )
      if ( mem1 .eq. 0 ) then
         call bummer('argos: from malloc: mem1=',mem1,faterr)
      endif
      ifirst = 1
*mdc*elseif (fps .and. obsolete)
*c
*c     # this code is for machines that support malloc(),
*c     # but do not support pointer syntax.
*c
*      integer mem1
*      parameter (lcored = 1 800 000)
*c
*      integer  malloc
*      external malloc
*c
*      call pbginf
*c
*      call getlcl( lcored, lcore, ierr )
*      if ( ierr .ne. 0 ) then
*         call bummer('argos: from getlcl: ierr=',ierr,faterr)
*      endif
*c
*c     #  malloc() argument is assumed to be in bytes.
*      mem1 = malloc( 8*lcore )
*      if ( mem1 .eq. 0 ) then
*         call bummer('argos: from malloc: mem1=',mem1,faterr)
*      endif
*      ifirst = 1
*mdc*elseif unix titan
*c
*c     # generic unix memory allocation using falloc() interface.
*c     # see also $COLUMBUS/special/unix/falloc.c
*c
*      integer mem1, offset
*      parameter (lcored =  1 800 000)
*      real*8 a(1)
*c
*      call pbginf
*c
*      call getlcl( lcored, lcore, ierr )
*      if ( ierr .ne. 0 ) then
*         call bummer('argos: from getlcl: ierr=',ierr,faterr)
*      endif
*c
*c     # on return from falloc(), a(offset+1:offset+lcore) is useable.
*      call falloc( lcore, 8, 0, a, mem1, offset )
*      if ( mem1 .eq. 0 ) then
*         call bummer('argos: from falloc: mem1=',mem1,faterr)
*      endif
*      ifirst = offset + 1
*mdc*else
*c
*c     # standard fortran workspace allocation.
*c     # mem1 is not used.  use blank common to reduce executable size.
*c
*      integer mem1
*      parameter ( lcored = 4 000 000 )
*      real*8    a
*      common // a( lcored )
*c
*      call pbginf
*c
*      call getlcl( lcored, lcore, ierr )
*      if ( ierr .ne. 0 ) then
*         call bummer('argos: from getlcl: ierr=',ierr,faterr)
*      elseif ( lcore .gt. lcored ) then
*c        # make sure lcore is consistent.  this check should be
*c        # redundant with logic within getlcl(), but is included
*c        # here just to be safe.
*         call bummer('argos: (lcore-lcored)=',
*     &    (lcore-lcored), faterr )
*      endif
*c
*      mem1   = 0
*      ifirst = 1
*mdc*endif
c
c     # lcore, mem1, and ifirst should now be defined.
c     # the values of lcore, mem1, and ifirst are passed to
c     # driver() to be printed.
c     # since these arguments are passed by expression, they
c     # must not be modified within driver().
c
*mdc*if (fps .and. obsolete)
*c
*c      # for machines that support malloc()-type calls, but do not
*c      # support pointer syntax,  the workaround is to pass the
*c      # address of a(*) by value.
*c
*      call driver( %val(mem1), (lcore), (mem1), (ifirst) )
*c
*mdc*else
c
      call driver( a(ifirst), (lcore), (mem1), (ifirst) )
c
*mdc*endif
c
c     # return from driver() implies successful execution.
c
c     # message-passing cleanup: stub if not in parallel
      call pend
c
      stop 'end of argos'
      end
