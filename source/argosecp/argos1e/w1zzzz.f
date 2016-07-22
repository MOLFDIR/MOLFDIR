*deck w1zzzz
*deck w1stup
*deck wtlab1
      subroutine w1zzzz( last, fcore, buffer, values, labels )
c
c  01-dec-90 SIFS version. -rls
c  11-aug-87 written by ron shepard.
c
      implicit logical(a-z)
c
c     # /bufout/ holds some output integral file parameters.
      integer
     & info,     ifmt1,   ifmt2,   itypea,  itypeb,
     & ibuf,     numout,  nrec,    ntape
      common /bufout/
     & info(10), ifmt1,   ifmt2,   itypea,  itypeb,
     & ibuf,     numout,  nrec,    ntape
c
      integer       l1rec
      equivalence ( l1rec, info(2) )
      integer       n1max
      equivalence ( n1max, info(3) )
c
      integer    nipv
      parameter( nipv=2 )
c
c     # dummy:
      integer last
      integer labels(nipv,*)
      real*8 fcore
      real*8 buffer(*), values(*)
c     # ibm+convex f77 bug; actual:
c     # integer labels(nipv,n1max)
c     # real*8 buffer(l1rec), values(n1max)
c
c     # local:
      integer ierr
      integer ibitv(1)
c
c     # bummer error types.
      integer   wrnerr,  nfterr,  faterr
      parameter(wrnerr=0,nfterr=1,faterr=2)
c
      integer    ibvtyp
      parameter( ibvtyp=0 )
c
c     # this routine is only to set up the entry points and should
c     # never be called.
      call bummer('w1zzzz called',0,faterr)
c
c**********************************************************************
      entry w1stup
c**********************************************************************
c
c     # perform initialization for the 1-e integral records.
c
      ibuf   = 0
      numout = 0
      nrec   = 0
      return
c
c**********************************************************************
      entry wtlab1( last, fcore, buffer, values, labels )
c**********************************************************************
c
c     # encode and write a 1-e integral buffer.
c
      numout = numout + ibuf
c
      call sifew1( ntape,  info,   nipv,   ibuf,   last,   itypea,
     &     itypeb, ifmt1,  ibvtyp, values, labels, fcore,  ibitv,
     &     buffer, nrec,   ierr )
c
      if ( ierr .ne. 0 ) then
        call bummer('wtlab1: from sifew1(), ierr=',ierr,faterr)
      endif
c
c     # allow for partial record writes.
      numout = numout - ibuf
c
      return
      end
