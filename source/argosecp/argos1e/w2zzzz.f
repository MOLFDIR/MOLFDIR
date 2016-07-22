*deck w2zzzz
*deck w2stup
*deck wtlab2
      subroutine w2zzzz( last, buffer, values, labels, ibitv )
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
      integer       l2rec
      equivalence ( l2rec, info(4) )
      integer       n2max
      equivalence ( n2max, info(5) )
c
      integer    nipv
      parameter( nipv=4 )
c
c     # dummy:
      integer last, labels(nipv,*), ibitv(*)
      real*8 buffer(*), values(*)
c     # ibm+convex f77 bug; actual:
c     # integer labels(nipv,n2max), ibitv( ((n2max+63)/64)*64 )
c     # real*8 buffer(l2rec), values(n2max)
c
c     # local:
      integer ierr
c
c     # bummer error types.
      integer   wrnerr,  nfterr,  faterr
      parameter(wrnerr=0,nfterr=1,faterr=2)
c
      integer    ibvtyp,   iwait
      parameter( ibvtyp=1, iwait=0 )
c
c     # reqnum is an asynchronous i/o handle.
c     # it must be saved between sifew2() and sif2w8() calls.
      integer reqnum
      save    reqnum
      data    reqnum / 0 /
c
c     # this routine is only to set up the entry points and should
c     # never be called.
      call bummer('w2zzzz called',0,faterr)
c
c**********************************************************************
      entry w2stup( ibitv )
c**********************************************************************
c
c     # perform initialization for the 2-e integral records.
c
      ibuf   = 0
      numout = 0
      nrec   = 0
c
      call izero( (((n2max+63)/64)*64), ibitv, 1 )
      return
c
c**********************************************************************
      entry wtlab2( last, buffer, values, labels, ibitv )
c**********************************************************************
c
c     # encode and write a 2-e integral record.
c
      numout = numout + ibuf
c
c     # wait for i/o on the last buffer to complete.
      if ( reqnum .ne. 0 ) then
        call sif2w8( ntape, info, reqnum, ierr )
c
        if ( ierr .ne. 0 ) then
          call bummer('wtlab2: from sif2w8(), ierr=',ierr,faterr)
        endif
      endif
c
c     # encode and initiate the write of the record.
c     # [this call returns reqnum associated with the i/o request.]
c
      call sifew2( ntape,  info,   nipv,   ibuf,   last,   itypea,
     &     itypeb, ifmt2,  ibvtyp, values, labels, ibitv,  buffer,
     &     iwait,  nrec,   reqnum, ierr )
c
      if ( ierr .ne. 0 ) then
        call bummer('wtlab2: from sifew2(), ierr=',ierr,faterr)
      endif
c
c     # reset numout and ibitv(*), allowing for partial record writes.
      numout = numout - ibuf
c
      call izero( (n2max-ibuf), ibitv(ibuf+1), 1 )
c
      return
      end
