      subroutine rdstvc(
     & aoints,  info,    buffer,  values,
     & labels,  nsym,    nbpsy,   mapin,
     & nnbft,   stvc,    score,   tcore,
     & vcore,   vecore,  symb,    ierr )
c
c  read the overlap, kinetic energy, potential energy, and
c  effective  arrays.
c
c  on exit, the integral file is positioned after the
c  last 1-e integral record.
c
c  input:
c  aoints = input file unit number.
c  info(*) = info array for this file.
c  buffer(1:l1rec) = record buffer.
c  values(1:n1max) = value buffer.
c  labels(1:2,1:n1max) = orbital label buffer.
c  nsym = number of symmetry blocks.
c  nbpsy(*) = number of basis functions per symmetry block.
c  mapin(*) = input_ao-to-ao mapping vector.
c  nnbft = leading dimension of stvc(*,1:3).
c
c  output:
c  stvc(*) = the overlap s1(*) matrix is returned in stvc(*,1), the
c           total kinetic matrix T1_total(*) is returned in stvc(*,2),
c           the 1-e potential matrix V1_total(*) is returned in
c           stvc(*,3), and any remaining hamiltonian contributions are
c           returned in stvc(*,4).
c           all arrays are returned symmetry-blocked lower-triangle
c           packed by rows.
c           all 1-e contributions on the integral file are summed
c           into this array.  consequently, the entries on the file
c           must be only the distinct array elements.
c  score  = frozen core contribution. tr( s1 * dfc ) = nfrzct
c  tcore  = frozen core contribution. tr( t1 * dfc )
c  vcore  = frozen core contribution. tr( v1 * dfc )
c  vecore = frozen core contribution. tr( h1 * dfc )
c  symb(1:nbft) = symmetry index of each basis function
c  ierr = error return code.
c       =  0 for normal return.
c       = -1 if no arrays were found on the integral file.
c       = -n if n symmetry blocking errors were detected.
c       >  0 for iostat error.
c
c  25-may-91 rdstvc() created from modified sifstv(). -rmp/rls
c  31-oct-90 sifstv() created from sifrsh(). -rls
c  08-oct-90 (columbus day) 1-e fcore change.  sifr1n() interface
c            used. ierr added. -rls
c  04-oct-90 sifskh() call added. -rls
c  26-jul-89 written by ron shepard.
c
      implicit logical(a-z)
c
      integer ia,ib
      integer  aoints, nsym,   nnbft,  ierr
      integer  info(*),        nbpsy(nsym),    labels(2,*),
     & mapin(*),       symb(*)
      real*8   score,  tcore,  vcore,  vecore
cmckim start
c      real*8   buffer(*),      values(*),      stvc(nnbft,4)
      real*8   buffer(*),      values(*),      stvc(nnbft,7),
     &         rtotbasis, sover, xnorm
      integer nbft, ntotbasis, maxdistinct
      parameter( maxdistinct=10)
      integer nuarep, nusoc, ndistinct,
     &        nequivalent(maxdistinct), nbasis(maxdistinct)
      integer lensoc
      integer ntribasis
cmckim end
c
c     # local...
      integer    itypea,   btypmx
      parameter( itypea=0, btypmx=6 )
c
      integer i, j,   nntot,  isym,   nrec,   last,   lastb,  lasta
      integer symtot(36), idummy(1), btypes(0:btypmx)
      real*8  fcore(7), socx, socy, socz
c
c     # bummer error types.
      integer   wrnerr,  nfterr,  faterr
      parameter(wrnerr=0,nfterr=1,faterr=2)
c
      integer nndxf
c
c     # set the btypes(*) array:
c     #            0:s1, 1:t1, 2:v1, 3:vec, 4:vfc, 5:vref, 6:generic_h1
      data btypes/ 1,    2,    3,    4,     4,     4,      4          /
c
      nndxf(i) = (i * (i - 1)) / 2
c
c     # nntot = the actual number of elements in the arrays.
      nntot=0
      do 20 isym=1,nsym
         nntot = nntot + nndxf( nbpsy(isym) + 1 )
20    continue
c
      if ( nntot .gt. nnbft ) then
c        # inconsistent nnbft value.
         call bummer('rdstvc: (nntot-nnbft)=',(nntot-nnbft),wrnerr)
         ierr = -2
         return
      endif
c
c     # initialize the output arrays...
c
      call wzero( nntot, stvc(1,1), 1 )
      call wzero( nntot, stvc(1,2), 1 )
      call wzero( nntot, stvc(1,3), 1 )
      call wzero( nntot, stvc(1,4), 1 )
cmckim start
      call wzero( nntot, stvc(1,5), 1 )
      call wzero( nntot, stvc(1,6), 1 )
      call wzero( nntot, stvc(1,7), 1 )
cmckim end
      fcore(1) = (0)
      fcore(2) = (0)
      fcore(3) = (0)
      fcore(4) = (0)
cmckim start
      fcore(5) = (0)
      fcore(6) = (0)
      fcore(7) = (0)
cmckim end
c
      call sifr1nmc(
     & aoints, info,   itypea, btypmx,
     & btypes, buffer, values, labels,
     & nsym,   nbpsy,  idummy, mapin,
     & nnbft,  stvc,   fcore,  symb,
     & symtot, lasta,  lastb,  last,
     & nrec,   ierr )
cmckim start
      print*,'AREP / ECP ints (non-zero only)'
      j=4
      i=0
      do 120 ia=1,28
         do 726 ib=1,ia
            i=i+1
      if(stvc(i,j).ne.0.0d0) write(6,68) ia, ib,stvc(i,j)
726   continue
120   continue
c     do 120 j=1,7
c     do 726 i=1,nnbft
c     if(stvc(i,j).ne.0.0d0) write(6,68) i, j,stvc(i,j)
c726  continue
c120  continue
  68  format(i5,2x,i5,10x,f20.16)
c 68  format(i5,2x,i5,10x,f14.6)
      nuarep=17
      open(unit=nuarep,access='sequential',form='unformatted',
     &     file='arepints',status='unknown')
      rtotbasis=(-1.0d0+SQRT(1.0d0+8.0d0*nnbft))/2.0d0
c     ntotbasis=NINT(rtotbasis) is equivalent with
      ntotbasis=NINT(rtotbasis)
      ntribasis=ntotbasis*(ntotbasis+1)/2
      if(nnbft.eq.ntribasis)then
         write(nuarep) nnbft
         call wrt( nnbft, stvc(1,4), nuarep)
         endfile(nuarep)
         close(nuarep)
         lensoc=2*ntotbasis*ntotbasis
         if(lensoc.le.4*nnbft)then
            nusoc=18
            open(unit=nusoc,access='sequential',form='unformatted',
     &           file='socints',status='unknown')
            write(nusoc) ntotbasis, lensoc
            call xyzsoc( ntotbasis, nnbft, stvc(1,1), stvc(1,5))
            call wrt( lensoc, stvc(1,1), nusoc)
            endfile(nusoc)
            close(nusoc)
         else
            print *, ' Error : lensoc.gt.4*nnbft'
         endif
      else
         write(6,9150) nnbft, ntribasis
         stop
      endif

cmckim end
c
c     # save the appropriate core values.
      score  = fcore(1)
      tcore  = fcore(2)
      vcore  = fcore(3)
      vecore = fcore(4)
cmckim start
      socx   = fcore(5)
      socy   = fcore(6)
      socz   = fcore(7)
cmckim end
c
 9110 format(' Number of Distinct Atoms ', i3)
 9120 format(' # of Eq. Atoms of Ith Distinct Atom ', 10i3)
 9130 format(' # of Eq. Bases of Ith Distinct Atom ', 10i3)
 9140 format(' Total # of Bases ', i4)
 9150 format(' Error :     nnbft ', i4,
     &       ' not equal to ntribasis ', i4)
      return
      end
