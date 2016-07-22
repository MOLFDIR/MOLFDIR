      subroutine out1e(
     & l1rec,  n1max,  buffer,  values,
     & labels, stvc,   info,    ms,
     & mnl,    slabel, bfnlab,  nbufo,
     & spo,    lblij,  plabs )
c
c  read in the stvc(*,*) array and write the output file header and
c  1-e integral arrays.
c
c  25-may-91 SIFS version. -rmp/rls
c
      implicit logical(a-z)
c
      integer     nlist, ifile, ifile2, nfile, nnam, idebug
      common /c1/ nlist, ifile, ifile2, nfile, nnam, idebug
c
      integer    ntitmx,    nbfmx
      parameter( ntitmx=20, nbfmx=255 )
c
      character*80    title
      common /ctitle/ title(ntitmx)
c
      integer       ipq,        mapin,        mapout,        symb
      common /cipq/ ipq(nbfmx), mapin(nbfmx), mapout(nbfmx), symb(nbfmx)
c
      real*8 repnuc
      integer  nst,    ns,     isfr,   nu,     iau,    ntitle,
     & nso,    nsopr,          nblpr,          nnbft
      common /c2/
     & repnuc, nst,    ns,     isfr,   nu,     iau,    ntitle,
     & nso(8), nsopr(8),       nblpr(8),       nnbft
c
c     # dummy:
      integer l1rec, n1max, nbufo
      integer info(*), ms(isfr), mnl(isfr), lblij(nbufo)
      real*8 buffer(l1rec), values(n1max), labels(2,n1max),
cmckim start
c     & stvc(nnbft,0:3), spo(nbufo)
     & stvc(nnbft,0:6), spo(nbufo)
cmckim end
      character*4 slabel(nst)
      character*8 bfnlab(isfr)
c
c     # see dump1() for actual plabs(*) declaration.
      integer plabs(*)
c
c     # local:
      integer i, j, ij, ierr, ist, iso, is, itypeb, ibfld, isym, ni,
     & iblko, ibufo
      real*8 score, tcore, vcore, vecore
      character*8 chrtyp
c
      integer nd(8), lxyzir(3), inam(4)
      character*3 ityp(8), mtype(nbfmx)
c
      real*8     zero,     small
      parameter( zero=0d0, small=1.d-15 )
c
c     # bummer error types.
      integer    wrnerr,  nfterr,  faterr
      parameter (wrnerr=0,nfterr=1,faterr=2)
c
      integer    more,   nomore
      parameter( more=0, nomore=1 )
c
c     # dummy lxyzir(*) values, default inam(*) and nd(*) values:
      data lxyzir / 3*1 /
      data inam   / 1, 2, 3, 4 /
      data nd     / 8*1 /
c
      ierr   = 0
      score  = zero
      tcore  = zero
      vcore  = zero
      vecore = zero
c
c     # write out the ms(*) and mnl(*) map vectors:
c
      write(nlist,6100) 'bfn_to_center map(*), i:map(i)'
      write(nlist,6110) (i,ms(i),i=1,isfr)
c
      write(nlist,6100) 'bfn_to_orbital_type map(*), i:map(i)'
      write(nlist,6110) (i,mnl(i),i=1,isfr)
c
c     # read in the stvc(*,*) array.
c
      call rdstvc(
     & ifile,  info,   buffer, values,
     & labels, nst,    nso,    mapin,
     & nnbft,  stvc,   score,  tcore,
     & vcore,  vecore, symb,   ierr )
cmckim start
cc
c      write(nlist,6010) score, tcore, vcore, vecore
cc
c      if ( ierr .ne. 0 ) then
c         call bummer('stvc: from rdstvc(), ierr=', ierr, faterr )
c      endif
cc
cc     # write the output file header.
cc
cc     # check for nonzero effective core integrals.
cc     # set nu=3 for stv; nu=4 for stvc.
cc
c      nu = 3
c      do 100 i = 1, nnbft
c         if ( stvc(i,3) .ne. zero ) then
c            nu = 4
c            goto 101
c         endif
c100   continue
c101   continue
cc
cc     # set the character irrep labels.
c      do 200 i = 1, nst
c         ityp(i) = slabel(i)(2:4)
c200   continue
cc
cc     # set ns = number of symmetry-distinct centers.
cc     # set mtype(i) = character center-label for each distinct center.
c      ns = 0
c      do 300 i = 1, isfr
c         ns = max( ns, ms(i) )
c         mtype( ms(i) ) = bfnlab(i)(4:6)
c300   continue
cc
cc     # add in the core contributions from the 1-e hamiltonian arrays.
c      repnuc = repnuc + tcore + vcore + vecore
cc
cc     # first output header record:
c      write (nfile) title(1), repnuc, nst, ns, isfr, nu
cc
cc     # second output header record:
cc      write (nfile) (nd(ist),ist=1,nst), (ityp(ist), ist=1,nst),
cc     & (nso(ist),ist=1,nst), (mtype(is), is=1,ns),
cc     & (ms(iso),iso=1,isfr), (mnl(iso), iso=1,isfr),
cc     & (inam(i), i=1,nu), lxyzir
cc
cc     # write out the output 1-e records:
cc
c      do 700 itypeb = 0, (nu-1)
cc
c         call siftyp( 0, itypeb, chrtyp )
cc
c         if ( idebug .ge. 1 ) then
cc           # write out the array.
c            call plsbkc( 'Total ' // chrtyp // ' array:',
c     &       stvc(1,itypeb), nst, slabel, nso, bfnlab, 2, nlist )
c         endif
cc
c         iblko = 0
c         ibufo = 0
c         ibfld = 0
cc
c         ij = 0
c         do 600 isym = 1, nst
c            ni = nso(isym)
c            do 500 i = 1, ni
c               do 400 j = 1, i
c                  ij = ij + 1
cc
c                  if ( abs(stvc(ij,itypeb)) .gt. small ) then
c                     if ( ibufo .eq. nbufo ) then
cc
cc                       # buffers are full. pack the labels and
cc                       # dump to the output file.
cc
c                        call dump1(
c     &                   nfile,  more,   ibufo,  nnbft,
c     &                   nbufo,  spo,    lblij,  plabs )
cc
c                        ibfld = ibfld + 1
c                        ibufo = 0
c                     endif
c                     ibufo        = ibufo + 1
c                     lblij(ibufo) = ij
c                     spo(ibufo)   = stvc(ij,itypeb)
c                  endif
c400            continue
c500         continue
c600      continue
cc
cc        # dump the last record of this type.
cc
c         call dump1(
c     &    nfile,  nomore, ibufo,  nnbft,
c     &    nbufo,  spo,    lblij,  plabs )
cc
c         ibufo = ibfld * nbufo + ibufo
c         ibfld = ibfld + 1
c         write(nlist,6200) ibufo, chrtyp, ibfld
c700   continue
cc
cmckim end
      return
6010  format(1p/' out1e:',
     & t10,'score  =',e25.15/
     & t10,'tcore  =',e25.15/
     & t10,'vcore  =',e25.15/
     & t10,'vecore =',e25.15)
6100  format(/1x,a,(1x,10i10))
6110  format(10(i4,':',i3))
6200  format(' out1e:',i12,1x,a,' integrals written in',i3,' records.')
      end
