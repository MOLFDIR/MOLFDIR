*deck wthead
      subroutine wthead( nsym,   ns,     nbft,   ntitle, title,
     &   repnuc, ityp,   nbpsy,  mtype,  ms,     mnl,    info,
     &   aoints, nlist,  map )
c
c  write the header info to the output aoints file.
c
c  input:
c  nsym = number of symmetry blocks.
c  ns   = number of symmetry inequivalent atom labels.
c  nbft = total number of basis functions.
c  ntitle = number of titles.
c  title(1:ntitle) = titles.
c  repnuc = nuclear repulsion energy.
c  ityp(1:nsym) = input symmetry labels.
c  nbpsy(1:nsym) = number of basis functions in each symmetry block.
c  mtype(1:ns) = atom labels.
c  ms(1:nbft) = basis_function-to-center mapping vector.
c  mnl(1:nbft) = integer codes for basis function types.
c                1:s, 2:p, 3:3s, 4:d, 5:4p, 6:f, 7:5s, 8:5d, 9:g
c  info(*) = info(*) array for the output file.
c  aoints = integral output file unit number.
c  nlist = listing file output unit.
c  map(1:2,1:nbft) = temporary scratch array for writing the output
c                    map(*) vectors.
c
c  01-dec-90 SIFS version written by ron shepard.
c
      implicit logical(a-z)
c
      integer    nbfmx,     nmap,   ninfo,   nenrgy,   nrtype
      parameter( nbfmx=255, nmap=2, ninfo=5, nenrgy=1, nrtype=-1 )
c
c     # dummy:
      integer nsym, ns, nbft, ntitle, aoints, nlist
      character*80  title(1:ntitle)
      real*8        repnuc
      character*3   ityp(1:nsym)
      integer       nbpsy(1:nsym)
      character*3   mtype(1:ns)
      integer       ms(1:nbft)
      integer       mnl(1:nbft)
      integer       info(1:ninfo)
      integer       map(nbft,nmap)
c
c     # local:
c
c     # bummer error types.
      integer   wrnerr,  nfterr,  faterr
      parameter(wrnerr=0,nfterr=1,faterr=2)
c
      integer    bmin,   bmax
      parameter( bmin=0, bmax=17 )
c
      integer      clip, ierr, i, j
      character*8  bfnlab(nbfmx)
      character*4  slabel(8)
      real*8       energy(nenrgy)
      integer      ietype(nenrgy)
      integer      imtype(nmap)
      character*2  btype(bmin:bmax)
c
      real*8   sifsce
      external sifsce
c
c     # map vector types.
      data imtype/ 3, 4 /
c
c     # basis function types.
      data btype/ '??', '_s', '_p', '3s', '_d', '4p', '_f', '5s', '5d',
     &      '_g', '6p', '6f', '_h', '7s', '7d', '7g', '_i', '??' /
c
c     # this routine is a bit of a hack.  the sifs data should
c     # be more intimately incorporated into argos. -rls
c
      if ( nbft .gt. nbfmx ) then
c       # this should eventually be handled more gracefully.
c       # the only problem is the bfnlab(*) array, which should
c       # really be a dummy argument anyway. -rls
        call bummer('wthead: (nbft-nbfmx)=', (nbft-nbfmx), faterr )
      endif
c
c     # set map(*,*) and create bfnlab(*) from the input arrays.
c
      do 20 i = 1, nbft
        map(i,1) = ms(i)
        map(i,2) = mnl(i)
        clip = min( max( bmin, mnl(i) ), bmax )
        write( bfnlab(i), fmt='(i3,a3,a2)' )
     &   i, mtype(ms(i)), btype(clip)
        do 10 j = 4, 8
c         # replace embedded spaces with a printing character to
c         # avoid confusion when printing labels later.
          if ( bfnlab(i)(j:j) .eq. ' ' ) then
            bfnlab(i)(j:j) = '_'
          endif
10      continue
20    continue
c
c     # create the output symmetry labels.
c
      do 40 i = 1, nsym
        slabel(i)(1:1) = ' '
        slabel(i)(2:4) = ityp(i)
40    continue
c
c     # the info(*) entries were passed in.
c
c     info(1) = fsplit
c     info(2) = l1rec
c     info(3) = n1max
c     info(4) = l2rec
c     info(5) = n2max
c
c     # setup for a single nuclear repulsion energy.
c
      energy(1) = repnuc
      ietype(1) = nrtype
c
c     # write the headers.
c
      call sifwh( aoints,  ntitle,  nsym,   nbft,   ninfo,   nenrgy,
     &   nmap,    title,   nbpsy,   slabel, info,   bfnlab,  ietype,
     &   energy,  imtype,  map,     ierr )
c
      if ( ierr .ne. 0 ) then
        call bummer('wthead: from sifwh(), ierr=', ierr, faterr )
      endif
c
      write(nlist,6100)'output SIFS file header information:'
      write(nlist,'(1x,a)') (title(i),i=1,ntitle)
c
      write(nlist,6100)'output energy(*) values:'
      call sifpre( 6, nenrgy, energy, ietype )
c
      write(nlist,6200)'total core energy =',
     & sifsce( nenrgy, energy, ietype )
c
      write(nlist,6040) nsym, nbft
c
      write(nlist,6050)'symmetry  =',(i,i=1,nsym)
      write(nlist,6060)'slabel(*) =',(slabel(i),i=1,nsym)
      write(nlist,6050)'nbpsy(*)  =',(nbpsy(i),i=1,nsym)
c
      write(nlist,6100)'info(*) =',(info(i),i=1,ninfo)
c
      write(nlist,6100)'output orbital labels, i:bfnlab(i)='
      write(nlist,6080)(i,bfnlab(i),i=1,nbft)
c
      write(nlist,6100)'bfn_to_center map(*), i:map(i)'
      write(nlist,6110) (i,map(i,1),i=1,nbft)
c
      write(nlist,6100)'bfn_to_orbital_type map(*), i:map(i)'
      write(nlist,6110) (i,map(i,2),i=1,nbft)
c
6040  format(/' nsym =',i2,' nbft=',i4/)
6050  format(1x,a,8i5)
6060  format(1x,a,8(1x,a4))
6080  format(10(i4,':',a8))
6100  format(/1x,a,(1x,10i10))
6110  format(10(i4,':',i3))
6200  format(/1x,a,(1x,1p4e20.12))
c
      return
      end
