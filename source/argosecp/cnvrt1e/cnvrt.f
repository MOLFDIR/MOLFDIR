      program cnvrt
c
c  this program transforms the integral file into one that is
c  compatible with the scf program by changing the indices or labels
c  of the one-electron integrals and by calculating the p and k/2
c  elements for the two-electron integrals.
c
c  version log:
c  11-sep-91 parallel version included in columbus distribution. -rjh
c  07-jul-91 preliminary parallel version using tcgmsg. -rjh
c  25-may-91 sifs version. minor cleanup. -rmp/rls
c  08-90 allocate arrays (rmp)
c  08-89 mdc update, zero-out improvement (rmp,mpn)
c  08-88 add sun, revise for distribution (eas,rmp)
c  1983 (cnvrt)   major revisions, r. m. pitzer
c  1981 (convert) major revisions, f. b. brown (i. shavitt group)
c  1979 (soconv)  y. yamaguchi & b. brooks (h. f. schaefer group)
c
c  cmdc info:
c  keyword   description
c  -------   -----------
c  ibm       ibm code.
c  vax       vax code.
c  sun       sun code.
c  stellar   stellar code.
c  harris    harris code.
c  cray      cray code. use compiler option for no double precision.
c  crayctss  ctss specific code.
c  parallel  tcgmsg parallel code.
c
      implicit logical(a-z)
c
      integer    ntitmx,    nbfmx,     ninfmx,    nengmx,    nmapmx
      parameter( ntitmx=20, nbfmx=255, ninfmx=20, nengmx=40, nmapmx=20 )
c
      integer     nlist, ifile, ifile2, nfile, nnam, idebug
      common /c1/ nlist, ifile, ifile2, nfile, nnam, idebug
c
      real*8 repnuc
      integer  nst,    ns,     isfr,   nu,     iau,    ntitle,
     & nso,    nsopr,          nblpr,          nnbft
      common /c2/
     & repnuc, nst,    ns,     isfr,   nu,     iau,    ntitle,
     & nso(8), nsopr(8),       nblpr(8),       nnbft
c
      character*80    title
      common /ctitle/ title(ntitmx)
c
      integer       ipq,        mapin,        mapout,        symb
      common /cipq/ ipq(nbfmx), mapin(nbfmx), mapout(nbfmx), symb(nbfmx)
c
c     # core(1:lencor) is used for miscellaneous  workspace.
      integer    lencor
      parameter( lencor=1 000 000 )
c
      real*8 core(lencor)
c
      character*8 bfnlab(nbfmx)
      real*8 energy(nengmx)
      integer ietype(20), imtype(nmapmx), info(20)
      character*4  slabel(8)
c
      integer i, buffer, values, labels, l1rec,
     & n1max, l2rec, n2max, mnl, ms, map, ninput, isto,
     & ninfo, ierr, nmap, nenrgy, isft, itotal, stvc, ibitv, itemp,
     & spo, lblij, plabs, psup, ksup, ilbl, klbl
      character*60 fname
c
c     # bummer error types.
      integer    wrnerr,  nfterr,  faterr
      parameter (wrnerr=0,nfterr=1,faterr=2)
c
c     # nbufo1 = number of 1-e values.
c     # nbufo2 = number of 2-e values.
      integer    nbufo1,      nbufo2
*mdc*if cray
*      parameter( nbufo1=4095, nbufo2=2730 )
*mdc*else
      parameter( nbufo1=2304, nbufo2=1152 )
*mdc*endif
c
      integer  atebyt, forbyt
      external atebyt, forbyt
c
      real*8   sifsce
      external sifsce
c
      integer  nodeid
      external nodeid
c
c     # parallel message passing initialization, or stub as appropriate.
      call pbginf
c
*mdc*if crayctss
*      call link('unit99=tty//')
*mdc*endif
c
c     # open the output listing file.  write to file 'cnvrtls'.
c
      nlist=6
*mdc*if harris ibm
*c     # use preconnected unit 6
*mdc*elseif parallel
*      fname = 'cnvrtls'
*      call trnfln( 1, fname )
*      call pfname( 1, fname )
*      open ( nlist, file=fname, status='unknown' )
*mdc*else
      fname = 'cnvrtls'
      call trnfln( 1, fname )
      open ( nlist, file=fname, status='unknown' )
*mdc*endif
c
      call ibummr( nlist )
c
c     # open the input file.  read from file 'cnvrtin'.
c
      ninput=5
*mdc*if harris
*c     # use preconnected unit 5
*mdc*elseif parallel
*      fname = 'cnvrtin'
*      call trnfln( 1, fname )
*      if ( nodeid() .ne. 0 ) call pfname( 1, fname )
*c     # this distributes the input file to all nodes.
*      call pfcopy( 99, 0, fname )
*      open ( ninput, file=fname, status='old' )
*mdc*else
c     # translate the filename and open.
      fname = 'cnvrtin'
      call trnfln( 1, fname )
      open ( ninput, file=fname, status='old' )
*mdc*endif
c
      nmap   = 0
      ierr   = 0
      ninfo  = 0
      isft   = 0
      nenrgy = 0
c
      isto   = 0
      ifile  = 4
      nfile  = 3
      idebug = 0
      ifile2 = 8
c-      read (ninput,'(26i3)') isto, ifile, nfile, idebug
      read (ninput,*) isto, ifile, nfile, idebug, ifile2
c
cmckim start
c     close (ninput)
cmckim end
c
      if ( ifile  .eq. 0 ) ifile  = 4
      if ( ifile2 .eq. 0 ) ifile2 = 8
      if ( nfile  .eq. 0 ) nfile  = 3
c
      write (nlist,10)
   10 format('1',28x,'program "cnvrt" 4.1b1'/
     1 29x,'columbus program system'/
     2 29x,'supermatrix formation program'//
     & 29x,'Programmed by Russell M. Pitzer'/
     3 29x,'version date: 11-sep-91')
c
      call who2c( 'CNVRT', nlist )
c
c     # open ifile.  read from file 'aoints'.
c     # open nfile.  write to file 'pkints'.
c
      fname = 'aoints'
      call trnfln( 1, fname )
*mdc*if parallel
*      call pfname( 1, fname )
*mdc*endif
      open (ifile, file=fname, status='old', form='unformatted')
c
      fname = 'pkints'
      call trnfln( 1, fname )
*mdc*if parallel
*      call pfname( 1, fname )
*mdc*endif
      open (nfile,file=fname, status='unknown', form='unformatted')
c
      write (nlist,40) ifile, nfile
   40 format(/' integrals are read in from file',i3,' (ifile).'/
     1  ' integrals are written out on file',i3,' (nfile).'//)
c
      call sifrh1(
     & ifile,  ntitle, nst,    isfr,
     & ninfo,  nenrgy, nmap,   ierr )
c
      if ( ierr .ne. 0 ) then
         call bummer('from sifrh1, ierr=', ierr, faterr )
      elseif ( ntitle .gt. ntitmx ) then
         call bummer('from sifrh1, ntitle=', ntitle, faterr )
      elseif ( isft .gt. nbfmx ) then
         call bummer('from sifrh1, isfr=', isfr, faterr )
      elseif ( ninfo .gt. ninfmx ) then
         call bummer('from sifrh1, ninfo=', ninfo, faterr )
      elseif ( nenrgy .gt. nengmx ) then
         call bummer('from sifrh1, nenrgy=', nenrgy, faterr )
      elseif ( nmap .gt. nmapmx ) then
         call bummer('from sifrh1, nmap=', nmap, faterr )
      endif
c
      call izero( 8, nso, 1 )
c
c     # read the second header record.
c     # core(*)==> mnl(1:isfr), ms(1:isfr), map(1:isfr,1:nmap)
c
      mnl = 1
      ms  = mnl + forbyt( isfr )
      map = ms  + forbyt( isfr )
c
      call sifrh2(
     & ifile,  ntitle, nst,    isfr,
     & ninfo,  nenrgy, nmap,   title,
     & nso,    slabel, info,   bfnlab,
     & ietype, energy, imtype, core(map),
     & ierr )
c
      if ( ierr .ne. 0 ) then
         call bummer('from sifrh2(), ierr=', ierr, faterr )
      endif
c
      write(nlist,6100) 'input SIFS file header information:'
      write(nlist,'(1x,a)') (title(i),i=1,ntitle)
c
      write(nlist,6100) 'input energy(*) values:'
      call sifpre( 6, nenrgy, energy, ietype )
c
      repnuc = sifsce( nenrgy, energy, ietype )
      write(nlist,6200) 'total core energy =', repnuc
c
      write(nlist,6040) nst, isfr
c
      write(nlist,6050) 'symmetry  =',(i,i=1,nst)
      write(nlist,6060) 'slabel(*) =',(slabel(i),i=1,nst)
      write(nlist,6050) 'nbpsy(*)  =',(nso(i),i=1,nst)
c
      write(nlist,6100) 'info(*) =',(info(i),i=1,ninfo)
c
      write(nlist,6100) 'input orbital labels, i:bfnlab(i)='
      write(nlist,6080) (i,bfnlab(i),i=1,isfr)
c
c     # initialize some addressing arrays.
c     # extract the bfn-to-center map vector, and
c     # the bfn-to-bfntyp map vector from map(*,*).
c
c     # core(*)==> mnl(1:isfr), ms(1:isfr), map(1:isfr,1:nmap)
c
      call getmap( nmap, imtype, core(map), core(mnl), core(ms) )
c
c     # extract some file and record length parameters.
      l1rec  = info(2)
      n1max  = info(3)
      l2rec  = info(4)
      n2max  = info(5)
c
c     # process the 1-e integral arrays.
c
c     # core(*)==> mnl(1:isfr), ms(1:isfr),
c     #            buffer(1:l1rec), values(1:n1max), labels(1:2,1:n1max)
c     #            stvc(1:nnbft,0:3), spo(1:nbufo1), lblij(1:nbufo1),
c     #            plabs(1:nbufo1)
      buffer = ms     + forbyt( isfr )
      values = buffer + atebyt( l1rec )
      labels = values + atebyt( n1max )
      stvc   = labels + forbyt( 2*n1max )
cmckim start
c      spo    = stvc   + atebyt( 4*nnbft )
      spo    = stvc   + atebyt( 7*nnbft )
cmckim end
      lblij  = spo    + atebyt( nbufo1 )
      plabs  = lblij  + forbyt( nbufo1 )
      itotal = plabs  + forbyt( nbufo1 ) - 1
c
      if ( itotal .gt. lencor ) then
         call bummer('before out1e(), itotal=', itotal, faterr )
      endif
c
      call out1e(
     & l1rec,        n1max,       core(buffer), core(values),
     & core(labels), core(stvc),  info,         core(ms),
     & core(mnl),    slabel,      bfnlab,       nbufo1,
     & core(spo),    core(lblij), core(plabs) )
cmckim start
cc
cc     # process the 2-e integral arrays.
cc     # core(*)==> buffer(1:l2rec), values(1:n2max), labels(1:4,1:n2max
cc     #            ibitv(1:n2max), psup(1:nbufo2), ksup(1:nbufo2),
cc     #            ilbl(1:nbufo2), klbl(1:nbufo2), plabs(1:nbufo2)
cc
c      buffer = 1
c      values = buffer + atebyt( l2rec )
c      labels = values + atebyt( n2max )
c      ibitv  = labels + forbyt( 4*n2max )
c      psup   = ibitv  + forbyt( ((n2max+63)/64)*64 )
c      ksup   = psup   + atebyt( nbufo2 )
c      ilbl   = ksup   + atebyt( nbufo2 )
c      klbl   = ilbl   + forbyt( nbufo2 )
c      plabs  = klbl   + forbyt( nbufo2 )
c      itotal = plabs  + atebyt( nbufo2 ) - 1
cc
c      if ( itotal .gt. lencor ) then
c         call bummer('before phk(), itotal=', itotal, faterr )
c      endif
cc
cc     # open the input 2-e integral file.
c      itemp = ifile2
c      fname = 'aoints2'
c      call trnfln( 1, fname )
c*mdc*if parallel
c*      call pfname( 1, fname )
c*mdc*endif
c      call sifo2f( ifile, itemp, fname, info, ifile2, ierr )
c      if ( ierr .ne. 0 ) then
c         call bummer('from sifo2f(), ierr=', ierr, faterr )
c      endif
cc
c      call phk(
c     & l2rec,        n2max,        core(buffer), core(values),
c     & core(labels), core(ibitv),  info,         nbufo2,
c     & core(psup),   core(ksup),   core(ilbl),   core(klbl),
c     & core(plabs) )
cc
c      call sifc2f( ifile2, info, ierr )
c      if ( ierr .ne. 0 ) then
c         call bummer('from sifc2f(), ierr=', ierr, faterr )
c      endif
cc
cmckim end
c     # message passing cleanup: stub otherwise
      call pend
c
      stop 'end of cnvrt'
6040  format(/' nsym =',i2,' nbft=',i4/)
6050  format(1x,a,8i5)
6060  format(1x,a,8(1x,a4))
6080  format(10(i4,':',a8))
6100  format(/1x,a,(1x,10i10))
6200  format(/1x,a,(1x,1p4e20.12))
c
      end
