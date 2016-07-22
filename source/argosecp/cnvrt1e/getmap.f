      subroutine getmap( nmap, imtype, map, mnl, ms)
c
c  extract mnl(*) and ms(*) from map(*,*).
c  compute ipq(*), nsopr(*), and nblpr(*).
c
c  25-may-91 written by russ pitzer.
c
      implicit logical(a-z)
c
      real*8 repnuc
      integer  nst,    ns,     isfr,   nu,     iau,    ntitle,
     & nso,    nsopr,          nblpr,          nnbft
      common /c2/
     & repnuc, nst,    ns,     isfr,   nu,     iau,    ntitle,
     & nso(8), nsopr(8),       nblpr(8),       nnbft
c
      integer    nbfmx
      parameter( nbfmx=255 )
c
      integer       ipq,        mapin,        mapout,        symb
      common /cipq/ ipq(nbfmx), mapin(nbfmx), mapout(nbfmx), symb(nbfmx)
c
c     # dummy:
      integer nmap
      integer imtype(*), mnl(isfr), ms(isfr), map(isfr,*)
c
c     # local:
      integer    mnltyp,   mstyp
      parameter( mnltyp=4, mstyp=3 )
      integer i, nbft, imnl, ims, nbi, j
c
      integer nndxf
      nndxf(i) = (i * (i - 1)) / 2
c
c     # initialize the ipq(*) array, and set the
c     # default mapin(*) and mapout(*) values.
      do 100 i = 1, nbfmx
         ipq(i)    = nndxf(i)
         mapin(i)  = i
         mapout(i) = i
         symb(i)   = 0
100   continue
c
c     # initialize the symmetry offset arrays.
      nbft  = 0
      nnbft = 0
      do 300 i = 1, 8
         nbi      = nso(i)
         nsopr(i) = nbft
         nblpr(i) = nnbft
         do 200 j = (nbft+1), (nbft+nbi)
            symb(j) = i
200      continue
         nbft  = nbft + nbi
         nnbft = nnbft + nndxf( nbi + 1 )
300   continue
c
      imnl = 0
      ims  = 0
      do 400 i = 1, nmap
         if ( imtype(i) .eq. mnltyp ) then
            imnl = i
         elseif ( imtype(i) .eq. mstyp ) then
            ims = i
         endif
400   continue
c
c     # extract the bfntyp(*) array from map(*,*).
      if ( imnl .eq. 0 ) then
c        # default: set everything to an s-type function.
         call iset( isfr, 1, mnl, 1 )
      else
         call icopy( isfr, map(1,imnl), 1, mnl, 1 )
      endif
c
c     # extract the bfn-to-center array from map(*,*).
      if ( ims .eq. 0 ) then
c        # default: set everything to one center.
         call iset( isfr, 1, ms, 1 )
      else
         call icopy( isfr, map(1,ims), 1, ms, 1 )
      endif
c
      return
      end
