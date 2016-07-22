*deck seg1mn
      subroutine seg1mn( a,      eta,       ica,    icb,    icxst,
     &   iprst,  ipt,    lmnp1,  lmnv,      mcons,  nc,     ncon,
     &   nf,     nfct,   ngw,    npair,     nprir,  nrcr,   nt,
     &   ntl,    ntu,    x,      y,         z,      zet,    lcore,
     &   ifirst, mem1 )
c
c  read the user input and compute the 1-e integrals.
c
      implicit logical(a-z)
c
c     # the following parameters should agree exactly with those in the
c     # calling program.  (rls)
c
c     # msu    = max number of symmetry inequivalent types of atoms.
      integer    msup
      parameter (msup=203)
c
c     # mstu   = max number of irreps.
      integer    mstup
      parameter (mstup=8)
c
c     # kaords = max number of ao reduction sets.
      integer    kaordp
      parameter (kaordp=11)
c
c     # mconsu = max number of contraction sets.
      integer    mconsp
      parameter (mconsp=50)
c
c     # mgcsu  = max number of symmetry orbital transformation sets.
      integer    mgcsup
      parameter (mgcsup=11)
c
c     # mru    = max number of irreps in an ao reduction set.
      integer    mrup
      parameter (mrup=24)
c
c     # mcsu   = max number of so's in a transformation matrix.
      integer    mcsup
      parameter (mcsup=24)
c
c     # mctu   = max number of ao's in a transformation matrix.
      integer    mctup
      parameter (mctup=30)
c
c     # mcru   = max number of core potential expansion functions.
      integer    mcrup
      parameter (mcrup=128)
c
c     # msfu   = max number of function sets.
      integer    msfup
      parameter (msfup=52)
c
      integer    mnsfup
      parameter (mnsfup=(msfup*(msfup+1))/2)
c
c     # mgu    = max number of operators in the nuclear
c     #          interchange group.
      integer    mgup
      parameter (mgup=49)
c
c     # msfru  = max number of so sets.
      integer    msfrup
      parameter (msfrup=312)
cbert parameter (msfrup=208)
c
c     # mnru   = max number of charge dists. from a function set pair.
      integer    mnrup
      parameter (mnrup=576)
c
      integer   nunits
      parameter(nunits=4)
      integer        iunits
      common /units/ iunits(nunits)
      integer                  nlist
      equivalence ( iunits(1), nlist )
      integer                  aoints
      equivalence ( iunits(2), aoints )
c
c     # filenames.
      character*60    fnames
      common /cfname/ fnames(nunits)
c
c     # /bufout/ holds some output integral file parameters.
      integer
     & info,     ifmt1,   ifmt2,   itypea,  itypeb,
     & ibuf,     numout,  nrec,    ntape
      common /bufout/
     & info(10), ifmt1,   ifmt2,   itypea,  itypeb,
     & ibuf,     numout,  nrec,    ntape
c
      integer      l1rec
      equivalence( l1rec, info(2) )
      integer      n1max
      equivalence( n1max, info(3) )
c
      integer
     & mblu,  mccu, mconu, mcu,  mcxu, mpru, nbft, nnbft,
     & mrcru, mstu, msu,   nblu, ng,   ns,   nst
      common /parmi/
     & mblu,  mccu, mconu, mcu,  mcxu, mpru, nbft, nnbft,
     & mrcru, mstu, msu,   nblu, ng,   ns,   nst
c
      integer
     & kaords, mconsu, mgcsu, mru,  mcsu, mctu, mcru, msfu, mgu,
     & msfru,  mnru,   ngcs,  nu,   mcxu2,
     & lxyzir,         inam,        nnam, mdum
      common /ntgr/
     & kaords, mconsu, mgcsu, mru,  mcsu, mctu, mcru, msfu, mgu,
     & msfru,  mnru,   ngcs,  nu,   mcxu2,
     & lxyzir(3),      inam(5),     nnam, mdum(32)
c
      integer        ipq
      common /dim21/ ipq(256)
c
c     # dummy:
      integer lcore, ifirst, mem1
      integer
     & ica(mcu,msu,*), icb(4,24,*), icxst(2,*), iprst(*),
     & ipt(*),         lmnp1(*),    lmnv(3,*),
     & mcons(*),       nc(*),       ncon(*),    nf(*),
     & nfct(*),        ngw(*),      npair(2,*), nprir(2,mstu,*),
     & nrcr(*),        nt(*),       ntl(*),     ntu(*)
c
      real*8
     & a(*),     eta(mrcru,mconu,*), x(mcu,*), y(mcu,*),
     & z(mcu,*), zet(mconu,*)
c
c     # local:
      integer iclock, i, n, narray
      integer
     & lcr(msup),       lls(msup),      nkcrl(6,msup), nkcru(6,msup),
     & nklsl(4,msup),   nklsu(4,msup),  mcrs(msup),    nir(kaordp),
     & maords(mgcsup),  icxsv1(mgcsup), nd(mstup),     nso(mstup),
     & nsopr(mstup),    nblpr(mstup),   nopir(mstup),  ilxyz(3,mstup),
     & la(mrup,kaordp), ncr(mcrup),     nct(msfup),    mgcs(msfup),
     & lb(msfrup),      ms(msfrup),     mnl(msfrup),   mau(mnrup),
     & mics(mnrup),     mjcs(mnrup),    icxast(mnsfup),
     & icxsv2(mgcsup,mgcsup),           idp(mstup,mstup,mstup),
     & ihwsp,           igh,            lmn1,          nn,
     & lmn1u,           ncru,           lproju,        ndfac,
     & lmax,            l1max2,         lmnpwr,        l2m1
c
      real*8 chg(msup), zcr(mcrup), ccr(mcrup), eps
c
      character*3 ityp(mstup), mtype(msup)
c
c     # bummer error types.
      integer    wrnerr,  nfterr,  faterr
      parameter (wrnerr=0,nfterr=1,faterr=2)
c
c     # timer operations.
      integer   tminit,  tmprt,  tmrein,  tmclr,  tmsusp,  tmresm
      parameter(tminit=1,tmprt=2,tmrein=3,tmclr=4,tmsusp=5,tmresm=6)
c
      integer  atebyt, forbyt
      external atebyt, forbyt
c
      kaords = kaordp
      mconsu = mconsp
      mgcsu  = mgcsup
      mru    = mrup
      mcsu   = mcsup
      mctu   = mctup
      mcru   = mcrup
      msfu   = msfup
      mgu    = mgup
      msfru  = msfrup
      mnru   = mnrup
c
c     # lcore = total available workspace on entry to this routine.
      lcore  = mblu
c
      do 20 i = 1, 3
        inam(i) = i
   20 continue
c
c     # set up ipq(*).
c
      do 30 i = 1, 255
        ipq(i) = (i * (i - 1) ) / 2
   30 continue
c
c     # reserve space for c(mctu,mcsu,mgcsu) at the end of a(1:mblu).
c     # afterwards, a(1:mcxu) is the remaining available workspace.
c     # a(*)--> 1:...55:c(1:csize)
c
      ipt(55) = mblu + 1 - atebyt( mctu * mcsu * mgcsu )
c
      mcxu = ipt(55) - ipt(1)
c
      if ( mcxu .lt. 0 ) then
        call bummer('seg1mn: before syminp() mcxu=',mcxu,faterr)
      endif
c
c     # push the current allocation onto the stack.
      call h2opsh( (lcore - mcxu) )
c
      call timer(' ', tminit, iclock, nlist )
c
c     # input processing.
c
            call syminp( a,      a(ipt(55)), ccr,    chg,    eta,
     &   ica,    idp,    ipt,    ityp,       la,     lb,     lcr,
     &   lls,    lmnp1,  lmnv,   lmn1u,      lproju, maords, mcons,
     &   mcrs,   mgcs,   mnl,    ms,         mtype,  nblpr,  nc,
     &   ncon,   ncr,    ncru,   nct,        nd,     nf,     nir,
     &   nkcrl,  nkcru,  nklsl,  nklsu,      nrcr,   nso,    nsopr,
     &   nt,     ntl,    ntu,    x,          y,      z,      zcr,
     &   zet,    lcore,  ifirst, mem1 )
c
      call timer('syminp required', tmrein, iclock, nlist )
c
c     # pop back to the current level.
      call h2opop
c
c     # allocate space for il(*), and compute the
c     # symmetry coefficient products.
c     # ipt(*)-->1:lmnv(1:3,1:lmnvmx), 2:il(1:nnbft), 3:cx(*)...,
c     #          55:c(*)
c
      ipt(3) = ipt(2) + forbyt( nnbft )
c
c     # mcxu is the remaining space.
      mcxu = ipt(55) - ipt(3)
c
c     # push the current allocation onto the high-water mark stack.
      call h2opsh( (lcore - mcxu) )
c
            call socfpd( a(ipt(55)), a(ipt(3)), icxast, icxst,
     &   icxsv1, icxsv2, idp,        a(ipt(2)), iprst,  la,
     &   lb,     maords, mau,        mcons,     mgcs,   mics,
     &   mjcs,   nblpr,  nc,         nd,        nf,     nir,
     &   npair,  nprir,  nrcr,       nsopr,     nt,     ntl,
     &   ntu  )
c
c     # pop back to the initial level.
      call h2opop
c
      call timer('socfpd required', tmrein, iclock, nlist )
c
c     # compute the one-electron integrals.
c
c     # on return from socfpd(*), cx(1:mcxu) was computed.
c     # allocate space.
c     # ipt(*)-->1:lmnv(1:3,1:lmnvmx), 2:il(1:nnbft), 3:cx(1:mcxu),
c     #          4:array(1:mpru,1:narray), 5:iarray(1:iamax,1:narray),
c     #          6:buffer(1:l1rec), 7:values(1:n1max),
c     #          8:labels(1:2,1:n1max), 9:hpt(1:ihwsp), 10:hwt(1:ihwsp),
c     #          11:dfac(1:ndfac), 12:binom(1:xyzdim), 13:lmf(1:l1max2),
c     #          14:lml(1:l1max2), 15:lmx(1:lmnpwr), 16:lmy(1:lmnpwr),
c     #          17:lmz(1:lmnpwr), 18:zlm(1:lmnpwr),
c     #          19:flmtx(1:3,1:lmnu2), 20:mc(1:3,1:l2m1),
c     #          21:mr(1:3,1:l2m1), 22:h2(1:nblu)
c
      if ( inam(nu) .eq. 5 ) then
c       # x,y,z components of spin-orbit integral are all computed
c       # together.
        narray = 3
      else
        narray = 1
      endif
c
      ihwsp = (lmn1u*(lmn1u+1))/2
      ipt(4)  = ipt(3)  + atebyt( mcxu )
      ipt(5)  = ipt(4)  + atebyt( narray * mpru )
      ipt(6)  = ipt(5)  + forbyt( narray * mpru )
      ipt(7)  = ipt(6)  + atebyt( l1rec )
      ipt(8)  = ipt(7)  + atebyt( n1max )
      ipt(9)  = ipt(8)  + forbyt( 2 * n1max )
      ipt(10) = ipt(9)  + atebyt( ihwsp )
      ipt(22) = ipt(10) + atebyt( ihwsp )
c
c     mblu = lcore - ipt(22) + 1
c
c     # compute gauss-hermite points and weights for s, t, v integrals.
c     # 10**(-12) needed to get 6-point values to sufficient accuracy.
      eps = 1.0d-12
      igh = 0
      do 40 lmn1 = 1, lmn1u
        call hermit(lmn1,a(ipt(9)+igh),a(ipt(10)+igh),eps)
        igh = igh + lmn1
   40 continue
c
      ntape = aoints
c
      write(nlist,*)
c
      do 60 n = 1, nu
c
        if( n.eq.4 ) then
c         # allocate space.
          ipt(10) = ipt(9)  + atebyt( 35 )
          ipt(11) = ipt(10) + atebyt( 35 )
          ndfac = max(4*lmn1u+2*lproju-3,6*lproju+3,4*lmn1u-1,
     &      2*lmn1u+2*lproju+1,4,ncru+4*lmn1u+2*lproju-1)
          ipt(12) = ipt(11) + atebyt( ndfac )
          ipt(13) = ipt(12) + atebyt( (lmn1u*(lmn1u+1))/2 )
          lmax =  max(1,lmn1u-1 + max(lmn1u-1,lproju))
          l1max2 = (lmax+1)**2
          ipt(14) = ipt(13) + forbyt( l1max2 )
          ipt(15) = ipt(14) + forbyt( l1max2 )
          lmnpwr = (((lmax*(lmax+2)*(lmax+4))/3)*(lmax+3)+
     &               (lmax+2)**2*(lmax+4))/16
          ipt(16) = ipt(15) + forbyt( lmnpwr )
          ipt(17) = ipt(16) + forbyt( lmnpwr )
          ipt(18) = ipt(17) + forbyt( lmnpwr )
          ipt(19) = ipt(18) + atebyt( lmnpwr )
          ipt(20) = ipt(19) + atebyt( 3*lproju**2 )
          l2m1 = 2*lproju-1
          ipt(21) = ipt(20) + forbyt( 3*l2m1 )
          ipt(22) = ipt(21) + forbyt( 3*l2m1 )
          call cortab(a(ipt(12)),a(ipt(11)),eps,a(ipt(19)),a(ipt(9)),
     &      a(ipt(10)),a(ipt(13)),a(ipt(14)),a(ipt(15)),a(ipt(16)),
     &      a(ipt(17)),lmax,lmn1u,lproju,a(ipt(20)),a(ipt(21)),ndfac,
     &      a(ipt(18)))
        endif
c
c       # push the current allocation onto the high-water mark stack.
        call h2opsh( (ipt(22) - 1) )
c
        nnam  = inam(n)
c
            call oneint( a,          ccr,       chg,        a(ipt(3)),
     &        eta,       a(ipt(22)), ica,
     &        icb,       icxast,     icxst,     a(ipt(2)),  ilxyz,
     &        iprst,     ipt,        lcr,       lls,        lmnp1,
     &        lproju,    mcons,      mcrs,      nc,         ncon,
     &        ncr,       nf,         nfct,      ngw,        nkcrl,
     &        nkcru,     nklsl,      nklsu,     nopir,      npair,
     &        nprir,     nrcr,       nt,        ntl,        ntu,
     &        x,         y,          z,         zcr,        zet,
     &        a(ipt(4)), a(ipt(5)),  a(ipt(6)), a(ipt(7)),  a(ipt(8)) )
c
c       # pop the high-water mark stack.
        call h2opop
   60 continue
      write(nlist,*)
      call timer('oneint required', tmclr, iclock, nlist )
c
      return
      end
