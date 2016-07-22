*deck oneint
      subroutine oneint( a,          ccr,        chg,        cx,
     &       eta,        h2,         ica,
     &       icb,        icxast,     icxst,      il,         ilxyz,
     &       iprst,      ipt,        lcr,        lls,        lmnp1,
     &       lproju,     mcons,      mcrs,       nc,         ncon,
     &       ncr,        nf,         nfct,       ngw,        nkcrl,
     &       nkcru,      nklsl,      nklsu,      nopir,      npair,
     &       nprir,      nrcr,       nt,         ntl,        ntu,
     &       x,          y,          z,          zcr,        zet,
     &       array,      iarray,     buffer,     values,     labels )
c
c  04-dec-90 array(*,*) added for spin-orbit integrals. -rls
c  01-dec-90 SIFS version. il(*) decoded with arithmetic. -rls
c
      implicit logical(a-z)
c
      integer    ilfact
      parameter( ilfact=1024 )
c
      integer   nunits
      parameter(nunits=4)
      integer        iunits
      common /units/ iunits(nunits)
      integer                  nlist
      equivalence ( iunits(1), nlist )
c
      real*8         cutoff, tol
      common /parmr/ cutoff, tol
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
      real*8
     & dxij,   dyij,   dzij,   fnfct,  rr,     xij,    xijm,
     & yij,    yijm,   zij,    zijm
      integer  ibl1,   ibl2,   icxi1,  icxi2,  ij,     ijsf,
     & ic,     icons,  igu,
     & ircru,  is,     isf,    itl,    itu,    jc,     jcons,
     & jgu,    jrcru,  js,     jsf,    jtl,    jtu,    lit,
     & ljt,    nblt1,  nc2,    nc1,    nop,    ntij1,  ntij2
      logical
     & esf,    esfc,   igueq1, jgueq1
      common /one/     dxij,   dyij,   dzij,   fnfct,  rr,
     & xij,    xijm,   yij,    yijm,   zij,    zijm,   ibl1,
     & ibl2,   icxi1,  icxi2,  ij,     ijsf,   ic,     icons,
     & igu,    ircru,  is,     isf,    itl,    itu,    jc,
     & jcons,  jgu,    jrcru,  js,     jsf,    jtl,    jtu,
     & lit,    ljt,    nblt1,  nc2,    nc1,    nop,    ntij1,
     & ntij2,  esf,    esfc,   igueq1, jgueq1
c
      integer        ipq
      common /dim21/ ipq(256)
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
      integer           ica(mcu,msu,*), icb(4,24,*), icxast(*),
     & icxst(2,*),      il(nnbft),      ilxyz(3,*),  iprst(*),
     & ipt(*),          lcr(*),         lls(*),      lmnp1(*),
     & lproju,          mcons(*),       mcrs(*),     nc(*),
     & ncon(*),         ncr(*),         nf(*),       nfct(*),
     & ngw(*),          nkcrl(6,*),     nkcru(6,*),  nklsl(4,*),
     & nklsu(4,*),      nopir(*),       npair(2,*),  nprir(2,mstu,*),
     & nrcr(*),         nt(*),          ntl(*),      ntu(*),
     & iarray(mpru,*),  labels(nipv,*)
      real*8
     & a(*),         ccr(*),       chg(*),        cx(*),
     & h2(*),
     & x(mcu,*),     y(mcu,*),     z(mcu,*),      eta(mrcru,mconu,*),
     & zcr(*),       zet(mconu,*), array(mpru,*), buffer(*),
     & values(*)
c     # ibm+convex f77 bug; actual:
c     # integer labels(nipv,n1max)
c     # real*8 buffer(l1rec), values(n1max)
c
c     # local:
      integer ixyz, ist, iblk, ifu, icu, isfis, jsfjs, ncc, igwu,
     & ig, ic1, jc1, ic2, jc2, icc, igw, jfu, jcu, if, jf, iop,
     & ipr, itst, iprm, inx, jnx, nwt, ibl, ircr, jrcr, iopu, npairi,
     & lblop, ipair, ipr1, ipr2, num, iju, iiju
      logical  es
c
c     # bummer error types.
      integer    wrnerr,  nfterr,  faterr
      parameter (wrnerr=0,nfterr=1,faterr=2)
c
      integer  atebyt, forbyt
      external atebyt, forbyt
c
      integer    msame,   nmsame,   nomore
      parameter( msame=0, nmsame=1, nomore=2 )
c
      real*8     zero
      parameter( zero=0d0 )
c
      integer last
      integer itypan(7), itypbn(7), iapt(3)
      real*8 fcore
c
      character*8 chrtyp
c
c     #             s    t    v  veff so:x so:y so:z
      data itypan / 0,   0,   0,   0,   2,   2,   2 /
      data itypbn / 0,   1,   2,   3,   0,   1,   2 /
c
c     # operator symmetry
c
      call izero( nst,   nopir, 1 )
      call izero( 3*nst, ilxyz, 1 )
c
      if ( nnam .eq. 5 ) then
c       # x, y, and z components of spin-orbit integrals are all
c       # computed together.
        do 102 ixyz = 1, 3
          ist                   = lxyzir(ixyz)
          nopir(ist)            = nopir(ist) + 1
          ilxyz(nopir(ist),ist) = ixyz
102     continue
        nop     = 3
        iapt(1) = 0
        iapt(2) = 0
        iapt(3) = 0
      else
        nopir(1)   = 1
        ilxyz(1,1) = 1
        nop        = 1
        iapt(1)    = 0
      endif
c
      iblk = 0
      ijsf = 0
      isf  = 0
      do 712 is = 1,ns
      ifu = nf(is)
      if(ifu.eq.0) go to 712
      icu=nc(is)
      isfis=isf
      jsf = 0
      do 708 js = 1,is
      jsfjs=jsf
      es = is.eq.js
      ic=icu
      ncc = 0
      if(.not.es) go to 200
c
c  center combinations for is=js
c
      do 140 jc = 1, ic
      igwu=0
      do 132 ig = 1, ng
        ic1 = ica(ic,is,ig)
        jc1 = ica(jc,is,ig)
        if(ic1.ne.ic.and.jc1.ne.ic) go to 132
        ic2=max(ic1,jc1)
        jc2=min(ic1,jc1)
c
        do 112 icc=ncc,1,-1
          if(jc2-icb(2,1,icc)) 112,136,116
  112   continue
c
  116   if(jc2.ne.jc) go to 132
c
        do 124 igw=igwu,1,-1
          if(ic1.eq.icb(1,igw,ncc+1).and.
     &       jc1.eq.icb(2,igw,ncc+1)) go to 132
  124   continue
c
        igwu=igwu+1
        icb(1,igwu,ncc+1)=ic1
        icb(2,igwu,ncc+1)=jc1
  132 continue
      ncc = ncc + 1
      nfct(ncc) = ic
      ngw(ncc)=igwu
      go to 140
  136 nfct(icc) = nfct(icc) + ic
  140 continue
      do 144 icc = 1, ncc-1
        nfct(icc) = nfct(icc)/2
  144 continue
      go to 300
c
c  center combinations for is>js
c
  200 jfu = nf(js)
      if(jfu.eq.0) go to 708
      jcu = nc(js)
      do 240 jc = 1, jcu
      do 232 ig = 1, ng
      if (ica(ic,is,ig) .ne. ic) go to 232
      jc1 = ica (jc,js,ig)
c
      do 212 icc=ncc,1,-1
        if(jc1-icb(2,1,icc)) 212,236,232
  212 continue
c
  232 continue
      ncc = ncc + 1
      icb(1,1,ncc)=ic
      icb(2,1,ncc)=jc
      nfct(ncc) = ic
      go to 240
  236 nfct(icc) = nfct(icc) + ic
  240 continue
c
c  evaluate integrals
c
  300 if(ncc.gt.mccu) then
        call bummer('change mccup (one place) to ',ncc,faterr)
      endif
      isf=isfis
      do 704 if = 1, ifu
      isf = isf + 1
      icons=mcons(isf)
      ircru=nrcr(icons)
      lit=lmnp1(icons)
      igu=ncon(icons)
      itl=ntl(isf)
      itu=ntu(isf)
      jsf=jsfjs
      if(es) jfu=if
      do 700 jf = 1, jfu
      jsf = jsf + 1
      esf=isf.eq.jsf
      jcons=mcons(jsf)
      jrcru=nrcr(jcons)
      ljt=lmnp1(jcons)
      ijsf = ijsf + 1
      ibl1=0
      ibl2=0
      do 308 ist=1,nst
        iop=nopir(ist)
        if(iop.ne.0) then
          if(esf) ibl1=ibl1+iop*nprir(1,ist,ijsf)
          ibl2=ibl2+iop*nprir(2,ist,ijsf)
        endif
  308 continue
      iju=nt(isf)*nt(jsf)
      ntij2=iju*npair(2,ijsf)
      icxi2=icxst(2,ijsf)
      if(esf) then
        nblt1=npair(1,ijsf)
        ntij1=iju*nblt1
        nblu=ircru*ibl1+ipq(ircru)*ibl2
        if(nnam.eq.5) then
          icxi1=icxast(ijsf)
        else
          icxi1=icxst(1,ijsf)
        endif
      else
        nblu=ircru*jrcru*ibl2
      endif
      if(nblu.eq.0) go to 696
c     # allocate space.
c     # ipt(*)-->1:lmnv(1:3,1:lmnvmx), 2:il(1:nnbft), 3:cx(1:mcxu),
c     #          4:array(1:mpru,1:narray), 5:iarray(1:iamax,1:narray),
c     #          6:buffer(1:l1rec), 7:values(1:n1max),
c     #          8:labels(1:2,1:n1max), 9:hpt(1:ihwsp), 10:hwt(1:ihwsp),
c     #          11:dfac(1:ndfac), 12:binom(1:xyzdim), 13:lmf(1,l1max2),
c     #          14:lml(1:l1max2), 15:lmx(1:lmnpwr), 16:lmy(1:lmnpwr),
c     #          17:lmz(1:lmnpwr), 18:zlm(1:lmnpwr),
c     #          19:flmtx(1:3,1:lmnu2), 20:mc(1:3,1:l2m1),
c     #          21:mr(1:3,1:l2m1), 22:h2(1:nblu), 23:ijx(1:iiju),
c     #          24:ijy(1,iiju), 25:ijz(1:iiju), 26:g2(1:ic1)
      ipt(23) = ipt(22) + atebyt( nblu )
      if( nnam.le.3 ) then
        iiju = iju
      else
        iiju = 0
      endif
      ipt(24) = ipt(23) + forbyt( iiju )
      ipt(25) = ipt(24) + forbyt( iiju )
      ipt(26) = ipt(25) + forbyt( iiju )
      ipr=iprst(ijsf)
c
      if ( (ipt(26)-1) .gt. mblu ) then
        call bummer('oneint: mblu too small ',(ipt(26)-1-mblu),faterr)
      endif
c
c     # push the current alloction onto the high-water mark stack.
      call h2opsh( ipt(26)-ipt(22) )
c
      jgu=ncon(jcons)
      jtl=ntl(jsf)
      jtu=ntu(jsf)
      call wzero(nblu,h2,1)
      if ( (.not. es) .or. esf ) then
c
        do 424 icc = 1, ncc
          ic = icb(1,1,icc)
          jc = icb(2,1,icc)
          fnfct = nfct(icc)
            call stvcz( a,          ccr,        chg,        cx,
     &      eta,        a(ipt(26)), h2,         a(ipt(23)), a(ipt(24)),
     &      a(ipt(25)), ilxyz,      ipt,        lcr,        lls,
     &      a(ipt(1)),  lproju,     mcrs,       nc,         ncr,
     &      nkcrl,      nkcru,      nklsl,      nklsu,      nopir,
     &      nprir,      nt,         x,          y,          z,
     &      zcr,        zet )
424     continue
      else
        do 524 icc = 1, ncc
          igwu = ngw(icc)
          itst = 0
          do 520 iprm = 1, 2
            if ( iprm .eq. 1 ) then
              inx=1
              jnx=2
            else
              inx=2
              jnx=1
            endif
            jc = icb(jnx,1,icc)
            if ( jc .eq. itst ) go to 520
            ic = icb(inx,1,icc)
            nwt = 1
            do 512 igw = 2, igwu
              if( icb(jnx,igw,icc) .lt. jc ) go to 520
              nwt = nwt + 1
512         continue
            fnfct = (nwt * nfct(icc))
            call stvcz( a,          ccr,        chg,        cx,
     &      eta,        a(ipt(26)), h2,         a(ipt(23)), a(ipt(24)),
     &      a(ipt(25)), ilxyz,      ipt,        lcr,        lls,
     &      a(ipt(1)),  lproju,     mcrs,       nc,         ncr,
     &      nkcrl,      nkcru,      nklsl,      nklsu,      nopir,
     &      nprir,      nt,         x,          y,          z,
     &      zcr,    zet )
            itst = jc
520       continue
524     continue
      endif
c
c     # pop back to the initial level.
      call h2opop
c
c     # put integrals and labels into the intermediate storage arrays.
c
      ibl = 0
      do 642 ircr = 1, ircru
        if(esf) jrcru = ircr
        do 640 jrcr = 1, jrcru
          do 636 ist = 1, nst
            iopu = nopir(ist)
            if ( esf .and. jrcr .eq. ircr ) then
              npairi = nprir(1,ist,ijsf)
            else
              npairi = nprir(2,ist,ijsf)
            endif
            if ( (iopu .ne. 0) .and. (npairi .ne. 0) ) then
              do 628 iop = 1, iopu
                lblop = ilxyz(iop,ist)
                do 624 ipair = 1, npairi
                  ibl = ibl + 1
                  if ( abs(h2(ibl)) .gt. cutoff) then
                    iapt(lblop)                 = iapt(lblop) + 1
                    array(  iapt(lblop), lblop) = h2(ibl)
                    iarray( iapt(lblop), lblop) = (ipr+ipair)
                  endif
624             continue
628           continue
            endif
            ipr = ipr + npairi
636       continue
640     continue
642   continue
696   continue
700   continue
704   continue
708   continue
712   continue
c
c     # write out the components of the operator arrays.
c
      do 860 lblop = 1, nop
c
c       # check that array(*) was correctly allocated.
        if ( iapt(lblop) .gt. mpru ) then
           call bummer('oneint: array(*) overflow, iapt=',
     &      iapt(lblop), faterr )
        endif
c
c       # initialize the integral record counters for this array.
c
        call w1stup
c
c       # SIFS frozen core for this array.
        fcore = zero
c
c       # SIFS integral type.
        itypea = itypan(nnam + lblop - 1)
        itypeb = itypbn(nnam + lblop - 1)
c
c       # loop over output records.
c       # do while ( ipr1 .le. iapt(lblop) )...
        ipr1 = 1
820     if ( ipr1 .le. iapt(lblop) ) then
          num  = min( (n1max - ibuf), (iapt(lblop) - ipr1 + 1) )
          ipr2 = ipr1 + num - 1
c
c         # fill up the record.
c
          do 840 ipr = ipr1, ipr2
            ibuf           = ibuf + 1
            labels(1,ibuf) =      il(iarray(ipr,lblop)) / ilfact
            labels(2,ibuf) = mod( il(iarray(ipr,lblop)),  ilfact )
            values(ibuf)   = array(ipr,lblop)
840       continue
          ipr1 = ipr2 + 1
c
c         # write out the record.
c
          if ( ipr2 .ne. iapt(lblop ) ) then
c           # more elements of this component still to go.
            last = msame
          elseif ( (nnam .eq. inam(nu)) .and. (lblop .eq. nop) ) then
c           # last component of the last 1-e operator.
            last = nomore
          else
c           # last record of this array, but more 1-e arrays to go.
            last = nmsame
          endif
          call wtlab1( last, fcore, buffer, values, labels )
c
c         # note: the "do while" construct is used since it is not
c         #       guaranteed that ibuf=0 on return from wtlab1().
c
          go to 820
        endif
c
        call siftyp( itypea, itypeb, chrtyp )
        write (nlist,6010) numout, chrtyp, nrec
c
860   continue
c
      if ( nnam .eq. 5 ) then
c       # all done with the antisymmetric coefficients.
c       # release the workspace, and adjust mcxu and mblu.
c       # this should probably be done in the higher-level
c       # calling program, but for now it's done down here.
        mblu = mblu + mcxu - mcxu2
        mcxu = mcxu2
      endif
c
      return
6010  format(' oneint:',i6,1x,a,' integrals were written in',
     & i3,' records.')
      end
