*deck twoint
      subroutine twoint( a,          cx,         eta,        h4,
     &      ica,         icb,        icxst,      il,         iprst,
     &      ipt,         lmnp1,      lmnv,       mcons,      nc,
     &      ncon,        nf,         nfct,       ngw,        npair,
     &      nprir,       nrcr,       nt,         ntl,        ntu,
     &      x,           y,          z,          zet,        buffer,
     &      values,      labels,     ibitv )
c
c  11-sep-91 final parallel version. -rls/rjh
c  01-dec-90 SIFS version. -rls
c
      implicit logical(a-z)
c
      integer   nunits
      parameter(nunits=4)
      integer        iunits
      common /units/ iunits(nunits)
      integer                  nlist
      equivalence ( iunits(1), nlist )
c
      integer
     & mblu,  mccu, mconu, mcu,  mcxu, mpru, nbft, nnbft,
     & mrcru, mstu, msu,   nblu, ng,   ns,   nst
      common /parmi/
     & mblu,  mccu, mconu, mcu,  mcxu, mpru, nbft, nnbft,
     & mrcru, mstu, msu,   nblu, ng,   ns,   nst
c
      logical       eprsf,    esfi,    esfk
      common /lgcl/ eprsf(3), esfi(3), esfk(3)
c
      integer
     & itl,    itu,    jtl,    jtu,    ktl,    ktu,    ltl,    ltu,
     & jcu,    kcu,    lcu,    inx,    jnx,    knx,    lnx,    nwt,
     & nwt1,           ijsf,           klsf,           icxs,
     & kcxs,           npri,           nprk,           iesfb,  kesfb,
     & ircru,  jrcru,  krcru,  lrcru
      common /ntgr/
     & itl,    itu,    jtl,    jtu,    ktl,    ktu,    ltl,    ltu,
     & jcu,    kcu,    lcu,    inx,    jnx,    knx,    lnx,    nwt,
     & nwt1(3),        ijsf(3),        klsf(3),        icxs(2,3),
     & kcxs(2,3),      npri(2,3),      nprk(2,3),      iesfb,  kesfb,
     & ircru,  jrcru,  krcru,  lrcru
c
      integer
     & ntij,   ntik,   ntil,   ntkl,   ntjl,   ntjk,   ntij1,
     & ntjk1,  ntkl1,  ijcxst,         ikcxst,         ilcxst,
     & klcxst,         jlcxst,         jkcxst
      common /cxindx/
     & ntij,   ntik,   ntil,   ntkl,   ntjl,   ntjk,   ntij1,
     & ntjk1,  ntkl1,  ijcxst(2),      ikcxst(2),      ilcxst(2),
     & klcxst(2),      jlcxst(2),      jkcxst(2)
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
      integer        ipq
      common /dim21/ ipq(256)
c
      integer       npriri,        nprirk
      common /ikpr/ npriri(2,3,8), nprirk(2,3,8)
c
      integer    nipv
      parameter( nipv=4 )
c
c     # dummy:
       integer         ica(mcu,msu,*), icb(4,24,*),     icxst(2,*),
     &  il(*),         iprst(*),       ipt(*),          lmnp1(*),
     &  lmnv(3,*),     mcons(*),       nc(*),           ncon(*),
     &  nf(*),         nfct(*),        ngw(*),          npair(2,*),
     &  nrcr(*),       nt(*),          nprir(2,mstu,*), ntl(*),
     &  ntu(*),        labels(nipv,*), ibitv(*)
c
       real*8
     &  a(*),      cx(*),     eta(mrcru,mconu,*),  h4(*),
     &  x(mcu,*),  y(mcu,*),  z(mcu,*),            zet(mconu,*),
     &  buffer(*), values(*)
c     # ibm+convex f77 bug; actual:
c     # integer labels(nipv,n2max), ibitv( ((n2max+63)/64)*64 )
c     # real*8 buffer(l2rec), values(n2max)
c
c     # local:
       integer npkflg, isf, is, ifu, isfis, iksfis, ilsfis, ic,
     &  icu, ieqs1, jsf, js, jfu, ieqs2, jsfjs, ijsfjs, jlsfjs,
     &  ksf, ks, kfu, ieqs3, ksfks, iksfks, jksfks, lsf, ls, lfu,
     &  ncc, lsfls, klsfls, jlsfls, if, iksfif, ilsfif, ieqsf1,
     &  jf, jlsfjf, ieqsf2, kf, ieqsf3, lf, ibl, iscm, ist, nbli,
     &  nblk, jesfb, iijgt, iijx, iijy, iijz, kklgt, kklx, kkly,
     &  kklz, kblu, icc, itst, igwu, iprm, lc1, ngw1, igw, kc1,
     &  itst1, kc2, lc2, ic1, jc1, mxblu
c
      integer    iju(3),      ijb(2,3), klu(3),  klb(2,3), niqd(3),
     & nqd(3)
      integer    iperm(4,24), ip21(4),  ip31(6), ip41(12), ip51(4),
     & ip61(12), ip71(12),    ip75(3),  ip83(4), ip85(6)
c
      logical eijs, ejks, ekls
c
      character*8 chrtyp
c
      integer    nomore
      parameter( nomore = 2 )
c
c     # bummer error types.
      integer   wrnerr,  nfterr,  faterr
      parameter(wrnerr=0,nfterr=1,faterr=2)
c
      integer  atebyt, forbyt
      external atebyt, forbyt
c
*mdc*if parallel
*      integer me, nproc, ijkl
*      integer  nodeid, nnodes
*      external nodeid, nnodes
*mdc*endif
c
      data iperm /
     & 1,2,3,4, 2,1,3,4, 1,3,2,4, 3,1,2,4, 2,3,1,4, 3,2,1,4,
     & 1,2,4,3, 2,1,4,3, 1,4,2,3, 4,1,2,3, 2,4,1,3, 4,2,1,3,
     & 1,3,4,2, 3,1,4,2, 1,4,3,2, 4,1,3,2, 3,4,1,2, 4,3,1,2,
     & 2,3,4,1, 3,2,4,1, 2,4,3,1, 4,2,3,1, 3,4,2,1, 4,3,2,1 /
c
      data ip21 /1,7,13,19/
      data ip31 /1,3,9,5,11,17/
      data ip41 /1,3,5,7,9,11,13,15,17,19,21,23/
      data ip51 /10,4,2,1/
      data ip61 /4,2,1,10,8,7,16,14,13,22,20,19/
      data ip71 /17,11,9,18,5,3,12,6,1,10,4,2/
      data ip75 /9,3,1/
      data ip83 /1,2,7,8/
      data ip85 /1,3,7,9,13,15/
c
c     # initialize the integral record counters and the pk bit-vector.
c
      call w2stup( ibitv )
c
      itypea = 3
      itypeb = 0
c
      npkflg = 0
c
      nwt1(3) = 1
      isf = 0
c     # ijsf(1) is ijsf
      ijsf(1) = 0
c     # ijsf(2) is iksf
      ijsf(2) = 0
c     # ijsf(3) is ilsf
      ijsf(3) = 0
c
*mdc*if parallel
*c     # These are used to associate shell blocks to processes.
*      me = nodeid()
*      nproc = nnodes()
*      ijkl = me
*mdc*endif
c
      do 990 is = 1,ns
      ifu = nf(is)
      if(ifu.eq.0) go to 990
      isfis= isf
      iksfis = ijsf(2)
      ilsfis = ijsf(3)
      ic = nc(is)
      icu = nc(is)
      ieqs1 = 8
      jsf = 0
c     # klsf(2) is jlsf
      klsf(2) = 0
c     # klsf(3) is jksf
      klsf(3) = 0
      do 980 js = 1,is
      eijs=is.eq.js
      if(eijs) then
        ieqs1=4
      else
        jfu = nf(js)
        if(jfu.eq.0) go to 980
      endif
      ieqs2 = ieqs1
      jcu = nc(js)
      jsfjs = jsf
      ijsfjs = ijsf(1)
      jlsfjs = klsf(2)
      ksf = 0
      ijsf(2) = iksfis
c     # klsf(1) is klsf
      klsf(1) = 0
      do 970 ks = 1,js
      ejks=js.eq.ks
      if(ejks) then
        ieqs2=ieqs1-2
      else
        kfu = nf(ks)
        if(kfu.eq.0) go to 970
      endif
      ieqs3 = ieqs2
      kcu = nc(ks)
      ksfks= ksf
      iksfks = ijsf(2)
      jksfks = klsf(3)
      lsf = 0
      ijsf(3) = ilsfis
      klsf(2) = jlsfjs
      do 960 ls = 1,ks
      ekls=ks.eq.ls
      if(ekls) then
        ieqs3=ieqs2-1
      else
        lfu = nf(ls)
        if(lfu.eq.0) go to 960
      endif
      ncc = 0
      lcu = nc(ls)
      lsfls = lsf
      klsfls = klsf(1)
      jlsfls = klsf(2)
      call cecob( ic, ica, icb, ieqs3, is, js, ks, ls, ncc, nfct, ngw )
      isf = isfis
      ijsf(1) = ijsfjs
      ijsf(2) = iksfks
      do 930 if = 1, ifu
      isf = isf + 1
      ircru=nrcr(mcons(isf))
      itl = ntl(isf)
      itu = ntu(isf)
      iksfif = ijsf(2)
      ilsfif = ijsf(3)
      ieqsf1 = 8
      jsf = jsfjs
      klsf(2) = jlsfls
      klsf(3) = jksfks
      if(eijs) jfu=if
      do 930 jf = 1, jfu
      jlsfjf = klsf(2)
      jsf = jsf + 1
      jrcru=nrcr(mcons(jsf))
      jtl = ntl(jsf)
      jtu = ntu(jsf)
      ijsf(1) = ijsf(1) + 1
      iju(1) = nt(isf)*nt(jsf)
      ntij=iju(1)*npair(2,ijsf(1))
      esfi(1)=isf.eq.jsf
      if(esfi(1)) then
        ieqsf1=4
        ntij1=iju(1)*npair(1,ijsf(1))
        ijb(1,1)=ircru
      endif
      ijb(2,1)=ircru*jrcru
      ieqsf2 = ieqsf1
      ksf = ksfks
      ijsf(2) = iksfif
      klsf(1) = klsfls
      if(ejks) kfu=jf
      do 930 kf = 1, kfu
      ksf = ksf + 1
      krcru=nrcr(mcons(ksf))
      ktl = ntl(ksf)
      ktu = ntu(ksf)
      ijsf(2) = ijsf(2) + 1
      iju(2) = nt(isf)*nt(ksf)
      ntik=iju(2)*npair(2,ijsf(2))
      klsf(3) = klsf(3) + 1
      klu(3) = nt(jsf)*nt(ksf)
      ntjk=klu(3)*npair(2,klsf(3))
      esfi(2)=isf.eq.ksf
      if(esfi(2)) ijb(1,2)=ircru
      ijb(2,2)=ircru*krcru
      esfk(3)=jsf.eq.ksf
      if(esfk(3)) then
        ieqsf2=ieqsf1-2
        ntjk1=klu(3)*npair(1,klsf(3))
        klb(1,3)=jrcru
      endif
      klb(2,3)=jrcru*krcru
      ieqsf3 = ieqsf2
      lsf = lsfls
      ijsf(3) = ilsfif
      klsf(2) = jlsfjf
      if(ekls) lfu=kf
      do 920 lf = 1, lfu
      lsf = lsf + 1
      lrcru=nrcr(mcons(lsf))
      ltl = ntl(lsf)
      ltu = ntu(lsf)
      ijsf(3) = ijsf(3) + 1
      iju(3) = nt(isf)*nt(lsf)
      ntil=iju(3)*npair(2,ijsf(3))
      klsf(1) = klsf(1) + 1
      klu(1) = nt(ksf)*nt(lsf)
      ntkl=klu(1)*npair(2,klsf(1))
      klsf(2) = klsf(2) + 1
      klu(2) = nt(jsf)*nt(lsf)
      ntjl=klu(2)*npair(2,klsf(2))
      esfi(3)=isf.eq.lsf
      if(esfi(3)) ijb(1,3)=ircru
      ijb(2,3)=ircru*lrcru
      esfk(2)=jsf.eq.lsf
      if(esfk(2)) klb(1,2)=jrcru
      klb(2,2)=jrcru*lrcru
      esfk(1)=ksf.eq.lsf
      if(esfk(1)) then
        ieqsf3=ieqsf2-1
        ntkl1=klu(1)*npair(1,klsf(1))
        klb(1,1)=krcru
      endif
      klb(2,1)=krcru*lrcru
c
*mdc*if parallel
*      ijkl = ijkl + 1
*      if ( mod(ijkl,nproc) .ne. 0 ) goto 920
*mdc*endif
c
c     # set up arrays by permutation
      ibl = 0
      do 40 iscm = 1, 3
        if( (iscm .ne. 1) .and.
     &   (max( ijsf(iscm), klsf(iscm)) .eq. ijsf(iscm-1)) ) then
c         # (same block of integrals)
          nqd(iscm) = nqd(iscm-1)
          if(esfi(iscm-1)) then
            npri(1,iscm)=npri(1,iscm-1)
            do 22 ist=1,nst
              npriri(1,iscm,ist)=npriri(1,iscm-1,ist)
22          continue
          endif
          npri(2,iscm)=npri(2,iscm-1)
          do 24 ist=1,nst
            npriri(2,iscm,ist)=npriri(2,iscm-1,ist)
24        continue
          if ( esfk(iscm-1) ) then
            nprk(1,iscm)=nprk(1,iscm-1)
            do 26 ist=1,nst
              nprirk(1,iscm,ist)=nprirk(1,iscm-1,ist)
26          continue
          endif
          nprk(2,iscm)=nprk(2,iscm-1)
          do 28 ist=1,nst
            nprirk(2,iscm,ist)=nprirk(2,iscm-1,ist)
28        continue
          niqd(iscm) = 0
        else
c         # (new block of integrals)
          if ( esfi(iscm) ) then
            npri(1,iscm) = npair(1,ijsf(iscm))
            do 30 ist = 1, nst
              npriri(1,iscm,ist)=nprir(1,ist,ijsf(iscm))
30          continue
          endif
          npri(2,iscm) = npair(2,ijsf(iscm))
          do 32 ist = 1, nst
            npriri(2,iscm,ist) = nprir(2,ist,ijsf(iscm))
32        continue
          if ( esfk(iscm) ) then
            nprk(1,iscm) = npair(1,klsf(iscm))
            do 34 ist = 1, nst
              nprirk(1,iscm,ist) = nprir(1,ist,klsf(iscm))
34          continue
          endif
          nprk(2,iscm) = npair(2,klsf(iscm))
          do 36 ist = 1, nst
            nprirk(2,iscm,ist) = nprir(2,ist,klsf(iscm))
36        continue
          nblu = 0
          eprsf(iscm) = ijsf(iscm).eq.klsf(iscm)
          do 38 ist = 1, nst
            if ( esfi(iscm) ) then
              nbli = ijb(1,iscm) * npriri(1,iscm,ist)
     &         + ipq(ijb(1,iscm)) * npriri(2,iscm,ist)
            else
              nbli = ijb(2,iscm)*npriri(2,iscm,ist)
            endif
            if ( eprsf(iscm) ) then
              nblu = nblu + (nbli * (nbli + 1)) / 2
            else
              if ( esfk(iscm) ) then
                nblk = klb(1,iscm) * nprirk(1,iscm,ist)
     &           + ipq(klb(1,iscm)) * nprirk(2,iscm,ist)
              else
                nblk = klb(2,iscm) * nprirk(2,iscm,ist)
              endif
              nblu = nblu + nbli * nblk
            endif
38        continue
          nqd(iscm)  = nblu
          niqd(iscm) = nblu
        endif
40    continue
c
      do 60 jesfb = 1, 2
        ijcxst(jesfb) = icxst(jesfb,ijsf(1))
        ikcxst(jesfb) = icxst(jesfb,ijsf(2))
        ilcxst(jesfb) = icxst(jesfb,ijsf(3))
        klcxst(jesfb) = icxst(jesfb,klsf(1))
        jlcxst(jesfb) = icxst(jesfb,klsf(2))
        jkcxst(jesfb) = icxst(jesfb,klsf(3))
60    continue
c
      do 910 iscm = 1, 3
      if ( niqd(iscm) .eq. 0 ) go to 910
      iesfb = 2
      kesfb = 2
c
c     # allocate nblu workspace for this shell-triple component.
      nblu   = nqd(iscm)
c     # allocate space.
c     # ipt(*)-->1:lmnv(1:3,1:lmnvmx), 2:il(1:nnbft), 3:cx(1:mcxu),
c     #          4:buffer(1:l2rec), 5:values(1:n2max),
c     #          6:labels(1:4,1:n2max), 7:ibitv(1:n2max), 8:h4(1:nblu)
c     #          9:ijgt(1:iju(iscm)), 10:ijx(1:iju(iscm)),
c     #          11:ijy(1:iju(iscm)), 12:ijz(1:iju(iscm)),
c     #          13:klgt(1:klu(iscm)), 14:klx(1:klu(iscm)),
c     #          15:kly(1:klu(iscm)), 16:klz(1:klu(iscm)),
c     #          17:g4(1:ic3)
      ipt(9)  = ipt(8)  + atebyt( nblu )
      ipt(10) = ipt(9)  + forbyt( iju(iscm) )
      ipt(11) = ipt(10) + forbyt( iju(iscm) )
      ipt(12) = ipt(11) + forbyt( iju(iscm) )
      ipt(13) = ipt(12) + forbyt( iju(iscm) )
      ipt(14) = ipt(13) + forbyt( klu(iscm) )
      ipt(15) = ipt(14) + forbyt( klu(iscm) )
      ipt(16) = ipt(15) + forbyt( klu(iscm) )
      ipt(17) = ipt(16) + forbyt( klu(iscm) )
        if ( (ipt(17)-1) .gt. mblu ) then
          call bummer('twoint: mblu too small ',(ipt(17)-1-mblu),faterr)
        endif
c
c     # push the current alloction onto the high-water mark stack.
      call h2opsh( (ipt(17)-ipt(8)) )
c
      call wzero(nblu,h4,1)
c
      nwt = 1
      inx = 1
      jnx = 2
      knx = 3
      lnx = 4
      go to (100,200,300,400,500,600,700,800), ieqsf3
c
c  isf = jsf = ksf = lsf section
c
  100 do 110 icc = 1, ncc
c     # ieqsf3=1, ieqs3=1
  110       call sf3eq1( a,          cx,         eta,        a(ipt(17)),
     &       h4,         icb,        icc,        ipt,        is,
     &       isf,        a(ipt(9)),  a(ipt(10)), a(ipt(11)), a(ipt(12)),
     &       a(ipt(13)), a(ipt(14)), a(ipt(15)), a(ipt(16)), lmnp1,
     &       lmnv,       mcons,      nc,         ncon,       nfct,
     &       ngw,        nrcr,       nt,         ntl,        ntu,
     &       x,          y,          z,          zet )
      go to 900
c
c  isf = jsf = ksf > lsf section
c
  200 do 299 icc=1,ncc
      if(ieqs3.ne.1) go to 220
c     # ieqsf3=2, ieqs3=1
      itst = 0
      igwu = ngw(icc)
      do 219 iprm = 1, 4
      inx = iperm(1,ip21(iprm))
      jnx = iperm(2,ip21(iprm))
      knx = iperm(3,ip21(iprm))
      lnx = iperm(4,ip21(iprm))
      lc1 = icb(lnx,1,icc)
      if(lc1.eq.itst) go to 219
      nwt = 1
      if(igwu.eq.1) go to 217
      ngw1 = 1
      do 216 igw=2,igwu
        if(icb(lnx,igw,icc)-lc1) 218, 214, 215
  214   ngw1 = ngw1 + 1
  215   nwt = nwt + 1
  216 continue
      nwt = nwt/ngw1
  217       call sf3eq2( a,          cx,         eta,        a(ipt(17)),
     &       h4,         icb,        icc,        ipt,        is,
     &       isf,        ls,         lsf,        a(ipt(9)),  a(ipt(10)),
     &       a(ipt(11)), a(ipt(12)), a(ipt(13)), a(ipt(14)), a(ipt(15)),
     &       a(ipt(16)), lmnp1,      lmnv,       mcons,      nc,
     &       ncon,       nfct,       ngw,        nrcr,       nt,
     &       ntl,        ntu,        x,          y,          z,
     &       zet )
  218 itst= lc1
  219 continue
      go to 299
c     # ieqsf3=2, ieqs3=2
  220       call sf3eq2( a,          cx,         eta,        a(ipt(17)),
     &       h4,         icb,        icc,        ipt,        is,
     &       isf,        ls,         lsf,        a(ipt(9)),  a(ipt(10)),
     &       a(ipt(11)), a(ipt(12)), a(ipt(13)), a(ipt(14)), a(ipt(15)),
     &       a(ipt(16)), lmnp1,      lmnv,       mcons,      nc,
     &       ncon,       nfct,       ngw,        nrcr,       nt,
     &       ntl,        ntu,        x,          y,          z,
     &       zet )
  299 continue
      go to 900
c
c  isf = jsf > ksf = lsf section
c
  300 do 399 icc = 1, ncc
      if(ieqs3.ne.1) go to 330
c     # ieqsf3=3, ieqs3=1
      itst = 0
      igwu = ngw(icc)
      do 319 iprm = 1, 6
      inx = iperm(1,ip31(iprm))
      jnx = iperm(2,ip31(iprm))
      knx = iperm(3,ip31(iprm))
      lnx = iperm(4,ip31(iprm))
      kc1 = icb(knx,1,icc)
      lc1 = icb(lnx,1,icc)
      itst1=ipq(kc1)+lc1
      if (itst1 .le. itst) go to 319
      nwt = 1
      if (igwu .eq. 1) go to 317
      ngw1 = 1
      do 316 igw = 2, igwu
        kc2 = icb(knx,igw,icc)
        lc2 = icb(lnx,igw,icc)
        if(ipq(max(kc2,lc2))+min(kc2,lc2)-itst1) 318, 314, 315
  314   ngw1 = ngw1 + 1
  315   nwt = nwt + 1
  316 continue
      nwt = nwt/ngw1
  317       call sf3eq3( a,          cx,         eta,        a(ipt(17)),
     &       h4,         icb,        icc,        ipt,        iscm,
     &       is,         isf,        ks,         ksf,        a(ipt(9)),
     &       a(ipt(10)), a(ipt(11)), a(ipt(12)), a(ipt(13)), a(ipt(14)),
     &       a(ipt(15)), a(ipt(16)), lmnp1,      lmnv,       mcons,
     &       nc,         ncon,       nfct,       ngw,        nrcr,
     &       nt,         ntl,        ntu,        x,          y,
     &       z,          zet )
  318 itst= itst1
  319 continue
      go to 399
c     # ieqsf3=3, ieqs3=3
  330       call sf3eq3( a,          cx,         eta,        a(ipt(17)),
     &       h4,         icb,        icc,        ipt,        iscm,
     &       is,         isf,        ks,         ksf,        a(ipt(9)),
     &       a(ipt(10)), a(ipt(11)), a(ipt(12)), a(ipt(13)), a(ipt(14)),
     &       a(ipt(15)), a(ipt(16)), lmnp1,      lmnv,       mcons,
     &       nc,         ncon,       nfct,       ngw,        nrcr,
     &       nt,         ntl,        ntu,        x,          y,
     &       z,          zet )
  399 continue
      go to 900
c
c  isf = jsf > ksf > lsf section
c
  400 do 499 icc = 1, ncc
      go to (410,420,430,440), ieqs3
c     # ieqsf3=4, ieqs3=1
  410 itst = 0
      igwu = ngw(icc)
      do 419 iprm = 1, 12
      inx = iperm(1,ip41(iprm))
      jnx = iperm(2,ip41(iprm))
      knx = iperm(3,ip41(iprm))
      lnx = iperm(4,ip41(iprm))
      lc1 = icb(lnx,1,icc)
      kc1 = icb(knx,1,icc)
      itst1 = kc1 + lc1 * icu
      if (itst1 .le. itst) go to 419
      nwt = 1
      if (igwu .eq. 1) go to 417
      ngw1 = 1
      do 416 igw = 2, igwu
        if(icb(knx,igw,icc)+icb(lnx,igw,icc)*icu-itst1) 418, 414, 415
  414   ngw1 = ngw1 + 1
  415   nwt = nwt + 1
  416 continue
      nwt = nwt/ngw1
  417       call sf3eq4( a,          cx,         eta,        a(ipt(17)),
     &       h4,         icb,        icc,        ipt,        iscm,
     &       is,         isf,        ks,         ksf,        ls,
     &       lsf,        a(ipt(9)),  a(ipt(10)), a(ipt(11)), a(ipt(12)),
     &       a(ipt(13)), a(ipt(14)), a(ipt(15)), a(ipt(16)), lmnp1,
     &       lmnv,       mcons,      nc,         ncon,       nfct,
     &       ngw,        nrcr,       nt,         ntl,        ntu,
     &       x,          y,          z,          zet )
  418 itst= itst1
  419 continue
      go to 499
c     # ieqsf3=4, ieqs3=2; use first part of ip41 in place of ip42
  420 itst = 0
      igwu = ngw(icc)
      do 429 iprm = 1, 3
      inx = iperm(1,ip41(iprm))
      jnx = iperm(2,ip41(iprm))
      knx = iperm(3,ip41(iprm))
      kc1 = icb(knx,1,icc)
      if(kc1.eq.itst) go to 429
      nwt = 1
      if(igwu.eq.1) go to 427
      ngw1 = 1
      do 426 igw = 2, igwu
        if(icb(knx,igw,icc)-kc1) 428, 424, 425
  424   ngw1 = ngw1 + 1
  425   nwt = nwt + 1
  426 continue
      nwt = nwt /ngw1
  427       call sf3eq4( a,          cx,         eta,        a(ipt(17)),
     &       h4,         icb,        icc,        ipt,        iscm,
     &       is,         isf,        ks,         ksf,        ls,
     &       lsf,        a(ipt(9)),  a(ipt(10)), a(ipt(11)), a(ipt(12)),
     &       a(ipt(13)), a(ipt(14)), a(ipt(15)), a(ipt(16)), lmnp1,
     &       lmnv,       mcons,      nc,         ncon,       nfct,
     &       ngw,        nrcr,       nt,         ntl,        ntu,
     &       x,          y,          z,          zet )
  428 itst = kc1
  429 continue
      go to 499
c     # ieqsf3=4, ieqs3=3; use first part of ip21 in place of ip43
  430 itst = 0
      igwu = ngw(icc)
      do 439 iprm = 1, 2
      knx = iperm(3,ip21(iprm))
      lnx = iperm(4,ip21(iprm))
      lc1 = icb (lnx,1,icc)
      if(lc1.eq.itst) go to 439
      nwt = 1
      if (igwu .eq. 1) go to 437
      ngw1 = 1
      do 436 igw = 2, igwu
        if(icb(lnx,igw,icc)-lc1) 438, 434, 435
  434   ngw1 = ngw1 + 1
  435   nwt = nwt + 1
  436 continue
      nwt = nwt /ngw1
  437       call sf3eq4( a,          cx,         eta,        a(ipt(17)),
     &       h4,         icb,        icc,        ipt,        iscm,
     &       is,         isf,        ks,         ksf,        ls,
     &       lsf,        a(ipt(9)),  a(ipt(10)), a(ipt(11)), a(ipt(12)),
     &       a(ipt(13)), a(ipt(14)), a(ipt(15)), a(ipt(16)), lmnp1,
     &       lmnv,       mcons,      nc,         ncon,       nfct,
     &       ngw,        nrcr,       nt,         ntl,        ntu,
     &       x,          y,          z,          zet )
  438 itst = lc1
  439 continue
      go to 499
c     # ieqsf3=4, ieqs3=4
  440       call sf3eq4( a,          cx,         eta,        a(ipt(17)),
     &       h4,         icb,        icc,        ipt,        iscm,
     &       is,         isf,        ks,         ksf,        ls,
     &       lsf,        a(ipt(9)),  a(ipt(10)), a(ipt(11)), a(ipt(12)),
     &       a(ipt(13)), a(ipt(14)), a(ipt(15)), a(ipt(16)), lmnp1,
     &       lmnv,       mcons,      nc,         ncon,       nfct,
     &       ngw,        nrcr,       nt,         ntl,        ntu,
     &       x,          y,          z,          zet )
  499 continue
      go to 900
c
c  isf > jsf = ksf = lsf section
c
  500 do 599 icc = 1, ncc
      if(ieqs3.ne.1) go to 550
c     # ieqsf3=5, ieqs3=1
      itst = 0
      igwu = ngw(icc)
      do 519 iprm = 1, 4
      inx = iperm(1,ip51(iprm))
      jnx = iperm(2,ip51(iprm))
      knx = iperm(3,ip51(iprm))
      lnx = iperm(4,ip51(iprm))
      ic1 = icb(inx,1,icc)
      if (ic1 .eq. itst) go to 519
      nwt = 1
      if (igwu .eq. 1) go to 517
      ngw1 = 1
      do 516 igw = 2, igwu
        if (icb(inx,igw,icc) - ic1) 518, 514, 515
  514   ngw1 = ngw1 + 1
  515   nwt = nwt + 1
  516 continue
      nwt = nwt/ngw1
  517       call sf3eq5( a,          cx,         eta,        a(ipt(17)),
     &       h4,         icb,        icc,        ipt,        is,
     &       isf,        js,         jsf,        a(ipt(9)),  a(ipt(10)),
     &       a(ipt(11)), a(ipt(12)), a(ipt(13)), a(ipt(14)), a(ipt(15)),
     &       a(ipt(16)), lmnp1,      lmnv,       mcons,      nc,
     &       ncon,       nfct,       ngw,        nrcr,       nt,
     &       ntl,        ntu,        x,          y,          z,
     &       zet )
  518 itst = ic1
  519 continue
      go to 599
c     # ieqsf3=5, ieqs3=5
  550       call sf3eq5( a,          cx,         eta,        a(ipt(17)),
     &       h4,         icb,        icc,        ipt,        is,
     &       isf,        js,         jsf,        a(ipt(9)),  a(ipt(10)),
     &       a(ipt(11)), a(ipt(12)), a(ipt(13)), a(ipt(14)), a(ipt(15)),
     &       a(ipt(16)), lmnp1,      lmnv,       mcons,      nc,
     &       ncon,       nfct,       ngw,        nrcr,       nt,
     &       ntl,        ntu,        x,          y,          z,
     &       zet )
  599 continue
      go to 900
c
c  isf > jsf = ksf > lsf section
c
  600 do 699 icc=1,ncc
      go to (610,620,601,601,650,660), ieqs3
  601 call bummer('stop at 601; ieqs3 = ',ieqs3,faterr)
c     # ieqsf3=6, ieqs3=1
  610 itst = 0
      igwu = ngw(icc)
      do 619 iprm = 1, 12
      inx = iperm(1,ip61(iprm))
      jnx = iperm(2,ip61(iprm))
      knx = iperm(3,ip61(iprm))
      lnx = iperm(4,ip61(iprm))
      lc1 = icb(lnx,1,icc)
      ic1 = icb(inx,1,icc)
      itst1 = ic1 + lc1 * icu
      if (itst1 .le. itst) go to 619
      nwt = 1
      if (igwu .eq. 1) go to 617
      ngw1 = 1
      do 616 igw = 2, igwu
        if (icb(inx,igw,icc)+icb(lnx,igw,icc)*icu-itst1) 618, 614, 615
  614   ngw1 = ngw1+ 1
  615   nwt = nwt + 1
  616 continue
      nwt = nwt/ngw1
  617       call sf3eq6( a,          cx,         eta,        a(ipt(17)),
     &       h4,         icb,        icc,        ipt,        iscm,
     &       is,         isf,        js,         jsf,        ls,
     &       lsf,        a(ipt(9)),  a(ipt(10)), a(ipt(11)), a(ipt(12)),
     &       a(ipt(13)), a(ipt(14)), a(ipt(15)), a(ipt(16)), lmnp1,
     &       lmnv,       mcons,      nc,         ncon,       nfct,
     &       ngw,        nrcr,       nt,         ntl,        ntu,
     &       x,          y,          z,          zet )
  618 itst = itst1
  619 continue
      go to 699
c     # ieqsf3=6, ieqs3=2; use first part of ip61 in place of ip62
  620 itst = 0
      igwu = ngw(icc)
      do 629 iprm = 1, 3
      inx = iperm(1,ip61(iprm))
      jnx = iperm(2,ip61(iprm))
      knx = iperm(3,ip61(iprm))
      ic1 = icb(inx,1,icc)
      if (ic1 .eq. itst) go to 629
      nwt = 1
      if (igwu .eq. 1) go to 627
      ngw1 = 1
      do 626 igw = 2, igwu
        if (icb(inx,igw,icc) - ic1) 628, 624, 625
  624   ngw1 = ngw1 + 1
  625   nwt = nwt + 1
  626 continue
      nwt = nwt/ngw1
  627       call sf3eq6( a,          cx,         eta,        a(ipt(17)),
     &       h4,         icb,        icc,        ipt,        iscm,
     &       is,         isf,        js,         jsf,        ls,
     &       lsf,        a(ipt(9)),  a(ipt(10)), a(ipt(11)), a(ipt(12)),
     &       a(ipt(13)), a(ipt(14)), a(ipt(15)), a(ipt(16)), lmnp1,
     &       lmnv,       mcons,      nc,         ncon,       nfct,
     &       ngw,        nrcr,       nt,         ntl,        ntu,
     &       x,          y,          z,          zet )
  628 itst = ic1
  629 continue
      go to 699
c     # ieqsf3=6, ieqs3=5; use first part of ip21 in place of ip65
  650 itst = 0
      igwu = ngw(icc)
      do 659 iprm = 1, 3
      jnx = iperm(2,ip21(iprm))
      knx = iperm(3,ip21(iprm))
      lnx = iperm(4,ip21(iprm))
      lc1 = icb(lnx,1,icc)
      if (lc1 .eq. itst) go to 659
      nwt = 1
      if (igwu .eq. 1) go to 657
      ngw1 = 1
      do 656 igw = 2, igwu
        if (icb(lnx,igw,icc) - lc1) 658, 654, 655
  654   ngw1 = ngw1 + 1
  655   nwt = nwt + 1
  656 continue
      nwt = nwt/ngw1
  657       call sf3eq6( a,          cx,         eta,        a(ipt(17)),
     &       h4,         icb,        icc,        ipt,        iscm,
     &       is,         isf,        js,         jsf,        ls,
     &       lsf,        a(ipt(9)),  a(ipt(10)), a(ipt(11)), a(ipt(12)),
     &       a(ipt(13)), a(ipt(14)), a(ipt(15)), a(ipt(16)), lmnp1,
     &       lmnv,       mcons,      nc,         ncon,       nfct,
     &       ngw,        nrcr,       nt,         ntl,        ntu,
     &       x,          y,          z,          zet )
  658 itst = lc1
  659 continue
      go to 699
c     # ieqsf3=6, ieqs3=6
  660       call sf3eq6( a,          cx,         eta,        a(ipt(17)),
     &       h4,         icb,        icc,        ipt,        iscm,
     &       is,         isf,        js,         jsf,        ls,
     &       lsf,        a(ipt(9)),  a(ipt(10)), a(ipt(11)), a(ipt(12)),
     &       a(ipt(13)), a(ipt(14)), a(ipt(15)), a(ipt(16)), lmnp1,
     &       lmnv,       mcons,      nc,         ncon,       nfct,
     &       ngw,        nrcr,       nt,         ntl,        ntu,
     &       x,          y,          z,          zet )
  699 continue
      go to 900
c
c  isf > jsf > ksf = lsf section
c
  700 do 799 icc=1,ncc
      go to (710,701,730,701,750,701,770), ieqs3
  701 call bummer('stop at 701; ieqs3 = ',ieqs3,faterr)
c     # ieqsf3=7, ieqs3=1
  710 itst = 0
      igwu = ngw(icc)
      do 719 iprm = 1, 12
      inx = iperm(1,ip71(iprm))
      jnx = iperm(2,ip71(iprm))
      knx = iperm(3,ip71(iprm))
      lnx = iperm(4,ip71(iprm))
      jc1 = icb(jnx,1,icc)
      ic1 = icb(inx,1,icc)
      itst1 = ic1 + jc1 * icu
      if (itst1 .le. itst) go to 719
      nwt = 1
      if (igwu .eq. 1) go to 717
      ngw1 = 1
      do 716 igw = 2, igwu
        if(icb(inx,igw,icc)+icb(jnx,igw,icc)*icu-itst1) 718, 714, 715
  714   ngw1 = ngw1 + 1
  715   nwt = nwt + 1
  716 continue
      nwt = nwt/ngw1
  717       call sf3eq7( a,          cx,         eta,        a(ipt(17)),
     &       h4,         icb,        icc,        ipt,        iscm,
     &       is,         isf,        js,         jsf,        ks,
     &       ksf,        a(ipt(9)),  a(ipt(10)), a(ipt(11)), a(ipt(12)),
     &       a(ipt(13)), a(ipt(14)), a(ipt(15)), a(ipt(16)), lmnp1,
     &       lmnv,       mcons,      nc,         ncon,       nfct,
     &       ngw,        nrcr,       nt,         ntl,        ntu,
     &       x,          y,          z,          zet )
  718 itst= itst1
  719 continue
      go to 799
c     # ieqsf3=7, ieqs3=3
  730 itst = 0
      igwu = ngw(icc)
      do 739 iprm = 1, 2
      inx = iperm(1,iprm)
      jnx = iperm(2,iprm)
      jc1 = icb(jnx,1,icc)
      if (jc1 .eq. itst) go to 739
      nwt = 1
      if (igwu .eq. 1) go to 737
      ngw1 = 1
      do 736 igw = 2, igwu
        if(icb(jnx,igw,icc) - jc1) 738, 734, 735
  734   ngw1 = ngw1 + 1
  735   nwt = nwt + 1
  736 continue
      nwt = nwt /ngw1
  737       call sf3eq7( a,          cx,         eta,        a(ipt(17)),
     &       h4,         icb,        icc,        ipt,        iscm,
     &       is,         isf,        js,         jsf,        ks,
     &       ksf,        a(ipt(9)),  a(ipt(10)), a(ipt(11)), a(ipt(12)),
     &       a(ipt(13)), a(ipt(14)), a(ipt(15)), a(ipt(16)), lmnp1,
     &       lmnv,       mcons,      nc,         ncon,       nfct,
     &       ngw,        nrcr,       nt,         ntl,        ntu,
     &       x,          y,          z,          zet )
  738 itst = jc1
  739 continue
      go to 799
c     # ieqsf3=7, ieqs3=5
  750 itst = 0
      igwu = ngw(icc)
      do 759 iprm = 1, 3
      jnx = iperm(2,ip75(iprm))
      knx = iperm(3,ip75(iprm))
      lnx = iperm(4,ip75(iprm))
      jc1 = icb(jnx,1,icc)
      if (jc1 .eq. itst) go to 759
      nwt = 1
      if (igwu .eq. 1) go to 757
      ngw1 = 1
      do 756  igw = 2, igwu
        if (icb(jnx,igw,icc) - jc1) 758, 754, 755
  754   ngw1 = ngw1 + 1
  755   nwt = nwt + 1
  756 continue
      nwt = nwt/ngw1
  757       call sf3eq7( a,          cx,         eta,        a(ipt(17)),
     &       h4,         icb,        icc,        ipt,        iscm,
     &       is,         isf,        js,         jsf,        ks,
     &       ksf,        a(ipt(9)),  a(ipt(10)), a(ipt(11)), a(ipt(12)),
     &       a(ipt(13)), a(ipt(14)), a(ipt(15)), a(ipt(16)), lmnp1,
     &       lmnv,       mcons,      nc,         ncon,       nfct,
     &       ngw,        nrcr,       nt,         ntl,        ntu,
     &       x,          y,          z,          zet )
  758 itst = jc1
  759 continue
      go to 799
c     # ieqsf3=7, ieqs3=7
  770       call sf3eq7( a,          cx,         eta,        a(ipt(17)),
     &       h4,         icb,        icc,        ipt,        iscm,
     &       is,         isf,        js,         jsf,        ks,
     &       ksf,        a(ipt(9)),  a(ipt(10)), a(ipt(11)), a(ipt(12)),
     &       a(ipt(13)), a(ipt(14)), a(ipt(15)), a(ipt(16)), lmnp1,
     &       lmnv,       mcons,      nc,         ncon,       nfct,
     &       ngw,        nrcr,       nt,         ntl,        ntu,
     &       x,          y,          z,          zet )
  799 continue
      go to 900
c
c  isf > jsf > ksf > lsf section
c
  800 do 899 icc=1,ncc
      go to (810,820,830,840,850,860,870,880), ieqs3
c     # ieqsf3=8, ieqs3=1
  810 itst = 0
      igwu = ngw(icc)
      do 819 iprm = 1, 24
      inx = iperm(1,iprm)
      jnx = iperm(2,iprm)
      knx = iperm(3,iprm)
      lnx = iperm(4,iprm)
      lc1 = icb(lnx,1,icc)
      kc1 = icb(knx,1,icc)
      jc1 = icb(jnx,1,icc)
      itst1 = jc1 + (kc1 + lc1 * icu) * icu
      if (itst1 .le. itst) go to 819
      nwt = 1
      do 816 igw = 2, igwu
        if(icb(jnx,igw,icc)+(icb(knx,igw,icc)+icb(lnx,igw,icc)*icu)*icu
     1    .lt.itst1) go to 818
        nwt = nwt + 1
  816 continue
            call sf3eq8( a,          cx,         eta,        a(ipt(17)),
     &       h4,         icb,        icc,        ipt,        iscm,
     &       is,         isf,        js,         jsf,        ks,
     &       ksf,        ls,         lsf,        a(ipt(9)),  a(ipt(10)),
     &       a(ipt(11)), a(ipt(12)), a(ipt(13)), a(ipt(14)), a(ipt(15)),
     &       a(ipt(16)), lmnp1,      lmnv,       mcons,      nc,
     &       ncon,       nfct,       nrcr,       nt,         ntl,
     &       ntu,        x,          y,          z,          zet )
  818 itst = itst1
  819 continue
      go to 899
c     # ieqsf3=8, ieqs3=2
  820 itst = 0
      igwu = ngw(icc)
      do 829 iprm = 1, 6
      inx = iperm(1,iprm)
      jnx = iperm(2,iprm)
      knx = iperm(3,iprm)
      kc1 = icb(knx,1,icc)
      jc1 = icb(jnx,1,icc)
      itst1 = jc1 + kc1 * icu
      if (itst1 .le. itst) go to 829
      nwt = 1
      do 826 igw = 2, igwu
        if(icb(jnx,igw,icc)+icb(knx,igw,icc)*icu.lt.itst1) go to 828
        nwt = nwt + 1
  826 continue
            call sf3eq8( a,          cx,         eta,        a(ipt(17)),
     &       h4,         icb,        icc,        ipt,        iscm,
     &       is,         isf,        js,         jsf,        ks,
     &       ksf,        ls,         lsf,        a(ipt(9)),  a(ipt(10)),
     &       a(ipt(11)), a(ipt(12)), a(ipt(13)), a(ipt(14)), a(ipt(15)),
     &       a(ipt(16)), lmnp1,      lmnv,       mcons,      nc,
     &       ncon,       nfct,       nrcr,       nt,         ntl,
     &       ntu,        x,          y,          z,          zet )
  828 itst = itst1
  829 continue
      go to 899
c     # ieqsf3=8, ieqs3=3
  830 itst = 0
      igwu = ngw(icc)
      do 839 iprm = 1, 4
      inx = iperm(1,ip83(iprm))
      jnx = iperm(2,ip83(iprm))
      knx = iperm(3,ip83(iprm))
      lnx = iperm(4,ip83(iprm))
      lc1 = icb(lnx,1,icc)
      jc1 = icb(jnx,1,icc)
      itst1 = jc1 + lc1 * icu
      if (itst1 .le. itst) go to 839
      nwt = 1
      do 836 igw = 2, igwu
        if(icb(jnx,igw,icc)+icb(lnx,igw,icc)*icu.lt.itst1) go to 838
        nwt = nwt + 1
  836 continue
            call sf3eq8( a,          cx,         eta,        a(ipt(17)),
     &       h4,         icb,        icc,        ipt,        iscm,
     &       is,         isf,        js,         jsf,        ks,
     &       ksf,        ls,         lsf,        a(ipt(9)),  a(ipt(10)),
     &       a(ipt(11)), a(ipt(12)), a(ipt(13)), a(ipt(14)), a(ipt(15)),
     &       a(ipt(16)), lmnp1,      lmnv,       mcons,      nc,
     &       ncon,       nfct,       nrcr,       nt,         ntl,
     &       ntu,        x,          y,          z,          zet )
  838 itst = itst1
  839 continue
      go to 899
c     # ieqsf3=8, ieqs3=4
  840 itst = 0
      igwu = ngw(icc)
      do 849 iprm = 1, 2
      inx = iperm(1,iprm)
      jnx = iperm(2,iprm)
      jc1 = icb(jnx,1,icc)
      if (jc1 .eq. itst) go to 849
      nwt = 1
      if (igwu .eq. 1) go to 847
      if(icb(jnx,2,icc).lt.jc1) go to 849
      nwt = 2
  847       call sf3eq8( a,          cx,         eta,        a(ipt(17)),
     &       h4,         icb,        icc,        ipt,        iscm,
     &       is,         isf,        js,         jsf,        ks,
     &       ksf,        ls,         lsf,        a(ipt(9)),  a(ipt(10)),
     &       a(ipt(11)), a(ipt(12)), a(ipt(13)), a(ipt(14)), a(ipt(15)),
     &       a(ipt(16)), lmnp1,      lmnv,       mcons,      nc,
     &       ncon,       nfct,       nrcr,       nt,         ntl,
     &       ntu,        x,          y,          z,          zet )
      itst = jc1
  849 continue
      go to 899
c     # ieqsf3=8, ieqs3=5
  850 itst = 0
      igwu = ngw(icc)
      do 859 iprm = 1, 6
      jnx = iperm(2,ip85(iprm))
      knx = iperm(3,ip85(iprm))
      lnx = iperm(4,ip85(iprm))
      lc1 = icb(lnx,1,icc)
      kc1 = icb(knx,1,icc)
      itst1 = kc1 + lc1 * jcu
      if (itst1 .le. itst) go to 859
      nwt = 1
      if (igwu .eq. 1) go to 857
      do 856 igw = 2, igwu
        if(icb(knx,igw,icc)+icb(lnx,igw,icc)*jcu.lt.itst1) go to 858
        nwt = nwt + 1
  856 continue
  857       call sf3eq8( a,          cx,         eta,        a(ipt(17)),
     &       h4,         icb,        icc,        ipt,        iscm,
     &       is,         isf,        js,         jsf,        ks,
     &       ksf,        ls,         lsf,        a(ipt(9)),  a(ipt(10)),
     &       a(ipt(11)), a(ipt(12)), a(ipt(13)), a(ipt(14)), a(ipt(15)),
     &       a(ipt(16)), lmnp1,      lmnv,       mcons,      nc,
     &       ncon,       nfct,       nrcr,       nt,         ntl,
     &       ntu,        x,          y,          z,          zet )
  858 itst = itst1
  859 continue
      go to 899
c     # ieqsf3=8, ieqs3=6; use first part of ip85 in place of ip86
  860 itst = 0
      igwu = ngw(icc)
      do 869 iprm = 1, 2
      jnx = iperm(2,ip85(iprm))
      knx = iperm(3,ip85(iprm))
      kc1 = icb(knx,1,icc)
      if (kc1 .eq. itst) go to 869
      nwt = 1
      if (igwu .eq. 1) go to 867
      if(icb(knx,2,icc).lt.kc1) go to 869
      nwt = 2
  867       call sf3eq8( a,          cx,         eta,        a(ipt(17)),
     &       h4,         icb,        icc,        ipt,        iscm,
     &       is,         isf,        js,         jsf,        ks,
     &       ksf,        ls,         lsf,        a(ipt(9)),  a(ipt(10)),
     &       a(ipt(11)), a(ipt(12)), a(ipt(13)), a(ipt(14)), a(ipt(15)),
     &       a(ipt(16)), lmnp1,      lmnv,       mcons,      nc,
     &       ncon,       nfct,       nrcr,       nt,         ntl,
     &       ntu,        x,          y,          z,          zet )
      itst = kc1
  869 continue
      go to 899
c     # ieqsf3=8, ieqs3=7; use first part of ip21 in place of ip87
  870 itst = 0
      igwu = ngw(icc)
      do 879 iprm = 1, 2
      knx = iperm(3,ip21(iprm))
      lnx = iperm(4,ip21(iprm))
      lc1 = icb (lnx,1,icc)
      if (lc1 .eq. itst) go to 879
      nwt = 1
      if (igwu .eq. 1) go to 877
      if(icb(lnx,2,icc).lt.lc1) go to 879
      nwt = 2
  877       call sf3eq8( a,          cx,         eta,        a(ipt(17)),
     &       h4,         icb,        icc,        ipt,        iscm,
     &       is,         isf,        js,         jsf,        ks,
     &       ksf,        ls,         lsf,        a(ipt(9)),  a(ipt(10)),
     &       a(ipt(11)), a(ipt(12)), a(ipt(13)), a(ipt(14)), a(ipt(15)),
     &       a(ipt(16)), lmnp1,      lmnv,       mcons,      nc,
     &       ncon,       nfct,       nrcr,       nt,         ntl,
     &       ntu,        x,          y,          z,          zet )
      itst = lc1
  879 continue
      go to 899
c     # ieqsf3=8, ieqs3=8
  880       call sf3eq8( a,          cx,         eta,        a(ipt(17)),
     &       h4,         icb,        icc,        ipt,        iscm,
     &       is,         isf,        js,         jsf,        ks,
     &       ksf,        ls,         lsf,        a(ipt(9)),  a(ipt(10)),
     &       a(ipt(11)), a(ipt(12)), a(ipt(13)), a(ipt(14)), a(ipt(15)),
     &       a(ipt(16)), lmnp1,      lmnv,       mcons,      nc,
     &       ncon,       nfct,       nrcr,       nt,         ntl,
     &       ntu,        x,          y,          z,          zet )
  899 continue
  900 continue
c
c     # write out the integrals for this component of the shell-triple.
            call wtint2( h4,     il,     iprst,  iscm,   buffer, values,
     &           labels, ibitv )
c
c     # reclaim kblu workspace, and pop back to the intitial level.
c     mblu = mblu + kblu
      call h2opop
c
  910 continue
c     # the complete set of integrals has been calculated for the
c     # construction of a disjoint subset of p and k elements.
c     # set the pk flag.
      if ( ibuf .ne. 0 ) then
        npkflg = npkflg - ibitv(ibuf) + 1
        ibitv(ibuf) = +1
      endif
  920 continue
  930 continue
  960 continue
  970 continue
  980 continue
  990 continue
c
c     # write out the last 2-e integral record.
c
      call wtlab2( nomore, buffer, values, labels, ibitv )
c
      call siftyp( itypea, itypeb, chrtyp )
      write( nlist, 6010 ) numout, chrtyp, npkflg, nrec
c
c     # get the high-water mark for a(*) workspace usage.
      call h2omtr( mxblu )
      write( nlist, '(a,i9)' ) ' twoint: maximum mblu needed = ',mxblu
c
      return
6010  format(/' twoint:',i12,1x,a,' integrals and',i9,
     & ' pk flags were written in',i6,' records.'/)
      end
