*deck socfpd
      subroutine socfpd( c,           cx,         icxast, icxst,
     &   icxsv1, icxsv2, idp,         il,         iprst,  la,
     &   lb,     maords, mau,         mcons,      mgcs,   mics,
     &   mjcs,   nblpr,  nc,          nd,         nf,     nir,
     &   npair,  nprir,  nrcr,        nsopr,      nt,     ntl,
     &   ntu  )
c
c  01-dec-90 unnecessary statement labels removed to allow
c            optimization. il(*) encoded with arithmetic. -rls
c
      implicit logical(a-z)
c
c     # il(*) packing factor. see also oneint() and wtint2().
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
      integer
     & icxast(*),        icxst(2,*),   icxsv1(*),    icxsv2(mgcsu,*),
     & idp(mstu,mstu,*), il(nnbft),    iprst(*),     la(mru,*),
     & lb(*),            maords(*),    mau(*),       mcons(*),
     & mgcs(*),          mics(*),      mjcs(*),      nblpr(*),
     & nc(*),            nd(*),        nf(*),        nir(*),
     & npair(2,*),       nrcr(*),      nsopr(*),     nprir(2,mstu,*),
     & nt(*),            ntl(*),       ntu(*)
      real*8 c(mctu,mcsu,*), cx(*)
c
c     # local:
      integer mcxu1, icx, ipair, ijsf, isf, isfr, igcs, jgcs, is,
     & ifu, icu, isfis, isfris, jsf, jsfr, js, jsfjs, jsfrjs, jfu,
     & jcu, if, isfrif, iaords, iru, ircru, itl, itu, jf, jaords,
     & jtl, jsfrjf, jtu, jru, ist, jrcru, ircr, isfrib, jrcr,
     & jsfrjb, iesfb, npr, ipr, ics, ir, lai, iau, jcs, jr, laj,
     & isv1, icxu, icxa, isv2, ict, ic, ictic, jct, jc, jctjc, it,
     & jt, ia, icxl, mnsf, npru
      logical ea, easfb, es, esf, esfb, esfbc, msfbct
c
      real*8     a0
      parameter (a0=0.0d0)
c
c     # bummer error types.
      integer    wrnerr,  nfterr,  faterr
      parameter (wrnerr=0,nfterr=1,faterr=2)
c
c     # check the il(*) packing factor.
      if ( nbft .ge. ilfact ) then
        call bummer('socfpd: larger ilfact required, nbft=',
     &   nbft, faterr )
      endif
c
      ea = inam(nu).eq.5
c
c     # mcxu1 is the original mcxu offset.
      mcxu1 = mcxu
      icx   = 0
      ipair = 0
c
c     # ijsf = one dimensionalization of is, js, if, jf
c     # isfr = one dimensionalization of is, if, ir
      ijsf = 0
      isf  = 0
      isfr = 0
      do 12 igcs = 1, ngcs
        icxsv1(igcs) = 0
        do 10 jgcs = 1, ngcs
          icxsv2(jgcs,igcs) = 0
10      continue
12    continue
      do 340 is = 1, ns
        ifu = nf(is)
        if(ifu.eq.0) go to 340
        icu = nc(is)
        isfis = isf
        isfris = isfr
        jsf = 0
        jsfr = 0
        do 330 js = 1,is
          jsfjs = jsf
          jsfrjs = jsfr
          es = is.eq.js
          if(.not.es) then
            jfu = nf(js)
            if(jfu.eq.0) go to 330
            jcu = nc(js)
          endif
          isf = isfis
          isfr = isfris
          do 324 if = 1,ifu
            isf = isf+1
            isfrif = isfr
            igcs = mgcs(isf)
            iaords = maords(igcs)
            iru = nir(iaords)
            ircru = nrcr(mcons(isf))
            itl = ntl(isf)
            itu = ntu(isf)
            jsf = jsfjs
            jsfr = jsfrjs
            if(es) jfu = if
            do 322 jf = 1,jfu
              jsf = jsf+1
              jgcs = mgcs(jsf)
              jaords = maords(jgcs)
              jtl = ntl(jsf)
              jsfrjf = jsfr
              ijsf = ijsf+1
              esf = isf.eq.jsf
              if(esf) then
                if(ircru.ge.2) then
                  jtu = ntu(jsf)
                  jru = nir(jaords)
                else
                  npair(2,ijsf) = 0
                  icxst(2,ijsf) = 0
                  do 20 ist = 1,nst
                    nprir(2,ist,ijsf) = 0
20                continue
                endif
              else
                jrcru = nrcr(mcons(jsf))
                jtu   = ntu(jsf)
                jru   = nir(jaords)
              endif
              iprst(ijsf) = ipair
              isfr = isfrif
              do 320 ircr = 1,ircru
                isfrib = isfr
                jsfr = jsfrjf
                if(esf) jrcru = ircr
                do 310 jrcr = 1,jrcru
                  jsfrjb = jsfr
                  esfb = esf.and.ircr.eq.jrcr
                  easfb = ea.and.esfb
                  if(esfb) then
                    iesfb = 1
                    npru = (iru * (iru + 1)) / 2
                  else
                    iesfb = 2
                    npru = iru * jru
                  endif
c
                  if ( npru .gt. mnru ) then
                    call bummer('change mnrup (one place) to ',
     &               npru,faterr)
                  endif
c
                  npr = 0
                  do 75 ist = 1, nst
                    ipr = npr
                    ics = 0
                    isfr = isfrib
                    do 70 ir = 1, iru
                      isfr = isfr + 1
                      lai = la(ir,iaords)
                      iau = nd(lai)
                      if(esfb) jru = ir
                      jsfr = jsfrjb
                      jcs = 0
                      do 65 jr = 1, jru
                        jsfr = jsfr + 1
                        laj = la(jr,jaords)
                        if ( idp(lai,laj,ist) .ne. 0 ) then
                          ipair = ipair + 1
                          il(ipair) = (nsopr(lai) + lb(isfr))
     &                     * ilfact + (nsopr(laj) + lb(jsfr))
c
                          npr = npr + 1
                          mau(npr)  = iau
                          mics(npr) = ics
                          mjcs(npr) = jcs
                        endif
                        jcs = jcs + nd(laj)
65                    continue
                      ics = ics + iau
70                  continue
                    nprir(iesfb,ist,ijsf) = npr - ipr
75                continue
                  npair(iesfb,ijsf) = npr
c
                  if ( npr .eq. 0 ) go to 310
c
                  if ( esfb ) then
                    isv1 = icxsv1(igcs)
                    if ( isv1 .ne. 0 ) then
                      icxst(1,ijsf) = icxst(1,isv1)
                      if(ea) icxast(ijsf) = icxast(isv1)
                      go to 310
                    endif
                    icxsv1(igcs) = ijsf
                    icxst(1,ijsf) = icx
                    icxu = icx+(npr*ipq(icu*nt(isf)+1))
                    if ( ea ) then
                      mcxu = mcxu-(npr*ipq(icu*nt(isf)+1))
                      icxa = mcxu
                      icxast(ijsf) = icxa
                    endif
                  else
                    isv2 = icxsv2(jgcs,igcs)
                    if ( isv2 .ne. 0 ) then
                      icxst(2,ijsf) = icxst(2,isv2)
                      go to 310
                    endif
                    icxsv2(jgcs,igcs) = ijsf
                    icxst(2,ijsf) = icx
                    icxu = icx+npr*icu*nt(isf)*jcu*nt(jsf)
                  endif
                  if ( icxu .gt. mcxu ) then
                    call bummer('socfpd: (icxu-mcxu)=',
     &               (icxu-mcxu), faterr )
                  endif
c
c                 # set the high-water mark.
c                 # account for both low-address allocations,
c                 # from (1 : icxu), and high-address allocations,
c                 # from (mcxu+1 : mcxu1).
                  call h2oset( (icxu + mcxu1 - mcxu) )
c
c                 # compute new cx block
c
                  ict = 0
                  do 298 ic = 1, icu
                    ictic = ict
                    if(esfb) jcu = ic
                    jct = 0
                    do 296 jc = 1, jcu
                      jctjc = jct
                      esfbc = esfb .and. ic .eq. jc
                      ict = ictic
                      do 294 it = itl, itu
                        ict = ict + 1
                        if(esfbc) jtu = it
                        jct = jctjc
                        do 292 jt = jtl, jtu
                          jct = jct + 1
                          msfbct =  .not. esfb .or.
     &                     (ic.eq.jc .and. it.eq.jt)
                          do 290 ipr = 1, npr
                            icx = icx + 1
                            cx(icx) = a0
                            if(easfb) then
                              icxa = icxa + 1
                              cx(icxa) = a0
                            endif
                            iau = mau(ipr)
                            ics = mics(ipr)
                            jcs = mjcs(ipr)
                            do 280 ia = 1, iau
                              ics = ics + 1
                              jcs = jcs + 1
                              cx(icx) = cx(icx)
     &                         + (c(ict,ics,igcs) * c(jct,jcs,jgcs))
                              if(easfb) cx(icxa) = cx(icxa)
     &                         + (c(ict,ics,igcs) * c(jct,jcs,jgcs))
                              if(msfbct) go to 280
                              cx(icx) = cx(icx)
     &                         + (c(jct,ics,igcs) * c(ict,jcs,jgcs))
                              if(easfb) cx(icxa) = cx(icxa)
     &                         - (c(jct,ics,igcs) * c(ict,jcs,jgcs))
280                         continue
290                       continue
292                     continue
294                   continue
296                 continue
298               continue
310             continue
320           continue
322         continue
324       continue
330     continue
340   continue
c
      if ( ea ) then
c
c       # move antisymmetric coefficient products down.
c
c       #       offsets: 0           mcxu1  mblu
c       #                |            |     |
c       # before: a(*)-->.............c(*)
c       #
c       #                0      mcxu mcxu1  mblu
c       #                |       |    |     |
c       # now:    a(*)-->cx(*)...x(*),c(*)
c       #
c       #                0   mcxu2 mcxu  (mblu+mcxu)
c       #                |     |   |        |
c       # after:  a(*)-->cx(*),x(*),........
c
c       # on return, mcxu2 is the offset for the s.o. coefficients.
        mcxu2 = icx
c
        icxl  = icx + 1
        icxu  = mcxu - icx
        mcxu  = mcxu1 - icxu
        do 370 icx = icxl, mcxu
          cx(icx) = cx(icx+icxu)
370     continue
c       # make pointers relative to mcxu2 instead of 0.
        do 380 mnsf = 1, ijsf
          icxast(mnsf) = icxast(mnsf) - icxu
380     continue
c       # on return, mcxu and mblu divide the workspace as:
c       # a(*)-->[cx//x](1:mcxu),w(1:mblu)
        mblu = mblu - mcxu
        write (nlist,6010) mcxu, mcxu2, mblu
      else
c       # on return, mcxu and mblu divide the workspace as:
c       # a(*)-->cx(1:mcxu),w(1:mblu)
        mcxu = icx
        mblu = mblu - mcxu
        write (nlist,6020) mcxu, mblu
      endif
c
      return
6010  format(/' socfpd: mcxu=',i9,' mcxu2=',i9,' left=',i9)
6020  format(/' socfpd: mcxu=',i9,' left=',i9)
      end
