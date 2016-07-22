*deck wtint2
      subroutine wtint2( h4,      il,     iprst,  iscm,
     & buffer, values, labels, ibitv )
c
c  assign orbital labels and write records of 2-e integrals.
c
c  01-dec-90 SIFS version. ls(2,*) used as a 2d array. -rls
c
      implicit logical(a-z)
c
      integer    ilfact
      parameter( ilfact=1024 )
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
      logical       eprsf,    esfi,    esfk
      common /lgcl/ eprsf(3), esfi(3), esfk(3)
c
      integer
     & itl,    itu,    jtl,    jtu,    ktl,    ktu,    ltl,    ltu,
     & jcu,    kcu,    lcu,    inx,    jnx,    knx,    lnx,    nwt,
     & nwt1,           ijsf,           klsf,           icxs,
     & kcxs,           npri,           nprk,           iesfbx, kesfbx,
     & ircrux, jrcrux, krcrux, lrcrux
      common /ntgr/
     & itl,    itu,    jtl,    jtu,    ktl,    ktu,    ltl,    ltu,
     & jcu,    kcu,    lcu,    inx,    jnx,    knx,    lnx,    nwt,
     & nwt1(3),        ijsf(3),        klsf(3),        icxs(2,3),
     & kcxs(2,3),      npri(2,3),      nprk(2,3),      iesfbx, kesfbx,
     & ircrux, jrcrux, krcrux, lrcrux
c
      integer       npriri,        nprirk
      common /ikpr/ npriri(2,3,8), nprirk(2,3,8)
c
      real*8
     & xij,    xijm,   yij,    yijm,   zij,    zijm,   xkl,    xklm,
     & ykl,    yklm,   zkl,    zklm
      integer
     & icons,  ircru,  jcons,  jrcru,  kcons,  krcru,  lcons,  lrcru
      logical
     & esfc,   esfcij, esfckl
      common /misc/
     & xij,    xijm,   yij,    yijm,   zij,    zijm,   xkl,    xklm,
     & ykl,    yklm,   zkl,    zklm,
     & icons,  ircru,  jcons,  jrcru,  kcons,  krcru,  lcons,  lrcru,
     & esfc,   esfcij, esfckl
c
      integer    nipv
      parameter( nipv=4 )
c
c     # dummy:
      integer iscm
      integer il(*), iprst(*), labels(nipv,*), ibitv(*)
      real*8 h4(*), buffer(*), values(*)
c     # ibm+convex f77 bug; actual:
c     # integer labels(nipv,n2max), ibitv( ((n2max+63)/64)*64 )
c     # real*8 buffer(l2rec), values(n2max)
c
c     # local:
      integer ibl, ipr, kprib, ircr, jrcr, iesfb, iprkb, kpr, krcr,
     & lrcr, kesfb, ist, npairi, npairk, kpri, ipair, kpair
      integer    msame
      parameter( msame=0 )
c
      ibl = 0
      ipr = iprst(ijsf(iscm))
      if ( eprsf(iscm) ) then
        kprib = ipr
      else
        kprib = iprst(klsf(iscm))
      endif
      do 176 ircr = 1, ircru
        if ( esfi(iscm) ) jrcru = ircr
        do 174 jrcr = 1, jrcru
          if ( esfi(iscm) .and. (ircr .eq. jrcr) ) then
            iesfb = 1
          else
            iesfb = 2
          endif
          iprkb = ipr
          kpr = kprib
          if ( eprsf(iscm) ) krcru = ircr
          do 172 krcr = 1, krcru
            if ( eprsf(iscm) ) then
              if ( ircr .eq. krcr ) then
                lrcru = jrcr
              elseif ( esfk(iscm) ) then
                lrcru = krcr
              else
                lrcru = jrcru
              endif
            else
              if ( esfk(iscm) ) lrcru = krcr
            endif
            do 170 lrcr = 1,lrcru
              if ( esfk(iscm) .and. (krcr .eq. lrcr) ) then
                kesfb = 1
              else
                kesfb = 2
              endif
              ipr = iprkb
              do 160 ist = 1, nst
                npairi = npriri(iesfb,iscm,ist)
                npairk = nprirk(kesfb,iscm,ist)
                if ( npairi .eq. 0 ) then
                  kpr = kpr + npairk
                  go to 160
                elseif ( npairk .eq. 0 ) then
                  ipr = ipr + npairi
                  go to 160
                endif
                kpri = kpr
                do 150 ipair = 1, npairi
                  ipr = ipr + 1
                  if ( eprsf(iscm) .and. (ircr .eq. krcr)
     &             .and. (jrcr .eq. lrcr) ) npairk = ipair
                  kpr = kpri
                  do 140 kpair = 1, npairk
                    kpr = kpr + 1
                    ibl = ibl + 1
                    if ( abs(h4(ibl)) .gt. cutoff ) then
                      if ( ibuf .eq. n2max ) then
                        call wtlab2( msame, buffer, values, labels,
     &                   ibitv )
                      endif
                      ibuf           = ibuf + 1
                      labels(1,ibuf) = il(ipr) / ilfact
                      labels(2,ibuf) = mod( il(ipr), ilfact )
                      labels(3,ibuf) = il(kpr) / ilfact
                      labels(4,ibuf) = mod( il(kpr), ilfact )
                      values(ibuf)   = h4(ibl)
                    endif
140               continue
150             continue
160           continue
170         continue
172       continue
174     continue
176   continue
c
      return
      end
