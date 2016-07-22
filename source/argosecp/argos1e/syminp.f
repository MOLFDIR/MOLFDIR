*deck syminp
      subroutine syminp( a,      c,          ccr,    chg,    eta,
     &   ica,    idp,    ipt,    ityp,       la,     lb,     lcr,
     &   lls,    lmnp1,  lmnv,   lmn1u,      lproju, maords, mcons,
     &   mcrs,   mgcs,   mnl,    ms,         mtype,  nblpr,  nc,
     &   ncon,   ncr,    ncru,   nct,        nd,     nf,     nir,
     &   nkcrl,  nkcru,  nklsl,  nklsu,      nrcr,   nso,    nsopr,
     &   nt,     ntl,    ntu,    x,          y,      z,      zcr,
     &   zet,    lcore,  ifirst, mem1 )
c
c  read and verify the user input: symmetry info, basis sets,
c  geometry, etc.
c
c  11-sep-91 parallel code added. -rls/rjh
c  12-jun-91 l2opxv() call added. -rls
c  01-mar-91 aoint2,fsplit,l1rec,l2rec input order changed. -rls/rmp
c
      implicit logical(a-z)
c
      integer   nunits
      parameter(nunits=4)
      integer        iunits
      common /units/ iunits(nunits)
      integer                  nlist
      equivalence ( iunits(1), nlist )
      integer                  aoints
      equivalence ( iunits(2), aoints )
      integer                  aoint2
      equivalence ( iunits(3), aoint2 )
      integer                  ninput
      equivalence ( iunits(4), ninput )
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
      integer         only1e
      common /conly1/ only1e
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
      integer        ipq
      common /dim21/ ipq(256)
c
c     # dummy:
      integer lcore, ifirst, mem1
      integer
     & ica(mcu,msu,*), idp(mstu,mstu,*), ipt(*),     la(mru,*),
     & lb(*),          lcr(*),           lls(*),     lmnp1(*),
     & lmnv(3,*),      lmn1u,            lproju,     maords(*),
     & mcons(*),       mcrs(*),          mgcs(*),    mnl(*),
     & ms(*),          nblpr(*),         nc(*),      ncon(*),
     & ncr(*),         ncru,             nct(*),     nd(*),
     & nf(*),          nir(*),           nkcrl(6,*), nkcru(6,*),
     & nklsl(4,*),     nklsu(4,*),       nrcr(*),    nso(*),
     & nsopr(*),       nt(*),            ntl(*),     ntu(*)
      real*8
     & a(*),               c(mctu,mcsu,*), ccr(*),   chg(*),
     & eta(mrcru,mconu,*), x(mcu,*),       y(mcu,*), z(mcu,*),
     & zcr(*),             zet(mconu,*)
      character*3 ityp(*), mtype(*)
c
c     # local:
      integer i, ntitle, itol, icut, inrm, ncrs, fsplit, l1rec, l2rec,
     & ngen, ncons, naords, ist, ndpt, idpt, jst, kst, iprd, ierr,
     & iaords, iru, ir, igcs, icsu, ictu, ics, ict, ic, icons, iconu,
     & ircru, icon, ircr, kcru, icrs, lcru, llsu, lp1, nbfcr, kcrl,
     & kcr, l, ngenp1, isf, isfr, is, icu, ig, if, ilmnp1, lai,
     & nsf, itl, itu, jcts, it, jct, jt, ixyz, i2, i3, jconu, jsfr,
     & jcon, js, jcu, jc, ngp1, icsr, ncol, iso, icol, ibvtyp, l1recx,
     & l2recx, n1max, n2max, iau, ia, leig, lmn, lmnvmx, ircrl, ircrh
      integer ixyzir(3), lbla(16), lblso(16), iprdsm(3,7), ndptsm(0:9)
      real*8 twoc, prtint, sum, t, repnuc, chgprd, c1, ccc, eval
c
      character*80 title(5)
      character*3 ltyp(16)
      character*1 ibl, ipc, isk, lblsh(21)
c
      real*8            a0,       a1s2,       a1,       a2,
     & aln10,           a4,       a10
      parameter(        a0=0.0d0, a1s2=0.5d0, a1=1.0d0, a2=2.0d0,
     & aln10=2.30258d0, a4=4.0d0, a10=10.0d0 )
c
c     # lrcmxp is the program default 1-e and 2-e record length.
c     # there is also an SIFS default, which may be different.
      integer    lrcmxp
      parameter( lrcmxp=4096 )
c
c     # bummer error types.
      integer    wrnerr,  nfterr,  faterr
      parameter (wrnerr=0,nfterr=1,faterr=2)
c
c     # for workspace allocation.
      integer  atebyt, forbyt
      external atebyt, forbyt
c
      integer  iodfac, nodeid
      external iodfac, nodeid
c
      data ibl, isk /' ','0'/, lblsh /'s','p','d','f','g','h','i','k',
     &  'l','m','n','o','q','r','t','u','v','w','x','y','z'/
c
c     # nst=4 and nst=8 product triples.
      data iprdsm / 2,3,4, 2,5,6, 2,7,8, 3,5,7, 3,6,8, 4,5,8, 4,6,7 /
c
c     # the number of product triples for each nst.
      data (ndptsm(i),i=0,9) /
     & -1, -1, 0, -1, 1, -1, -1, -1, 7, -1 /
c
c     # assign the default unit numbers and filenames.
      ninput = 5
      fnames(4)='argosin'
      nlist  = 6
      fnames(1)='argosls'
      aoints = 4
      fnames(2)='aoints'
      aoint2 = 8
      fnames(3)='aoints2'
c
c     # translate filenames if necessary.
      call trnfln( nunits, fnames )
c
*mdc*if parallel
*c     # Everyone has their own aoints_xxx, aoints2_xxx and argosls_xxx.
*c     # if not node 0 also rename argosin to avoid clashes.
*      call pfname( 3, fnames )
*      if ( nodeid() .ne. 0 ) call pfname(1,fnames(4))
*c     # copy the input file from node=0 to the other nodes.
*      call pfcopy( 999, 0, fnames(4) )
*c-debug      call flush(6)
*mdc*endif
c
c     # open the input file.
c
      open(unit=ninput,status='old',file=fnames(4))
c
c     # open the output listing file.
c
*mdc*if ibm
*c  use preconnected unit 6
*mdc*else
      open(unit=nlist,status='unknown',file=fnames(1))
*mdc*endif
      call ibummr(nlist)
c
c     # echo the input cards.
      write(nlist,'(a)')'echo of the argos input file:'
      call echoin( ninput, nlist, ierr )
      if ( ierr .ne. 0 ) then
        call bummer('syminp: from echoin, ierr=', ierr, faterr )
      endif
c
      rewind ninput
c
c     # read the first title here.  remaining titles are read later.
c     # *** the next time the input to this program is changed, all
c     #     of the titles should be read together. ***
c
      read (ninput,'(a)') title(1)
c
      ntitle = 1
c
c     # intitalize some input variables.
c     # [01-dec-90 aoint2, fsplit, l1rec, l2rec added. -rls]
      itol   =  0
      icut   =  0
      only1e =  0
      inrm   =  0
      ncrs   =  0
      l1rec  = -1
      l2rec  = -1
      fsplit =  2
c
      read (ninput,*)  ngen,   ns,     naords, ncons,  ngcs,   itol,
     & icut,   aoints, only1e, inrm,   ncrs,   l1rec,  l2rec,  aoint2,
     & fsplit
c
      if ( ns .gt. msu ) then
        call bummer('change msup (two places) to ',ns,faterr)
      elseif ( naords .gt. kaords ) then
        call bummer('change kaordp (one place) to ',naords,faterr)
      elseif ( ncons .gt. mconsu ) then
        call bummer('change mconsp (two places) to ',ncons,faterr)
      elseif ( ngcs .gt. mgcsu ) then
        call bummer('change mgcsup (one place) to ',ngcs,faterr)
      endif
c
c     # reset aoints, aoint2, only1e, and fsplit if necessary.
      if ( aoints .le. 0 ) aoints = 4
      if ( aoint2 .le. 0 ) aoint2 = 8
      if ( fsplit .ne. 1 ) fsplit = 2
*mdc*if parallel
*      if ( fsplit .ne. 2 ) then
*        write(nlist, *) 'Need fsplit = 2 in parallel...resetting'
*        fsplit = 2
*      endif
*mdc*endif
      if ( only1e .ne. 0 ) then
c       # set only1e=0 for off, and only1e=1 for on.
        only1e = 1
        if ( fsplit .eq. 1 ) then
c         # this is inconsistent.  however, instead of treating
c         # this as an error, reset fsplit and continue.  the only
c         # harm is that the user may have to recompute the 1-e
c         # integrals later if this was not the right choice.
          write(nlist,*)'only1e is inconsistent with fsplit.'
     &     //' resetting fsplit=2 and continuing...'
          fsplit = 2
        endif
      endif
c
c     # read number of irreps, degeneracy, and character labels.
c
c     # *** the next time the input to this program is changed,
c     #     all of the items in this list should be read with
c     #     list-directed i/o.  ***
c
      read (ninput,'(i3,12(i3,a3))') nst,(nd(ist),ityp(ist),ist=1,nst)
c
      if ( nst .gt. mstu ) then
        call bummer('change mstup (two places) to ',nst,faterr)
      endif
c
      call izero( nst,           nso, 1 )
      call izero( mstu*mstu*nst, idp, 1 )
c
c     # assign the totally symmetric irrep binary products.
      do 104 ist = 1,nst
        idp(ist,ist,1) = 1
        idp(ist,1,ist) = 1
        idp(1,ist,ist) = 1
  104 continue
c
c     # read the number of product triples.
c
      ndpt = -1
      read(ninput,*) ndpt
c
      if ( ndpt .ge. 0 ) then
c
c       # read in the totally symmetric irrep product
c       # triples from the input file.
c
        do 108 idpt = 1, ndpt
c
          read (ninput,*) ist, jst, kst
c
          idp(ist,jst,kst) = 1
          idp(ist,kst,jst) = 1
          idp(jst,ist,kst) = 1
          idp(jst,kst,ist) = 1
          idp(kst,ist,jst) = 1
          idp(kst,jst,ist) = 1
  108   continue
      else
c
c       # assign default values based on nst.
c       # 04-dec-90 ndp<0 option added. -rls
c
        ndpt = ndptsm( min( max( 0, nst), 9 ) )
        if ( ndpt .lt. 0 ) then
          call bummer('syminp: unknown ndpt defaults for nst=',
     &     nst, faterr )
        else
          do 112 iprd = 1, ndpt
            ist = iprdsm(1,iprd)
            jst = iprdsm(2,iprd)
            kst = iprdsm(3,iprd)
c
            idp(ist,jst,kst) = 1
            idp(ist,kst,jst) = 1
            idp(jst,ist,kst) = 1
            idp(jst,kst,ist) = 1
            idp(kst,ist,jst) = 1
            idp(kst,jst,ist) = 1
  112     continue
        endif
      endif
c
      do 116 iaords = 1, naords
c
        read (ninput,*) iru, (la(ir,iaords), ir=1,iru)
c
        if ( iru .gt. mru ) then
          call bummer('change mrup (one place) to ',iru,faterr)
        endif
        nir(iaords) = iru
  116 continue
c
      do 128 igcs = 1,ngcs
c
        read (ninput,*) icsu, ictu, maords(igcs)
c
        if ( icsu .gt. mcsu ) then
          call bummer('change mcsup (one place) to ',icsu,faterr)
        elseif ( ictu .gt. mctu ) then
          call bummer('change mctup (one place) to ',ictu,faterr)
        endif
c
        do 124 ics = 1,icsu
c
          read (ninput,*) (c(ict,ics,igcs), ict=1,ictu)
c
          do 120 ict = 1,ictu
            c1 = c(ict,ics,igcs)
            ccc = abs(c1)
            if ( ccc .ne. a0 .and. ccc .ne. a1 ) then
               c(ict,ics,igcs) = sign(sqrt(ccc),c1)
            endif
  120     continue
  124   continue
c
  128 continue
c
c     # read in exponents and contraction coefficients.
c
      do 136 icons = 1,ncons
c
        read (ninput,*) iconu, lmnp1(icons), ircru
c
        if ( ircru .eq. 0 ) ircru = 1
        if ( ircru .gt. mrcru ) then
          call bummer('change mrcrup (one place) to ',ircru,faterr)
        elseif ( iconu .gt. mconu ) then
          call bummer('change mconup (one place) to ',iconu,faterr)
        endif
        nrcr(icons) = ircru
        ncon(icons) = iconu
c
        do 132 icon = 1,iconu
c
          read (ninput,*) zet(icon,icons), (eta(ircr,icon,icons),
     &     ircr = 1,ircru)
c
  132   continue
  136 continue
c
      if ( ncrs .eq. 0 ) go to 160
c
c     # read in expansions for core and spin-orbit potentials.
c
      ncru = -2
      lproju = 0
      kcru = 0
      do 156 icrs = 1,ncrs
c
        read (ninput,*) lcru, llsu
        lproju = max(lproju, lcru-1, llsu)
c
c       if ( lcru .gt. 5 ) then
c         call bummer('lcru too large ',lcru,faterr)
c       elseif ( llsu .gt. 4 ) then
c         call bummer('llsu too large ',llsu,faterr)
c       endif
        lcr(icrs) = lcru
        lls(icrs) = llsu
c
        do 144 lp1 = 1,(lcru+1)
c
          read (ninput,*) nbfcr
c
          kcrl = kcru+1
          kcru = kcru+nbfcr
          nkcrl(lp1,icrs) = kcrl
          nkcru(lp1,icrs) = kcru
          do 140 kcr = kcrl,kcru
c
            read (ninput,*) ncr(kcr), zcr(kcr), ccr(kcr)
            if ( ncr(kcr) .lt. 0 ) then
              call bummer('ncr(kcr) too small ',ncr(kcr),faterr)
            endif
            ncru = max(ncru,ncr(kcr))
c
  140     continue
  144   continue
c
        do 152 l = 1,llsu
c
          read (ninput,*) nbfcr
c
          kcrl = kcru+1
          kcru = kcru+nbfcr
          nklsl(l,icrs) = kcrl
          nklsu(l,icrs) = kcru
          do 148 kcr = kcrl,kcru
c
            read (ninput,*) ncr(kcr), zcr(kcr), ccr(kcr)
cmckim start
            ccr(kcr)=ccr(kcr)/dfloat(l)
cmckim end
            if ( ncr(kcr) .lt. -2 ) then
              call bummer('ncr(kcr) too small ',ncr(kcr),faterr)
            endif
            ncru = max(ncru,ncr(kcr))
c
  148     continue
  152   continue
  156 continue
      if ( kcru .gt. mcru ) then
        call bummer('change mcrup (one place) to ',kcru,faterr)
      endif
c
  160 continue
c
      ngenp1 = ngen + 1
      ng     = ngenp1
c
c     # read in atomic labels, charges, coordinates, basis sets,
c     # and core and spin-orbit potentials
c
      call izero(ns,mcrs,1)
      lmn1u = 0
      isf = 0
      isfr = 0
      do 184 is = 1,ns
c
c       # *** the next time the input to this program is changed,
c       #     all of the items in this list should be read with
c       #     list-directed i/o.  ***
c
        read (ninput,'(a3,2i3,f3.0)') mtype(is),nf(is),nc(is),chg(is)
c
        icu = nc(is)
        if ( icu .gt. mcu ) then
          call bummer('change mcup (one place) to ',icu,faterr)
        endif
        do 164 ic = 1,icu
c
          read (ninput,*) x(ic,is), y(ic,is), z(ic,is)
c
  164   continue
        if ( icu .gt. 1 ) then
          do 168 ig = 2, ng
c
            read (ninput,*) (ica(ic,is,ig), ic=1,icu)
c
  168     continue
        endif
c
        do 180 if = 1,nf(is)
          isf = isf+1
c
          read (ninput,*) icons, igcs
c
          mcons(isf) = icons
          iaords = maords(igcs)
          iru =  nir(iaords)
          mgcs(isf) = igcs
          ilmnp1 = lmnp1(icons)
          lmn1u = max(lmn1u,ilmnp1)
          nt(isf) = ipq(ilmnp1+1)
          ntu(isf) = (nt(isf)*(ilmnp1+2))/3
          ntl(isf) = ntu(isf)-nt(isf)+1
          nct(isf) = icu*nt(isf)
          ircru = nrcr(icons)
          do 176 ircr = 1,ircru
            do 172 ir = 1,iru
              isfr = isfr+1
              lai = la(ir,iaords)
              nso(lai) = nso(lai)+1
              lb(isfr) = nso(lai)
  172       continue
  176     continue
  180   continue
        if ( ncrs .gt. 0 ) then
          read(ninput,*) mcrs(is)
        endif
  184 continue
      lmnvmx = (lmn1u*(lmn1u + 1)*(lmn1u + 2))/6
      ipt(2) = ipt(1) + forbyt( 3*lmnvmx )
c     # ipt(*)-->1:lmnv(1:3,1:lmnvmx)
      call lmnvgn(lmn1u,lmnv)
c
c     # read up to three more titles.
c     # stop reading at eof or a blank line.
c     #      04-dec-90 extra titles added. -rls
c
      do 188 i = 2, 4
c
        read(ninput,'(a)',iostat=ierr) title(i)
c
        if ( ierr .eq. 0 ) then
          if ( title(i) .ne. ' ') then
            ntitle = i
          else
c           # logical eof exit is ok.
            go to 192
          endif
        elseif ( ierr .gt. 0 ) then
          call bummer('syminp: title(2:4) input error, ierr=',
     &     ierr,faterr)
        elseif ( ierr .lt. 0 ) then
c         # eof exit is ok.
          go to 192
        endif
  188 continue
c
c     # all done with the input file.
  192 continue
      close(unit=ninput)
c
c     # generate an additional title with a job and tdy stamp.
c
      ntitle = ntitle + 1
      title(ntitle)(1:40) = 'aoints SIFS file created by argos.'
      call siftdy( title(ntitle)(41:) )
c
      nsf = isf
      if ( nsf .gt. msfu ) then
        call bummer('change msfup (two places) to ',nsf,faterr)
      elseif ( isfr .gt. msfru ) then
        call bummer('change msfrup (one place) to ',isfr,faterr)
      endif
c
c     # nbft = total number of symmetry orbital basis functions.
c
      nbft = isfr
c
c     # determine the number of types of one-electron integrals.
c
c     # initialize nu for s(*),t(*),v(*).
      nu = 3
c
      if ( ncrs .gt. 0 ) then
        do 196 is = 1,ns
          if ( mcrs(is) .ne. 0 ) then
            if ( lcr(mcrs(is)) .gt. 0 ) then
              nu = 4
              inam(4) = 4
              go to 200
            endif
          endif
  196   continue
c
  200   continue
        do 204 is = 1, ns
          if ( mcrs(is) .ne. 0 ) then
            if ( lls(mcrs(is)) .gt. 0 ) then
              nu = nu + 1
              inam(nu) = 5
              go to 208
            endif
          endif
  204   continue
c
  208   continue
      endif
c
      if ( inrm .ne. 0 ) then
c
c       # normalization of symmetry orbitals.
c
        do 244 igcs = 1,ngcs
          isf = 0
          do 240 is = 1,ns
            icu = nc(is)
            do 236 if = 1,nf(is)
              isf = isf+1
              if ( igcs .ne. mgcs(isf) ) go to 236
              itl = ntl(isf)
              itu = ntu(isf)
              iaords = maords(igcs)
              iru = nir(iaords)
              ilmnp1 = lmnp1(mcons(isf))
              ics = 0
              do 232 ir = 1,iru
                iau = nd(la(ir,iaords))
                do 228 ia = 1,iau
                  ics = ics+1
                  prtint = a0
                  ict = 0
                  do 220 ic = 1,icu
                    jcts = ict
                    do 216 it = itl,itu
                      ict = ict+1
                      if ( it .gt. itl ) then
                        twoc = a2*c(ict,ics,igcs)
                        jct = jcts
                        do 212 jt = itl,it-1
                          jct = jct+1
                          prtint = prtint + twoc*c(jct,ics,igcs)
     &                     * (iodfac(lmnv(1,it),lmnv(1,jt),lmnv(2,it),
     &                               lmnv(2,jt),lmnv(3,it),lmnv(3,jt)))
  212                   continue
                      endif
                      prtint = prtint + c(ict,ics,igcs)**2
     &                 * (iodfac(lmnv(1,it),lmnv(1,it),lmnv(2,it),
     &                           lmnv(2,it),lmnv(3,it),lmnv(3,it)))
  216                 continue
  220             continue
                  if ( prtint .ne. a1 ) then
                    prtint = sqrt(prtint)
                    do 224 jct = 1,ict
                      c(jct,ics,igcs) = c(jct,ics,igcs)/prtint
  224               continue
                  endif
  228           continue
  232         continue
              go to 244
  236       continue
  240     continue
  244   continue
      endif
c
      call izero(3,ixyzir,1)
c
      if ( icut .le. 0 ) icut = 9
      cutoff = a1 / a10**icut
c
      if ( itol.le.0) itol = 20
      tol = aln10 * itol
c
c     # compute contracted orbital and lower-triangle-packed
c     # matrix symmetry offsets.
      nsopr(1) = 0
      nblpr(1) = 0
      do 288 ist = 2,nst
        nsopr(ist) = nsopr(ist-1) + nso(ist-1)
        nblpr(ist) = nblpr(ist-1) + ipq( nso(ist-1) + 1 )
  288 continue
c
c     # nnbft = total number of unique so function pairs.
c     # mpru = the number of elements in a diagonal,
c     #        symmetry-blocked, lower-triangle-packed matrix.  this
c     #        is also an upper bound to the size of any
c     #        symmetry-blocked array in which only unique
c     #        off-diagonal elements are stored and which is
c     #        associated with an operator that transforms as an
c     #        irreducible representation.
c
      nnbft = (nbft * (nbft + 1)) / 2
      mpru  = nblpr(nst) + ipq( nso(nst) + 1 )
c
c     # normalize the contraction coefficients.
c
      do 316 icons = 1, ncons
        iconu = ncon(icons)
        ircru = nrcr(icons)
        do 312 ircr =  1, ircru
          if ( iconu .eq. 1 ) then
            eta(1,1,icons) = a1
            go to 312
          endif
          prtint = eta(ircr,1,icons)**2
          do 304 icon = 2, iconu
            twoc  = a2 * eta(ircr,icon,icons)
            jconu = icon - 1
            do 300 jcon = 1, jconu
              t = (zet(icon,icons) + zet(jcon,icons)) * a1s2
              t = zet(icon,icons)/t * zet(jcon,icons)/t
              prtint = prtint + twoc * eta(ircr,jcon,icons)
     &         * sqrt(t**lmnp1(icons) * sqrt(t))
  300       continue
            prtint = prtint + eta(ircr,icon,icons)**2
  304     continue
          prtint = sqrt(prtint)
          do 308 icon = 1, iconu
            eta(ircr,icon,icons) = eta(ircr,icon,icons) / prtint
  308     continue
  312   continue
  316 continue
c
c     # calculate the nuclear repulsion energy.
c
      repnuc = a0
      do 332 is = 1, ns
        ic = nc(is)
        t = (ic) * chg(is)
c       # rearrangement theorem eliminates the ic loop in favor
c       # of the overall (ic) factor.
        do 328 js = 1, is
          chgprd = t * chg(js)
          if ( js .eq. is ) then
c           # repulsions within a set are double-counted, so
c           # compensate with a factor of 1/2.
            chgprd = a1s2 * chgprd
c           # don't include nc(is)=nc(js) self-repulsions.
            jcu = nc(js) - 1
          else
            jcu = nc(js)
          endif
          do 324 jc = 1, jcu
            repnuc = repnuc + chgprd / sqrt( (x(ic,is)-x(jc,js))**2
     1       + (y(ic,is)-y(jc,js))**2 + (z(ic,is)-z(jc,js))**2 )
  324     continue
  328   continue
  332 continue
c
      call gcentr( ica, nc )
c
      ngp1 = ng+1
      if ( ngp1 .gt. mgu ) then
        call bummer('change mgup (two places) to ',ngp1,faterr)
      endif
c
      write (nlist,600)
  600 format(30x,'program "argos" 4.1b2'/
     &  29x,'columbus program system'//
     &  13x,'this program computes integrals over symmetry orbitals'/
     &  17x,'of generally contracted gaussian atomic orbitals.'/
     &  25x,'programmed by russell m. pitzer'//
     &  29x,'version date: 01-sep-97'//' references:'//
     &  t5,'symmetry analysis (equal contributions):'/
     &  t5,'r. m. pitzer, j. chem. phys. 58, 3111 (1973).'//
     &  t5,'ao integral evaluation (hondo):'/
     &  t5,'m. dupuis, j. rys, and h. f. king, j. chem. phys. 65,',
     &     ' 111 (1976).'//
     &  t5,'general contraction of gaussian orbitals:'/
     &  t5,'r. c. raffenetti, j. chem. phys. 58, 4452 (1973).'//
     &  t5,'core potential ao integrals (meldps):'/
     &  t5,'l. e. mcmurchie and e. r. davidson, j. comput. phys.',
     &     ' 44, 289 (1981)'//
     &  t5,'spin-orbit and core potential integrals:'/
     &  t5,'r. m. pitzer and n. w. winter, int. j. quantum chem.',
     &     ' 40, 773 (1991)')
c
c     # print out the name and address of the local programmer
c     # responsible for maintaining this program.
c
      call who2c( 'ARGOS', nlist )
c
      write (nlist,604) lcore, mem1, ifirst
  604 format(/' workspace allocation parameters: lcore=',i10,
     & ' mem1=',i10,' ifirst=',i10)
c
      write (nlist,608) (iunits(i),fnames(i),i=1,nunits)
  608 format(/' filenames and unit numbers:'/
     & ' unit description',                      t37,'filename'/
     & 1x,4('-'),1x,11('-'),                     t37,10('-')/
     & 1x,i3,2x,'listing file:',                 t37,a/
     & 1x,i3,2x,'1-e integral file:',            t37,a/
     & 1x,i3,2x,'2-e integral file [fsplit=2]:', t37,a/
     & 1x,i3,2x,'input file:',                   t37,a )
c
      write (nlist,612) ngen,   ns,     naords, ncons,  ngcs,   itol,
     &  icut,   aoints, only1e, inrm,   ncrs,   l1rec,  l2rec,  aoint2,
     &  fsplit
  612 format(/' argos input parameters and titles:'/
     & ' ngen   =',i3,' ns     =',i3,' naords =',i3,' ncons  =',i3,
     & ' ngcs   =',i3,' itol   =',i3/
     & ' icut   =',i3,' aoints =',i3,' only1e =',i3,' inrm   =',i3,
     & ' ncrs   =',i3/
     & ' l1rec  =',i10,           5x,' l2rec  =',i10,           5x,
     & ' aoint2 =',i3,' fsplit =',i3/)
c
      write (nlist,'(a)') (title(i),i=1,ntitle)
      write (nlist,616) (ist, ist=1,nst)
  616 format(//'irrep',9x,12(i4,4x))
      write (nlist,620) (nd(ist), ist=1,nst)
  620 format('degeneracy',12i8)
      write (nlist,624) (ityp(ist), ist=1,nst)
  624 format('label',11x,12(a3,5x))
      write (nlist,628)
  628 format(//'direct product table')
      do 644 ist = 1,nst
        do 640 jst = 1,nst
          do 636 kst = 1,nst
            if ( idp(ist,jst,kst) .eq. 1 )
     1       write (nlist,632) ityp(ist), ityp(jst), ityp(kst)
  632        format(3x,'(',a3,') (',a3,') = ',a3)
  636     continue
  640   continue
  644 continue
c
      write (nlist,648) repnuc, itol, icut, aoints, aoint2
  648 format(//21x,'nuclear repulsion energy',f14.8///'primitive ao ',
     & 'integrals neglected if exponential factor below 10**(-',i2,')',/
     & 'contracted ao and so integrals neglected if value below 10**(-',
     & i2,')'/'symmetry orbital integrals written on units',2i3)
c
      isf = 0
      isfr = 0
      do 840 is = 1,ns
        icu = nc(is)
        write (nlist,656) mtype(is), chg(is), (ic, x(ic,is), y(ic,is),
     &   z(ic,is), ic=1,icu)
  656   format(//36x,a3,' atoms'//30x,'nuclear charge',f7.2//
     &    11x,'center',12x,'x',15x,'y',15x,'z'/(i14,7x,3f16.8))
        if ( icu .gt. 1 ) then
          write (nlist,660) (ica(ic,is,1), ic=1,icu)
  660     format(' operator',6x,'center interchanges'/
     &           '  1(id.)',6x,24i3)
          do 668 ig = 2,ngenp1
            write (nlist,664) ig, (ica(ic,is,ig), ic=1,icu)
  664       format(i3,'(gen.)',5x,24i3)
  668     continue
          do 672 ig = ngenp1+1,ng
            write (nlist,'(i3,11x,24i3)') ig, (ica(ic,is,ig), ic=1,icu)
  672     continue
        endif
        do 796 if = 1,nf(is)
          isf = isf+1
          itl = ntl(isf)
          itu = ntu(isf)
          igcs = mgcs(isf)
          iaords = maords(igcs)
          iru = nir(iaords)
          ictu = nct(isf)
          icons = mcons(isf)
          ircru = nrcr(icons)
          iconu = ncon(icons)
          icsu = 0
          do 704 ir = 1,iru
            icsu = icsu + nd(la(ir,iaords))
  704     continue
          write (nlist,708) lmnp1(icons), lblsh(lmnp1(icons))
  708     format(/i21,a1,' orbitals'//
     &      ' orbital exponents  contraction coefficients')
c         # use larger value than 4 for output lines longer than 80
          ircrh = min(4,ircru)
          do 712 icon = 1,iconu
            write (nlist,'(8(1pg16.7))') zet(icon,icons),
     &        (eta(ircr,icon,icons), ircr=1,ircrh)
  712     continue
          write (nlist,"(/21x,'symmetry orbital labels')")
          jsfr = isfr
          do 720 ir = 1,iru
            jsfr = jsfr + 1
            lai = la(ir,iaords)
            do 716 ia = 1,nd(lai)
              write (nlist,'(10x,7(i12,a3,i1))') (lb(jsfr+(ircr-1)*
     &          icsu),ityp(lai),ia, ircr=1,ircrh)
  716       continue
  720     continue
  724     if(ircrh.lt.ircru) then
            ircrl = ircrh + 1
            ircrh = min(ircrh+4,ircru)
            write (6,"(/20x,'contraction coefficients')")
            do 728 icon = 1,iconu
              write (nlist,'(16x,7(1pg16.7))')
     &          (eta(ircr,icon,icons), ircr=ircrl,ircrh)
  728       continue
            write (nlist,"(/21x,'symmetry orbital labels')")
            jsfr = isfr
            do 736 ir = 1,iru
              jsfr = jsfr + 1
              lai = la(ir,iaords)
              do 732 ia = 1,nd(lai)
                write (nlist,'(10x,7(i12,a3,i1))') (lb(jsfr+(ircr-1)*
     &            icsu),ityp(lai),ia, ircr=ircrl,ircrh)
  732         continue
  736       continue
            go to 724
          endif
          write (nlist,"(/11x,'symmetry orbitals')")
          icsr = 0
          ncol = 0
          do 760 ir = 1,iru
            lai = la(ir,iaords)
            iau = nd(lai)
            do 756 ia = 1,iau
              ncol = ncol+1
              ltyp(ncol) = ityp(lai)
              lbla(ncol) = ia
              if ( ncol .ne. 10 .and.
     &          (ir .ne. iru .or. ia .ne. iau) ) go to 756
              write (nlist,740) (ltyp(icol),lbla(icol), icol=1,ncol)
  740         format(' ctr, ao',16(3x,a3,i1))
              ict = 0
              do 752 ic = 1,icu
                do 748 it = itl,itu
                  ict = ict+1
                  write (nlist,744) ic, (lmnv(i,it), i=1,3),
     &              (c(ict,icsr+icol,igcs), icol=1,ncol)
  744             format(i3,', ',3i1,16f7.3)
  748           continue
  752         continue
              icsr = icsr+ncol
              ncol = 0
  756       continue
  760     continue
          do 792 ircr = 1,ircru
            ics = 0
            do 788 ir = 1,iru
              isfr = isfr+1
              lai = la(ir,iaords)
              iso = nsopr(lai)+lb(isfr)
              ms(iso) = is
c
c             # assign the mnl(*) values to the so functions.
c             # 1:s, 2:p, 3:3s, 4:d, 5:4p, 6:f, 7:5s, 8:5d, 9:g,...
c             #
c             # compute the L^2-operator expectation value and
c             # determine if it is an eigenvalue.
c
              lmn = lmnp1(icons) - 1
c             numxyz = ((lmn + 1) * (lmn + 2)) / 2
c             # ipt(*)-->1:lmnv(1:3,1:lmnvmx), 2:scr(1:xyzdim)
              call l2opxv( lmn, c(1,ics+1,igcs), itl, itu, lmnv,
     &         ipt(55)-ipt(2), a(ipt(2)), eval, leig )
              if ( leig .lt. 0 ) then
c
c               # not an eigenvalue. print a warning.
c
                write(nlist,768) leig, iso, ityp(lai), eval
  768           format(' syminp() warning: from l2opxv(), ierr=',i3,
     &            ' s.o. basis function', i3, a, ' is not an ',
     &            'L^2 eigenvector. <v|L^2|v>/<v|v> =', f12.8 )
c
c               # just assign the maximum value.
                leig = lmn
              endif
              mnl(iso) = ( lmnp1(icons)**2 + 2 * leig + 5 ) / 4
              iau = nd(lai)
              do 784 ia = 1,iau
                ics = ics+1
c
                if ( inam(nu) .ge. 5 .and. leig .eq. 1 ) then
c
c                 # determine angular momentum symmetries.
c
                  do 780 ixyz = 1, 3
                    i2 = mod(ixyz,  3) + 1
                    i3 = mod(ixyz+1,3) + 1
                    sum = a0
                    ict = 0
                    do 776 ic = 1, icu
                      do 772 it = itl,itu
                        ict = ict + 1
                        if( mod(lmnv(ixyz,it),2) .eq. 1 .and.
     &                      mod(lmnv(i2,  it),2) .eq. 0 .and.
     &                      mod(lmnv(i3,  it),2) .eq. 0 ) then
                          sum = sum + c(ict,ics,igcs)
                        endif
  772                 continue
  776               continue
                    if (  (sum .eq. a0)
     &               .or. (lai .eq. ixyzir(ixyz)) ) go to 780
                    if ( ixyzir(ixyz) .ne. 0 ) then
                      write (nlist,*) 'axis choice is unsuitable for'
     &                 //' spin-orbit integrals'
                      call bummer('syminp: ixyzir(ixyz)=',
     &                 ixyzir(ixyz),faterr)
                    endif
                    ixyzir(ixyz) = lai
  780             continue
                endif
  784         continue
  788       continue
  792     continue
  796   continue
c
        icrs = mcrs(is)
        if ( icrs .eq. 0 ) go to 840
        lcru = lcr(icrs)
        if ( lcru .lt. 0 ) go to 816
        kcrl = nkcrl(1,icrs)
        kcru = nkcru(1,icrs)
        write (nlist,804) mtype(is),lblsh(lcru+1),
c    &   (ncr(kcr),kcr=kcrl,kcru)
     &   (ncr(kcr), zcr(kcr), ccr(kcr), kcr=kcrl,kcru)
  804   format(//31x,a3,' core potential'//35x,a1,' potential'/
c    &    7h0powers,6x,7(i8,7x))
     &    19x,'powers',6x,'exponentials',4x,'coefficients'/
     &    (i22,7x,2(1pg16.7)))
c       write (nlist,828) (zcr(kcr), kcr=kcrl,kcru)
c       write (nlist,832) (ccr(kcr), kcr=kcrl,kcru)
        if ( lcru .eq. 0 ) go to 816
        do 812 lp1 = 1,lcru
          kcrl = nkcrl(lp1+1,icrs)
          kcru = nkcru(lp1+1,icrs)
          write (nlist,808) lblsh(lp1), lblsh(lcru+1),
c    &     (ncr(kcr),kcr=kcrl,kcru)
     &     (ncr(kcr), zcr(kcr), ccr(kcr), kcr=kcrl,kcru)
c 808     format(/33x,a1,' - ',a1,' potential'/7h0powers,6x,7(i8,7x))
  808     format(/33x,a1,' - ',a1,' potential'/
     &      19x,'powers',6x,'exponentials',4x,'coefficients'/
     &      (i22,7x,2(1pg16.7)))
c         write (nlist,828) (zcr(kcr), kcr=kcrl,kcru)
c         write (nlist,832) (ccr(kcr), kcr=kcrl,kcru)
  812   continue
c
  816   continue
        llsu = lls(icrs)
        if ( llsu .le. 0 ) go to 840
        write (nlist,820) mtype(is)
  820   format(//28x,a3,' spin-orbit potential')
        do 836 l = 1,llsu
          kcrl = nklsl(l,icrs)
          kcru = nklsu(l,icrs)
c         write (nlist,824) lblsh(l+1), (ncr(kcr), kcr=kcrl,kcru)
          write (nlist,824) lblsh(l+1),
     &     (ncr(kcr), zcr(kcr), ccr(kcr), kcr=kcrl,kcru)
c 824     format(/10x,a1,10h potential/7h0powers,6x,7(i8,7x))
  824     format(/35x,a1,' potential'/
     &      19x,'powers',6x,'exponentials',4x,'coefficients'/
     &      (i22,7x,2(1pg16.7)))
c         write (nlist,828) (zcr(kcr), kcr=kcrl,kcru)
c 828     format(13h exponentials,7(1pg15.7))
c         write (nlist,832) (ccr(kcr), kcr=kcrl,kcru)
c 832     format(13h coefficients,7(1pg15.7))
  836   continue
  840 continue
c
      if ( inam(nu) .eq. 5 ) then
        do 860 ixyz = 1, 3
          i2 = mod(ixyz,  3) + 1
          i3 = mod(ixyz+1,3) + 1
          do 856 ist = 1, nst
            if ( idp(ist,ixyzir(i2),ixyzir(i3)) .ne. 0 ) then
              lxyzir(ixyz) = ist
              go to 860
            endif
  856     continue
  860   continue
        write (nlist,864) (ityp(lxyzir(ixyz)), ixyz=1,3)
  864 format(/'lx: ',a3,10x,'ly: ',a3,10x,'lz: ',a3)
      endif
c
c     # set up the 1-e record parameters.
      ibvtyp = 0
      if ( l1rec .eq. 0 ) then
c       # use the default value for this program.
        l1recx = lrcmxp
      else
c       # use the input value. [(-1) invokes the SIFS default.]
        l1recx = l1rec
      endif
c
      call sifcfg( 1, l1recx, nbft, ibvtyp, ifmt1, l1rec, n1max, ierr )
c
      if ( ierr .ne. 0 ) then
        call bummer('syminp: from 1-e sifcfg(), ierr=',ierr,faterr)
      endif
c
c     # set up the 2-e record parameters.
      ibvtyp = 1
      if ( l2rec .eq. 0 ) then
c       # use the default value for this program.
        l2recx = lrcmxp
      else
c       # use the input value. [(-1) envokes the SIFS default.]
        l2recx = l2rec
      endif
c
      call sifcfg( 2, l2recx, isfr, ibvtyp, ifmt2, l2rec, n2max, ierr )
c
      if ( ierr .ne. 0 ) then
        call bummer('syminp: from 2-e sifcfg(), ierr=',ierr,faterr)
      endif
c
c     # set up the info(*) array for the output file.
      info(1) = fsplit
      info(2) = l1rec
      info(3) = n1max
      info(4) = l2rec
      info(5) = n2max
c
c     # open the aoints file.
c
      open(unit=aoints,status='unknown',form='unformatted',
     & file=fnames(2))
c
      rewind aoints
c
c     # write out the header information
c
c     # ipt(*)-->1:lmnv(1:3,1:lmnvmx), 2:map(1:2,1:nbft)
c     # push map(*) allocation onto the stack.
      call h2opsh( forbyt(2*nbft) )
c
      call wthead( nst,    ns,     isfr,   ntitle, title,  repnuc, ityp,
     &   nso,      mtype,  ms,     mnl,    info,   aoints, nlist,
     &   a(ipt(2)) )
c
c     # pop back to the initial level.
      call h2opop
c
c     # multiply normalization constants into contraction coefficients.
c
      do 876 icons = 1,ncons
        iconu = ncon(icons)
        ircru = nrcr(icons)
        do 872 icon = 1,iconu
          do 868 ircr = 1,ircru
            t = a4 * zet(icon,icons)
            eta(ircr,icon,icons) =
     &       sqrt( t**lmnp1(icons) / a2 * sqrt(t / a2) )
     &       * eta(ircr,icon,icons)
  868     continue
  872   continue
  876 continue
      write (nlist,880) isfr, (ityp(ist), nso(ist), ist=1,nst)
  880 format(//i9,' symmetry orbitals,',3x,4(3x,a3,':',i4),
     &                               (/31x,4(3x,a3,':',i4)))
c
      return
      end
