*deck cecob
      subroutine cecob(
     & ic, ica, icb, ieqs3, is, js, ks, ls, ncc, nfct, ngw )
c
      implicit integer(a-z)
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
      common /ntgr/itl,itu,jtl,jtu,ktl,ktu,ltl,ltu,jcu,kcu,lcu,inx,
     1  jnx,knx,lnx,nwt,nwt1(3),ijsf(3),klsf(3),icxs(2,3),kcxs(2,3),
     2  npri(2,3),nprk(2,3),iesfb,kesfb,ircru,jrcru,krcru,lrcru
      dimension ica(mcu,msu,*), icb(4,24,*), nfct(*), ngw(*)
c
c     # bummer error types.
      integer   wrnerr,  nfterr,  faterr
      parameter (wrnerr=0,nfterr=1,faterr=2)
c
      go to (100,200,300,400,500,600,700,800), ieqs3
c
c  center combinations for is=js=ks=ls
c
  100 do 170 jc = 1, ic
      do 165 kc = 1, jc
      do 160 lc = 1, kc
      igw = 0
      do 140 ig = 1, ng
      ic1 = ica(ic,is,ig)
      jc1 = ica(jc,is,ig)
      kc1 = ica(kc,is,ig)
      lc1 = ica(lc,is,ig)
      if(ic1.ne.ic.and.jc1.ne.ic.and.kc1.ne.ic.and.lc1.ne.ic) go to 140
      ic2 = max(ic1,jc1)
      jc2 = min(ic1,jc1)
      kc2 = max(kc1,lc1)
      lc2 = min(kc1,lc1)
      ic3 = max(ic2,kc2)
      kc2 = min(ic2,kc2)
      lc3 = min(jc2,lc2)
      jc2 = max(jc2,lc2)
      jc3 = max(jc2,kc2)
      kc3 = min(jc2,kc2)
      do 115 icc = ncc, 1, -1
        if(jc3 - icb(2,1,icc)) 115, 105, 120
  105   if(kc3 - icb(3,1,icc)) 115, 110, 120
  110   if(lc3 - icb(4,1,icc)) 115, 145, 120
  115 continue
  120 if(jc3 .ne. jc .or. kc3 .ne. kc .or. lc3 .ne. lc) go to 140
      do 130 igw1 = igw, 1, -1
        if (ic1.eq.icb(1,igw1,ncc+1).and.
     &      jc1.eq.icb(2,igw1,ncc+1).and.
     &      kc1.eq.icb(3,igw1,ncc+1).and.
     &      lc1.eq.icb(4,igw1,ncc+1)) go to 140
  130 continue
      igw = igw + 1
      icb(1,igw,ncc+1) = ic1
      icb(2,igw,ncc+1) = jc1
      icb(3,igw,ncc+1) = kc1
      icb(4,igw,ncc+1) = lc1
  140 continue
      ncc = ncc + 1
      nfct(ncc) = ic
      ngw(ncc) = igw
      go to 150
  145 nfct(icc) = nfct(icc) + ic
  150 continue
  160 continue
  165 continue
  170 continue
      do 190  icc = 1, ncc
        jc1  = icb(2,1,icc)
        kc1  = icb(3,1,icc)
        lc1  = icb(4,1,icc)
        idc = 1
        if (jc1 .ne. ic) idc = 2
        if (kc1 .ne. jc1) idc = idc + 1
        if (lc1 .ne. kc1) idc = idc + 1
        nfct(icc) = nfct(icc)/idc
  190 continue
      go to 900
c
c  center combinations for is=js=ks>ls
c
  200 do 270 jc = 1, ic
      do 265 kc = 1, jc
      do 260 lc = 1, lcu
      igw = 0
      do 240 ig = 1, ng
      ic1 = ica(ic,is,ig)
      jc1 = ica(jc,is,ig)
      kc1 = ica(kc,is,ig)
      if(ic1.ne.ic.and.jc1.ne.ic.and.kc1.ne.ic) go to 240
      lc1 = ica(lc,ls,ig)
      ic2 = max(ic1,jc1)
      jc2 = min(ic1,jc1)
      ic3 = max(ic2,kc1)
      kc2 = min(ic2,kc1)
      jc3 = max(jc2,kc2)
      kc3 = min(jc2,kc2)
      do 215 icc = ncc, 1, -1
        if(jc3 - icb(2,1,icc)) 215, 205, 220
  205   if(kc3 - icb(3,1,icc)) 215, 210, 220
  210   if(lc1 - icb(4,1,icc)) 215, 245, 220
  215 continue
  220 if(jc3 .ne. jc .or. kc3 .ne. kc .or. lc1 .ne. lc) go to 240
      do 230 igw1 = igw, 1, -1
        if (ic1.eq.icb(1,igw1,ncc+1).and.
     &      jc1.eq.icb(2,igw1,ncc+1).and.
     &      kc1.eq.icb(3,igw1,ncc+1)) go to 240
  230 continue
      igw = igw + 1
      icb(1,igw,ncc+1) = ic1
      icb(2,igw,ncc+1) = jc1
      icb(3,igw,ncc+1) = kc1
      icb(4,igw,ncc+1) = lc1
  240 continue
      ncc = ncc + 1
      nfct(ncc) = ic
      ngw(ncc) = igw
      go to 260
  245 nfct(icc) = nfct(icc) + ic
  260 continue
  265 continue
  270 continue
      do 290 icc = 1, ncc
        jc1  = icb(2,1,icc)
        kc1  = icb(3,1,icc)
        idc = 1
        if ( jc1 .ne. ic) idc = 2
        if (kc1 .ne. jc1) idc = idc + 1
        nfct(icc) = nfct(icc)/idc
  290 continue
      go to 900
c
c  center combinations for is=js>ks=ls
c
  300 do 370 jc = 1, ic
      do 365 kc = 1, kcu
      do 360 lc = 1, kc
      igw = 0
      do 340 ig = 1, ng
      ic1 = ica(ic,is,ig)
      jc1 = ica(jc,is,ig)
      if(ic1.ne.ic.and.jc1.ne.ic) go to 340
      kc1 = ica(kc,ks,ig)
      lc1 = ica(lc,ks,ig)
      ic2 = max(ic1,jc1)
      jc2 = min(ic1,jc1)
      kc2 = max(kc1,lc1)
      lc2 = min(kc1,lc1)
      do 315 icc = ncc, 1, -1
        if (jc2 - icb(2,1,icc)) 315, 305, 320
  305   if (kc2 - icb(3,1,icc)) 315, 310, 320
  310   if (lc2 - icb(4,1,icc)) 315, 345, 320
  315 continue
  320 if (jc2 .ne. jc .or. kc2 .ne. kc .or. lc2 .ne. lc) go to 340
      do 330 igw1 = igw, 1, -1
        if( ic1.eq.icb(1,igw1,ncc+1).and.
     &      jc1.eq.icb(2,igw1,ncc+1).and.
     &      kc1.eq.icb(3,igw1,ncc+1).and.
     &      lc1.eq.icb(4,igw1,ncc+1)) go to 340
  330 continue
      igw = igw + 1
      icb (1,igw,ncc+1) = ic1
      icb (2,igw,ncc+1) = jc1
      icb (3,igw,ncc+1) = kc1
      icb (4,igw,ncc+1) = lc1
  340 continue
      ncc = ncc + 1
      nfct(ncc) =  ic
      ngw(ncc) = igw
      go to 360
  345 nfct(icc) = nfct(icc) + ic
  360 continue
  365 continue
  370 continue
      go to 480
c
c  center combinations for is=js>ks>ls
c
  400 do 470 jc = 1, ic
      do 465 kc = 1, kcu
      do 460 lc = 1, lcu
      igw = 0
      do 440 ig = 1, ng
      ic1 = ica(ic,is,ig)
      jc1 = ica(jc,is,ig)
      if(ic1.ne.ic.and.jc1.ne.ic) go to 440
      kc1 = ica(kc,ks,ig)
      lc1 = ica(lc,ls,ig)
      ic2 = max(ic1,jc1)
      jc2 = min(ic1,jc1)
      do 415 icc = ncc, 1, -1
        if (jc2 - icb(2,1,icc)) 415, 405, 420
  405   if (kc1 - icb(3,1,icc)) 415, 410, 420
  410   if (lc1 - icb(4,1,icc)) 415, 445, 420
  415 continue
  420 if(jc2 .ne. jc .or. kc1 .ne. kc .or. lc1 .ne. lc) go to 440
      do 430 igw1 = igw, 1, -1
        if (ic1.eq.icb(1,igw1,ncc+1).and.
     &      jc1.eq.icb(2,igw1,ncc+1)) go to 440
  430 continue
      igw = igw + 1
      icb(1,igw,ncc+1) = ic1
      icb(2,igw,ncc+1) = jc1
      icb(3,igw,ncc+1) = kc1
      icb(4,igw,ncc+1) = lc1
  440 continue
      ncc = ncc + 1
      nfct(ncc) = ic
      ngw(ncc) = igw
      go to 460
  445 nfct(icc) = nfct(icc) + ic
  460 continue
  465 continue
  470 continue
  480 do 490 icc = 1, ncc
        if(icb(2,1,icc).ne.ic) nfct(icc)=nfct(icc)/2
  490 continue
      go to 900
c
c  center combinations for is>js=ks=ls
c
  500 do 570 jc = 1, jcu
      do 565 kc = 1, jc
      do 560 lc = 1, kc
      igw =0
      do 540 ig = 1, ng
      if (ica(ic,is,ig) .ne. ic) go to 540
      jc1 = ica(jc,js,ig)
      kc1 = ica(kc,js,ig)
      lc1 = ica(lc,js,ig)
      jc2 = max (jc1,kc1)
      kc2 = min (jc1,kc1)
      jc3 = max (jc2,lc1)
      lc2 = min (jc2,lc1)
      kc3 = max (kc2,lc2)
      lc3 = min (kc2,lc2)
      do 515 icc = ncc, 1, -1
      if (jc3 - icb(2,1,icc)) 515, 505, 520
  505 if (kc3 - icb(3,1,icc)) 515, 510, 520
  510 if (lc3 - icb(4,1,icc)) 515, 545, 520
  515 continue
  520 if (jc3.ne.jc.or.kc3.ne.kc.or.lc3.ne.lc) go to 540
      do 530 igw1 = igw, 1, -1
        if (jc1.eq.icb(2,igw1,ncc+1).and.
     &      kc1.eq.icb(3,igw1,ncc+1).and.
     &      lc1.eq.icb(4,igw1,ncc+1)) go to 540
  530 continue
      igw = igw + 1
      icb (1,igw,ncc+1) = ic
      icb (2,igw,ncc+1) = jc1
      icb (3,igw,ncc+1) = kc1
      icb (4,igw,ncc+1) = lc1
  540 continue
      ncc = ncc + 1
      nfct(ncc) = ic
      ngw(ncc) = igw
      go to 560
  545 nfct(icc) = nfct(icc) + ic
  560 continue
  565 continue
  570 continue
      go to 900
c
c  center combinations for is>js=ks>ls
c
  600 do 670 jc = 1, jcu
      do 665 kc = 1, jc
      do 660 lc = 1, lcu
      igw = 0
      do 640 ig = 1, ng
      if (ica(ic,is,ig) .ne. ic) go to 640
      jc1 = ica(jc,js,ig)
      kc1 = ica(kc,js,ig)
      lc1 = ica(lc,ls,ig)
      jc2 = max (jc1, kc1)
      kc2 = min (jc1, kc1)
      do 615 icc= ncc, 1, -1
        if (jc2 - icb(2,1,icc)) 615, 605, 620
  605   if (kc2 - icb(3,1,icc)) 615, 610, 620
  610   if (lc1 - icb(4,1,icc)) 615, 645, 620
  615 continue
  620 if (jc2.ne.jc.or.kc2.ne.kc.or.lc1.ne.lc) go to 640
      do 630 igw1 = igw, 1, -1
        if (jc1.eq.icb(2,igw1,ncc+1).and.
     &      kc1.eq.icb(3,igw1,ncc+1)) go to 640
  630 continue
      igw = igw + 1
      icb (1,igw,ncc+1) = ic
      icb (2,igw,ncc+1) = jc1
      icb (3,igw,ncc+1) = kc1
      icb (4,igw,ncc+1) = lc1
  640 continue
      ncc = ncc + 1
      nfct(ncc) = ic
      ngw (ncc) = igw
      go to 660
  645 nfct(icc) = nfct(icc) + ic
  660 continue
  665 continue
  670 continue
      go to 900
c
c  center combinations for is>js>ks=ls
c
  700 do 770 jc = 1, jcu
      do 765 kc = 1, kcu
      do 760 lc = 1, kc
      igw = 0
      do 740 ig = 1, ng
      if (ica(ic,is,ig) .ne. ic) go to 740
      jc1 = ica(jc,js,ig)
      kc1 = ica(kc,ks,ig)
      lc1 = ica(lc,ks,ig)
      kc2 = max (kc1, lc1)
      lc2 = min (kc1, lc1)
      do 715 icc = ncc, 1, -1
        if (jc1 - icb(2,1,icc)) 715, 705, 720
  705   if (kc2 - icb(3,1,icc)) 715, 710, 720
  710   if (lc2 - icb(4,1,icc)) 715, 745, 720
  715 continue
  720 if (jc1.ne.jc.or.kc2.ne.kc.or.lc2.ne.lc) go to 740
      do 730 igw1 = igw, 1, -1
        if(kc1.eq.icb(3,igw1,ncc+1).and.
     &     lc1.eq.icb(4,igw1,ncc+1)) go to 740
  730 continue
      igw = igw + 1
      icb (1,igw,ncc+1) = ic
      icb (2,igw,ncc+1) = jc1
      icb (3,igw,ncc+1) = kc1
      icb (4,igw,ncc+1) = lc1
  740 continue
      ncc = ncc + 1
      nfct(ncc) = ic
      ngw(ncc) = igw
      go to 760
  745 nfct(icc) = nfct(icc) + ic
  760 continue
  765 continue
  770 continue
      go to 900
c
c  center combinations for is>js>ks>ls
c
  800 do 870 jc = 1, jcu
      do 865 kc = 1, kcu
      do 860 lc = 1, lcu
      do 840 ig = 1, ng
      if (ica(ic,is,ig) .ne. ic) go to 840
      jc1 = ica(jc,js,ig)
      kc1 = ica(kc,ks,ig)
      lc1 = ica(lc,ls,ig)
      do 815 icc = ncc, 1, -1
        if (jc1 - icb(2,1,icc)) 815, 805, 840
  805   if (kc1 - icb(3,1,icc)) 815, 810, 840
  810   if (lc1 - icb(4,1,icc)) 815, 845, 840
  815 continue
  840 continue
      ncc = ncc + 1
      icb (1, 1, ncc) = ic
      icb (2, 1, ncc) = jc
      icb (3, 1, ncc) = kc
      icb (4, 1, ncc) = lc
      nfct(ncc) = ic
      go to 860
  845 nfct(icc) = nfct(icc) + ic
  860 continue
  865 continue
  870 continue
  900 if(ncc.gt.mccu) then
        write (nlist,*) 'cecob: mccup too small', ncc
        call bummer('cecob: ncc=',ncc,faterr)
      endif
      return
      end
