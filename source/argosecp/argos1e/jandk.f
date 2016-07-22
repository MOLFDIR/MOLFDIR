*deck jandk
      subroutine jandk( a,      eta,    g4,     ipt,    is,     isf,
     &  ic,     js,     jsf,    jc,     ks,     ksf,    kc,     ls,
     &  lsf,    lc,     ijgt,   ijx,    ijy,    ijz,    klgt,   klx,
     &  kly,    klz,    lmnp1,  lmnv,   mcons,  ncon,   nrcr,   nt,
     &  ntl,    ntu,    x,      y,      z,      zet )
      implicit real*8 (a-h,o-z)
      logical esfc, esfcij, esfckl, igueq1, jgueq1, kgueq1, lgueq1
      parameter (a1s2=0.5d0)
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
      common /shlnos/lit,ljt,lkt,llt,lklt,ljklt,ij,kl,ijkl
      common /shlinf/ xi,yi,zi,rij,xk,yk,zk,rkl,igu,jgu,kgu,lgu,nc4,
     1  nc3,nc2,nc1,igueq1,jgueq1,kgueq1,lgueq1
      common/misc/xij,xijm,yij,yijm,zij,zijm,xkl,xklm,ykl,yklm,zkl,zklm,
     1icons,ircru,jcons,jrcru,kcons,krcru,lcons,lrcru,esfc,esfcij,esfckl
      common /setint/ dxij,dyij,dzij,dxkl,dykl,dzkl,bp01,b00,b10,xcp00,
     1  xc00,ycp00,yc00,zcp00,zc00,f00,ni,nj,nk,nl,nmax,mmax
      common /dim21/ ipq(256)
c
c     # bummer error types.
      integer   wrnerr,  nfterr,  faterr
      parameter(wrnerr=0,nfterr=1,faterr=2)
c
      integer  atebyt, forbyt
      external atebyt, forbyt
c
      dimension a(*), eta(mrcru,mconu,*), g4(*), ipt(*), ijgt(*),
     1  ijx(*), ijy(*), ijz(*), klgt(*), klx(*), kly(*), klz(*),
     2  lmnp1(*), lmnv(3,*), mcons(*), ncon(*), nrcr(*), nt(*), ntl(*),
     3  ntu(*), x(mcu,*), y(mcu,*), z(mcu,*), zet(mconu,*)
c
      icons=mcons(isf)
      lit=lmnp1(icons)
      jcons=mcons(jsf)
      ljt=lmnp1(jcons)
      kcons=mcons(ksf)
      lkt=lmnp1(kcons)
      lcons=mcons(lsf)
      llt=lmnp1(lcons)
      lklt=lkt*llt
      ljklt=ljt*lklt
      ngtl=1
      ngtk=nt(lsf)
      ngtj=ngtk*nt(ksf)
      ngti=ngtj*nt(jsf)
c
c  ishell
c
      xi=x(ic,is)
      yi=y(ic,is)
      zi=z(ic,is)
      igu=ncon(icons)
      mini=ntl(isf)
      maxi=ntu(isf)
      ircru=nrcr(icons)
c
c  jshell
c
      xj=x(jc,js)
      yj=y(jc,js)
      zj=z(jc,js)
      jgu=ncon(jcons)
      minj=ntl(jsf)
      maxj=ntu(jsf)
      jrcru=nrcr(jcons)
c
c  prepare items for pairs of (i,j) functions
c
      esfcij=isf.eq.jsf.and.ic.eq.jc
      xij=a1s2*(xi+xj)
      dxij=xi-xj
      xijm=a1s2*dxij
      yij=a1s2*(yi+yj)
      dyij=yi-yj
      yijm=a1s2*dyij
      zij=a1s2*(zi+zj)
      dzij=zi-zj
      zijm=a1s2*dzij
      rij=dxij*dxij+dyij*dyij+dzij*dzij
      ij=0
      do 150 i=mini,maxi
        nx=ljklt*lmnv(1,i) + 1
        ny=ljklt*lmnv(2,i) + 1
        nz=ljklt*lmnv(3,i) + 1
        do 140 j=minj,maxj
          ij=ij+1
          ijx(ij)=nx+lklt*lmnv(1,j)
          ijy(ij)=ny+lklt*lmnv(2,j)
          ijz(ij)=nz+lklt*lmnv(3,j)
          ijgt(ij)=ngti*(i-mini)+ngtj*(j-minj)+1
  140   continue
  150 continue
c
c  kshell
c
      xk=x(kc,ks)
      yk=y(kc,ks)
      zk=z(kc,ks)
      kgu=ncon(kcons)
      mink=ntl(ksf)
      maxk=ntu(ksf)
      krcru=nrcr(kcons)
c
c  lshell
c
      xl=x(lc,ls)
      yl=y(lc,ls)
      zl=z(lc,ls)
      lgu=ncon(lcons)
      minl=ntl(lsf)
      maxl=ntu(lsf)
      lrcru=nrcr(lcons)
c
c  prepare items for pairs of (k,l) functions
c
      esfckl=ksf.eq.lsf.and.kc.eq.lc
      xkl=a1s2*(xk+xl)
      dxkl=xk-xl
      xklm=a1s2*dxkl
      ykl=a1s2*(yk+yl)
      dykl=yk-yl
      yklm=a1s2*dykl
      zkl=a1s2*(zk+zl)
      dzkl=zk-zl
      zklm=a1s2*dzkl
      rkl=dxkl*dxkl+dykl*dykl+dzkl*dzkl
      kl=0
      do 350 k=mink,maxk
        nx=llt*lmnv(1,k)
        ny=llt*lmnv(2,k)
        nz=llt*lmnv(3,k)
        do 340 l=minl,maxl
          kl=kl+1
          klx(kl)=nx+lmnv(1,l)
          kly(kl)=ny+lmnv(2,l)
          klz(kl)=nz+lmnv(3,l)
          klgt(kl)=ngtk*(k-mink)+ngtl*(l-minl)
  340   continue
  350 continue
c
c  prepare miscellaneous items
c
      nroots=(lit+ljt+lkt+llt-2)/2
      esfc=isf.eq.ksf.and.ic.eq.kc.and.jsf.eq.lsf.and.jc.eq.lc
      ijkl=ij*kl
      ngout=ngti*nt(isf)
c
c  allocate space for transforming ao integrals
c
      igueq1=igu.eq.1
      jgueq1=jgu.eq.1
      kgueq1=kgu.eq.1
      lgueq1=lgu.eq.1
      nc1=lrcru*ngout
      if(esfckl) then
        nc2=ipq(krcru+1)*ngout
      else
        nc2=krcru*nc1
        if(igueq1.and.esfc.and.(.not.jgueq1)) then
          nc3=ipq(jrcru+1)*ngout
          nc4=nc3
          go to 660
        endif
      endif
      nc3=jrcru*nc2
      if(esfc) then
        if(esfcij) then
          nc4=ipq(ircru+1)
        else
          nc4=ircru*jrcru
        endif
        nc4=ipq(nc4+1)*ngout
      else
        if(esfcij) then
          nc4=ipq(ircru+1)*nc2
        else
          nc4=ircru*nc3
        endif
      endif
c
  660 if(igueq1) then
        ic3=0
      else
        ic3=nc4
      endif
      if(jgueq1) then
        ic2=0
      else
        ic2=nc3
      endif
      if(kgueq1) then
        ic1=0
      else
        ic1=nc2
      endif
      if(lgueq1) then
        ic0=0
      else
        ic0=nc1
      endif
c
      nxyzin = nroots*lit*ljklt
      n1 = nroots + 1
c     # allocate space.
c     # ipt(*)-->1:lmnv(1:3,1:lmnvmx), 2:il(1:nnbft), 3:cx(1:mcxu),
c     #          4:buffer(1:l2rec), 5:values(1:n2max),
c     #          6:labels(1:4,1:n2max), 7:ibitv(1:n2max), 8:h4(1:nblu),
c     #          9:ijgt(1:iju(iscm)), 10:ijx(1:iju(iscm)),
c     #          11:ijy(1:iju(iscm)), 12:ijz(1:iju(iscm)),
c     #          13:klgt(1:klu(iscm)), 14:klx(1:klu(iscm)),
c     #          15:kly(1:klu(iscm)), 16:klz(1:klu(iscm)), 17:g4(1:ic3),
c     #          18:g3(1:ic2), 19:g2(1:ic1), 20:g1(1:ic0),
c     #          21:gout(1:ngout), 22:in1(1:lit+ljt-1),
c     #          23:in(1:lit+ljt-1), 24:kn(1:lkt+llt-1),
c     #          25:uf(1:nroots), 26:wf(1:nroots), 27:ff(1:nroots+n1),
c     #          (cf and sf must be kept together for droot)
c     #          28:cf(1:n1,1:n1), 29:sf(1:n1,1:n1), 30:af(1:n1),
c     #          31:rt(1:n1), 32:r(1:nroots,1:nroots),
c     #          (34-36 use same space as 27-33)
c     #          33:w(1:nroots,1:nroots), 34:xin(1:nxyzin),
c     #          35:yin(1:nxyzin), 36:zin(1:nxyzin)
      ipt(18) = ipt(17) + atebyt( ic3 )
      ipt(19) = ipt(18) + atebyt( ic2 )
      ipt(20) = ipt(19) + atebyt( ic1 )
      ipt(21) = ipt(20) + atebyt( ic0 )
      ipt(22) = ipt(21) + atebyt( ngout )
      ipt(23) = ipt(22) + forbyt( lit + ljt - 1 )
      ipt(24) = ipt(23) + forbyt( lit + ljt - 1 )
      ipt(25) = ipt(24) + forbyt( lkt + llt - 1 )
      ipt(26) = ipt(25) + atebyt( nroots )
      ipt(27) = ipt(26) + atebyt( nroots )
      ipt(28) = ipt(27) + 2*atebyt( nroots + n1 )
      ipt(29) = ipt(28) + 2*atebyt( n1**2 )
      ipt(30) = ipt(29) + 2*atebyt( n1**2 )
      ipt(31) = ipt(30) + 2*atebyt( n1 )
      ipt(32) = ipt(31) + 2*atebyt( n1 )
      ipt(33) = ipt(32) + 2*atebyt( nroots**2 )
      ipt(34) = ipt(27)
      ipt(35) = ipt(34) + atebyt( nxyzin )
      ipt(36) = ipt(35) + atebyt( nxyzin )
      ipt(37) = max( ipt(33) + 2*atebyt( nroots**2 ),
     &               ipt(36) + atebyt( nxyzin ) )
      if ( (ipt(37)-1) .gt. mblu ) then
        call bummer('jandk: mblu too small ',(ipt(37)-1-mblu),faterr)
      endif
c     # set the high-water mark for g4(*) usage.
      call h2oset( (ipt(37)-ipt(17)) )
c
c  compute two-electron integrals
c
      call wzero(nc4,g4,1)
      if(ijkl.eq.1) then
            call s0000( eta(1,1,icons), eta(1,1,jcons), eta(1,1,kcons),
     &  eta(1,1,lcons), g4,             a(ipt(18)),     a(ipt(19)),
     &  a(ipt(20)),     a(ipt(21)),     zet )
      else
            call genral( a,              eta(1,1,icons), eta(1,1,jcons),
     &   eta(1,1,kcons), eta(1,1,lcons), g4,             a(ipt(18)),
     &   a(ipt(19)),     a(ipt(20)),     a(ipt(21)),     ijgt,
     &   ijx,            ijy,            ijz,            klgt,
     &   klx,            kly,            klz,            a(ipt(22)),
     &   a(ipt(23)),     a(ipt(24)),     ipt,            nroots,
     &   a(ipt(25)),     a(ipt(26)),     n1,             a(ipt(34)),
     &   a(ipt(35)),     a(ipt(36)),     zet )
      endif
      return
      end
