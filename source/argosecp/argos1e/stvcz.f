*deck stvcz
      subroutine stvcz( a,          ccr,        chg,        cx,
     &       eta,       g2,         h2,         ijx,        ijy,
     &       ijz,       ilxyz,      ipt,        lcr,        lls,
     &       lmnv,      lproju,     mcrs,       nc,         ncr,
     &       nkcrl,     nkcru,      nklsl,      nklsu,      nopir,
     &       nprir,     nt,         x,          y,          z,
     &       zcr,       zet )
c
      implicit real*8 (a-h,o-z)
      logical chsign,ec,esf,esfb,esfc,esfbc,igueq1,jgueq1
      parameter (a1s2=0.5d0)
c
      integer   nunits
      parameter(nunits=4)
      integer        iunits
      common /units/ iunits(nunits)
      integer                  nlist
      equivalence ( iunits(1), nlist )
c
      common /parmr/ cutoff, tol
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
      common /one/dxij,dyij,dzij,fnfct,rr,xij,xijm,yij,yijm,zij,zijm,
     1  ibl1,ibl2,icxi1,icxi2,ij,ijsf,ic,
     2  icons,igu,ircru,is,isf,itl,itu,jc,jcons,jgu,jrcru,js,jsf,jtl,
     3  jtu,lit,ljt,nblt1,nc2,nc1,nop,ntij1,ntij2,esf,esfc,igueq1,jgueq1
      common /stv/ xint,yint,zint,t,x0,y0,z0,xi,yi,zi,xj,yj,zj,ni,nj
      common /dim21/ ipq(256)
      dimension a(*),ccr(*),chg(*),cx(*),eta(mrcru,mconu,*),g2(*),h2(*),
     1  ijx(*),ijy(*),ijz(*),ilxyz(3,*),ipt(*),lcr(*),
     2  lls(*),lmnv(3,*),mcrs(*),nc(*),ncr(*),nkcrl(6,*),nkcru(6,*),
     3  nklsl(4,*),nklsu(4,*),nopir(*),nprir(2,mstu,*),nt(*),x(mcu,*),
     4  y(mcu,*),z(mcu,*),zcr(*),zet(mconu,*)
c
c  bummer error types.
      integer    wrnerr,  nfterr,  faterr
      parameter (wrnerr=0,nfterr=1,faterr=2)
c
      integer  atebyt, forbyt
      external atebyt, forbyt
c
      lijt = lit*ljt
      ec   = ic.eq.jc
      esfc = esf.and.ec
      icx2 = icxi2+((ic-1)*nc(js)+jc-1)*ntij2
      if(.not.esfc.and.ircru.ne.1.and.esf) then
        ibld=ibl2-ibl1
        icx3=icx2-(ic-jc)*(nc(is)-1)*ntij2
      endif
      if(esf) icx1=icxi1+ipq((ic-1)*nt(isf)+1)*nblt1+(jc-1)*ntij1
c
c  ishell
c
      xi=x(ic,is)
      yi=y(ic,is)
      zi=z(ic,is)
c
c  jshell
c
      xj=x(jc,js)
      yj=y(jc,js)
      zj=z(jc,js)
      xij=a1s2*(xi+xj)
      dxij=xi-xj
      xijm=a1s2*dxij
      yij=a1s2*(yi+yj)
      dyij=yi-yj
      yijm=a1s2*dyij
      zij=a1s2*(zi+zj)
      dzij=zi-zj
      zijm=a1s2*dzij
      rr=dxij*dxij+dyij*dyij+dzij*dzij
c
c  prepare items for pairs of (i,j) functions
c
      ij=0
      do 20 it=itl,itu
        nx=ljt*lmnv(1,it)+1
        ny=ljt*lmnv(2,it)+1
        nz=ljt*lmnv(3,it)+1
        do 16 jt=jtl,jtu
          ij=ij+1
          ijx(ij)=nx+lmnv(1,jt)
          ijy(ij)=ny+lmnv(2,jt)
          ijz(ij)=nz+lmnv(3,jt)
   16   continue
   20 continue
      ij=nop*ij
      nc1=jrcru*ij
      if(esfc) then
        nc2=ipq(ircru+1)*ij
      else
        nc2=ircru*nc1
      endif
      igueq1=igu.eq.1
      jgueq1=jgu.eq.1
      if(igueq1) then
        ic1=0
      else
        ic1=nc2
      endif
      if(jgueq1) then
        ic0=0
      else
        ic0=nc1
      endif
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
c     #          24:ijy(1,iiju), 25:ijz(1:iiju), 26:g2(1:ic1),
c     #          27:g1(1:ic0), 28:gout(1:ij),
c     #        if s, t, or v integrals, then
c     #          29:xin(1:nxyzin), 30:yin(1:nxyzin), 31:zin(1:nxyzin),
c     #          32:uf(1:nroots), 33:wf(1,nroots), 34:ff(1:nroots+n1),
c     #          (cf and sf must be kept together for droot)
c     #          35:cf(1:n1,1:n1), 36:sf(1:n1,1:n1), 37:af(1:n1),
c     #          38:rt(1:n1), 39:r(1:nroots,1:nroots),
c     #          40:w(1:nroots,1:nroots)
c     #        elseif c or z integrals, then
c     #          29:crda(1:lit,1:3), 30:crdb(1:ljt,1:3),
c     #          (for pseud1)
c     #          31:ang(1:ltot1,1:ltot1), 32:q(1:ltot1,1:ltot1),
c     #          33:qsum(1:ltot1,1:ltot1), 34:xab(1:ltot1),
c     #          35:yab:(1:ltot1), 36:zab(1:ltot1),
c     #          (for pseud2 and pseud3)
c     #          37:anga(1:lit,1:mproju,1:lamau),
c     #          38:angb(1:ljt,1:mproju,1:lambu),
c     #          39:qsum(1:ltot1,1:lambu,1:lamau), 40:apwr(1:ljt),
c     #          41:aterm1(1:ljt), 42:aterm2(1:ljt), 43:bpref(1:ljt),
c     #          44:bpwr(1:lit), 45:bterm1(1:lit), 46:ssi(ltot1+lproju),
c     #          47:abess(1:lamau), 48:bbess(1:lambu),
c     #          49:ptpow(1:ltot1), 50:q2(1:lambu,1:lamau)
c     #        endif
      ipt(27) = ipt(26) + atebyt( ic1 )
      ipt(28) = ipt(27) + atebyt( ic0 )
      ipt(29) = ipt(28) + atebyt( ij )
      go to (22,24,26,30,30), nnam
   22 nroots = 0
      nxyzin = lijt
      go to 28
   24 nroots = 0
      nxyzin = 2*lijt
      go to 28
   26 nroots = (lit+ljt)/2
      nxyzin = nroots*lijt
   28 n1 = nroots + 1
      ipt(30) = ipt(29) + atebyt( nxyzin )
      ipt(31) = ipt(30) + atebyt( nxyzin )
      ipt(32) = ipt(31) + atebyt( nxyzin )
      ipt(33) = ipt(32) + atebyt( nroots )
      ipt(34) = ipt(33) + atebyt( nroots )
      ipt(35) = ipt(34) + 2*atebyt( nroots + n1 )
      ipt(36) = ipt(35) + 2*atebyt( n1**2 )
      ipt(37) = ipt(36) + 2*atebyt( n1**2 )
      ipt(38) = ipt(37) + 2*atebyt( n1 )
      ipt(39) = ipt(38) + 2*atebyt( n1 )
      ipt(40) = ipt(39) + 2*atebyt( nroots**2 )
      ipt(51) = ipt(40) + 2*atebyt( nroots**2 )
      go to 32
   30 ipt(30) = ipt(29) + atebyt( 3*lit )
      ipt(31) = ipt(30) + atebyt( 3*ljt )
      ltot1 = lit+ljt-1
      ltot12 = ltot1**2
      ipt(32) = ipt(31) + atebyt( ltot12 )
      ipt(33) = ipt(32) + atebyt( ltot12 )
      ipt(34) = ipt(33) + atebyt( ltot12 )
      ipt(35) = ipt(34) + atebyt( ltot1 )
      ipt(36) = ipt(35) + atebyt( ltot1 )
      ipt(37) = ipt(36) + atebyt( ltot1 )
      mproju = 2*lproju+1
      lamau = lit+lproju
      ipt(38) = ipt(37) + atebyt( lit*mproju*lamau )
      lambu = ljt+lproju
      ipt(39) = ipt(38) + atebyt( ljt*mproju*lambu )
      ipt(40) = ipt(39) + atebyt( ltot1*lambu*lamau )
      ipt(41) = ipt(40) + atebyt( ljt )
      ipt(42) = ipt(41) + atebyt( ljt )
      ipt(43) = ipt(42) + atebyt( ljt )
      ipt(44) = ipt(43) + atebyt( ljt )
      ipt(45) = ipt(44) + atebyt( lit )
      ipt(46) = ipt(45) + atebyt( lit )
      ipt(47) = ipt(46) + atebyt( ltot1+lproju )
      ipt(48) = ipt(47) + atebyt( lamau )
      ipt(49) = ipt(48) + atebyt( lambu )
      ipt(50) = ipt(49) + atebyt( ltot1 )
      ipt(51) = ipt(50) + atebyt( lambu*lamau )
   32 continue
c
      if ( (ipt(51)-1) .gt. mblu ) then
        call bummer('oneint: mblu too small ',(ipt(51)-1-mblu),faterr)
      endif
c     if ( mgout .gt. mblu ) then
c       call bummer('stvcz: (mgout-mblu)=',(mgout-mblu),faterr)
c     endif
c
c     # set the high-water mark.
c     call h2oset( mgout )
      call h2oset( ipt(51)-ipt(26) )
c
      call wzero( nc2, g2, 1 )
c
      go to (36,38,40,42,54), nnam
   36       call sints( eta(1,1,icons), eta(1,1,jcons), g2,
     &      a(ipt(27)), a(ipt(28)),    	a(ipt(9)),      a(ipt(10)),
     &      ijx,        ijy,            ijz,            a(ipt(29)),
     &      a(ipt(30)), a(ipt(31)),     zet )
      go to 58
   38       call tints( eta(1,1,icons), eta(1,1,jcons), g2,
     &      a(ipt(27)), a(ipt(28)),     a(ipt(9)),      a(ipt(10)),
     &      ijx,        ijy,            ijz,            a(ipt(29)),
     &      a(ipt(30)), a(ipt(31)),     zet )
      go to 58
   40       call vints( a,              chg,            eta(1,1,icons),
     &  eta(1,1,jcons), g2,             a(ipt(27)),     a(ipt(28)),
     &  a(ipt(9)),      a(ipt(10)),     ijx,            ijy,
     &  ijz,            ipt,            nc,             nroots,
     &  n1,             a(ipt(32)),     a(ipt(33)),     x,
     &  a(ipt(29)),     y,              a(ipt(30)),     z,
     &  a(ipt(31)),     zet )
      go to 58
   42       call cints( a,              ccr,            a(ipt(29)),
     &                  a(ipt(30)),     eta(1,1,icons), eta(1,1,jcons),
     &                  g2,             a(ipt(27)),     a(ipt(28)),
     &                  ipt,            lcr,            lambu,
     &                  ltot1,          mcrs,           mproju,
     &                  nc,             ncr,            nkcrl,
     &                  nkcru,          x,              y,
     &                  z,              zcr,            zet )
      go to  58
   54       call lsints( a,              ccr,            a(ipt(29)),
     &                   a(ipt(30)),     eta(1,1,icons), eta(1,1,jcons),
     &                   g2,             a(ipt(27)),     a(ipt(28)),
     &                   ipt,            lls,            lambu,
     &                   ltot1,          mcrs,           mproju,
     &                   nc,             ncr,            nklsl,
     &                   nklsu,          x,              y,
     &                   z,              zcr,            zet )
   58 ndx=0
      ndxa=0
      do 92 ircr=1,ircru
      if(esfc) jrcru=ircr
      do 90 jrcr=1,jrcru
      esfb=esf.and.ircr.eq.jrcr
      esfbc=esfb.and.ec
      chsign=nnam.ge.5.and.esf.and.ircr.lt.jrcr
      if(.not.esfb) go to 60
      icx=icx1
      go to 66
   60 if((.not.esf).or.esfc) go to 64
      if(ircr.gt.jrcr) go to 62
      ndxa=(ipq(jrcr)+(ircr-1))*ibl2-(jrcr-1)*ibld
      icx=icx3
      ngti=nop
      ngtj=(nop*nt(isf))
      go to 70
   62 if(jrcr.eq.1) ndxa=(ipq(ircr)+(jrcr-1))*ibl2-(ircr-1)*ibld
   64 icx=icx2
   66 ngti=(nop*nt(jsf))
      ngtj=nop
   70 ndxi=ndx
      do 88 it=itl,itu
      ndxj=ndxi
      if(esfbc) jtu=it
      ndxi=ndxi+ngti
      do 86 jt=jtl,jtu
        index=ndxj
        ndxj=ndxj+ngtj
        ndxb=ndxa
        do 84 ist=1,nst
          iopu=nopir(ist)
          if(esfb) then
            npr=nprir(1,ist,ijsf)
          else
            npr=nprir(2,ist,ijsf)
          endif
          if(iopu.eq.0) go to 82
          if(npr.eq.0) go to 84
          do 80 iop=1,iopu
            val=g2(index+ilxyz(iop,ist))
            if(abs(val).le.cutoff) go to 78
            if(chsign) val=-val
            val=fnfct*val
            do 76 ibl=1,npr
              h2(ndxb+ibl)=h2(ndxb+ibl)+cx(icx+ibl)*val
   76       continue
   78       ndxb=ndxb+npr
   80     continue
   82     icx=icx+npr
   84   continue
   86 continue
   88 continue
      ndxa=ndxb
      ndx=ndx+ij
   90 continue
   92 continue
      return
      end
