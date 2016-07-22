*deck lsints
      subroutine lsints( a,              ccr,            crda,
     &                   crdb,           etai,           etaj,
     &                   g2,             g1,             gout,
     &                   ipt,            lls,            lambu,
     &                   ltot1,          mcrs,           mproju,
     &                   nc,             ncr,            nklsl,
     &                   nklsu,          x,              y,
     &                   z,              zcr,            zet )
c
c  lcru is the max l value + 1 for the potential.
c  ncr contains the value of n for each term.
c  zcr contains the value of alpha for each term.
c  ccr contains the coefficient of each term.
c
      implicit real*8 (a-h,o-z)
      logical esf, esfc, igueq1, jgueq1
      parameter (a1s2=0.5d0, a1=1.0d0, a4=4.0d0)
      common /parmr/ cutoff, tol
c
      integer
     & mblu,  mccu, mconu, mcu,  mcxu, mpru, nbft, nnbft,
     & mrcru, mstu, msu,   nblu, ng,   ns,   nst
      common /parmi/
     & mblu,  mccu, mconu, mcu,  mcxu, mpru, nbft, nnbft,
     & mrcru, mstu, msu,   nblu, ng,   ns,   nst
c
      common /one/dxij,dyij,dzij,fnfct,rr,xij,xijm,yij,yijm,zij,zijm,
     1  ibl1,ibl2,icxi1,icxi2,ij,ijsf,ic,
     2  icons,igu,ircru,is,isf,itl,itu,jc,jcons,jgu,jrcru,js,jsf,jtl,
     3  jtu,lit,ljt,nblt1,nc2,nc1,nop,ntij1,ntij2,esf,esfc,igueq1,jgueq1
      common /stv/ xint,yint,zint,t,x0,y0,z0,xi,yi,zi,xj,yj,zj,ni,nj
      common /callin/ xka,yka,zka,ca,xkb,ykb,zkb,cb,tai,taj,aa,taa,
     1  aarr1,aarr2,xk,yk,zk,fctr2,kcrs,lcru
      common /dim21/ ipq(256)
      dimension a(*),ccr(*),crda(lit,3),crdb(ljt,3),etai(mrcru,*),
     1  etaj(mrcru,*),g2(*),g1(*),gout(*),ipt(*),lls(*),
     2  mcrs(*),nc(*),ncr(*),nklsl(4,*),nklsu(4,*),x(mcu,*),
     3  y(mcu,*),z(mcu,*),zcr(*),zet(mconu,*)
c
      do 120 i=1,3
        crda(1,i)=a1
  120 crdb(1,i)=a1
      if(esfc) iu=ipq(lit+1)
      if(igueq1) then
        fctr1=a4*etai(1,1)
      else
        fctr1=a4
      endif
      if(jgueq1) fctr1=fctr1*etaj(1,1)
c
c  i primitive
c
      do 490 ig=1,igu
      if(.not.igueq1) call wzero(nc1,g1,1)
      ai=zet(ig,icons)
      tai=ai+ai
c
c  j primitive
c
      if(esfc) jgu=ig
      do 390 jg=1,jgu
      if(.not.jgueq1) call wzero(ij,gout,1)
      aj=zet(jg,jcons)
      taj=aj+aj
      aa=ai+aj
      taa=aa+aa
      apr=ai*aj/aa
c
c  form spin-orbit potential integrals
c
      fctr2=fctr1
      if((esfc.and..not.igueq1).and.ig.eq.jg) fctr2=a1s2*fctr2
      do 350 ks=1,ns
      kcrs=mcrs(ks)
      if(kcrs.eq.0) go to 350
      lcru=lls(kcrs)+1
      if(lcru.le.1) go to 340
      do 330 kc=1,nc(ks)
      xc=x(kc,ks)
      yc=y(kc,ks)
      zc=z(kc,ks)
      xka=xc-xi
      yka=yc-yi
      zka=zc-zi
      ca=sqrt(xka*xka+yka*yka+zka*zka)
      if(lit.ge.2) then
        crda(2,1)=xka
        crda(2,2)=yka
        crda(2,3)=zka
        if(lit.ge.3) then
          do 220 i=1,3
            do 210 j=3,lit
              crda(j,i)=crda(2,i)*crda(j-1,i)
  210       continue
  220     continue
        endif
      endif
      xkb=xc-xj
      ykb=yc-yj
      zkb=zc-zj
      cb=sqrt(xkb*xkb+ykb*ykb+zkb*zkb)
      if(ljt.ge.2) then
        crdb(2,1)=xkb
        crdb(2,2)=ykb
        crdb(2,3)=zkb
        if(ljt.ge.3) then
          do 240 i=1,3
            do 230 j=3,ljt
              crdb(j,i)=crdb(2,i)*crdb(j-1,i)
  230       continue
  240     continue
        endif
      endif
      aarr2=apr*(ca-cb)**2
      if(aarr2.le.tol) then
        call pseud3( a, a(ipt(37)), a(ipt(38)), ccr, crda, crdb,
     &    a(ipt(19)), gout, ipt, lambu, ltot1, a(ipt(20)), a(ipt(21)),
     &    mproju, ncr, nklsl, nklsu, a(ipt(39)), zcr )
      endif
  330 continue
  340 continue
  350 continue
c
c  j transformation
c
      if(jgueq1) go to 400
c
      j1=0
      do 380 jrcr=1,jrcru
        do 370 i=1,ij
          g1(j1+i)=g1(j1+i)+gout(i)*etaj(jrcr,jg)
  370   continue
        j1=j1+ij
  380 continue
  390 continue
  400 continue
c
c  i transformation
c
c 400 if(igueq1) return
      if(igueq1) return
c
      if(esfc) i1=0
      ij2=0
      do 470 ircr=1,ircru
        j1=0
        if(esfc) then
          do 440 jrcr=1,ircr
            i1m=i1
            do 430 i=1,iu
              i1n=i1m
              do 420 j=1,iu
                do 410 k=1,3
                  g2(ij2+k)=g2(ij2+k)+g1(j1+k)*etai(ircr,ig)-
     1                                g1(i1n+k)*etai(jrcr,ig)
  410           continue
                ij2=ij2+3
                j1=j1+3
                i1n=i1n+(3*iu)
  420         continue
              i1m=i1m+3
  430       continue
  440     continue
          i1=i1+ij
        else
          do 460 jrcr=1,jrcru
            do 450 i=1,ij
              g2(ij2+i)=g2(ij2+i)+g1(j1+i)*etai(ircr,ig)
  450       continue
            ij2=ij2+ij
            j1=j1+ij
  460     continue
        endif
  470 continue
  490 continue
      return
      end
