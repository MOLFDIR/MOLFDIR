*deck tints
      subroutine tints( etai, etaj, g2, g1, gout, hpt, hwt, ijx, ijy,
     &  ijz, xin, yin, zin, zet )
      implicit real*8 (a-h,o-z)
      logical esf, esfc, igueq1, jgueq1
      parameter (a0=0.0d0, a1s2=0.5d0, a1=1.0d0, a2=2.0d0)
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
      dimension etai(mrcru,*), etaj(mrcru,*), g2(*), g1(*), gout(*),
     1  hpt(*), hwt(*), ijx(*), ijy(*), ijz(*), xin(*), yin(*), zin(*),
     2  zet(mconu,*)
c
      lijt = lit*ljt
      if(igueq1) then
        fctr1=etai(1,1)
      else
        fctr1=a1
      endif
      if(jgueq1) fctr1=fctr1*etaj(1,1)
c
c  i primitive
c
      do 490 ig=1,igu
      if(.not.igueq1) call wzero(nc1,g1,1)
      ai=zet(ig,icons)
      arri=ai*rr
c
c  j primitive
c
      if(esfc) jgu=ig
      do 390 jg=1,jgu
      if(.not.jgueq1) call wzero(ij,gout,1)
      aj=zet(jg,jcons)
      aa=ai+aj
      aaa=(ai-aj)/aa
      dum=aj*arri/aa
      if(dum.gt.tol) go to 390
c
c  form kinetic energy integrals
c
      x0=xij+aaa*xijm
      y0=yij+aaa*yijm
      z0=zij+aaa*zijm
      t=sqrt(aa)
      fctr2=fctr1*exp(-dum)/(t*aa**3)
      if((esfc.and..not.igueq1).and.ig.eq.jg) fctr2=a1s2*fctr2
      dum=ai*aj
      tx=dum*dxij
      ty=dum*dyij
      tz=dum*dzij
      dum=dum*aa
      txx=dum-a2*tx*tx
      tyy=dum-a2*ty*ty
      tzz=dum-a2*tz*tz
      nij=0
      tii=a0
      ti=a0
      do 330 in=1,lit
      ni=in
      tjj=a0
      tj=a0
      do 320 jn=1,ljt
      nj=jn
      nij=nij+1
      call stvint(hpt,hwt)
      xin(nij)=xint
      xin(nij+lijt)=txx*xint
      yin(nij)=yint
      yin(nij+lijt)=tyy*yint
      zin(nij)=zint
      zin(nij+lijt)=tzz*zint
      if(jn.eq.1) go to 210
      dum=tj+tj
      xin(nij+lijt)=xin(nij+lijt)+dum*tx*xin(nij-1)
      yin(nij+lijt)=yin(nij+lijt)+dum*ty*yin(nij-1)
      zin(nij+lijt)=zin(nij+lijt)+dum*tz*zin(nij-1)
      if(jn.eq.2) go to 210
      xin(nij+lijt)=xin(nij+lijt)+tjj*xin(nij-2)
      yin(nij+lijt)=yin(nij+lijt)+tjj*yin(nij-2)
      zin(nij+lijt)=zin(nij+lijt)+tjj*zin(nij-2)
  210 if(in.eq.1) go to 310
      dum=ti+ti
      xin(nij+lijt)=xin(nij+lijt)+dum*tx*xin(nij-ljt)
      yin(nij+lijt)=yin(nij+lijt)+dum*ty*yin(nij-ljt)
      zin(nij+lijt)=zin(nij+lijt)+dum*tz*zin(nij-ljt)
      if(jn.eq.1) go to 220
      dum=-tj*ti
      xin(nij+lijt)=xin(nij+lijt)+dum*xin(nij-ljt-1)
      yin(nij+lijt)=yin(nij+lijt)+dum*yin(nij-ljt-1)
      zin(nij+lijt)=zin(nij+lijt)+dum*zin(nij-ljt-1)
  220 if(in.eq.2) go to 310
      xin(nij+lijt)=xin(nij+lijt)+tii*xin(nij-2*ljt)
      yin(nij+lijt)=yin(nij+lijt)+tii*yin(nij-2*ljt)
      zin(nij+lijt)=zin(nij+lijt)+tii*zin(nij-2*ljt)
  310 tjj=tjj-tj*ai
      tj=tj+ai
  320 continue
      tii=tii+ti*aj
      ti=ti-aj
  330 continue
      do 350 i=1,ij
        nx=ijx(i)
        ny=ijy(i)
        nz=ijz(i)
        gout(i)=gout(i)+fctr2*(xin(nx+lijt)*yin(ny)*zin(nz)+xin(nx)*
     1    yin(ny+lijt)*zin(nz)+xin(nx)*yin(ny)*zin(nz+lijt))
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
c
c  i transformation
c
  400 if(igueq1) return
c
      if(esfc) i1=0
      ij2=0
      do 470 ircr=1,ircru
        j1=0
        if(esfc) then
          do 430 jrcr=1,ircr
            do 410 i=1,ij
              g2(ij2+i)=g2(ij2+i)+g1(j1+i)*etai(ircr,ig)+
     1                            g1(i1+i)*etai(jrcr,ig)
  410       continue
            ij2=ij2+ij
            j1=j1+ij
  430     continue
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
