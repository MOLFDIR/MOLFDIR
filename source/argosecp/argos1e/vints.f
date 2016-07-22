*deck vints
      subroutine vints( a,              chg,            etai,
     &  etaj,           g2,             g1,             gout,
     &  hpt,            hwt,            ijx,            ijy,
     &  ijz,            ipt,            nc,             nroots,
     &  n1,             uf,             wf,             x,
     &  xin,            y,              yin,            z,
     &  zin,            zet )
      implicit real*8 (a-h,o-z)
      logical esf, esfc, igueq1, jgueq1
      parameter (a1s2=0.5d0, asrtpi=1.1283791670955126d0)
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
      dimension a(*), chg(*), etai(mrcru,*), etaj(mrcru,*), g2(*),
     1  g1(*), gout(*), hpt(*), hwt(*), ijx(*), ijy(*), ijz(*), ipt(*),
     2  nc(*), uf(*), wf(*), x(mcu,*), xin(*), y(mcu,*), yin(*),
     3  z(mcu,*), zin(*), zet(mconu,*)
c
      lijt = lit*ljt
      mmu=lijt*nroots
      if(igueq1) then
        fctr1=asrtpi*etai(1,1)
      else
        fctr1=asrtpi
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
c  form nuclear attraction integrals
c
      ax=xij+aaa*xijm
      ay=yij+aaa*yijm
      az=zij+aaa*zijm
      fctr2=fctr1*exp(-dum)/aa
      if((esfc.and..not.igueq1).and.ig.eq.jg) fctr2=a1s2*fctr2
      aax=aa*ax
      aay=aa*ay
      aaz=aa*az
      do 356 ks=1,ns
        fctr3=-chg(ks)*fctr2
        do 354 kc=1,nc(ks)
          cx=x(kc,ks)
          cy=y(kc,ks)
          cz=z(kc,ks)
          xx=aa*((ax-cx)**2+(ay-cy)**2+(az-cz)**2)
          if(nroots.le.3) then
            call rt123(nroots,xx,uf,wf)
          elseif(nroots.eq.4) then
            call root4(xx,uf,wf)
          elseif(nroots.eq.5) then
            call root5(xx,uf,wf)
          else
            call droot(nroots,n1,xx,a(ipt(34)),a(ipt(35)),a(ipt(36)),
     &        a(ipt(37)),a(ipt(38)),a(ipt(39)),a(ipt(40)),uf,wf)
          endif
          mm=0
          do 340 k=1,nroots
            uu=aa*uf(k)
            ww=wf(k)*fctr3
            tt=aa+uu
            t=sqrt(tt)
            x0=(aax+uu*cx)/tt
            y0=(aay+uu*cy)/tt
            z0=(aaz+uu*cz)/tt
            do 330 in=1,lit
              ni=in
              do 320 jn=1,ljt
                nj=jn
                mm=mm+1
                call stvint(hpt,hwt)
                xin(mm)=xint
                yin(mm)=yint
                zin(mm)=zint*ww
  320         continue
  330       continue
  340     continue
          do 352 i=1,ij
            nx=ijx(i)-lijt
            ny=ijy(i)-lijt
            nz=ijz(i)-lijt
            do 350 mm=lijt,mmu,lijt
              gout(i)=gout(i)+xin(nx+mm)*yin(ny+mm)*zin(nz+mm)
  350       continue
  352     continue
  354   continue
  356 continue
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
