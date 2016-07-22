*deck genral
      subroutine genral( a,              etai,           etaj,
     &   etak,           etal,           g4,             g3,
     &   g2,             g1,             gout,           ijgt,
     &   ijx,            ijy,            ijz,            klgt,
     &   klx,            kly,            klz,            in1,
     &   in,             kn,             ipt,            nroots,
     &   uf,             wf,             n1,             xin,
     &   yin,            zin,            zet )
      implicit real*8 (a-h,o-z)
      logical esfc, esfcij, esfckl, igueq1, jgueq1, kgueq1, lgueq1
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
      common /shlinf/ xi,yi,zi,rij,xk,yk,zk,rkl,igu,jgu,kgu,lgu,nc4,
     1  nc3,nc2,nc1,igueq1,jgueq1,kgueq1,lgueq1
      common /shlnos/lit,ljt,lkt,llt,lklt,ljklt,ij,kl,ijkl
      common/misc/xij,xijm,yij,yijm,zij,zijm,xkl,xklm,ykl,yklm,zkl,zklm,
     1icons,ircru,jcons,jrcru,kcons,krcru,lcons,lrcru,esfc,esfcij,esfckl
      common /setint/ dxij,dyij,dzij,dxkl,dykl,dzkl,bp01,b00,b10,xcp00,
     1  xc00,ycp00,yc00,zcp00,zc00,f00,ni,nj,nk,nl,nmax,mmax
      dimension a(*),etai(mrcru,*),etaj(mrcru,*),etak(mrcru,*),
     1  etal(mrcru,*),g4(*),g3(*),g2(*),g1(*),gout(*),ijgt(*),ijx(*),
     2  ijy(*),ijz(*),klgt(*),klx(*),kly(*),klz(*),in1(*),in(*),kn(*),
     3  ipt(*),uf(*),wf(*),xin(*),yin(*),zin(*),zet(mconu,*)
c
      lijklt=lit*ljklt
      mmu=nroots*lijklt
      ni=lit-1
      nj=ljt-1
      nk=lkt-1
      nl=llt-1
      nmax=ni+nj
      mmax=nk+nl
      ihi=nmax+1
      do 12 i=1,ihi
        if(i.le.lit) then
          in1(i)=ljklt*(i-1)+1
        else
          in1(i)=ljklt*(lit-1)+lklt*(i-lit)+1
        endif
   12 continue
      ihi=mmax+1
      do 22 i=1,ihi
        if(i.le.lkt) then
          kn(i)=llt*(i-1)
        else
          kn(i)=llt*(lkt-1)+i-lkt
        endif
   22 continue
      if(igueq1) then
        fctr1=asrtpi*etai(1,1)
      else
        fctr1=asrtpi
      endif
      if(jgueq1) fctr1=fctr1*etaj(1,1)
      if(kgueq1) fctr1=fctr1*etak(1,1)
      if(lgueq1) fctr1=fctr1*etal(1,1)
      kgl=1
c
c  i primitive
c
      do 550 ig=1,igu
      if(.not.igueq1) call wzero(nc3,g3,1)
      fctr2=fctr1
      ai=zet(ig,icons)
      arij=ai*rij
      if(esfc) kgl=ig
c
c  j primitive
c
      if(esfcij) jgu=ig
      do 350 jg=1,jgu
      if(.not.jgueq1) call wzero(nc2,g2,1)
      if((esfcij.and..not.igueq1).and.jg.eq.ig) fctr2=a1s2*fctr2
      aj=zet(jg,jcons)
      aa=ai+aj
      aarij=aj*arij/aa
      if(aarij.gt.tol) go to 350
      fctr3=fctr2/aa
      aaa=(ai-aj)/aa
      xa=xij+aaa*xijm
      ya=yij+aaa*yijm
      za=zij+aaa*zijm
      axai=aa*(xa-xi)
      ayai=aa*(ya-yi)
      azai=aa*(za-zi)
      axak=aa*(xa-xk)
      ayak=aa*(ya-yk)
      azak=aa*(za-zk)
c
c  k primitive
c
      do 250 kg=kgl,kgu
      if(.not.kgueq1) call wzero(nc1,g1,1)
      bk=zet(kg,kcons)
      brkl=bk*rkl
c
c  l primitive
c
      if(esfc.and.(kg.eq.ig)) then
        lgl=jg
      else
        lgl=1
      endif
      if(esfckl) lgu=kg
      do 150 lg=lgl,lgu
      if(.not.lgueq1) call wzero(ijkl,gout,1)
      bl=zet(lg,lcons)
      bb=bk+bl
      dum=aarij+bl*brkl/bb
      if(dum.gt.tol) go to 150
      apb=aa+bb
      expe=fctr3*exp(-dum)/(bb*sqrt(apb))
      if(((esfc.and..not.(igueq1.and.jgueq1)).and.(kg.eq.ig)).and.
     1  lg.eq.jg) expe=a1s2*expe
      if((esfckl.and..not.kgueq1).and.lg.eq.kg) expe=a1s2*expe
      bbb=(bk-bl)/bb
      xb=xkl+bbb*xklm
      yb=ykl+bbb*yklm
      zb=zkl+bbb*zklm
      atb=aa*bb
      rho=atb/apb
      xx=rho*((xa-xb)**2+(ya-yb)**2+(za-zb)**2)
      bxbi=bb*(xb-xi)
      bybi=bb*(yb-yi)
      bzbi=bb*(zb-zi)
      bxbk=bb*(xb-xk)
      bybk=bb*(yb-yk)
      bzbk=bb*(zb-zk)
      c1x=bxbk+axak
      c2x=aa*bxbk
      c3x=bxbi+axai
      c4x=bb*axai
      c1y=bybk+ayak
      c2y=aa*bybk
      c3y=bybi+ayai
      c4y=bb*ayai
      c1z=bzbk+azak
      c2z=aa*bzbk
      c3z=bzbi+azai
      c4z=bb*azai
c
c  roots and weights for quadrature
c
      if(nroots.le.3) then
        call rt123(nroots,xx,uf,wf)
      elseif(nroots.eq.4) then
        call root4(xx,uf,wf)
      elseif(nroots.eq.5) then
        call root5(xx,uf,wf)
      else
        call droot(nroots,n1,xx,a(ipt(27)),a(ipt(28)),a(ipt(29)),
     &    a(ipt(30)),a(ipt(31)),a(ipt(32)),a(ipt(33)),uf,wf)
      endif
c
c  compute two-electron integrals for each root
c
      mm=0
      ihi=nmax+1
      do 120 m=1,nroots
        u2=uf(m)*rho
        f00=expe*wf(m)
        do 100 i=1,ihi
          in(i)=in1(i)+mm
  100   continue
        dum=atb+u2*apb
        dum2=dum+dum
        bp01=(aa+u2)/dum2
        b00=u2/dum2
        b10=(bb+u2)/dum2
        xcp00=(u2*c1x+c2x)/dum
        xc00 =(u2*c3x+c4x)/dum
        ycp00=(u2*c1y+c2y)/dum
        yc00 =(u2*c3y+c4y)/dum
        zcp00=(u2*c1z+c2z)/dum
        zc00 =(u2*c3z+c4z)/dum
        call xyzint(in,kn,xin,yin,zin)
        mm=mm+lijklt
  120 continue
c
c  form (i,j//k,l) integrals over functions
c
      do 126 i=1,ij
        nx=ijx(i)-lijklt
        ny=ijy(i)-lijklt
        nz=ijz(i)-lijklt
        nijgt=ijgt(i)
        do 124 k=1,kl
          mx=nx+klx(k)
          my=ny+kly(k)
          mz=nz+klz(k)
          n=nijgt+klgt(k)
          do 122 mm=lijklt,mmu,lijklt
            gout(n)=gout(n)+xin(mx+mm)*yin(my+mm)*zin(mz+mm)
  122     continue
  124   continue
  126 continue
c
c  l transformation
c
      if(lgueq1) go to 200
c
      l1=0
      do 140 lrcr=1,lrcru
        do 135 n=1,ijkl
          g1(l1+n)=g1(l1+n)+gout(n)*etal(lrcr,lg)
  135   continue
        l1=l1+ijkl
  140 continue
  150 continue
c
c  k transformation
c
  200 if(kgueq1) go to 280
c
      if(esfckl) k1=0
      kl2=0
      do 240 krcr=1,krcru
        l1=0
        if(esfckl) then
          do 210 lrcr=1,krcr
            do 205 n=1,ijkl
              g2(kl2+n)=g2(kl2+n)+g1(l1+n)*etak(krcr,kg)+
     1                            g1(k1+n)*etak(lrcr,kg)
  205       continue
            kl2=kl2+ijkl
            l1=l1+ijkl
  210     continue
          k1=k1+ijkl
        else
          do 230 lrcr=1,lrcru
            do 225 n=1,ijkl
              g2(kl2+n)=g2(kl2+n)+g1(l1+n)*etak(krcr,kg)
  225       continue
            kl2=kl2+ijkl
            l1=l1+ijkl
  230     continue
        endif
  240 continue
  250 continue
c
c  j transformation
c
  280 if(jgueq1) go to 410
c
      jkl3=0
      if(esfc.and.igueq1) then
        ij2=0
        do 310 jrcr=1,jrcru
          kl2=0
          do 305 lrcr=1,jrcr
            do 300 nij=1,ij
              ij2n=ij2+nij
              do 290 nkl=1,ij
                g3(jkl3+nkl)=g3(jkl3+nkl)+g2(kl2+nkl)*etaj(jrcr,jg)+
     1                                    g2(ij2n)*etaj(lrcr,jg)
                ij2n=ij2n+ij
  290         continue
              jkl3=jkl3+ij
              kl2=kl2+ij
  300       continue
  305     continue
          ij2=ij2+ijkl
  310   continue
      else
        do 340 jrcr=1,jrcru
          kl2=0
          do 335 krcr=1,krcru
            if(esfckl) lrcru=krcr
            do 330 lrcr=1,lrcru
              do 325 n=1,ijkl
                g3(jkl3+n)=g3(jkl3+n)+g2(kl2+n)*etaj(jrcr,jg)
  325         continue
              jkl3=jkl3+ijkl
              kl2=kl2+ijkl
  330       continue
  335     continue
  340   continue
      endif
  350 continue
c
c  i transformation
c
  410 if(igueq1) return
c
      if(esfc) ij3=0
      if(esfcij) ikl3i=0
      ijkl4=0
      do 545 ircr=1,ircru
      jkl3j=0
      if(esfcij) jrcru=ircr
      do 540 jrcr=1,jrcru
      if(esfc) jkl3k=jkl3j
      if(esfcij) ikl3k=ikl3i
      if(esfc.and.esfcij) kij3=ij3
      if(esfc) krcru=ircr
      do 520 krcr=1,krcru
      if(esfc) then
        jkl3=jkl3k
        lij3=ij3
        if(esfckl) then
          ikl3=ikl3k
          if(krcr.eq.ircr) then
            lrcru=jrcr
          else
            lrcru=krcr
          endif
          do 450 lrcr=1,lrcru
            do 445 n=1,ijkl
              g4(ijkl4+n)=g4(ijkl4+n)+g3(jkl3+n)*etai(ircr,ig)+
     1                                g3(ikl3+n)*etai(jrcr,ig)+
     2                                g3(lij3+n)*etai(krcr,ig)+
     3                                g3(kij3+n)*etai(lrcr,ig)
  445       continue
            ijkl4=ijkl4+ijkl
            jkl3=jkl3+ijkl
            ikl3=ikl3+ijkl
            lij3=lij3+nc2
  450     continue
          kij3=kij3+nc2
          jkl3k=jkl3k+(krcr*ijkl)
          ikl3k=ikl3k+(krcr*ijkl)
        else
          if(krcr.eq.ircr) then
            lrcru=jrcr
          else
            lrcru=jrcru
          endif
          do 470 lrcr=1,lrcru
            do 465 nij=1,ij
              lij3n=lij3+nij
              do 463 nkl=1,ij
                g4(ijkl4+nkl)=g4(ijkl4+nkl)+g3(jkl3+nkl)*etai(ircr,ig)+
     1                                      g3(lij3n)*etai(krcr,ig)
                lij3n=lij3n+ij
  463         continue
              ijkl4=ijkl4+ij
              jkl3=jkl3+ij
  465       continue
            lij3=lij3+nc2
  470     continue
          jkl3k=jkl3k+nc1
        endif
      else
        if(esfckl) lrcru=krcr
        if(esfcij) then
          do 490 lrcr=1,lrcru
            do 485 n=1,ijkl
              g4(ijkl4+n)=g4(ijkl4+n)+g3(jkl3j+n)*etai(ircr,ig)+
     1                                g3(ikl3k+n)*etai(jrcr,ig)
  485       continue
            ijkl4=ijkl4+ijkl
            jkl3j=jkl3j+ijkl
            ikl3k=ikl3k+ijkl
  490     continue
        else
          do 510 lrcr=1,lrcru
            do 505 n=1,ijkl
              g4(ijkl4+n)=g4(ijkl4+n)+g3(jkl3j+n)*etai(ircr,ig)
  505       continue
            ijkl4=ijkl4+ijkl
            jkl3j=jkl3j+ijkl
  510     continue
        endif
      endif
  520 continue
      if(esfc) jkl3j=jkl3j+nc2
      if(esfc) ij3=ij3+ijkl
  540 continue
      if(esfcij) ikl3i=ikl3i+nc2
  545 continue
  550 continue
      return
      end
