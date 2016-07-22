*deck s0000
      subroutine s0000( etai,           etaj,          etak,
     &  etal,           g4,             g3,            g2,
     &  g1,             gout,           zet )
      implicit real*8 (a-h,o-z)
      logical esfc, esfcij, esfckl, igueq1, jgueq1, kgueq1, lgueq1
      parameter (a1s2=0.5d0, asrtpi=1.1283791670955126d0,
     1  pie4=7.85398163397448d-01)
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
      common/misc/xij,xijm,yij,yijm,zij,zijm,xkl,xklm,ykl,yklm,zkl,zklm,
     1icons,ircru,jcons,jrcru,kcons,krcru,lcons,lrcru,esfc,esfcij,esfckl
      dimension etai(mrcru,*),etaj(mrcru,*),etak(mrcru,*),
     1  etal(mrcru,*),g4(*),g3(*),g2(*),g1(*),gout(*),zet(mconu,*)
c
      abx=xij-xkl
      aby=yij-ykl
      abz=zij-zkl
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
      px=abx+aaa*xijm
      py=aby+aaa*yijm
      pz=abz+aaa*zijm
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
      bl=zet(lg,lcons)
      bb=bk+bl
      dum=aarij+bl*brkl/bb
      if(dum.gt.tol) go to 150
      expe=fctr3*exp(-dum)/bb
      if(((esfc.and..not.(igueq1.and.jgueq1)).and.(kg.eq.ig)).and.
     1  lg.eq.jg) expe=a1s2*expe
      if((esfckl.and..not.kgueq1).and.lg.eq.kg) expe=a1s2*expe
      bbb=(bk-bl)/bb
      apb=aa+bb
      x=((px-bbb*xklm)**2+(py-bbb*yklm)**2+(pz-bbb*zklm)**2)*aa*bb/apb
      if(x.gt.5.0d+00) go to 50
      if(x.gt.1.0d+00) go to 30
      if(x.gt.3.0d-7) go to 20
      ww1=1.0d+00-x/3.0d+00
      go to 100
   20 f1=          ((((((((-8.36313918003957d-08*x+1.21222603512827d-06
     1)*x-1.15662609053481d-05 )*x+9.25197374512647d-05
     2)*x-6.40994113129432d-04 )*x+3.78787044215009d-03
     3)*x-1.85185172458485d-02 )*x+7.14285713298222d-02
     4)*x-1.99999999997023d-01 )*x+3.33333333333318d-01
      ww1=(x+x)*f1+exp(-x)
      go to 100
   30 if(x.gt.3.0d+00) go to 40
      y=x-2.0d+00
      f1=        ((((((((((-1.61702782425558d-10*y+1.96215250865776d-09
     1)*y-2.14234468198419d-08 )*y+2.17216556336318d-07
     2)*y-1.98850171329371d-06 )*y+1.62429321438911d-05
     3)*y-1.16740298039895d-04 )*y+7.24888732052332d-04
     4)*y-3.79490003707156d-03 )*y+1.61723488664661d-02
     5)*y-5.29428148329736d-02 )*y+1.15702180856167d-01
      ww1=(x+x)*f1+exp(-x)
      go to 100
   40 y=x-4.0d+00
      f1=        ((((((((((-2.62453564772299d-11*y+3.24031041623823d-10
     1)*y-3.614965656163d-09)*y+3.760256799971d-08)*y-3.553558319675d-07
     2)*y+3.022556449731d-06)*y-2.290098979647d-05)*y+1.526537461148d-04
     3)*y-8.81947375894379d-04 )*y+4.33207949514611d-03
     4)*y-1.75257821619926d-02 )*y+5.28406320615584d-02
      ww1=(x+x)*f1+exp(-x)
      go to 100
   50 if(x.gt.15.0d+00) go to 70
      e=exp(-x)
      if(x.gt.10.0d+00) go to 60
      ww1=    (((((( 4.6897511375022d-01/x-6.9955602298985d-01)/x
     1+5.3689283271887d-01)/x-3.2883030418398d-01)/x
     2+2.4645596956002d-01)/x-4.9984072848436d-01)/x
     3-3.1501078774085d-06)*e + sqrt(pie4/x)
      go to 100
   60 ww1=       (((-1.8784686463512d-01/x+2.2991849164985d-01)/x
     1-4.9893752514047d-01)/x-2.1916512131607d-05)*e + sqrt(pie4/x)
      go to 100
   70 if(x.gt.33.0d+00) go to 90
      e=exp(-x)
      ww1=        (( 1.9623264149430d-01/x-4.9695241464490d-01)/x
     1-6.0156581186481d-05)*e + sqrt(pie4/x)
      go to 100
   90 ww1=sqrt(pie4/x)
  100 gout(1)=ww1*expe/sqrt(apb)
c
c  l transformation
c
      if(lgueq1) go to 200
c
      do 140 lrcr=1,lrcru
        g1(lrcr)=g1(lrcr)+gout(1)*etal(lrcr,lg)
  140 continue
  150 continue
c
c  k transformation
c
  200 if(kgueq1) go to 280
c
      kl2=0
      do 240 krcr=1,krcru
        if(esfckl) then
          do 210 lrcr=1,krcr
            g2(kl2+lrcr)=g2(kl2+lrcr)+g1(lrcr)*etak(krcr,kg)+
     1                                g1(krcr)*etak(lrcr,kg)
  210     continue
          kl2=kl2+krcr
        else
          do 230 lrcr=1,lrcru
            g2(kl2+lrcr)=g2(kl2+lrcr)+g1(lrcr)*etak(krcr,kg)
  230     continue
          kl2=kl2+lrcru
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
        do 310 jrcr=1,jrcru
          do 300 lrcr=1,jrcr
            g3(jkl3+lrcr)=g3(jkl3+lrcr)+g2(lrcr)*etaj(jrcr,jg)+
     1                                  g2(jrcr)*etaj(lrcr,jg)
  300     continue
          jkl3=jkl3+jrcr
  310   continue
      else
        do 340 jrcr=1,jrcru
          kl2=0
          do 330 krcr=1,krcru
            if(esfckl) lrcru=krcr
            do 320 lrcr=1,lrcru
              g3(jkl3+lrcr)=g3(jkl3+lrcr)+g2(kl2+lrcr)*etaj(jrcr,jg)
  320       continue
            jkl3=jkl3+lrcru
            kl2=kl2+lrcru
  330     continue
  340   continue
      endif
  350 continue
c
c  i transformation
c
  410 if(igueq1) return
c
      if(esfc) ij3=-nc2+1
      if(esfcij) ikl3i=0
      ijkl4=0
      do 550 ircr=1,ircru
        jkl3j=0
        if(esfcij) jrcru=ircr
        do 540 jrcr=1,jrcru
          jkl3=jkl3j
          if(esfcij) ikl3=ikl3i
          if(esfc) krcru=ircr
          do 520 krcr=1,krcru
            if(esfc) then
              if(esfckl) then
                if(krcr.eq.ircr) then
                  lrcru=jrcr
                else
                  lrcru=krcr
                endif
                do 450 lrcr=1,lrcru
                  g4(ijkl4+lrcr)=g4(ijkl4+lrcr)+
     1                           g3(jkl3+lrcr)*etai(ircr,ig)+
     2                           g3(ikl3+lrcr)*etai(jrcr,ig)+
     3                           g3(ij3+nc2*lrcr)*etai(krcr,ig)+
     4                           g3(ij3+nc2*krcr)*etai(lrcr,ig)
  450           continue
                ijkl4=ijkl4+lrcru
                jkl3=jkl3+krcr
                ikl3=ikl3+krcr
              else
                if(krcr.eq.ircr) then
                  lrcru=jrcr
                else
                  lrcru=jrcru
                endif
                do 470 lrcr=1,lrcru
                  g4(ijkl4+lrcr)=g4(ijkl4+lrcr)+
     1                           g3(jkl3+lrcr)*etai(ircr,ig)+
     2                           g3(ij3+nc2*lrcr)*etai(krcr,ig)
  470           continue
                ijkl4=ijkl4+lrcru
                jkl3=jkl3+jrcru
              endif
            else
              if(esfckl) lrcru=krcr
              if(esfcij) then
                do 490 lrcr=1,lrcru
                  g4(ijkl4+lrcr)=g4(ijkl4+lrcr)+
     1                           g3(jkl3+lrcr)*etai(ircr,ig)+
     2                           g3(ikl3+lrcr)*etai(jrcr,ig)
  490           continue
                ijkl4=ijkl4+lrcru
                jkl3=jkl3+lrcru
                ikl3=ikl3+lrcru
              else
                do 510 lrcr=1,lrcru
                  g4(ijkl4+lrcr)=g4(ijkl4+lrcr)+
     1                           g3(jkl3+lrcr)*etai(ircr,ig)
  510           continue
                ijkl4=ijkl4+lrcru
                jkl3=jkl3+lrcru
              endif
            endif
  520     continue
          jkl3j=jkl3j+nc2
          if(esfc) ij3=ij3+1
  540   continue
        if(esfcij) ikl3i=ikl3i+nc2
  550 continue
      return
      end
