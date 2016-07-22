*deck aoso2
      subroutine aoso2( h4, cx, iscm, val )
      implicit real*8 (a-h,o-z)
c
      integer
     & mblu,  mccu, mconu, mcu,  mcxu, mpru, nbft, nnbft,
     & mrcru, mstu, msu,   nblu, ng,   ns,   nst
      common /parmi/
     & mblu,  mccu, mconu, mcu,  mcxu, mpru, nbft, nnbft,
     & mrcru, mstu, msu,   nblu, ng,   ns,   nst
c
      parameter (a0=0.0d0)
      common /ntgr/itl,itu,jtl,jtu,ktl,ktu,ltl,ltu,jcu,kcu,lcu,inx,
     1  jnx,knx,lnx,nwt,nwt1(3),ijsf(3),klsf(3),icxs(2,3),kcxs(2,3),
     2  npri(2,3),nprk(2,3),iesfb,kesfb,ircru,jrcru,krcru,lrcru
      common /ikpr/ npriri(2,3,8), nprirk(2,3,8)
      common /sf3ao/ ibl
c
      dimension cx(*), h4(*)
      icx = icxs(iesfb,iscm)
      kcxst = kcxs(kesfb,iscm)
      do 56 ist=1,nst
      npairi = npriri(iesfb,iscm,ist)
      npairk = nprirk(kesfb,iscm,ist)
      if(npairi.eq.0) go to 52
      if(npairk.ne.0) go to 20
      icx=icx+npairi
      go to 56
   20 do 48 ijbl=1,npairi
        icx=icx+1
        if(cx(icx).eq.a0) go to 44
        do 40 klbl=1,npairk
          h4(ibl+klbl)=h4(ibl+klbl)+cx(kcxst+klbl)*(cx(icx)*val)
   40   continue
   44   ibl=ibl+npairk
   48 continue
   52 kcxst = kcxst + npairk
   56 continue
      return
      end
