*deck aoso2e
      subroutine aoso2e( h4, cx, iscm, val )
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
      dimension cx(*), h4(*)
c
      icx = icxs(iesfb,iscm)
      jcx = kcxs(iesfb,iscm)
      do 72 ist=1,nst
      kcxst = jcx
      lcxst = icx
      do 64 ijbl=1,npriri(iesfb,iscm,ist)
      icx=icx+1
      jcx=jcx+1
      if(cx(icx).eq.a0.and.cx(jcx).eq.a0) then
        ibl=ibl+ijbl
      else
        kcx = kcxst
        lcx = lcxst
        if(icx.eq.jcx) then
          do 48 klbl=1,ijbl
            kcx=kcx+1
            ibl=ibl+1
            h4(ibl)=h4(ibl)+(cx(icx)*val)*cx(kcx)
   48     continue
        else
          do 56 klbl=1,ijbl
            kcx=kcx+1
            lcx=lcx+1
            ibl=ibl+1
            h4(ibl)=h4(ibl)+(cx(icx)*val)*cx(kcx)+(cx(jcx)*val)*cx(lcx)
   56     continue
        endif
      endif
   64 continue
   72 continue
      return
      end
