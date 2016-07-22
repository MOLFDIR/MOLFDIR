c
c  RCS $Revision: 1.1.1.1 $  $Date: 2001/04/09 08:15:53 $
c
*deck gcentr
      subroutine gcentr(ica,nc)
c
c  generate group of center-interchange operators
c
      integer
     & mblu,  mccu, mconu, mcu,  mcxu, mpru, nbft, nnbft,
     & mrcru, mstu, msu,   nblu, ng,   ns,   nst
      common /parmi/
     & mblu,  mccu, mconu, mcu,  mcxu, mpru, nbft, nnbft,
     & mrcru, mstu, msu,   nblu, ng,   ns,   nst
c
      dimension ica(mcu,msu,*), nc(*)
c
      if (ng .eq. 1) go to 56
c
c  set up identity operator
c
      do 16 is=1,ns
        icu=nc(is)
        if(icu.eq.1) go to 16
        do 12 ic=1,icu
          ica(ic,is,1)=ic
   12   continue
   16 continue
c
c        do 52(+3) ig=2,ng
c
      ig=2
   20 do 52 jg=2,ig
      igp=ig
      jgp=jg
c
c  generate new operator candidate
c
   24 do 32 is=1,ns
        icu=nc(is)
        if(icu.eq.1) go to 32
        do 28 ic=1,icu
          ica(ic,is,ng+1)=ica(ica(ic,is,igp),is,jgp)
   28   continue
   32 continue
c
c  check against list of operators
c
      do 44 kg=1,ng
        do 40 is=1,ns
          icu=nc(is)
          if(icu.eq.1) go to 40
          do 36 ic=1,icu
            if(ica(ic,is,ng+1).ne.ica(ic,is,kg)) go to 44
   36     continue
   40   continue
c       # not a new operator
        go to 48
   44 continue
c     # new operator found
      ng=ng+1
   48 if(jgp.eq.ig) go to 52
c
c  take product in other order
c
      igp=jg
      jgp=ig
      go to 24
   52 continue
      if(ig.eq.ng) go to 56
      ig=ig+1
      go to 20
c
c  set up operators for unique nuclei
c
   56 do 64 is=1,ns
      if(nc(is).eq.1) then
        do 60 ig=1,ng
          ica(1,is,ig)=1
   60   continue
      endif
   64 continue
      return
      end
