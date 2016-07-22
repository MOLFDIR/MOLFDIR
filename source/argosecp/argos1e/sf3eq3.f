*deck sf3eq3
      subroutine sf3eq3( a,        cx,       eta,      g4,
     &         h4,       icb,      icc,      ipt,      iscmci,
     &         is,       isf,      ks,       ksf,      ijgt,
     &         ijx,      ijy,      ijz,      klgt,     klx,
     &         kly,      klz,      lmnp1,    lmnv,     mcons,
     &         nc,       ncon,     nfct,     ngw,      nrcr,
     &         nt,       ntl,      ntu,      x,        y,
     &         z,        zet )
      implicit real*8 (a-h,o-z)
      logical iceqjc, kceqlc
      common /parmr/ cutoff, tol
c
      integer
     & mblu,  mccu, mconu, mcu,  mcxu, mpru, nbft, nnbft,
     & mrcru, mstu, msu,   nblu, ng,   ns,   nst
      common /parmi/
     & mblu,  mccu, mconu, mcu,  mcxu, mpru, nbft, nnbft,
     & mrcru, mstu, msu,   nblu, ng,   ns,   nst
c
      common /ntgr/itl,itu,jtl,jtu,ktl,ktu,ltl,ltu,jcu,kcu,lcu,inx,
     1  jnx,knx,lnx,nwt,nwt1(3),ijsf(3),klsf(3),icxs(2,3),kcxs(2,3),
     2  npri(2,3),nprk(2,3),iesfb,kesfb,ircru,jrcru,krcru,lrcru
      common /cxindx/ ntij,ntik,ntil,ntkl,ntjl,ntjk,ntij1,ntjk1,ntkl1,
     1  ijcxst(2),ikcxst(2),ilcxst(2),klcxst(2),jlcxst(2),jkcxst(2)
      common /dim21/ ipq(256)
      common /ikpr/ npriri(2,3,8), nprirk(2,3,8)
      common /sf3ao/ ibl
      dimension a(*),cx(*),eta(mrcru,mconu,*),g4(*),icb(4,24,*),h4(*),
     1  ipt(*),ijgt(*),ijx(*),ijy(*),ijz(*),klgt(*),klx(*),kly(*),
     2  klz(*),lmnp1(*),lmnv(3,*),mcons(*),nc(*),ncon(*),nfct(*),ngw(*),
     3  nrcr(*),nt(*),ntl(*),ntu(*),x(mcu,*),y(mcu,*),z(mcu,*),
     4  zet(mconu,*)
      dimension icxbg(3,3), jbl(2,2), jbld(2), kcxbg(3,3)
c
      ic=icb(inx,1,icc)
      jc=icb(jnx,1,icc)
      kc=icb(knx,1,icc)
      lc=icb(lnx,1,icc)
      iceqjc=ic.eq.jc
      kceqlc=kc.eq.lc
      ngt=(nt(isf)*nt(ksf))**2
      if(iscmci.eq.2) go to 60
c
      do 16 jesfb=1,2
        do 14 lesfb=1,2
          jbl(jesfb,lesfb)=0
          do 12 ist=1,nst
            jbl(jesfb,lesfb)=jbl(jesfb,lesfb)+
     &       npriri(jesfb,1,ist)*nprirk(lesfb,1,ist)
   12     continue
          jbld(jesfb)=jbl(jesfb,2)-jbl(jesfb,1)
   14   continue
   16 continue
      jblii=ipq(krcru)*jbl(1,2)+krcru*jbl(1,1)
      jblij=ipq(krcru)*jbl(2,2)+krcru*jbl(2,1)
      jblid=jblij-jblii
      fnfct=(nwt*nfct(icc))
      icxbg(1,1)=ijcxst(1)+ipq((ic-1)*nt(isf)+1)*npri(1,1)+(jc-1)*ntij1
      icxbg(2,1)=ijcxst(2)+((ic-1)*nc(is)+(jc-1))*ntij
      icxbg(3,1)=icxbg(2,1)-(ic-jc)*(nc(is)-1)*ntij
      kcxbg(1,1)=klcxst(1)+ipq((kc-1)*nt(ksf)+1)*nprk(1,1)+(lc-1)*ntkl1
      kcxbg(2,1)=klcxst(2)+((kc-1)*nc(ks)+(lc-1))*ntkl
      kcxbg(3,1)=kcxbg(2,1)-(kc-lc)*(nc(ks)-1)*ntkl
      call jandk(a,eta,g4,ipt,is,isf,ic,is,isf,jc,ks,ksf,kc,ks,ksf,lc,
     1  ijgt,ijx,ijy,ijz,klgt,klx,kly,klz,lmnp1,lmnv,mcons,ncon,
     2  nrcr,nt,ntl,ntu,x,y,z,zet)
      ndx=1
      do 54 ircr=1,ircru
      if(iceqjc) jrcru=ircr
      do 52 jrcr=1,jrcru
      if(ircr-jrcr) 18, 20, 22
   18 iblkl=(ipq(jrcr)+(ircr-1))*jblij-(jrcr-1)*jblid
      iesfb=2
      icxbg1=icxbg(3,1)
      ngti=(nt(ksf)*nt(ksf))
      ngtj=(nt(isf)*(nt(ksf)*nt(ksf)))
      go to 26
   20 iblkl=ipq(ircr)*jblij+(ircr-1)*jblii
      iesfb=1
      go to 24
   22 iblkl=(ipq(ircr)+(jrcr-1))*jblij-(ircr-1)*jblid
      iesfb=2
   24 icxbg1=icxbg(iesfb,1)
      ngti=(nt(isf)*(nt(ksf)*nt(ksf)))
      ngtj=(nt(ksf)*nt(ksf))
   26 do 50 krcr=1,krcru
      if(kceqlc) lrcru=krcr
      do 48 lrcr=1,lrcru
      if(krcr-lrcr) 28, 30, 32
   28 iblb=iblkl+(ipq(lrcr)+(krcr-1))*jbl(iesfb,2)-(lrcr-1)*jbld(iesfb)
      kesfb=2
      kcxbg1=kcxbg(3,1)
      ngtk=1
      ngtl=nt(ksf)
      go to 36
   30 iblb=iblkl+ipq(krcr)*jbl(iesfb,2)+(krcr-1)*jbl(iesfb,1)
      kesfb=1
      go to 34
   32 iblb=iblkl+(ipq(krcr)+(lrcr-1))*jbl(iesfb,2)-(krcr-1)*jbld(iesfb)
      kesfb=2
   34 kcxbg1=kcxbg(kesfb,1)
      ngtk=nt(ksf)
      ngtl=1
   36 icxs(iesfb,1)=icxbg1
      ndxi=ndx
      do 46 it=itl,itu
        if(iceqjc.and.ircr.eq.jrcr) jtu = it
        ndxj=ndxi
        do 44 jt=itl,jtu
          kcxs(kesfb,1)=kcxbg1
          ndxk=ndxj
          do 42 kt=ktl,ktu
            if(kceqlc.and.krcr.eq.lrcr) ltu = kt
            ndxl=ndxk
            do 40 lt=ktl,ltu
              val=g4(ndxl)
              if(abs(val).le.cutoff) go to 38
              val=fnfct*val
              ibl=iblb
              call aoso2( h4, cx, 1, val )
   38         ndxl=ndxl+ngtl
              kcxs(kesfb,1)=kcxs(kesfb,1)+nprk(kesfb,1)
   40       continue
            ndxk=ndxk+ngtk
   42     continue
          ndxj=ndxj+ngtj
        icxs(iesfb,1)=icxs(iesfb,1)+npri(iesfb,1)
   44   continue
        ndxi=ndxi+ngti
   46 continue
      ndx=ndx+ngt
   48 continue
   50 continue
   52 continue
   54 continue
      return
c
   60 icxbg(2,2)=ikcxst(2)+((ic-1)*nc(ks)+(kc-1))*ntik
      icxbg(2,3)=icxbg(2,2)+(lc-kc)*ntik
      kcxbg(2,2)=icxbg(2,3)+(jc-ic)*nc(ks)*ntik
      kcxbg(2,3)=kcxbg(2,2)+(kc-lc)*ntik
      jbl1=0
      jbl2=0
      do 62 ist=1,nst
        jbl1=jbl1+ipq(npriri(2,2,ist)+1)
        jbl2=jbl2+npriri(2,2,ist)**2
   62 continue
      jbldi=jbl2-jbl1
      if(iceqjc.or.kceqlc) then
        fnfct=(nwt*nfct(icc))
      else
        do 64 igw=2,ngw(icc)
          if((icb(inx,igw,icc).eq.ic.and.
     &        icb(jnx,igw,icc).eq.jc).or.
     &       (icb(knx,igw,icc).eq.kc.and.
     &        icb(lnx,igw,icc).eq.lc)) go to 66
   64   continue
        iscmu=3
        fnfct=(nwt*nfct(icc))
        go to 68
   66   fnfct=(2*nwt*nfct(icc))
      endif
      iscmu=2
   68 do 98 iscm=2,iscmu
      if(iscm.eq.2) then
        call jandk(a,eta,g4,ipt,is,isf,ic,ks,ksf,kc,is,isf,jc,ks,ksf,lc,
     1    ijgt,ijx,ijy,ijz,klgt,klx,kly,klz,lmnp1,lmnv,mcons,ncon,
     2    nrcr,nt,ntl,ntu,x,y,z,zet)
      else
        call jandk(a,eta,g4,ipt,is,isf,ic,ks,ksf,lc,is,isf,jc,ks,ksf,kc,
     1    ijgt,ijx,ijy,ijz,klgt,klx,kly,klz,lmnp1,lmnv,mcons,ncon,
     2    nrcr,nt,ntl,ntu,x,y,z,zet)
      endif
      ndx=1
      ikrcr=0
      do 96 ircr=1,ircru
      if((iceqjc.and.kceqlc)) jrcru=ircr
      do 94 krcr=1,krcru
      ikrcr=ikrcr+1
      jlrcr=0
      do 92 jrcr=1,jrcru
      if(iceqjc.and.kceqlc) then
        if(ircr.eq.jrcr) then
          lrcru = krcr
        else
          lrcru = krcru
        endif
      endif
      do 90 lrcr=1,lrcru
      jlrcr=jlrcr+1
      if(ikrcr-jlrcr) 70, 72, 74
   70 iblb=(ipq(jlrcr)+(ikrcr-1))*jbl2-(jlrcr-1)*jbldi
      icxs(2,iscm)=kcxbg(2,iscm)
      kcxbg1=icxbg(2,iscm)
      ngtk=1
      ngtl=(nt(isf)*nt(ksf))
      go to 78
   72 iblb=(ipq(ikrcr+1)-1)*jbl2-(ikrcr-1)*jbldi
      go to 76
   74 iblb=(ipq(ikrcr)+(jlrcr-1))*jbl2-(ikrcr-1)*jbldi
   76 icxs(2,iscm)=icxbg(2,iscm)
      kcxbg1=kcxbg(2,iscm)
      ngtk=(nt(isf)*nt(ksf))
      ngtl=1
   78 ndxk=ndx
      do 88 it=itl,itu
        if(((iceqjc.and.kceqlc).and.ikrcr.eq.jlrcr)) jtu=it
        do 86 kt=ktl,ktu
          kcxs(2,iscm)=kcxbg1
          ndxl=ndxk
          do 84 jt=itl,jtu
            if((iceqjc.and.kceqlc).and.ikrcr.eq.jlrcr) then
              if(it.eq.jt) then
                ltu = kt
              else
                ltu = ktu
              endif
            endif
            do 82 lt=ktl,ltu
              val=g4(ndxl)
              if(abs(val).le.cutoff) go to 80
              val=fnfct*val
              ibl=iblb
              if(ikrcr.eq.jlrcr) then
                call aoso2e( h4, cx, iscm, val )
              else
                call aoso2( h4, cx, iscm, val )
              endif
   80         ndxl=ndxl+ngtl
              kcxs(2,iscm)=kcxs(2,iscm)+npri(2,2)
   82       continue
   84     continue
          ndxk=ndxk+ngtk
          icxs(2,iscm)=icxs(2,iscm)+npri(2,2)
   86   continue
   88 continue
      ndx=ndx+ngt
   90 continue
   92 continue
   94 continue
   96 continue
   98 continue
      return
      end
