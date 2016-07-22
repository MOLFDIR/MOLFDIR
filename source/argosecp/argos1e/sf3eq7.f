*deck sf3eq7
      subroutine sf3eq7( a,        cx,       eta,      g4,
     &         h4,       icb,      icc,      ipt,      iscmci,
     &         is,       isf,      js,       jsf,      ks,
     &         ksf,      ijgt,     ijx,      ijy,      ijz,
     &         klgt,     klx,      kly,      klz,      lmnp1,
     &         lmnv,     mcons,    nc,       ncon,     nfct,
     &         ngw,      nrcr,     nt,       ntl,      ntu,
     &         x,        y,        z,        zet )
      implicit real*8 (a-h,o-z)
      logical kceqlc
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
      dimension a(*),cx(*),eta(mrcru,mconu,*),g4(*),h4(*),icb(4,24,*),
     1  ipt(*),ijgt(*),ijx(*),ijy(*),ijz(*),klgt(*),klx(*),kly(*),
     2  klz(*),lmnp1(*),lmnv(3,*),mcons(*),nc(*),ncon(*),nfct(*),ngw(*),
     3  nrcr(*),nt(*),ntl(*),ntu(*),x(mcu,*),y(mcu,*),z(mcu,*),
     4  zet(mconu,*)
      dimension icxbg(3), jbl(2), kcxbg(3,3)
c
      ic=icb(inx,1,icc)
      jc=icb(jnx,1,icc)
      kc=icb(knx,1,icc)
      lc=icb(lnx,1,icc)
      kceqlc=kc.eq.lc
      do 12 jesfb=iscmci,2
      jbl(jesfb)=0
      do 10 ist=1,nst
        jbl(jesfb)=jbl(jesfb)+
     &    npriri(2,iscmci,ist)*nprirk(jesfb,iscmci,ist)
   10   continue
   12 continue
      if(iscmci.eq.2) go to 60
c
      jbld=jbl(2)-jbl(1)
      jblkl=ipq(krcru)*jbl(2)+krcru*jbl(1)
      fnfct=(nwt*nfct(icc))
      icxbg(1)=ijcxst(2)+((ic-1)*nc(js)+(jc-1))*ntij
      kcxbg(1,1)=klcxst(1)+ipq((kc-1)*nt(ksf)+1)*nprk(1,1)+(lc-1)*ntkl1
      kcxbg(2,1)=klcxst(2)+((kc-1)*nc(ks)+(lc-1))*ntkl
      kcxbg(3,1)=kcxbg(2,1)-(kc-lc)*(nc(ks)-1)*ntkl
      call jandk(a,eta,g4,ipt,is,isf,ic,js,jsf,jc,ks,ksf,kc,ks,ksf,lc,
     1  ijgt,ijx,ijy,ijz,klgt,klx,kly,klz,lmnp1,lmnv,mcons,ncon,
     2  nrcr,nt,ntl,ntu,x,y,z,zet)
      ndxj=1
      ngtj=nt(ksf)**2
      iblbij=0
      do 48 ircr=1,ircru
      do 46 jrcr=1,jrcru
      do 44 krcr=1,krcru
      if(kceqlc) lrcru=krcr
      do 42 lrcr=1,lrcru
      if(krcr-lrcr) 18, 20, 22
   18 iblb=iblbij+(ipq(lrcr)+(krcr-1))*jbl(2)-(lrcr-1)*jbld
      kesfb=2
      kcxbg1=kcxbg(3,1)
      ngtk=1
      ngtl=nt(ksf)
      go to 26
   20 iblb=iblbij+ipq(krcr)*jbl(2)+(krcr-1)*jbl(1)
      kesfb=1
      go to 24
   22 iblb=iblbij+(ipq(krcr)+(lrcr-1))*jbl(2)-(krcr-1)*jbld
      kesfb=2
   24 kcxbg1=kcxbg(kesfb,1)
      ngtk=nt(ksf)
      ngtl=1
   26 icxs(2,1)=icxbg(1)
      do 40 it=itl,itu
        do 38 jt=jtl,jtu
          kcxs(kesfb,1)=kcxbg1
          ndxk=ndxj
          do 36 kt=ktl,ktu
            if(krcr.eq.lrcr.and.kceqlc) ltu=kt
            ndxl=ndxk
            do 34 lt=ltl,ltu
              val=g4(ndxl)
              if(abs(val).le.cutoff) go to 30
              val=fnfct*val
              ibl=iblb
              call aoso2( h4, cx, 1, val )
   30         ndxl=ndxl+ngtl
              kcxs(kesfb,1)=kcxs(kesfb,1)+nprk(kesfb,1)
   34       continue
            ndxk=ndxk+ngtk
   36     continue
          ndxj=ndxj+ngtj
          icxs(2,1)=icxs(2,1)+npri(2,1)
   38   continue
   40 continue
      continue
   42 continue
   44 continue
      iblbij=iblbij+jblkl
   46 continue
   48 continue
      return
c
   60 icxbg(2)=ikcxst(2)+((ic-1)*nc(ks)+(kc-1))*ntik
      icxbg(3)=icxbg(2)+(lc-kc)*ntik
      kcxbg(2,3)=jkcxst(2)+((jc-1)*nc(ks)+(kc-1))*ntjk
      kcxbg(2,2)=kcxbg(2,3)+(lc-kc)*ntjk
      if(kceqlc) then
        fnfct=(nwt*nfct(icc))
      else
        do 64 igw=2,ngw(icc)
          if(icb(inx,igw,icc).eq.ic.and.
     &       icb(jnx,igw,icc).eq.jc.and.
     &       icb(knx,igw,icc).eq.lc) go to 66
   64   continue
        iscmu = 3
        fnfct=(nwt*nfct(icc))
        go to 68
   66   fnfct=(2*nwt*nfct(icc))
      endif
      iscmu = 2
   68 do 98 iscm=2,iscmu
      if(iscm.eq.2) then
        call jandk(a,eta,g4,ipt,is,isf,ic,ks,ksf,kc,js,jsf,jc,ks,ksf,lc,
     1    ijgt,ijx,ijy,ijz,klgt,klx,kly,klz,lmnp1,lmnv,mcons,ncon,
     2    nrcr,nt,ntl,ntu,x,y,z,zet)
      else
        call jandk(a,eta,g4,ipt,is,isf,ic,ks,ksf,lc,js,jsf,jc,ks,ksf,kc,
     1    ijgt,ijx,ijy,ijz,klgt,klx,kly,klz,lmnp1,lmnv,mcons,ncon,
     2    nrcr,nt,ntl,ntu,x,y,z,zet)
      endif
      ndx=0
      iblb=0
      do 96 ircr=1,ircru
      do 94 krcr=1,krcru
      do 92 jrcr=1,jrcru
      do 90 lrcr=1,lrcru
      icxs(2,iscm)=icxbg(iscm)
      do 88 it=itl,itu
        do 86 kt=ktl,ktu
          kcxs(2,iscm)=kcxbg(2,iscm)
          do 84 jt=jtl,jtu
            do 82 lt=ltl,ltu
              ndx=ndx+1
              val=g4(ndx)
              if(abs(val).le.cutoff) go to 80
              val=fnfct*val
              ibl=iblb
              call aoso2( h4, cx, iscm, val )
   80         kcxs(2,iscm)=kcxs(2,iscm)+nprk(2,2)
   82       continue
   84     continue
          icxs(2,iscm)=icxs(2,iscm)+npri(2,2)
   86   continue
   88 continue
      iblb=iblb+jbl(2)
   90 continue
   92 continue
   94 continue
   96 continue
   98 continue
      return
      end
