*deck sf3eq5
      subroutine sf3eq5( a,        cx,       eta,      g4,
     &         h4,       icb,      icc,      ipt,      is,
     &         isf,      js,       jsf,      ijgt,     ijx,
     &         ijy,      ijz,      klgt,     klx,      kly,
     &         klz,      lmnp1,    lmnv,     mcons,    nc,
     &         ncon,     nfct,     ngw,      nrcr,     nt,
     &         ntl,      ntu,      x,        y,        z,
     &         zet )
      implicit real*8 (a-h,o-z)
      logical ec
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
      dimension ec(3), icxbg(3), jbl(2), kcxbg(3,3)
c
      ic=icb(inx,1,icc)
      jc=icb(jnx,1,icc)
      kc=icb(knx,1,icc)
      lc=icb(lnx,1,icc)
      icxbg(1)=ijcxst(2)+((ic-1)*nc(js)+(jc-1))*ntij
      icxbg(2)=icxbg(1)+(kc-jc)*ntij
      icxbg(3)=icxbg(2)+(lc-kc)*ntij
      kcxbg(1,1)=jkcxst(1)+ipq((kc-1)*nt(jsf)+1)*nprk(1,1)+(lc-1)*ntjk1
      kcxbg(1,3)=jkcxst(1)+ipq((jc-1)*nt(jsf)+1)*nprk(1,1)+(kc-1)*ntjk1
      kcxbg(1,2)=kcxbg(1,3)+(lc-kc)*ntjk1
      kcxbg(2,1)=jkcxst(2)+((kc-1)*nc(js)+(lc-1))*ntjk
      kcxbg(3,1)=kcxbg(2,1)-(kc-lc)*((nc(js)-1)*ntjk)
      kcxbg(2,2)=kcxbg(2,1)+(jc-kc)*nc(js)*ntjk
      kcxbg(3,2)=kcxbg(2,2)-(jc-lc)*((nc(js)-1)*ntjk)
      kcxbg(2,3)=kcxbg(2,2)+(kc-lc)*ntjk
      kcxbg(3,3)=kcxbg(2,3)-(jc-kc)*((nc(js)-1)*ntjk)
      ec(1)=kc.eq.lc
      ec(2)=jc.eq.lc
      ec(3)=jc.eq.kc
      do 12 jesfb=1,2
        jbl(jesfb)=0
        do 10 ist=1,nst
          jbl(jesfb)=jbl(jesfb)+npriri(2,1,ist)*nprirk(jesfb,1,ist)
   10   continue
   12 continue
      jbld=jbl(2)-jbl(1)
      jblkl=ipq(jrcru)*jbl(2)+jrcru*jbl(1)
      if(ec(1)) then
        nwt1(1) = 1
        if(ec(3)) then
          iscmu = 1
          go to 48
        else
          iscmu = 2
          go to 44
        endif
      endif
      if(ec(3)) then
        iscml = 2
        iscmu = 3
        nwt1(2) = 1
        go to 52
      endif
      if(ngw(icc).eq.1) go to 40
      isw2 = 2
      isw3 = 3
      do 24 igw=2,ngw(icc)
        if(icb(inx,igw,icc).ne.ic) go to 24
        if (icb(knx,igw,icc) .eq. jc) isw2 = 1
        if (icb(lnx,igw,icc) .eq. kc) isw3 = min(isw3,2)
        if (icb(lnx,igw,icc) .eq. jc) isw3 = 1
   24 continue
      if(isw3.eq.1) then
        if(isw2.eq.1) then
          iscmu = 1
          nwt1(1) = 3
          go to 48
        else
          iscmu = 2
          nwt1(1) = 2
          go to 44
        endif
      elseif(isw3.eq.2) then
        iscmu = 2
        nwt1(1) = 1
        nwt1(2) = 2
        go to 48
      elseif(isw2.eq.1) then
        iscml = 2
        iscmu = 3
        nwt1(2) = 2
        go to 52
      endif
   40 iscmu = 3
      nwt1(1) = 1
   44 nwt1(2) = 1
   48 iscml = 1
   52 nwt2 = nwt * nfct(icc)
      ngtj=nt(jsf)**2
      do 98 iscm=iscml,iscmu
      fnfct=nwt1(iscm)*nwt2
      if(iscm-2) 56, 60, 64
   56 call jandk(a,eta,g4,ipt,is,isf,ic,js,jsf,jc,js,jsf,kc,js,jsf,lc,
     1  ijgt,ijx,ijy,ijz,klgt,klx,kly,klz,lmnp1,lmnv,mcons,ncon,
     2  nrcr,nt,ntl,ntu,x,y,z,zet)
      go to 68
   60 call jandk(a,eta,g4,ipt,is,isf,ic,js,jsf,kc,js,jsf,jc,js,jsf,lc,
     1  ijgt,ijx,ijy,ijz,klgt,klx,kly,klz,lmnp1,lmnv,mcons,ncon,
     2  nrcr,nt,ntl,ntu,x,y,z,zet)
      go to 68
   64 call jandk(a,eta,g4,ipt,is,isf,ic,js,jsf,lc,js,jsf,jc,js,jsf,kc,
     1  ijgt,ijx,ijy,ijz,klgt,klx,kly,klz,lmnp1,lmnv,mcons,ncon,
     2  nrcr,nt,ntl,ntu,x,y,z,zet)
   68 ndxj=1
      iblbij=0
      do 96 ircr=1,ircru
      do 94 jrcr=1,jrcru
      do 92 krcr=1,jrcru
      if(ec(iscm)) lrcru=krcr
      do 90 lrcr=1,lrcru
      if(krcr-lrcr) 70, 72, 74
   70 iblb=iblbij+(ipq(lrcr)+(krcr-1))*jbl(2)-(lrcr-1)*jbld
      kesfb=2
      kcxbg1=kcxbg(3,iscm)
      ngtk=1
      ngtl=nt(jsf)
      go to 78
   72 iblb=iblbij+ipq(krcr)*jbl(2)+(krcr-1)*jbl(1)
      kesfb=1
      go to 76
   74 iblb=iblbij+(ipq(krcr)+(lrcr-1))*jbl(2)-(krcr-1)*jbld
      kesfb=2
   76 kcxbg1=kcxbg(kesfb,iscm)
      ngtk=nt(jsf)
      ngtl=1
   78 icxs(2,iscm)=icxbg(iscm)
      do 88 it=itl,itu
        do 86 jt=jtl,jtu
          kcxs(kesfb,iscm)=kcxbg1
          ndxk=ndxj
          do 84 kt=jtl,jtu
            if(krcr.eq.lrcr.and.ec(iscm)) ltu=kt
            ndxl=ndxk
            do 82 lt=jtl,ltu
              val=g4(ndxl)
              if(abs(val).le.cutoff) go to 80
              val=fnfct*val
              ibl=iblb
              call aoso2( h4, cx, iscm, val )
   80         ndxl=ndxl+ngtl
              kcxs(kesfb,iscm)=kcxs(kesfb,iscm)+nprk(kesfb,1)
   82       continue
            ndxk=ndxk+ngtk
   84     continue
          ndxj=ndxj+ngtj
          icxs(2,iscm)=icxs(2,iscm)+npri(2,1)
   86   continue
   88 continue
   90 continue
   92 continue
      iblbij=iblbij+jblkl
   94 continue
   96 continue
   98 continue
      return
      end
