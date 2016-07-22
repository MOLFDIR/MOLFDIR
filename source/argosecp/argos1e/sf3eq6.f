*deck sf3eq6
      subroutine sf3eq6( a,        cx,       eta,      g4,
     &         h4,       icb,      icc,      ipt,      iscmci,
     &         is,       isf,      js,       jsf,      ls,
     &         lsf,      ijgt,     ijx,      ijy,      ijz,
     &         klgt,     klx,      kly,      klz,      lmnp1,
     &         lmnv,     mcons,    nc,       ncon,     nfct,
     &         ngw,      nrcr,     nt,       ntl,      ntu,
     &         x,        y,        z,        zet )
      implicit real*8 (a-h,o-z)
      logical jceqkc
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
      dimension icxbg(3),jbl(2), kcxbg(3,3)
c
      ic=icb(inx,1,icc)
      jc=icb(jnx,1,icc)
      kc=icb(knx,1,icc)
      lc=icb(lnx,1,icc)
      jceqkc=jc.eq.kc
      jesfbl=(5-iscmci)/2
      do 12 jesfb=jesfbl,2
        jbl(jesfb)=0
        do 10 ist=1,nst
          jbl(jesfb)=jbl(jesfb)+
     &      npriri(2,iscmci,ist)*nprirk(jesfb,iscmci,ist)
   10   continue
   12 continue
      if(iscmci.eq.3) go to 60
c
      icxbg(1)=ijcxst(2)+((ic-1)*nc(js)+(jc-1))*ntij
      icxbg(2)=icxbg(1)+(kc-jc)*ntij
      kcxbg(2,1)=klcxst(2)+((kc-1)*nc(ls)+(lc-1))*ntkl
      kcxbg(2,2)=kcxbg(2,1)+(jc-kc)*nc(ls)*ntkl
      if(jceqkc) then
        fnfct=(nwt*nfct(icc))
      else
        do 16 igw=2,ngw(icc)
          if(icb(inx,igw,icc).eq.ic.and.
     &       icb(jnx,igw,icc).eq.kc.and.
     &       icb(lnx,igw,icc).eq.lc) go to 24
   16   continue
        iscml=1
        fnfct=(nwt*nfct(icc))
        go to 28
   24   fnfct=(2*nwt*nfct(icc))
      endif
      iscml=2
   28 do 48 iscm=iscml,2
      if(iscm.eq.1) then
        call jandk(a,eta,g4,ipt,is,isf,ic,js,jsf,jc,js,jsf,kc,ls,lsf,lc,
     1    ijgt,ijx,ijy,ijz,klgt,klx,kly,klz,lmnp1,lmnv,mcons,ncon,
     2    nrcr,nt,ntl,ntu,x,y,z,zet)
      else
        call jandk(a,eta,g4,ipt,is,isf,ic,js,jsf,kc,js,jsf,jc,ls,lsf,lc,
     1    ijgt,ijx,ijy,ijz,klgt,klx,kly,klz,lmnp1,lmnv,mcons,ncon,
     2    nrcr,nt,ntl,ntu,x,y,z,zet)
      endif
      ndx=0
      iblb=0
      do 46 ircr=1,ircru
      do 44 jrcr=1,jrcru
      do 42 krcr=1,krcru
      do 40 lrcr=1,lrcru
      icxs(2,iscm)=icxbg(iscm)
      do 38 it=itl,itu
        do 36 jt=jtl,jtu
          kcxs(2,iscm)=kcxbg(2,iscm)
          do 34 kt=ktl,ktu
            do 32 lt=ltl,ltu
              ndx=ndx+1
              val=g4(ndx)
              if(abs(val).le.cutoff) go to 30
              val=fnfct*val
              ibl=iblb
              call aoso2( h4, cx, iscm, val )
   30         kcxs(2,iscm)=kcxs(2,iscm)+nprk(2,1)
   32       continue
   34     continue
          icxs(2,iscm)=icxs(2,iscm)+npri(2,1)
   36   continue
   38 continue
      iblb=iblb+jbl(2)
   40 continue
   42 continue
   44 continue
   46 continue
   48 continue
      return
c
   60 fnfct=(nwt*nfct(icc))
      jbld=jbl(2)-jbl(1)
      jbljk=ipq(jrcru)*jbl(2)+jrcru*jbl(1)
      icxbg(3)=ilcxst(2)+((ic-1)*nc(ls)+(lc-1))*ntil
      kcxbg(1,3)=jkcxst(1)+ipq((jc-1)*nt(jsf)+1)*nprk(1,3)+(kc-1)*ntjk1
      kcxbg(2,3)=jkcxst(2)+((jc-1)*nc(js)+(kc-1))*ntjk
      kcxbg(3,3)=kcxbg(2,3)-(jc-kc)*(nc(js)-1)*ntjk
      call jandk(a,eta,g4,ipt,is,isf,ic,ls,lsf,lc,js,jsf,jc,js,jsf,kc,
     1  ijgt,ijx,ijy,ijz,klgt,klx,kly,klz,lmnp1,lmnv,mcons,ncon,
     2  nrcr,nt,ntl,ntu,x,y,z,zet)
      ndxl=1
      ngtl=nt(jsf)**2
      iblbil=0
      do 98 ircr=1,ircru
      do 96 lrcr=1,lrcru
      do 94 jrcr=1,jrcru
      if(jceqkc) krcru=jrcr
      do 92 krcr=1,krcru
      if(jrcr-krcr) 70, 72, 74
   70 iblb=iblbil+(ipq(krcr)+(jrcr-1))*jbl(2)-(krcr-1)*jbld
      kesfb=2
      kcxbg1=kcxbg(3,3)
      ngtj=1
      ngtk=nt(jsf)
      go to 78
   72 iblb=iblbil+ipq(krcr)*jbl(2)+(krcr-1)*jbl(1)
      kesfb=1
      go to 76
   74 iblb=iblbil+(ipq(jrcr)+(krcr-1))*jbl(2)-(jrcr-1)*jbld
      kesfb=2
   76 kcxbg1=kcxbg(kesfb,3)
      ngtj=nt(jsf)
      ngtk=1
   78 icxs(2,3)=icxbg(3)
      do 88 it=itl,itu
        do 86 lt=ltl,ltu
          kcxs(kesfb,3)=kcxbg1
          ndxj=ndxl
          do 84 jt=jtl,jtu
            if(jrcr.eq.krcr.and.jceqkc) ktu=jt
            ndxk=ndxj
            do 82 kt=ktl,ktu
              val=g4(ndxk)
              if(abs(val).le.cutoff) go to 80
              val=fnfct*val
              ibl=iblb
              call aoso2( h4, cx, 3, val )
   80         ndxk=ndxk+ngtk
              kcxs(kesfb,3)=kcxs(kesfb,3)+nprk(kesfb,3)
   82       continue
            ndxj=ndxj+ngtj
   84     continue
          ndxl=ndxl+ngtl
          icxs(2,3)=icxs(2,3)+npri(2,3)
   86   continue
   88 continue
   92 continue
   94 continue
      iblbil=iblbil+jbljk
   96 continue
   98 continue
      return
      end
