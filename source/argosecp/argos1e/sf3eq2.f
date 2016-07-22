*deck sf3eq2
      subroutine sf3eq2( a,        cx,       eta,      g4,
     &         h4,       icb,      icc,      ipt,      is,
     &         isf,      ls,       lsf,      ijgt,     ijx,
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
      dimension ec(3), icxbg(3,3), jbl(2), kcxbg(3)
c
      ic=icb(inx,1,icc)
      jc=icb(jnx,1,icc)
      kc=icb(knx,1,icc)
      lc=icb(lnx,1,icc)
      icxbg(1,1)=ijcxst(1)+ipq((ic-1)*nt(isf)+1)*npri(1,1)+(jc-1)*ntij1
      icxbg(1,2)=icxbg(1,1)+(kc-jc)*ntij1
      kcxbg(3)=ilcxst(2)+((ic-1)*nc(ls)+(lc-1))*ntil
      kcxbg(1)=kcxbg(3)+(kc-ic)*(nc(ls)*ntil)
      kcxbg(2)=kcxbg(1)+(jc-kc)*(nc(ls)*ntil)
      icxbg(1,3)=ijcxst(1)+ipq((jc-1)*nt(isf)+1)*npri(1,1)+(kc-1)*ntij1
      icxbg(2,1)=ijcxst(2)+((ic-1)*nc(is)+(jc-1))*ntij
      icxbg(3,1)=icxbg(2,1)-(ic-jc)*((nc(is)-1)*ntij)
      icxbg(2,2)=icxbg(2,1)+(kc-jc)*ntij
      icxbg(3,2)=icxbg(2,2)-(ic-kc)*((nc(is)-1)*ntij)
      icxbg(2,3)=icxbg(2,2)+(jc-ic)*nc(is)*ntij
      icxbg(3,3)=icxbg(2,3)-(jc-kc)*((nc(is)-1)*ntij)
      ec(1)=ic.eq.jc
      ec(3)=jc.eq.kc
      ec(2)=ic.eq.kc
      do 12 jesfb=1,2
        jbl(jesfb)=0
        do 10 ist=1,nst
          jbl(jesfb)=jbl(jesfb)+npriri(jesfb,1,ist)*nprirk(2,1,ist)
   10   continue
   12 continue
      jbl1=(ircru*lrcru)*jbl(1)
      jbl2=(ircru*lrcru)*jbl(2)
      jbld=jbl2-jbl1
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
        if(icb(lnx,igw,icc).ne.lc) go to 24
        if(icb(jnx,igw,icc).eq.kc) isw2=1
        if(icb(inx,igw,icc).eq.jc) isw3=min(isw3,2)
        if(icb(inx,igw,icc).eq.kc) isw3=1
   24 continue
      if(isw3.eq.1) then
        if(isw2.eq.1) then
          iscmu = 1
          nwt1 (1) = 3
          go to 48
        else
          iscmu = 2
          nwt1(1) = 2
          go to 44
        endif
      endif
      if(isw3.eq.2) then
        iscmu = 2
        nwt1(1) = 1
        nwt1(2) = 2
        go to 48
      endif
      if(isw2.eq.1) then
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
      ngt=nt(isf)*(nt(isf)*(nt(isf)*nt(lsf)))
      do 98 iscm=iscml,iscmu
      fnfct=nwt1(iscm)*nwt2
      if(iscm-2) 56, 60, 64
   56 call jandk(a,eta,g4,ipt,is,isf,ic,is,isf,jc,is,isf,kc,ls,lsf,lc,
     1  ijgt,ijx,ijy,ijz,klgt,klx,kly,klz,lmnp1,lmnv,mcons,ncon,
     2  nrcr,nt,ntl,ntu,x,y,z,zet)
      go to 68
   60 call jandk(a,eta,g4,ipt,is,isf,ic,is,isf,kc,is,isf,jc,ls,lsf,lc,
     1  ijgt,ijx,ijy,ijz,klgt,klx,kly,klz,lmnp1,lmnv,mcons,ncon,
     2  nrcr,nt,ntl,ntu,x,y,z,zet)
      go to 68
   64 call jandk(a,eta,g4,ipt,is,isf,jc,is,isf,kc,is,isf,ic,ls,lsf,lc,
     1  ijgt,ijx,ijy,ijz,klgt,klx,kly,klz,lmnp1,lmnv,mcons,ncon,
     2  nrcr,nt,ntl,ntu,x,y,z,zet)
   68 ndx=1
      do 96 ircr=1,ircru
      if(ec(iscm)) jrcru=ircr
      do 94 jrcr=1,jrcru
      if(ircr-jrcr) 70, 72, 74
   70 iblb=(ipq(jrcr)+(ircr-1))*jbl2-(jrcr-1)*jbld
      iesfb=2
      icxbg1=icxbg(3,iscm)
      ngti=(nt(isf)*nt(lsf))
      ngtj=(nt(isf)*(nt(isf)*nt(lsf)))
      go to 78
   72 iblb=ipq(ircr)*jbl2+(ircr-1)*jbl1
      iesfb=1
      go to 76
   74 iblb=(ipq(ircr)+(jrcr-1))*jbl2-(ircr-1)*jbld
      iesfb=2
   76 icxbg1=icxbg(iesfb,iscm)
      ngti=(nt(isf)*(nt(isf)*nt(lsf)))
      ngtj=(nt(isf)*nt(lsf))
   78 do 92 krcr=1,ircru
      do 90 lrcr=1,lrcru
      icxs(iesfb,iscm)=icxbg1
      ndxi=ndx
      do 88 it=itl,itu
        if(ircr.eq.jrcr.and.ec(iscm)) jtu = it
        ndxj=ndxi
        do 86 jt=itl,jtu
          kcxs(2,iscm)=kcxbg(iscm)
          ndxl=ndxj
          do 84 kt=itl,itu
            do 82 lt=ltl,ltu
              val=g4(ndxl)
              if(abs(val).le.cutoff) go to 80
              val=fnfct*val
              ibl=iblb
              call aoso2( h4, cx, iscm, val )
   80         ndxl=ndxl+1
              kcxs(2,iscm)=kcxs(2,iscm)+nprk(2,1)
   82       continue
   84     continue
          ndxj=ndxj+ngtj
          icxs(iesfb,iscm)=icxs(iesfb,iscm)+npri(iesfb,1)
   86   continue
        ndxi=ndxi+ngti
   88 continue
      iblb=iblb+jbl(iesfb)
      ndx=ndx+ngt
   90 continue
   92 continue
   94 continue
   96 continue
   98 continue
      return
      end
