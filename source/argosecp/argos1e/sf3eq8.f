*deck sf3eq8
      subroutine sf3eq8( a,        cx,       eta,      g4,
     &         h4,       icb,      icc,      ipt,      iscm,
     &         is,       isf,      js,       jsf,      ks,
     &         ksf,      ls,       lsf,      ijgt,     ijx,
     &         ijy,      ijz,      klgt,     klx,      kly,
     &         klz,      lmnp1,    lmnv,     mcons,    nc,
     &         ncon,     nfct,     nrcr,     nt,       ntl,
     &         ntu,      x,        y,        z,        zet )
      implicit real*8 (a-h,o-z)
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
      common /ikpr/ npriri(2,3,8), nprirk(2,3,8)
      common /sf3ao/ ibl
      dimension a(*),cx(*),eta(mrcru,mconu,*),g4(*),h4(*),icb(4,24,*),
     1  ipt(*),ijgt(*),ijx(*),ijy(*),ijz(*),klgt(*),klx(*),kly(*),
     2  klz(*),lmnp1(*),lmnv(3,*),mcons(*),nc(*),ncon(*),nfct(*),
     3  nrcr(*),nt(*),ntl(*),ntu(*),x(mcu,*),y(mcu,*),z(mcu,*),
     4  zet(mconu,*)
c
      fnfct=nwt*nfct(icc)
      ic=icb(inx,1,icc)
      jc=icb(jnx,1,icc)
      kc=icb(knx,1,icc)
      lc=icb(lnx,1,icc)
      jbl2=0
      do 10 ist=1,nst
        jbl2=jbl2+npriri(2,iscm,ist)*nprirk(2,iscm,ist)
   10 continue
      ndx=0
      iblb=0
      if(iscm-2) 18, 48, 78
c
   18 icxbg=ijcxst(2)+((ic-1)*nc(js)+(jc-1))*ntij
      kcxbg=klcxst(2)+((kc-1)*nc(ls)+(lc-1))*ntkl
      call jandk(a,eta,g4,ipt,is,isf,ic,js,jsf,jc,ks,ksf,kc,ls,lsf,lc,
     1  ijgt,ijx,ijy,ijz,klgt,klx,kly,klz,lmnp1,lmnv,mcons,ncon,
     2  nrcr,nt,ntl,ntu,x,y,z,zet)
      do 38 ircr=1,ircru
      do 36 jrcr=1,jrcru
      do 34 krcr=1,krcru
      do 32 lrcr=1,lrcru
      icxs(2,1)=icxbg
      do 30 it=itl,itu
        do 28 jt=jtl,jtu
          kcxs(2,1)=kcxbg
          do 26 kt=ktl,ktu
            do 24 lt=ltl,ltu
              ndx=ndx+1
              val=g4(ndx)
              if(abs(val).le.cutoff) go to 20
              val=fnfct*val
              ibl=iblb
              call aoso2( h4, cx, 1, val )
   20         kcxs(2,1)=kcxs(2,1)+nprk(2,1)
   24       continue
   26     continue
          icxs(2,1)=icxs(2,1)+npri(2,1)
   28   continue
   30 continue
      iblb=iblb+jbl2
   32 continue
   34 continue
   36 continue
   38 continue
      return
c
   48 icxbg=ikcxst(2)+((ic-1)*nc(ks)+(kc-1))*ntik
      kcxbg=jlcxst(2)+((jc-1)*nc(ls)+(lc-1))*ntjl
      call jandk(a,eta,g4,ipt,is,isf,ic,ks,ksf,kc,js,jsf,jc,ls,lsf,lc,
     1  ijgt,ijx,ijy,ijz,klgt,klx,kly,klz,lmnp1,lmnv,mcons,ncon,
     2  nrcr,nt,ntl,ntu,x,y,z,zet)
      do 68 ircr=1,ircru
      do 66 krcr=1,krcru
      do 64 jrcr=1,jrcru
      do 62 lrcr=1,lrcru
      icxs(2,2)=icxbg
      do 60 it=itl,itu
        do 58 kt=ktl,ktu
          kcxs(2,2)=kcxbg
          do 56 jt=jtl,jtu
            do 54 lt=ltl,ltu
              ndx=ndx+1
              val=g4(ndx)
              if(abs(val).le.cutoff) go to 50
              val=fnfct*val
              ibl=iblb
              call aoso2( h4, cx, 2, val )
   50         kcxs(2,2)=kcxs(2,2)+nprk(2,2)
   54       continue
   56     continue
          icxs(2,2)=icxs(2,2)+npri(2,2)
   58   continue
   60 continue
      iblb=iblb+jbl2
   62 continue
   64 continue
   66 continue
   68 continue
      return
c
   78 icxbg=ilcxst(2)+((ic-1)*nc(ls)+(lc-1))*ntil
      kcxbg=jkcxst(2)+((jc-1)*nc(ks)+(kc-1))*ntjk
      call jandk(a,eta,g4,ipt,is,isf,ic,ls,lsf,lc,js,jsf,jc,ks,ksf,kc,
     1  ijgt,ijx,ijy,ijz,klgt,klx,kly,klz,lmnp1,lmnv,mcons,ncon,
     2  nrcr,nt,ntl,ntu,x,y,z,zet)
      do 98 ircr=1,ircru
      do 96 lrcr=1,lrcru
      do 94 jrcr=1,jrcru
      do 92 krcr=1,krcru
      icxs(2,3)=icxbg
      do 90 it=itl,itu
        do 88 lt=ltl,ltu
          kcxs(2,3)=kcxbg
          do 86 jt=jtl,jtu
            do 84 kt=ktl,ktu
              ndx=ndx+1
              val=g4(ndx)
              if(abs(val).le.cutoff) go to 80
              val=fnfct*val
              ibl=iblb
              call aoso2( h4, cx, 3, val )
   80         kcxs(2,3)=kcxs(2,3)+nprk(2,3)
   84       continue
   86     continue
          icxs(2,3)=icxs(2,3)+npri(2,3)
   88   continue
   90 continue
      iblb=iblb+jbl2
   92 continue
   94 continue
   96 continue
   98 continue
      return
      end
