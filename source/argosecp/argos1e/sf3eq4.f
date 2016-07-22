c
c  RCS $Revision: 1.1.1.1 $  $Date: 2001/04/09 08:15:53 $
c
*deck sf3eq4
      subroutine sf3eq4( a,        cx,       eta,      g4,
     &         h4,       icb,      icc,      ipt,      iscmci,
     &         is,       isf,      ks,       ksf,      ls,
     &         lsf,      ijgt,     ijx,      ijy,      ijz,
     &         klgt,     klx,      kly,      klz,      lmnp1,
     &         lmnv,     mcons,    nc,       ncon,     nfct,
     &         ngw,      nrcr,     nt,       ntl,      ntu,
     &         x,        y,        z,        zet )
      implicit real*8 (a-h,o-z)
      logical iceqjc
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
      dimension icxbg(3,3), jbl(2), kcxbg(3)
c
      ic=icb(inx,1,icc)
      jc=icb(jnx,1,icc)
      kc=icb(knx,1,icc)
      lc=icb(lnx,1,icc)
      iceqjc=ic.eq.jc
      do 12 jesfb=iscmci,2
        jbl(jesfb)=0
        do 10 ist=1,nst
          jbl(jesfb)=jbl(jesfb)+
     &      npriri(jesfb,iscmci,ist)*nprirk(2,iscmci,ist)
   10   continue
   12 continue
      if(iscmci.eq.2) go to 60
c
      jbl1=(krcru*lrcru)*jbl(1)
      jbl2=(krcru*lrcru)*jbl(2)
      jbld=jbl2-jbl1
      fnfct=(nwt*nfct(icc))
      icxbg(1,1)=ijcxst(1)+ipq((ic-1)*nt(isf)+1)*npri(1,1)+(jc-1)*ntij1
      icxbg(2,1)=ijcxst(2)+((ic-1)*nc(is)+(jc-1))*ntij
      icxbg(3,1)=icxbg(2,1)-(ic-jc)*(nc(is)-1)*ntij
      kcxbg(1)=klcxst(2)+((kc-1)*nc(ls)+(lc-1))*ntkl
      call jandk(a,eta,g4,ipt,is,isf,ic,is,isf,jc,ks,ksf,kc,ls,lsf,lc,
     1  ijgt,ijx,ijy,ijz,klgt,klx,kly,klz,lmnp1,lmnv,mcons,ncon,
     2  nrcr,nt,ntl,ntu,x,y,z,zet)
      ndx=1
      ngt=nt(isf)*(nt(isf)*(nt(ksf)*nt(lsf)))
      do 48 ircr=1,ircru
      if(iceqjc) jrcru=ircr
      do 46 jrcr=1,jrcru
      if(ircr-jrcr) 18, 20, 22
   18 iblb=(ipq(jrcr)+(ircr-1))*jbl2-(jrcr-1)*jbld
      iesfb=2
      icxbg1=icxbg(3,1)
      ngti=(nt(ksf)*nt(lsf))
      ngtj=(nt(isf)*(nt(ksf)*nt(lsf)))
      go to 26
   20 iblb=ipq(ircr)*jbl2+(ircr-1)*jbl1
      iesfb=1
      go to 24
   22 iblb=(ipq(ircr)+(jrcr-1))*jbl2-(ircr-1)*jbld
      iesfb=2
   24 icxbg1=icxbg(iesfb,1)
      ngti=(nt(isf)*(nt(ksf)*nt(lsf)))
      ngtj=(nt(ksf)*nt(lsf))
   26 do 44 krcr=1,krcru
      do 42 lrcr=1,lrcru
      icxs(iesfb,1)=icxbg1
      ndxi=ndx
      do 40 it=itl,itu
        if(iceqjc.and.ircr.eq.jrcr) jtu=it
        ndxj=ndxi
        do 38 jt=itl,jtu
          kcxs(2,1)=kcxbg(1)
          ndxl=ndxj
          do 36 kt=ktl,ktu
            do 34 lt=ltl,ltu
              val=g4(ndxl)
              if(abs(val).le.cutoff) go to 30
              val=fnfct*val
              ibl=iblb
              call aoso2( h4, cx, 1, val )
   30         ndxl=ndxl+1
              kcxs(2,1)=kcxs(2,1)+nprk(2,1)
   34       continue
   36     continue
          ndxj=ndxj+ngtj
          icxs(iesfb,1)=icxs(iesfb,1)+npri(iesfb,1)
   38   continue
        ndxi=ndxi+ngti
   40 continue
      iblb=iblb+jbl(iesfb)
      ndx=ndx+ngt
   42 continue
   44 continue
   46 continue
   48 continue
      return
c
   60 icxbg(2,2)=ikcxst(2)+((ic-1)*nc(ks)+(kc-1))*ntik
      icxbg(2,3)=icxbg(2,2)+(jc-ic)*nc(ks)*ntik
      kcxbg(3)=ilcxst(2)+((ic-1)*nc(ls)+(lc-1))*ntil
      kcxbg(2)=kcxbg(3)+(jc-ic)*nc(ls)*ntil
      if(iceqjc) then
        fnfct=(nwt*nfct(icc))
      else
        do 64 igw=2,ngw(icc)
          if(icb(knx,igw,icc).eq.kc.and.
     &       icb(lnx,igw,icc).eq.lc.and.
     &       icb(inx,igw,icc).eq.jc) go to 66
   64   continue
        iscmu=3
        fnfct=(nwt*nfct(icc))
        go to 68
   66   fnfct=(2*nwt*nfct(icc))
      endif
      iscmu=2
   68 do 98 iscm=2,iscmu
      if(iscm.eq.2) then
        call jandk(a,eta,g4,ipt,is,isf,ic,ks,ksf,kc,is,isf,jc,ls,lsf,lc,
     1    ijgt,ijx,ijy,ijz,klgt,klx,kly,klz,lmnp1,lmnv,mcons,ncon,
     2    nrcr,nt,ntl,ntu,x,y,z,zet)
      else
        call jandk(a,eta,g4,ipt,is,isf,jc,ks,ksf,kc,is,isf,ic,ls,lsf,lc,
     1    ijgt,ijx,ijy,ijz,klgt,klx,kly,klz,lmnp1,lmnv,mcons,ncon,
     2    nrcr,nt,ntl,ntu,x,y,z,zet)
      endif
      ndx=0
      iblb=0
      do 96 ircr=1,ircru
      do 94 krcr=1,krcru
      do 92 jrcr=1,ircru
      do 90 lrcr=1,lrcru
      icxs(2,iscm)=icxbg(2,iscm)
      do 88 it=itl,itu
        do 86 kt=ktl,ktu
          kcxs(2,iscm)=kcxbg(iscm)
          do 84 jt=itl,itu
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
