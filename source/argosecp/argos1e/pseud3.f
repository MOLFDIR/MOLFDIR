*deck pseud3
      subroutine pseud3( a, anga, angb, ccr, crda, crdb, flmtx, gout,
     &  ipt, lambu, ltot1, mc, mr, mproju, ncr, nklsl, nklsu, qsum,
     &  zcr )
c
c  computes spin-orbit potential integrals
c
      implicit real*8 (a-h,o-z)
      logical esf, esfc, igueq1, jgueq1
      parameter (a0=0.0d0)
c
      integer
     & mblu,  mccu, mconu, mcu,  mcxu, mpru, nbft, nnbft,
     & mrcru, mstu, msu,   nblu, ng,   ns,   nst
      common /parmi/
     & mblu,  mccu, mconu, mcu,  mcxu, mpru, nbft, nnbft,
     & mrcru, mstu, msu,   nblu, ng,   ns,   nst
c
      common /one/dxij,dyij,dzij,fnfct,rr,xij,xijm,yij,yijm,zij,zijm,
     1  ibl1,ibl2,icxi1,icxi2,ij,ijsf,ic,
     2  icons,igu,ircru,is,isf,itl,itu,jc,jcons,jgu,jrcru,js,jsf,jtl,
     3  jtu,lit,ljt,nblt1,nc2,nc1,nop,ntij1,ntij2,esf,esfc,igueq1,jgueq1
      common /callin/ xka,yka,zka,ca,xkb,ykb,zkb,cb,tai,taj,aa,taa,
     1  aarr1,aarr2,xk,yk,zk,fctr2,kcrs,lcru
      dimension a(*),anga(lit,mproju,*),angb(ljt,mproju,*),ccr(*),
     &  crda(lit,3),crdb(ljt,3),flmtx(3,*),gout(*),ipt(*),mc(3,*),
     &  mr(3,*),ncr(*),nklsl(4,*),nklsu(4,*),qsum(ltot1,lambu,*),
     &  zcr(*)
c
      if(ca.eq.a0) then
        rka=a0
        lmau=1
      else
        xka=-xka/ca
        yka=-yka/ca
        zka=-zka/ca
        rka=tai*ca
        lmau=lcru+(lit-1)
      endif
      if(cb.eq.a0) then
        rkb=a0
        lmbu=1
      else
        xkb=-xkb/cb
        ykb=-ykb/cb
        zkb=-zkb/cb
        rkb=taj*cb
        lmbu=lcru+(ljt-1)
      endif
      if((ca.eq.a0).and.(cb.eq.a0)) then
        lhi=min(lcru,lit,ljt)
        llo=mod(lit,2)+2
        if(llo.ne.(mod(ljt,2)+2).or.llo.gt.lhi) return
        inc=2
      elseif(ca.eq.a0) then
        lhi=min(lcru,lit)
        llo=mod(lit,2)+2
        if(llo.gt.lhi) return
        inc=2
      elseif(cb.eq.a0) then
        lhi=min(lcru,ljt)
        llo=mod(ljt,2)+2
        if(llo.gt.lhi) return
        inc=2
      else
        lhi=lcru
        llo=2
        inc=1
      endif
      do 88 l=llo,lhi,inc
      mhi=l+l-3
      lmalo=max(l-(lit-1),1)
      lmahi=min(lmau,l+(lit-1))
      lmblo=max(l-(ljt-1),1)
      lmbhi=min(lmbu,l+(ljt-1))
c
c  compute radial integrals
c
      kcrl=nklsl(l-1,kcrs)
      kcru=nklsu(l-1,kcrs)
            call rad2( a,          ccr,        ipt,        kcrl,
     &                 kcru,       l,          lambu,      lmahi,
     &                 lmalo,      lmbhi,      lmblo,      ltot1,
     &                 ncr,        qsum,       rka,        rkb,
     &                 zcr )
c
c  compute angular integrals and combine with radial integrals
c
      ijt=0
      do 84 it=itl,itu
      call ang2(anga,a(ipt(12)),crda,a(ipt(11)),it,l,lit,lmalo,lmahi,
     &  a(ipt(13)),a(ipt(14)),a(ipt(15)),a(ipt(16)),a(ipt(17)),
     &  a(ipt(1)),mproju,xka,yka,zka,a(ipt(18)))
      do 80 jt=jtl,jtu
      call ang2(angb,a(ipt(12)),crdb,a(ipt(11)),jt,l,ljt,lmblo,lmbhi,
     &  a(ipt(13)),a(ipt(14)),a(ipt(15)),a(ipt(16)),a(ipt(17)),
     &  a(ipt(1)),mproju,xkb,ykb,zkb,a(ipt(18)))
      do 76 lama=lmalo,lmahi
      ldifa1=abs(l-lama)+1
      nlmau=lit-mod(lit-ldifa1,2)
      do 72 lamb=lmblo,lmbhi
      ldifb=abs(l-lamb)
      nlmbu=(ljt-1)-mod((ljt-1)-ldifb,2)
      nlo=ldifa1+ldifb
      nhi=nlmau+nlmbu
      do 68 n=nlo,nhi,2
      nlmalo=max(ldifa1,n-nlmbu)
      nlmahi=min(nlmau,n-ldifb)
      do 64 kt=1,3
        s=a0
        do 60 m=1,mhi
          angp=a0
          do 56 nlma=nlmalo,nlmahi,2
            angp=angp+
     1        anga(nlma,mr(kt,m),lama)*angb((n+1)-nlma,mc(kt,m),lamb)-
     2        anga(nlma,mc(kt,m),lama)*angb((n+1)-nlma,mr(kt,m),lamb)
   56     continue
          s=s+angp*flmtx(kt,(l-2)**2+m)
   60   continue
        gout(ijt+kt)=gout(ijt+kt)+s*qsum(n,lamb,lama)
   64 continue
   68 continue
   72 continue
   76 continue
      ijt=ijt+3
   80 continue
   84 continue
   88 continue
      return
      end
