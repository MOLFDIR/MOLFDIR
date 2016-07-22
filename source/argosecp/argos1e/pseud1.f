c
c  RCS $Revision: 1.1.1.1 $  $Date: 2001/04/09 08:15:53 $
c
*deck pseud1
      subroutine pseud1(a,ang,ccr,crda,crdb,gout,ipt,lmnv,ltot1,ncr,
     &  nkcrl,nkcru,q,qsum,xab,yab,zab,zcr)
c
c  compute type 1 core potential integrals
c
c  03-jun-92 fujitsu compiler directive added to workaround
c            a compiler optimization bug (fix by Roger Edberg). -rls
c
      implicit real*8 (a-h,o-z)
      logical esf, esfc, igueq1, jgueq1
      parameter (a0=0.0d0)
      common /parmr/ cutoff, tol
      common /one/dxij,dyij,dzij,fnfct,rr,xij,xijm,yij,yijm,zij,zijm,
     1  ibl1,ibl2,icxi1,icxi2,ij,ijsf,ic,
     2  icons,igu,ircru,is,isf,itl,itu,jc,jcons,jgu,jrcru,js,jsf,jtl,
     3  jtu,lit,ljt,nblt1,nc2,nc1,nop,ntij1,ntij2,esf,esfc,igueq1,jgueq1
      common /callin/ xka,yka,zka,ca,xkb,ykb,zkb,cb,tai,taj,aa,taa,
     1  aarr1,aarr2,xk,yk,zk,fctr2,kcrs,lcru
      dimension a(*),ang(ltot1,*),ccr(*),crda(lit,3),crdb(ljt,3),
     &  gout(*),ipt(*),lmnv(3,*),ncr(*),nkcrl(6,*),nkcru(6,*),
     &  q(ltot1,*),qsum(ltot1,*),xab(*),yab(*),zab(*),zcr(*)
c     # should include kcr loop in recur1 and use recurrence relations
c     # on qsum
c
      rp2=xk*xk+yk*yk+zk*zk
      if(rp2.eq.a0) then
        rp=a0
        arp2=a0
        alpt=a0
        rk=a0
        lamu=1
      else
        rp=sqrt(rp2)
        xk=xk/rp
        yk=yk/rp
        zk=zk/rp
        arp2=aa*rp2
        alpt=aa*arp2
        rk=taa*rp
        lamu=ltot1
      endif
c
c  compute radial integrals and sum over potential terms
c
      call wzero((ltot1*lamu),qsum,1)
      kcrl=nkcrl(1,kcrs)
      kcru=nkcru(1,kcrs)
c     do 40 kcr=kcrl,kcru
c       alpha=aa+zcr(kcr)
c       # exponential factor from q functions included in dum
c       dum=aarr1+zcr(kcr)*arp2/alpha
c       if(dum.gt.tol) go to 40
c       prd=fctr2*ccr(kcr)*exp(-dum)
c       t=alpt/alpha
c       call recur1(alpha,a(ipt(11)),ncr(kcr),ltot1,q,t,rk)
c       do 30 lam=1,lamu
c         nhi=ltot1-mod(ltot1-lam,2)
c         do 20 n=lam,nhi,2
c           qsum(n,lam)=qsum(n,lam)+prd*q(n,lam)
c  20     continue
c  30   continue
c  40 continue
      call rad1(aa,aarr1,alpt,arp2,ccr,a(ipt(11)),fctr2,kcrl,kcru,
     &  lamu,ltot1,ncr,qsum,rk,tol,zcr)
c
      ijt=0
      do 90 it=itl,itu
        na1=lmnv(1,it)+1
        la1=lmnv(2,it)+1
        ma1=lmnv(3,it)+1
        do 80 jt=jtl,jtu
          ijt=ijt+1
          s=a0
          nb1=lmnv(1,jt)+1
          lb1=lmnv(2,jt)+1
          mb1=lmnv(3,jt)+1
c
c  compute angular integrals
c
          call facab(a(ipt(12)),na1,nb1,crda(1,1),crdb(1,1),xab)
          call facab(a(ipt(12)),la1,lb1,crda(1,2),crdb(1,2),yab)
          call facab(a(ipt(12)),ma1,mb1,crda(1,3),crdb(1,3),zab)
          call ang1(ang,a(ipt(11)),na1+nb1-1,la1+lb1-1,ma1+mb1-1,lamu,
     &      a(ipt(13)),a(ipt(14)),a(ipt(15)),a(ipt(16)),a(ipt(17)),
     &      ltot1,xab,yab,zab,xk,yk,zk,a(ipt(18)))
c
c  combine angular and radial integrals
c
          do 70 lam=1,lamu
            nhi=ltot1-mod(ltot1-lam,2)
*mdc*if fujitsu
*c           # need vocl here to force scalar execution - compiler bug??
*c           # 03-jun-92 Roger Edberg
**vocl loop,scalar
*mdc*endif
            do 60 n=lam,nhi,2
              s=s+ang(n,lam)*qsum(n,lam)
   60       continue
   70     continue
          gout(ijt)=gout(ijt)+s
   80   continue
   90 continue
      return
      end
