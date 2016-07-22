*deck dnode
      subroutine dnode(k,a,rt)
c
c  return in rt(i) the ith root of a polynomial
c  of order k whose mth coefficient is stored in a(m+1).
c  the initial values in rt must bracket the final values.
c
*mdc*if real16 vax cray ibm
*      implicit real*16 (a-h,o-z)
*mdc*else
      implicit real*8 (a-h,o-z)
*mdc*endif
*mdc*if real16q ibm vax
*c *** this mdc block is not necessary since the constants will be ***
*c *** correctly converted at compile time.  should it be removed? ***
*      parameter (a0=0.0q0, a1s16=6.25q-2, a1s4=0.25q0, a3s4=0.75q0)
*mdc*else
      parameter (a0=0.0d0, a1s16=6.25d-2, a1s4=0.25d0, a3s4=0.75d0)
*mdc*endif
*mdc*if cray
*      parameter (accrt=1.0d-21)
*mdc*elseif real16q vax ibm rs6000 fujitsu
*      parameter (accrt=1.0q-21)
*mdc*else
      parameter (accrt=1.0d-12)
*mdc*endif
c
      integer   nunits
      parameter(nunits=4)
      integer        iunits
      common /units/ iunits(nunits)
      integer                  nlist
      equivalence ( iunits(1), nlist )
c
c     common /ffm/ x,nn,n1,k
      dimension a(*), rt(*)
c
c     # bummer error types.
      integer    wrnerr,  nfterr,  faterr
      parameter (wrnerr=0,nfterr=1,faterr=2)
c
      k1=k+1
      r2=a0
      p2=a(1)
      do 95 m=1,k
      r1=r2
      p1=p2
      r2=rt(m)
      p2=a(k1)
      do 10 i=1,k
        p2=p2*r2+a(k1-i)
   10 continue
      prod=p1*p2
      if(prod.ge.a0) then
        write (nlist,15) m,k
   15   format(/' dnode: root number',i4,
     +    ' was not found for polynomial of order',i4//)
        call bummer('dnode: root not found, m=',m,faterr)
      endif
c
      r5=r1
      p5=p1
      r6=r2
      p6=p2
   30 r3=r5
      p3=p5
      r4=r6
      p4=p6
      r =(r3*p4-r4*p3)/(p4-p3)
      dr=r4-r3
      delta=dr
      dr = a1s16 * dr
      r5=r-dr
      r6=r+dr
      if(abs(delta).lt.accrt.or.r5.eq.r.or.r6.eq.r) go to 90
      if(r5.lt.r3) r5=r3
      if(r6.gt.r4) r6=r4
      p5=a(k1)
      p6=p5
      do 40 i=1,k
        p5=p5*r5+a(k1-i)
        p6=p6*r6+a(k1-i)
   40 continue
   45 prod=p5*p6
      if(prod.lt.a0) go to 30
      prod=p3*p5
      if(prod.gt.a0) go to 60
      r5=a1s4*r3+a3s4*r5
      p5=a(k1)
      do 50 i=1,k
        p5=p5*r5+a(k1-i)
   50 continue
      go to 45
   60 r6=a1s4*r4+a3s4*r6
      p6=a(k1)
      do 70 i=1,k
        p6=p6*r6+a(k1-i)
   70 continue
      go to 45
   90 rt(m)=r
   95 continue
      return
      end
