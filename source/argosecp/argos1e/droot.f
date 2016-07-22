c
c  RCS $Revision: 1.1.1.1 $  $Date: 2001/04/09 08:15:53 $
c
*deck droot
      subroutine droot(nroots,n1,xx,ff,c,s,a,rt,r,w,uf,wf)
c
c  ith root of the jth rys polynomial is returned in r(i,j)
c  with the corresponding weight factor in w(i,j).  j=1,2,...,n
c  this version uses christoffel formula for weights.
c
*mdc*if real16 vax ibm cray
*      implicit real*16 (a-h,o-z)
*      real*8 xx,uf,wf
*mdc*else
      implicit real*8 (a-h,o-z)
      real*8 xx,uf,wf
*mdc*endif
*mdc*if real16q ibm vax rs6000 fujitsu
*c *** this mdc block is not necessary since the constants will be ***
*c *** correctly converted at compile time.  should it be removed? ***
*      parameter (a0=0.0q0, a1s2=0.5q0, a1=1.0q0, a4=4.0q0)
*mdc*else
      parameter (a0=0.0d0, a1s2=0.5d0, a1=1.0d0, a4=4.0d0)
*mdc*endif
c     common /ffm/ x,nn,n1,k
c     dimension uf(*), wf(*)
c     dimension ff(19), c(10,10), s(10,10), a(10), rt(10),
c    &  r(9,9), w(9,9)
      dimension ff(*), c(n1,*), s(n1,*), a(*), rt(*),
     &  r(nroots,*), w(nroots,*), uf(*), wf(*)
c
      n=max(nroots,2)
      x=xx
c     n1=n+1
      nn=n+n
      call dfunc(nn,x,ff,c)
      do 15 i=1,n1
        do 10 j=1,n1
          s(i,j)=ff(i+j-1)
   10   continue
   15 continue
      call dsmit(n1,c,s,a,rt)
      do 25 i=1,n
        do 20 j=1,i
          w(i,j)=a0
          r(i,j)=a0
   20   continue
   25 continue
      wsum=ff(1)
      w(1,1)=wsum
      r(1,1)=ff(2)/wsum
      dum=sqrt(c(2,3)**2-a4*c(1,3)*c(3,3))
      r(1,2)=a1s2*(-c(2,3)-dum)/c(3,3)
      r(2,2)=a1s2*(-c(2,3)+dum)/c(3,3)
      if(n.eq.2) go to 50
      do 30 i=3,n1
        rt(i)=a1
   30 continue
      rt(1)=r(1,2)
      rt(2)=r(2,2)
      do 45 k=3,n
        k1=k+1
        do 35 i=1,k1
          a(i)=c(i,k1)
   35   continue
        call dnode(k,a,rt)
        do 40 i=1,k
          r(i,k)=rt(i)
   40   continue
   45 continue
   50 continue
      do 70 k=2,n
        do 65 i=1,k
          root=r(i,k)
          dum=a1/ff(1)
          do 60 j=1,k-1
            j1=j+1
            poly=c(j1,j1)
            do 55 m=1,j
              poly=poly*root+c(j1-m,j1)
   55       continue
            dum=dum+poly*poly
   60     continue
          w(i,k)=a1/dum
   65   continue
   70 continue
      do 75 k=1,nroots
        dum=r(k,nroots)
        uf(k)=dum/(a1-dum)
        wf(k)=w(k,nroots)
   75 continue
      return
      end
