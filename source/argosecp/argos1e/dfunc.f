*deck dfunc
      subroutine dfunc(nn,x,ff,term)
*mdc*if real16 vax cray ibm
*      implicit real*16 (a-h,o-z)
*mdc*else
      implicit real*8 (a-h,o-z)
*mdc*endif
*mdc*if real16q ibm vax rs6000 fujitsu
*      parameter (a0=0.0q0, a1s2=0.5q0, a1=1.0q0 )
*      parameter (pie4=0.7853981633974483096156608 q0)
*mdc*else
      parameter (a0=0.0d0, a1s2=0.5d0, a1=1.0d0)
      parameter (pie4=0.7853981633974483096156608 d0)
*mdc*endif
*mdc*if fps164
*      parameter (xsw=19.0d0)
*mdc*elseif cray
*      parameter (xsw=33.0d0)
*mdc*elseif real16q vax ibm rs6000 fujitsu
*      parameter (xsw=39.0q0)
*mdc*else
      parameter (xsw=15.0d0)
*mdc*endif
c     common /ffm/ x,nn,n1,k1
      dimension ff(*), term(*)
c
      e=a1s2*exp(-x)
      fac0=nn
      fac0=fac0+a1s2
      if(x.gt.xsw) go to 50
c
c  use power series
c
   10 fac=fac0
      term0=e/fac
      sum=term0
      ku=(x-fac0)
c     # sum increasing terms forwards
      do 20 k=1,ku
        fac=fac+a1
        term0=term0*x/fac
        sum=sum+term0
   20 continue
      i=1
      fac=fac+a1
      term(1)=term0*x/fac
      suma=sum+term(1)
      if(sum.eq.suma) go to 85
   40 i=i+1
      fac=fac+a1
      term(i)=term(i-1)*x/fac
      sum1=suma
      suma=suma+term(i)
      if(sum1-suma) 40,85,40
c
c  use asymptotic series
c
   50 sum=sqrt(pie4/x)
      if(nn.eq.0) go to 70
      fac=-a1s2
      do 60 k=1,nn
      fac=fac+a1
   60 sum=sum*fac/x
   70 i=1
      term(1)=-e/x
      suma=sum+term(1)
      if(sum.eq.suma) go to 85
      fac=fac0
      ku=(x+fac0-a1)
      do 80 i=2,ku
        fac=fac-a1
        term(i)=term(i-1)*fac/x
        sum1=suma
        suma=suma+term(i)
        if(sum1.eq.suma) go to 85
   80 continue
c     # xsw set too low. use power series.
      go to 10
c
c  sum decreasing terms backwards
c
   85 sum1=a0
c
c  11-feb-90 cray vectorization suppressed to avoid a numeric exception
c            which occured for x=5670. - rick ross/rls
cdir$ novector
      do 90 k=1,i
        sum1=sum1+term(i+1-k)
   90 continue
cdir$ vector
      ff(nn+1)=sum+sum1
c
c  use recurrence relation
c
      if(nn.eq.0) return
      do 95 k=1,nn
        fac0=fac0-a1
        ff(nn+1-k)=(e+x*ff(nn+2-k))/fac0
   95 continue
      return
      end
