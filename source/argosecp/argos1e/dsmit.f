*deck dsmit
      subroutine dsmit(n1,c,s,v,y)
c
c  return an n1 by n1 triangular matrix c such that
c  c(t)sc=i, where i is an n1 by n1 identity matrix.
c
*mdc*if real16 vax ibm cray
*      implicit real*16 (a-h,o-z)
*mdc*else
      implicit real*8 (a-h,o-z)
*mdc*endif
*mdc*if real16q ibm vax
*c *** this mdc block is not necessary since the constants will be ***
*c *** correctly converted at compile time.  should it be removed? ***
*      parameter (a0=0.0q0, a1=1.0q0)
*mdc*else
      parameter (a0=0.0d0, a1=1.0d0)
*mdc*endif
c     common /ffm/ x,nn,n1,k1
      dimension c(n1,*), s(n1,*), v(*), y(*)
c
      do 20 i=1,n1
        do 10 j=1,i
          c(i,j)=a0
   10   continue
   20 continue
      do 80 j=1,n1
        kmax=j-1
        fac=s(j,j)
        do 30 k=1,kmax
          v(k)=a0
          y(k)=s(k,j)
   30   continue
        do 60 k=1,kmax
          dot=a0
          do 40 i=1,k
            dot=c(i,k)*y(i)+dot
   40     continue
          do 50 i=1,k
            v(i)=v(i)-dot*c(i,k)
   50     continue
          fac = fac - dot * dot
   60   continue
        fac=a1/sqrt(fac)
        c(j,j)=fac
        do 70 k=1,kmax
          c(k,j)=fac*v(k)
   70   continue
   80 continue
      return
      end
