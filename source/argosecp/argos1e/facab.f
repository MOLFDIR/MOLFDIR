*deck facab
      subroutine facab(binom,na1,nb1,crda,crdb,xab)
      implicit real*8 (a-h,o-z)
      parameter (a1=1.0d0, a2=2.0d0, a3=3.0d0, a4=4.0d0, a6=6.0d0)
      dimension binom(*), crda(*), crdb(*), xab(*)
c
      call wzero(na1+nb1-1,xab,1)
      naind=(na1*(na1-1))/2
      nbind=(nb1*(nb1-1))/2
      do 12 ia1=1,na1
        do 10 ib1=1,nb1
          xab((ia1-1)+ib1)=xab((ia1-1)+ib1)+
     1                       (binom(naind+ia1)*crda((na1+1)-ia1))*
     2                        binom(nbind+ib1)*crdb((nb1+1)-ib1)
   10   continue
   12 continue
      return
      end
