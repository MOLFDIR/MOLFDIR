      Subroutine WriTwoe (iprnt,nkr,mdint,ikr,jkr,indk,indl,rl,nsp,rkl)
c
      IMPLICIT REAL*8 (A-H, O-Z)
c
      INCLUDE "vecs.inc"
      INCLUDE "thres.inc"
c
      complex*16 rl(nsp,nsp),rkl(*)
      integer indk(*),indl(*),ikr,jkr,nsp,nkr,iprnt,mdint
c
c     4 sets are written : 1) (ibar,jbar|kbar,lbar)
c                          2) (ibar,j   |kbar,l   )
c                          3) (ibar,j   |k   ,lbar)
c                          4) (ibar,j   |k   ,l   )
c                              
c     1) canonical list  : (i => j), (ij => kl), (ij => lk)
c     2) canonical list  : (i => j), (k => l), (ij => kl)
c     3) canonical list  : (i => j), (k => l), (ij => kl)
c     4) canonical list  : (i => j)
c
      if (ikr.gt.0) return
      nz = 0
      ikra = - ikr
      i = krb(ikra,1)
      if (jkr.LT.0) then ! set 1)
         jkra = - jkr
         j = krb(jkra,1)
         do 10 kkr = 1, ikra
            k = krb(kkr,1)
            lkrmax = ikra
            if (ikra.EQ.jkra) lkrmax = kkr
            if (ikra.EQ.kkr) lkrmax = jkra
            if (kkr.GT.jkra) lkrmax = min0(ikra - 1, lkrmax)
            do 11 lkr = 1, lkrmax
               l = krb(lkr,1)
#if defined (BIT64)
               if (cabs(rl(k,l)).gt.throut) then
#else
               if (cdabs(rl(k,l)).gt.throut) then
#endif
                  nz = nz + 1
                  indk(nz) = - kkr
                  indl(nz) = - lkr
                  rkl(nz) = rl(k,l) 
                  if (iprnt.ge.2) write(6,1000) i,j,k,l,rkl(nz)
               endif
   11       continue
   10    continue
      else! sets 2), 3) and 4)
         j = kra(jkr,1)
         do 20 kkr = 1, ikra
            k = krb(kkr,1)
            lkrmax = kkr
            if (ikra.EQ.kkr) lkrmax = jkr
            do 21 lkr = 1, lkrmax
               l = kra(lkr,1)
#if defined (BIT64)
               if (cabs(rl(k,l)).gt.throut) then
#else
               if (cdabs(rl(k,l)).gt.throut) then
#endif
                  nz = nz + 1
                  indk(nz) = - kkr
                  indl(nz) = lkr
                  rkl(nz) = rl(k,l)
                  if (iprnt.ge.2) write(6,1000) i,j,k,l,rkl(nz)
               endif
   21       continue
   20    continue
         do 30 kkr = 1, ikra
            k = kra(kkr,1)
            lkrmax = kkr
            if (ikra.EQ.kkr) lkrmax = jkr
            do 31 lkr = 1, lkrmax
               l = krb(lkr,1)
#if defined (BIT64)
               if (cabs(rl(k,l)).gt.throut) then
#else
               if (cdabs(rl(k,l)).gt.throut) then
#endif
                  nz = nz + 1
                  indk(nz) = kkr
                  indl(nz) = - lkr
                  rkl(nz) = rl(k,l)
                  if (iprnt.ge.2) write(6,1000) i,j,k,l,rkl(nz)
               endif
   31       continue
   30    continue
c        goto 42
         do 40 kkr = 1, nkr
            k = kra(kkr,1)
            do 41 lkr = 1, nkr
               l = kra(lkr,1)
#if defined (BIT64)
               if (cabs(rl(k,l)).gt.throut) then
#else
               if (cdabs(rl(k,l)).gt.throut) then
#endif
                  nz = nz + 1
                  indk(nz) = kkr
                  indl(nz) = lkr
                  rkl(nz) = rl(k,l)
                  if (iprnt.ge.2) write(6,1000) i,j,k,l,rkl(nz)
               endif
   41       continue
   40    continue
   42    continue
      endif
c
      write (mdint) ikr,jkr,nz,
     &              (indk(inz),indl(inz),inz=1,nz),
     &              (rkl(inz),inz=1,nz)
c
 1000 format (4i4,2x,2f20.16)
      return
      end
