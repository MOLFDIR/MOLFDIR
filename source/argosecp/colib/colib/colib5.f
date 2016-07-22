*colib5.f
*colib part=5 of 9.  misc precision neutral (w) linpack routines.
*version=4.1 last modified: 24-nov-90
c
c  RCS $Revision: 1.3 $  $Date: 2001/09/07 02:25:12 $
c
c  see colib1.f for version history info.
c
*deck wspfa
      subroutine wspfa(ap,n,kpvt,info)
*mdc*if cray slinpack
*c     # use single precision library routine.
*      real ap(*)
*      integer n,kpvt(*),info
*      call sspfa(ap,n,kpvt,info)
*mdc*elseif dlinpack
*c     # use double precision library routine.
*      double precision ap(*)
*      integer n,kpvt(*),info
*      call dspfa(ap,n,kpvt,info)
*mdc*else
c     # generic fortran version from linpack dspfa. -rls
      integer n,kpvt(1),info
      real*8 ap(1)
c
c     dspfa factors a real*8 symmetric matrix stored in
c     packed form by elimination with symmetric pivoting.
c
c     to solve  a*x = b , follow dspfa by dspsl.
c     to compute  inverse(a)*c , follow dspfa by dspsl.
c     to compute  determinant(a) , follow dspfa by dspdi.
c     to compute  inertia(a) , follow dspfa by dspdi.
c     to compute  inverse(a) , follow dspfa by dspdi.
c
c     on entry
c
c        ap      real*8 (n*(n+1)/2)
c                the packed form of a symmetric matrix  a .  the
c                columns of the upper triangle are stored sequentially
c                in a one-dimensional array of length  n*(n+1)/2 .
c                see comments below for details.
c
c        n       integer
c                the order of the matrix  a .
c
c     output
c
c        ap      a block diagonal matrix and the multipliers which
c                were used to obtain it stored in packed form.
c                the factorization can be written  a = u*d*trans(u)
c                where  u  is a product of permutation and unit
c                upper triangular matrices , trans(u) is the
c                transpose of  u , and  d  is block diagonal
c                with 1 by 1 and 2 by 2 blocks.
c
c        kpvt    integer(n)
c                an integer vector of pivot indices.
c
c        info    integer
c                = 0  normal value.
c                = k  if the k-th pivot block is singular. this is
c                     not an error condition for this subroutine,
c                     but it does indicate that dspsl or sspdi may
c                     divide by zero if called.
c
c     packed storage
c
c          the following program segment will pack the upper
c          triangle of a symmetric matrix.
c
c                k = 0
c                do 20 j = 1, n
c                   do 10 i = 1, j
c                      k = k + 1
c                      ap(k)  = a(i,j)
c             10    continue
c             20 continue
c
c     linpack. this version dated 08/14/78 .
c     james bunch, univ. calif. san diego, argonne nat. lab.
c
c     subroutines and functions
c
c     blas daxpy,dswap,idamax
c     fortran abs,max,sqrt
c
c     internal variables
c
      real*8 ak,akm1,bk,bkm1,denom,mulk,mulkm1,t
      real*8 absakk,alpha,colmax,rowmax
      integer idamax,ij,ijj,ik,ikm1,im,imax,imaxp1,imim,imj,imk
      integer j,jj,jk,jkm1,jmax,jmim,k,kk,km1,km1k,km1km1,km2,kstep
      logical swap
      real*8    zero,     one,     r17,      eight
      parameter(zero=0d0, one=1d0, r17=17d0, eight=8d0)
c
c
c     initialize
c
c     alpha is used in choosing pivot block size.
c     alpha = (1.0d0 + sqrt(17.0d0))/8.0d0
      alpha = (one + sqrt(r17))/eight
c
      info = 0
c
c     main loop on k, which goes from n to 1.
c
      im = 0
      k  = n
      ik = (n*(n - 1))/2
   10 continue
c
c        leave the loop if k=0 or k=1.
c
c     ...exit
         if (k .eq. 0) go to 200
         if (k .gt. 1) go to 20
            kpvt(1) = 1
            if (ap(1) .eq. zero) info = 1
c     ......exit
            go to 200
   20    continue
c
c        this section of code determines the kind of
c        elimination to be performed.  when it is completed,
c        kstep will be set to the size of the pivot block, and
c        swap will be set to .true. if an interchange is
c        required.
c
         km1 = k - 1
         kk = ik + k
         absakk = abs(ap(kk))
c
c        determine the largest off-diagonal element in
c        column k.
c
         imax = idamax(k-1,ap(ik+1),1)
         imk = ik + imax
         colmax = abs(ap(imk))
         if (absakk .lt. alpha*colmax) go to 30
            kstep = 1
            swap = .false.
         go to 90
   30    continue
c
c           determine the largest off-diagonal element in
c           row imax.
c
            rowmax = zero
            imaxp1 = imax + 1
            im = imax*(imax - 1)/2
            imj = im + 2*imax
            do 40 j = imaxp1, k
               rowmax = max(rowmax,abs(ap(imj)))
               imj = imj + j
   40       continue
            if (imax .eq. 1) go to 50
               jmax = idamax(imax-1,ap(im+1),1)
               jmim = jmax + im
               rowmax = max(rowmax,abs(ap(jmim)))
   50       continue
            imim = imax + im
            if (abs(ap(imim)) .lt. alpha*rowmax) go to 60
               kstep = 1
               swap = .true.
            go to 80
   60       continue
            if (absakk .lt. alpha*colmax*(colmax/rowmax)) go to 70
               kstep = 1
               swap = .false.
            go to 80
   70       continue
               kstep = 2
               swap = imax .ne. km1
   80       continue
   90    continue
         if (max(absakk,colmax) .ne. zero) go to 100
c
c           column k is zero.  set info and iterate the loop.
c
            kpvt(k) = k
            info = k
         go to 190
  100    continue
         if (kstep .eq. 2) go to 140
c
c           1 x 1 pivot block.
c
            if (.not.swap) go to 120
c
c              perform an interchange.
c
               call dswap(imax,ap(im+1),1,ap(ik+1),1)
               imj = ik + imax
               do 110 jj = imax, k
                  j = k + imax - jj
                  jk = ik + j
                  t = ap(jk)
                  ap(jk) = ap(imj)
                  ap(imj) = t
                  imj = imj - (j - 1)
  110          continue
  120       continue
c
c           perform the elimination.
c
            ij = ik - (k - 1)
            do 130 jj = 1, km1
               j = k - jj
               jk = ik + j
               mulk = -ap(jk)/ap(kk)
               t = mulk
               call daxpy(j,t,ap(ik+1),1,ap(ij+1),1)
               ijj = ij + j
               ap(jk) = mulk
               ij = ij - (j - 1)
  130       continue
c
c           set the pivot array.
c
            kpvt(k) = k
            if (swap) kpvt(k) = imax
         go to 190
  140    continue
c
c           2 x 2 pivot block.
c
            km1k = ik + k - 1
            ikm1 = ik - (k - 1)
            if (.not.swap) go to 160
c
c              perform an interchange.
c
               call dswap(imax,ap(im+1),1,ap(ikm1+1),1)
               imj = ikm1 + imax
               do 150 jj = imax, km1
                  j = km1 + imax - jj
                  jkm1 = ikm1 + j
                  t = ap(jkm1)
                  ap(jkm1) = ap(imj)
                  ap(imj) = t
                  imj = imj - (j - 1)
  150          continue
               t = ap(km1k)
               ap(km1k) = ap(imk)
               ap(imk) = t
  160       continue
c
c           perform the elimination.
c
            km2 = k - 2
            if (km2 .eq. 0) go to 180
               ak = ap(kk)/ap(km1k)
               km1km1 = ikm1 + k - 1
               akm1 = ap(km1km1)/ap(km1k)
               denom = one - ak*akm1
               ij = ik - (k - 1) - (k - 2)
               do 170 jj = 1, km2
                  j = km1 - jj
                  jk = ik + j
                  bk = ap(jk)/ap(km1k)
                  jkm1 = ikm1 + j
                  bkm1 = ap(jkm1)/ap(km1k)
                  mulk = (akm1*bk - bkm1)/denom
                  mulkm1 = (ak*bkm1 - bk)/denom
                  t = mulk
                  call daxpy(j,t,ap(ik+1),1,ap(ij+1),1)
                  t = mulkm1
                  call daxpy(j,t,ap(ikm1+1),1,ap(ij+1),1)
                  ap(jk) = mulk
                  ap(jkm1) = mulkm1
                  ijj = ij + j
                  ij = ij - (j - 1)
  170          continue
  180       continue
c
c           set the pivot array.
c
            kpvt(k) = 1 - k
            if (swap) kpvt(k) = -imax
            kpvt(k-1) = kpvt(k)
  190    continue
         ik = ik - (k - 1)
         if (kstep .eq. 2) ik = ik - (k - 2)
         k = k - kstep
      go to 10
  200 continue
*mdc*endif
      return
      end
*deck wspsl
      subroutine wspsl(ap,n,kpvt,b)
*mdc*if cray slinpack
*c     # use single precision library routine.
*      integer n,kpvt(*)
*      real ap(*),b(*)
*      call sspsl(ap,n,kpvt,b)
*mdc*elseif dlinpack
*c     # use double precision library routine.
*      integer n,kpvt(*)
*      double precision ap(*),b(*)
*      call dspsl(ap,n,kpvt,b)
*mdc*else
c     # generic fortran version from lipack dspsl. -rls
      integer n,kpvt(n)
      real*8 ap(21),b(n)
c
c     dspsl solves the real*8 symmetric system
c     a * x = b
c     using the factors computed by dspfa.
c
c     on entry
c
c        ap      real*8(n*(n+1)/2)
c                the output from dspfa.
c
c        n       integer
c                the order of the matrix  a .
c
c        kpvt    integer(n)
c                the pivot vector from dspfa.
c
c        b       real*8(n)
c                the right hand side vector.
c
c     on return
c
c        b       the solution vector  x .
c
c     error condition
c
c        a division by zero may occur if  sspco  has set rcond .eq. 0.0
c        or  dspfa  has set info .ne. 0  .
c
c     to compute  inverse(a) * c  where  c  is a matrix
c     with  p  columns
c           call dspfa(ap,n,kpvt,info)
c           if (info .ne. 0) go to ...
c           do 10 j = 1, p
c              call dspsl(ap,n,kpvt,c(1,j))
c        10 continue
c
c     linpack. this version dated 08/14/78 .
c     james bunch, univ. calif. san diego, argonne nat. lab.
c
c     subroutines and functions
c
c     blas daxpy,ddot
c     fortran abs
c
c     internal variables.
c
      real*8 ak,akm1,bk,bkm1,ddot,denom,temp
      integer ik,ikm1,ikp1,k,kk,km1k,km1km1,kp
      real*8    one
      parameter(one=1d0)
c
c     loop backward applying the transformations and
c     d inverse to b.
c
      k = n
      ik = (n*(n - 1))/2
   10 if (k .eq. 0) go to 80
         kk = ik + k
         if (kpvt(k) .lt. 0) go to 40
c
c           1 x 1 pivot block.
c
            if (k .eq. 1) go to 30
               kp = kpvt(k)
               if (kp .eq. k) go to 20
c
c                 interchange.
c
                  temp = b(k)
                  b(k) = b(kp)
                  b(kp) = temp
   20          continue
c
c              apply the transformation.
c
               call daxpy(k-1,b(k),ap(ik+1),1,b(1),1)
   30       continue
c
c           apply d inverse.
c
            b(k)=b(k)/ap(kk)
            k = k - 1
            ik = ik - k
         go to 70
   40    continue
c
c           2 x 2 pivot block.
c
            ikm1 = ik - (k - 1)
            if (k .eq. 2) go to 60
               kp = abs(kpvt(k))
               if (kp .eq. k - 1) go to 50
c
c                 interchange.
c
                  temp = b(k-1)
                  b(k-1) = b(kp)
                  b(kp) = temp
   50          continue
c
c              apply the transformation.
c
               call daxpy(k-2,b(k),ap(ik+1),1,b(1),1)
               call daxpy(k-2,b(k-1),ap(ikm1+1),1,b(1),1)
   60       continue
c
c           apply d inverse.
c
            km1k = ik + k - 1
            kk = ik + k
            ak = ap(kk)/ap(km1k)
            km1km1 = ikm1 + k - 1
            akm1 = ap(km1km1)/ap(km1k)
            bk = b(k)/ap(km1k)
            bkm1 = b(k-1)/ap(km1k)
            denom = ak*akm1 - one
            b(k) = (akm1*bk - bkm1)/denom
            b(k-1) = (ak*bkm1 - bk)/denom
            k = k - 2
            ik = ik - (k + 1) - k
   70    continue
      go to 10
   80 continue
c
c     loop forward applying the transformations.
c
      k = 1
      ik = 0
   90 if (k .gt. n) go to 160
         if (kpvt(k) .lt. 0) go to 120
c
c           1 x 1 pivot block.
c
            if (k .eq. 1) go to 110
c
c              apply the transformation.
c
               b(k) = b(k) + ddot(k-1,ap(ik+1),1,b(1),1)
               kp = kpvt(k)
               if (kp .eq. k) go to 100
c
c                 interchange.
c
                  temp = b(k)
                  b(k) = b(kp)
                  b(kp) = temp
  100          continue
  110       continue
            ik = ik + k
            k = k + 1
         go to 150
  120    continue
c
c           2 x 2 pivot block.
c
            if (k .eq. 1) go to 140
c
c              apply the transformation.
c
               b(k) = b(k) + ddot(k-1,ap(ik+1),1,b(1),1)
               ikp1 = ik + k
               b(k+1) = b(k+1) + ddot(k-1,ap(ikp1+1),1,b(1),1)
               kp = abs(kpvt(k))
               if (kp .eq. k) go to 130
c
c                 interchange.
c
                  temp = b(k)
                  b(k) = b(kp)
                  b(kp) = temp
  130          continue
  140       continue
            ik = ik + k + k + 1
            k = k + 2
  150    continue
      go to 90
  160 continue
*mdc*endif
      return
      end
