C
C     ================
      SUBROUTINE RDTWO
C     ================
      IMPLICIT REAL*8 (A-H, O-Z)
C
      INCLUDE 'param.inc'
      INCLUDE 'common.inc'
      INCLUDE 'general.inc'
C
      CHARACTER*10 DATEX,TIMEX*8
      DIMENSION KR(-N1:N1),KRA(N1,3),KRB(N1,3)
      INTEGER INDK(N3),INDL(N3)
C
      MDINT = 1
      NUNIQ = 1
      OPEN(MDINT,FILE='MDCINT',FORM='UNFORMATTED')
      Read (mdint,err=10000,end=10000) datex,timex,nkr,
     &             (kra(i,1),krb(i,1),i=1,nkr)
      Do ik = 1, nkr
         kr(ik)  = kra(ik,1)
         kr(-ik) = krb(ik,1)
      EndDo
C--!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      nkrcoul = nkr
  10  read (mdint,ERR=10010,END=10010) ikr,jkr,nz,
     &             (indk(inz),indl(inz),inz=1,nz),
     &             (twoinr(inz),twoini(inz),inz=nuniq+1,nuniq+nz)
      If (nuniq+nz.GT.N4A) Call Q2Job (3,'P.RDTWO','N4A',nuniq+nz)
      If (ikr.EQ.0) GoTo 20 ! End of File
C-->  Distribute this set and generate time-reversed integrals.
C-->  K|ia> = |ib> ; K|ib> = - |ia>
      i = kr(ikr)
      itr = kr(-ikr)
      j = kr(jkr)
      jtr = kr(-jkr)
      SignIJ = SIGN(1,ikr) * SIGN(1,jkr)
      do inz = 1, nz
         nuniq = nuniq + 1
         kkr = indk(inz)
         k = kr(kkr)
         ktr = kr(-kkr)
         lkr = indl(inz)
         l = kr(lkr)
         ltr = kr(-lkr)
         SignKL = SIGN(1,kkr) * SIGN(1,lkr)
C--> Original integral plus time-reversed partners
         INDTWR(I,J,K,L) = NUNIQ
         INDTWR(JTR,ITR,K,L) = NUNIQ * SignIJ
         INDTWR(I,J,LTR,KTR) = NUNIQ * SignKL
         INDTWR(JTR,ITR,LTR,KTR) = NUNIQ * SignIJ * SignKL
         INDTWI(I,J,K,L) = NUNIQ
         INDTWI(JTR,ITR,K,L) = NUNIQ * SignIJ
         INDTWI(I,J,LTR,KTR) = NUNIQ * SignKL
         INDTWI(JTR,ITR,LTR,KTR) = NUNIQ * SignIJ * SignKL
C--> Complex conjugate plus time-reversed partners
         INDTWR(J,I,L,K) = NUNIQ
         INDTWR(ITR,JTR,L,K) = NUNIQ * SignIJ
         INDTWR(J,I,KTR,LTR) = NUNIQ * SignKL
         INDTWR(ITR,JTR,KTR,LTR) = NUNIQ * SignIJ * SignKL
         INDTWI(J,I,L,K) = - NUNIQ
         INDTWI(ITR,JTR,L,K) = - NUNIQ * SignIJ
         INDTWI(J,I,KTR,LTR) = - NUNIQ * SignKL
         INDTWI(ITR,JTR,KTR,LTR) = - NUNIQ * SignIJ * SignKL
C--> Particle interchanged plus time-reversed partners
         INDTWR(K,L,I,J) = NUNIQ
         INDTWR(LTR,KTR,I,J) = NUNIQ * SignKL
         INDTWR(K,L,JTR,ITR) = NUNIQ * SignIJ
         INDTWR(LTR,KTR,JTR,ITR) = NUNIQ * SignIJ * SignKL
         INDTWI(K,L,I,J) = NUNIQ
         INDTWI(LTR,KTR,I,J) = NUNIQ * SignKL
         INDTWI(K,L,JTR,ITR) = NUNIQ * SignIJ
         INDTWI(LTR,KTR,JTR,ITR) = NUNIQ * SignIJ * SignKL
C--> Particle interchanged and complex conjugated plus time-reversed partners
         INDTWR(L,K,J,I) = NUNIQ
         INDTWR(KTR,LTR,J,I) = NUNIQ * SignKL
         INDTWR(L,K,ITR,JTR) = NUNIQ * SignIJ
         INDTWR(KTR,LTR,ITR,JTR) = NUNIQ * SignIJ * SignKL
         INDTWI(L,K,J,I) = - NUNIQ
         INDTWI(KTR,LTR,J,I) = - NUNIQ * SignKL
         INDTWI(L,K,ITR,JTR) = - NUNIQ * SignIJ
         INDTWI(KTR,LTR,ITR,JTR) = - NUNIQ * SignIJ * SignKL
      Enddo
      Goto 10
 20   Continue
      CLOSE(1)
      Write (6,1000) "Coulomb",datex,timex,nuniq-1
      If (.NOT.BREIT) Return
      NUNIQC = NUNIQ
      OPEN(MDINT,FILE='MDBINT',FORM='UNFORMATTED')
      Read (mdint,err=10001,end=10001) datex,timex,nkr,
     &             (kra(i,1),krb(i,1),i=1,nkr)
      If (NKR.NE.NKRCOUL) Then
         Write (6,*) 'Basis set size on Coulomb file :',nkrcoul
         Write (6,*) 'Basis set size on Gaunt file :  ',nkr
         Stop 'Gaunt and Coulomb 2-el files incompatible'
      Endif
  30  read (mdint,ERR=10011,END=10011) ikr,jkr,nz,
     &             (indk(inz),indl(inz),inz=1,nz),
     &             (twoinr(inz),twoini(inz),inz=nuniq+1,nuniq+nz)
      If (nuniq+nz.GT.N4A) Call Q2Job (3,'P.RDTWO','N4A',nuniq+nz)
      If (ikr.EQ.0) GoTo 40 ! End of File
C-->  Distribute this set and generate time-reversed integrals.
C-->  K|ia> = |ib> ; K|ib> = - |ia>
      i = kr(ikr)
      itr = kr(-ikr)
      j = kr(jkr)
      jtr = kr(-jkr)
      SignIJ = - SIGN(1,ikr) * SIGN(1,jkr)
      do inz = 1, nz
         nuniq = nuniq + 1
         kkr = indk(inz)
         k = kr(kkr)
         ktr = kr(-kkr)
         lkr = indl(inz)
         l = kr(lkr)
         ltr = kr(-lkr)
         SignKL = - SIGN(1,kkr) * SIGN(1,lkr)
C--> Original integral plus time-reversed partners
         INDGAR(I,J,K,L) = NUNIQ
         INDGAR(JTR,ITR,K,L) = NUNIQ * SignIJ
         INDGAR(I,J,LTR,KTR) = NUNIQ * SignKL
         INDGAR(JTR,ITR,LTR,KTR) = NUNIQ * SignIJ * SignKL
         INDGAI(I,J,K,L) = NUNIQ
         INDGAI(JTR,ITR,K,L) = NUNIQ * SignIJ
         INDGAI(I,J,LTR,KTR) = NUNIQ * SignKL
         INDGAI(JTR,ITR,LTR,KTR) = NUNIQ * SignIJ * SignKL
C--> Complex conjugate plus time-reversed partners
         INDGAR(J,I,L,K) = NUNIQ
         INDGAR(ITR,JTR,L,K) = NUNIQ * SignIJ
         INDGAR(J,I,KTR,LTR) = NUNIQ * SignKL
         INDGAR(ITR,JTR,KTR,LTR) = NUNIQ * SignIJ * SignKL
         INDGAI(J,I,L,K) = - NUNIQ
         INDGAI(ITR,JTR,L,K) = - NUNIQ * SignIJ
         INDGAI(J,I,KTR,LTR) = - NUNIQ * SignKL
         INDGAI(ITR,JTR,KTR,LTR) = - NUNIQ * SignIJ * SignKL
C--> Particle interchanged plus time-reversed partners
         INDGAR(K,L,I,J) = NUNIQ
         INDGAR(LTR,KTR,I,J) = NUNIQ * SignKL
         INDGAR(K,L,JTR,ITR) = NUNIQ * SignIJ
         INDGAR(LTR,KTR,JTR,ITR) = NUNIQ * SignIJ * SignKL
         INDGAI(K,L,I,J) = NUNIQ
         INDGAI(LTR,KTR,I,J) = NUNIQ * SignKL
         INDGAI(K,L,JTR,ITR) = NUNIQ * SignIJ
         INDGAI(LTR,KTR,JTR,ITR) = NUNIQ * SignIJ * SignKL
C--> Particle interchanged and complex conjugated plus time-reversed partners
         INDGAR(L,K,J,I) = NUNIQ
         INDGAR(KTR,LTR,J,I) = NUNIQ * SignKL
         INDGAR(L,K,ITR,JTR) = NUNIQ * SignIJ
         INDGAR(KTR,LTR,ITR,JTR) = NUNIQ * SignIJ * SignKL
         INDGAI(L,K,J,I) = - NUNIQ
         INDGAI(KTR,LTR,J,I) = - NUNIQ * SignKL
         INDGAI(L,K,ITR,JTR) = - NUNIQ * SignIJ
         INDGAI(KTR,LTR,ITR,JTR) = - NUNIQ * SignIJ * SignKL
      Enddo
      Goto 30
 40   Continue
      CLOSE(1)
      Write (6,1000) "Gaunt",datex,timex,nuniq-nuniqc
      RETURN
 1000 Format (/1X,A7,' integral file was generated at ',A10,1x,A8,
     &/' Read',I8,' unique integrals')
10000 STOP 'ERROR READING HEADER OF MDCINT'
10001 STOP 'ERROR READING HEADER OF MDBINT'
10010 STOP 'ERROR READING INTEGRALS FROM MDCINT'
10011 STOP 'ERROR READING INTEGRALS FROM MDBINT'
      END
