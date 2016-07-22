C
      SUBROUTINE ORBSIN
C
      IMPLICIT REAL*8 (A-H, O-Z)
C
      INCLUDE 'param.inc'
      INCLUDE 'files.inc'
      INCLUDE 'symm.inc'
      INCLUDE 'general.inc'
      INCLUDE 'iterat.inc'
C
      integer iod(n3)
      namelist /orbdel/ ndelete
C
      OPEN (5, FILE='MOLFDIR.INP', FORM='FORMATTED')
      do i = 1, n3
	 iod(i) = 0
      enddo
      ndelete = 0
      call locate('ORBDEL',6,itest)
      if (itest.gt.0) then
         read (5,orbdel,err=1)
      else
         goto 1
      endif
      read (5,*,end=900,err=900) (iod(i),i=1,ndelete)
      close (5)
C
    1 OPEN (MRCONEE,FILE=FNAM(1),FORM='UNFORMATTED')
      READ (MRCONEE) NORB,BREIT,ECORE
      IF (NORB.GT.N3) CALL Q2JOB(3,'P.ORBSIN','N3',NORB)
      READ (MRCONEE) NSYMRP,(REPN(IRP),IRP=1,NSYMRP)
      READ (MRCONEE) NREP,(REPNA(IRP),IRP=1,2*NREP)
      READ (MRCONEE) ((MULTB(I,J),I=1,2*NREP),J=1,2*NREP)
      READ (MRCONEE) (IRPMO(IMO),IRPAMO(IMO),EPS(IMO),IMO=1,NORB)
      CLOSE (MRCONEE)
C
      NREP = 2 * NREP
C
      WRITE(6,1200)
      WRITE(6,1220) BREIT,ECORE,NORB
      morb = 0
      mdel = 0
      DO 300 I=1,NORB
      do j = 1, ndelete
         if (iod(j).EQ.i) Then
            Write (6,1230) I,0,REPN(IRPMO(morb+1)),
     &                     EPS(morb+1),'(DELETED)'
            do imo = morb + 1, morb + (norb-i)
               irpmo(imo) = irpmo(imo+1)
               irpamo(imo) = irpamo(imo+1)
               eps(imo) = eps(imo+1)
            enddo
	    mdel = mdel + 1
            indmo(i) = 0
            Goto 300
         endif
      enddo
      morb = morb + 1
      indmo(i) = morb
      WRITE(6,1230) I,MORB,REPN(IRPMO(morb)),EPS(morb)
 300  CONTINUE
      If (morb.NE.norb) Write (6,1240) norb-morb,morb
      ndelete = norb-morb
      norb = morb
C     ------------------------------------
C     MAKE INVERSE OF MULTIPLICATION TABLE
C     ------------------------------------
      DO 500 IR = 1, NREP
      DO 400 JR = 1, NREP
      KR = MULTB(IR,JR)
      MULTBI(IR,KR) = JR
  400 CONTINUE
  500 CONTINUE
C     ---------------------------------------
C     DETERMINE POSITION OF A1 REPRESENTATION
C     ---------------------------------------
      DO 40 IR = 1, NREP
      DO 39 JR = 1, NREP
      IF (MULTB(IR,JR).NE.JR) GOTO 40
   39 CONTINUE
      IRA1 = IR
      GOTO 41
   40 CONTINUE
      CALL Q2JOB (6,'A1 REPRESENTATION NOT FOUND',' ',0)
   41 CONTINUE
C
      RETURN
  900 Call Q2Job (2,'No indices of deletable orbitals',' ',0)
      Return
 1200 FORMAT(//1X,'Information read from MRCONEE ')
 1220 FORMAT(//1X,'Breit Interaction included :',T50,L1
     +        /1X,'Core Energy :',T30,G20.10
     +        /1X,'Number of orbitals :',T47,I4
     +       //1X,'Orbital',T10,'Index',
     +         T20,'Representation',T42,'Orbital Energy')
 1230 FORMAT(2X,I4,T10,1X,I4,T20,A14,T40,G20.10,5X,A)
 1240 FORMAT(//1X,'Number of orbitals reduced with :',I4
     +       /1X,'Number of active orbitals is now :',I4)
      END
