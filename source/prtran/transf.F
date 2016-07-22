C
C     ===========================================================
      SUBROUTINE TRANSF (NHERP,PROPAO,PROPAOT,WORKA,WORKB,PROPMO)
C     Transform all property integrals
C     ===========================================================
C
      IMPLICIT REAL*8 (A-H, O-Z)
C
#if defined (BIT64)
      PARAMETER (A0=(0.0,0.0),A1=(1.0,0.0))
#else
      PARAMETER (A0=(0.D0,0.D0),A1=(1.D0,0.D0))
#endif
      INCLUDE "vecs.inc"
      INCLUDE "sym.inc"
      INCLUDE "inpt.inc"
      INCLUDE "files.inc"
      CHARACTER*32 ACHAR
      REAL*8 PROPAO(NHERP,NHERP),PROPAOT(NHERP*(NHERP+1)/2)
      COMPLEX*16 PROPMO(NSP,NSP),WORKA(NHERP,NSP),WORKB(NHERP,NSP)
      COMPLEX*16 EXPECV
C
      OPEN (MDPROP,FILE=FNAME(6),FORM='UNFORMATTED')
      OPEN (AOPROPER,FILE=FNAME(5),FORM='UNFORMATTED')
    1 READ (AOPROPER,END=2,ERR=2) ACHAR
      IF (ACHAR(1:8).NE.'********') GOTO 1
      IF (IPRNT.GT.1)
     &WRITE (6,1000) ACHAR(25:32),ACHAR(9:16),ACHAR(17:24)
      IF (ACHAR(17:18).EQ.'AN'.OR.ACHAR(17:18).EQ.'SY') THEN
         IF (ACHAR(17:18).EQ.'SY') FACTOR = 1.0D0
         IF (ACHAR(17:18).EQ.'AN') FACTOR = - 1.0D0
         READ (AOPROPER) PROPAOT
         IJ = 0
         DO J = 1, NHERP
            DO I = 1, J
               IJ = IJ + 1
               PROPAO(I,J) = PROPAOT(IJ)
               PROPAO(J,I) = PROPAOT(IJ) * FACTOR
            ENDDO
         ENDDO
      ELSE
         READ (AOPROPER) PROPAO
      ENDIF
      CALL UDPU (NHERP,NSP,N2,PROPAO,UA,UB,WORKA,WORKB,PROPMO)
C
      IF (IPRNT.GE.1) THEN
         WRITE (6,1010) ACHAR(25:32)
         DO I = 1, NSP
            WRITE (6,1011) I,REPN(IMOREP(I)),PROPMO(I,I)
         ENDDO
         WRITE (6,*)
         IF (IPRNT.GE.3) THEN
            WRITE (6,1012)
            DO I = 1, NSP
               DO J = 1, NSP
                  WRITE (6,1013) I,J,PROPMO(I,J)
               ENDDO
            ENDDO
            WRITE (6,*)
         ENDIF
      ENDIF
C
C     Calculate expectation value of operator and write it to output
C
      EXPECV = A0
      DO I = 1, NSP
         EXPECV = EXPECV + PROPMO(I,I) * OCCMO(I)
      ENDDO
      WRITE (6,1014) ACHAR(25:32),EXPECV
C
C     WRITE PROPMO TO FILE
C
      WRITE (MDPROP) ACHAR
      WRITE (MDPROP) PROPMO
C
#if defined (CRAY) || defined (LINUX) || defined (SUN)
      CALL FLUSH (6)
#endif
#if defined (IBM) 
      CALL FLUSH_(6)
#endif
C
      GOTO 1
    2 CONTINUE
      CLOSE (MDPROP)
      WRITE (6,1001) FNAME(6)
C
 1000 FORMAT (/' Transforming ',A8,' evaluation date ',A8,' label ',A8)
 1001 FORMAT (/' Finished transformation : integrals on ',A8)
 1010 FORMAT (/' Diagonal elements of ',A8,
     & ' operator in molecular spinor basis'
     & //' Spinor Symmetry',14X,'Real part',11X,'Imaginary Part')
 1011 FORMAT (I6,2X,A14,2X,F18.10,2X,F18.10)
 1012 FORMAT (/' Complete property matrix in molecular spinor basis')
 1013 FORMAT (2I5,F18.10,2X,F18.10)
 1014 FORMAT (' Expectation value of operator ',A8,' :',2F18.10)
      RETURN
      END
