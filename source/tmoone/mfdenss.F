C
C     ==================
      SUBROUTINE MFDENSS
C     ==================
C
C     FORM THE DENSITY MATRIX IN THE SYM. ADDAPTED BASIS
C
      INCLUDE 'param.inc'
      INCLUDE 'common.inc'
      INCLUDE 'densfock.inc'
C
      COMPLEX*16 D (2 * N21)
      EQUIVALENCE (D, DAAR)
C
      OPEN (MFDENSM, FILE='MFDENSM', FORM='UNFORMATTED')
      REWIND (MFDENSM)
C
C     ----------------------
      DO 260 IRP = 1, NSYMRP
C     ----------------------
C
        NBH = NBSIM (IRP) * (NBSIM (IRP) + 1) / 2
        IF (NBH.GT.2*N21) CALL Q2JOB(3,'MFDENSS','N21',NBH/2)
        MOF = 1
        MOL = 0
        IF (NCL (IRP) .GT. 0) THEN
          MOL = NCL (IRP)
          CALL DENSS (IRP, MOF, MOL, D, .FALSE.)
          WRITE (MFDENSM) (D (I), I = 1, NBH)
        ENDIF
        IF (NOP (IRP) .GT. 0) THEN
          MOF = MOL + 1
          MOL = MOF + NOP (IRP) - 1
          CALL DENSS (IRP, MOF, MOL, D, .TRUE.)
          WRITE (MFDENSM) (D (I), I = 1, NBH)
        ENDIF
 260  CONTINUE
      CLOSE (MFDENSM)
      RETURN
      END
