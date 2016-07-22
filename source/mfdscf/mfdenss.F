C
C     ==================
      SUBROUTINE MFDENSS(D)
C     ==================
C
C     FORM THE DENSITY MATRIX IN THE SYM. ADDAPTED BASIS
C
      INCLUDE 'paracom.inc'
      INCLUDE 'general.inc'
C
      COMPLEX*16 D(*)
C
      OPEN (MFDENSM, FILE='MFDENSM', FORM='UNFORMATTED',
     +      ACCESS='DIRECT',RECL=16 * LREC)
      IRECDP = IRECDC
      IRECDC = IRECDC + 1
      IF (IRECDC .GT. N29) IRECDC = 1
C
C     ----------------------
      DO 260 IRP = 1, NSYMRP
C     ----------------------
C
        NBH = NBSIM (IRP) * (NBSIM (IRP) + 1) / 2
        MOF = 1
        MOL = 0
        IF (NCL (IRP) .GT. 0) THEN
          MOL = NCL (IRP)
          CALL DENSS (IRP, MOF, MOL, D, .FALSE.,0)
          CALL WDENS(IRECDC,D,NBH,IRP,1)
        ENDIF
        IF (NOP (IRP,1) .GT. 0) THEN
          MOF = MOL + 1
          MOL = MOF + NOP (IRP,1) - 1
          CALL DENSS (IRP, MOF, MOL, D, .TRUE.,1)
          CALL WDENS(IRECDC,D,NBH,IRP,2)
        ENDIF
        IF (NOP (IRP,2) .GT. 0) THEN
          MOF = MOL + 1
          MOL = MOF + NOP (IRP,2) - 1
          CALL DENSS (IRP, MOF, MOL, D, .TRUE.,2)
          CALL WDENS(IRECDC,D,NBH,IRP,3)
        ENDIF
 260  CONTINUE
      CLOSE (MFDENSM)
      RETURN
      END
