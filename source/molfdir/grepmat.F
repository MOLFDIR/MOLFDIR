C
C     ==================
      SUBROUTINE GREPMAT
C     ==================
      IMPLICIT REAL*8 (A-H,O-Z)
C
      INCLUDE 'paracom.inc'
#if defined (PARALLEL)
      INCLUDE 'mpidef.inc'
#else
      PARAMETER (mpirnk = 0)
#endif
C
      COMMON/GROUPR/PROJOP(48,16),UU(2,2,48)
      COMPLEX*16 PROJOP,UU
      COMPLEX*16 U2(4,48)
      EQUIVALENCE (UU(1,1,1),U2(1,1))
      COMPLEX*16 VPROD,CFA(N3),CFB(N3),REPMAT(4,4),SN
      COMPLEX*16 PREPMAT(4,4,48,16)
      INTEGER IDIM(16),INDSFT(N9),ITHIRP(16),IPRTNR(16)
      COMPLEX*16 A0,A1
#if defined (BIT64)
      PARAMETER (A1=(1.0,0.0),A0=(0.0,0.0))
#else
      PARAMETER (A1=(1.D0,0.D0),A0=(0.D0,0.D0))
#endif
      REAL*8 RNORM(4)
      LOGICAL PREP(16)
      COMPLEX*16 XOVLAP
C
      IF (NEQSYMF.GT.N9) CALL Q2JOB(3,'GREPMAT','N9',NEQSYMF)
C
      DO I = 1, 16
         PREP(I) = .TRUE.
      ENDDO
      IFMAX=0
      IFS=0
      DO 300 IF=1,NEQSYMF
        IF (IF.LE.IFMAX) GOTO 300
        IFS=IFS+1
        IFMIN=IFMAX+1
        IFMAX=IFMAX+NSYMC(IFS)
        DO 310 I=IFMIN,IFMAX
          INDSFT(I)=INDCOM(IFS)
 310      CONTINUE
 300    CONTINUE
C
      IREP=1
      DO 200 IRP=1,NSYMRP
        DO 210 I=IREP,IREP+IDIMRP(IRP)-1
          IDIM(I)=IDIMRP(IRP)
          ITHIRP(I)=IRP
          IPRTNR(I)=I-IREP+1
 210      CONTINUE
        IREP=IREP+IDIMRP(IRP)
 200    CONTINUE
C
      IREP=IREP-1
      WRITE(15) (IDIM(I),I=1,IREP),(ITHIRP(I),I=1,IREP),
     +                             (IPRTNR(I),I=1,IREP)
C
      ISFL=0
      DO 100 IF=1,NEQSYMF
        IF (IF.LE.ISFL) GOTO 100
        IRP=IRPSOR(IF)
        IRPDIM=IDIM(IRP)
        ISFF=IF
        ISFL=IF+IRPDIM-1
        NBAS=INDSOR(IF+1)-INDSOR(IF)
        IND=INDSFT(ISFL)+1
        KND=IND+NBAS-1
C
        DO 110 ISYMOP=1,NSYMOP
C
          DO 120 ISF=ISFF,ISFL
C
C         -----------------------------------------------------------
C         OPERATE WITH SYMOP ISYMOP ON SYMFIE ISF; RESULT IN CFA, CFB
C         -----------------------------------------------------------
C
          INS=INDSOR(ISF)-IND+1
          DO 10 J=IND,KND
            IT=ICOMOP(J,ISYMOP)
            ITA=IABS(IT)
#if defined (BIT64)
            SN=CMPLX(IT/ITA)
#else
            SN=DCMPLX(IT/ITA)
#endif
            ITA=ITA-IND+1
            CFA(ITA)=SN*(CSYMORA(INS+J)*U2(1,ISYMOP)+
     +                   CSYMORB(INS+J)*U2(3,ISYMOP))
            CFB(ITA)=SN*(CSYMORA(INS+J)*U2(2,ISYMOP)+
     +                   CSYMORB(INS+J)*U2(4,ISYMOP))
 10         CONTINUE
C
C         -----------------------------------------------------
C         CALCULATE DOT PRODUCT OF ISF WITH ALL PARTNERS OF ISF
C         -----------------------------------------------------
C
          DO 20 JSF=ISFF,ISFL
            JNS=INDSOR(JSF)
            VPROD = XOVLAP(NBAS,IND,ICENT,ITYPE,CSYMORA(JNS+1),
     &                     CSYMORB(JNS+1),CFA,CFB)
            REPMAT(JSF-ISFF+1,ISF-ISFF+1)=VPROD
 20         CONTINUE
 120      CONTINUE
C
        IF (ISYMOP.EQ.1) THEN
          DO 500 ISF=1,IRPDIM
#if defined (BIT64)
            RNORM(ISF)=1.0D0/SQRT(ABS(REPMAT(ISF,ISF)))
#else
            RNORM(ISF)=1.0D0/DSQRT(CDABS(REPMAT(ISF,ISF)))
#endif
 500        CONTINUE
          ENDIF
        DO 510 ISF=1,IRPDIM
          DO 520 JSF=1,IRPDIM
            REPMAT(ISF,JSF)=REPMAT(ISF,JSF)*RNORM(ISF)*RNORM(JSF)
 520        CONTINUE
 510      CONTINUE
C
C       --------------------------------------------------------
C       NOW, REPMAT CONTAINS THE REPRESENTATION MATRIX FOR SYMOP
C       ISYMOP FOR REPRESENTATION IRP TO IRP+IRPDIM-1
C       IF FOUND FOR FIRST TIME : WRITE TO MFDSYMC
C       ELSE : CHECK WITH PREVIOUS MATRIX FOR INCONSISTENCIES
C       --------------------------------------------------------
C
        IF (PREP(IRP)) THEN
           IF ((PRNT.GE.2).and.(mpirnk.eq.0)) THEN
              WRITE(6,1010) IRP,ISYMOP,ISFF,ISFL
              DO 130 I=1,IRPDIM
                WRITE(6,1000) (REPMAT(I,J),J=1,IRPDIM)
 130          CONTINUE
           ENDIF
           WRITE(15) IRP,IRPDIM,ISYMOP,
     +       ((REPMAT(I,J),J=1,IRPDIM),I=1,IRPDIM)
           DO I = 1, IRPDIM
              DO J = 1, IRPDIM
                 PREPMAT(I,J,ISYMOP,IRP) = REPMAT(I,J)
              ENDDO
           ENDDO
C          ------------------------------------------
C          Test whether the representation is unitary
C          ------------------------------------------
#if defined (BIT64)
           CALL CGEMM ('C','N',IRPDIM,IRPDIM,IRPDIM,A1,
#else
           CALL ZGEMM ('C','N',IRPDIM,IRPDIM,IRPDIM,A1,
#endif
     &     PREPMAT(1,1,ISYMOP,IRP),4,PREPMAT(1,1,ISYMOP,IRP),4,
     &     A0,REPMAT,4)
           DO I = 1, IRPDIM
              DO J = 1, IRPDIM
#if defined (BIT64)
                 CDIF = CABS(REPMAT(I,J)-PREPMAT(I,J,1,IRP))
#else
                 CDIF = CDABS(REPMAT(I,J)-PREPMAT(I,J,1,IRP))
#endif
                 IF (CDIF.GT.1D-10) THEN
                    if (mpirnk.eq.0) then
                    WRITE(6,1010) IRP,ISYMOP,ISFF,ISFL
                    DO K=1,IRPDIM
                      WRITE(6,1000) (PREPMAT(K,L,ISYMOP,IRP),L=1,IRPDIM)
                    ENDDO
                    WRITE(6,'(/A)') ' Non-Unitary matrix : product is'
                    DO K=1,IRPDIM
                      WRITE(6,1000) (REPMAT(K,L),L=1,IRPDIM)
                    ENDDO
                    endif
                    CALL Q2JOB(6,'NON-UNITARY REP. MAT. FOUND',' ',0)
                 ENDIF
              ENDDO
           ENDDO
       ELSE
           DO I = 1, IRPDIM
              DO J = 1, IRPDIM
#if defined (BIT64)
                 CDIF = CABS(REPMAT(I,J)-PREPMAT(I,J,ISYMOP,IRP))
#else
                 CDIF = CDABS(REPMAT(I,J)-PREPMAT(I,J,ISYMOP,IRP))
#endif
                 IF (CDIF.GT.1D-10) THEN
                    if (mpirnk.eq.0) then
                    WRITE(6,1010) IRP,ISYMOP,ISFF,ISFL
                    DO K=1,IRPDIM
                      WRITE(6,1000) (REPMAT(K,L),L=1,IRPDIM)
                    ENDDO
                    WRITE(6,'(/A)') ' Standard representation matrix'
                    DO K=1,IRPDIM
                      WRITE(6,1000) (PREPMAT(K,L,ISYMOP,IRP),L=1,IRPDIM)
                    ENDDO
                    endif
                    CALL Q2JOB(6,'INCONSISTENT REP. MAT. FOUND',' ',0)
                 ENDIF
              ENDDO
           ENDDO
       ENDIF
C
 110    CONTINUE
        PREP(IRP) = .FALSE.
 100  CONTINUE
      CLOSE (15)
      RETURN
 1000 FORMAT(4('(',F8.3,',',F8.3,')',2x))
 1010 Format (' Representation',i4,' Operation',i4,' From symfie',i4,
     &' to',i4)
      END
