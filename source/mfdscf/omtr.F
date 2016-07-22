C
C     ===============================================================
      SUBROUTINE OMTR (NL, NR, N, AN, AR, AI, BN, BR, BI, CN, CR, CI)
C     ===============================================================
      IMPLICIT REAL*8 (A-H, O-Z)
C
      INTEGER AN, BN, CN
      REAL*8 BR (BN, BN), BI (BN, BN)
      REAL*8 CR (CN, CN), CI (CN, CN)
      REAL*8 AR (AN, AN), AI (AN, AN)
      DO I=1,N
      DO J=1,NL
         IF (DABS(AI(I,J)).GT.1.D-12) GOTO 100         
      ENDDO
      ENDDO
      AMX=0.0D0
      DO I=1,N
      DO J=1,NR
         IF (DABS(BI(I,J)).GT.1.D-12) GOTO 100           
      ENDDO
      ENDDO
      DO 10 I = 1, NL
      DO 20 J = 1, NR
      CR (I, J) = 0.0D0
      CI (I, J) = 0.0D0
      DO 30 K = 1, N
      CR (I, J) = CR (I, J) + AR (K, I) * BR (K ,J)
   30 CONTINUE
   20 CONTINUE
   10 CONTINUE
      RETURN
  100 CONTINUE
      DO 110 I = 1, NL
      DO 120 J = 1, NR
      CR (I, J) = 0.0D0
      CI (I, J) = 0.0D0
      DO 130 K = 1, N
      CR (I, J) = CR (I, J) +
     +            AR (K, I) * BR (K ,J) - AI (K, I) * BI (K, J)
      CI (I, J) = CI (I, J) +
     +            AR (K, I) * BI (K, J) + AI (K, I) * BR (K, J)
  130 CONTINUE
  120 CONTINUE
  110 CONTINUE
      RETURN
      END
