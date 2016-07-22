C
C     ========================================
      INTEGER FUNCTION INTTYP(IMM,JMM,KMM,LMM)
C     ========================================
C
      INTEGER IMM,JMM,KMM,LMM
C
C     --------------
C     DETERMINE TYPE
C     --------------
      IF (KMM.GT.JMM) THEN
         IF (JMM.GT.LMM) THEN
            IF (IMM.NE.KMM) THEN
               INTTYP = 13
               RETURN
            ELSE
               INTTYP = 10
               RETURN
            ENDIF
         ELSE
            IF (JMM.NE.LMM) THEN
               IF (KMM.NE.LMM) THEN
                  INTTYP = 14
                  RETURN
               ELSE
                  INTTYP = 8
                  RETURN
               ENDIF
            ELSE
               IF (IMM.NE.KMM) THEN
                  INTTYP = 11
                  RETURN
               ELSE
                  INTTYP = 2
                  RETURN
               ENDIF
            ENDIF
         ENDIF
      ELSE
         IF (KMM.NE.LMM) THEN
            IF (IMM.NE.JMM) THEN
               IF (JMM.NE.KMM) THEN
                  INTTYP = 12
                  RETURN
               ELSE
                  INTTYP = 9
                  RETURN
               ENDIF
            ELSE
               IF (JMM.NE.KMM) THEN
                  INTTYP = 5
                  RETURN
               ELSE
                  INTTYP = 4
                  RETURN
               ENDIF
            ENDIF
         ELSE
            IF (JMM.NE.KMM) THEN
               IF (IMM.NE.JMM) THEN
                  INTTYP = 7
                  RETURN
               ELSE
                  INTTYP = 3
                  RETURN
               ENDIF
            ELSE
               IF (IMM.NE.JMM) THEN
                  INTTYP = 6
                  RETURN
               ELSE
                  INTTYP = 1
                  RETURN
               ENDIF
            ENDIF
         ENDIF
      ENDIF
      PRINT*, ' ERROR DETERMINING INTEGRAL TYPE '
      PRINT*, 'IMM, JMM, KMM, LMM = ', IMM,JMM,KMM,LMM
      RETURN
      END
