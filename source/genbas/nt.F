C     ===========================
      INTEGER FUNCTION NT (ITYPX)
C     ===========================
C
      IF     (ITYPX.EQ.1) THEN
                               NT = 0
      ELSEIF (ITYPX.LE.4) THEN
                               NT = 1
      ELSEIF (ITYPX.LE.10) THEN
                               NT = 2
      ELSEIF (ITYPX.LE.20) THEN
                               NT = 3
      ELSEIF (ITYPX.LE.35) THEN
                               NT = 4
      ELSEIF (ITYPX.LE.56) THEN
                               NT = 5
      ELSE
             CALL Q2JOB (6,'WRONG TYPE IN NT',' ',ITYPX)
      ENDIF
      RETURN
      END
