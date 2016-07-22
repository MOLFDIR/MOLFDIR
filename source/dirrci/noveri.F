C
      INTEGER FUNCTION NOVERI (N,I)
C
      II = MIN0(I,N-I)
      NI = 1
      DO 1 J = 0, II - 1
    1 NI = (N-J) * NI
      DO 2 J = 1, II
    2 NI = NI / J
      NOVERI = NI
C
      RETURN
      END
