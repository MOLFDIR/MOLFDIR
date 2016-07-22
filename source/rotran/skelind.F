      SUBROUTINE SKELIND (NSP,NSYMRP,MOFSET,INDKLP,INDKL)
C
C     Generates index array to reorder the skeleton integrals for
C     the transformation to the reduced matrix elements
C     Written august 1994 by Luuk Visscher
C
      INTEGER MOFSET(NSYMRP+1),INDKLP(NSYMRP,NSYMRP,2),INDKL(*)
C
      IND = 1
      DO IA = 1, NSYMRP
         DO JA = 1, NSYMRP
            INDKLP(IA,JA,1) = IND
            DO I = MOFSET(IA)+1, MOFSET(IA+1)
               DO J = MOFSET(JA)+1, MOFSET(JA+1)
                  JI = (J-1)*NSP + I 
                  INDKL(IND) = JI
                  IND = IND + 1
               ENDDO
            ENDDO
         ENDDO
      ENDDO
      DO IA = 1, NSYMRP
         DO JA = 1, NSYMRP
            INDKLP(IA,JA,2) = IND
            DO I = MOFSET(IA)+1, MOFSET(IA+1)
               DO J = MOFSET(JA)+1, MOFSET(JA+1)
                  IJ = (I-1)*NSP + J 
                  INDKL(IND) = IJ
                  IND = IND + 1
               ENDDO
            ENDDO
         ENDDO
      ENDDO
      RETURN
      END
