      Subroutine DayTime (Datum,Tijd)
      Character*10 Datum,Tijd*8,String*26
#if defined (CRAY) || defined (HP9) || defined (DEC) || defined (SGI)
      Call Date(Datum)
      Call Time(Tijd)
#endif
#if defined (LINUX)
      Call fdate(String)
      Do i = 1, 10
         Datum(i:i) = String(i:i)
      EndDo
      Do i = 1, 8
         Tijd(i:i) = String(i+11:i+11)
      EndDo
#endif
#if defined (SUN)
      Call fdate(String)
      Do i = 1, 10
         Datum(i:i) = String(i:i)
      EndDo
      Do i = 1, 8
         Tijd(i:i) = String(i+11:i+11)
      EndDo
#endif
#if defined (IBM) 
      Call fdate_ (String)
      Do i = 1, 10
         Datum(i:i) = String(i:i)
      EndDo
      Do i = 1, 8
         Tijd(i:i) = String(i+11:i+11)
      EndDo
#endif
      Return
      End
