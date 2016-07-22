#if defined (LINUX)
#include <sys/types.h>
#include <time.h>

/*
  Routine to return to FORTRAN the current date in
  same format as the C routine ctime.

  character*(*) date
  call util_date(date)
*/


void util_date__(char *date, int nlen)
{
  time_t t = time((time_t *) 0);
  char *tmp = ctime(&t);

  (void) string_to_fortchar(date, nlen, tmp);
}
#endif

int fortchar_to_string(const char *f, int flen, char *buf, const int buflen)
{
  while (flen-- && f[flen] == ' ')
          ;

  if (flen < 0) flen=0;         /* Empty strings break use of strtok
                                   since consecutive separators are
                                   treated as one */
  if ((flen+1) >= buflen)
    return 0;                   /* Won't fit */

  flen++;
  buf[flen] = 0;
  while(flen--)
    buf[flen] = f[flen];

  return 1;
}
int string_to_fortchar(char *f, int flen, char *buf)
{
  int len = (int) strlen(buf), i;
  if (len > flen)
  return 0;                   /* Won't fit */
  for (i=0; i<len; i++)
    f[i] = buf[i];
  for (i=len; i<flen; i++)
    f[i] = ' ';
  return 1;
}
