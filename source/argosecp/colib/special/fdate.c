/*
  Fortran interface to the standard C ctime() function.
  27-apr-92 -ron shepard

  Usage:

      character*24 cdate ! variable of at least 24 characters.
      character*24 fdate ! external function.
      external     fdate

      cdate = fdate()    ! assigns current date and time values.

  This function is required in the COLUMBUS Program System for the
  following machines:
  IBM RS6000

*/

#include "extsymbols.h"  /* Define the external symbol: */

#include <time.h>

#ifndef NULL
#define NULL    0
#endif

#define LENGTH 24

#if defined(CRAY)
  /* fortran characters are passed by descriptor */
#include <fortran.h>
void EXTERNAL_FDATE( fdesc )
_fcd fdesc ; /* descriptor */
#else
  /* fortran expands the character parameters automatically */
void EXTERNAL_FDATE( fstring, flen )
char *fstring ; /* string address */
int  flen     ; /* string length  */
#endif

{
  char *external ;
  long int length ;
  char   *internal ;
  time_t temp ;
  int i ;

  /* localize the string address */
#if defined(CRAY)
  /* _fcdtocp() is a macro that extracts the address bits from fdesc */
  external = _fcdtocp( fdesc ) ;
#else
  external = fstring ;
#endif

  temp = time( (time_t *)NULL ) ;
  internal = ctime( &temp ) ;

  /* copy the first LENGTH chars to the output result */
  for( i=0; i < LENGTH; i++ )
    *(external+i) = *(internal+i) ;
  
  return ;
}
