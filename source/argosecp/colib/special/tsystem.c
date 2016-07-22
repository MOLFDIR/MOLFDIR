#if defined(CRAY)
/* fortran character strings are passed by descriptor */
#include <fortran.h>
#include <stdlib.h>
int EXTERNAL_TSYSTEM( fdesc )
_fcd fdesc ; /* fortran character descriptor */
{
  char *name ;  /* pointer to the fortran string */
  int lenname ; /* fortran-generated length value */
  int i, ierr ;

  /* localize the string address and argument length */

  /* _fcdtocp() and _fcdlen() are macros that extract the string address
     bits and string length respectively from fdesc */
  name    = _fcdtocp( fdesc ) ;
  lenname = _fcdlen( fdesc ) ;
  ierr = system(name);


  return( ierr ) ;
}
#endif /* _CRAY */
