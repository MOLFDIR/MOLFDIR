/* 

  Fortran interface to the C malloc()/calloc() functions.  
  (patterned after the Stardent library routine falloc() )
  14-apr-92 -ron shepard

  Usage:

      real*8 work(1) ! define a reference array with elements of the
                     ! correct size to ensure correct byte alignment.
      integer nelem, elsize, clean, addr, offset
      elsize = 8  ! 8 for real*8 (see below).
      clean  = 0  ! don't bother to initialize to zero.

      call falloc( nelem, elsize, clean, work, addr, offset )

      if ( addr .ne. 0 ) then
         ! work(offset+1 : offset+nelem) is usable.
      else
         ! memory allocation error
      endif

  This function is required in the COLUMBUS Program System for the
  following machines:
  IBM RS6000
  FUJITSU VP2000
  FPS 500EA (optional, FPS may also use fdma.c and %val() convention)

*/

#include "extsymbols.h"  /* Define the external symbol: */
#include "stdio.h" 
void EXTERNAL_FALLOC( nelem, elsize, clean, work, addr, offset )

#if defined(DEC) || defined(SGI)
/* have to return integer*4 arguments as int and integer*8 arguments as long */
int  *nelem;   /* input: number of elements */
int  *elsize;  /* input: element size; 8 for real*8, 4 for integer*4, etc. */
int  *clean;   /* input: 1 to initialize the allocated space to zero.
                         0 to leave the allocated space unitialized. */
char *work;    /* input: work array as declared in the calling program */
int  *addr;    /* output: pointer to the allocated memory. 
                          0 for allocation error. */
int  *offset;  /* output: offset to be used in the calling program */
#else
long *nelem;   /* input: number of elements */
long *elsize;  /* input: element size; 8 for real*8, 4 for integer*4, etc. */
long *clean;   /* input: 1 to initialize the allocated space to zero.
                         0 to leave the allocated space unitialized. */
char *work;    /* input: work array as declared in the calling program */
long *addr;    /* output: pointer to the allocated memory. 
                          0 for allocation error. */
long *offset;  /* output: offset to be used in the calling program */
#endif

{
  char *dummy ;
  extern char *malloc();
  extern char *calloc();
  unsigned long tmp1,tmp2;
#if defined (SGI) || defined (DEC)
  int  elsizecpy; 
#else
  long elsizecpy;
#endif 
  

  /* allocate nelem+1 elements to allow for alignment */
#if defined (DEC) || defined (SGI)
  if ( *clean ) {
    *addr = (int) (dummy 
		    = calloc( (unsigned) (*nelem + 1), (unsigned) *elsize ) ) ;
  } else {
    *addr = (int) (dummy
		    = malloc( (unsigned) (*elsize * (*nelem + 1)) ) ) ;
  }
#else
  if ( *clean ) {
    *addr = (long) (dummy 
		    = calloc( (unsigned) (*nelem + 1), (unsigned) *elsize ) ) ;
  } else {
    *addr = (long) (dummy
		    = malloc( (unsigned) (*elsize * (*nelem + 1)) ) ) ;
  }
#endif

  /* 
    The following expressions are not strictly legal because pointer 
    arithmetic is being performed on different arrays.  However, this 
    seems to work alright on essentially all machines, and this routine 
    would be rather difficult to design without resorting to this tactic.
  */

   tmp1 = (unsigned long) dummy ;
   tmp2 = (unsigned long) work ;

   elsizecpy= *elsize ;
  if ( *elsize == 4  || *elsize == 8 )
  { tmp1 = tmp1 >> 2;
   tmp2 = tmp2 >> 2; 
   elsizecpy= elsizecpy / 4 ;}
  

  if ( tmp1 > tmp2 ) {
    *offset = ((long) tmp1 - (long) tmp2) / elsizecpy + 1 ;
  } else {
    /*
      this integer division must round towards zero.  In C, this 
      is only enforced for positive dividends. (K&R 1st Ed. p. 188,
      K&R 2nd Ed. p. 205, ANSI X3.159-1989 p. 47)  Therefore, the
      numerator has been rearranged to guarantee correct behavior.
    */
    *offset = -(((long) tmp2 - (long) tmp1) / elsizecpy) ; 
  }
#if defined(debug)
    printf(" falloc: dummy = %lu work = %lu offset = %ld \n",
   (unsigned long) dummy, (unsigned long) work, *offset );
#endif 
  
  return ;
}
