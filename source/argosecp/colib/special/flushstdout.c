#include <stdio.h>
#include "extsymbols.h"  /* Define the external symbol: */

void EXTERNAL_FLUSHSTDOUT() 
{
   fflush(stdout);
}
