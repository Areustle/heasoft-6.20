#include "cfortran.h"
#ifdef g77Fortran
void putc_(char *chr_ptr)
 
/* Write a single character to the terminal.
 *
 * chr_ptr   Input   The character to be written
 */
{
   write(0,&chr_ptr[0],1);
   return;
}
#endif
