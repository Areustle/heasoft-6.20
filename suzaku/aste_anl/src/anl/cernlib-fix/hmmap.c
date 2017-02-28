/*CMZ :  4.20/04 05/08/93  15.06.17  by  Rene Brun*/
/*-- Author :    Fons Rademakers   20/03/91*/

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>
#include <errno.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/mman.h>
#include <sys/stat.h>

#ifndef MAP_FILE
#define MAP_FILE	(0)
#endif

static int  fd_pawc;
static int len;

/***********************************************************************
 *                                                                     *
 *   Create a memory mapped file and map common on it.                 *
 *   mfile         the filename                                        *
 *   mflen         length of filename                                  *
 *   base_common   common that should be mapped to file                *
 *   size          length of common in 32 bit words                    *
 *   comaddr       starting address of the mapping on output           *
 *                                                                     *
 ***********************************************************************/
int hcreatei_(mfile, mflen, base_common, size, comaddr)
  char *mfile;
  int *mflen, *base_common, *size;
  unsigned long *comaddr;
{
   caddr_t         paddr;
   char           *file, *buf;
   int             istat;
   unsigned long   inter;

   len = *size * 4;
#if 0
   file = calloc(*mflen+6, 1);
   strcpy(file, "/tmp/");
   strncat(file, mfile, *mflen);
#else
   file = calloc(*mflen+1, 1);
   strncpy(file, mfile, *mflen);
#endif

   buf = calloc(len, 1);

   fd_pawc = open(file,O_RDWR|O_CREAT,0666);
   free(file);
   if (fd_pawc < 0) {
      perror("open");
      istat = -errno;
      free(buf);
   } else
     {
      /* reserve space in file */
      istat = write(fd_pawc, buf, len);
      free(buf);
      if (istat != len) {
        istat = -1;
        close(fd_pawc);
        return(istat);
      }
      paddr = mmap((caddr_t) base_common + 0x10000000, len,
                   PROT_READ|PROT_WRITE, MAP_FILE|MAP_SHARED, fd_pawc, 0);
      if (paddr == NULL) {
         perror("mmap");
         istat = -errno;
         close(fd_pawc);
      } else {
         istat    = 0;
         inter    = (unsigned long) paddr;
         *comaddr = (inter >> 2);
      }
    }
  return(istat);
}

/***********************************************************************
 *                                                                     *
 *   Map common to file.                                               *
 *   mfile         the filename                                        *
 *   mflen         length of filename                                  *
 *   base_common   common that should be mapped to file                *
 *   size          length of common in 32 bit words                    *
 *   comaddr       starting address of the mapping on output           *
 *                                                                     *
 ***********************************************************************/
int hmapi_(mfile, mflen, base_common, comaddr)
  char *mfile;
  int *mflen, *base_common;
  unsigned long *comaddr;
{
   caddr_t         paddr;
   unsigned long   inter;
   int             istat;
   char           *file;
   struct stat     buf;

#if 0
   file = calloc(*mflen+6, 1);
   strcpy(file, "/tmp/");
   strncat(file, mfile, *mflen);
#else
   file = calloc(*mflen+1, 1);
   strncpy(file, mfile, *mflen);
#endif

   fd_pawc = open(file,O_RDWR);
   if (fd_pawc < 0 ) {
      perror("open");
      istat = -errno;
   } else {
      stat(file,&buf);
      len = buf.st_size;
      paddr = mmap((caddr_t) base_common, len,
                   PROT_READ|PROT_WRITE, MAP_FILE|MAP_SHARED, fd_pawc, 0);
      if (paddr == NULL) {
         perror("mmap");
         istat = -errno;
         close(fd_pawc);
      } else {
         istat    = 0;
         inter    = (unsigned long) paddr;
         *comaddr = (inter >> 2);
      }
   }
   free(file);
   return(istat);
}

/***********************************************************************
 *                                                                     *
 *   Unmap common and close file.                                      *
 *   comaddr       address of common that should be unmapped           *
 *                                                                     *
 ***********************************************************************/
int hfreem_(comaddr)
  int *comaddr;
{
   int istat;

   istat = munmap((caddr_t) *comaddr, len);
   if (istat == -1) {
      perror("munmap");
      istat = -errno;
   }
   close(fd_pawc);
   return(istat);
}
