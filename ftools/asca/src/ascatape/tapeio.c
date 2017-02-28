/*****************************************************************************

  TAPEIO: General tape reading,writing and manipulation routines for Unix 
	  systems
  
  Author:  Don Jennings, Code 631, NASA/GSFC/HSTX
  Date:    4/15/94
  Version: 1.1

******************************************************************************

The TAPEIO module contains 18 functions:

  getTapePos      return the current tape position value
  openTapeDrive   opens a tape device for I/O, returns a pointer to the drive
  rewindTapeDrive rewinds tapedrive opened for I/O
  closeTapeDrive  closes an opened tapedrive
  verifyTapeDrive verify that a tape drive device exists on the system
  createDirectory creates a new directory for I/O
  createFile      creates a new file for I/O, returns a pointer to the file
  openFile        opens an existing file for I/O, returns a ponter to the file
  closeFile       closes a file opened for I/O
  compareFiles    byte by byte comparision of a tape file and a disk file
  readFile        reads current file from the tapedrive
  readBuffer      reads a buffer of data from an opened device
  writeFile       writes a given file to the tapedrive at the current position
  writeBuffer     writes a buffer of data to an opened file
  writeEOFmark    writes an End of File (EOF) marker at the current tape posit.
  writeEOTmark    writes an End of Tape (EOT) marker at the current tape posit.
  skipFiles       skips N files forward or backward on the tapedrive
  errorLookup     retrieves error message associated with passed error code

All functions except errorLookup return a value. If that value is negitive
then its absolute value represents a standard system error. The errorLookup
function will return the error string associated with that system error
if invoked.

Each utility has a fortran binder compatable function associated with it. The 
binder compatable function has the same name as the true function with a "_" 
character appended onto the end. The binder compatable functions also do not 
return a status value, but instead have an extra calling parameter called 
"status" that passes the return value back to the calling program. 
Additionally, all binder compatable functions that accept character strings 
assume that the passed strings have a '\0' null character appended to their 
end. Additionally, each utility has a CFORTRAN binder interface (if you do
not know what CFORTRAN is then do not worry about this).

The header file for this module, tapeio.h, has two #define macros that must
be set depending upon the configuration tapeio is to be used in. If tapeio
is to run on a solaris paltform then the #define SOLARIS statement should
be uncommented ("regular" BSD unix platforms want to have this statment
commented out). If tapeio is to be used in conjunction with the CFORTRAN
library then the #define CFORTRAN statement should be uncommented.

******************************************************************************

Revision History:

 04/12/94 D. Jennings, Program Modules completed.
 04/15/94 D. Jennings, Added fortran bindings to all functions.
 05/18/94 D. Jennings, Added cfortran preprocessor stuff.
 05/24/94 D. Jennings, Rewrote skipFiles to use read commands instead of
		       ioctl calls. Differences among various vender systems
		       made ioctl file skips too undependable.
 06/20/94 D. Jennings, added createDirectory function.
 06/28/94 D. Jennings, added writeBuffer function.
 08/02/94 D. Jennings, added code to make tapeio work on solaris 2.3 systems.
 08/02/94 D. Jennings, changed header structure of module to be usable in
                       XTE project.
 08/03/94 D. Jennings, added readBuffer function.
 08/03/94 D. Jennings, added getTapePos function.
 08/03/94 D. Jennings, added compareFiles function.

******************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#ifndef __APPLE__
#include <malloc.h> 
#endif
#include <sys/types.h>
#include <sys/uio.h>
#include <sys/mtio.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <limits.h>       /* definition of OPEN_MAX */
#include <sys/ioctl.h>
#include <errno.h>
#include "tapeio.h"

#define  MAXBUF 28800     /* maximum buffer size for readFile and writeFile */
#define  CREATE_MODE 420                              /* file creation mask */
#define  SKIP_FILE_BLOCK 28800   /* number of bytes to read in when skiping */

int tapepos   = 0;  /* global file position pointer; 0 ==> undefined postion */
int ejectFlag = 0;  /* global flag to eject tape upon rewind */

/*****************************************************************************/
/*
  getTapePos: returns the current tape position of the opened tape device
  as measured in file positions. If on the first file then the tape position
  is 1. If the tape drive device is not opened or if a serious positioning
  error has occured then getTapePos returns a negative value.
*/

int getTapePos()
{
  if(tapepos == 0) return(TIO_CORRUPT_TAPE_POS);
  else return(tapepos);
}
#ifdef CFORTRAN
FCALLSCFUN0(INT,getTapePos,GETTAPEPOS,gettapepos)
#endif
/*****************************************************************************/
/*
  The following is the fortran binder function to getTapePos
*/
void getTapePos_(tapePos)
int *tapePos;
{
 *tapePos = getTapePos();
}
/*****************************************************************************/
/*
  openTapeDrive: Opens a tapedrive for I/O. As input it requires the name
  of a system tape drive device (ie, /dev/rmt3, /dev/nrst4) and a open mode
  ( < 0 ==> read only, 0 ==> write only, > 0 ==> read and write). It returns
  a pointer to the drive if there were no errors, else it returns an error
  status code ( > 0 ==> no errors, returned pointer; < 0 ==> error occured)

  The global tapepos variable is set to 1 upon opening as well.

  INPUTS:

   char* drive        full path of input tape device name
   int   mode         < 0 ==> open drive for readonly
		      = 0 ==> open drive for writeonly
		      > 0 ==> open drive for read and write

  RETURN:

   int  drivePointer  positive int ==> pointer to open drive,
		      negitive int ==> error status

*/ 
int openTapeDrive(drive,mode)
char *drive;
int  mode;
{
  int  drivePointer;
  int  open_mode=0;
  char *ptr;

/*
  decide how to open the drive based upon value of mode parameter
*/
  if(mode <= TIO_OPEN_READ_ONLY)
    {
      open_mode = O_RDONLY;
    } 
  if(mode == TIO_OPEN_WRITE_ONLY)
    {
      open_mode = O_WRONLY;
    }
  if(mode >= TIO_OPEN_READ_WRITE)
    {
      open_mode = O_RDWR;
    }

 /* 
    use the OPEN command to get a file descriptor for desired system
    tape drive
 */

  drivePointer = open(drive,open_mode);
  
  /*
     check the output value of open to make sure tape drive has
     been properly accessed. If drivePointer is less than 0 then
     there was a problem ==> return errno value.
  */

    if(drivePointer < 0) return(-1*errno);
    else 
      {
	tapepos = 1;
	return(drivePointer);
      }
}
#ifdef CFORTRAN
FCALLSCFUN2(INT,openTapeDrive,OPENTAPEDRIVE,opentapedrive,PSTRING,INT)
#endif
/*****************************************************************************/
/*
  The following is the fortran binder function to openTapeDrive.
*/
void openTapeDrive_(drive,drive_size,mode,drivePointer,status)
char *drive;
int  *drive_size;
int  *mode;
int  *drivePointer;
int  *status;
{
  char *tmp_drive;
  tmp_drive = malloc(*drive_size+1);
  strncat(tmp_drive,drive,*drive_size);
  *drivePointer = openTapeDrive(tmp_drive,*mode);
  if(*drivePointer > 0) 
    {  
      *status = 0;
    }
  else 
    {
      *drivePointer = 0;
      *status = errno*-1;
    }
}  
/****************************************************************************/
/*
  rewindTapeDrive: rewinds an opened tape drive device. As input it takes
  a pointer to the tape device to rewind, and returns an error code (0 ==>
  no error, < 0 ==> error occured).

  INPUTS
  
    int   drivePointer  pointer to opened tape device

  RETURN

    int   err_code       0 ==> no error
		       < 0 ==> error occured
*/
int rewindTapeDrive(drivePointer)
int drivePointer;
{
  struct mtop mt_command;
  int    i;

  if(ejectFlag) mt_command.mt_op = MTOFFL;
  else mt_command.mt_op = MTREW;
  mt_command.mt_count = 1;

  i = ioctl(drivePointer,MTIOCTOP,&mt_command);
  if(i < 0) 
    {
      tapepos = 0;
      return(-1*errno);
    }
  else 
    {
      if(ejectFlag) tapepos = 0;
      else tapepos = 1;
      return(TIO_SUCCESS);
    }
}
#ifdef CFORTRAN
FCALLSCFUN1(INT,rewindTapeDrive,REWINDTAPEDRIVE,rewindtapedrive,INT)
#endif
/*****************************************************************************/
/*
  The following is the Fortran binder function to rewindTapeDrive
*/
void rewindTapeDrive_(drivePointer,status)
int *drivePointer;
int *status;
{
  *status = rewindTapeDrive(*drivePointer);
}
/*****************************************************************************/
/*
  closeTapeDrive: close an opened tape device. As input it takes a pointer
  to an opened tape device, and returns an error status (0 ==> no error,
  < 0 ==> error occured).

  INPUTS

    int  drivePointer  pointer to opened tape deivce

  RETURN
  
    int  err_code      0   ==> no error
		       < 0 ==> error occured
*/
int closeTapeDrive(drivePointer)
int drivePointer;
{
  int status;
  /*
    first, rewind the tape drive
  */
  status = rewindTapeDrive(drivePointer);
  if(status < 0) return status;
  /*
    now close the drivePointer
  */
  status = close(drivePointer);
  tapepos = 0;
  if(status < 0) return(-1*errno);
  else return(TIO_SUCCESS);
}
#ifdef CFORTRAN
FCALLSCFUN1(INT,closeTapeDrive,CLOSETAPEDRIVE,closetapedrive,INT)
#endif
/*****************************************************************************/
/*
  The following is the Fortran binder function to closeTapeDrive
*/
void closeTapeDrive_(drivePointer,status)
int *drivePointer;
int *status;
{
  *status = closeTapeDrive(*drivePointer);
}
/*****************************************************************************/
/*
  verifyTapeDrive: verifies that the passed drive name actually names a
  tape drive device file resident on the host system. As input it takes
  the name of a tape device file, and as output it returns an error status
  code (0 > ==> device exists and is usable, < 0 ==> problem).

  INPUT

    char* drive     name of the tape device file

  RETURN
  
    int   status         0 ==> no errors; device exists and is usable
		       < 0 ==> error status code
*/
int verifyTapeDrive(drive)
char *drive;
{
  int     status;
  u_short ctype;
  struct  stat buf;
/*
  call up the stats on the passed file name
*/
  status = stat(drive,&buf);
  if(status != 0) return(errno*-1);
/*
  make sure this file is a "character special" file
*/
  ctype = buf.st_mode & S_IFCHR;
  if(ctype != S_IFCHR) return(TIO_NOT_TAPE_DEVICE);
  else return(TIO_SUCCESS);
}
#ifdef CFORTRAN
FCALLSCFUN1(INT,verifyTapeDrive,VERIFYTAPEDRIVE,verifytapedrive,PSTRING)
#endif
/*****************************************************************************/
/*
  The following is the Fortran binder function to verifyTapeDrive
*/
void verifyTapeDrive_(drive,status)
char *drive;
int  *status;
{
  *status = verifyTapeDrive(drive);
}
/*****************************************************************************/
/*
  createDirectory: if the passed directory does not exist on the system
  then create it. As input createDirectory takes the name of a directory.
  As output it returns an error status code (0 ==> directory exists and
  is readable and writable and executable, < 0 ==> problem)

  INPUT

    char* directory   name of the desired directory

  RETURN
  
    int   status         0 ==> no errors; directory exists and is usable
		       < 0 ==> error status code
*/
int createDirectory(dir)
char *dir;
{
  int     status;
  u_short ctype;
  mode_t  mode;
  struct  stat buf;
/*
  call up the stats on the passed directory name
*/
  status = stat(dir,&buf);
  if(status != 0) 
    {
      if(errno == ENOENT)
	{
	  /* directory does not exist, so lets try to make it */
          mode = S_IRUSR | S_IWUSR | S_IXUSR;   
	  status = mkdir(dir,mode);
          if(status != 0) return (errno*-1);
	}
      else return(errno*-1);
    }
  else
    {
    /* file already exists; make sure it is a directory and is rwx by user */
      ctype = buf.st_mode & S_IFDIR;
      if(ctype != S_IFDIR) return(-20);
      ctype = buf.st_mode & S_IREAD;
      if(ctype != S_IREAD) return(TIO_NOT_READIBLE);
      ctype = buf.st_mode & S_IWRITE;
      if(ctype != S_IWRITE) return(TIO_NOT_WRITABLE);
      ctype = buf.st_mode & S_IEXEC;
      if(ctype != S_IEXEC) return(TIO_NOT_EXECUTABLE);
    }
  return(TIO_SUCCESS);
}
#ifdef CFORTRAN
FCALLSCFUN1(INT,createDirectory,CREATEDIRECTORY,createdirectory,PSTRING)
#endif
/*****************************************************************************/
/*
  The following is the Fortran binder function to createDirectory
*/
void createDirectory_(dir,status)
char *dir;
int  *status;
{
  *status = createDirectory(dir);
}
/*****************************************************************************/
/*
  createFile: opens a new disk file for I/O. As input it takes the name
  of the file to create and returns either a file pointer or an error
  status ( > 0 ==> file pointer returned, < 0 ==> error status code).

  INPUT

    char* fname  name of file to create

  RETURN
  
    int   filePointer  > 0 ==> valid file pointer
		       < 0 ==> error status code
*/   
int createFile(fname)
char *fname;
{
  int mode;
  int filePointer;

  mode        = O_RDWR | O_CREAT;
  filePointer = open(fname,mode,CREATE_MODE);

  if(filePointer < 0) return(-1*errno);
  else return(filePointer);
}
#ifdef CFORTRAN
FCALLSCFUN1(INT,createFile,CREATEFILE,createfile,PSTRING)
#endif
/*****************************************************************************/
/*
  The following is the Fortran binder function to createFile
*/
void createFile_(fname,filePointer,status)
char *fname;
int  *filePointer;
int  *status;
{  
  *status = createFile(fname);
  if(*status < 0)
    {
      *filePointer = 0;
      *status      = *status*-1;
    }
  else
    {
      *filePointer = *status;
      *status      = 0;
    }
}
/*****************************************************************************/
/*
  openFile: opens an existing file for I/O. As input it takes the name of
  an existing file to open and returns either a pointer to the opende file
  or an error status code ( > 0 ==> file pointer, < 0 ==> error status).

  INPUT

    char*  fname        name of file to open

  RETURN

    int    filePointer  > 0 ==> valid file pointer
			< 0 ==> error status code
*/
int openFile(fname)
char *fname;
{
  int mode;
  int filePointer;

  mode        = O_RDONLY;
  filePointer = open(fname,mode);

  if(filePointer < 0) return(-1*errno);
  else return(filePointer);
}
#ifdef CFORTRAN
FCALLSCFUN1(INT,openFile,OPENFILE,openfile,PSTRING)
#endif
/*****************************************************************************/
/*
  The following is the Fortran binder function to openFile
*/
void openFile_(fname,filePointer,status)
char *fname;
int  *filePointer;
int  *status;
{
  *status = openFile(fname);

  if(*status < 0)
    {
      *filePointer = 0;
      *status      = *status*-1;
    }
  else
    {
      *filePointer = *status;
      *status      = 0;
    }
}
/*****************************************************************************/
/*
  closeFile: closes and opened file. As input it takes a pointer to an opened
  disk file and returns an error status code (0 ==> no error, < 0 ==>
  error occured).

  INPUT

   int  filePointer  pointer to opened file

  RETURN

    int  err_code    0   ==> no errors
		     < 0 ==> error occured
*/
int closeFile(filePointer)
int filePointer;
{
  int status;

  status = close(filePointer);
  
  if(status < 0) return(-1*errno);
  else return(TIO_SUCCESS);
}
#ifdef CFORTRAN
FCALLSCFUN1(INT,closeFile,CLOSEFILE,closefile,INT)
#endif
/*****************************************************************************/
/*
  The following is the Fortran binder function to closeFile
*/
void closeFile_(filePointer,status)
int *filePointer;
int *status;
{
  *status = closeFile(*filePointer);
}
/*****************************************************************************/
/*
  compareFiles: compares two files byte by byte to make sure that they are
  the same. The first file is assumed to be on tape, the second file on
  disk. It returns 0 for a successful compare and less than zero if the
  two files are different.

  INPUT

   int  drivePointer pointer to opened tape drive at desried file
   int  filePointer  pointer to opened disk file

  RETURN

    int  err_code    0 ==> no errors
		   < 0 ==> error occured
*/
int compareFiles(drivePointer,filePointer)
int drivePointer;
int filePointer;
{
  int status = TIO_SUCCESS;
  int s1 = 1;
  int s2 = 1;
  char *buf1[SKIP_FILE_BLOCK+1];
  char *buf2[SKIP_FILE_BLOCK+1];
  struct mtop mt_command;

  while(s1 == s2 && s1 > 0 && s2 > 0 && status == TIO_SUCCESS)
    {
      s1 = read(drivePointer,buf1,SKIP_FILE_BLOCK);
      s2 = read(filePointer,buf2,SKIP_FILE_BLOCK);
      if(strncmp((const char *)buf1,(const char *)buf2,s1) != 0) status = TIO_FILES_NOT_EQUAL;
    }
  if(s1 != 0) while(read(drivePointer,buf1,SKIP_FILE_BLOCK) != 0);
#ifdef SOLARIS
  mt_command.mt_op    = MTFSF;
  mt_command.mt_count = 1;
  ioctl(drivePointer,MTIOCTOP,&mt_command);
#endif
  ++tapepos;
  if(s1 != 0 || s2 != 0) status = TIO_FILES_NOT_EQUAL;
  return(status);
}
#ifdef CFORTRAN
FCALLSCFUN2(INT,compareFiles,COMPAREFILES,comparefiles,INT,INT)
#endif
/*****************************************************************************/
/*
  The following is the Fortran binder function to compareFiles
*/
void compareFiles_(drivePointer,filePointer,status)
int *drivePointer;
int *filePointer;
int *status;
{
  *status = compareFiles(*drivePointer,*filePointer);
}
/*****************************************************************************/
/*
  readFile: reads the next file from an opened tape device and copies it to
  an opened disk file. As input it expects a pointer to an opened tape device,
  a pointer to an opened disk file and the tape device blocking factor. It
  returns a status error.

  INPUT

   int  drivePointer  pointer to opened tape device
   int  filePointer   pointeer to opened desination disk file
   int  blksize       blocksize of file on tape

  RETURN

   int  err_code        0 ==> no error
		      < 0 ==> error occured
*/
int readFile(drivePointer,filePointer,blksize)
int drivePointer;
int filePointer;
int blksize;
{
  int   i;
  int   j;
  int   status = TIO_SUCCESS;
  char *buf;
  struct mtop mt_command;

  buf = malloc(MAXBUF);

  if(tapepos == 0) return (TIO_CORRUPT_TAPE_POS);

  /*
     read the next file off the tape one block at a time and write it to
     the output file
  */

 i = 1;
 j = 1;
 while(j > 0 && (i = read(drivePointer,buf,blksize)) > 0 )
				    j = write(filePointer,buf,i);

  if(j < 0 || i < 0) 
    {
      tapepos = 0;
      status  = -1*errno;
    }
  else 
    {
      tapepos++;
#ifdef SOLARIS
      mt_command.mt_op    = MTFSF;
      mt_command.mt_count = 1;
      status = ioctl(drivePointer,MTIOCTOP,&mt_command);
      if(status < 0) status = -1*errno;
#endif
      return(status);
    }
}
#ifdef CFORTRAN
FCALLSCFUN3(INT,readFile,READFILE,readfile,INT,INT,INT)
#endif
/*****************************************************************************/
/*
  The following is the Fortran binder function to readFile
*/
void readFile_(drivePointer,filePointer,blksize,status)
int *drivePointer;
int *filePointer;
int *blksize;
int *status;
{
  *status = readFile(*drivePointer,*filePointer,*blksize);
}
/*****************************************************************************/
/*
  readBuffer: reads numBytes bytes from an opened tape device and places it 
  into the buffer buff. As input it expects a pointer to an opened tape device,
  a character buffer and the number of bytes to place in the buffer. If EOF
  is encountered before numBytes is read then readBuffer returns the number 
  of bytes actually read and skips to the next file; note that it does not
  continue reading from the next file to finish filling the buffer. It
  returns either the number of bytes read (positiv number) or a status error
  (negative number).

  INPUT

   int  drivePointer  pointer to opened tape device
   int  buffer        output buffer
   int  numbytes      number of bytes to read from device

  RETURN

   int  err_code      > 0 ==> number of bytes read
		      < 0 ==> error occured
*/

int readBuffer(drivePointer,buff,numBytes)
int  drivePointer;
char *buff;
int  numBytes;
{
  int   status = 1;
  int   i      = 0;
  int   pass   = 0;
  int   rem    = 0;
  int   count  = 0;
  char  *tmpBuf[SKIP_FILE_BLOCK+1];
  struct mtop  mt_command;

  if(tapepos == 0) return (TIO_CORRUPT_TAPE_POS);


  /*
     read a buffer of data from the current tape position one tape record at
     at time. Note that we do not attempt to cross file boundries!
  */

  pass = numBytes/SKIP_FILE_BLOCK;
  for(i = 0; i < pass && status > 0; ++i)
    {
      status = read(drivePointer,tmpBuf,SKIP_FILE_BLOCK);
      if(status < 0) status = -1*errno;
      else
	{
	  count += status;
	  if(strncpy(buff+i*SKIP_FILE_BLOCK,(const char *)tmpBuf,status) == NULL) 
	     status = TIO_STRING_COPY_ERROR;
	}
    }
  if(status > 0 && (rem = numBytes - count) != 0)
    {
      status = read(drivePointer,tmpBuf,SKIP_FILE_BLOCK);
      if(status < 0) status = -1*errno;
      else
	{
	  if(status < rem) rem = status; 
	  if(strncpy(buff+pass*SKIP_FILE_BLOCK,(const char *)tmpBuf,rem) == NULL) 
	    status = TIO_STRING_COPY_ERROR;
	  else count += rem;
	}
    }

  if(status == 0)
    {
      tapepos++;
#ifdef SOLARIS
      mt_command.mt_op    = MTFSF;
      mt_command.mt_count = 1;
      i = ioctl(drivePointer,MTIOCTOP,&mt_command);
#endif
    }
  if(status < 0) return(status);
  else return(count);
}
#ifdef CFORTRAN
FCALLSCFUN3(INT,readBuffer,READBUFFER,readbuffer,INT,PSTRING,INT)
#endif
/*****************************************************************************/
/*
  The following is the Fortran binder function to readBuffer
*/
void readBuffer_(drivePointer,buff,numBytes,status)
int  *drivePointer;
char *buff;
int  *numBytes;
int  *status;
{
  *status = readBuffer(*drivePointer,buff,*numBytes);
}
/*****************************************************************************/
/*
  writeFile: writes a file to the current position of the specified tape
  device. As input it expects a pointer to an opened tape device,
  a pointer to an opened disk file and the tape device blocking factor. It
  returns a status error.

  INPUT

   int  drivePointer  pointer to opened tape device
   int  filePointer   pointeer to opened desination disk file
   int  blksize       blocksize of file on tape

  RETURN

   int  err_code        0 ==> no error
		      < 0 ==> error occured
*/
int writeFile(drivePointer,filePointer,blksize)
int drivePointer;
int filePointer;
int blksize;
{
  int  i=0;
  int  j=0;
  int  status = TIO_SUCCESS;
  char *buf;

  buf = malloc(MAXBUF);

  if(tapepos == 0) return(TIO_CORRUPT_TAPE_POS);

  /*
     read the input file one block at a time and write it to
     the tapedrive
  */

  while(j > 0 && (i = read(filePointer,buf,blksize)) > 0 )
				    j = write(drivePointer,buf,i);
  if(j < 0 || i < 0) status = -1*errno;
  else 
    {
      i = writeEOFmark(drivePointer);
      if( i < 0) 
	{
	  tapepos = 0;
	  status = -1*errno;
	}
      else tapepos++;  
    }
  return(status);
}
#ifdef CFORTRAN
FCALLSCFUN3(INT,writeFile,WRITEFILE,writefile,INT,INT,INT)
#endif
/*****************************************************************************/
/*
  The following is the Fortran binder function to writeFile
*/
void writeFile_(drivePointer,filePointer,blksize,status)
int *drivePointer;
int *filePointer;
int *blksize;
int *status;
{
  *status = writeFile(*drivePointer,*filePointer,*blksize);
}
/*****************************************************************************/
/*
  writeBuffer: writes a buffer of data to the current position of the 
  file pointed to by the passed file pointer. As input it expects a pointer 
  to an opened file, a character buffer of data to write and the number of
  bytes to write.

  INPUT

   int  filePointer   pointer to opened desination disk file
   char buff          buffer of data to write
   int  buffSize      number of bytes to write

  RETURN

   int  err_code        0 ==> no error
		      < 0 ==> error occured
*/
int writeBuffer(filePointer,buff,buffSize)
int  filePointer;
char *buff;
int  buffSize;
{
  int   status;

  status = write(filePointer,buff,buffSize);
  if(status != buffSize) return(TIO_WRITE_ERROR);
  else return(TIO_SUCCESS);
}
#ifdef CFORTRAN
FCALLSCFUN3(INT,writeBuffer,WRITEBUFFER,writebuffer,INT,PSTRING,INT)
#endif
/*****************************************************************************/
/*
  The following is the Fortran binder function to writeBuffer
*/
void writeBuffer_(filePointer,buff,buffSize,status)
int  *filePointer;
char *buff;
int  *buffSize;
int  *status;
{
  *status = writeBuffer(*filePointer,buff,*buffSize);
}
/*****************************************************************************/
/*
  writeEOFmark: writes a single End Of File (EOF) tape mark to the current
  postion on tape. As input it takes a pointer to the tape device, and returns
  an error code (0 ==> no error, < 0 ==> error occured).

  INPUTS
  
    int   drivePointer  pointer to opened tape device

  RETURN

    int   err_code       0 ==> no error
		       < 0 ==> error occured
*/
int writeEOFmark(drivePointer)
int drivePointer;
{
  struct mtop mt_command;
  int    i;

  mt_command.mt_op    = MTWEOF;
  mt_command.mt_count = 1;

  i = ioctl(drivePointer,MTIOCTOP,&mt_command);
  if(i < 0) return(-1*errno);
  else return(TIO_SUCCESS);
}
#ifdef CFORTRAN
FCALLSCFUN1(INT,writeEOFmark,WRITEEOFMARK,writeeofmark,INT)
#endif
/*****************************************************************************/
/*
  The following is the Fortran binder function to writeEOFmark
*/
void writeEOFmark_(drivePointer,status)
int *drivePointer;
int *status;
{
  *status = writeEOFmark(*drivePointer);
}
/*****************************************************************************/
/*
  writeEOTmark: writes a double End Of File (EOF) tape mark to the current
  postion on tape, thus making an Eno Of Tape (EOT) mark. As input it takes 
  a pointer to the tape device, and returns an error code (0 ==> no error, < 
  0 ==> error occured).

  INPUTS
  
    int   drivePointer  pointer to opened tape device

  RETURN

    int   err_code       0 ==> no error
		       < 0 ==> error occured
*/
int writeEOTmark(drivePointer)
int drivePointer;
{
  struct mtop mt_command;
  int    i;

  mt_command.mt_op    = MTWEOF;
  mt_command.mt_count = 2;

  i = ioctl(drivePointer,MTIOCTOP,&mt_command);
  if(i < 0) return(-1*errno);
  else return(TIO_SUCCESS);
}
#ifdef CFORTRAN
FCALLSCFUN1(INT,writeEOTmark,WRITEEOTMARK,writeeotmark,INT)
#endif
/*****************************************************************************/
/*
  The following is the Fortran binder function to writeEOTmark
*/
void writeEOTmark_(drivePointer,status)
int *drivePointer;
int *status;
{
  *status = writeEOTmark(*drivePointer);
}
/*****************************************************************************/
/*
   skipFiles: skips N files forward or backwards on the specified tape device.
   As input it expects a pointer to an opened tape device, the number of
   files to skip and the direction (forward, backwards). It returns a status
   code.

  INPUTS

    int  drivePointer  pointer to opened tape device
    int  nfiles        number of files to skip
    int  direction     > 0 ==> skip forward from current position
		       < 0 ==> skip backward from current position 

  RETURN

    int  err_code      > 0 ==> no error
		       < 0 ==> error occured
*/
int skipFiles(drivePointer,nfiles,direction)
int drivePointer;
int nfiles;
int direction;
{
  struct mtop mt_command;
  int    i,j,skip;
  int    status = TIO_SUCCESS;
  char   *buf;

  i   = 0;
  buf = malloc(SKIP_FILE_BLOCK);
  
  if(tapepos == 0) return (TIO_CORRUPT_TAPE_POS);

  if(direction <= TIO_SKIP_BACKWARD)
    {
      skip = tapepos - nfiles - 1;
      if(skip < 0) status = TIO_BOT_SKIP;
      else
	{
	  i = rewindTapeDrive(drivePointer);
	  if(i < 0) 
	    {
	      tapepos = 0;
	      status = -1*errno;
	    }
	  else
	    {
              i = 0;
	      j = 1;
	      while(j <= skip && i >= 0)
		{
		  while((i = read(drivePointer,buf,SKIP_FILE_BLOCK)) > 0 );
#ifdef SOLARIS
		  mt_command.mt_op    = MTFSF;
		  mt_command.mt_count = 1;
		  i = ioctl(drivePointer,MTIOCTOP,&mt_command);
#endif
		  ++j;
		}
	      if(i < 0) 
		{
		  tapepos = 0;
		  status = TIO_READ_ERROR;
		}
	      else
		{
		  tapepos = tapepos + skip;
		}
	    }
	}
    }
  else
    {
      if(direction >= TIO_SKIP_FORWARD)
	{
#ifdef SOLARIS
	  if(read(drivePointer,buf,SKIP_FILE_BLOCK) < 1)
	    {
	      mt_command.mt_op    = MTBSR;
	      mt_command.mt_count = 1;
	      i = ioctl(drivePointer,MTIOCTOP,&mt_command);
	      if(i < 0) return(-1*errno);
	    }
#endif 
	  j = 1;
	  while(j <= nfiles && i >= 0)
	    {
	      while((i = read(drivePointer,buf,SKIP_FILE_BLOCK)) > 0 );
	      ++j;
#ifdef SOLARIS
	      mt_command.mt_op    = MTFSF;
	      mt_command.mt_count = 1;
	      i = ioctl(drivePointer,MTIOCTOP,&mt_command);
	      if(i < 0) status = -1*errno;
#endif
	    }
	  if(i < 0) 
	    {
	      tapepos = 0;
	      status = TIO_READ_ERROR;
	    }
	  else tapepos = tapepos + nfiles;
	}
      else status = TIO_UNKNOWN_SKIP_VALUE;
    }
  free(buf);
  return(status);
}
#ifdef CFORTRAN
FCALLSCFUN3(INT,skipFiles,SKIPFILES,skipfiles,INT,INT,INT)
#endif
/*****************************************************************************/
/*
  The following is the Fortran binder function to skipFiles
*/
void skipFiles_(drivePointer,nfiles,direction,status)
int *drivePointer;
int *nfiles;
int *direction;
int *status;
{
  *status = skipFiles(*drivePointer,*nfiles,*direction);
}

/*****************************************************************************/
/*
  errorLookup: associates error messages with system error return codes. As
  input it takes a standard system error code and returns (in the message
  parameter) a string that describes the error.

  INPUTS

    int   error    error status code

  OUTPUT
    
    char* message  error description string

  RETURN

   none

*/
void errorLookup(error,message)
int  error;
char *message;
{
  if(error > 0) error = error*-1;
  switch (error)
    {
    case -1:     strcpy(message,"Not owner"); break;
    case -2:     strcpy(message,"No such file or directory"); break;  
    case -3:     strcpy(message,"No such process"); break;
    case -4:     strcpy(message,"Interrupted system call"); break;  
    case -5:     strcpy(message,"I/O Error"); break;
    case -6:     strcpy(message,"No such device or address"); break;  
    case -7:     strcpy(message,"Arg list too long  "); break;
    case -8:     strcpy(message,"Exec format error  "); break;
    case -9:     strcpy(message,"Bad file number  "); break;
    case -10:    strcpy(message,"No children  "); break;
    case -11:    strcpy(message,"No more processes")  ; break;
    case -12:    strcpy(message,"Not enough core  "); break;
    case -13:    strcpy(message,"Permission denied ") ; break;
    case -14:    strcpy(message,"Bad address  "); break;
    case -15:    strcpy(message,"Block device required"); break;  
    case -16:    strcpy(message,"Mount device busy  "); break;
    case -17:    strcpy(message,"File exists  "); break;
    case -18:    strcpy(message,"Cross-device link")  ; break;
    case -19:    strcpy(message,"No such device  "); break;
    case -20:    strcpy(message,"Not a directory "); break;
    case -21:    strcpy(message,"Is a directory  "); break;
    case -22:    strcpy(message,"Invalid argument ") ; break;
    case -23:    strcpy(message,"File table overflow")  ; break;
    case -24:    strcpy(message,"Too many open files ") ; break;
    case -25:    strcpy(message,"Not a typewriter  "); break;
    case -26:    strcpy(message,"Text file busy  "); break;
    case -27:    strcpy(message,"File too large  "); break;
    case -28:    strcpy(message,"No space left on device") ; break; 
    case -29:    strcpy(message,"Illegal seek  "); break;
    case -30:    strcpy(message,"Read-only file system") ; break; 
    case -31:    strcpy(message,"Too many links  "); break;
    case -32:    strcpy(message,"Broken pipe  "); break;
/* The following are tapeio specific error codes */
    case TIO_NOT_TAPE_DEVICE :    strcpy(message,"Not a tape device"); break;
    case TIO_CORRUPT_TAPE_POS:  strcpy(message,"Corrupt Tape Postion"); break;
    case TIO_BOT_SKIP:   strcpy(message,"Cannot skip past tape start"); break;
    case TIO_NOT_READIBLE:     strcpy(message,"Not readable by owner"); break;
    case TIO_NOT_WRITABLE:     strcpy(message,"Not writable by owner"); break;
    case TIO_NOT_EXECUTABLE: strcpy(message,"Not executable by owner"); break;
    case TIO_WRITE_ERROR: strcpy(message,"Error writing to device"); break;
    case TIO_READ_ERROR:   strcpy(message,"Error reading from device"); break;
    case TIO_UNKNOWN_SKIP_VALUE: strcpy(message,"Unknown skip action"); break; 
    case TIO_FILES_NOT_EQUAL:   strcpy(message,"Files not equivalent"); break;
    case TIO_STRING_COPY_ERROR: strcpy(message,"String copy error"); break;

/* in case the error message is not defined here, return "unknown mesage" */
    default:    strcpy(message,"Unknown error");
    }
  return;
}
#ifdef CFORTRAN
FCALLSCSUB2(errorLookup,ERRORLOOKUP,errorlookup,INT,PSTRING)
#endif
/*****************************************************************************/
/*
  The following is the Fortran binder function to errorLookup
*/
void errorLookup_(error,message)
int  *error;
char *message;
{
  errorLookup(*error,message);
}
/*****************************************************************************/



