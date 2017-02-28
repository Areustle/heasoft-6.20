/* 
   define the SOLARIS flag if this code is being compiled for use on a
   solaris 2.3 or higher platform
*/

/* #define SOLARIS */

/*
  if lower case solaris flag is defined (by an external process) then  
  define SOLARIS automatically
*/

#ifdef solaris
#define SOLARIS
#endif

/*
   define the CFORTRAN flag if this code is being used with the cfortran.h
   header file
*/

#define CFORTRAN 

#ifdef CFORTRAN
#include "cfortran.h"
#endif

/* prototype functions depending upon compliler type (c++, ANSI c, K&R c) */

#ifndef __cplusplus
#ifndef __STDC__
  int getTapePos();
  int openTapeDrive();
  int rewindTapeDrive();
  int closeTapeDrive();
  int verifyTapeDrive();
  int createDirectory();
  int createFile();
  int openFile();
  int closeFile();
  int compareFiles();
  int readFile();
  int readBuffer();
  int writeFile();
  int writeBuffer();
  int writeEOFmark();
  int writeEOTmark();
  int skipFiles();
  void errorLookup();
#else
  int getTapePos(void);
  int openTapeDrive(char *drive, int mode);
  int rewindTapeDrive(int drivePointer);
  int closeTapeDrive(int drivePointer);
  int verifyTapeDrive(char *drive);
  int createDirectory(char *dir);
  int createFile(char *fname);
  int openFile(char *fname);
  int closeFile(int filePointer);
  int compareFiles(int drivePointer,int filePointer);
  int readFile(int drivePointer,int filePointer,int blksize);
  int readBuffer(int drivePointer, char *buff,int numBytes);
  int writeFile(int drivePointer,int filePointer,int blksize);
  int writeBuffer(int filePointer,char *buff,int buffSize);
  int writeEOFmark(int drivePointer);
  int writeEOTmark(int drivePointer);
  int skipFiles(int drivePointer,int nfiles,int direction);
  void errorLookup(int error,char *message);
#endif
#else
extern "C" 
{
  int getTapePos(void);
  int openTapeDrive(char *drive, int mode);
  int rewindTapeDrive(int drivePointer);
  int closeTapeDrive(int drivePointer);
  int verifyTapeDrive(char *drive);
  int createDirectory(char *dir);
  int createFile(char *fname);
  int openFile(char *fname);
  int closeFile(int filePointer);
  int compareFiles(int drivePointer,int filePointer);
  int readFile(int drivePointer,int filePointer,int blksize);
  int readBuffer(int drivePointer, char *buff,int numBytes);
  int writeFile(int drivePointer,int filePointer,int blksize);
  int writeBuffer(int filePointer,char *buff,int buffSize);
  int writeEOFmark(int drivePointer);
  int writeEOTmark(int drivePointer);
  int skipFiles(int drivePointer,int nfiles,int direction);
  void errorLookup(int error,char *message);
}
#endif

/* define TAPEIO specific error codes */

#define TIO_SUCCESS                0
#define TIO_NOT_TAPE_DEVICE    -1001
#define TIO_CORRUPT_TAPE_POS   -1002
#define TIO_BOT_SKIP           -1003
#define TIO_NOT_READIBLE       -1004
#define TIO_NOT_WRITABLE       -1005
#define TIO_NOT_EXECUTABLE     -1006
#define TIO_WRITE_ERROR        -1007
#define TIO_READ_ERROR         -1008
#define TIO_UNKNOWN_SKIP_VALUE -1009
#define TIO_FILES_NOT_EQUAL    -1010
#define TIO_STRING_COPY_ERROR  -1011

/* define TAPEIO constants */

#define TIO_OPEN_READ_ONLY -1
#define TIO_OPEN_WRITE_ONLY 0
#define TIO_OPEN_READ_WRITE 1
#define TIO_SKIP_FORWARD    1
#define TIO_SKIP_BACKWARD  -1


