/*
 *      Filename: lheasoft-version-info.c
 *
 *   Description: Provides a mechanism for identifying the LHEASOFT
 *                software release version with which this tool
 *                was distributed.
 *
 *         Usage: Methods are provided for put/get-ting the
 *                version information I/O of a static string.
 *
 *    Change Log: See cvs log info at bottom
 *
 *   Author/Date: James Peachey, HEASARC/GSFC/NASA, 7 December, 1999
 */
#include <stdio.h>
#include <string.h>

#define LHEASOFT_INSTANTIATE 1
#include "lheasoft-version-info.h"

static char lheasoft_version[LHEASOFT_BUFSIZE] = LHEASOFT_VERSION;

void get_lheasoft_version_info(char* fulltoolname, char* msg) {
  char date[] = __DATE__;
/* variable to select the filename portion of the full tool name */
  char* toolname = NULL;
  char* tmpptr = NULL;

/* check for bad inputs */
  if(!fulltoolname || !msg) return;

/* find end of directory part of tool name */
  toolname = strrchr(fulltoolname, '/');

/* if toolname is null, it means rindex didn't find a /, so just
 * use the full tool name; otherwise, increment toolname, to skip
 * the / itself */
  if(!toolname++) toolname = fulltoolname;

/* convert toolname to uppercase */
  tmpptr = toolname + strlen(toolname);
  while(--tmpptr >= toolname) { *tmpptr = toupper(*tmpptr); }
  
/* finally, compose a nice message */
  strncat(msg, toolname, LHEASOFT_BUFSIZE - 1);
  strncat(msg, " was built for release with: ",
          LHEASOFT_BUFSIZE - strlen(msg) - 1);
  strncat(msg, lheasoft_version, LHEASOFT_BUFSIZE - strlen(msg) - 1);
  strncat(msg, " on ", LHEASOFT_BUFSIZE - strlen(msg) - 1);
  strncat(msg, date, LHEASOFT_BUFSIZE - strlen(msg) - 1);

  return;
}

void put_lheasoft_version_info(char* msg) {
  strncpy(lheasoft_version, msg, LHEASOFT_BUFSIZE - 1);
  return;
}

/******************************************************************************/
/* $Log: lheasoft-version-info.c,v $
/* Revision 3.4  2000/09/20 20:02:04  peachey
/* Cosmetic change to version message.
/*
/* Revision 3.3  2000/07/19 17:42:23  peachey
/* Append compilation date automatically, and list version as 'develop
/* version'.
/*
/* Revision 3.2  1999/12/16 18:21:01  peachey
/* Changed global variable g_lheasoft_version to a static variable.
/* Added method put_lheasoft_version_info to change this variable,
/* and changed lheasoft_version_info to put_lheasoft_version_info.
/* Also provided more safety when putting together the output string.
/* */
/******************************************************************************/
