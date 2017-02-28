/*
 *      Filename: lheasoft-version-info.h
 *
 *   Description: Provides a mechanism for identifying the LHEA
 *                software release version with which this tool
 *                was distributed.
 *
 *    Change Log: See cvs log info at bottom
 *
 *   Author/Date: James Peachey, HEASARC/GSFC/NASA, 7 December, 1999
 */
#ifndef LHEASOFT_VERSION_INFO_H
#define LHEASOFT_VERSION_INFO_H 1

#define LHEASOFT_BUFSIZE 1024

/* Define version info conditionally, so that a source file may
 * override the default version information at compile-time.
 */
#ifndef LHEASOFT_VERSION
#define LHEASOFT_VERSION "HEASOFT Release Version 6.20"
#endif

void get_lheasoft_version_info(char* fulltoolname, char* msg);
void put_lheasoft_version_info(char* msg);

#endif

/******************************************************************************/
/* $Log: lheasoft-version-info.h,v $
/* Revision 3.30  2016/11/23 17:38:35  irby
/* Update version number to 6.20 for the upcoming release.
/*
/* Revision 3.29  2016/05/04 01:46:47  irby
/* Update version number string for 6.19 release.
/*
/* Revision 3.28  2016/02/03 16:54:27  irby
/* Update version number / release date strings for 6.18 release.
/*
/* Revision 3.27  2015/07/16 14:58:10  irby
/* Update heasoft version number to 6.17.
/*
/* Revision 3.26  2014/06/23 17:51:30  irby
/* Update version strings for HEASoft 6.16 release.
/*
/* Revision 3.25  2013/11/14 17:10:35  irby
/* Update release version to 6.15.
/*
/* Revision 3.24  2013/07/26 17:18:41  irby
/* Update version to 6.14.
/*
/* Revision 3.23  2012/12/13 21:11:53  irby
/* Update version number to 6.13.
/*
/* Revision 3.22  2012/03/12 20:55:46  irby
/* Update version number for HEASoft/FTOOLS v6.12 release.
/*
/* Revision 3.21  2011/04/07 20:59:02  irby
/* Update ftools version # to 6.11.
/*
/* Revision 3.20  2010/07/28 19:59:15  irby
/* Update version # to 6.10.
/*
/* Revision 3.19  2010/03/23 20:35:43  irby
/* Update version number for HEASOFT 6.9 release.
/*
/* Revision 3.18  2009/11/23 15:46:22  irby
/* Update version numbers for HEASoft 6.8 release.
/*
/* Revision 3.17  2009/08/14 17:54:04  irby
/* Update version number.
/*
/* Revision 3.16  2008/11/06 19:58:28  irby
/* Update version number for HEASoft 6.6 release.
/*
/* Revision 3.15  2008/06/23 20:40:22  irby
/* Update ftools version # to 6.5.
/*
/* Revision 3.14  2007/12/04 16:13:11  irby
/* Update version numbers for HEASOFT 6.4 release.
/*
/* Revision 3.13  2007/06/25 18:20:38  irby
/* Update HEASoft/FTools version number to 6.3.
/*
/* Revision 3.12  2007/03/05 16:41:36  irby
/* Update version numbers and release date for HEASoft 6.2.
/*
/* Revision 3.11  2006/07/19 19:16:46  irby
/* Update version numbers for 6.1 release, ~~ 20 July 2006.
/*
/* Revision 3.10  2005/04/01 19:13:02  irby
/* Update version number.
/*
/* Revision 3.9  2004/05/10 18:03:36  irby
/* Update version # to 5.3.1.
/*
/* Revision 3.8  2004/01/06 20:26:02  irby
/* Update to version 5.3...
/*
/* Revision 3.7  2001/07/20 14:38:13  irby
/* Update to release version 5.1.
/*
/* Revision 3.6  2000/07/19 17:42:23  peachey
/* Append compilation date automatically, and list version as 'develop
/* version'.
/*
/* Revision 3.5  2000/04/13 13:22:54  peachey
/* Updated to match most up-to-date version number.
/*
/* Revision 3.4  2000/02/25 13:08:20  peachey
/* Update release version
/*
/* Revision 3.3  1999/12/16 18:21:01  peachey
/* Changed global variable g_lheasoft_version to a static variable.
/* Added method put_lheasoft_version_info to change this variable,
/* and changed lheasoft_version_info to put_lheasoft_version_info.
/* Also provided more safety when putting together the output string.
/* */
/******************************************************************************/
