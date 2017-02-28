/**************************************************************************************

	I N T E G R A L   S C I E N C E   D A T A   C E N T E R

	   M A K E F I L E   S U P P O R T   R O U T I N E S

Copyright:	(c) 1998 ISDC http://obswww.unige.ch/isdc/
File:		isdc_install.c
Description:	ISDC's own version of BSD style install program
Authors:	Jerzy.Borkowski@obs.unige.ch (JB)
History:	07-Aug-98 (JB) : initial release together with new set
			of Makefiles and configure scripts.
		05-Feb-99 (JB) : try to unlink file first, before overwritting
			output file
		08-Feb-99 (JB) : always write to dest dir, even if input file name
			is not in current dir.
		07-Dec-99 (JB) : changed command line format.
		24-Jun-02 (JB) : SPR 01569 (warning fixes)

**************************************************************************************/


#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <string.h>
#include <unistd.h>
#include <pwd.h>
#include <grp.h>


#define	BUFSIZE			(8192)
#define	MY_PATH_MAX		(1024)

#define	II_OK			(0)
#define	II_BAD_MODE		(50)
#define	II_CANT_INSTALL		(51)
#define	II_NUL_PTR		(52)
#define	II_BAD_ARG		(53)
#define	II_FOPEN_FAILED		(54)
#define	II_FREAD_FAILED		(55)
#define	II_FWRITE_FAILED	(56)
#define	II_FCHMOD_FAILED	(57)
#define	II_FCLOSE_FAILED	(58)
#define	II_MKDIR_FAILED		(59)
#define	II_GETPWNAM_FAILED	(60)
#define	II_GETGRNAM_FAILED	(61)
#define	II_CHOWN_FAILED		(62)

#define	II_PARSER_DEFAULT		(0)
#define	II_PARSER_FILE_OCTAL_MODE	(1)
#define	II_PARSER_DIRNAME		(2)
#define	II_PARSER_DIR_OCTAL_MODE	(3)
#define	II_PARSER_USERNAME		(4)
#define	II_PARSER_GROUPNAME		(5)
#define	II_PARSER_ERROR_MODE		(6)


uid_t	ii_uid = -1;
gid_t	ii_gid = -1;
int	ii_file_mode = 0644;
int	ii_dir_mode = 0755;
char	ii_dir[MY_PATH_MAX] = "";
int	ii_errors = 1;				/* 1 - fail on error, 0 - continue with next file */
int	ii_quiet_mode = 0;

char	ii_at_least_1_installed = 0;
char	*ii_argv0 = NULL;


void	ii_usage(void)
 {
   printf("\nusage :\n\n");
   printf("  %s file_or_option [file_or_option] [file_or_option] ...\n", ii_argv0);
   printf("\nfile_or_option is either name of file to be installed or :\n\n");
   printf("  -c   (create directory specified by previous -d and/or -M, -u, -g)\n");
   printf("  -d install_directory   (has to be specified at least once)\n");
   printf("  -e integer   (nonzero - fail on first error, 0 - continue with next file)\n");
   printf("  -g group_id_or_name   (chgrp to group_name, default -1 disables chgrp)\n");
   printf("  -m octal_mode   (create mode for files, default is 0644)\n");
   printf("  -M octal_mode   (create mode for directories, default is 0755)\n");
   printf("  -q   (be quiet when no files to install given in command line)\n");
   printf("  -u user_id_or_name   (chown to user_name, default -1 disables chown)\n");
   printf("\nThe same option can be specified many times. Any option\n");
   printf("is effective only for subsequent files and directories.\n");
   printf("\nauthor: Jerzy.Borkowski@obs.unige.ch\n");
   printf("\n");
 }


int	ii_mkdir(char *dirname)
 { char		token_dir[MY_PATH_MAX];
   char		try_dir[2 * MY_PATH_MAX];
   char		try_dir_slash_dot[2 * MY_PATH_MAX];
   struct stat	statbuf;
   int		first;
   char		*p;

   if (NULL == dirname)
     { printf("ERROR (in ii_mkdir): invalid (null ptr) directory name\n");
       return(II_NUL_PTR);
     }

   if (0 == dirname[0])
     { printf("ERROR (in ii_mkdir): invalid (empty) directory name\n");
       return(II_BAD_ARG);
     }

   strncpy(token_dir, dirname, MY_PATH_MAX);
   token_dir[MY_PATH_MAX - 1] = 0;

   if ('/' == token_dir[0])  strcpy(try_dir, "/");  /* abs path name */
   else strcpy(try_dir, "");			/* relative pathname */

   for (first = 1;; first = 0)
    { p = strtok((first ? token_dir : NULL), "/");
      if (NULL == p) break;			/* all done */

      if (!first) strcat(try_dir, "/");
      strcat(try_dir, p);			/* current partial dir */ 
      strcpy(try_dir_slash_dot, try_dir);
      strcat(try_dir_slash_dot, "/.");		/* the same but with /. at the end */

      if (0 == stat(try_dir_slash_dot, &statbuf)) /* does the directory component exist ? */
        { if (S_IFDIR & statbuf.st_mode) continue;  /* this directory level already exists */
          printf("ERROR (in ii_mkdir): path component %s is not a directory\n", try_dir_slash_dot);
          return(II_MKDIR_FAILED);		/* component is not a dir (file ? symlink ?) */
        }
      else					/* have to create this level */
        { if (mkdir(try_dir, ii_dir_mode))
            { printf("ERROR (in ii_mkdir): cannot create directory %s\n", try_dir);
              return(II_MKDIR_FAILED);
            }
	  if ((-1 != ii_uid) || (-1 != ii_gid))
	    { if (chown(try_dir, ii_uid, ii_gid))
                { printf("ERROR (in ii_mkdir): cannot chown directory %s to uid %d, gid %d\n", try_dir, ii_uid, ii_gid);
                  if (ii_errors) return(II_CHOWN_FAILED);
                }
            }
        }
    }

   if ((-1 != ii_uid) || (-1 != ii_gid))
     { strcpy(try_dir_slash_dot, try_dir);
       strcat(try_dir_slash_dot, "/.");		/* the same but with /. at the end */
       if (chown(try_dir_slash_dot, ii_uid, ii_gid))
         { printf("ERROR (in ii_mkdir): cannot chown directory %s to uid %d, gid %d\n", try_dir_slash_dot, ii_uid, ii_gid);
           if (ii_errors) return(II_CHOWN_FAILED);
         }
     }
   ii_at_least_1_installed = 1;
   return(II_OK);
 }


int	ii_filecopy(char *infname)
 { char		outfname[MY_PATH_MAX], buf[BUFSIZE];
   struct stat	fst, infst, outfst;
   FILE		*inf, *outf;
   int		delta, i;
   char		*p;

   if (NULL == infname)
     { printf("ERROR (in ii_filecopy): invalid (null ptr) input file name\n");
       return(II_NUL_PTR);
     }

   if (0 == ii_dir[0])
     { printf("ERROR (in ii_filecopy): invalid (empty) install directory\n");
       return(II_BAD_ARG);
     }

   strcpy(outfname, ii_dir);

   p = strrchr(infname, '/');
   if (NULL == p)  strcat(outfname, infname);
   else  strcat(outfname, p + 1);

   if (0 == stat(infname, &infst))
     if (0 == stat(outfname, &outfst))
       if (infst.st_ino == outfst.st_ino)
         if (infst.st_dev == outfst.st_dev)
 	   { printf("WARNING (in ii_filecopy): attempt copy file onto itself: %s --> %s, ignored\n", infname, outfname);
 	     return(II_OK);
           }
   
   unlink(outfname);	/* remove out file if it exists */

		/* open input and output files */

   if (NULL == (inf = fopen(infname, "r")))
     { printf("ERROR (in ii_filecopy): couldn't open file for reading: %s\n", infname);
       return(II_FOPEN_FAILED);
     }

   if (NULL == (outf = fopen(outfname, "w")))
     { printf("ERROR (in ii_filecopy): couldn't open file for writing: %s\n", outfname);
       return(II_FOPEN_FAILED);
     }

   fstat(fileno(inf), &fst);

		/* copy up to BUFSIZE bytes in one iteration */

   for (i=0; i<fst.st_size; i+=delta)
     { delta = BUFSIZE;
       if ((i+delta) > fst.st_size) delta = fst.st_size - i;

       if (delta != fread(buf, 1, delta, inf))
         { printf("ERROR (in ii_filecopy): read from file %s failed\n", infname);
           return(II_FREAD_FAILED);
         }
       if (delta != fwrite(buf, 1, delta, outf))
         { printf("ERROR (in ii_filecopy): write to file %s failed\n", outfname);
           return(II_FWRITE_FAILED);
         }
     }

   if (fchmod(fileno(outf), ii_file_mode))
     { printf("ERROR (in ii_filecopy): cannot chmod file %s to mode 0%o\n", outfname, ii_file_mode);
       return(II_FCHMOD_FAILED);
     }

   if (fclose(outf))
     { printf("ERROR (in ii_filecopy): fclose of file %s failed\n", outfname);
       return(II_FCLOSE_FAILED);
     }

   if (fclose(inf))
     { printf("ERROR (in ii_filecopy): fclose of file %s failed\n", infname);
       return(II_FCLOSE_FAILED);
     }

   if ((-1 != ii_uid) || (-1 != ii_gid))
     { if (chown(outfname, ii_uid, ii_gid))
         { printf("ERROR (in ii_mkdir): cannot chown file %s to uid %d, gid %d\n", outfname, ii_uid, ii_gid);
           if (ii_errors) return(II_CHOWN_FAILED);
         }
     }

   ii_at_least_1_installed = 1;
   return(II_OK);
 }


int	main(int argc, char **argv)
{ int		i, r, ii_parse_mode, itmp;
  struct passwd	*my_passwd;
  struct group	*my_group;
  char		tmp_c;
  unsigned	u;

ii_argv0 = argv[0];
umask(0);

ii_parse_mode = II_PARSER_DEFAULT;

for (i=1; i<argc; i++)
 { 
   switch (ii_parse_mode)
    {
      case II_PARSER_FILE_OCTAL_MODE:
		if (1 != sscanf(argv[i], "%o%c", &u, &tmp_c))
		  { printf("ERROR (in main): invalid file octal mode: %s\n", argv[i]);
		    if (ii_errors)  { ii_usage(); return(II_BAD_MODE); }
		  }
		ii_file_mode = (int)u;
		ii_parse_mode = II_PARSER_DEFAULT;
		break;

      case II_PARSER_DIR_OCTAL_MODE:
		if (1 != sscanf(argv[i], "%o%c", &u, &tmp_c))
		  { printf("ERROR (in main): invalid directory octal mode: %s\n", argv[i]);
		    if (ii_errors)  { ii_usage(); return(II_BAD_MODE); }
		  }
		ii_dir_mode = (int)u;
		ii_parse_mode = II_PARSER_DEFAULT;
		break;

      case II_PARSER_DIRNAME:
		strncpy(ii_dir, argv[i], MY_PATH_MAX - 1);
		ii_dir[MY_PATH_MAX - 1] = 0;
		if (strlen(ii_dir) <= 0)
		  { printf("ERROR (in main): invalid (empty) install directory: %s\n", argv[i]);
		    if (ii_errors)  { ii_usage(); return(II_BAD_ARG); }
		  }
		else  { if (ii_dir[strlen(ii_dir) - 1] != '/')  strcat(ii_dir, "/"); }
		  
		ii_parse_mode = II_PARSER_DEFAULT;
		break;

      case II_PARSER_USERNAME:
		if (1 == sscanf(argv[i], "%d%c", &itmp, &tmp_c))
		  { ii_uid = itmp;
		  }
		else
		  { my_passwd = getpwnam(argv[i]);
		    if (NULL == my_passwd)
		      { printf("ERROR (in main): no such user %s\n", argv[i]);
		        if (ii_errors)  { ii_usage(); return(II_GETPWNAM_FAILED); }
		      }
		    else  ii_uid = my_passwd->pw_uid;
		  }
		  
		ii_parse_mode = II_PARSER_DEFAULT;
		break;

      case II_PARSER_GROUPNAME:
		if (1 == sscanf(argv[i], "%d%c", &itmp, &tmp_c))
		  { ii_gid = itmp;
		  }
		else
		  { my_group = getgrnam(argv[i]);
		    if (NULL == my_group)
		      { printf("ERROR (in main): no such group %s\n", argv[i]);
		        if (ii_errors)  { ii_usage(); return(II_GETGRNAM_FAILED); }
		      }
		    else  ii_gid = my_group->gr_gid;
		  }
		  
		ii_parse_mode = II_PARSER_DEFAULT;
		break;

      case II_PARSER_ERROR_MODE:
		if (1 == sscanf(argv[i], "%d%c", &itmp, &tmp_c))
		  { ii_errors = itmp;
		  }
		else
		  { printf("ERROR (in main): invalid error mode %s\n", argv[i]);
		    if (ii_errors)  { ii_usage(); return(II_BAD_ARG); }
		  }

		ii_parse_mode = II_PARSER_DEFAULT;
		break;

      default:
		if (0 == strcmp("-m", argv[i]))  { ii_parse_mode = II_PARSER_FILE_OCTAL_MODE; break; }
		else if (0 == strcmp("-M", argv[i]))  { ii_parse_mode = II_PARSER_DIR_OCTAL_MODE; break; }
		else if (0 == strcmp("-u", argv[i]))  { ii_parse_mode = II_PARSER_USERNAME; break; }
		else if (0 == strcmp("-g", argv[i]))  { ii_parse_mode = II_PARSER_GROUPNAME; break; }
		else if (0 == strcmp("-e", argv[i]))  { ii_parse_mode = II_PARSER_ERROR_MODE; break; }
		else if (0 == strcmp("-d", argv[i]))  { ii_parse_mode = II_PARSER_DIRNAME; break; }
		else if (0 == strcmp("-q", argv[i]))  { ii_quiet_mode = 1; break; }
		else if (0 == strcmp("-c", argv[i]))
		  { if (II_OK != (r = ii_mkdir(ii_dir)))
		      { printf("ERROR (in main): function ii_mkdir(%s) failed\n", ii_dir);
		        if (ii_errors)  { ii_usage(); return(r); }
		      }
		  }
		else if (II_OK != (r = ii_filecopy(argv[i])))
		  { printf("ERROR (in main): function ii_filecopy(%s) failed\n", argv[i]);
		    if (ii_errors)  { ii_usage(); return(r); }
		  }
		break;
    }
 }

if (!ii_at_least_1_installed)
  { if (ii_errors && (0 == ii_quiet_mode))  { ii_usage(); return(II_BAD_ARG); }
  }

return(II_OK);
}
