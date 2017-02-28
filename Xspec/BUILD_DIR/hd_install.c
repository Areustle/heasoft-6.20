/******************************************************************************
 *     Project: HEADAS                                                        *
 *                                                                            *
 *   File name: hd_install.c                                                  *
 *                                                                            *
 *        NOTE: THIS IS NOT A STANDARD UNIX INSTALL UTILITY!                  *
 *                                                                            *
 * Description: File installation utility for use within a build/make         *
 *              context. First argument (the source file) must be a           *
 *              file or a symbolic link which points to a file. Second        *
 *              argument (the destination file) may or may not exist.         *
 *              If second argument exists as a file or symbolic link, the     *
 *              utility will first remove the destination before installing   *
 *              the source file. If second argument is a directory, the       *
 *              utility will install the source file into that directory.     *
 *              In all cases, the source file will only be installed if its   *
 *              modification date is more recent than the destination.        *
 *                                                                            *
 *              By default, the file is installed with cp -p but the          *
 *              user may also specify the install program and options         *
 *              to use (e.g. ln, ln -s, cp -i, mv)                            *
 *                                                                            *
 *              If possible, all parts of the output path will be created.    *
 *              Thus, the directory tree in which the destination file        *
 *              will be installed need not exist before this utility is       *
 *              called.                                                       *
 *                                                                            *
 *       Usage: hd_install [hd_install-opts] src dest [prog] [prog-opts]      *
 *                                                                            *
 *      Author: James Peachey, LAC, for HEASARC/GSFC/NASA                     *
 *                                                                            *
 *  Change log: see CVS Change log at the end of the file.                    *
 ******************************************************************************/
#ifdef WIN32
#include <windows.h>
#else
#include <sys/stat.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <fcntl.h>
#include <unistd.h>
#include <dirent.h>
#endif

#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <stdlib.h>
#include <time.h>

enum InstallConstants {
  /* Constants. */
  OK = 0,
  MAX_ARG_LEN = 1024,
  MAX_PATH_LEN = 4096,
  MAX_NUM_ARGS = 128,
  FINISHED = 1,

  /* Error codes. */
  ERROR_START = 100,

  /* Usage errors. */
  MISSING_ARG,
  INCOMPATIBLE_ARGUMENTS,
  SRC_FILE_MISSING,
  INVALID_OPTION,
  NON_DIRECTORY_EXISTS,
  CANNOT_ACCESS_DIR,
  CANNOT_CREATE,

  /* Program errors. */
  TOO_MANY_ARGS,
  PATH_TOO_LONG,
  BAD_ARGV_TERMINATION,
  MALLOC_FAILED,
  NULL_POINTER,
  FILE_INACCESSIBLE,
  UNEXPECTED,
  ERROR_STOP
};

#define BIT(N) 1 << N

typedef enum InstallOptions {
  NONE = BIT(0),
  DONE = BIT(1),
  ABSOLUTE_PATH = BIT(2),
  RELATIVE_PATH = BIT(3),
  WARN_IF_SRC_MISSING = BIT(4),
  ERR_IF_SRC_MISSING = BIT(5),
  VERBOSE = BIT(6),
  FORCE_INSTALL = BIT(7),
  DEFAULT = WARN_IF_SRC_MISSING | VERBOSE
} InstallOptions;

enum hd_file_types_e {
  FT_UNKNOWN = 0,
  FT_FILE = 1,
  FT_DIR = 2,
  FT_LINK = 3
};

typedef struct hd_file_attributes_s {
#ifdef WIN32
  ULONGLONG m_mtime;
  /* No equivalent to inode on Windows. */
#else
  time_t m_mtime;
  ino_t m_inode;
#endif
  char m_accessible;
  char m_type;
} hd_file_attributes_s;

typedef struct hd_dir_s {
#ifdef WIN32
  char m_dir_name[MAX_PATH_LEN];
  char m_current_fname[MAX_PATH_LEN];
  HANDLE m_handle;
  size_t m_dir_name_size;
#else
  DIR* m_dir_p;
#endif
} hd_dir_s;

#ifdef WIN32
typedef unsigned int mode_t;
#endif
/******************************************************************************
 * Static function declarations.                                              *
 ******************************************************************************/
static int HDgetFileAttributes(const char* name, hd_file_attributes_s* att,
    int status);

static int HDdiffFileID(hd_file_attributes_s* f1, hd_file_attributes_s* f2);

static int HDopenDirectory(const char* src, hd_dir_s* dir, int status);

static int HDcloseDirectory(hd_dir_s* dir, int status);

static int HDreadDirectory(hd_dir_s* dir, const char** file, int status);

static int MakeDirectory(char* dir, mode_t dirmode, int status);

static int MakeOneDirectory(const char* dir, mode_t dirmode, int status);

static char** FindFirstNonOption(char** argv);

static int CheckCmdLine(int argc, char** argv, int status);

static int HDgetCurrentDirectory(char* pwd, int status);

static int SplitPath(const char* orig, char*** pathArray, int status);

static int CleanPath(const char* orig, char** clean, int status);

static int ChangeDirectory(const char* newDir, int status);

static int MakeAbsolutePath(const char* inpath, char** outpath, int status);

static int MakeRelativePath(char* inpath, char* base,
    char** outpath, int status);

static int ResolveLinks(const char* inpath, char** outpath, int status);

static int SplitFile(const char* orig, char** dirPart,
  char** filePart, int status);

static int AppendPath(const char* orig, const char* newSeg,
    char** outPath, int status);

static int InstallFiles(char* src, char* dest,
    char** cmd, InstallOptions* opts, int status);

static int InstallDirectory(const char* src, const char* dest,
    char** cmd, InstallOptions* opts, int status);

static void Delete2Darray(char** array);
/******************************************************************************
 * Global function declarations.                                              *
 ******************************************************************************/
int HDinstall_file(int argc, char** argv, int status);

/******************************************************************************
 * Static variable declarations.                                              *
 ******************************************************************************/
static const char* sBanner = "hd_install";

static const char* sErrFmt = "%s ERROR: %s\n";

static char sPWD[MAX_PATH_LEN] = "";
/******************************************************************************
 * Static function definitions                                                *
 ******************************************************************************/
static int HDgetFileAttributes(const char* name, hd_file_attributes_s* att,
    int status) {
  if(OK != status) return status;
  if(NULL == name || NULL == att) return NULL_POINTER;

  /* Initialize att. */
  att->m_mtime = 0;
  att->m_accessible = 0;
  att->m_type = FT_UNKNOWN;

  do {
#ifdef WIN32
    /* This function has problems. When compiled with Visual Studio 7.0
       FindFirstFile seems to fail on the third file, regardless whether
       the file exists. The cause of this problem is currently unknown. */
    WIN32_FIND_DATA winData;
    union {
      FILETIME ft;
      ULONGLONG ll;
    } ctime, mtime;
    HANDLE handle;
    char* tmp_p;

    if(INVALID_HANDLE_VALUE != (handle = FindFirstFile(name, &winData))) {
      FindClose(handle);

      /* Take the latest value of either creation or mod time. */
      ctime.ft = winData.ftCreationTime;
      mtime.ft = winData.ftLastWriteTime;
      att->m_mtime = ctime.ll < mtime.ll ? mtime.ll : ctime.ll;

      /* See if this is a file or a directory. */
      if(FILE_ATTRIBUTE_DIRECTORY & winData.dwFileAttributes) {
        att->m_type = FT_DIR;
      } else {
        att->m_type = FT_FILE;
      }
      att->m_accessible = 1;

    } else {
      status = FILE_INACCESSIBLE;
      continue;
    }

#else
    struct stat myStat;

    /* Unix-only member initialization. */
    att->m_inode = 0;

    if(stat(name, &myStat) && lstat(name, &myStat)) {
      status = FILE_INACCESSIBLE;
      continue;
    }

    /* Stat succeeded, so get all info from struct. */
    att->m_mtime = myStat.st_mtime;
    att->m_inode = myStat.st_ino;
    att->m_accessible = 1;

    if(S_ISREG(myStat.st_mode)) att->m_type = FT_FILE;
    else if(S_ISDIR(myStat.st_mode)) att->m_type= FT_DIR;
    else if(S_ISLNK(myStat.st_mode)) att->m_type= FT_LINK;
#endif

  } while(0);
  return status;
}

static int HDdiffFileID(hd_file_attributes_s* f1, hd_file_attributes_s* f2) {
  if(NULL == f1 || NULL == f2) return NULL_POINTER;
  if(f1 == f2) return OK;
#ifdef WIN32
  /* To do: determine if there are circumstances in which the same
     file might appear under two different names, and how to deal
     with such a situation. */
  return -1;
#else
  if(0 == f1->m_inode) return -1;
  else if(0 == f2->m_inode) return 1;
  return f1->m_inode - f2->m_inode;
#endif
}

static int HDopenDirectory(const char* src, hd_dir_s* dir, int status) {
  if(OK != status) return status;
  if(NULL == src || NULL == dir) return NULL_POINTER;

  do {
#ifdef WIN32
    WIN32_FIND_DATA ignored;
    size_t bufsize = strlen(src) + 1;
    char* tmp_p;

    /* Initialize directory very paranoically. */
    *dir->m_dir_name = '\0';
    *dir->m_current_fname = '\0';
    dir->m_handle = INVALID_HANDLE_VALUE;
    dir->m_dir_name_size = 0;

    /* Cap size of directory, allowing room for wildcard and \*. */
    if(bufsize > MAX_PATH_LEN - 3) {
      status = PATH_TOO_LONG;
      continue;
    }

    /* Store size of directory name for future reference. */
    dir->m_dir_name_size = bufsize;

    /* Store directory name in both buffers. */
    strcpy(dir->m_dir_name, src);
    for(tmp_p = dir->m_dir_name; *tmp_p; ++tmp_p)
      if('/' == *tmp_p) *tmp_p = '\\';

    strcpy(dir->m_current_fname, dir->m_dir_name);
    strcat(dir->m_current_fname, "\\*");

    if(INVALID_HANDLE_VALUE == (dir->m_handle =
        FindFirstFile(dir->m_current_fname, &ignored))) {
      status = CANNOT_ACCESS_DIR;
      continue;
    }
#else
    if(NULL == (dir->m_dir_p = opendir(src))) {
      dir->m_dir_p = NULL;
      status = CANNOT_ACCESS_DIR;
      continue;
    }
#endif
  } while(0);

  return status;
}

static int HDcloseDirectory(hd_dir_s* dir, int status) {
  if(OK != status) return status;
  if(NULL == dir) return NULL_POINTER;

#ifdef WIN32
  FindClose(dir->m_handle);
  dir->m_dir_name_size = 0;
  dir->m_handle = INVALID_HANDLE_VALUE;
  *dir->m_current_fname = '\0';
  *dir->m_dir_name = '\0';
#else
  if(NULL == dir->m_dir_p) return status;
  closedir(dir->m_dir_p);

  dir->m_dir_p = NULL;
#endif

  return status;
}

static int HDreadDirectory(hd_dir_s* dir, const char** file, int status) {
  if(OK != status) return status;
  if(NULL == dir || NULL == file) return NULL_POINTER;

  do {
#ifdef WIN32
    WIN32_FIND_DATA winData;
    if(!FindNextFile(dir->m_handle, &winData)) {
      if(ERROR_NO_MORE_FILES == GetLastError())
        status = FINISHED;
      else
        status = UNEXPECTED;
    } else {
      /* Compute the size needed to store this file's full path, including
         room for the / as well as the terminating 0. */
      size_t bufsize = dir->m_dir_name_size + strlen(winData.cFileName) + 2;

      if(bufsize > MAX_PATH_LEN) {
        status = PATH_TOO_LONG;
        continue;
      }

      /* Create fully qualified file name. */
      strncpy(dir->m_current_fname, winData.cFileName, MAX_PATH_LEN - 1);
      dir->m_current_fname[MAX_PATH_LEN - 1] = '\0';

      *file = dir->m_current_fname;
    }
#else
    struct dirent* entry_p;

    if(NULL == (entry_p = readdir(dir->m_dir_p))) {
      status = FINISHED;
    } else {
      *file = entry_p->d_name;
    }
#endif
  } while(0);

  return status;
}

static int MakeDirectory(char* dir, mode_t dirmode, int status) {
  if(OK != status) return status;

  do {
    char* ptr = dir;

    /* Progressively try to create each sub-path of the full directory
       name, starting with the first sub-path. */
    while(NULL != (ptr = strstr(ptr, "/"))) {

      /* Replace the current / with 0. That will null-terminate the
         current sub-path. */
      *ptr = '\0';

      /* Attempt to create the sub-path which was just created. */
      status = MakeOneDirectory(dir, dirmode, status);

      /* Put the / back and go on to the next character. That way the
         next sub-path will contain the current sub-path plus the next
         segment. */
      *ptr++ = '/';

      if(OK != status) break;
    }

    if(OK != status) continue;

    /* All sub-paths have been created except the last one, so do that now. */
    status = MakeOneDirectory(dir, dirmode, status);

  } while(0);
  return status;
}

static int MakeOneDirectory(const char* dir, mode_t dirmode, int status) {
  if(OK != status) return status;

  do {
    hd_file_attributes_s dirAttribs;

    /* Consider empty or null input to be /, which we hope exists. */
    if(NULL == dir || '\0' == *dir) continue;

    /* Use stat and lstat to see if something already exists with name dir. */
    if(OK == HDgetFileAttributes(dir, &dirAttribs, status)) {
      /* One or the other of stat or lstat succeeded (returned 0), so
         something exists with name dir; handle all possibilities: */

      /* If directory already exists, we're done. */
      if(FT_DIR == dirAttribs.m_type) continue;

      /* If it's a broken link, try once to delete it, and otherwise
         there is no way to recover. */
      if(FT_LINK == dirAttribs.m_type) {
        status = remove(dir);
        if(OK != status) status = errno ? errno : status;
      }
      else status = NON_DIRECTORY_EXISTS;
    }

    if(OK != status) continue;

    /* Attempt to create the directory and report any error. */
#ifdef WIN32
    /* To do: translate mode into proper Windows security settings. */
    if(!CreateDirectory(dir, NULL)) status = CANNOT_CREATE;
#else
    if(mkdir(dir, dirmode)) status = CANNOT_CREATE;
#endif

  } while(0);
  return status;
}

static char** FindFirstNonOption(char** argv) {
  argv++; /* Skip name of this. */
  while(NULL != argv && NULL != *argv) {
    /* First argument which has no leading dash is not an option. */
    if('-' != **argv) break;

    /* Or, if a double-dash, end options so that filenames with a dash
       can be handled, like grep. */
    else if(0 == strcmp(*argv, "--")) { argv++; break; }

    /* Go on to next argument. */
    ++argv;
  }
  return argv;
}

static int CheckCmdLine(int argc, char** argv, int status) {
  if(OK != status) return status;
  do {
    if(MAX_NUM_ARGS < argc) {
      status = TOO_MANY_ARGS;
      continue;
    }

    if(OK != status) continue;

    /* Make sure argv is properly NULL terminated as required by ANSI C. */
    if(NULL != argv[argc]) {
      fprintf(stderr, sErrFmt, sBanner,
        "improperly terminated command line arguments");
      status = BAD_ARGV_TERMINATION;
      continue;
    }
  } while(0);

  return status;
}

static int HDgetCurrentDirectory(char* pwd, int status) {
  if(OK != status) return status;
  do {
    if(NULL == pwd) {
      status = NULL_POINTER;
      continue;
    }

    /* First time this is called, poll the OS for information. */
    if('\0' == *sPWD) {
      /* Use getcwd. */
#ifdef WIN32
      if(0 == GetCurrentDirectory(MAX_PATH_LEN - 1, sPWD)) {
#else
      if(NULL == getcwd(sPWD, MAX_PATH_LEN - 1)) {
#endif
        /* If this failed, we're in trouble. */
        *sPWD = '\0';
        status = CANNOT_ACCESS_DIR;
        continue;
      }
      sPWD[MAX_PATH_LEN - 1] = '\0';
    }

    /* Return the canonical pwd buffer. */
    strcpy(pwd, sPWD);

  } while(0);

  return status;
}

/* Note that in the following function, all leading /s are silently
   discarded! Also, caller is reponsible for freeing each element in
   the array as well as the array itself! */
static int SplitPath(const char* orig, char*** pathArray, int status) {
  char* work = NULL;
  char** tmpArray = NULL;
  if(OK != status) return status;
  do {
    char** curToken;
    char* tmp_p;
    size_t numTok;

    if(NULL == orig || NULL == pathArray) {
      status = NULL_POINTER;
      continue;
    }

    *pathArray = NULL;

    if(NULL == (work = (char*) malloc((1u + strlen(orig)) * sizeof(char)))) {
      status = MALLOC_FAILED;
      continue;
    }

    /* Minimum number of segments is 1. */
    numTok = 1u;

    /* Copy input to work buffer, and while we're at it, count occurrences
       of /. This will compute max number of path segments. */
    for(tmp_p = work; '\0' != *orig; ++orig, ++tmp_p) {
      if('/' == (*tmp_p = *orig)) ++numTok;
    }

    /* Null terminate the work buffer. */
    *tmp_p = '\0';

    /* Create an array to hold the max number of segments plus
       a terminating NULL. Use calloc so these are initialized to NULL. */
    if(NULL == (tmpArray = (char**) calloc(1u + numTok, sizeof(char*)))) {
      status = MALLOC_FAILED;
      continue;
    }

    /* Use strtok to chop up the work buffer and put copies of the
       segments in tmpArray. */
    curToken = tmpArray;
    tmp_p = strtok(work, "/");
    while(NULL != tmp_p) {
      /* Skip any segments which are . */
      if(0 == strcmp(tmp_p, ".")) /* do nothing */;

      /* Consolidate strings of form "blah/.." where blah is not "..".
         If tmp_p is .. and there are tokens before the current
         one, free the previous segment and skip the .. */
      else if(0 == strcmp(tmp_p, "..") && curToken > tmpArray &&
          (NULL != *(curToken - 1)) && 0 != strcmp(*(curToken - 1), "..")) {
        /* Erase the previous token. */
        --curToken;
        free(*curToken);
        *curToken = NULL;
      }

      /* Normal case: put a copy of tmp_p into the array. */
      else {
        *curToken = (char*) malloc((1u + strlen(tmp_p)) * sizeof(char));
        if(NULL == *curToken) {
          status = MALLOC_FAILED;
          break;
        }
        strcpy(*curToken, tmp_p);

        /* Go on to next token. */
        ++curToken;
      }

      /* In all cases, continue to the next token. */
      tmp_p = strtok(NULL, "/");
    }

  } while(0);

  /* Clean-up. */
  if(OK == status) *pathArray = tmpArray;
  else Delete2Darray(tmpArray);

  memset(work, '\0', (1u + strlen(orig)) * sizeof(char));
  free(work);

  return status;
}

static int CleanPath(const char* orig, char** clean, int status) {
  char** tokens = NULL;
  char* tmpArray = NULL;
  if(OK != status) return status;
  do {
    char** curToken;
    size_t cleanLen;
    size_t tmpLen;
    unsigned char leading_slash = 0u;
    unsigned char trailing_slash = 0u;

    if(NULL == orig || NULL == clean) {
      status = NULL_POINTER;
      continue;
    }

    *clean = NULL;

    /* Determine if there are leading slashes. */
    if('/' == *orig) leading_slash = 1u;

    /* Determine if there are trailing slashes. */
    tmpLen = strlen(orig);
    if(0u < tmpLen && '/' == orig[tmpLen - 1u]) trailing_slash = 1u;

    /* Determine the length of the final output path. Minimum is
       2 characters: one is either . or /, and the other is the
       terminating 0. In addition, need to allow for trailing
       slash if one was detected above. */
    cleanLen = 2u + trailing_slash;

    /* Parse the path, cleaning . .. and multiple consecutive slashes. */
    status = SplitPath(orig, &tokens, status);
    if(OK != status) continue;

    for(curToken = tokens; NULL != *curToken; ++curToken) {
      /* Include room in final output path for / after each token. */
      cleanLen += 1u + strlen(*curToken);
    }

    /* Create space for output path. */
    if(NULL == (tmpArray = (char*) malloc(cleanLen * sizeof(char)))) {
      status = MALLOC_FAILED;
      continue;
    }

    /* SplitPath removes all slashes, so leading slash
       needs to be added again by hand if it was present originally. */
    if(leading_slash) strcpy(tmpArray, "/");
    else *tmpArray = '\0';

    /* Copy the tokens into the final output path. */
    for(curToken = tokens; NULL != *curToken; ++curToken) {
      strcat(tmpArray, *curToken);
      strcat(tmpArray, "/");
    }

    /* At this point if the path is blank it must mean that there was
       no leading slash, and that the cleaned path evaluated to nothing.
       Thus it is a relative path which ended up going nowhere, so it
       really should be . */
    if('\0' == *tmpArray) strcpy(tmpArray, ".");

    /* Handle trailing slash. This is tricky because all slashes were
       eaten by SplitPath, but slashes may have been added in the loop above. */
    tmpLen = strlen(tmpArray);
    if(trailing_slash) {
      /* Make sure the trailing slash is present no matter what. */
      if(0u == tmpLen || '/' != tmpArray[tmpLen - 1u])
        strcat(tmpArray, "/");
    } else {
      /* Make sure the trailing slash is removed no matter what. */
      if(0u < tmpLen && '/' == tmpArray[tmpLen - 1u])
        tmpArray[tmpLen - 1u] = '\0';
    }

  } while(0);

  /* Clean-up. */
  if(OK == status) *clean = tmpArray;
  else free(tmpArray);

  Delete2Darray(tokens);

  return status;
}

static int ChangeDirectory(const char* newDir, int status) {
  char pwd[MAX_PATH_LEN];
  char* newpwd = NULL;
  if(OK != status) return status;
  do {
    if(NULL == newDir) {
      status = NULL_POINTER;
      continue;
    }

    /* Blank directory is a no-op. */
    if('\0' == *newDir) continue;

    /* First, change the working directory as far as the OS is concerned. */
#ifdef WIN32
    if(!SetCurrentDirectory(newDir)) status = CANNOT_ACCESS_DIR;
#else
    if(chdir(newDir)) status = CANNOT_ACCESS_DIR;
#endif

    if(OK != status) continue;

    /* Next, change the working directory as far as this program
       is concerned. */

    /* If a relative path was specified, prepend the current absolute path. */
    if('/' != *newDir) {
      char* tmp = NULL;

      status = HDgetCurrentDirectory(pwd, status);

      status = AppendPath(pwd, newDir, &tmp, status);

      status = CleanPath(tmp, &newpwd, status);

      free(tmp);

    } else {
      status = CleanPath(newDir, &newpwd, status);
    }

    if(OK != status) continue;

    if(MAX_PATH_LEN - 1 < strlen(newpwd)) {
      status = PATH_TOO_LONG;
      continue;
    }

    strcpy(sPWD, newpwd);

  } while(0);

  free(newpwd);

  return status;
}

static int MakeAbsolutePath(const char* inpath, char** outpath, int status) {
  char* newPath = NULL;
  if(OK != status) return status;
  do {

    if(NULL == inpath || NULL == outpath) {
      status = NULL_POINTER;
      continue;
    }

    *outpath = NULL;

    /* Make sure it's not already absolute. */
    if('/' != *inpath) {
      char pwd[MAX_PATH_LEN];

      /* Prepend current directory to the path. */
      status = HDgetCurrentDirectory(pwd, status);

      status = AppendPath(pwd, inpath, &newPath, status);

    } else {
      if(NULL == (newPath =
        (char*) malloc((1u + strlen(inpath)) * sizeof(char)))) {
        status = MALLOC_FAILED;
        continue;
      }
      strcpy(newPath, inpath);
    }

  } while(0);

  if(OK == status) *outpath = newPath;
  else free(newPath);

  return status;
}

/* Note: this function only manipulates the arguments as strings.
   The argument "base" must be the name of a directory, but this
   function does not check that! Caveat programmer. */
static int MakeRelativePath(char* inpath, char* base,
    char** outpath, int status) {
  char* inpathCopy = NULL;
  char* baseCopy = NULL;
  char* inpathAbs = NULL;
  char* baseAbs = NULL;
  char** inSplit = NULL;
  char** baseSplit = NULL;
  char** tmpSeg;
  char* tmp_p = NULL;
  if(OK != status) return status;
  do {
    char** inSeg;
    char** baseSeg;
    size_t pathLen;
    unsigned char trailing_slash = 0u;

    if(NULL == inpath || NULL == base || NULL == outpath) {
      status = NULL_POINTER;
      continue;
    }

    /* Determine if there is a trailing slash. */
    if(0u < strlen(inpath) && '/' == inpath[strlen(inpath) - 1u])
      trailing_slash = 1u;

    *outpath = NULL;

    /* Resolve (expand) all sym links so the relative path will be accurate. */
    status = ResolveLinks(inpath, &inpathCopy, status);
    status = ResolveLinks(base, &baseCopy, status);

    /* Render both inpath and base as absolute paths. */
    status = MakeAbsolutePath(inpathCopy, &inpathAbs, status);
    status = MakeAbsolutePath(baseCopy, &baseAbs, status);

    /* Split them up, cleaning in the process. */
    status = SplitPath(inpathAbs, &inSplit, status);
    status = SplitPath(baseAbs, &baseSplit, status);

    if(OK != status) continue;

    /* Find first distinct segment in each path. */
    for(inSeg = inSplit, baseSeg = baseSplit;
        NULL != *inSeg && NULL != *baseSeg; ++inSeg, ++baseSeg) {
      if(0 != strcmp(*inSeg, *baseSeg)) break;
    }

    /* Compute size for output. */
    /* Minimum size is 2, for . plus terminating 0. Also leave room
       for trailing slash if there need be one. */
    pathLen = (2u + trailing_slash) * sizeof(char);

    /* For each remaining segment in the base,
       need enough room for ../ */
    for(tmpSeg = baseSeg; NULL != *tmpSeg; pathLen += sizeof("../"), ++tmpSeg);

    /* Also need room for all remaining segments in the input, including
       a slash after each. */
    for(tmpSeg = inSeg; NULL != *tmpSeg;
        pathLen += (1u + strlen(*tmpSeg)) * sizeof(char), ++tmpSeg);

    /* Make space for the output. */
    if(NULL == (tmp_p = (char*) malloc(pathLen))) {
      status = MALLOC_FAILED;
      continue;
    }

    /* Blank the output. */
    *tmp_p = '\0';

    /* Add leading ../s. */
    for(tmpSeg = baseSeg; NULL != *tmpSeg; ++tmpSeg) {
      strcat(tmp_p, "../");
    }

    /* Add remaining segments from output. */
    for(tmpSeg = inSeg; NULL != *tmpSeg; ++tmpSeg) {
      strcat(tmp_p, *tmpSeg);
      strcat(tmp_p, "/");
    }

    /* A blank path at this point means inpath and base were de facto
       the same directory, so make the relative path simply . */
    if('\0' == *tmp_p) strcpy(tmp_p, ".");

    /* Handle trailing slash. This is tricky because all slashes were
       eaten by SplitPath, but may have been added in the loops above. */
    pathLen = strlen(tmp_p);
    if(trailing_slash) {
      /* Make sure the trailing slash is present no matter what. */
      if(0u == pathLen || '/' != tmp_p[pathLen - 1u])
        strcat(tmp_p, "/");
    } else {
      /* Make sure the trailing slash is removed no matter what. */
      if(0u < pathLen && '/' == (tmp_p)[pathLen - 1u])
        (tmp_p)[pathLen - 1u] = '\0';
    }
  } while(0);

  /* Handle return value and clean-up. */
  if(OK == status) *outpath = tmp_p;
  else free(tmp_p);

  Delete2Darray(baseSplit);
  Delete2Darray(inSplit);
  free(baseAbs);
  free(inpathAbs);
  free(baseCopy);
  free(inpathCopy);

  return status;
}

static int ResolveLinks(const char* inpath, char** outpath, int status) {
  char* work = NULL;
  if(OK != status) return status;
  if(NULL == inpath || NULL == outpath) return NULL_POINTER;

  *outpath = NULL;

  /* Copy the input to the work buffer. */
  work = malloc((1 + strlen(inpath) * sizeof(char)));
  if(NULL == work) return MALLOC_FAILED;

  strcpy(work, inpath);

#ifdef WIN32
    /* No sym link, so the above copy block is all that is needed. */
#else
  do {
    char linkValue[MAX_PATH_LEN + 1];
    char* currentPos;
    int linkLen;
    char done = 0;
    char endsInSlash;
    char checkSeg;

    currentPos = work;

    /* Go through copy of inpath, temporarily blanking out each
       successive / and checking whether the resulting sub-path
       is a sym-link. */
    while('\0' != *currentPos && ! done) {
      /* Init flags to indicate whether we are at the end of a segment
         (checkSeg) and whether segment ends in a slash (endsInSlash). */
      endsInSlash = checkSeg = 0;

      /* If the current character is / and it's not a leading slash,
         this is the end of the segment, so it should be checked. */
      if('/' == *currentPos && currentPos != work) checkSeg = endsInSlash = 1;

      /* If the next character is 0, this is the end of the whole
         inpath, so this is the end of the segment, and it is the
         last segment. */
      if('\0' == *(currentPos + 1) && currentPos != work) checkSeg = 1;

      if(checkSeg) {
        if(endsInSlash) *currentPos = '\0';

        /* Get the value of the current subpath if it is a link. */
        linkLen = readlink(work, linkValue, MAX_PATH_LEN);

        if(0 > linkLen) {
          switch(errno) {
            case EINVAL:
              /* The file exists, but is not a link. That is OK,
                 but we must continue to process the rest of the
                 path. */
              break;
            case ENOENT:
            case ENOTDIR:
              /* These errors signify that we are dealing with a
                 path which does not exist. That is OK, and it means
                 no further action is necessary. */
              done = 1;
              break;
            default:
              /* Otherwise, this error is really an error. */
              status = errno;
              break;
          }
          if(OK != status) break; /* out of while loop */
        } else {
          /* It is a sym-link, so use its value (where it points)
             for the output path. */

          char* path1 = NULL;
          char* path2 = NULL;

          /* Null-terminate the linkValue buffer. */
          linkValue[linkLen] = '\0';

          /* Move past the current segment. */
          ++currentPos;

          if('/' == *linkValue) {
            /* Absolute path simply replaces the first part of the buffer. */
            status = AppendPath(linkValue, currentPos, &path1, status);
            free(work);
            if(OK != status) break;
            work = path1;
            currentPos = work;
          } else {
            size_t offset = strlen(work);
            /* It's a relative path, so substitute linkValue for the last
               segment of work, while preserving leading slash (if any). */
            while(0u != offset && '/' != work[offset-1]) --offset;
            work[offset] = '\0';
            status = AppendPath(work, linkValue, &path1, status);
            status = AppendPath(path1, currentPos, &path2, status);
            free(path1);
            free(work);
            if(OK != status) break;
            work = path2;
            currentPos = work + offset;
          }
          continue;
        }
        if(endsInSlash) *currentPos = '/';
      }
      ++currentPos;
    }

  } while(0);
#endif

  *outpath = work;

  return status;
}

static int SplitFile(const char* orig, char** dirPart,
  char** filePart, int status) {
  if(OK != status) return status;
  do {
    size_t lastSlash;
    size_t fileLen;
    size_t idx;
    if(NULL == orig) {
      status = NULL_POINTER;
      continue;
    }

    /* Find the last / in the original string. */
    lastSlash = 0u;
    for(idx = 0u; '\0' != orig[idx]; ++idx) {
      if('/' == orig[idx]) lastSlash = idx;
    }
    fileLen = idx - lastSlash + 1u;

    if(NULL != dirPart) {
      *dirPart = (char*) malloc((1u + lastSlash) * sizeof(char));
      if(NULL == *dirPart) {
        status = MALLOC_FAILED;
        continue;
      }
      /* Copy directory part into dirPart. */
      for(idx = 0u; idx < lastSlash; ++idx) (*dirPart)[idx] = orig[idx];
  
      /* Explicitly terminate, because input ends with a / */
      (*dirPart)[idx] = '\0';

    }

    if(NULL != filePart) {
      *filePart = (char*) malloc(fileLen * sizeof(char));
      if(NULL == *filePart) {
        status = MALLOC_FAILED;
        continue;
      }

      /* File part starts *after* the last /, if any */
      if('/' == orig[lastSlash]) ++lastSlash;


      /* Copy file part into filePart. */
      for(idx = 0u; idx < fileLen; ++idx)
        (*filePart)[idx] = orig[idx + lastSlash];
    }

  } while(0);

  if(OK != status) {
    if(NULL != filePart) {
      free(*filePart);
      *filePart = NULL;
    }
    if(NULL != dirPart) {
      free(*dirPart);
      *dirPart = NULL;
    }
  }
  return status;
}

static int AppendPath(const char* orig, const char* newSeg,
    char** outPath, int status) {
  char* tmpPath = NULL;
  if(OK != status) return status;
  do {
    size_t origLen;
    size_t newLen;
    unsigned char add_middle_slash = 1u;

    if(NULL == orig || NULL == newSeg || NULL == outPath) {
      status = NULL_POINTER;
      continue;
    }

    *outPath = NULL;

    origLen = strlen(orig);
    if(0u == origLen || '/' == orig[origLen - 1u]) add_middle_slash = 0u;
    
    newLen = strlen(newSeg);
    if(0u == newLen || '/' == *newSeg) add_middle_slash = 0u;

    /* Create space to hold the whole path. Allow room not only
       for the terminating 0, but for the / between the segments
       if needed. */
    tmpPath = (char*) malloc(
      (1u + add_middle_slash + origLen + newLen) * sizeof(char));

    if(NULL == tmpPath) {
      status = MALLOC_FAILED;
      continue;
    }

    strcpy(tmpPath, orig);
    if(add_middle_slash) strcat(tmpPath, "/");
    strcat(tmpPath, newSeg);

  } while(0);

  if(OK == status) *outPath = tmpPath;
  else free(tmpPath);

  return status;
}

static int InstallFiles(char* src, char* dest,
    char** cmd, InstallOptions* opts, int status) {
    char* destDir = NULL;
    char* destFile = NULL;
  if(OK != status) return status;
  do {
    static char* defaultCmd[] = { "cp", "-p", NULL };
    static char srcCpy[MAX_PATH_LEN];
    static char destCpy[MAX_PATH_LEN];
    static char* argv[MAX_NUM_ARGS];
    char** tmp;
    char* tmp_cp;
    char* full_cmd;
    hd_file_attributes_s srcAttribs;
    hd_file_attributes_s destAttribs;
    char destExists;
    char pwd[MAX_PATH_LEN];
    int char_count = 0;

    /* Confirm that the source exists, and that it is not a broken
       symlink, but do not throw an error if these conditions are
       not met. Instead issue a (suppressible) warning. */
    if(OK != HDgetFileAttributes(src, &srcAttribs, status) ||
        FT_LINK == srcAttribs.m_type) {
      if(ERR_IF_SRC_MISSING & *opts) {
        fprintf(stderr, "%s ERROR: file/directory does not exist; "
            "not installing %s\n", sBanner, src);
        status = SRC_FILE_MISSING;
      } else if(WARN_IF_SRC_MISSING & *opts) {
        printf("%s warning: file/directory does not exist; not installing %s\n",
          sBanner, src);
        fflush(stdout);
      }
      continue;
    }
    
    /* Find out whether the destination exists. */
    if(OK == HDgetFileAttributes(dest, &destAttribs, status))
      destExists = 1;
    else
      destExists = 0;

    /* Determine the directory and file portions of the destination,
       on a case-by-case basis. */
    if(FT_FILE == srcAttribs.m_type) {

      /* Case 1: source is file, destination does not exist. */
      if(!destExists) {
        /* If last character in dest is a /, make a directory and
           recurse. */
        for(tmp_cp = dest; '\0' != *tmp_cp && '\0' != *(tmp_cp + 1); ++tmp_cp)
          /* do nothing */;
        if('/' == *tmp_cp) {
          status = MakeDirectory(dest, 0755, status);
          status = InstallFiles(src, dest, cmd, opts, status);
          if(NON_DIRECTORY_EXISTS == status) {
            fprintf(stderr, "%s ERROR: some part of the following path exists, "
              "but is not a directory: %s\n", sBanner, dest);
          }
          continue;
        }

        /* Non-existent dest does not end with /, so just treat as a file. */
        status = SplitFile(dest, &destDir, &destFile, status);
      }

      /* Case 2: source is file, destination is file. */
      else if(FT_FILE == destAttribs.m_type) {
        status = SplitFile(dest, &destDir, &destFile, status);

        /* Do nothing if destination is more recent than source and the
           "force installation" option was not chosen. */
        if(srcAttribs.m_mtime <= destAttribs.m_mtime &&
            !(FORCE_INSTALL & *opts)) {
          if(VERBOSE & *opts) {
            printf("%s: Destination %s is up to date.\n", sBanner, dest);
            fflush(stdout);
          }
          continue;
        }
      }

      /* Case 3: source is file, destination is directory. */
      else if(FT_DIR == destAttribs.m_type) {
        char* srcFile = NULL;
        char* newDest = NULL;
        /* Append file part of src file name to destination. */
        status = SplitFile(src, NULL, &srcFile, status);
        status = AppendPath(dest, srcFile, &newDest, status);

        /* Recurse; newDest is now a file name one level "down". */
        status = InstallFiles(src, newDest, cmd, opts, status);
        free(srcFile);
        free(newDest);
        continue;
      }

      /* Case 4: source is file, destination is broken link. */
      else if(FT_LINK == destAttribs.m_type) {
        status = SplitFile(dest, &destDir, &destFile, status);
      }
      /* Anything else this program can't handle. */
      else status = INCOMPATIBLE_ARGUMENTS;
    }

    else if(FT_DIR == srcAttribs.m_type) {
      /* Case 5: source is dir, destination does not exist. */
      if(!destExists) {
        status = MakeDirectory(dest, 0755, status);
      }

      /* Case 6: source is dir, destination is file. */
      else if(FT_FILE == destAttribs.m_type) {
        status = INCOMPATIBLE_ARGUMENTS;
      }

      /* Case 7: source is dir, destination is directory. */
      else if(FT_DIR == destAttribs.m_type) {
        /* Make sure source and destination are not the same directory. */
        if(!HDdiffFileID(&srcAttribs, &destAttribs)) {
          if(VERBOSE & *opts) {
            printf("%s: Source %s is the same as destination %s\n",
              sBanner, src, dest);
            fflush(stdout);
          }
          continue;
        }
      }

      /* Case 8: source is dir, destination is broken link. */
      else if(FT_LINK == destAttribs.m_type) {
        status = remove(dest);
        if(OK != status) status = errno ? errno : status;
        status = MakeDirectory(dest, 0755, status);
      }

      /* Anything else this program can't handle. */
      else status = INCOMPATIBLE_ARGUMENTS;

      status = InstallDirectory(src, dest, cmd, opts, status);

      continue;
    }

    /* Anything else this program can't handle. */
    else status = INCOMPATIBLE_ARGUMENTS;

    if(INCOMPATIBLE_ARGUMENTS == status) {
      fprintf(stderr, "%s ERROR: do not know how to install %s to %s\n",
        sBanner, src, dest);
    } else if(NON_DIRECTORY_EXISTS == status) {
        fprintf(stderr, "%s ERROR: some part of the following path exists, "
          "but is not a directory: %s\n", sBanner, dest);
    }

    if(OK != status) continue;

    if(VERBOSE & *opts) {
      printf("%s: Installing %s in %s\n", sBanner, src, dest);
      fflush(stdout);
    }

    /* At this point, src is the full name of a real source file, dest is
       a name which either is or can be interpreted as the full name of
       a real destination file, destDir is the name of the destination
       directory only, and destFile is the name of the file part only
       of the destination. */

    /* Decide which option: absolute or relative paths, and adjust
       src accordingly. */
    if(ABSOLUTE_PATH & *opts) {
      status = MakeAbsolutePath(src, &tmp_cp, status);
    } else {
      status = MakeRelativePath(src, destDir, &tmp_cp, status);
    }

    if(OK != status) continue;

    /* Need static copies of the src and destination, so make sure
       neither is too long. Note that the child cds to the destination
       directory, so here we are interested only in the file name portion
       of the destination. */
    if(MAX_PATH_LEN - 1 < strlen(tmp_cp) || MAX_PATH_LEN - 1 < strlen(destFile))
    {
      free(tmp_cp);
      status = PATH_TOO_LONG;
      continue;
    }

    /* Make static copies. */
    strcpy(srcCpy, tmp_cp);
    strcpy(destCpy, destFile);

    /* Done with tmp_cp. */
    free(tmp_cp);

    /* Use default command if no command was given. */
    if(NULL == cmd || NULL == *cmd) cmd = defaultCmd;

    /* Copy command array into new argv */
    for(tmp = argv; NULL != *cmd; ++cmd, ++tmp) *tmp = *cmd;

    /* Append copies of src and destination files, and terminating NULL. */
    *tmp++ = srcCpy;
    *tmp++ = destCpy;
    *tmp++ = NULL;

    /* Count up number of characters in the command, including room
       for a space between each word and quotes around the argument. */
    for (tmp = argv; NULL != *tmp; ++tmp) char_count += strlen(*tmp) + 3;

    full_cmd = (char *) malloc(char_count * sizeof(char));
    *full_cmd = '\0';

    /* Copy the whole command into one buffer. */
    for (tmp = argv; ; ++tmp) {
      strcat(full_cmd, "'");
      strcat(full_cmd, *tmp);
      strcat(full_cmd, "'");
      if (NULL == *(tmp + 1)) break;
      strcat(full_cmd, " ");
    }

    /* Ready to fork/exec the install command. At this point, everything
       which can go wrong should have, so try to remove the file. */
    if(destExists) {
      status = remove(dest);
      if(OK != status) status = errno ? errno : status;
    }

    if(OK != status) continue;

    status = MakeDirectory(destDir, 0755, status);
    if(NON_DIRECTORY_EXISTS == status) {
      fprintf(stderr, "%s ERROR: some part of the following path exists, "
        "but is not a directory: %s\n", sBanner, destDir);
    }
    status = HDgetCurrentDirectory(pwd, status);
    status = ChangeDirectory(destDir, status);
    if(OK != status) continue;
    
    status = system(full_cmd);

    status >>= 8;

    ChangeDirectory(pwd, OK);

    free(full_cmd);
    
  } while(0);

  free(destFile);
  free(destDir);

  return status;
}

/* Note: the following function assumes both src and dest are real,
   actual, stat-able directories! */
static int InstallDirectory(const char* src, const char* dest,
    char** cmd, InstallOptions* opts, int status) {
  char* newSrc = NULL;
  char* newDest = NULL;

  if(OK != status) return status;
  do {
    hd_dir_s dir_struct;
    const char* fName = NULL;

    /* Open the directory. */
    status = HDopenDirectory(src, &dir_struct, status);
    if (OK != status) continue;

    /* Read the directory, and use each entry to construct new
       src and new dest. */
    while(OK == (status = HDreadDirectory(&dir_struct, &fName, status))) {
      if(0 == strcmp(fName, ".") || 0 == strcmp(fName, ".."))
        continue;
      status = AppendPath(src, fName, &newSrc, status);
      status = AppendPath(dest, fName, &newDest, status);
      status = InstallFiles(newSrc, newDest, cmd, opts, status);
    }

    if (FINISHED == status) status = OK;

    HDcloseDirectory(&dir_struct, OK);

  } while(0);

  free(newDest);
  free(newSrc);
  return status;
}

static void Delete2Darray(char** array) {
  char** tmp;
  if(NULL != array)
    for(tmp = array; NULL != *tmp; ++tmp) free(*tmp);
  free(array);
}
/******************************************************************************
 * Global function definitions                                                *
 ******************************************************************************/
int HDinstall_file(int argc, char** argv, int status) {
  char* src = NULL;
  char* dest = NULL;
  if(OK != status) return status;
  do {
    const char* usage = "usage: hd_install [-aefhrsvw] src-file dest-file";
    char** nextArg;
    char** tmpArg;
    char* tmp_cp;
    InstallOptions opts = DEFAULT;

    /* Process command line options which apply to this program. */
    nextArg = FindFirstNonOption(argv);
    for(tmpArg = argv + 1; tmpArg < nextArg; ++tmpArg) {
      /* Handle special cases for help and version info. */
      if(0 == strcmp(*tmpArg, "--help")) strcpy(*tmpArg, "-h");
      else if(0 == strcmp(*tmpArg, "--version")) strcpy(*tmpArg, "-v");

      /* Skip the first character, which is -, but then check each
         subsequent character against the known options. */
      for(tmp_cp = *tmpArg + 1u; '\0' != *tmp_cp; ++tmp_cp) {
        switch(*tmp_cp) {
          case 'a':
            if(RELATIVE_PATH & opts) {
              fprintf(stderr, sErrFmt, sBanner,
                  "Cannot specify both -a and -r");
              status = INVALID_OPTION;
            }
            opts |= ABSOLUTE_PATH;
            break;
          case 'e':
            opts |= ERR_IF_SRC_MISSING;
            break;
          case 'f':
            opts |= FORCE_INSTALL;
            break;
          case 'h':
            printf("%s: %s\n", sBanner, usage);
            printf("%s: Arguments:\n", sBanner);
            printf("%s: -a use absolute path to source.\n", sBanner);
            printf("%s: -e give error if source does not exist "
                "(default is warning only).\n", sBanner);
            printf("%s: -f force installation even if destination is more recent.\n", sBanner);
            printf("%s: -h print this message.\n", sBanner);
            printf("%s: -r use relative path to source (default).\n", sBanner);
            printf("%s: -s silent operation (errors still reported).\n",
                sBanner);
            printf("%s: -v version information.\n", sBanner);
            printf("%s: -w if source does not exist, give no warning "
                "(silent no-op).\n", sBanner);
            printf("%s: -- signifies (forces) end of options.\n", sBanner);
            fflush(stdout);
            opts |= DONE;
            break;
          case 'r':
            if(ABSOLUTE_PATH & opts) {
              fprintf(stderr, sErrFmt, sBanner,
                  "Cannot specify both -a and -r");
              status = INVALID_OPTION;
            }
            opts |= RELATIVE_PATH;
            break;
          case 's':
            opts &= ~VERBOSE;
            break;
          case 'v':
            printf("%s: %s\n", sBanner, "version 1.0");
            fflush(stdout);
            opts |= DONE;
            break;
          case 'w':
            opts &= ~WARN_IF_SRC_MISSING;
            break;
          default:
            fprintf(stderr, "%s ERROR: Invalid option %c\n", sBanner, *tmp_cp);
            status = INVALID_OPTION;
            break;
        }
        if(OK != status || DONE & opts) break;
      }
      if(OK != status || DONE & opts) break;
    }

    if(OK != status || DONE & opts) continue;

    /* First argument is required to be the name of the source. */
    if(NULL != *nextArg && '\0' != **nextArg) {
      status = CleanPath(*nextArg, &src, status);
      ++nextArg;
    } else {
      fprintf(stderr, sErrFmt, sBanner, usage);
      status = MISSING_ARG;
      continue;
    }

    /* Next argument is required to be the name of the destination. */
    if(NULL != *nextArg && '\0' != **nextArg) {
      status = CleanPath(*nextArg, &dest, status);
      ++nextArg;
    } else {
      fprintf(stderr, sErrFmt, sBanner, usage);
      status = MISSING_ARG;
      continue;
    }

    /* Give information. */
    /* Perform installation. */
    status = InstallFiles(src, dest, nextArg, &opts, status);

  } while(0);

  free(dest);
  free(src);

  return status;
}

int main(int argc, char** argv) {
  int status = CheckCmdLine(argc, argv, OK);

  status = HDinstall_file(argc, argv, status);

  if(ERROR_START <= status && ERROR_STOP >= status);
  else if(OK != status) perror(sBanner);
  return status;
}
/******************************************************************************
 * $Log: hd_install.c,v $
 * Revision 1.15  2016/09/12 14:44:43  jasercio
 * Synchronization with with El Capitan patch release 20160831
 *
 * Revision 1.11.36.2  2016/06/27 19:23:41  jasercio
 * El Capitan Update:  Updated ST heacore packages to HEAD version
 *
 * Revision 1.14  2015/09/16 14:53:25  irby
 * Restore revision 1.12.
 *
 * Revision 1.12  2014/08/05 20:25:04  peachey
 * Add single quotes around each argument when constructing a command.
 * This prevents special characters from being processed by the shell
 * when system() is used to execute the command.
 *
 * Revision 1.11  2004/04/09 18:33:04  irby
 * Apply bug fix submitted by Yoshitaka Ishisaki (Astro-E2/ISAS) in
 * ResolveLinks: when a relative symbolic path is found, preserve the
 * leading slash (if any) when resolving/substituting the value of the
 * link back into the working path.
 *
 * Revision 1.10  2003/10/27 19:23:33  peachey
 * First cut at meaningful native Windows functions to perform detailed
 * file and directory attribute-related functions (replacemements for
 * unistd.h and other non-ISO features not present in Visual Studio
 * environment.
 *
 * Revision 1.8  2003/10/24 14:43:42  peachey
 * More changes for Windows: isolate calls to stat inside a new function
 * HDgetFileAttributes. Isolate opendir/readdir/closedir inside new
 * functions HDopenDirectory/HDreadDirectory/HDcloseDirectory.
 *
 * Revision 1.7  2003/10/23 16:20:51  peachey
 * More refactorings for Windows support. Encapsulate calls to
 * opendir/readdir inside new functions HDopenDirectory/HDreadDirectory.
 * Begin to add #ifdef WIN32 as needed.
 *
 * Revision 1.6  2003/10/23 14:35:14  peachey
 * Changes to support Windows. Replace fork/exec with system.
 *
 * Revision 1.5  2002/10/21 15:52:32  peachey
 * Add -f option to force installation even if the destination is more
 * recent than the source.
 *
 * Revision 1.4  2002/10/17 16:26:15  peachey
 * Put shell status into correct range by right shifting it 8 bits.
 *
 * Revision 1.3  2002/08/13 16:32:40  peachey
 * Oops, clean up that last commit.
 *
 * Revision 1.2  2002/08/13 16:28:21  peachey
 * Correct complete misuse of getcwd. Why did it ever work the way it was?
 *
 * Revision 1.1  2002/08/07 20:39:00  peachey
 * Installation utility, which is *not* a standard UNIX install utility,
 * but handles installations conveniently in the context of a Makefile.
 *
 ******************************************************************************/
