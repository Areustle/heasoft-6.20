/*****************************
    Write_ANLCmodule
*****************************/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <unistd.h>

static char template_c[] = "\
/**************************************/\n\
/*\n\
/*  %s\n\
/*\n\
/*	%s created by %s\n\
/*\n\
/**************************************/\n\
\n\
#include <stdio.h>\n\
#include <stdlib.h>\n\
#include <string.h>\n\
#include \"cfortran.h\"\n\
#include \"hbook.h\"\n\
#include \"cli.h\"\n\
#include \"com.h\"\n\
#include \"bnk.h\"\n\
#include \"evs.h\"\n\
#include \"anl.h\"\n\
#include \"anl_misc.h\"\n\
\n\
char %s_version[] = \"version ?.?\";\n\
\n\
void\n\
%s_startup(int *status)\n\
{\n\
	*status = ANL_OK;\n\
}\n\
\n\
void\n\
%s_init(int *status)\n\
{\n\
	EVSDEF(\"%s:BEGIN\");\n\
	EVSDEF(\"%s:ENTRY\");\n\
	EVSDEF(\"%s:OK\");\n\
	\n\
	*status = ANL_OK;\n\
}\n\
\n\
void\n\
%s_com(int *status)\n\
{\n\
	*status = ANL_OK;\n\
}\n\
\n\
void\n\
%s_his(int *status)\n\
{\n\
	*status = ANL_OK;\n\
}\n\
\n\
void\n\
%s_bgnrun(int *status)\n\
{\n\
	EVSSET(\"%s:BEGIN\");\n\
	\n\
	*status = ANL_OK;\n\
}\n\
\n\
void\n\
%s_ana(int nevent, int eventid, int *status)\n\
{\n\
	EVSSET(\"%s:ENTRY\");\n\
	\n\
	EVSSET(\"%s:OK\");\n\
	\n\
	*status = ANL_OK;\n\
}\n\
\n\
void\n\
%s_endrun(int *status)\n\
{\n\
	*status = ANL_OK;\n\
}\n\
\n\
void\n\
%s_exit(int *status)\n\
{\n\
	*status = ANL_OK;\n\
}\n\
";

static char template_f[] = "\
c======================================\n\
c\n\
c %s\n\
c\n\
c      %s created by %s\n\
c\n\
c======================================\n\
\n\
      Subroutine %s_startup(status)\n\
      Implicit NONE\n\
c output\n\
      Integer status\n\
c\n\
      status = ANL_OK\n\
      Return\n\
      End\n\
\n\
      Subroutine %s_init(status)\n\
      Implicit NONE\n\
c output\n\
      Integer status\n\
c include\n\
      Include 'Includes.inc'\n\
c EVS\n\
      Call EVSdef('%s:BEGIN')\n\
      Call EVSdef('%s:ENTRY')\n\
      Call EVSdef('%s:OK')\n\
c\n\
      Call ANL_put_version('%s', 'version 1.0')\n\
      status = ANL_OK\n\
      Return\n\
      End\n\
\n\
      Subroutine %s_com(status)\n\
      Implicit NONE\n\
c output\n\
      Integer status\n\
c include\n\
      Include 'Includes.inc'\n\
c\n\
      status = ANL_OK\n\
      Return\n\
      End\n\
\n\
      Subroutine %s_his(status)\n\
      Implicit NONE\n\
c output\n\
      Integer status\n\
c include\n\
      Include 'Includes.inc'\n\
c\n\
      status = ANL_OK\n\
      Return\n\
      End\n\
\n\
      Subroutine %s_bgnrun(status)\n\
      Implicit NONE\n\
c output:\n\
      Integer status\n\
c include\n\
      Include 'Includes.inc'\n\
c function:\n\
      Logical EVS\n\
c  EVS set\n\
      Call EVSset('%s:BEGIN')\n\
c\n\
      status = ANL_OK\n\
      Return\n\
      End\n\
\n\
      Subroutine %s_ana(nevent, eventid, status)\n\
      Implicit NONE\n\
c input:\n\
      Integer nevent, eventid\n\
c output:\n\
      Integer status\n\
c include\n\
      Include 'Includes.inc'\n\
c function:\n\
      Logical EVS\n\
c\n\
      Call EVSset('%s:ENTRY')\n\
c\n\
      Call EVSset('%s:OK')\n\
c\n\
      status = ANL_OK\n\
      Return\n\
      End\n\
\n\
      Subroutine %s_endrun(status)\n\
      Implicit NONE\n\
c output\n\
      Integer status\n\
c include\n\
      Include 'Includes.inc'\n\
c\n\
      status = ANL_OK\n\
      Return\n\
      End\n\
\n\
      Subroutine %s_exit(status)\n\
      Implicit NONE\n\
c output\n\
      Integer status\n\
c include\n\
      Include 'Includes.inc'\n\
c\n\
      status = ANL_OK\n\
      Return\n\
      End\n\
";

static void
Write_ANLmodule(tmpl, file, Cname, Uname, Lname)
	char *tmpl, *file, *Cname, *Uname, *Lname;
{
	time_t t;
	struct tm *ji;
	char date[20];
	FILE *fp;

	if ((file[0]=='-') && (strlen(file)==1)) {
	  fp = stdout;
	} else {
	  fp = fopen(file, "w");
	  if ( NULL == fp ) return;
	}
	time(&t);
	ji = localtime(&t);
	sprintf(date, "%04d/%02d/%02d",
			1900+ji->tm_year, ji->tm_mon+1, ji->tm_mday);
	fprintf(fp, tmpl,
			Cname, date, getlogin(),
			Cname,								/* version */
			Cname,								/* startup */
			Cname, Cname, Cname, Cname,			/* init */
			Cname,								/* com */
			Cname,								/* his */
			Cname, Cname,						/* bgnrun */
			Cname, Cname, Cname,				/* ana */
			Cname,								/* endrun */
			Cname,								/* exit */
			Cname,								/* comment */
			NULL								/* dummy */
			);
	fclose(fp);
}

void
Write_AnlCmodule(file, Cname, Uname, Lname)
	char *file, *Cname, *Uname, *Lname;
{
	Write_ANLmodule(template_c, file, Cname, Uname, Lname);
}

void
Write_AnlFmodule(file, Cname, Uname)
	char *file, *Cname, *Uname;
{
	Write_ANLmodule(template_f, file, Cname, Uname, NULL);
}
