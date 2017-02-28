/*
 * $Source: /headas/headas/attitude/tasks/prefilter/pill.c,v $
 * $Revision: 1.4 $
 * $Date: 2005/09/14 21:41:12 $
 */

#include <stdio.h>	/* pil.h depends on stdio.h but does not include it */

#include "pil.h"
#include "pill.h"
#include "report.h"


static void pill_error (const char * module, const char * parameter, int status)
{
	report_t info = { 0 };
	info.module = module;
	report_detail(&info, REPORT_ERROR,
		"couldn't get parameter '%s' [%s]\n",
		parameter, PIL_err_handler(status));
}


int PILLGetInt (const char * name, int * output)
{
	int status = PILGetInt(name, output);
	if (status)
		pill_error(__func__, name, status);
	return status;
}


int PILLGetReal (const char * name, double * output)
{
	int status = PILGetReal(name, output);
	if (status)
		pill_error(__func__, name, status);
	return status;
}


int PILLGetString (const char * name, char * output)
{
	int status = PILGetString(name, output);
	if (status)
		pill_error(__func__, name, status);
	return status;
}


int PILLGetBool (const char * name, int * output)
{
	int status = PILGetBool(name, output);
	if (status)
		pill_error(__func__, name, status);
	return status;
}


int PILLGetFname (const char * name, char * output)
{
	int status = PILGetFname(name, output);
	if (status)
		pill_error(__func__, name, status);
	return status;
}


int PILLGetRealVector (const char * name, int length, double * output)
{
	int status = PILGetRealVector(name, length, output);
	if (status)
		pill_error(__func__, name, status);
	return status;
}


