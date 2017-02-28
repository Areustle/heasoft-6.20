/*
 * $Source: /headas/headas/attitude/lib/rew/report.c,v $
 * $Revision: 1.5 $
 * $Date: 2011/10/12 21:51:26 $
 *
 *
 * $Log: report.c,v $
 * Revision 1.5  2011/10/12 21:51:26  rwiegand
 * Moved math functions into library.
 *
 * Revision 1.4  2006/10/31 19:15:12  rwiegand
 * Update chat level of monitor messages.
 *
 * Revision 1.3  2005/08/29 11:46:01  rwiegand
 * Added debug message control.
 *
 * Revision 1.2  2003/07/25 20:02:15  rwiegand
 * Updates for WCS compliance.
 *
 * Revision 1.1  2003/05/14 13:41:39  rwiegand
 * Support for logging and reading/writing FITS images and keywords.
 *
 */


#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>

#include "report.h"


#define STATIC_REPORT_FUNCS 8
#define CODE_STRING_BUFFER 80



static int used = 0;
static int space = STATIC_REPORT_FUNCS;
static int (*functions0[STATIC_REPORT_FUNCS])(report_t *);
static int (**functions)(report_t *) = functions0;

static int DEBUG = 0;



static int distribute_info (report_t * info)
{
static int active = 0;
	int i = 0;

	if (!active)
		{
			active = 1;
			for (; i < used; ++i)
				{
					if ((*functions[i])(info))
						i = used;
				}
			active = 0;
		}
	else
		{
			report_t x = { __func__, __FILE__, REPORT_WARNING, 0,
							"recursive report attempted\n" };
			report_stdout(&x);
			report_stdout(info);
		}

	return i;
}




int report_info (report_t * info)
{
	return distribute_info(info);
}


int add_report_function (int (*f)(report_t *info))
{
	int code = 0;
	report_t info = { __func__ };

	if (!f)
		{
			code = REPORT_BAD_FUNCTION;
			report_detail(&info, REPORT_WARNING, "bad function\n");
		}

	if (used == space)
		{
			if (functions != functions0)
				{
					int n;
					int (**x)(report_t *);
					n = space + 10;
					x = realloc(functions, n * sizeof(int (*)(report_t *)));
					if (x != functions)
						{
							space = n;
							functions = (int (**)(report_t *)) x;
						}
					else
						{
							code = REPORT_ALLOCATE_FAILED;
							report_detail(&info, REPORT_WARNING, "reallocation failed\n");
						}
				}
			else
				{
					int n;
					void * x;
					n = 2 * STATIC_REPORT_FUNCS;
					x = malloc(n * sizeof(int (*)(report_t *)));
					if (x)
						{
							space = n;
							functions = (int (**)(report_t *)) x;
						}
					else
						{
							code = REPORT_ALLOCATE_FAILED;
							report_detail(&info, REPORT_WARNING, "allocation failed\n");
						}
				}
		}

	if (!code)
		{
			functions[used] = f;
			++used;
		}

	return code;
}


int remove_report_function (int (*f)(report_t *))
{
	int code = 0;
	int found, i;

	found = 0;

	for (i = used; !found && (i > 0); --i)
		{
			if (functions[i-1] == f)
				{
					int j;
					found = 1;
					--used;
					for (j = i-1; j < used; ++j)
						functions[j] = functions[j+1];
				}
		}

	if (!found)
		{
			report_t info = { "remove_report_function" };
			code = REPORT_BAD_FUNCTION;
			report_detail(&info, REPORT_WARNING, "invalid input\n");
		}

	return code;
}
 

int shutdown_report_module ()
{
	if (functions != functions0)
		free(functions);

	functions = functions0;
	used = 0;
	space = STATIC_REPORT_FUNCS;

	return 0;
}
 

int report_detail_aux (report_t * info, const char * format, va_list * args)
{
	char buffer[10240];

#ifdef HAVE_vsnprintf
	vsnprintf(buffer, sizeof(buffer), format, *args);
#else
	vsprintf(buffer, format, *args);
#endif

	info->string = buffer;

	return report_info(info);
}


int report_detail (report_t * info, int code, const char * format, ...)
{
	int result;
	va_list args;
	va_start(args, format);

	if (code != REPORT_DEFAULT)
		info->code = code;

	result = report_detail_aux(info, format, &args);

	va_end(args);
	return result;
}


/*
 * TODO: allow user to specify their own codes and code_strings
 */

const char * report_code_string (int code, char * space)
{
	const char * p = 0;

	switch (code) {

		case REPORT_NULL:
			p = "null";
			break;

		case REPORT_NONE:
			p = "none";
			break;

		case REPORT_STATUS:
			p = "status";
			break;

		case REPORT_EXCEPTION:
			p = "exception";
			break;

		case REPORT_ERROR:
			p = "error";
			break;

		case REPORT_WARNING:
			p = "warning";
			break;

		case REPORT_DEBUG:
			p = "debug";
			break;

		case REPORT_VERBOSE:
			p = "verbose";
			break;

		case REPORT_MONITOR:
			p = "monitor";
			break;

		case REPORT_SILENT:
			p = "silent";
			break;

		case REPORT_FLUSH:
			p = "flush";
			break;

		default:
			p = "unknown";
			if (space)
				{
					sprintf(space, "%d", code);
					p = space;
				}

	}

	return p;
}


int report_stdout (report_t * info)
{
	if (info->code == REPORT_FLUSH)
		{
			fflush(stdout);
		}
	else if (info->code == REPORT_NONE)
		{
			if (info->module)
				printf("%s: %s", info->module, info->string);
			else
				printf("%s", info->string);
		}
	else if (info->code != REPORT_SILENT)
		{
			char buffer[CODE_STRING_BUFFER];
			const char * code = report_code_string(info->code, buffer);
			if (info->module)
				printf("%s: %s: %s", code, info->module, info->string);
			else
				printf("%s: %s", code, info->string);
		}

	return 0;
}



int report_code_aux (int code, const char * format, va_list * args)
{
	report_t info = { 0 };
	info.code = code;
	return report_detail_aux(&info, format, args);
}


int report_code (int code, const char * format, ...)
{
	int result;
	va_list args;
	va_start(args, format);
	result = report_code_aux(code, format, &args);
	va_end(args);
	return result;
}


#if 0

#define REPORT_TEMPLATE(_function,_code) \
int _function (const char * format, ...) \
{ \
	char buffer[4096]; \
	report_t info = { 0 }; \
	int result; \
	va_list args; \
	va_start(args, format); \
	vsprintf(buffer, format, args); \
	va_end(args); \
	info.code = _code; \
	info.string = buffer; \
	return report_info(&info); \
}

REPORT_TEMPLATE(report_status, REPORT_STATUS)
REPORT_TEMPLATE(report_error, REPORT_ERROR)
REPORT_TEMPLATE(report_warning, REPORT_WARNING)
REPORT_TEMPLATE(report_verbose, REPORT_VERBOSE)
REPORT_TEMPLATE(report_debug, REPORT_DEBUG)

#else


int report_status (const char * format, ...)
{
	int code;
	va_list args;
	va_start(args, format);
	code = report_code_aux(REPORT_STATUS, format, &args);
	va_end(args);
	return code;
}


int report_error (const char * format, ...)
{
	int result;
	va_list args;
	va_start(args, format);
	result = report_code_aux(REPORT_ERROR, format, &args);
	va_end(args);
	return result;
}


int report_warning (const char * format, ...)
{
	int result;
	va_list args;
	va_start(args, format);
	result = report_code_aux(REPORT_WARNING, format, &args);
	va_end(args);
	return result;
}


int report_verbose (const char * format, ...)
{
	int result;
	va_list args;
	va_start(args, format);
	result = report_code_aux(REPORT_VERBOSE, format, &args);
	va_end(args);
	return result;
}


int report_debug (const char * format, ...)
{
	int result;
	va_list args;
	va_start(args, format);
	result = report_code_aux(REPORT_DEBUG, format, &args);
	va_end(args);
	return result;
}


int report_monitor (const char * format, ...)
{
	int result;
	va_list args;
	va_start(args, format);
	result = report_code_aux(REPORT_MONITOR, format, &args);
	va_end(args);
	return result;
}

#endif


#include "headas.h"


int STDERR_CHATTER = 1;


int stderr_chatter (int chatter)
{
	int old = STDERR_CHATTER;
	STDERR_CHATTER = chatter;
	return old;
}


int report_headas (report_t * info)
{
	int chat = 0;
	int error = 0;

	/* map report code to headas chat code */
	switch (info->code)
		{
			case REPORT_NULL:
			case REPORT_SILENT:
				chat = -1;
				break;

			case REPORT_ERROR:
			case REPORT_EXCEPTION:
				chat = 1;
				error = 1;
				break;

			case REPORT_WARNING:
				error = 2;
				chat = 2;
				break;

			case REPORT_MONITOR:
				chat = 0;
				break;

			case REPORT_STATUS:
				chat = 3;
				break;

			case REPORT_VERBOSE:
				chat = 4;
				break;

			case REPORT_DEBUG:
				chat = 5;
				break;

			default:
				chat = 3;
				break;
		}

	if (chat >= 0)
		{
			char buffer[CODE_STRING_BUFFER];

			const char * code = report_code_string(info->code, buffer);

			if (info->module)
				headas_chat(chat, "%s: %s: %s", code, info->module, info->string);
			else
				headas_chat(chat, "%s: %s", code, info->string);

			if (error && error <= STDERR_CHATTER)
			{
				if (info->module)
					HD_fprintf(hd_err, "%s: %s: %s", code, info->module, info->string);
				else
					HD_fprintf(hd_err, "%s: %s", code, info->string);
			}
		}

	report_fits_errors();

	return 0;
}


int report_fits_errors ()
{
	char buffer[1024];
	int count = 0;

	while (fits_read_errmsg(buffer))
		{
			++count;
			HD_fprintf(hd_err, "fitsio: %s\n", buffer);
		}

	return count;
}



void debug_set (int flags)
{
	DEBUG = flags;
}


void debug_toggle (int flags)
{
	DEBUG ^= flags;
}


int debug_test (int flags)
{
	return DEBUG & flags;
}

