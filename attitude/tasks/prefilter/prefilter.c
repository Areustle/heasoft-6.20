/*
 * $Source: /headas/headas/attitude/tasks/prefilter/prefilter.c,v $
 * $Revision: 1.16 $
 * $Date: 2005/09/14 21:41:12 $
 *
 * $Log: prefilter.c,v $
 * Revision 1.16  2005/09/14 21:41:12  rwiegand
 * Deleted local copy of report.[ch] and updated report calling sequence.
 * Pruned unused mean alignment structure.
 *
 * Revision 1.15  2005/03/04 19:33:53  rwiegand
 * Reload FITS header after writing TNULLn.
 *
 * Revision 1.14  2004/12/30 22:56:26  rwiegand
 * Implemented writing NULLs for parameters which can not be calculated
 * at particular timestamps because of missing dependencies (e.g., pointing
 * vector cannot be found without attitude).  Require an attitude record
 * within attextrap of output record time or set attitude (and attitude
 * dependent parameters) to NULL.
 *
 * Revision 1.13  2004/12/13 22:30:34  rwiegand
 * Corrected spelling of TUNITn keyword.
 *
 * Revision 1.12  2003/11/26 21:06:37  rwiegand
 * Updated parameter stamping call.
 *
 * Revision 1.11  2003/06/23 22:57:37  rwiegand
 * Added iteration code that indicates that current iteration should be skipped.
 *
 * Revision 1.10  2003/03/31 14:42:13  rwiegand
 * Was failing to capture error code during setup.  Deleted dead code.
 *
 * Revision 1.9  2003/03/20 19:02:04  rwiegand
 * Call fits_write_chksum (set DATASUM, CHECKSUM) before closing the output file.
 *
 * Revision 1.8  2003/01/22 17:32:43  rwiegand
 * Added parameter for writing HISTORY keywords to output.  Fixed conversion
 * of string to AtTime.
 *
 * Revision 1.7  2002/12/06 20:14:21  rwiegand
 * Added Filter object to pass function pointers for iteration, initialization,
 * status checking.  Broke derive.c into separate files for FORTRAN calling
 * routines, iteration, initialization.  Made compare mode less XTE-centric.
 * Added parameters for pointing axis and boresight.  Allow loading two line
 * elements from FITS or text files.
 *
 * Revision 1.6  2002/11/26 20:13:19  rwiegand
 * Corrected problem with duplicate final record in output.  Made TLE input
 * more robust.
 *
 * Revision 1.5  2002/03/21 19:36:56  rwiegand
 * Added support for FITS header keywords
 *
 * Revision 1.4  2002/03/15 18:12:40  rwiegand
 * Updated FITS columns with comments (TTYPExxx) and units (TUNITSxxx).  Apply
 * clobber parameter
 *
 * Revision 1.3  2002/01/31 13:54:10  rwiegand
 * Renamed tool prefilter
 *
 * Revision 1.2  2002/01/30 19:48:58  rwiegand
 * Fixed output problem.  The derived values were not being converted from
 * the derived type to the FITS type before output which resulted in gargage
 * output.
 *
 * Revision 1.1  2002/01/28 16:09:02  rwiegand
 * Initial revision
 *
 */

#include <stdlib.h>
#include <stdio.h>
#include <ctype.h>
#include <string.h>

#include "fitsio.h"           /* fits */
#include "headas_utils.h"

#include "prefilter.h"        /* prefilter */
#include "report.h"           /* prefilter */


/*
 * prefilter internal definitions
 */

/*
 * Details is based on the input parameters and holds information
 * needed to set up the Context for an iteration
 */

/* prefilter core data */
struct Details
{
	int code;

	int parameterCount;
	int parameterSpace;
	Parameter ** parameters;

	int orderedCount;
	Parameter ** orderedParameters;

	fitsfile * fptr;
	int fitsRow;

	Filter * filter;
};


static int put_keyword_fits (fitsfile * fptr, Keyword * keyword, int * status);


int details_code (const Details * details)
{
	return details->code;
}


static int prepare_output (Details * details)
{
	fitsfile * fptr = 0;
	int status = 0;
	Output * output = details->filter->output;

	if (!details->code)
		{
			char buffer[FLEN_FILENAME];
			const char * outname = output->outname;
			const char * extension = output->extension;
			const char * clobber = "";

			if (!outname)
				outname = "PREFILTER.fits";

			if (!extension)
				extension = "PREFILTER";

			if (output->clobber)
				if (outname[0] != '!')
					clobber = "!";

			sprintf(buffer, "%s%s", clobber, outname);

			fits_create_file(&fptr, buffer, &status);
			if (status)
				report_warning("unable to create FITS output %s [%d]\n",
						buffer, status);
			else
				{
					strcpy(buffer, extension);
					fits_create_tbl(fptr, BINARY_TBL, 0,
							0, 0, 0, 0, buffer, &status);
					if (status)
						report_warning("unable to create table %s [%d]\n",
								buffer, status);
				}

				if (status)
					details->code = PREFILTER_ERROR;
		}

	if (!details->code)
		{
			int i;
			int column = 0;

			for (i = 0; i < details->parameterCount; ++i)
				{
					Parameter * p = details->parameters[i];
					if (p->mode == PREFILTER_OUTPUT)
						{
							char datatype = 0;
							int repeat;
							char tform[64];
							int inttype = 0;

							if (p->fitsArray > 1)
								repeat = p->fitsArray;
							else
								repeat = 1;

							switch (p->fitsType)
								{
									case TBIT:
										datatype = 'X';
										break;

									case TBYTE:
										datatype = 'B';
										inttype = 1;
										break;

									case TLOGICAL:
										datatype = 'L';
										break;

									case TSTRING:
										datatype = 'A';
										break;

									case TSHORT:
										datatype = 'I';
										inttype = 1;
										break;

									case TINT: /* not used? */
										details->code = PREFILTER_ERROR;
										datatype = '?';
										break;

									case TLONG:
										datatype = 'J';
										inttype = 1;
										break;

									case TFLOAT:
										datatype = 'E';
										break;

									case TDOUBLE:
										datatype = 'D';
										break;

									default:
										details->code = PREFILTER_ERROR;
										report_error("parameter %s has unexpected type %d\n",
												p->name, p->fitsType);

								}

							if (!details->code)
								{
									char *name = (char *) p->fitsName;

									sprintf(tform, "%d%c", repeat, datatype);

									++column;
									p->fitsColumn = column;

									fits_insert_col(fptr, column, name, tform, &status);
									if (status)
										report_warning("unable to insert %s column\n", name);

									/* assign comment string */
									if (!status && p->fitsComment)
										{
											char keyname[FLEN_KEYWORD];
											char comment[FLEN_COMMENT];
											sprintf(keyname, "TTYPE%d", p->fitsColumn);
											strncpy(comment, p->fitsComment, sizeof(comment));
											fits_modify_comment(fptr, keyname, comment, &status);
											if (status)
												report_warning("unable to insert %s comment [%s]\n",
														keyname, comment);
										}

									if (!status && p->fitsUnits)
										{
											char keyname[FLEN_KEYWORD];
											char units[FLEN_VALUE];
											sprintf(keyname, "TUNIT%d", p->fitsColumn);
											strncpy(units, p->fitsUnits, sizeof(units));
											fits_write_key(fptr, TSTRING, keyname, &units,
													"physical units of field", &status);
											if (status)
												report_warning("unable to insert %s units [%s]\n",
														keyname, units);
										}

									if (!status && p->fitsNull && inttype)
										{
											sprintf(p->fitsNull->name, "TNULL%d", p->fitsColumn);
											put_keyword_fits(fptr, p->fitsNull, &status);
											if (status)
												report_warning("unable to insert %s TNULLn [%d]\n",
														p->fitsNull->name, status);
										}
								}
						}
				}

			if (!column)
				{
					details->code = PREFILTER_DONE;
					report_warning("no columns selected for output\n");
				}

			if (fits_set_hdustruc(fptr, &status))
				report_warning("unable to reset FITS structure [%d]\n");

			if (status)
				{
					details->code = PREFILTER_ERROR;
					report_error("unable to create FITS file\n");
				}
		}

	if (!details->code)
			details->fptr = fptr;

	return details->code;
}


/*
 * convert from derivedType to fitsType
 * pin points to repeat primitives of derivedType
 * pout points to (space for) repeat primitives of fitsType
 */
typedef union
{
	void * pvoid;
	unsigned char * pbyte;
	unsigned char * puchar;
	char * pchar;
	unsigned int * puint;
	int * pint;
	unsigned short * pushort;
	short * pshort;
	unsigned long * pulong;
	long * plong;
	double * pdouble;
	float * pfloat;
} caster;

static int type_conversion (int derivedType, int fitsType, int repeat,
		void * pin, void * pout)
{
	int code = 0;

	int i;

	caster in;
	caster out;

	in.pvoid = pin;
	out.pvoid = pout;

	for (i = 0; !code && i < repeat; ++i)
		{
			double x = 0;

			switch (derivedType)
				{
					case PREFILTER_CHAR:
						x = in.pchar[i];
						break;

					case PREFILTER_SHORT:
						x = in.pshort[i];
						break;

					case PREFILTER_INT:
						x = in.pshort[i];
						break;

					case PREFILTER_LONG:
						x = in.plong[i];
						break;

					case PREFILTER_FLOAT:
						x = in.pfloat[i];
						break;

					case PREFILTER_DOUBLE:
						x = in.pdouble[i];
						break;

					default:
						code = PREFILTER_SETUP_ERROR;
						report_error("unexpected derived type %d\n", derivedType);
				}

			switch (fitsType)
				{
					case TBIT:
						out.pbyte[i] = x;
						break;

					case TBYTE:
						out.pbyte[i] = x;
						break;

					case TLOGICAL:
						out.pchar[i] = x ? 'T' : 'F';
						break;

					case TSTRING:
						code = PREFILTER_SETUP_ERROR;
						report_error("unable to convert numeric to FITS TSTRING\n");
						break;

					case TUSHORT:
						out.pushort[i] = x;
						break;

					case TSHORT:
						out.pshort[i] = x;
						break;

					case TUINT:
						out.puint[i] = x;
						break;

					case TINT:
						out.pint[i] = x;
						break;

					case TULONG:
						out.pulong[i] = x;
						break;

					case TLONG:
						out.plong[i] = x;
						break;

					case TDOUBLE:
						out.pdouble[i] = x;
						break;

					case TFLOAT:
						out.pfloat[i] = x;
						break;

					case TCOMPLEX: /* untested */
						out.pfloat[i] = x;
						break;

					case TDBLCOMPLEX: /* untested */
						out.pdouble[i] = x;
						break;

					default:
						code = PREFILTER_SETUP_ERROR;
						report_error("unexpected output type %d\n", fitsType);
				}
		}

	return code;
}


static int fits_type_size (int fitsType)
{
	int size = 0;

	switch (fitsType)
		{
			case TBIT:
				size = sizeof(char);
				break;

			case TBYTE:
				size = sizeof(char);
				break;

			case TLOGICAL:
				size = sizeof(char);
				break;

			case TSTRING:
				report_error("bad type FITS TSTRING\n");
				break;

			case TUSHORT:
				size = sizeof(unsigned short);
				break;

			case TSHORT:
				size = sizeof(short);
				break;

			case TUINT:
				size = sizeof(unsigned int);
				break;

			case TINT:
				size = sizeof(int);
				break;

			case TULONG:
				size = sizeof(unsigned long);
				break;

			case TLONG:
				size = sizeof(long);
				break;

			case TDOUBLE:
				size = sizeof(double);
				break;

			case TFLOAT:
				size = sizeof(float);
				break;

			case TCOMPLEX:
				size = 2 * sizeof(float);
				break;

			case TDBLCOMPLEX:
				size = 2 * sizeof(double);
				break;

			default:
				report_error("unexpected FITS type %d\n", fitsType);
		}

	return size;
}


static int output_parameters (Details * details, Derived * derived)
{
	++details->fitsRow;

	if (!details->code)
		{
			int status = 0;
			int i;
			char buffer[1024];
			char * space = &buffer[0];
			size_t bytes = sizeof(space);
			int heap = 0;

			for (i = 0; i < details->parameterCount; ++i)
				{
					Parameter * p = details->parameters[i];
					if (p->mode == PREFILTER_OUTPUT)
						{
							long repeat;
							void * pin;

							pin = ((char *) derived) + p->derivedOffset;
							repeat = (p->fitsArray > 1) ? p->fitsArray : 1;

							/* make sure space is large enough for the conversion */
							if (repeat * fits_type_size(p->fitsType) > bytes)
								{
									if (heap)
										free(space);
									bytes = repeat * fits_type_size(p->fitsType);
									space = malloc(bytes);
									if (space)
										heap = 1;
									else
										{
											details->code = PREFILTER_ERROR;
											report_error("unable to allocate %u bytes\n", bytes);
										}
								}

							if (type_conversion(p->derivedType, p->fitsType, repeat,
									pin, space))
								{
									details->code = PREFILTER_OUTPUT_ERROR;
									report_error("data conversion failed\n");
								}

							else if (p->fitsNull)
								{
									if (fits_write_colnull(details->fptr, p->fitsType,
														p->fitsColumn, details->fitsRow, 1, repeat,
														space, p->fitsNull->pvoid, &status))
										{
											details->code = PREFILTER_OUTPUT_ERROR;
											report_error("unable to write column %s data [%d]\n",
													p->fitsName, status);
										}
								}

							else if (fits_write_col(details->fptr, p->fitsType, p->fitsColumn,
											details->fitsRow, 1, repeat, space, &status))
								{
									details->code = PREFILTER_OUTPUT_ERROR;
									report_error("unable to write column %s data [%d]\n",
											p->fitsName, status);
								}
						}
				}

			if (heap)
				free(space);

			if (status)
					details->code = PREFILTER_SETUP_ERROR;
		}

	return details->code;
}


void destroy_parameter (Parameter * p)
{
	free((char *) p->name);
	free((char *) p->fitsName);

	if (p->dependencies)
		free(p->dependencies);

	p->name = 0;
	p->fitsName = 0;
	p->dependencies = 0;

	free(p);
}


void destroy_details (Details * details)
{
	int i;

	for (i = 0; i < details->parameterCount; ++i)
		destroy_parameter(details->parameters[i]);

	if (details->parameters)
		free(details->parameters);
	details->parameters = 0;
	details->parameterCount = 0;
	details->parameterSpace = 0;

	if (details->orderedParameters)
		free(details->orderedParameters);
	details->orderedParameters = 0;
	details->orderedCount = 0;

	if (details->fptr)
		{
			int status = 0;
			if (details->code)
				fits_delete_file(details->fptr, &status);
			else
				{
					HDpar_stamp(details->fptr, 0, &status);
					fits_write_chksum(details->fptr, &status);
					fits_close_file(details->fptr, &status);
				}
		}
}


Parameter * create_parameter (const char * name)
{
	int i, length;
	char * x;

	size_t bytes = sizeof(Parameter);
	Parameter * p = (Parameter *) calloc(1, bytes);

	length = strlen(name);

	p->name = x = malloc(length + 1);
	strcpy(x, name);

	p->fitsName = x = malloc(length + 1);
	strcpy(x, name);
	for (i = 0; i < length; ++i)
		if (islower(x[i]))
			 x[i] = toupper(x[i]);

	return p;
}


/* compare two strings without respect to case */
static int name_compare (const char * s1, const char * s2)
{
	int compare = 0;
	while (!compare && (*s1 || *s2))
		{
			int u1 = islower(*s1) ? toupper(*s1) : *s1;
			int u2 = islower(*s2) ? toupper(*s2) : *s2;
			compare = u1 - u2;
			++s1;
			++s2;
		}
	return compare;
}


Parameter * find_parameter (Details * details, const char * name)
{
	Parameter * found = 0;
	int i;

	for (i = 0; !found && i < details->parameterCount; ++i)
		{
			Parameter * p = details->parameters[i];
			if (!name_compare(name, p->name))
				found = p;
		}

	return found;
}


int define_parameter (Filter * filter, Parameter * parameter)
{
	Details * details = filter->details;

	if (find_parameter(details, parameter->name))
		{
			details->code = PREFILTER_SETUP_ERROR;
			report_error("duplicate parameter name [%s]\n",
					parameter->name);
		}

	/* check dependencies */
	if (!details->code && parameter->dependencies)
		{
			/* simply require every dependency to be already defined */
			Parameter ** q = parameter->dependencies;
			while (*q)
				{
					int i;
					int found = 0;
					for (i = 0; !found && i < details->parameterCount; ++i)
						if (details->parameters[i] == *q)
							found = 1;
					if (!found)
						{
							details->code = PREFILTER_SETUP_ERROR;
							report_error("parameter %s has invalid dependency %s\n"
									"[dependencies must be defined before use]\n",
									parameter->name, (*q)->name);
						}
					++q;
				}
		}

	/* ensure there is space for the parameter */
	if (!details->code && (details->parameterCount == details->parameterSpace))
		{
			size_t bytes;
			details->parameterSpace += 40;
			bytes = details->parameterSpace * sizeof(Derivation *);

			if (details->parameters)
				details->parameters = realloc(details->parameters, bytes);
			else
				details->parameters = malloc(bytes);

			if (!details->parameters)
				{
					details->code = PREFILTER_SETUP_ERROR;
					report_error("unable to allocate %d bytes", bytes);
				}
		}

	if (!details->code)
		details->parameters[details->parameterCount++] = parameter;

	return details->code;
}


/*
 * Create (and return ownership of) a null terminated Parameter * list
 */
Parameter ** create_dependency_list (int length)
{
	Parameter ** list = (Parameter **) calloc(length + 1, sizeof(Parameter *));
	list[length] = 0; /* really 0 */
	return list;
}


/*
 * Does p1 depend on p2? (possibly indirectly)
 * Parameter sorting is not a stunning example of efficiency, but the
 * parameter list only has to be sorted once per run and I do not anticipate
 * deep dependency chains.
 */
int parameter_requires (const Parameter * p1, const Parameter * p2)
{
	int depends = 0;
	Parameter * q;
	Parameter ** list = p1->dependencies;

	if (list)
		{
			while (!depends && (q = *list++))
				if (q == p2)
					depends = 1;
				else
					depends = parameter_requires(q, p2);
		}

	return depends;
}


int compare_parameters (const void * pv1, const void * pv2)
{
	int compare = 0;

	const Parameter * p1 = * (const Parameter **) pv1;
	const Parameter * p2 = * (const Parameter **) pv2;

	if (parameter_requires(p1, p2))
			compare = 1;

	if (!compare && parameter_requires(p2, p1))
			compare = -1;

	/* if not otherwise ordered, use order of definition */
	if (!compare)
		compare = p1->defined - p2->defined;

	return compare;
}


/*
 * Initializes output mode for each parameter and determines which
 * derivation functions need to be invoked and their order.
 *
 * If no output is specified [by argument_output(Arguments *)], all
 * parameters will be output.
 */

int set_parameter_modes (Details * details)
{
	int i;
	const char ** pp, ** output;

	output = details->filter->output->parameters;

	for (i = 0; i < details->parameterCount; ++i)
		{
			Parameter * p = details->parameters[i];
			p->mode = output ? PREFILTER_IGNORE : PREFILTER_OUTPUT;
			p->defined = i;
		}

	for (pp = output; pp && *pp; ++pp)
		{
			const char * name = *pp;
			Parameter * p = find_parameter(details, name);
			if (p)
				{
					Parameter * q;
					Parameter ** list = p->dependencies;

					p->mode = PREFILTER_OUTPUT;

					/* ensure dependencies are available */
					if (list)
						while ((q = *list++)) /* extra parens avoid bogus warning */
							if (q->mode == PREFILTER_IGNORE)
								q->mode = PREFILTER_TRANSIENT;
				}
			else
				{
					/* define_parameter is supposed to prevent this... */
					details->code = PREFILTER_SETUP_ERROR;
					report_error("invalid parameter reference [%s]\n", name);
				}
		}

	if (!details->code)
		{
			/* set up space for ordered parameters */
			size_t bytes = details->parameterCount * sizeof(Parameter *);

			details->orderedCount = 0;
			details->orderedParameters = (Parameter **) malloc(bytes);

			for (i = 0; i < details->parameterCount; ++i)
				{
					Parameter * p = details->parameters[i];
					if (p->mode != PREFILTER_IGNORE)
						{
							/* only call the derivation function once per iteration */
							int found = 0, j;
							for (j = 0; !found && j < details->orderedCount; ++j)
								if (details->orderedParameters[j]->derivation == p->derivation)
									found = 1;
							if (!found)
								details->orderedParameters[details->orderedCount++] = p;
						}
				}

			qsort(details->orderedParameters, details->orderedCount,
					sizeof(Parameter *), compare_parameters);
		}

	return details->code;
}


void test_name_compare_aux (const char * s1, const char * s2)
{
	int c = name_compare(s1, s2);
	printf("name_compare(%s, %s) = %d\n",
			s1, s2, c);
}


void test_name_compare ()
{
	test_name_compare_aux("cat", "dog");
	test_name_compare_aux("dog", "cat");
	test_name_compare_aux("dog", "dog1");
	test_name_compare_aux("dog", "DOG");
	test_name_compare_aux("doG", "DOG");
	test_name_compare_aux("DOG", "dOG");
	test_name_compare_aux("DOG", "cat");
}


void dump_parameter (const char * prefix, const Parameter * p)
{
	Parameter * q;
	Parameter ** list = p->dependencies;

	printf("%s%s [mode %d, defined %d]\n",
			prefix, p->name, p->mode, p->defined);
	printf("%sdependencies\n", prefix);

	if (list)
		while ((q = *list++)) /* extra parens avoid bogus warning */
				printf("\t%s%s\n", prefix, q->name);
}


void dump_details (const Details * details)
{
	int i;

	printf("details [code %d]\n", details->code);

	printf("%d of %d parameters\n",
			details->parameterCount, details->parameterSpace);
	for (i = 0; i < details->parameterCount; ++i)
			dump_parameter("\t", details->parameters[i]);

	printf("ordered parameters\n");
	for (i = 0; i < details->orderedCount; ++i)
			dump_parameter("\t", details->orderedParameters[i]);
}


int filter_code (const Filter * filter)
{
	int code = 0;
	if (filter->details)
		code = filter->details->code;
	return code;
}


int argument_code (const Filter * filter)
{
	return filter->argument_code(filter->arguments);
}


int context_code (const Filter * filter)
{
	return filter->context_code(filter->context);
}


int derived_code (const Filter * filter)
{
	return filter->derived_code(filter->derived);
}


static int filter_derivation (Details * details)
{
	int i;
	Filter * filter = details->filter;

	/* initialize derivations (zeroth iteration) */
	for (i = 0; i < details->orderedCount; ++i)
		{
			const Parameter * p = details->orderedParameters[i];
			if (p->initialization)
				(*p->initialization)(filter->arguments, filter->context);
		}

	/* iterate over space performing derivations */
	while (!details_code(details) && !context_code(filter))
		{
			filter->iterate(filter->arguments, filter->context);

			if (!context_code(filter))
				{
					for (i = 0; i < details->orderedCount; ++i)
						{
							const Parameter * p = details->orderedParameters[i];
							(*p->derivation)(filter->context, filter->derived);
						}

					if (derived_code(filter))
						details->code = PREFILTER_ERROR;
					else
						output_parameters(details, filter->derived);
				}
			else if (context_code(filter) == PREFILTER_SKIP)
				clear_context_code(filter->context);
		}

	if (!details->code && context_code(filter) != PREFILTER_DONE)
		details->code = PREFILTER_ERROR;

	return details->code;
}


int initialize_keyword (Keyword * pkeyword, int type, const char * name,
		const void * value, const char * comment)
{
	int code = 0;

	strncpy(pkeyword->name, name, FLEN_KEYWORD);
	pkeyword->type = type;

	if (comment)
		strncpy(pkeyword->comment, comment, FLEN_COMMENT);
	else
		pkeyword->comment[0] = 0;

	switch (type)
	{
		case TBYTE:
			pkeyword->value.bvalue = * (char *) value;
			pkeyword->pvoid = &pkeyword->value.bvalue;
			break;

		case TSHORT:
			pkeyword->value.svalue = * (short *) value;
			pkeyword->pvoid = &pkeyword->value.svalue;
			break;

		case TINT:
			pkeyword->value.ivalue = * (int *) value;
			pkeyword->pvoid = &pkeyword->value.ivalue;
			break;

		case TLONG:
			pkeyword->value.lvalue = * (long *) value;
			pkeyword->pvoid = &pkeyword->value.lvalue;
			break;

		case TFLOAT:
			pkeyword->value.fvalue = * (float *) value;
			pkeyword->pvoid = &pkeyword->value.fvalue;
			break;

		case TDOUBLE:
			pkeyword->value.dvalue = * (double *) value;
			pkeyword->pvoid = &pkeyword->value.dvalue;
			break;

		case TSTRING:
			strncpy(pkeyword->value.string, value, FLEN_VALUE);
			pkeyword->pvoid = pkeyword->value.string;
			break;

		default:
			{
				code = 2;
				report_warning("ignoring keyword %s with unexpected type %d\n",
						name, type);
			}
	}

	return code;
}


Keyword * create_keyword (int type, const char * name,
		const void * value, const char * comment)
{
	Keyword * keyword = (Keyword *) calloc(1, sizeof(Keyword));
	if (initialize_keyword(keyword, type, name, value, comment))
		{
			free(keyword);
			keyword = 0;
		}
	return keyword;
}


int add_keyword (KeywordBuffer * buffer, int type, const char * name,
		const void * value, const char * comment)
{
	int error = 0;
	Keyword * pkeyword = 0;

	if (buffer->used == buffer->allocated)
		{
#define KEYWORD_DELTA 64
			size_t bytes = (buffer->used + KEYWORD_DELTA) * sizeof(Keyword);

			if (buffer->heap)
				buffer->keywords = realloc(buffer->keywords, bytes);
			else
				buffer->keywords = malloc(bytes);

			if (!buffer->keywords)
				{
					error = 1;
					report_error("unable to allocate %d bytes\n", bytes);
				}
			else
				buffer->heap = 1;
		}

	pkeyword = buffer->keywords + buffer->used;

	error = initialize_keyword(pkeyword, type, name, value, comment);

	++buffer->used;

	return error;
}


void release_keyword_buffer (KeywordBuffer * buffer)
{
	if (buffer->heap)
		free(buffer->keywords);
}


int put_keyword_fits (fitsfile * fptr, Keyword * keyword, int * status)
{
	fits_write_key(fptr, keyword->type, keyword->name,
				keyword->pvoid, keyword->comment, status);
	return *status;
}


int put_keywords (Filter * filter, KeywordBuffer * buffer)
{
	Details * details = filter->details;
	fitsfile * fptr = details->fptr;

	int oldhdu = 0;

/* move to specified data unit */
	if (!details->code)
		{
			int status = 0;
			int type = 0;
			/* fits_get_hdu_num returns hdu instead of status */
			if (!fits_get_hdu_num(fptr, &oldhdu))
				status = SEEK_ERROR;

			fits_movabs_hdu(fptr, buffer->hdu, &type, &status);

			if (status)
				{
					details->code = PREFILTER_ERROR;
					report_error("unable to change to HDU %d\n", buffer->hdu);
				}
		}

	if (!details->code)
		{
			int i;
			int status = 0;
			for (i = 0; !details->code && i < buffer->used; ++i)
				{
					Keyword * keyword = &buffer->keywords[i];
					if (put_keyword_fits(fptr, keyword, &status))
						{
							details->code = PREFILTER_ERROR;
							report_error("unable to write key %s [%d]\n",
									keyword->name, status);
						}
				}
		}

	if (!details->code)
		{
			int status = 0;

			if (buffer->timestamp)
				fits_write_date(fptr, &status);

			if (buffer->checksums)
				fits_write_chksum(fptr, &status);

			if (status)
				{
					details->code = PREFILTER_ERROR;
					report_error("unable to update timestamp/checksums");
				}
		}

	/* return to previous HDU */
	if (oldhdu)
		{
			int status = 0;
			int type = 0;
			fits_movabs_hdu(fptr, oldhdu, &type, &status);
		}

	return details->code;
}


int prefilter_execute (Filter * filter)
{
	Details details = { 0 };

	details.filter = filter;
	filter->details = &details;

	/* initialization */
	if (!details.code)
		if (filter->initialization(filter->arguments, filter))
			details.code = PREFILTER_SETUP_ERROR;

	if (!details.code)
		set_parameter_modes(&details);

	if (!details.code)
		prepare_output(&details);

	if (0)
		dump_details(&details);

	/* derivation */
	if (!details.code)
		filter_derivation(&details);

	/* finalization */
	if (!details.code)
		if (filter->finalization(filter->arguments, filter))
			details.code = PREFILTER_ERROR;

	destroy_details(&details);

	return details.code;
}

