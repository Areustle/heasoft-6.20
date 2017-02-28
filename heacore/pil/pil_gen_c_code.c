/*
 * pil_gen_c_code.c, ver 0.1 27-Sep.2000 (JB)
 *
 * this program generates C code which reads all parameters from
 * given parameter file (so you don't have to write this code
 * yourself)
 *
 * note: Vector and VarVector functions are not supported.
 *
*/


#include <stdio.h>
/* SCREW 1145: Use string.h instead of strings.h. */
#include <string.h>
#include <pil.h>


int	main(int argc, char **argv)
{ int		r, i, parcnt, minmaxflag;
  PIL_VALUE	vmin, vmax;
  PIL_PARAM	pp;
  char		*my_prefix;

if ((3 != argc) && (2 != argc))
  { printf("usage:\n\tpil_gen_c_code parameter_filename [variables_prefix]\n\n");
    return(10);
  }

if (3 == argc)  { my_prefix = argv[2]; }
else  { my_prefix = "myprefix_"; }

PILSpecialMode |= PIL_NO_POSITIONAL | PIL_BYPASS_NAMING | PIL_OPEN_RDONLY;

strncpy(PILModuleName, argv[1], PIL_LINESIZE - 1);
PILModuleName[PIL_LINESIZE - 1] = 0;

if (PIL_OK != (r = PILInit(argc, argv)))
  { printf("PILInit failed : %s\n", PIL_err_handler(r));
    return(10);
  } 

if (PIL_OK != (r = PILGetNumParameters(&parcnt)))
  { printf("PILGetNumParameters failed : %s\n", PIL_err_handler(r));
    return(11);
  } 

printf("/*********** generated automatically by pil_gen_c_code ************/\n");
printf("\n");
printf("/* you may need to uncomment the following 2 lines :\n");
printf("\n");
printf("/* #include <stdio.h> */\n");
printf("/* #include <pil.h> */\n");
printf("\n");

printf("\n");
printf("/*********** VARIABLES DECLARATIONS: this code goes into *.h ************/\n");
printf("\n");
for (i=0; i<parcnt; i++)
 { if (PIL_OK != (r = PILGetParameter(i, &pp, &minmaxflag, &vmin, &vmax)))
     { printf("PILGetParameter failed : %s\n", PIL_err_handler(r));
       return(12);
     }
   if (PIL_FORMAT_OK != pp.format) continue;
   if (0 == strcmp(pp.strname, "mode")) continue;
   if (0 == strcmp(pp.strname, "pil_ext_indir")) continue;
   if (0 == strcmp(pp.strname, "ServerMode")) continue;

   switch (pp.type)
    {
      case PIL_TYPE_BOOL:
		printf("extern\tint\t%s%s;\n", my_prefix, pp.strname);
		break;
      case PIL_TYPE_INT4:
		printf("extern\tint\t%s%s;\n", my_prefix, pp.strname);
		break;
      case PIL_TYPE_REAL4:
		printf("extern\tfloat\t%s%s;\n", my_prefix, pp.strname);
		break;
      case PIL_TYPE_REAL8:
		printf("extern\tdouble\t%s%s;\n", my_prefix, pp.strname);
		break;
      case PIL_TYPE_STRING:
		printf("extern\tchar\t%s%s[PIL_LINESIZE];\n", my_prefix, pp.strname);
		break;
      case PIL_TYPE_FNAME:
		printf("extern\tchar\t%s%s[PIL_LINESIZE];\n", my_prefix, pp.strname);
		break;
    }
 }
printf("\n");

printf("\n");
printf("/*********** VARIABLES DEFINITIONS: this code goes into *.c ************/\n");
printf("\n");
for (i=0; i<parcnt; i++)
 { if (PIL_OK != (r = PILGetParameter(i, &pp, &minmaxflag, &vmin, &vmax)))
     { printf("PILGetParameter failed : %s\n", PIL_err_handler(r));
       return(12);
     }
   if (PIL_FORMAT_OK != pp.format) continue;
   if (0 == strcmp(pp.strname, "mode")) continue;
   if (0 == strcmp(pp.strname, "pil_ext_indir")) continue;
   if (0 == strcmp(pp.strname, "ServerMode")) continue;

   switch (pp.type)
    {
      case PIL_TYPE_BOOL:
		printf("int\t%s%s = 0;\n", my_prefix, pp.strname);
		break;
      case PIL_TYPE_INT4:
		printf("int\t%s%s = 0;\n", my_prefix, pp.strname);
		break;
      case PIL_TYPE_REAL4:
		printf("float\t%s%s = 0.0;\n", my_prefix, pp.strname);
		break;
      case PIL_TYPE_REAL8:
		printf("double\t%s%s = 0.0;\n", my_prefix, pp.strname);
		break;
      case PIL_TYPE_STRING:
		printf("char\t%s%s[PIL_LINESIZE];\n", my_prefix, pp.strname);
		break;
      case PIL_TYPE_FNAME:
		printf("char\t%s%s[PIL_LINESIZE];\n", my_prefix, pp.strname);
		break;
    }
 }
printf("\n");

printf("\n");
printf("/*********** FUNCTION DEFINITION: this code goes into *.c ************/\n");
printf("\n");
printf("int\t%sread_all_pars(void)\n", my_prefix);
printf(" { int\tr;\n");
printf("\n");
for (i=0; i<parcnt; i++)
 { if (PIL_OK != (r = PILGetParameter(i, &pp, &minmaxflag, &vmin, &vmax)))
     { printf("PILGetParameter failed : %s\n", PIL_err_handler(r));
       return(12);
     }
   if (PIL_FORMAT_OK != pp.format) continue;
   if (0 == strcmp(pp.strname, "mode")) continue;
   if (0 == strcmp(pp.strname, "pil_ext_indir")) continue;
   if (0 == strcmp(pp.strname, "ServerMode")) continue;

   switch (pp.type)
    {
      case PIL_TYPE_BOOL:
		printf("   if (PIL_OK != (r = PILGetBool(\"%s\", &%s%s))) return(r);\n", pp.strname, my_prefix, pp.strname);
		break;
      case PIL_TYPE_INT4:
		printf("   if (PIL_OK != (r = PILGetInt(\"%s\", &%s%s))) return(r);\n", pp.strname, my_prefix, pp.strname);
		break;
      case PIL_TYPE_REAL4:
		printf("   if (PIL_OK != (r = PILGetReal4(\"%s\", &%s%s))) return(r);\n", pp.strname, my_prefix, pp.strname);
		break;
      case PIL_TYPE_REAL8:
		printf("   if (PIL_OK != (r = PILGetReal(\"%s\", &%s%s))) return(r);\n", pp.strname, my_prefix, pp.strname);
		break;
      case PIL_TYPE_STRING:
		printf("   if (PIL_OK != (r = PILGetString(\"%s\", &(%s%s[0])))) return(r);\n", pp.strname, my_prefix, pp.strname);
		break;
      case PIL_TYPE_FNAME:
		printf("   if (PIL_OK != (r = PILGetFname(\"%s\", &(%s%s[0])))) return(r);\n", pp.strname, my_prefix, pp.strname);
		break;
    }
 }
printf("   return(PIL_OK);\n");
printf(" }\n");
printf("\n");

PILClose(PIL_OK);
return(PIL_OK);
}
