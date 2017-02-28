/*
 * itkStaticApplication.r --
 *
 *	This file creates resources which bind in the static version of the
 *  pkgIndex tclIndex and itk's Tcl code files.
 *
 * Jim Ingham for Itcl 2.2
 * 
 * Copyright (c) 1996 Lucent Technologies
 *
 * See the file "license.terms" for information on usage and redistribution
 * of this file, and for a DISCLAIMER OF ALL WARRANTIES.
 *
 * SCCS: @(#) itkStaticApplication.r 1.5 96/10/03 17:54:21
 */

#include <Types.r>
#include <SysTypes.r>

#define TK_LIBRARY_RESOURCES   3000
#define POW_LIBRARY_RESOURCES  4000
#define FV_LIBRARY_RESOURCES   4500


/* 
 * We now load the Tk library into the resource fork of the application.
 */

read 'TEXT' (TK_LIBRARY_RESOURCES+1, "tk", purgeable) 
	"::tk:library:tk.tcl";
read 'TEXT' (TK_LIBRARY_RESOURCES+2, "button", purgeable) 
	"::tk:library:button.tcl";
read 'TEXT' (TK_LIBRARY_RESOURCES+3, "dialog", purgeable) 
	"::tk:library:dialog.tcl";
read 'TEXT' (TK_LIBRARY_RESOURCES+4, "entry", purgeable) 
	"::tk:library:entry.tcl";
read 'TEXT' (TK_LIBRARY_RESOURCES+5, "focus", purgeable) 
	"::tk:library:focus.tcl";
read 'TEXT' (TK_LIBRARY_RESOURCES+6, "listbox", purgeable) 
	"::tk:library:listbox.tcl";
read 'TEXT' (TK_LIBRARY_RESOURCES+7, "menu", purgeable) 
	"::tk:library:menu.tcl";
read 'TEXT' (TK_LIBRARY_RESOURCES+8, "optionMenu", purgeable) 
	"::tk:library:optMenu.tcl";
read 'TEXT' (TK_LIBRARY_RESOURCES+9, "palette", purgeable) 
	"::tk:library:palette.tcl";
read 'TEXT' (TK_LIBRARY_RESOURCES+10, "scale", purgeable) 
	"::tk:library:scale.tcl";
read 'TEXT' (TK_LIBRARY_RESOURCES+11, "scrollbar", purgeable) 
	"::tk:library:scrlbar.tcl";
read 'TEXT' (TK_LIBRARY_RESOURCES+12, "tearoff", purgeable) 
	"::tk:library:tearoff.tcl";
read 'TEXT' (TK_LIBRARY_RESOURCES+13, "text", purgeable) 
	"::tk:library:text.tcl";
read 'TEXT' (TK_LIBRARY_RESOURCES+14, "tkerror", purgeable) 
	"::tk:library:bgerror.tcl";
read 'TEXT' (TK_LIBRARY_RESOURCES+15, "Console", purgeable) 
	"::tk:library:console.tcl";
read 'TEXT' (TK_LIBRARY_RESOURCES+16, "msgbox", purgeable, preload) 
	"::tk:library:msgbox.tcl";
read 'TEXT' (TK_LIBRARY_RESOURCES+17, "comdlg", purgeable, preload) 
	"::tk:library:comdlg.tcl";


/* 
 * We now load the pow library into the resource fork of the application.
 */

/*  #include "powMacResource.r"          Lets put into a folder instead */


/* 
 * We now load the fv library into the resource fork of the application.
 */

/*  #include "fvMacResource.r"           Lets put into a folder instead */


/* 
 * We now put the initialization script into resource fork of the application.
 */

data 'TEXT' (9999,"itkwishrc",purgeable, preload) {
	"# Tcl init file\n"
	"package ifneeded Itcl 3.2 {source -rsrc itcl}\n"
	"package ifneeded Itk 3.2 {source -rsrc itk}\n"
	"package ifneeded Iwidgets 3.0.0 {source -rsrc iwidgets}\n"
        "package ifneeded http 2.3 {source -rsrc Http}\n"
	"set env(FITSVIEWER_LIBRARY) \"$env(HOME)fv Sources\"\n"
	"source $env(FITSVIEWER_LIBRARY):fvInit.tcl\n"
};
