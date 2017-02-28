//C++
#include "TclRegEx.h"
#include <XSstreams.h>
#include <cstring>
#include <iostream>

const string TclRegEx::s_REAL = string("-?(\\.\\d+|\\d+(\\.\\d*)?)(?!\\d*\\.)(?:[eE](?:\\+|-)?\\d*)?");

TclRegEx::TclRegEx(const string& exp) : m_strExp(exp) {
    m_interp = Tcl_CreateInterp();
    createExpFromType(exp);
}

bool TclRegEx::createExpFromType(const string& exp) {
    bool success = true;

    Tcl_Obj* stringObj = Tcl_NewStringObj(exp.c_str(), exp.size());
    Tcl_IncrRefCount(stringObj);
    if(!exp.empty()) {
	m_regexp = Tcl_GetRegExpFromObj(m_interp, 
					stringObj,
					TCL_REG_ADVANCED);

	if(strcmp(m_interp->result, ""))
	    throw BadExpression(m_interp->result, -1);
    }
    else
	success = false;

    Tcl_DecrRefCount(stringObj);
    return success;
}

bool TclRegEx::regex_search(const string& s, result_type& matches, int flags)
{
    return regex_search(s.begin(), s.end(), matches, flags);
}

bool TclRegEx::regex_search(string::const_iterator beg, string::const_iterator end, 
			    result_type& matches, int flags)
//bool TclRegEx::regex_search(const string& s, int offset, result_type& matches, int flags) 
{
    matches.clear();

    string search(beg, end);

    Tcl_Interp* error = Tcl_CreateInterp();

    Tcl_Obj* objString = Tcl_NewStringObj(search.c_str(), search.size());
    Tcl_IncrRefCount(objString);

    // Returns 1 if objString contains a match of m_regexp, 0 if no match,
    // and -1 if an error occurs.  The 4th arg is an offset into objString,
    // The 5th indicates the number of subexpressions to remember, where
    // -1 tells it to remember all.  
    int success = Tcl_RegExpExecObj(error, m_regexp, 
				    objString, 
				    0, -1, flags);
    Tcl_DecrRefCount(objString);

    if(success < 0)
	tcout << "Error: " << error->result << std::endl;
    else {
       // The Tcl_RegExpInfo struct has 3 members: 
       //   nsubs - number of parenthesized subexpressions in the reg ex.
       //   matches - points to an array of nsubs values containing the bounds
       //             of each subexpression.  Bounds are stored in
       //             Tcl_RegExpIndices struct containing "start" and "end".
       //             If subexp is not part of match, bounds are set to -1.
       //             The first elem in matches is for the ENTIRE matched range.
       //  extendStart - a long value unused here, is only set when TCL_REG_CANMATCH
       //              flag is sent.
	Tcl_RegExpInfo info;
	Tcl_RegExpGetInfo(m_regexp, &info);

	int resultSize = info.nsubs + 1;

	std::vector<sub_match> match_info;

	match_info.reserve(resultSize);

	sub_match sub;

	for(int i = 0; i < resultSize; ++i) {
	    sub.matched = success && 
		(info.matches[i].start != info.matches[i].end);

	    sub.first = sub.second = end;

	    if(sub.matched) 
	    {
		sub.first  = beg + info.matches[i].start;
		sub.second = beg + info.matches[i].end;
	    }

	    match_info.push_back(sub);
	}

	matches.subs(match_info);
    }
    Tcl_DeleteInterp(error);

    return success;
}

string TclRegEx::regSub(const string& inString, const string& substString)
{
   char cOutBuff[] = "cOutBuff";
   string modified;

   Tcl_SetVar(m_interp, cOutBuff, "",0);
   string cmdStr = "regsub -all ";
   cmdStr += "{" + m_strExp + "}";
   cmdStr += " \"" + inString + "\" ";
   if (substString.length())
   {
      cmdStr += substString;
   }
   else
   {
      cmdStr += "\"\"";
   }
   cmdStr += " cOutBuff";
   char* c_cmdStr = new char[cmdStr.length() + 1];
   strcpy(c_cmdStr, cmdStr.c_str());
   Tcl_Eval(m_interp, c_cmdStr);
   delete [] c_cmdStr;
   const char* result = Tcl_GetVar(m_interp, cOutBuff, 0);
   if (result)
   {
      modified = string(result);
   }
   Tcl_UnsetVar(m_interp, cOutBuff, 0);
   Tcl_ResetResult(m_interp);
   return modified;
}


TclRegEx::~TclRegEx()
{
    Tcl_DeleteInterp(m_interp);
}
