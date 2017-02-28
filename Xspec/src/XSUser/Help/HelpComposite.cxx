//   Read the documentation to learn more about C++ code generator
//   versioning.
//	  %X% %Q% %Z% %W%
#include "XSstreams.h"

// HelpComposite
#include <XSUser/Help/HelpComposite.h>
#include <iomanip>
#include <ios>
#include <iostream>


// Class HelpComposite 

HelpComposite::HelpComposite(const HelpComposite &right)
  : Help(right),
    m_mapParams(right.m_mapParams)
{
}

HelpComposite::HelpComposite (const string& cmd, const string& path, Help::DocTypes docType, bool online)
  : Help(cmd, path, docType, true, online)              
{
}

HelpComposite::HelpComposite (const Help& right)
  : Help(right)
{
    isComposite(true);
}


HelpComposite::~HelpComposite()
{
  std::map<string,Help*>::iterator itHelp = m_mapParams.begin();
  std::map<string,Help*>::iterator itEnd = m_mapParams.end();
  while (itHelp != itEnd)
  {
     delete itHelp->second;
     ++itHelp;
  }
}


HelpComposite & HelpComposite::operator=(const HelpComposite &right)
{
    return *this;
}


void HelpComposite::addOrUpdateComponent (const string& cmd, Help* help)
{
    std::map<string,Help*>::iterator p = m_mapParams.find(cmd);
    if ( p != m_mapParams.end())
    {
        delete p->second;
        m_mapParams.erase(p);
    }
    m_mapParams.insert(std::map<string,Help*>::value_type(cmd,help));
}

void HelpComposite::execute (const StringArray& strParams, int index) const
{
    using namespace std;

    int size = strParams.size();

    if(index == size - 1)
    {
	Help::execute(strParams, index);
    }
    else 
    {
	Help* match;

	if(mapParams(strParams[++index], match) != 0)
        {
	    match->execute(strParams, index);
        }
	else 
        { 
            if ( index >= 1 && strParams[index][0] != '?')
            {
	        tcout << "\nInvalid help option."; 
            }
            tcout << " Valid options are:\n";
	    int count = 0;

	    std::map<string, Help*>::const_iterator 
		i_mapParamsBegin = m_mapParams.begin(),
		i_mapParamsEnd   = m_mapParams.end();

	    while(i_mapParamsBegin != i_mapParamsEnd) 
            {
		if(!(count % 3)) tcout << std::endl;

		tcout << setiosflags(ios::left) << setw(20) << i_mapParamsBegin->first.c_str();

		++count;
		++i_mapParamsBegin;
	    }
	    tcout << std::endl << std::endl;
            tcout << "  To get help for a particular model component, enter \n\"help model <comp name>\"."
                << " (To view list of model components, \nenter \"model\".)" 
                << std::endl << std::endl; 
	}
    }        
}

bool HelpComposite::mapParams (const string& cmd, Help*& match, bool exactOnly) const
{
    std::map<string, Help*>::const_iterator itr = m_mapParams.lower_bound(cmd);
    std::map<string, Help*>::const_iterator itrEnd = m_mapParams.end();

    if  (itr != itrEnd)     
    {
       if (exactOnly)
       {
          if (itr->first == cmd)
          {
             match = itr->second;
             return true;
          }
       }
       else   // allow abbreviated commands
       {
          if (itr->first.find(cmd) == 0) 
          {
	      match = itr->second;
	      return true;
          }
       }
    }
    return false;
}

// Additional Declarations
