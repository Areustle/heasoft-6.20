//   Read the documentation to learn more about C++ code generator
//   versioning.
//	  %X% %Q% %Z% %W%

// Help
#include <XSUser/Help/Help.h>
#include <XSUser/Help/HelpComposite.h>
#include <XSUser/Global/XSGlobal.h>
#include <XSUser/Global/Global.h>
#include <XSUser/UserInterface/xstcl.h>
#include <XSUtil/Parse/XSparse.h>
#include <XSUtil/Utils/XSutility.h>
#include <locale>
#include <sys/types.h>
#include <unistd.h>
#include <fstream>
#include <cstring>


// Class Help 
const string Help::PREFIX = "XS";
const std::string Help::PDF_EXT = ".pdf";
const std::string Help::HTML_EXT = ".html";
std::string Help::s_pdfCommand = std::string("acroread  -openInNewWindow -tempFileTitle");
std::string Help::s_htmlCommand = std::string("mozilla");

Help* Help::s_pHelpTree = 0;

Help::Help(const Help &right)
  : m_isComposite(right.m_isComposite),
    m_isOnline(right.m_isOnline),
    m_docType(right.m_docType),
    m_strFile(right.m_strFile),
    m_strRelativePath(right.m_strRelativePath)
{
}

Help::Help (const string& fileName, const string& path, Help::DocTypes docType, bool composite, bool online)
  : m_isComposite(composite),
    m_isOnline(online),
    m_docType(docType),
    m_strFile(fileName),
    m_strRelativePath(path)
{
   const string::size_type pathSize = m_strRelativePath.size();
   if(!pathSize || m_strRelativePath[pathSize-1] != '/') 
        m_strRelativePath.push_back('/');
}


Help::~Help()
{
}


Help & Help::operator=(const Help &right)
{
    return *this;
}


void Help::initHelpTree ()
{

    using namespace std;
    static const StringArray dirs = parseUserHelpDir();

    //oh no, memory allocation!
    int nSize = 1024, nDirNum = dirs.size();
    XSutility::auto_array_ptr<char> pFileName (new char[nSize]);
    char* fileName (pFileName.get());

    XSGlobal::GlobalData* global = XSGlobal::GlobalData::Instance();
    if (global->useOnlineHelp())
    {
       processOnlineFiles();
    }

    for(int j = 0; j < nDirNum; ++j) 
    {
       FILE* cmdPipe = popen((string("ls -1 ") + dirs[j]).c_str(), "r");

       while(fgets(fileName, nSize, cmdPipe) != 0) {
	   //strips off trailing newline
	   string strFileName(fileName, strlen(fileName) - 1);
	   nameToObjects(strFileName, dirs[j], false);
       }
       pclose(cmdPipe);
    }
}

void Help::execute (const StringArray& strParams, int index) const
{
    const string launchScript("/browserLaunch.tcl");
    XSGlobal::GlobalData* global = XSGlobal::GlobalData::Instance();
    string scriptLoc(global->scriptDir());
    scriptLoc += launchScript;
    string fileLoc(m_strRelativePath);
    fileLoc += m_strFile;
    if (m_docType == HTML)
    {
       Tcl_SetVar(interp, "browser", s_htmlCommand.c_str(), 0);
       Tcl_SetVar(interp, "fileLoc", fileLoc.c_str(), 0);
       if (m_isOnline)
       {
          Tcl_SetVar(interp, "online", "1", 0);         
       }
       else
       {
          Tcl_SetVar(interp, "online", "0", 0);         
       }
       Tcl_EvalFile(interp, scriptLoc.c_str());
    }
    else
    {
       // Assume PDF also means offline.
       string windowName (fileToCommand(m_strFile));
       // all very nice but won't really work unless s_pdfCommand supports it.
       // so we must hardcode s_pdfCommands FTTB.
       const string acroreadTitleFlag("tempFileTitle");
       string windowTitle(" ");
       string::size_type loc = s_pdfCommand.rfind(acroreadTitleFlag);
       if (loc != string::npos && loc == (s_pdfCommand.length() - 
                   acroreadTitleFlag.length()))
       {
          windowTitle = " \"XSPEC Help: ";
          windowTitle += windowName;
          windowTitle += "\" ";
       }
       string strCommand = s_pdfCommand + windowTitle + m_strRelativePath  + m_strFile + "&";
       system(strCommand.c_str());
    }
}

void Help::copy ()
{
}

void Help::erase () throw ()
{
}

const StringArray Help::parseUserHelpDir ()
{
    using namespace XSGlobal;

    //unfortunately, we don't know the size...
    StringArray arrDirs;
    GlobalData* global = GlobalData::Instance();

    if (!global->useOnlineHelp())
       arrDirs.push_back(s_pHelpTree->m_strRelativePath);

    arrDirs.push_back(global->scriptDir());
    if (global->userScriptDir().length()) 
    {
        arrDirs.push_back(global->userScriptDir());
    }


    if (global->defaultLocalModelDirectory().length()) 
    {
        arrDirs.push_back(global->defaultLocalModelDirectory());
    }


    const std::map<string, string>& settings = global->settings();
    std::map<string, string>::const_iterator i_Settings = settings.find("USER_HELP_DIRECTORIES");

    if(i_Settings != settings.end()) {
	const string strUserHelpDirs = i_Settings->second;

	size_t nPos = 0, nLast = 0, nDelim = 0;

	do {
	    nDelim = strUserHelpDirs.find(':', nLast);
	    nPos = nDelim != string::npos ? nDelim : strUserHelpDirs.size();

	    arrDirs.push_back(strUserHelpDirs.substr(nLast, nPos - nLast));

	    nLast = nDelim + 1;
	} while(nDelim != string::npos);
    }
    return arrDirs;
}

void Help::helpTree (Help* tree)
{
  s_pHelpTree = tree;
}

string Help::fileToCommand (const string& fileName) const
{
  string tmp (fileName);
  string cmd ("");

  // get rid of prefix and suffix.
  if ( fileName.find(PREFIX) != string::npos )
  {
        tmp = fileName.substr(PREFIX.length());
  }
  tmp = tmp.substr(0,tmp.find("."));
  int  N (tmp.length());

  int e (0);
  int b (0);
  while ( e < N )
  {
        if ( std::isupper(tmp[e]) || e == N - 1)
        {
                if (e == N - 1) 
                {
                        cmd += tmp.substr(b,e-b+1);       
                }
                else
                {
                        cmd += tmp.substr(b,e-b);
                        cmd += " ";
                        tmp[e] = tolower(tmp[e]);       
                        b = e;
                }
        }
        ++e;       
  }    
  return cmd;
}

void Help::nameToObjects (const string& fileName, const string& dir, bool online)
{
  if (fileName.substr(0, PREFIX.length()) == PREFIX) 
  {
      // Find type and location of extension.
      string::size_type extLoc = fileName.find('.');
      if (extLoc == string::npos)
         return;
      string SUFFIX = fileName.substr(extLoc, fileName.length() - extLoc);
      DocTypes fileType;
      if (SUFFIX == HTML_EXT)
         fileType = HTML;
      else if (SUFFIX == PDF_EXT)
         fileType = PDF;
      else
         return;

      Help *i_pHelpTree = s_pHelpTree;
      HelpComposite* t_Parent = static_cast<HelpComposite*>(i_pHelpTree);

      string strParsed, strParent;

      int nLength = static_cast<int>(extLoc);

      int nPos = PREFIX.length();
      int nLast;

      //iterates through the filename finding 
      //each word beginning with a capital letter
      for(int i = nPos; i <= nLength; ++i) {
	  //must be careful not to try and dereference fileName[nLength]
	  if(i == nLength || (fileName[i] >= 'A' && fileName[i] <= 'Z')) {
	      nLast = nPos;
	      nPos = i;

	      if(nPos != nLast) {
		  //retrieves first word beginning with a capital letter
		  strParsed = fileName.substr(nLast, nPos - nLast);
		  strParsed[0] = (char)(tolower(strParsed[0]));

		  HelpComposite* t_pciComposite;
		  Help* t_pComponent = 0;

		  //pre: i_pHelpTree != NULL
		  //post: t_pciComposite != NULL ::= i_pHelpTree is either cast or copied
		  if(i_pHelpTree->isComposite())
		      t_pciComposite = static_cast<HelpComposite*>(i_pHelpTree);
		  else {
		      //not a composite, so create a new composite
		      t_pciComposite = new HelpComposite(*i_pHelpTree);
		      t_Parent->addOrUpdateComponent(strParent, t_pciComposite);
		  }

		  t_Parent = t_pciComposite;
		  strParent = strParsed;

		  Help* found(0);
		  t_pComponent = (t_pciComposite->mapParams(strParsed, found, true) ? found : 0);

		  if(!t_pComponent) {
		      t_pComponent = new Help(fileName.substr(0, nPos) + SUFFIX, dir, fileType, false, online);
		      t_pciComposite->addOrUpdateComponent(strParsed, t_pComponent);
		  }

		  i_pHelpTree = t_pComponent;
	      }
	  }
      }
  } // end if start w/ PREFIX
}

void Help::processOnlineFiles ()
{
    using namespace std;
    XSGlobal::GlobalData* global = XSGlobal::GlobalData::Instance();
    const string fileList = global->managerDir() + "/manualList.txt";
    ifstream ifs(fileList.c_str());
    if (!ifs)
    {
       string msg("Unable to open file ");
       msg += fileList + "\n";
       throw YellowAlert(msg);
    }
    // 1st line should contain url
    string webURL;
    getline(ifs, webURL);
    s_pHelpTree->m_strRelativePath = webURL;
    const string WS(" \t");
    while (!ifs.eof())
    {
       string line;
       getline(ifs, line);
       const string manualFile(XSparse::trimWhiteSpace(line));
       if (manualFile.length())
          nameToObjects(manualFile, webURL, true); 
    }
}

// Additional Declarations
