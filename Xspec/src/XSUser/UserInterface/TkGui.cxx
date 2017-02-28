//   Read the documentation to learn more about C++ code generator
//   versioning.
//	  %X% %Q% %Z% %W%

// TkGui
#include <XSUser/UserInterface/TkGui.h>


// Class TkGUI 
TkGUI* TkGUI::s_instance = 0;

TkGUI::TkGUI (char* console, char* text, char* plot) throw (TclInitErr)
      : m_plotPath(plot),
        m_textPath(text),
        m_conPath(console)
{

    int status = Tcl_LinkVar(interp,plot,(char *) &m_plotPath,TCL_LINK_STRING);
    if (status != TCL_OK) throw TclInitErr("Tk Error: invalid Tcl plot window name.");
    status = Tcl_LinkVar(interp,text,(char *) &m_textPath,TCL_LINK_STRING);
    if (status != TCL_OK) throw TclInitErr("Tk Error: invalid Tcl text window name.");
    status = Tcl_LinkVar(interp,console,(char *) &m_conPath,TCL_LINK_STRING);
    if (status != TCL_OK) throw TclInitErr("Tk Error: invalid Tcl console window name.");


}


TkGUI::~TkGUI()
{
}


TkGUI* TkGUI::Instance (const string& console, const string& text, const string& plot)
{
  if (s_instance == 0)
  {
        s_instance = new TkGUI(const_cast<char*>(console.c_str()),
                                           const_cast<char*>(text.c_str()),
                                           const_cast<char*>(plot.c_str())  );
  } 

  return s_instance;
}

// Additional Declarations
//   Tk_Window tkwin  = Tk_CreateWindowFromPath(interp,Tk_MainWindow(interp),
//          const_cast<char*>(windowName.c_str()),NULL);
//  setWindows(windowName,tkwin);
