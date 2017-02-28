//   Read the documentation to learn more about C++ code generator
//   versioning.
//	  %X% %Q% %Z% %W%

#include <XSModel/Model/Component/Component.h>
// ParameterLink
#include <XSModel/Parameter/ParameterLink.h>
// sstream
#include <sstream>
// XSutility
#include <XSUtil/Utils/XSutility.h>
// Parameter
#include <XSModel/Parameter/Parameter.h>

#include <fstream>
#include <XSUtil/FunctionUtils/XSModelFunction.h>
#include <XSUtil/Parse/XSparse.h>
#include <XSstreams.h>


// Class Parameter::ResetFailure 

Parameter::ResetFailure::ResetFailure()
 : RedAlert(" resetting parameter from file")
{
}


// Class Parameter::InvalidInput 

Parameter::InvalidInput::InvalidInput (const string& name)
   : YellowAlert(" invalid parameter input: ")
{
   tcerr << name << '\n';
}


// Class Parameter::CantFreeze 

Parameter::CantFreeze::CantFreeze (const string& msg)
   : YellowAlert(" cannot freeze: ")
{
  tcerr << msg << '\n';
}


// Class Parameter 
ParamLinkList* Parameter::s_links = 0;
bool Parameter::s_onlyPrintFree = false;

Parameter::Parameter(const Parameter &right)
      : m_index(right.m_index), 
        m_name(right.m_name), 
        m_isLinked(right.m_isLinked), 
        m_changed(right.m_changed), 
        m_isModelDoomed(right.m_isModelDoomed),
        m_parent(right.m_parent),
        m_thisLink(0)  
{
  if (right.m_thisLink) m_thisLink = right.m_thisLink->clone();
}

Parameter::Parameter (Component* p)
      : m_index(0),
        m_name(""),
        m_isLinked(false),         
        m_changed(true),
        m_isModelDoomed(false),
        m_parent(p),
        m_thisLink(0)  
{
}


Parameter::~Parameter()
{
   // Let's not assume that because there's still at least one Parameter
   // object remaining (ie. this one), that ParamLinkList is still in
   // existence.  Some Parameters objects exist outside of Xspec's
   // Component objects, such as with model functions that call Xspec's
   // TableModel wrappers to perform interpolations.  That creates
   // Parameters which can exist until the program ends and ParamLinkList
   // is no more.  However those "rogue" Parameter objects can't have links.
   if (m_isLinked)
   {
      // Calls back on Parameter's unlink() function.
      // Cannot throw if ParameterLink's destructor cannot throw.
      ParamLinkList::Instance()->unlink(this); 
   }
}


std::istream& Parameter::get (std::istream& s) throw (YellowAlert&)
{
  string rawfromInput("");
  getline(s,rawfromInput);
  // strip leading blanks & tabs.
  int first = rawfromInput.find_first_not_of(" \t");
  string fromInput(""); 
  if (first >=0 ) fromInput =  rawfromInput.substr(first);
  // modify calls Parameter::Link and changeValue which are
  // designed to emit a message and leave the parameter in an unchanged
  // state. It sends a 'true' flag to indicate that its parent component
  // needs to be recomputed.
  try
  {
  	modify(fromInput);
  }
  catch ( YellowAlert& )
  {
	// modify will leave parameter values unchanged.
  }
  return s;
}

void Parameter::link (const string& toLink)
{
    try
    {
            const string OPS("+-*/=");
            string correctedLink(toLink);
            XSparse::separate(correctedLink,OPS);
            thisLink(new ParameterLink(correctedLink,this));
            links()->linkList(this,thisLink());
            m_isLinked = true;
            m_changed = true;
    }
    catch (YellowAlert&)
    {
            // comment from above block?? don't understand what I meant BD 11/6/00
            // allow the case where the ScaleParam is linked to a constant.
            // thus for a ScaleParam s,  newpar #s  a, and newpar #s = a 
            // are identical.
            throw ParameterLink::InvalidInput( " - ignored" );  
            // this changes the message and allows the error to be defused in modify           
    }
}

bool Parameter::compare (const Parameter& right) const
{
  if (m_name != right.m_name) return false;
  if (m_parent != right.m_parent) return false;
  return true;
}

void Parameter::modify (const string& input)
{
  // we're going to filter out XSPEC's /* 
  // character before we get here. This function
  // deals with blank lines and lines with potentially valid input.
  // This is analogous to the changeParamValue() function of Xspec11.
  try
  {
        switch  (input[0])
        {
                case '=': 
                        thaw(); // a linked parameter cannot be frozen.
                        link(input.substr(1));  
                        break;
                case '/':
                        // do nothing.
                        break;
                default:
                        changeValue(input);
                        break;
        }
  }        
  catch (ParameterLink::InvalidInput) 
  { 

        // stop here with no action being taken to the parameter.
        // because the ParameterLink constructor throws the exception
        // no link will have been created.   
  }
}

void Parameter::freeze ()
{
  string msg (" Parameter: ");
  if (parent()) 
  {
        msg += parent()->name();
        msg += ':';
  }
  msg += name();
  msg += " is a constant or switch ";
  throw CantFreeze(msg);
}

void Parameter::reset () throw (Parameter::ResetFailure)
{

  using namespace std;
  try
  {
     static const string PARAMETERTYPEINDICATORS("*$");
     // Table models and mdef components will throw here.
     string sourceFile(parent()->modfileSource());
     string line;

     if (sourceFile.empty())
     {
        // This could be a Python-coded local model from PyXspec.
        //   It will have no model.dat file, instead check parInfoCache.
        ParInfoContainer::const_iterator itParInfo =
               XSModelFunction::parInfoCache().find(parent()->name());
        if (itParInfo != XSModelFunction::parInfoCache().end())
        {
           const std::vector<string>& parInfoStrings = itParInfo->second;
           string nameTest;
           int k = 0;
           int n = (int)parInfoStrings.size();
           while ( k < n )
           {
              line = parInfoStrings[k];
              istringstream s(line);
              s >> nameTest;
              if (nameTest.length()>1 && PARAMETERTYPEINDICATORS.find(nameTest[0]) != string::npos)
                 nameTest = nameTest.substr(1);
              if ( nameTest == m_name ) break;
              ++k;
           }     
           if ( k == n) throw Parameter::ResetFailure();
        }
        else
        {
          // No model.dat, not a table model, not a Python model or mdef.  What is this?
          throw Parameter::ResetFailure();
        }        
     }
     else
     {
        ifstream modfile(sourceFile.c_str());
        if (modfile.fail()) throw Parameter::ResetFailure();

        modfile.seekg(parent()->modfileLocation());

        getline(modfile,line);
     
        string nameTest;
        int k = 0;
        int n = parent()->numParams();
        while ( k < n )
        {     
              getline(modfile,line);
              istringstream s(line);
              s >> nameTest;
              if (nameTest.length()>1 && PARAMETERTYPEINDICATORS.find(nameTest[0]) != string::npos)
                 nameTest = nameTest.substr(1);
              if ( nameTest == m_name ) break;
              k++;   
        } 
        if ( k == n) throw Parameter::ResetFailure();
        modfile.close();
     }
     init(line);
     if (!isFrozen()) thaw();
  }
  catch (XSModelFunction::NoSuchComponent&)
  {
     // This catch block is a (temporary?) fix to deal with the 
     // resetting of table models.  The assumption is that only for
     // table models can a NoSuchComponent be thrown down below.  
     // Every other component in use must already have been found
     // in a .dat file.  At the moment, simply do nothing to the
     // current table param setting.
  }
}

void Parameter::unlink ()
{
  // delete link object and nullify it. 
  delete m_thisLink;
  m_thisLink = 0;
  // Tell the parameter it's not linked.
  m_isLinked = false;
}

void Parameter::initializeLinks ()
{
  links(ParamLinkList::Instance());
}

size_t Parameter::dataGroup () const
{
  if (m_parent) 
  {
          return m_parent->dataGroup();
  }
  else return ((size_t)1 << 31);
}

void Parameter::untie (bool preserve)
{
  if (preserve) 
  {
        doPreserve();       
  }
  else
  {
        if (isLinked())
        {
                // set linked flag to false and remove from 
                // parameter link structures.
                links()->unlink(this);
                // re-read parameter value from model.dat file (default
                // implementation of reset() or whatever it takes to 
                // reinitialized the parameter.
                reset();       
        }  
  }
}

void Parameter::reindex (int offset)
{
  m_index += offset;
}

Parameter* Parameter::lookupCoParameter (size_t i) const
{
  if (m_parent) return m_parent->getLocalParameter(i);
  else return 0;  // should never be called in this instance but
                  // the test makes this one harmless.

  return m_parent->getLocalParameter(i);
}

bool Parameter::isFrozen () const
{

  return true;
}

void Parameter::setComputeForLinks (bool flag) const
{
  s_links->setCompute(this,flag);
}

void Parameter::setCompute (bool flag) const
{
  // An isolated param such as a TableModParam simply used during
  // an interpolation outside the context of xspec models, will
  // not have a parent component.
  if (m_parent)
     m_parent->recompute(flag);
}

string Parameter::linkString () const
{
  if (isLinked())
  {
        std::ostringstream s("");
          m_thisLink->putLink(s);
          return s.str();
  }
  else return string("");
}

void Parameter::setParent (Component* p)
{
  if (!m_parent) m_parent = p;   
}

const string& Parameter::modelName () const
{
  return m_parent->parentName();
}

void Parameter::findPersistentLink (Parameter* par, std::set<Parameter*>& parsToProcess, std::map<Parameter*,Parameter*>& processedPars)
{
   // This is a recursive function.
   std::pair<Parameter*,Parameter*> newRoute(par, 0);
   if (par->isLinked())
   {
      // Complexity compromise: only bother trying to do a re-route
      // for the simplest link expressions, ie. only 1 link parameter
      // with no scaling.
      ParameterLink* link = par->thisLink();
      if (link->members().size() == 1 && link->linkString().tokenList().size() == 1)
      {
         Parameter* newPar = const_cast<Parameter*>(link->members()[0]);
         if (newPar->isModelDoomed())
         {
            // All doomed pars must either be waiting in parsToProcess, or
            // already handled and placed in processedPars.
            std::map<Parameter*,Parameter*>::iterator itPar =
                        processedPars.find(newPar);
            if (itPar != processedPars.end())
               newRoute.second = itPar->second;
            else
            {
               findPersistentLink(newPar, parsToProcess, processedPars);
               newRoute.second = processedPars.find(newPar)->second;
            }
         }
         else
         {
            newRoute.second = newPar;
         }
      }
   }
   parsToProcess.erase(parsToProcess.find(par));
   processedPars.insert(newRoute);
}

bool Parameter::isLinkedToFrozen (const Parameter* par)
{
   // A recursive function.  Determines whether or not every parameter
   // in a link statement is frozen.  Note that if parameter itself is not
   // linked, it simply returns the isFrozen state.
   bool isFrozen = true;
   if (par->isLinked())
   {
      const std::vector<const Parameter*>& toPars = par->m_thisLink->members();
      for (size_t i=0; i<toPars.size(); ++i)
      {
         if (!Parameter::isLinkedToFrozen(toPars[i]))
         {
            isFrozen = false;
            break;
         }
      }
   }
   else
   {
      isFrozen = par->isFrozen();
   }
   return isFrozen;
}

// Additional Declarations
      std::ostream& operator << (std::ostream& s, const Parameter& right)
      {
         if (!Parameter::onlyPrintFree() || (!right.isFrozen() && !right.isLinked()))
         {
            right.put(s);
            s << std::endl;
         }
         return s;
      }

      std::istream& operator >> (std::istream& s, Parameter& right)
      {
         right.get(s);
         return s;
      }        
