//   Read the documentation to learn more about C++ code generator
//   versioning.
//	  %X% %Q% %Z% %W%

// MdefContainer
#include <XSModel/GlobalContainer/MdefContainer.h>
#include <XSstreams.h>
#include <XSUtil/Error/Error.h>
#include <sstream>
#include <iomanip>
#include <memory>

namespace XSContainer {

    // Class XSContainer::MdefContainer 
    const string MdefContainer::s_NOT_FOUND = string("hN#@xnNOT_FOUNDp!749w9q");

    MdefContainer::MdefContainer()
      : m_components()
    {
    }


    MdefContainer::~MdefContainer()
    {
         CompMap::iterator itComp = m_components.begin();
         CompMap::iterator itCompEnd = m_components.end();
         while (itComp != itCompEnd)
         {
            delete itComp->second.second;
            ++itComp;
         }
    }


    void MdefContainer::displayComponents () const
    {
       using namespace std; 

       if (m_components.empty())
          tcout << "No user-defined models" << endl;
       else
       {
          tcout << "-- Name ---- Type ----- Expresssion -----" << endl;
          ios_base::fmtflags saveFmt(tcout.flags());
          CompMap::const_iterator itComp = m_components.begin();
          CompMap::const_iterator itCompEnd = m_components.end();
          while (itComp != itCompEnd)
          {
             const string& type = itComp->second.first;           
             const string& name = itComp->first;
             const MathExpression* expr = itComp->second.second->generator();
             tcout << left << setw(12) << name << setw(5) << type
                   << expr->exprString() << endl;
             ++itComp;
          }
          tcout.flags(saveFmt);
       }
    }

    string MdefContainer::removeComponent (const string& name)
    {
       string compType(s_NOT_FOUND);
       CompMap::iterator itComp = m_components.find(name);
       if (itComp != m_components.end())
       {
          compType = itComp->second.first;
          delete itComp->second.second;
          m_components.erase(itComp);
       }
       return compType;
    }

    bool MdefContainer::addComponent (const string& name, const string& type, const string& expression, std::pair<Real,Real> eLimits)
    {
       // Don't let in a name the same as not_found indicator.  It's extremely
       // unlikely anyone could match the not_found name accidentally.
       if (name == s_NOT_FOUND)
       {
          string err(name);
          err += " is an internally reserved name and is unavailable for mdefine.\n";
          throw YellowAlert(name); 
       }

       // Don't step on any pre-existing component.  Instead return
       // false and if calling function decides it's OK, it can call
       // removeComponent before calling this again.

       // Assume type has already been validated.
       CompMap::iterator itExisting = m_components.find(name);
       if (itExisting != m_components.end())
          return false;

       std::auto_ptr<MathExpression> expr(new MathExpression(eLimits,type));
       // This may throw
       expr->init(expression);
       XSCall<MathExpression>* callExpr = new XSCall<MathExpression>(expr.get());
       expr.release();
       m_components[name] = std::make_pair(type,callExpr);
       return true;
    }

    void MdefContainer::displayComponent (const string& name) const
    {
       using namespace std; 

       CompMap::const_iterator itComp = m_components.find(name);
       if (itComp != m_components.end())
       {
          ios_base::fmtflags saveFmt(tcout.flags());
          const string& type = itComp->second.first;           
          const string& name = itComp->first;
          const MathExpression* expr = itComp->second.second->generator();
          tcout << left << setw(12) << name << setw(5) << type
                << expr->exprString() << endl;
          tcout.flags(saveFmt);
       }
       else
       {
          tcout << "\"" << name << "\" is not a user defined model" << endl;
       }
    }

    MdefContainer::CompPair MdefContainer::getComponent (const string& name) const
    {
       CompPair result(s_NOT_FOUND,static_cast<XSCall<MathExpression>*>(0));
       // Require an exact name match, partially name not good enough.  Assume
       // case has already been lowered.
       CompMap::const_iterator itFound = m_components.find(name);
       if (itFound != m_components.end())
       {
          result = itFound->second;
       }
       return result;
    }

    // Additional Declarations

} // namespace XSContainer
