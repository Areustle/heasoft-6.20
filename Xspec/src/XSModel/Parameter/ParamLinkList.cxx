//   Read the documentation to learn more about C++ code generator
//   versioning.
//	  %X% %Q% %Z% %W%
#include <XSModel/GlobalContainer/ModelContainer.h>
#include <XSModel/Model/ModelBase.h>
#include <XSModel/Model/Component/Component.h>
#include <XSModel/Model/Component/ComponentGroup.h>
#include <XSModel/Parameter/ResponseParam.h>

// ParameterLink
#include <XSModel/Parameter/ParameterLink.h>
// Parameter
#include <XSModel/Parameter/Parameter.h>
// ParamLinkList
#include <XSModel/Parameter/ParamLinkList.h>



// Class ParamLinkList 
ParamLinkList* ParamLinkList::s_instance = 0;


ParamLinkList::ParamLinkList()
{
}


ParamLinkList::~ParamLinkList()
{
}


void ParamLinkList::unlink (Parameter* p)
{
  // remove the link from the list...
  m_linkList.erase(p);  
  //  delete the link object from the parameter, and nullify it.
  // set the link flag to false.
  p->unlink();         
}

ParamLinkList* ParamLinkList::Instance ()
{
  if (s_instance==0) {s_instance = new ParamLinkList; }
  return s_instance;






}

void ParamLinkList::setCompute (const Parameter* param, bool flag) const
{
  std::map<Parameter*,ParameterLink*>::const_iterator itLinks (m_linkList.begin());
  std::map<Parameter*,ParameterLink*>::const_iterator itEnd (m_linkList.end());

  std::vector<const Parameter*> leftPars;
  while (itLinks != itEnd)
  {
     if ( itLinks->second->findParam(param))
     {
        Parameter* leftPar = itLinks->first;
        leftPar->setCompute(flag);
        leftPars.push_back(leftPar);
     }
     ++itLinks;       
  }
  // Now handle links to links through recursive calls if necessary.
  for (size_t i=0; i<leftPars.size(); ++i)
  {
     setCompute(leftPars[i], flag);
  }
}

void ParamLinkList::removeDependentParameterLinks (Parameter* p)
{
  std::map<Parameter*,ParameterLink*>::iterator  il (m_linkList.begin());
  std::map<Parameter*,ParameterLink*>::iterator  ilEnd (m_linkList.end());
  while ( il != ilEnd )
  {
     const std::vector<const Parameter*>& linkMembers = il->second->members();
     size_t LN (linkMembers.size());
     bool linkRemoved = false;
     for ( size_t j = 0; j < LN && !linkRemoved; ++j)
     {
        if ( linkMembers[j] == p )
        {
           std::map<Parameter*,ParameterLink*>::iterator ilSave (il);
           ++ilSave; 
           // Parameter::doPreserve erases the entry pointed to by il so looks like
           // it invalidates the iterator;
           // it also calls Parameter::unlink() and sets its value to the 
           // current (linked) value.
           il->first->doPreserve();
           il = ilSave;
           linkRemoved = true;
        }    
     }
     if (!linkRemoved)
        ++il;
  }
}

void ParamLinkList::updateResponseParamLinkers (const ResponseParam* respPar) const
{
   // Arrive here after respPar's value has changed AND the resulting gain shift
   // has been applied to respPar's Response parent.  This can happen during fit
   // OR from user prompt in xsGain handler.  No need to call this recursively
   // on leftPar (see setCompute for example).  Through a circuitous route, 
   // the reevaluateLink function should end up back here with leftPar as the
   // parameter.

  std::map<Parameter*,ParameterLink*>::const_iterator itLinks (m_linkList.begin());
  std::map<Parameter*,ParameterLink*>::const_iterator itEnd (m_linkList.end());

  while (itLinks != itEnd)
  {
     if ( itLinks->second->findParam(respPar))
     {
        ResponseParam* leftPar = dynamic_cast<ResponseParam*>(itLinks->first);
        if (!leftPar)
           throw RedAlert("Invalid response parameter link: ParamLinkList::updateResponseParamLinkers");
        leftPar->reevaluateLink();
     }
     ++itLinks;       
  }
}

void ParamLinkList::checkLinksToZeroNorms(std::set<int>& parsWithZeroNorms) const
{
   if (parsWithZeroNorms.size())
   {
      std::map<Parameter*,ParameterLink*>::const_iterator itLinks = m_linkList.begin();
      std::map<Parameter*,ParameterLink*>::const_iterator itEnd = m_linkList.end();
      while (itLinks != itEnd && parsWithZeroNorms.size())
      {
         const Parameter* par = itLinks->first;
         // Careful, response params have no parent.
         if (par->parent() && !par->parent()->isZeroNorm())
         {
            const std::vector<const Parameter*>& toPars = itLinks->second->members();
            for (size_t i=0; i<toPars.size() && parsWithZeroNorms.size(); ++i)
            {
               const Parameter* toPar = toPars[i];
               recursiveCheckZeroLinks(toPar, parsWithZeroNorms);
            }
         }
         ++itLinks;
      }
   }
}

void ParamLinkList::recursiveCheckZeroLinks(const Parameter* par, std::set<int>& parsWithZeroNorms) const
{
   if (!par->isLinked())
   {
      // If we made it into here, par can't be a response param.
      if (par->parent()->isZeroNorm())
      {
         const ModelBase* modParent = par->parent()->parent()->parent();
         int fullIdx = (modParent->index() << XSContainer::ModelContainer::SHIFT()) + par->index();
         // par will only be in parsWithZeroNorms if it is
         //   a VARIABLE parameter.
         if (parsWithZeroNorms.find(fullIdx) != parsWithZeroNorms.end())
         {
            parsWithZeroNorms.erase(fullIdx);
         }
      }
   }
   else
   {
      const std::vector<const Parameter*>& toPars = par->thisLink()->members();
      for (size_t i=0; i<toPars.size() && parsWithZeroNorms.size(); ++i)
         recursiveCheckZeroLinks(toPars[i], parsWithZeroNorms);
   }
}

// Additional Declarations
