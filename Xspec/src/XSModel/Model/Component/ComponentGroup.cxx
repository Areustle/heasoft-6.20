//   Read the documentation to learn more about C++ code generator
//   versioning.
//	  %X% %Q% %Z% %W%

#include <XSModel/GlobalContainer/ModelContainer.h>
#include <XSModel/Model/ModelBase.h>
#include <XSModel/Model/UniqueEnergy.h>
#include <XSModel/Model/Component/AddComponent.h>
#include <XSModel/Model/Component/AddTableComponent.h>
#include <XSModel/Model/Component/Component.h>
#include <XSModel/Model/Component/ComponentCreator.h>
#include <XSModel/Model/Component/MulComponent.h>
#include <XSModel/Model/Component/MulTableComponent.h>
#include <XSModel/Model/Component/SumComponent.h>
#include <XSModel/Model/Component/ComponentGroup.h>
#include <XSModel/Parameter/ModParam.h>
#include <XSModel/Parameter/Parameter.h>
#include <XSUtil/Utils/XSutility.h>
#include <XSUtil/Parse/ModelExpression.h>
#include <XSUtil/Parse/ModelExprContexts.h>
#include <XSUtil/Parse/XSModExpTree.h>
#include <XSstreams.h>
#include <XSsymbol.h>
#include <stack>
#include <memory>
#include <cmath>

int ComponentGroup::s_count = 0;

// Class ComponentGroup::NoSuchComponent 

ComponentGroup::NoSuchComponent::NoSuchComponent (const string& msg)
{
}


// Class ComponentGroup 
const string ComponentGroup::s_GROUPTEST = "$GROUP";

ComponentGroup::ComponentGroup(const ComponentGroup &right)
  : m_index(right.m_index), 
    m_nested(right.m_nested),
    m_componentInfo(right.m_componentInfo),// non-owning. 
    m_groupFlux(right.m_groupFlux->clone(this)),  // owning
    m_parent(right.m_parent),
    m_elements(),
    m_individualSources()
{
   ComponentListConstIterator ic = right.m_elements.begin();
   ComponentListConstIterator icEnd = right.m_elements.end();
   while ( ic != icEnd)
   {
         m_elements.push_back((*ic)->clone(this));   
         ++ic;    
   }     
   ++s_count;
}

ComponentGroup::ComponentGroup (ModelExpression<ModExprTreeMember>& gs, ModelBase* p, bool nested)
  : m_index(++s_count), 
    m_nested(nested),
    m_componentInfo(&gs),// non-owning.
    m_groupFlux(new SumComponent(this)),  // owning
    m_parent(p),  
    m_elements(),
    m_individualSources()
{
}


ComponentGroup::~ComponentGroup()
{
  delete m_groupFlux;

  ComponentListIterator it = m_elements.begin();
  ComponentListIterator itEnd = m_elements.end();
  while (it != itEnd)
  {
      //SumComponent's are owned by their ComponentGroup - don't delete them here
      if(!dynamic_cast<SumComponent*>(*it))
	  delete *it;

      ++it;
  }
  --s_count;
}


ComponentGroup & ComponentGroup::operator=(const ComponentGroup &right)
{
  ComponentGroup __temp(right);
  swap(__temp);
  return *this;
}

const string& ComponentGroup::parentName () const
{
  return m_parent->name();
}


void ComponentGroup::calculate (bool saveComponentFlux, bool frozen)
{
  ComponentListIterator end=m_elements.end();
  ComponentListIterator beg=m_elements.begin();

  // scheme: perform the model calculations if needed.
  for (ComponentListIterator it = beg ;  it!=end; ++it)
  {
     // recompute component if parameters have changed.
     // the parameter changed attribute sets the recompute flag
     // to true if one or more of the parameters have changed.
     if ((*it)->recompute() | (*it)->isGroup() | frozen ) 
     {
        (*it)->calculate(saveComponentFlux,frozen);  
        // most of the time, we've calculated the model and don't need
        // to change it again until the parameter is changed. In the
        // case of 'frozen' used in computing norms, we will need to recompute
        // the entire model.
     }
 }
}

const ArrayContainer& ComponentGroup::energyArray () const
{
  return m_parent->energy();
}

void ComponentGroup::setElement (XSModExpTree<ComponentGroup*>& componentGroups)
{
   int gt=0;
   int N (m_componentInfo->compSpecs().size());
   for ( int j = 0; j < N; ++j)
   {
      ComponentCreator create;
      const string& componentName = m_componentInfo->getComponentString(j);
      gt = componentName.find(ComponentGroup::GROUPTEST());
   // if this is not a symbol placeholder for a nested group...
   //  create a component of the appropriate type and read it.
      if ( gt < 0  )
      {                
         m_elements.push_back(create.GetComponent(componentName,this));
         incrementModelComponentCount();
         Component* newComponent = m_elements.back();
	 newComponent->index(m_componentInfo->getComponentSequence(j));
         updateExpression(newComponent, componentName, j);
      }
      else
   // if it is, store the address of the SumComponent that's in the
   // location nn given by  $GROUPnn.
      {
         int w = 0;
         string groupNo = componentName.substr(componentName.find_first_not_of(ComponentGroup::GROUPTEST()));
         sscanf (groupNo.c_str(),"%d",&w);
         ComponentGroup* cg = componentGroups.values()[w];
         SumComponent* gf = cg->groupFlux();

         gf->name(componentName);
         m_elements.push_back(gf);
      }
   }
}

size_t ComponentGroup::dataGroup () const
{
  return m_parent->dataGroupNumber();
}

bool ComponentGroup::operator > (const ComponentGroup& right) const
{
  return !operator>(right);
}

ComponentGroup* ComponentGroup::clone () const
{
  return  new ComponentGroup(*this);
}

bool ComponentGroup::operator < (const ComponentGroup& right) const
{
  bool result (false);
  if ( m_componentInfo->group() > right.m_componentInfo->group() ) 
  {
          return true;
  }
  else 
  {
        if ( m_componentInfo->group() == right.m_componentInfo->group() )
        {
                if (m_componentInfo->location() 
                        > right.m_componentInfo->location()) result =  true;
        }
  }
  return result;
}

const Component* ComponentGroup::getElements (const string& componentName) const
{
  ComponentListConstIterator comp(m_elements.begin());
  ComponentListConstIterator compEnd(m_elements.end());
  while ( comp != compEnd )
  {
        if  (  (*comp)->name() == componentName ) break;

        ++comp;       
  }
  if ( comp != compEnd ) return *comp;
  // fix this.
  else throw Component::NoSuchComponent(componentName);
}

void ComponentGroup::setElements (const string& componentName, const Component* newElement)
{
  ComponentListIterator rs(m_elements.begin());
  ComponentListIterator rsEnd(m_elements.end());

  while ( rs != rsEnd )
  {
        if  (  (*rs)->name() == componentName ) 
        {
                rs = m_elements.erase(rs);
                m_elements.insert(rs,newElement->clone(this));
                break;
        }
        ++rs;       
  }
}

void ComponentGroup::resetComponentFlux () const
{
  ComponentListConstIterator end=m_elements.end();
  ComponentListConstIterator beg=m_elements.begin();

  for (ComponentListConstIterator it = beg ;  it!=end; ++it)
  {
     (*it)->restoreUniquePhotonArray();
  }
}

void ComponentGroup::swap (ComponentGroup& right) throw ()
{
  std::swap(m_index,right.m_index);
  std::swap(m_nested,right.m_nested);
  std::swap(m_componentInfo,right.m_componentInfo);
  std::swap(m_groupFlux,right.m_groupFlux);
  std::swap(m_parent,right.m_parent);
  std::swap(m_elements,right.m_elements);
  std::swap(m_individualSources,right.m_individualSources);
}

void ComponentGroup::registerParameters () const
{
  ComponentListConstIterator rE(m_elements.begin());
  ComponentListConstIterator rEnd(m_elements.end());

  while (rE != rEnd)
  {
        (*rE)->registerParameters();
        ++rE;       
  }
}

size_t ComponentGroup::parameterIndexBase () const
{
  return m_parent->parameterIndexBase();
}

size_t ComponentGroup::incrementModelParameterCount ()
{
  return m_parent->incrementModelParameterCount();
}

size_t ComponentGroup::incrementModelComponentCount ()
{
  return m_parent->incrementModelComponentCount();
}

size_t ComponentGroup::modelIndex () const
{
  return m_parent->index();
}

Parameter* ComponentGroup::getLocalParameter (size_t i) const
{
  return m_parent->getLocalParameter(i);
}

Parameter* ComponentGroup::localParameter (size_t i) const
{
  ComponentListConstIterator rc(m_elements.begin());      
  ComponentListConstIterator rcEnd(m_elements.end());      
  Parameter* result(0);     
  while (rc != rcEnd)
  {
        if ( (result = (*rc)->localParameter(i)) != 0 ) 
           break;
        ++rc;
  }
  return  result;
}

const SumComponent* ComponentGroup::sum () const
{
  return m_groupFlux;
}

// Method to push indices of all parameters which are not norms onto a
// integer array.

void ComponentGroup::allButNorms (IntegerArray& paramsToFreeze) const
{

  ComponentListConstIterator cEnd = m_elements.end();
  for (ComponentListConstIterator c = m_elements.begin(); c != cEnd; ++c) {                        
    std::vector<ModParam*> vp = (*c)->getVariableParameters();
    size_t N = vp.size();
    for ( size_t j = 0; j < N; ++j) {
      if ( vp[j]->name() != "norm" ) {
	paramsToFreeze.push_back((m_parent->index() << 16) + vp[j]->index());
      }
    }
  }

}

void ComponentGroup::setComputeFlag (bool value)
{
  ComponentListConstIterator end=m_elements.end();
  ComponentListConstIterator beg=m_elements.begin();

  // scheme: perform the model calculations if needed.
  // recompute component if parameters have changed.
  // the parameter changed attribute sets the recompute flag
  // to true if one or more of the parameters have changed.
  // on initialization this might switch computing off altogether.
  // during a fit one probably needs manual control over this flag
  // as well. In principle also value could be false to use only
  // the last computed model.
  for (ComponentListConstIterator it = beg ;  it!=end; ++it)  
     (*it)->recompute(value);
}

std::list<ModParam*> ComponentGroup::normParams () const
{
  std::list<ModParam*> norms;
  ComponentListConstIterator cEnd = m_elements.end();
  for (ComponentListConstIterator c = m_elements.begin(); c != cEnd; ++c)
  {
     // ignore the SumComponents that represent groups.
     if ( !(*c)->isGroup())
     {
         AddComponent* a = dynamic_cast<AddComponent*> (*c);
         if (a)
         {
            // every AddComponent has exactly one norm
            // parameter of type ModParam, added on construction.
            ModParam* p = static_cast<ModParam*>(a->parameter("norm"));
            norms.push_back(p);
         }
     }            
  }       

  return norms;
}

void ComponentGroup::clearArrays (const std::set<UniqueEnergy*>& currentEngs)
{
  m_groupFlux->clearArrays(currentEngs);

  ComponentListIterator cEnd = m_elements.end();
  for (ComponentListIterator c = m_elements.begin(); c != cEnd; ++c)
  {                        
     if ( !(*c)->isGroup()) 
        (*c)->clearArrays(currentEngs);
  }       
}

std::vector<ModParam*> ComponentGroup::getVariableParameters () const
{
  ComponentListConstIterator cl = m_elements.begin();
  ComponentListConstIterator clEnd = m_elements.end();

  std::vector<ModParam*> vp;
  while (cl != clEnd)
  {
        std::vector<ModParam*> cvp = (*cl)->getVariableParameters();
        std::copy(cvp.begin(),cvp.end(),back_inserter(vp));
        ++cl;       
  }
  return vp;
}

void ComponentGroup::saveComponentFlux (bool setSaveFlux) const
{
  ComponentListConstIterator end=m_elements.end();
  ComponentListConstIterator beg=m_elements.begin();

  for (ComponentListConstIterator it = beg ;  it!=end; ++it)
  {
        (*it)->saveUniquePhotonArray(setSaveFlux);
  }
}

void ComponentGroup::deleteComponent (int sequenceNumber, int index)
{
  ComponentListIterator doomed(m_elements.begin());
  for (int ic = 0; ic < sequenceNumber; ++ic)
     ++doomed;
  delete *doomed;
  m_elements.erase(doomed);
  m_componentInfo->deleteWordBySequence(index);
}

void ComponentGroup::setNestedElement (const string& componentName, Component* newElement)
{
  ComponentListIterator rs(m_elements.begin());
  ComponentListIterator rsEnd(m_elements.end());

  // nested element does not reset pointer to current component group.
  while ( rs != rsEnd )
  {
     if  (  (*rs)->name() == componentName ) 
     {
        // delete current pointer and replace with nested version.
        // setNestedElement is called from ModelBase::clone which has
        // just created ptrs like rs.
        delete *rs;
        rs = m_elements.erase(rs);
        m_elements.insert(rs,newElement);
        break;
     }
     ++rs;       
  }  
}

void ComponentGroup::resetElement (XSModExpTree<ComponentGroup*>& newGroups, const std::vector<Component*>& existingComponents)
{
   const size_t nComps = m_componentInfo->compSpecs().size();
   for (size_t i=0; i<nComps; ++i)
   {
      const string& componentName = m_componentInfo->getComponentString(i);
      if (componentName.find(ComponentGroup::GROUPTEST()) ==
            string::npos)
      {
         // This gets the word sequence, which corresponds to component 
         // index number.
         int newIndex = m_componentInfo->getComponentSequence(i);
         Component* compToTransfer = existingComponents[newIndex-1];
         compToTransfer->index(newIndex);
         compToTransfer->setParent(this);
         m_elements.push_back(compToTransfer);
      }
      else
      {
         // Store the address of the SumComponent that's in
         // the location nn given by  $GROUPnn.
	 int w = 0;
	 string groupNo = componentName.substr(componentName.find_first_not_of(ComponentGroup::GROUPTEST()));
	 sscanf (groupNo.c_str(),"%d",&w);
         ComponentGroup* cg = newGroups.values()[w];
	 SumComponent* gf = cg->groupFlux();

	 gf->name(componentName);
	 m_elements.push_back(gf);
      }
   }
}

Component* ComponentGroup::componentByNumber (int index) const
{
  Component* target(0);
  ComponentListConstIterator cl = m_elements.begin();
  ComponentListConstIterator clEnd = m_elements.end();
  while ( cl != clEnd && !target )
  {
     if ( (*cl)->index() == index )
        target = *cl;
     ++cl;       
  }

  return target;
}

void ComponentGroup::insertComponent (int location, int index, const string& name, char op)
{
    ComponentListIterator ic (m_elements.begin());
    ComponentCreator create;
    Component::getSetupData(name);
    string fullName(Component::currentModelName());
    Component* newComponent (create.GetComponent(fullName,this));
    newComponent->index(index);
    m_componentInfo->insertWordBySequence(index,fullName,op);

    addToElements(location, newComponent);

    ic = m_elements.begin();
    ComponentListIterator icEnd = m_elements.end();
    // This call is only needed in case of table models, particularly
    // if a re-prompt for file name was necessary.
    updateExpression(newComponent, fullName, location);
    incrementModelComponentCount();
}

void ComponentGroup::exchangeComponent (int location, Component* newComponent)
{
   // Introduced for use by editmod exchange:
   // Simply replace an existing component pointer with a new one
   // created from the input name.  This will DESTROY the old
   // component, so it ASSUMES all parameter deregistering has
   // been done already.
   Component* oldComponent = 0;
   int count = 0;
   ComponentListIterator itComp = m_elements.begin();
   ComponentListIterator itEnd = m_elements.end();
   if (static_cast<size_t>(location) < m_elements.size())
   {
      while (count != location && itComp != itEnd)
      {      
         ++itComp;
         ++count;
      }
      if (itComp != itEnd)
         oldComponent = *itComp;
   }
   if (!oldComponent)
   {
      std::ostringstream oss;
      oss << "Error searching for component at location " << location
         << " in ComponentGroup::exchangeComponent.\n";
      throw RedAlert(oss.str());
   }
   *itComp = newComponent;
   newComponent->index(oldComponent->index());
   delete oldComponent;
}

Component* ComponentGroup::createComponent (string& name)
{
   // Introduced for use in editmod, to ensure new component can be created 
   // before modifying anything in its ultimately intended model.  This
   // function can throw.
   ComponentCreator create;
   Component::getSetupData(name);
   string fullName(Component::currentModelName());
   Component* newComponent (create.GetComponent(fullName,this));
   // file name may have changed due to re-prompting while in
   // create.GetComponent.
   string tableModName = checkCurrentTableModelExpr(newComponent, name);
   if (tableModName.length())
      name = tableModName;
   return newComponent;
}

void ComponentGroup::deregisterParameters () const
{
  ComponentListConstIterator rE(m_elements.begin());
  ComponentListConstIterator rEnd(m_elements.end());

  while (rE != rEnd)
  {
     (*rE)->deregisterParameters();
     ++rE;       
  }
}

Component* ComponentGroup::firstComponent ()
{
  if ( !m_elements.empty() ) 
     return m_elements.front();
  else return 0;
}

void ComponentGroup::resizeElements (size_t n)
{
   m_elements.resize(n);
}

void ComponentGroup::addToElements (int componentOffset, Component* newComponent)
{
    ComponentListIterator ic = m_elements.begin();

    if ( componentOffset < static_cast<int>(m_elements.size()))
    {
        for (int j = 0; j < componentOffset; ++j)
	    ++ic;

        ic = m_elements.insert(ic, newComponent);
    }
    else // no indexes to update.
	m_elements.push_back(newComponent);
    newComponent->setParent(this);
}

void ComponentGroup::updateExpression (Component* newComponent, const string& oldName, size_t wordIdx)
{
   // If this is a table model, it's possible the file name has 
   // changed as a result of re-prompting.  Therefore we need
   // to re-insert this possibly new name back into the expression.
   string tableName(checkCurrentTableModelExpr(newComponent, oldName));
   if (tableName.length())
      m_componentInfo->setComponentString(tableName, wordIdx);
   else
      m_componentInfo->setComponentString(newComponent->name(),wordIdx);
}


std::vector<size_t> ComponentGroup::postfixOperatorLocs (size_t endIdx, size_t nComps, size_t skipIdx, size_t nSkip) const
{
   // ASSUME nComps from right to left starting with endIdx contain no 
   // non-skipped "+" operations.  Will search (from right-to-left) for
   // 3 symbol types: '(', ')', and components.  '*'s will NOT be searched
   // for (since they may or may not exist in tandem with parentheses),
   // but their positions can be inferred.
   std::vector<size_t> postfixStarLocs;
   
   string::size_type compLoc = 
      static_cast<string::size_type>(m_componentInfo->getComponentLocation(endIdx));
   string exprStr = m_componentInfo->exprString();
   if (nSkip)
   {
      // This is a final summation.  Additive parens and everything 
      // inside them will be replaced by a single sum component.
      string::size_type lPar = m_componentInfo->parenLocs().first;
      string::size_type rPar = m_componentInfo->parenLocs().second;
      exprStr.replace(lPar, rPar-lPar+1, rPar-lPar+1, ' ');
   }
   std::map<string::size_type, char> symbolLocs;
   symbolLocs[compLoc] = 'c';
   std::stack<char> opHolder;
   // Note that this will miss any rParens to the right of the endIdx component.
   // That's OK since we can just ASSUME parentheses are balanced within each
   // subgroup.
   string::size_type rParenLoc = exprStr.rfind(')',compLoc);
   string::size_type lParenLoc = exprStr.rfind('(',compLoc);
   if (rParenLoc != string::npos)
      symbolLocs[rParenLoc] = ')';
   if (lParenLoc != string::npos)
      symbolLocs[lParenLoc] = '(';
   char prevSymb = 0;
   size_t compsProcessed=0;
   size_t curIdx = endIdx+1;
   while (compsProcessed != nComps)
   {
      std::map<string::size_type,char>::reverse_iterator itCur = symbolLocs.rbegin();
      // First time in loop, this has to be a component.
      char curSymb =itCur->second;
      string::size_type nextLoc=string::npos;
      if (curSymb == ')')
      {
         if (prevSymb == 'c' || prevSymb == '(')
         {
            opHolder.push('*');
         }
         opHolder.push(')');
         nextLoc = exprStr.rfind(')',itCur->first-1);
      }
      else if (curSymb == '(')
      {
         // Don't just assume matching ')' is in stack.  If it was 
         // to the right of the endIdx component, it won't be.
         if (opHolder.size())
            opHolder.pop();
         // Was there a '*' associated with this bracket?  If so,
         // now is the time to place it.
         if (opHolder.size() && opHolder.top() == '*')
         {
            postfixStarLocs.push_back(curIdx);
            opHolder.pop();
         }
         nextLoc = exprStr.rfind('(', itCur->first-1);
      }
      else // Assume component
      {
         --curIdx;
         if (prevSymb == 'c' || prevSymb == '(')
         {
            postfixStarLocs.push_back(curIdx);
         }
         
         if (nSkip && curIdx == skipIdx)
         {
            compsProcessed += nSkip;
            // Now slide curIdx to the last (leftmost) of the skipped components.
            curIdx -= (nSkip-1);
         }
         else
         {
            ++compsProcessed;
         }
         
         if (compsProcessed != nComps)
         {
            nextLoc = static_cast<string::size_type>(m_componentInfo->getComponentLocation(curIdx-1));
         }
      }
      symbolLocs.erase(itCur->first);
      if (nextLoc != string::npos)
      {
         symbolLocs[nextLoc] = curSymb;
      }
      prevSymb = curSymb;
   } // end comps loop
   
   // opHolder stack may still contain '*'s due to '('s remaining to the
   // left of the first component.
   while (opHolder.size())
   {
      if (opHolder.top() == '*')
         postfixStarLocs.push_back(curIdx);
      opHolder.pop();
   } 
   
   // debug
//   for (size_t i=0; i<postfixStarLocs.size(); ++i)
//      tcerr <<postfixStarLocs[i]<<" ";
//   tcerr <<std::endl;
         
   return postfixStarLocs;
}

// Additional Declarations
std::ostream & operator<<(std::ostream& s, const ComponentGroup& right)
{
  ComponentListConstIterator p = right.elements().begin();
  while (p != right.elements().end())
  {
	int w = (*p)->name().find("$GROUP");
	if ( w < 0 ) s << **p;
        ++p;
  }  
  return s;      
}

void ComponentGroup::setComponentInfo(ModelExpression<ModExprTreeMember>* expr)
{
        m_componentInfo = expr;       
}

const std::set<UniqueEnergy*>& ComponentGroup::uniqueEnergyArray () const
{
   return m_parent->getUniqueEnergies();
}

string ComponentGroup::checkCurrentTableModelExpr (const Component* component, const string& prevExprString)
{
   string newComponentName;
   if (const AddTableComponent* addTable = dynamic_cast<const AddTableComponent*>(component))
   {
      const string& newFileName = addTable->tableFile();
      newComponentName = "atable{" + newFileName + "}";
   }
   else if (const MulTableComponent* mulTable = dynamic_cast<const MulTableComponent*>(component))
   {
      const string& newFileName = mulTable->tableFile();
      // This could begin with mtable or etable.  If it made it
      // this far, oldName has already been validated and only
      // need to check 'eE' or 'mM'.
      if (prevExprString[0] == 'e' || prevExprString[0] == 'E')
         newComponentName = "etable{";
      else
         newComponentName = "mtable{";
      newComponentName += newFileName + "}";
   }
   return newComponentName;
}

void ComponentGroup::checkZeroNorms (std::set<int>& parsWithZeroNorms)
{
  bool isParen(m_componentInfo->parenLocs().first != string::npos);
  bool allSubgroupsAreZero=true;
  ComponentListIterator itComp = m_elements.begin();
  ComponentListIterator itEnd = m_elements.end();
  if (isParen)
  {
     std::pair<size_t,size_t> bracketIndices(compsInsideParentheses(itComp, itEnd));
     // Must have 1 or more plus signs if inside parentheses.
     size_t iPlus=0;
     size_t iComp=bracketIndices.first;
     size_t nextPlusPos = m_componentInfo->plusLocs()[iPlus];
     ComponentListIterator startGroup(itComp);
     while (itComp != itEnd)
     {
        bool isNormZero=false;
        AddComponent* addComp = dynamic_cast<AddComponent*>(*itComp);
        if (addComp)
        {
           if (std::fabs(addComp->norm()) < SMALL)
              isNormZero=true;
           else
              allSubgroupsAreZero = false;
        }
        else if ((*itComp)->isGroup())
        {
           if (!(*itComp)->isZeroNorm())
              allSubgroupsAreZero = false;
        }
        ++iComp;
        ++itComp;
        if (itComp == itEnd || 
                m_componentInfo->getComponentLocation(iComp) > nextPlusPos)
        {
           // We've gone past a '+' or the end of the parentheses.           
           ComponentListIterator endGroup(itComp);
           itComp = startGroup;
           while (itComp != endGroup)
           {
              if (!(*itComp)->isGroup())
                 (*itComp)->isZeroNorm(isNormZero);
              ++itComp;
           }
           startGroup = endGroup;
           if (itComp != itEnd)
           {
              ++iPlus;
              nextPlusPos = iPlus < m_componentInfo->plusLocs().size() ?
                        m_componentInfo->plusLocs()[iPlus] : string::npos;
           }
        }
     } // end inside parentheses loop
     
     // Now set the components outside the parentheses.
     itComp = m_elements.begin();
     for (iComp=0; iComp<bracketIndices.first; ++iComp, ++itComp)
     {
        (*itComp)->isZeroNorm(allSubgroupsAreZero);
     }
     while (iComp < bracketIndices.second)
     {
        ++iComp;
        ++itComp;
     }
     while (itComp != m_elements.end())
     {
        (*itComp)->isZeroNorm(allSubgroupsAreZero);
        ++itComp;
     }
  } // end if parentheses
  else
  {
     bool isNormZero=false;
     AddComponent* addComp=0;
     while (!addComp && itComp != itEnd)
     {
        addComp = dynamic_cast<AddComponent*>(*itComp);
        if (addComp)
        {
           if (std::fabs(addComp->norm()) < SMALL)
              isNormZero=true;
        }
        ++itComp;
     }
     allSubgroupsAreZero = isNormZero;
     itComp = m_elements.begin();
     while (itComp != itEnd)
     {
        (*itComp)->isZeroNorm(isNormZero);
        ++itComp;
     }
  } // end if no parentheses
  // Must also set the groupFlux SumComponent.  If this is a nested
  //  ComponentGroup, those above it in the tree will need to check.
  m_groupFlux->isZeroNorm(allSubgroupsAreZero);
  
  getParsWithZeroNorms(parsWithZeroNorms);
} // end checkZeroNorms

void ComponentGroup::getParsWithZeroNorms(std::set<int>& parsWithZeroNorms) const
{
   ComponentListConstIterator itComp = m_elements.begin();
   ComponentListConstIterator itEnd = m_elements.end();
   while (itComp != itEnd)
   {
      const Component* comp = *itComp;
      if (!comp->isGroup() && comp->isZeroNorm())
      {
         std::vector<ModParam*> vp = comp->getVariableParameters();
         for (size_t i=0; i<vp.size(); ++i)
         {
            // The norm parameter itself should not be frozen.  The
            // model derivatives can be sensitive to the norm parameter
            // even when it's toggled about zero.
            if (vp[i]->name() != "norm")
            {
               int fullIndex = (m_parent->index() << XSContainer::ModelContainer::SHIFT())
                      + vp[i]->index();
               parsWithZeroNorms.insert(fullIndex);
            }
         }         
      }
      ++itComp;
   }
}

std::pair<size_t,size_t> ComponentGroup::compsInsideParentheses(ComponentListIterator& parBeg, ComponentListIterator& parEnd)
{
     // There can only be 0 or 1 set of parentheses because of the definition
     // of a component group and the treatment of nested groups. This
     // ASSUMES 1 set has been verified to exist. 

     std::pair<size_t,size_t> compIndices;
     parBeg = m_elements.begin();
     size_t iWord=0;
     size_t lb =  m_componentInfo->parenLocs().first;
     size_t rb =  m_componentInfo->parenLocs().second;
     while (m_componentInfo->getComponentLocation(iWord) < lb) 
     {
        ++parBeg;
        ++iWord;
     }
     compIndices.first = iWord;
     parEnd = parBeg;
     while (parEnd != m_elements.end() && 
        m_componentInfo->getComponentLocation(iWord) < rb)
     {
        ++iWord;
        ++parEnd;
     }
     compIndices.second = iWord;
     return compIndices;
}
