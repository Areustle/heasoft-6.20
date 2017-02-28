#include <XSModel/Model/CompCombiner.h>
#include <XSModel/Model/Model.h>
#include <XSModel/Model/ModelBase.h>
#include <XSModel/Model/Component/AddComponent.h>
#include <XSModel/Model/Component/ComponentGroup.h>
#include <XSModel/Model/Component/MixComponent.h>
#include <XSModel/Model/Component/MulComponent.h>
#include <XSModel/Model/Component/SumComponent.h>
#include <XSUtil/Error/Error.h>
#include <iterator>


CombineIterator::CombineIterator(const CompCombiner* combiner)
: m_combiner(combiner),
  m_nGroups(combiner->models().size()),
  m_iCurModel(0),
  m_itCurCompGroups(),
  m_iCalcSubgroups(m_nGroups, 0),
  m_subgroupLocs(m_nGroups),
  m_curCompLocs(),
  m_compStacks(m_nGroups),
  m_totalAccumulated(m_nGroups, 0),
  m_starLocs(),
  m_iCurStarLoc(0),
  m_parenFluxes(m_nGroups, 0),
  m_insIdx(0),
  m_nSkip(0),
  m_indivStacks(m_nGroups)
{
   for (size_t i=0; i<m_nGroups; ++i)
   {
      m_iCurModel = i;
      m_itCurCompGroups.push_back(combiner->models()[i]->m_modelBase->compGroupTree().postBegin());
      m_curCompLocs.push_back(CompLocInfo((*m_itCurCompGroups[i])->elements().begin(),0));      
      initForComponentGroup();
      initForSubgroup();
   }
   m_iCurModel = 0;
}

// Empty 'end' iterator.  Not to be used in ANY other context.
CombineIterator::CombineIterator()
: m_combiner(0),
  m_nGroups(0),
  m_iCurModel(string::npos),
  m_itCurCompGroups(),
  m_iCalcSubgroups(),
  m_subgroupLocs(),
  m_curCompLocs(),
  m_compStacks(),
  m_totalAccumulated(),
  m_starLocs(),
  m_iCurStarLoc(string::npos),
  m_parenFluxes(),
  m_insIdx(0),
  m_nSkip(0),
  m_indivStacks()
{
}

CombineIterator::~CombineIterator()
{
   for (size_t i=0; i<m_nGroups; ++i)
   {
      delete m_totalAccumulated[i];
      delete m_parenFluxes[i];
   }
}

void CombineIterator::initForComponentGroup()
{
   const bool isLastModel = m_iCurModel == (m_nGroups-1);
   if (isLastModel)
   {
      // These bookkeeping variables are the same for all model copies,
      //  so just set them when entering here for the last model.
      //  We do it for the last model rather than the first due to
      //  the order of usage in operator++:  must not change these
      //  until all model data group copies have been processed
      //  with the settings for the previous ComponentGroup.
      m_insIdx = 0;
      m_nSkip = 0;
   }
   m_iCalcSubgroups[m_iCurModel] = 0;
   m_subgroupLocs[m_iCurModel].clear();
   delete m_parenFluxes[m_iCurModel];
   m_parenFluxes[m_iCurModel]=0;
   
   const ComponentGroup* cgroup = *m_itCurCompGroups[m_iCurModel];
   const ModelExpression<ModExprTreeMember>* compInfo = cgroup->componentInfo();      
   bool noParens = compInfo->parenLocs().first == string::npos;
   const size_t nPlus = compInfo->plusLocs().size();
   if (noParens)
   {
      ComponentListConstIterator itFirst = cgroup->elements().begin();
      ComponentListConstIterator itLast = cgroup->elements().end();
      --itLast;
      CompLocInfo firstComp(itFirst, 0);
      size_t endIdx = static_cast<size_t>(std::distance(itFirst, itLast));
      CompLocInfo lastComp(itLast, endIdx);
      m_subgroupLocs[m_iCurModel].push_back(std::make_pair(firstComp,lastComp));
   }
   else
   {      
      const size_t lb = compInfo->parenLocs().first;
      ComponentListConstIterator itFirst = cgroup->elements().begin();
      
      // Now find first comp inside parentheses.
      size_t iComp=0;
      while (compInfo->getComponentLocation(iComp) < lb)
      {
         ++itFirst;
         ++iComp;
      }
      size_t firstIdx=iComp;
      const size_t firstInsideParIdx = firstIdx;
      ComponentListConstIterator itLast = itFirst;
      for (size_t iPlus=0; iPlus<nPlus; ++iPlus)
      {
         const size_t pl = compInfo->plusLocs()[iPlus];
         while (compInfo->getComponentLocation(iComp) < pl)
         {
            ++itLast;
            ++iComp;
         }
         --itLast;
         size_t endIdx = iComp-1;
         CompLocInfo firstComp(itFirst, firstIdx);
         CompLocInfo lastComp(itLast, endIdx);
         m_subgroupLocs[m_iCurModel].push_back(std::make_pair(firstComp,lastComp));
         // Now reset iterators for next subgroup.
         itFirst = ++itLast;
         firstIdx = iComp;
      }
      // Still need to get the subgroup to the right of the last '+'.
      const size_t rb = compInfo->parenLocs().second;
      while (iComp < compInfo->compSpecs().size() &&
                compInfo->getComponentLocation(iComp) < rb)
      {
         ++itLast;
         ++iComp;
      }
      --itLast;
      --iComp;
      CompLocInfo firstComp(itFirst, firstIdx);
      CompLocInfo lastComp(itLast,iComp);
      m_subgroupLocs[m_iCurModel].push_back(std::make_pair(firstComp,lastComp));
      if (isLastModel)
      {
         // See comments at top for why we wait till the last
         // model before resetting these.
         m_insIdx = iComp;
         m_nSkip = iComp - firstInsideParIdx + 1;
      }
      
      // Now add subgroup markers for the final combination involving
      //  components external to the parentheses.
      firstComp.m_itComp = cgroup->elements().begin();
      firstComp.m_iComp = 0;
      lastComp.m_itComp = cgroup->elements().end();
      --(lastComp.m_itComp);
      lastComp.m_iComp = cgroup->elements().size()-1;
      m_subgroupLocs[m_iCurModel].push_back(std::make_pair(firstComp,lastComp));
      
   } // end with parens
}

void CombineIterator::initForSubgroup()
{
   const bool isLastModel = m_iCurModel == (m_nGroups-1);
   if (isLastModel)
   {
      m_starLocs.clear();
      m_iCurStarLoc = 0;
   }
   const size_t nSubs = m_subgroupLocs[m_iCurModel].size();
   const size_t iSubgroup = m_iCalcSubgroups[m_iCurModel];
   m_curCompLocs[m_iCurModel] = 
        m_subgroupLocs[m_iCurModel][iSubgroup].second;
   CompLocInfo& curCompLoc = m_curCompLocs[m_iCurModel];
   Component* comp=0;
   bool doFinalParenCombo=false;
   size_t nAddSources=0;
   if (nSubs > 1 && iSubgroup == nSubs - 1)
   {
      // Subgroups within parentheses have been completed, now do final combination
      // of parentheses with external components.
      
      // Note that it can NEVER get in here for the first subgroup
      //  of a ComponentGroup.  This is important, as it is what allows
      //  initForComponentGroup to hold off the resetting of 
      //  m_insIdx and m_nSkip until the previous
      //  ComponentGroup has been processed (in operator++) for the
      //  last model data group copy. 
      doFinalParenCombo = true;
      if (m_combiner->doStoreSources())
      {
         nAddSources = 
                (*m_itCurCompGroups[m_iCurModel])->individualSources().size();
         m_indivStacks[m_iCurModel].resize(nAddSources);
      }
      
      if (isLastModel)
         postfixOperatorLocs(m_insIdx, m_nSkip);
      comp = (m_insIdx == curCompLoc.m_iComp) ?
            m_parenFluxes[m_iCurModel] : *(curCompLoc.m_itComp);
   }
   else
   {
      if (isLastModel && 
          m_subgroupLocs[m_iCurModel][iSubgroup].second.m_itComp != 
          m_subgroupLocs[m_iCurModel][iSubgroup].first.m_itComp)
      {
         postfixOperatorLocs(0,0);      
      }
      comp = *(curCompLoc.m_itComp);
   }
   SumComponent *subgroup=0;
   if (comp->toSumComponent())
   {
      // This is a GROUP or an m_parenFlux.
      subgroup = comp->toSumComponent();
   }
   else if (AddComponent* add = dynamic_cast<AddComponent*>(comp))
   {
      subgroup = new SumComponent(*add);
      subgroup->normalize();
   }
   else if (MulComponent* mul = dynamic_cast<MulComponent*>(comp))
      subgroup = new SumComponent(*mul);
   else
      throw RedAlert("Invalid first component in CombineIterator::operator++.");
   m_compStacks[m_iCurModel].push(subgroup);
   if (doFinalParenCombo && m_combiner->doStoreSources())
   {
      if (comp->toSumComponent())
      {
         // When doFinalParenCombo=true, this must be an m_parenFlux.
         //  It can't be a GROUP.
         std::list<SumComponent*>::iterator itAddSource = 
            (*m_itCurCompGroups[m_iCurModel])->individualSources().begin();
         std::list<SumComponent*>::iterator itEnd =
            (*m_itCurCompGroups[m_iCurModel])->individualSources().end();
         size_t iAddSource=0;
         while (itAddSource != itEnd)
         {
            std::stack<Component*>& indivStack = m_indivStacks[m_iCurModel][iAddSource];
            indivStack.push(*itAddSource);
            ++iAddSource;
            ++itAddSource;
         }
      }
      else
      {
         for (size_t iAddSource=0; iAddSource<nAddSources; ++iAddSource)
         {
            std::stack<Component*>& indivStack = m_indivStacks[m_iCurModel][iAddSource];
            SumComponent* sumCopy = new SumComponent(*subgroup);
            indivStack.push(sumCopy);
         }
      }
   }

   if (!curCompLoc.m_iComp)
   {
      curCompLoc.m_iComp = string::npos;
   }
   else
   {
      if (doFinalParenCombo && m_insIdx == curCompLoc.m_iComp)
      {
         curCompLoc.m_iComp = (m_nSkip > curCompLoc.m_iComp) ?
            string::npos : curCompLoc.m_iComp - m_nSkip;
         for (size_t i=0; i<m_nSkip; ++i)
            --(curCompLoc.m_itComp);
      }
      else
      {
         --(curCompLoc.m_itComp);
         --(curCompLoc.m_iComp);
      }
   }
      
}

void CombineIterator::postfixOperatorLocs (size_t skipIdx, size_t nSkip)
{
   // ASSUME nComps from right to left starting with endIdx contain no 
   // non-skipped "+" operations.  Will search (from right-to-left) for
   // 3 symbol types: '(', ')', and components.  '*'s will NOT be searched
   // for (since they may or may not exist in tandem with parentheses),
   // but their positions can be inferred.
   const size_t iSubgroup = m_iCalcSubgroups[m_iCurModel];
   size_t endIdx = m_subgroupLocs[m_iCurModel][iSubgroup].second.m_iComp;
   size_t nComps = endIdx - m_subgroupLocs[m_iCurModel][iSubgroup].first.m_iComp + 1;
   
   const ModelExpression<ModExprTreeMember>* expressionInfo =
      (*m_itCurCompGroups[m_iCurModel])->componentInfo(); 
   
   string::size_type compLoc = 
      static_cast<string::size_type>(expressionInfo->getComponentLocation(endIdx));
   string exprStr = expressionInfo->exprString();
   if (nSkip)
   {
      // This is a final summation.  Additive parens and everything 
      // inside them will be replaced by a single sum component.
      string::size_type lPar = expressionInfo->parenLocs().first;
      string::size_type rPar = expressionInfo->parenLocs().second;
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
            m_starLocs.push_back(curIdx);
            opHolder.pop();
         }
         nextLoc = exprStr.rfind('(', itCur->first-1);
      }
      else // Assume component
      {
         --curIdx;
         if (prevSymb == 'c' || prevSymb == '(')
         {
            m_starLocs.push_back(curIdx);
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
            nextLoc = static_cast<string::size_type>(expressionInfo->getComponentLocation(curIdx-1));
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
         m_starLocs.push_back(curIdx);
      opHolder.pop();
   } 
            
}

CombineIterator& CombineIterator::operator++()
{
   bool isLastOperation=false;
   const size_t i = m_iCurModel;
   if (m_iCurStarLoc == m_starLocs.size())
   {
      // An addition operation must be performed. 
       
      // We've reached the end of the subgroup (or it was just a single
      //   comp to begin with).
      if (m_compStacks[i].size() != 1)
         throw RedAlert("Component combination error durring addition operation.");
      SumComponent* stackCalc = m_compStacks[i].top()->toSumComponent();
      if (!stackCalc)
         throw RedAlert("Missing SumComponent in CombineIterator::operator++");
      m_compStacks[i].pop();
      
      
      // proceed to next subgroup
      bool isFinishedModel=false;
      ++m_iCalcSubgroups[i];
      if (m_iCalcSubgroups[i] == m_subgroupLocs[i].size())
      {
         // Final subgroup calculation.  stackCalc cannot merely be 
         //  a nested ComponentGroup pointer.  If we're in here ,this
         //  must either be the post-parentheses-calc combination stage
         //  or a ComponentGroup consisting of a single subgroup.
         
         SumComponent* cgroupFlux = (*m_itCurCompGroups[i])->groupFlux();
         // Need groupFlux to retain its "GROUPOx" name string,  
         // but the assignment to *stackCalc will unintentionally modify it.
         string storeName(cgroupFlux->name());
         (*cgroupFlux) = (*stackCalc);
         cgroupFlux->name(storeName);
         // Also due to above assignment, cgroupFlux has picked up stackCalc's 
         // Parameter* to an AddComp norm parameter.  cgroupFlux should
         // not retain this, otherwise if this is a nested CGroup, the
         // enclosing CGroup's SumComponent placeholder pointer will also
         // point to this norm parameter.  This condition caused parameter
         // linking errors when a data group copy model was made.
         std::vector<Parameter*> empty;
         cgroupFlux->setParameterSet(empty);
         
         if (m_combiner->doStoreSources() && m_subgroupLocs[i].size() == 1)
         {
            // For a ComponentGroup with a single subgroup, individualSource list
            //  would not have been filled in during the section below dealing 
            //  with inside-parentheses groups.  So do it now.
            (*m_itCurCompGroups[i])->individualSources().push_back(stackCalc->clone(*m_itCurCompGroups[i]));
         }
         
         if (!(*m_itCurCompGroups[i])->isNested())
         {
            if (!m_totalAccumulated[i])
            {
               m_totalAccumulated[i] = new SumComponent(*stackCalc);
            }
            else
            {
               (*m_totalAccumulated[i]) += (*stackCalc);
            }
            
            if (m_combiner->doStoreSources())
            {
               std::list<SumComponent*>::iterator itAddSource = 
                  (*m_itCurCompGroups[m_iCurModel])->individualSources().begin();
               std::list<SumComponent*>::iterator itEnd =
                  (*m_itCurCompGroups[m_iCurModel])->individualSources().end();
               while (itAddSource != itEnd)
               {
                  (*itAddSource)->fillPhotonArrays();
                  ++itAddSource;
               }
                  
               std::list<SumComponent*> finalStorage(m_combiner->models()[i]->m_modelBase->componentSource());
               finalStorage.splice(finalStorage.end(), 
                        (*m_itCurCompGroups[i])->individualSources());
               m_combiner->models()[i]->m_modelBase->componentSource(finalStorage);
            }
         }
         delete stackCalc;
         if (stackCalc == m_parenFluxes[i])
            m_parenFluxes[i] = 0;
         
         // proceed to next ComponentGroup
         m_itCurCompGroups[i].postOrderNext();
         if (m_itCurCompGroups[i] == m_combiner->models()[i]->m_modelBase->compGroupTree().end())
         {
            SumComponent* fluxForMod = m_totalAccumulated[i];
            Model* mod = m_combiner->models()[i];
            fluxForMod->fillPhotonArrays();
            mod->m_modelBase->setModelFlux(fluxForMod->photonArray());
            mod->m_modelBase->setModelFluxError(fluxForMod->photonErrArray());
            
            if (i==m_nGroups-1)
            {
               setToEnd();
               isLastOperation = true;
            }
            isFinishedModel = true;
         }
         else
            initForComponentGroup();
      } // end if final subgroup calculation
      else
      {
         // We've finished a subgroup that exists inside parentheses.
         if (m_combiner->doStoreSources())
         {
            std::list<SumComponent*>& indivSources = 
                (*m_itCurCompGroups[i])->individualSources();
            if (stackCalc->isGroup())
            {
               // Absorb nested group's source list.
               ComponentGroup* nestedGroup = stackCalc->parent();
               indivSources.splice(indivSources.end(),nestedGroup->individualSources());
            }
            else
            {
               indivSources.push_back(stackCalc->clone(*m_itCurCompGroups[i]));
            }
         }
         if (!m_parenFluxes[i])
         {
            if (stackCalc->isGroup())
               m_parenFluxes[i] = stackCalc->clone(*m_itCurCompGroups[i]);
            else
               m_parenFluxes[i] = stackCalc;
            // Not sure this is necessary, but it lets SumComponent::normalize
            //  recognize that m_parenFlux is a temporary SC that doesn't require
            //  normalization.
            m_parenFluxes[i]->name("SUM");
         }
         else
         {
            (*m_parenFluxes[i]) += (*stackCalc);
            if (!stackCalc->isGroup())
               delete stackCalc;
         }
      }
      
      if (!isFinishedModel)
         initForSubgroup();
   }  // end '+' operation
   else   
   {
      const size_t nSubs = m_subgroupLocs[i].size();
      const bool doFinalParenCombo = (nSubs > 1 && 
                m_iCalcSubgroups[m_iCurModel] == nSubs - 1);
      const size_t nextStar = m_starLocs[m_iCurStarLoc];
      const size_t nextComp = m_curCompLocs[i].m_iComp;
      if (nextComp != string::npos && nextComp >= nextStar)
      {
         Component* comp=0;      
         SumComponent* subgroup = 0;      
         if (doFinalParenCombo && nextComp == m_insIdx)
         {
            comp = m_parenFluxes[i];
         }
         else
         {
            comp = *(m_curCompLocs[i].m_itComp);
         }
         // If component is not followed by a star in postfix, it is either
         // the calc beginning (rightmost), or to the left of an ')'.  
         // It must be converted to a SumComponent.
         if (nextStar != nextComp)
         {
            // The subgroup object created in here will always INITIALLY be 
            //  popped 'second' off the stack.  It can get re-inserted in the
            //  stack and eventually popped first.  (This scenario happens
            //  for example with the m1 component of (a1*m1)*m2.)  It must
            //  always get deleted within a future operator++ call.
            if (AddComponent* add = dynamic_cast<AddComponent*>(comp))
            {
               subgroup = new SumComponent(*add);
               subgroup->normalize();
            }
            else if (MulComponent* mul = dynamic_cast<MulComponent*>(comp))
               subgroup = new SumComponent(*mul);
            else if (comp == m_parenFluxes[i])
            {
               // Will get in here when processing inner parens for something
               // like: (wa(po+ga))phabs
               subgroup = new SumComponent(*m_parenFluxes[i]);
            }
            else
               throw RedAlert("Invalid first component in CombineIterator::operator++.");
            m_compStacks[i].push(subgroup);            
         }
         else
            // This will always be popped 'first' off the stack and
            //   will never be re-inserted.  It will get deleted in operator++
            //   (during this iteration) if it is an m_parenFlux (for example the '(po+ga)'
            //   in (po+ga)*wa ).
            m_compStacks[i].push(comp);
            
         if (m_combiner->doStoreSources() && doFinalParenCombo)
         {
            // Fill stacks for each additive group
            std::list<SumComponent*>::iterator itAddSource = 
               (*m_itCurCompGroups[i])->individualSources().begin();
            std::list<SumComponent*>::iterator itEnd =
               (*m_itCurCompGroups[i])->individualSources().end();
            size_t iAddSource=0;
            if (nextComp == m_insIdx)
            {
               while (itAddSource != itEnd)
               {
                  std::stack<Component*>& indivStack = m_indivStacks[i][iAddSource];
                  indivStack.push(*itAddSource);
                  ++iAddSource;
                  ++itAddSource;
               }
            }
            else
            {
               while (itAddSource != itEnd)
               {
                  std::stack<Component*>& indivStack = m_indivStacks[i][iAddSource];
                  if (subgroup)
                  {
                     indivStack.push(new SumComponent(*subgroup));
                  }
                  else
                  {
                     // Shallow copy - comp will only ever be popped first,
                     //   therefore can't be modified.
                     indivStack.push(comp);
                  }
                  ++iAddSource;
                  ++itAddSource;
               }
            }
         }   
            
         // Now shift m_curCompLocs leftward to the next relevant
         //  component.    
         if (!nextComp)
         {
            // The leftmost component in CGroup has been processed.
            m_curCompLocs[i].m_iComp = string::npos;
         }
         else
         {
            if (doFinalParenCombo && nextComp == m_insIdx)
            {
               if (m_nSkip > m_curCompLocs[i].m_iComp)
               {
                  // Leftmost component has been processed.
                  m_curCompLocs[i].m_iComp = string::npos;                  
               }
               else
               {
                  m_curCompLocs[i].m_iComp -= m_nSkip;
                  for (size_t j=0; j<m_nSkip; ++j)
                     --(m_curCompLocs[i].m_itComp);
               }
            }
            else
            {
               --(m_curCompLocs[i].m_itComp);
               --(m_curCompLocs[i].m_iComp);
            }
         }
      } // end if stack-push only (no '*' operation yet)
      else
      {
         if (m_compStacks[i].size() < 2)
            throw RedAlert("Components missing during CombineIterator::operator++ multiplication.");
         // The second component from the top must always be a SumComponent.   
         Component* first = m_compStacks[i].top();
         SumComponent* subgroup=0;
         MixComponent* mix = dynamic_cast<MixComponent*>(first);
         if (mix)
         {
            // Mixing operations are performed for the ENTIRE group of models
            //  as soon as the first in the group is reached.  
            if (i==0)
            {
               std::vector<SumComponent*> compsToMix;
               for (size_t jMod=0; jMod<m_nGroups; ++jMod)
               {
                  m_compStacks[jMod].pop();
                  Component* second = m_compStacks[jMod].top();
                  m_compStacks[jMod].pop();
                  subgroup = second->toSumComponent();
                  if (!subgroup)
                     throw RedAlert("SumComponent missing during CombineIterator::operator++ mix multiplication.");
                  compsToMix.push_back(subgroup);
               }
               // Now perform the actual mixing operation (if more than 1 group).
               if (m_nGroups > 1)
                  (*mix)(compsToMix);
               for (size_t jMod=0; jMod<m_nGroups; ++jMod)
               {
                  m_compStacks[jMod].push(compsToMix[jMod]);
               }
            }
         }
         else
         {
            m_compStacks[i].pop();
            Component* second = m_compStacks[i].top();
            m_compStacks[i].pop();
            subgroup = second->toSumComponent();
            if (!subgroup)
               throw RedAlert("SumComponent missing during CombineIterator::operator++ multiplication.");

            // If first is an AddComponent, it couldn't have been normalized 
            // in the section above.  Do it now before combining with subgroup.
            if (AddComponent* add = dynamic_cast<AddComponent*>(first))
            {
               SumComponent tmpSC(*add);
               tmpSC.normalize();
               (*subgroup) *= tmpSC;
            }
            else
               (*subgroup) *= (*first);
            m_compStacks[i].push(subgroup);

         }
         if (first->toSumComponent())
         {
            delete first;
            // If first == m_parenFlux, it means we are finished using
            //   m_parenFlux and it is OK if it gets deleted.  Just
            //   make sure to null its pointer.
            if (first == m_parenFluxes[i])
               m_parenFluxes[i] = 0;
         } 

         if (m_combiner->doStoreSources() && doFinalParenCombo)
         {
            std::list<SumComponent*>::iterator itAddSource = 
               (*m_itCurCompGroups[i])->individualSources().begin();
            std::list<SumComponent*>::iterator itEnd =
               (*m_itCurCompGroups[i])->individualSources().end();
            size_t iAddSource=0;
            while (itAddSource != itEnd)
            {
               std::stack<Component*>& indivStack = m_indivStacks[i][iAddSource];            
               Component* indivFirst = indivStack.top();
               if (MixComponent* mix = dynamic_cast<MixComponent*>(indivFirst))
               {
                  // Mixing operations are performed for the ENTIRE group of models
                  //  as soon as the first in the group is reached.  
                  if (i==0)
                  {
                     std::vector<SumComponent*> compsToMix;
                     for (size_t jMod=0; jMod<m_nGroups; ++jMod)
                     {
                        m_indivStacks[jMod][iAddSource].pop();
                        Component* second = m_indivStacks[jMod][iAddSource].top();
                        m_indivStacks[jMod][iAddSource].pop();
                        subgroup = second->toSumComponent();
                        if (!subgroup)
                           throw RedAlert("SumComponent missing during CombineIterator::operator++ mix multiplication of source component.");
                        compsToMix.push_back(subgroup);
                     }
                     if (m_nGroups > 1)
                        (*mix)(compsToMix);
                     for (size_t jMod=0; jMod<m_nGroups; ++jMod)
                     {
                        m_indivStacks[jMod][iAddSource].push(compsToMix[jMod]);
                     }
                  }
               }
               else
               {
                  indivStack.pop();
                  Component* indivSecond = indivStack.top();
                  indivStack.pop();
                  SumComponent* indivSubgroup = indivSecond->toSumComponent();
                  if (!indivSubgroup)
                     throw RedAlert("SumComponent missing during CombineIterator::operator++ multiplication of source component.");

                  if (AddComponent* add = dynamic_cast<AddComponent*>(indivFirst))
                  {
                     SumComponent tmpSC(*add);
                     tmpSC.normalize();
                     (*indivSubgroup) *= tmpSC;
                  }
                  else
                     (*indivSubgroup) *= (*indivFirst);
                  indivStack.push(indivSubgroup);
                  if (indivFirst->toSumComponent())
                  {
                     if (indivFirst == *itAddSource)
                     {
                        *itAddSource = indivSubgroup;
                     }
                     delete indivFirst;
                  }
               }
               ++itAddSource;
               ++iAddSource;
            }
         }

         if (mix)
         {
            // If a mix component has just been handled, all groups
            //  will have been processed at once.
            m_iCurModel = m_nGroups - 1;
         }
         if (m_iCurModel==m_nGroups-1)
            ++m_iCurStarLoc;

      } // end if '*' operation
   } // end if not '+' operation
   if (!isLastOperation)
   {
      ++m_iCurModel;
      if (m_iCurModel == m_nGroups)
         m_iCurModel = 0;
   }
   return *this;
}

bool CombineIterator::operator == (const CombineIterator& right) const
{
   // There's a LOT of leeway as to how to define the equality condition
   //  for a CombineIterator.  Since I intend to use this mostly in the
   //  context of checking for the end() condition, I'll just do a
   //  minimal comparison of members -- the same members that are used
   //  to specify the 'end' condition.
   //  Note that this just ASSUMES the user is comparing iterators pointing
   //  into the same Model object.
   
   return (m_iCurModel == right.m_iCurModel && 
           m_iCurStarLoc == right.m_iCurStarLoc);
}

bool CombineIterator::operator != (const CombineIterator& right) const
{
   return !((*this) == right);
}

void CombineIterator::setToEnd()
{
   // This leaves the internal bookeeping in an inconsistent state.
   //  Therefore (as with STL) one must never do a ++ operation on
   //  an iterator that has reached the end.
   m_iCurModel = string::npos;   
   m_iCurStarLoc = string::npos;
}


CompCombiner::CompCombiner (const std::vector<Model*>& models, bool storeSources)
  : m_models(models),
    m_doStoreSources(storeSources)
{
   if (!models.size())
      throw RedAlert("Programmer Error: Empty models array sent to CompCombiner.");
}

CompCombiner::~CompCombiner()
{
}

CompCombiner::iterator CompCombiner::begin()
{
   return iterator(this);
}

CompCombiner::iterator CompCombiner::end()
{
   return iterator();
}
