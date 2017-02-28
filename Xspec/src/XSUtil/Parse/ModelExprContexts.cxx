//   Read the documentation to learn more about C++ code generator
//   versioning.
//	  %X% %Q% %Z% %W%
#include <XSUtil/Parse/ModelExpression.h>

#include <XSUtil/Utils/IosHolder.h>
#include <XSUtil/Utils/XSstream.h>
#include <algorithm>
#include <sstream>

// ModelExprContexts
#include <XSUtil/Parse/ModelExprContexts.h>


// Class ModExprTreeMember 

ModExprTreeMember::ModExprTreeMember()
  :m_location(-1),
   m_group(-1),
   m_parenLocs(string::npos,string::npos),
   m_plusLocs()
{
}


void ModExprTreeMember::sequenceSubExpressions (ModelExprTree::iterator& itExp, int& wordStart)
{
   // Need to sequence the comps in all the sub-Expressions in tree so
   // that when pieced back together they would be in the same 
   // left-right sequential order as the original ModelExpression.  Do this
   // by performing a PRE-ORDER tree traversal using recursion.
   const std::vector<HostClass::ComponentSpec>& expWords = itExp->compSpecs();
   ModelExprTree::iterator itOrig(itExp);
   for (size_t i=0; i<expWords.size(); ++i)
   {
      if (expWords[i].content.substr(0,6) == string("$GROUP"))
      {
         // Tree iterator increments in pre-order direction.
         ++itExp;
         sequenceSubExpressions(itExp, wordStart);
      }
      else
      {
         // This strange casting to a base class pointer is (deliberately) 
         // the only way to get to the protected HostClass 
         // setComponentSequence function.
         ModExprTreeMember* pSub = &(*itOrig);
         pSub->setComponentSequence(wordStart++,i);
      }      
   }
}

void ModExprTreeMember::contextSpecificInit (HostClass* host)
{
   m_parenLocs.second = string::npos;
   m_plusLocs.clear();

   const string& exprStr = host->exprString();

   // Find the one pair of parentheses that may be enclosing  
   // non-nested '+' signs.
   string tmpStr(HostClass::flagNonGroupParens(exprStr));

   m_parenLocs.first = tmpStr.find_first_of('(');
   if (m_parenLocs.first != string::npos)
   {
      m_parenLocs.second = tmpStr.find_first_of(')',m_parenLocs.first);
      if (m_parenLocs.second == string::npos)
         throw RedAlert("Unbalanced parentheses in tree sub-expression");
   }

   // Don't assume all plus signs are inside parentheses at this stage.  
   // When the TREE is in its final state this will be the case for each 
   // newly created sub-expression, but an existing sub-expression may
   // have a plus sign appended by an addcomp operation.

   size_t curPos=0;
   while (curPos < exprStr.length())
   {
      curPos = exprStr.find_first_of('+',curPos+1);
      if (curPos != string::npos)
         m_plusLocs.push_back(curPos);
   }
}

void ModExprTreeMember::Swap (ModExprTreeMember& right)
{
   std::swap(m_location,right.m_location);
   std::swap(m_group,right.m_group);
   std::swap(m_parenLocs,right.m_parenLocs);
   std::swap(m_plusLocs,right.m_plusLocs);
}

// Additional Declarations

// Class ModExprStandAlone 

ModExprStandAlone::ModExprStandAlone()
  :m_fullExprString()
{
}


void ModExprStandAlone::createExpTree (ModelExprTree& expTree) const
{
  // Get location of all TOP level component groups, not nested.
  std::vector<string::size_type> groupLocs = findGroups(m_fullExprString);
  string::size_type startPos = 0;
  IntegerArray groupBoundary;
  for (int iGroup=0; iGroup<(int)groupLocs.size(); ++iGroup)
  {
     string compGroupString(m_fullExprString.substr(startPos, 
                                groupLocs[iGroup] - startPos));
     // makeSubExpressions is a recursive function that will
     // create ModelExpression objs for any nested component groups,
     // and add them to the expTree in post-order.
     compGroupString = HostClass::flagNonGroupParens(compGroupString);
     makeSubExpressions(compGroupString, expTree);
     groupBoundary.push_back((int)expTree.size()-1);
     startPos = groupLocs[iGroup] + 1;
  }
  expTree.insertRoot(groupBoundary);

  // Set m_group to be the same for every sub-Exp in a top level
  // component group, enforcing the CONSTRAINT that only top level 
  // component groups will have m_group = m_location.
  ModelExprTree::iterator itExp = expTree.begin();
  ModelExprTree::iterator itEnd = expTree.end();
  for (int iGroup=0; iGroup<(int)groupBoundary.size(); ++iGroup)
  {
     int groupNum = groupBoundary[iGroup];
     while (itExp != itEnd && expTree.position(itExp) <= groupNum)
     {
        itExp->m_group = groupNum;
        ++itExp;
     }
  }

  // sequenceSubExpressions will recursively handle all expressions
  // within a top level component group. It also advances itExp.
  // Sequence numbers are 1-based.
  int wordStart=1;
  itExp = expTree.begin();
  while (itExp != itEnd)
  {
     ModExprTreeMember::sequenceSubExpressions(itExp, wordStart);
     ++itExp;
  }

}

std::vector<string::size_type> ModExprStandAlone::findGroups (const string& inString)
{
   // Intended to be of use PRIOR to creating sub-Expression object from
   // inString.  Therefore cannot use any Token location information.
   // Fill groupLocs with positions = 1 AFTER the end of a top level
   // (meaning non-nested) component group.  Essentially this means the
   // position of every top level '+', with the last entry being 
   // inString.length(). 
   std::vector<string::size_type> groupLocs =
      HostClass::findSameLevelPlus(inString);
   groupLocs.push_back(inString.length());
   return groupLocs;
}

void ModExprStandAlone::makeSubExpressions (const string& inString, ModelExprTree& expTree)
{
   // ASSUMES inString corresponds to a single component group, 
   // meaning it has no non-nested '+'.
   // Uses recursion to perform post-order traversal of sub-Expressions 
   // contained in inString.
   std::vector<string::size_type> nestedLocs = findNests(inString);
   size_t nNests = nestedLocs.size()/2;
   string subExpStr(inString);
   int offset = 0;
   IntegerArray subNodes;
   for (size_t i=0; i<nNests; ++i)
   {
      string::size_type nestPos = nestedLocs[i*2];
      string::size_type nestN = nestedLocs[i*2+1];
      string nestStr(inString.substr(nestPos, nestN));
      makeSubExpressions(nestStr, expTree);
      int locNum = static_cast<int>(expTree.size()-1);
      subNodes.push_back(locNum);
      string groupHolder(HostClass::makeGroupString(locNum));
      // Need to replace the nested part of subExpStr and keep track
      // of the size adjustment this causes.  Otherwise the positions
      // stored in nestedLocs would become meaningless after the 
      // first substitution.
      if (-1*offset > (int)nestPos)
         throw RedAlert("Position error during $GROUP substitution in expression string.");
      subExpStr.replace(static_cast<string::size_type>(nestPos+offset), 
                nestN, groupHolder);
      offset += (int)groupHolder.size() - (int)nestN;
   }
   ModelExpression<ModExprTreeMember> subExpression;
   subExpStr = HostClass::restoreNonGroupParens(subExpStr); 
   subExpression.init(subExpStr, false);
   subExpression.m_location = static_cast<int>(expTree.size());
   // Don't need to set m_group here.  This is best done at the top
   // of the expression tree in the analyze() function.
   expTree.insert(subExpression, subNodes);
}

std::vector<string::size_type> ModExprStandAlone::findNests (const string& inString)
{
   // ASSUMES inString corresponds to a single component group which
   // may or may not contain ()'s.  If it does, this looks for any 
   // ()'s 1 AND ONLY 1 level further down, and stores the starting
   // pos and nChars of the nested group.
   // Naturally assumes parentheses are balanced by this point.

   // Example: If inString = wa(pha(ga+wa(grad+bbody))+po+wa(ga+grad))
   //   insidePar = pha(ga+wa(grad+bbody))+po+wa(ga+grad) 
   //   groupLocs = [22, 25, 36] (pos relative to insidePar)
   //   return nestLocs = [3, 22, 29, 11] (pos relative to inString)
   std::vector<string::size_type> nestLocs;
   string::size_type leftParen = inString.find('(');
   if (leftParen != string::npos)
   {
      string::size_type rightParen = HostClass::findMatchingRightParen(inString, leftParen);
      string insidePar(inString.substr(leftParen+1, rightParen-leftParen-1));
      std::vector<string::size_type> groupLocs = findGroups(insidePar);
      string::size_type start = 0;
      for (size_t i=0; i<groupLocs.size(); ++i)
      {
         // If any '(' exists in this sub-group of insidePar, we've
         // found a nest. Record the positions of this sub-group 
         // relative to inString, not insidePar.
         string subGroup = insidePar.substr(start, groupLocs[i] - start);
         if (subGroup.find('(') != string::npos)
         {
            nestLocs.push_back(start + leftParen+1);
            nestLocs.push_back(subGroup.size());
         }
         start = groupLocs[i]+1;
      }
   }
   return nestLocs;
}

void ModExprStandAlone::contextSpecificInit (HostClass* host)
{
   m_fullExprString = host->exprString();
}

void ModExprStandAlone::Swap (ModExprStandAlone& right)
{
   std::swap(m_fullExprString,right.m_fullExprString);
}

// Additional Declarations
