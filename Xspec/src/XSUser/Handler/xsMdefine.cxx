#include <xsTypes.h>
#include <XSContainer.h>
#include <XSstreams.h>
#include <XSUtil/FunctionUtils/XSModelFunction.h>
#include <XSModel/GlobalContainer/MdefContainer.h>
#include <XSModel/GlobalContainer/ModelContainer.h>
#include <XSModel/Model/Model.h>
#include <XSUser/Global/Global.h>
#include <XSUser/Global/XSGlobal.h>
#include <XSUser/Handler/HandlerUtils.h>
#include <XSUser/Handler/XSinterface.h>
#include <XSUtil/Error/Error.h>
#include <XSUtil/Parse/XSparse.h>
#include <XSUtil/Utils/XSutility.h>
#include <utility>

namespace {
   void findModelsUsing(XSCall<MathExpression>* comp, std::vector<string>& modNames);
}

int
XSGlobal::xsMdefine(ClientData cdata,Tcl_Interp* tclInterp,int objc, Tcl_Obj* CONST  objv[])
{
   StringArray rawArgs;
   HandlerUtils::tclArgsToCpp(objc, objv, rawArgs);
   int status = doMdefine(rawArgs);
   if (!status)
      return globalData->autoSave(TCL_OK);
   else
      return globalData->autoSave(TCL_ERROR);
}

int
XSGlobal::doMdefine(const StringArray& rawArgs)
{
   using namespace XSContainer;
   using namespace std;

   vector<string> componentTypes;
   componentTypes.push_back("add");
   componentTypes.push_back("mul");
   componentTypes.push_back("con");

   const char* cmd = "mdefine";
   const size_t nArgs = rawArgs.size();
   MdefContainer* mdefs = models->userMdefs();
   if (nArgs == 1)
   {
      mdefs->displayComponents();
   }
   else
   {
      string arg1(rawArgs[1]);
      if (arg1 == "?")
         printDocs(cmd,"?");
      else
      {
         if (arg1.length() < 2)
         {
            tcerr << "Name assigned to mdefine model must be at least 2 characters."
                <<endl;
            return -1;
         }
         if (nArgs == 2)
         {
           mdefs->displayComponent(arg1);
         }
         else
         {
            // Is arg1 the name of a built-in or local model (case-insensitive)?
            // If so, don't allow.
            try
            {
               XSModelFunction::fullMatch(arg1);
               tcerr << "***Warning: "<< arg1 << " is a pre-defined model\n"
                  <<"   Please use a different name for your model." <<endl;
               return -1;
            }
            catch (XSModelFunction::NoSuchComponent&)
            {
               // All is well
            }
            catch (YellowAlert&)
            {
               // Should probably never end up in here, but just in case...
               tcerr << "Mdefine error while matching model name " <<arg1<<endl;
               return -1;
            }
            string arg2(rawArgs[2]);
            if (arg2 == ":")
            {
               MdefContainer::CompPair doomedComp(mdefs->getComponent(arg1));
               if (doomedComp.first == MdefContainer::NOT_FOUND())
               {
                  tcerr << "Cannot delete " << arg1 << ", model not found."
                    << endl;
                  return -1;
               }
               else
               {
                  // Delete component, but first remove all models that are
                  // currently making use of it, active or not.
                  vector<string> doomedModNames;
                  findModelsUsing(doomedComp.second, doomedModNames);
                  for (size_t i=0; i<doomedModNames.size(); ++i)
                  {
                     vector<Model*> doomedMods(models->lookupModelGroup(doomedModNames[i]));
                     models->rerouteBrokenParLinks(doomedMods);
                     models->remove(doomedModNames[i]);                     
                  }
                  string compType = mdefs->removeComponent(arg1);
                  tcout << "Mdefine " << compType << " model component definition " 
                        << arg1 << " is now erased." << endl;
                  tcout << "Any models using this component will also be deleted." <<endl;
                  if (doomedModNames.size())
                  {
                     models->Notify();
                  }
               }                              
            }
            else
            {
               // Creating a new expression.
               // Expression is allowed to have whitespace, so it may be
               // spread over many arguments.  Assume everything up to
               // the first encountered ":" is part of an expression.
               // If expression is quoted for some reason, it's OK if
               // it has whitespace inside.  MathExpression will remove
               // all whitespace upon construction.
               string expression;
               string compType("add");
               string::size_type breakPos = string::npos;
               string argRemainder;
               size_t iArg = 2;
               pair<Real,Real> eLimits(1.0e-20,1.0e20);
               while (iArg < nArgs)
               {
                  string testExpr(rawArgs[iArg]);
                  breakPos = testExpr.find(":");
                  if (breakPos != string::npos)
                  {
                     expression += testExpr.substr(0, breakPos);
                     argRemainder = testExpr.substr(breakPos+1);
                     break;
                  }
                  else
                     expression += testExpr;
                  ++iArg;
               }
               if (breakPos != string::npos)
               {
                  // Still have more to go... possible type info 
                  // and energy limits.
                  vector<string> extraArgs;
                  IntegerArray iParams;
                  vector<string> extraParams;
                  if (argRemainder.length())
                     extraArgs.push_back(argRemainder);
                  ++iArg;
                  while (iArg < nArgs)
                  {
                     extraArgs.push_back(rawArgs[iArg]);
                     ++iArg;
                  }
                  XSparse::collectParams(extraArgs, iParams, extraParams);
                  for (size_t i=0; i<iParams.size(); ++i)
                  {
                     istringstream iss(extraParams[i]);
                     switch (iParams[i])
                     {
                        case 0: // component type
                           {
                              string typeArg = XSutility::lowerCase(extraParams[i]);
                              bool isFound = false;
                              for (size_t j=0; !isFound && j<componentTypes.size(); ++j)
                                 if (typeArg == componentTypes[j])
                                    isFound = true;
                              if (isFound)
                                 compType = typeArg;
                              else
                              {
                                 tcerr << "Unrecognized type of mdefine model: "
                                    << typeArg << endl;
                                 tcerr << "Valid types are: ";
                                 for (size_t j=0; j<componentTypes.size(); ++j)
                                    tcerr << componentTypes[j] <<" ";
                                 tcerr << endl;
                                 return -1;
                              }

                           }
                           break;
                        case 1: // e min
                           {
                              Real testVal = .0;
                              if (!(iss >> testVal) || !iss.eof() || testVal < .0)
                              {
                                 tcerr << "Invalid emin value will be ignored: " 
                                        << extraParams[i] << endl;
                              }
                              else
                                 eLimits.first = testVal;
                           }
                           break;
                        case 2: // e max
                           {
                              Real testVal = .0;
                              if (!(iss >> testVal) || !iss.eof() || testVal < .0)
                              {
                                 tcerr << "Invalid emax value will be ignored: " 
                                        << extraParams[i] << endl;
                              }
                              else
                                 eLimits.second = testVal;
                           }
                           break;
                        default:
                           break;
                     }
                  } // end extra args loop
                  if (eLimits.first > eLimits.second)
                  {
                     Real tmp = eLimits.first;
                     eLimits.first = eLimits.second;
                     eLimits.second = tmp;
                     tcout << "In order to keep emin < emax, the input emin, emax have been reversed."
                        << " ("<<eLimits.first<<","<<eLimits.second<<")" << endl;
                  }

               } // end if ":" was found

               try
               {
                  MdefContainer::CompPair doomedComp(mdefs->getComponent(arg1));
                  if (doomedComp.first != MdefContainer::NOT_FOUND())
                  {
                     vector<string> doomedModNames;
                     findModelsUsing(doomedComp.second, doomedModNames);
                     if (doomedModNames.size())
                     {
                        string prompt("This will overwrite a currently existing mdefine model component,\n");
                        prompt += "  and will delete the models which are using it. Continue (y/n)? ";
                        int ans = XSutility::yesToQuestion(prompt, 0, tcin);
                        if (ans <= 0)
                           return 0;
                        for (size_t i=0; i<doomedModNames.size(); ++i)
                        {
                           vector<Model*> doomedMods(models->lookupModelGroup(doomedModNames[i]));
                           models->rerouteBrokenParLinks(doomedMods);
                           models->remove(doomedModNames[i]);                     
                        }
                     }
                     mdefs->removeComponent(arg1);
                     // This may throw
                     mdefs->addComponent(arg1, compType, expression, eLimits);
                     tcout << "A previously existing mdefine model component is now replaced."
                        << endl;
                     if (doomedModNames.size())
                        models->Notify();
                  }
                  else
                     // This may throw in oh so many ways.
                     mdefs->addComponent(arg1, compType, expression, eLimits);
               }
               catch (YellowAlert&)
               {
                  return -1;
               }
            } // end if creating a new expression
         } // end if nArgs > 2                  
      } // end if not "?"
   }  
   return 0;
}


namespace {
   void findModelsUsing(XSCall<MathExpression>* comp, std::vector<string>& modNames)
   {
      using namespace std;
      using namespace XSContainer;
      map<string,bool>::const_iterator itModName = 
            models->activeModelNames().begin();
      map<string,bool>::const_iterator itModNameEnd =
            models->activeModelNames().end();
      while (itModName != itModNameEnd)
      {
         vector<Model*> mods(models->lookupModelGroup(itModName->first));
         if (mods.size() && mods[0]->usingMdef(comp))
            modNames.push_back(itModName->first);
         ++itModName;
      }
   }
}
