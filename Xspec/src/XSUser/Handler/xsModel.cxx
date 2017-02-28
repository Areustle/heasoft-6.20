//
//
#include <XSModel/GlobalContainer/ModelContainer.h>
#include <XSModel/GlobalContainer/DataContainer.h>
#include <XSModel/Model/Model.h>
#include <XSModel/Model/Component/Component.h>
#include <XSModel/Parameter/Parameter.h> 
#include <XSUser/Global/XSGlobal.h>
#include <XSUser/Global/Global.h>
#include <XSUser/Handler/HandlerUtils.h>
#include <XSUser/Handler/XSinterface.h>     
#include <XSUtil/Parse/XSparse.h>
#include <XSUtil/FunctionUtils/XSModelFunction.h>
#include <XSFit/Fit/Fit.h>
#include <XSContainer.h>
#include <XSstreams.h>
#include <xsTypes.h>

int
XSGlobal::xsModel (ClientData cdata,Tcl_Interp* tclInterp,int objc, Tcl_Obj* CONST  objv[])
{
   StringArray rawArgs;
   HandlerUtils::tclArgsToCpp(objc, objv, rawArgs);
   int status = doModel(rawArgs);
   if (!status)
      return globalData->autoSave(TCL_OK);
   else
      return globalData->autoSave(TCL_ERROR);
}

int XSGlobal::doModel(const StringArray& rawArgs)
{
   using namespace XSContainer;
   static const string NONE("none");
   static const string CLEAR("clear");
   static const string ACTIVE("act");
   static const string INACTIVE("inact");
   using XSContainer::models;
   using XSContainer::datasets;

   const size_t nArgs = rawArgs.size();
   string cmdStr;

   if (nArgs == 1 )
   {
      // if there's no arguments or the argument that exists is blank
      // print list of valid models and return.
      XSModelFunction::printComponentList(tcout);
      return 0;                
   }
   else
   {

      // check for commands that erase previously
      // defined models.
      string actualFirstArg(rawArgs[1]);
      string lFirstArg(XSutility::lowerCase(actualFirstArg));
      bool patchedTableName = false;

      if ( lFirstArg.substr(0,5) == CLEAR )
      {
         // remove all models.
         models->clearLists();
	 models->Notify();
         return 0;
      }
      else if  (lFirstArg[0] == '?')
      {
         XSModelFunction::printComponentList(tcout);
         return 0;                 
      }
      else if ( lFirstArg.substr(0,4) == XSparse::NONE() )
      {
         // model none: remove default model
         std::vector<Model*> doomedMods(models->lookupModelGroup());
         models->rerouteBrokenParLinks(doomedMods);
         models->remove(Model::DEFAULT()); 
	 models->Notify();      
         return 0;                
      }
      else if ( nArgs >= 3 )
      {
         string modName = actualFirstArg;
         if (lFirstArg == "unnamed")
            modName = Model::DEFAULT();                       

         if ( Model* currMod = models->lookup(modName) )
         {
            // check for model <name> none: remove named model
            // check for model <name> active: make active
            // check for model <name> inactive: make inactive.
            string secondArg(XSutility::lowerCase(rawArgs[2]));
            if ( secondArg.substr(0,4) == XSparse::NONE())
            {
               std::vector<Model*> doomedMods(models->lookupModelGroup(modName));
               models->rerouteBrokenParLinks(doomedMods);
               models->remove(modName);
	       models->Notify(); 
            }
            else if ( secondArg.substr(0,3) == ACTIVE)
            {
               // If reached here, modName should be in
               // activeModelNames map.
               if (models->activeModelNames(modName))
               {
                  tcout << "\nModel is already active" << std::endl;
                  return 0;
               }
               else
               {
                  if (currMod->sourceNumber() > 
                      XSContainer::datasets->numSourcesForSpectra())
                  {
                     tcout << "\nUnable to activate model, no corresponding source in data."
                           << std::endl;
                     return -1;
                  }
                  models->designateActive(currMod);
                  datasets->Notify();
               }
            }  
            else if ( secondArg.substr(0,5) == INACTIVE)
            {
               if (models->activeModelNames(modName))
               {
                    models->deactivateModel(modName);
                    models->Notify();
               }
               else
               {
                  tcout << "\nModel is already marked inactive."<<std::endl;
                  return -1;
               }
            }
            else 
            {
               // print out the models with the input name
               std::vector<Model*> modGroup = models->lookupModelGroup(modName);
               modGroup[0]->printHeading();
               for (size_t j = 0; j < modGroup.size(); ++j)
               {
                  tcout  << *modGroup[j] << std::endl;      
               }
               modGroup[0]->printMixComp();
               tcout << string(72,'_') << '\n' << std::endl;    
            }
	    return 0;
         } // end if model exists
         else 
         {
            // Need to check for old-style syntax of "atable|mtable <file>".
            // If found, convert to "atable|mtable{<file>}".
            const string ATABLE("atable");
            const string MTABLE("mtable");
            if (ATABLE.find(lFirstArg) == 0)
            {
               actualFirstArg = ATABLE;
               patchedTableName = true;
            }
            else if (MTABLE.find(lFirstArg) == 0)
            {
               actualFirstArg = MTABLE;
               patchedTableName = true;
            }
            if (patchedTableName)
            {
               actualFirstArg += "{" + rawArgs[2] + "}";
            }                      
         }  
      } // end if nArgs >= 3

      // okay, we actually are creating rather than deleting 
      // models. parse into command and "batch" information,
      // supplied to parameter setting functions.
      string cmdLine = actualFirstArg;
      for (size_t j= patchedTableName ? 3 : 2; j < nArgs; ++j) 
      {
          cmdLine += " ";
          cmdLine += rawArgs[j];
      }

      std::deque<string> parameterStrings;
      XSparse::findBatchString(cmdLine,cmdStr,parameterStrings);

      string modelName("");
      string commandArgs("");
      size_t sourceNum (1);


      // first, split off the model string
      // syntax is model modelName:{sourceNum} <model definition string>
      // sourceNum is optional assignment to source.
      // create a temporary empty model and add it to the list.
      // modify model constructor if necessary to take a name as an
      // argument that has a default value ($DEFAULT).
      // parse the part of the string between the command name and the ":"
      // as the model Name.
      //...
      //...
      // if there is a numeric value between the ":"  and the next string token
      // [no space after the ":" allowed!], take it as a source number.
      // if the user typed an alpha value here, reject as syntax error.
      //...
      //...


      // define this here so copies can be deleted on an exception.
      Model* newModel (0);
      std::vector<Model*> dataGroupCopies; 
      bool added(false);
      try 
      {
         XSparse::parseInputModelString(cmdStr,modelName,sourceNum,commandArgs);

         // potential memory leak in presence of exception: design
         // needs to be reconsidered a little.

	 if (sourceNum > std::max(static_cast<size_t>(1),
			 XSContainer::datasets->numSourcesForSpectra()))
	 {
	    throw Model::NoSourceForModel();
	 }

         if ( modelName.length() == 0 )
         {
            // change to exception //
            modelName = Model::DEFAULT();
            if (sourceNum != 1 ) 
            {
               string msg(" models for multiple source ");
               msg += "analysis must be named\n";
               throw YellowAlert(msg);    

            }
         }

         // Patch fix:  This section should really be placed in a function 
         // in XSUtils when it's time to release an interface change.  
         // It looks for any '{}' pairs and removes all whitespace in between.
         // The purpose is to make expression parsing of atable/mtable easier
         // further on, since we know in this context that there is no need
         // for any whitespace between curly braces.  It does not check for
         // weird things like unmatched or nested braces, which will 
         // presumably cause a throw later.
         string::size_type ipos=0;
         while (ipos != string::npos && ipos < commandArgs.size())
         {
            string::size_type bpos = commandArgs.find_first_of('{',ipos);
            ipos = string::npos;
            if (bpos != string::npos)
            {
               string::size_type epos = commandArgs.find_first_of('}',bpos);
               if (epos != string::npos)
               {
                  string betweenCurls;
                  for (string::size_type jpos=bpos+1; jpos<epos; ++jpos)
                  {
                     char nws = commandArgs[jpos];
                     if (nws != ' ' && nws != '\t')
                        betweenCurls += nws;
                  }
                  commandArgs.replace(bpos+1, epos-bpos-1, betweenCurls);
                  // epos no longer points to '}', so find it again.
                  ipos = commandArgs.find_first_of('}',bpos);                  
               }
            }
         }    

         newModel = new Model(modelName,sourceNum,commandArgs);
         size_t nGroupsForSource = 0;
         std::map<size_t,std::map<size_t,size_t> >::const_iterator itStoG = 
                        datasets->sourceToDgs().find(sourceNum);
         std::vector<size_t> neededCopies;
         if (itStoG != datasets->sourceToDgs().end())
         {
            const std::map<size_t,size_t>& dGroups = itStoG->second;
            nGroupsForSource = dGroups.size();
            std::map<size_t,size_t>::const_iterator itGroups = dGroups.begin();
            std::map<size_t,size_t>::const_iterator itGroupsEnd = dGroups.end();
            if (itGroups != itGroupsEnd)
            {            
               // Assign this the lowest data group number for the source.
               newModel->dataGroupNumber(itGroups->first);
               ++itGroups;
            }
            while (itGroups != itGroupsEnd)
            {
               neededCopies.push_back(itGroups->first);
               ++itGroups;
            }
         }
         if (newModel->mixingLocations().first.size())
         {
            // At this point only check for the mere presence
            // of data.  If data is present, its validity will
            // have to be checked later with the MixBase object's
            // verifyData call.
            if (!datasets->dataArray().size())
            {
               string msg("Mixing model requires that data be loaded first\n");
               throw YellowAlert(msg);
            }
         }

         // process parameter strings here.

         bool promptUser(parameterStrings.empty());
         bool skipEntered(false);

         if (promptUser) 
         {
            // getParamValuesFromUser prompts only for the strings
            // related to the current model, not for the higher data groups.
            parameterStrings = newModel->getParamValuesFromUser();     

         }
         newModel->setParamValues(parameterStrings,skipEntered); 
         if (tpout.logChatterLevel() >= 25)
         {
            // debugging info
            tcout     <<  " Model: Input Parameter Definition Strings: \n"                                 
                      << " Model name: "   << newModel->name() 
                      << " data group: "   << newModel->dataGroupNumber() 
                      << " source number " << newModel->sourceNumber() 
                      << '\n';
            std::ostream_iterator<string> ss(tcout,"\n");
            std::copy(parameterStrings.begin(), parameterStrings.end(),ss);
         }
         // this has to be done here, rather than after all of the datagroup models
         // have been constructed. So for strong exception safety we need to remove this
         // if the data groups do not create correctly. But...  
	 models->setFirstOfModelName(newModel);
         models->addToList(newModel);
         added = true;
         // if there are higher data groups, then the parameter strings deque (set from
         // a script) will have the next needed string at the front.

         if (nGroupsForSource > 1) 
         {
            dataGroupCopies.resize(nGroupsForSource-1); 
            dataGroupCopies  = newModel->makeDataGroupCopies(neededCopies); 
            for (size_t j = 0; j < nGroupsForSource - 1; ++j)
            {
               if (skipEntered)
               {
                  dataGroupCopies[j]->linkDataGroupParams();
               }
               else
               {
                  if (promptUser)
                  {
                     parameterStrings 
                             = dataGroupCopies[j]->getParamValuesFromUser();  
                  }   

                  if (tpout.logChatterLevel() >= 25)
                  {
                     // debugging info
                     tcout     << " Model: Input Parameter Definition Strings: \n"                                 
                               << " Model name: "   
                               << dataGroupCopies[j]->name() 
                               << " data group: "   
                               << dataGroupCopies[j]->dataGroupNumber() 
                               << " source number " 
                               << dataGroupCopies[j]->sourceNumber() 
                               << '\n';
                     std::ostream_iterator<string> ss(tcout,"\n");
                     std::copy(parameterStrings.begin(), parameterStrings.end(),ss);
                  }                                                

                  dataGroupCopies[j]->setParamValues(parameterStrings,skipEntered);     
               }
               models->addToList(dataGroupCopies[j]);      
            }
         }

         const Real MAG_WARNING_LEVEL(1.0e10);
         int warnMagDataGroup=0;
         if(newModel->checkParameterMagnitudes(MAG_WARNING_LEVEL))
            warnMagDataGroup = newModel->dataGroupNumber();
         size_t iMod=0;
         while (!warnMagDataGroup && iMod < dataGroupCopies.size())
         {
            if(dataGroupCopies[iMod]->checkParameterMagnitudes(MAG_WARNING_LEVEL))
               warnMagDataGroup=dataGroupCopies[iMod]->dataGroupNumber();
            ++iMod;
         }

         // attach responses to the new models. If
         // no responses have been defined, this does nothing.
         models->attachResponses(modelName);

         // This needs to be called after responses are attached,
         // and does nothing if extended energies aren't in use.
         models->applyExtendedEnergies();

         // force computation of the models just created.
         models->setCompute(modelName);

         // This will create a MixUtility for any mix comps which need it
         //   (lowest dg only), and call its initialize function.
         models->initializeMixingTransformation(modelName);
         // If new model is active/on, it will be calculated 
         // through Fit::Update.  Else, do it here.  
         if (!nGroupsForSource)
         {
            // Note: for mixing models this can never occur, as it
            // has already been prevented above.
            std::vector<Component*> newComps;
            newModel->bundleComponents(newComps);
            for (size_t i=0; i<newComps.size(); ++i)
            {
               newComps[i]->initializeForFit();
            }
            models->calculate(modelName);
         }

         const size_t nShow = nGroupsForSource ? nGroupsForSource : 1;
         if (fit->renormType() != Fit::AUTO)
         {
            // If in Fit::AUTO mode, renormalizing will
            // take place during the Notify call below,
            // so let's not print out the params just yet.
            newModel->printHeading();                               
            for (size_t j = 0; j < nShow; ++j)
            {
               size_t nGroup = j ? neededCopies[j-1] : newModel->dataGroupNumber();
               tcout << *models->lookup(modelName,nGroup);   
            }
            newModel->printMixComp(); 
            tcout << string(72,'_') << '\n' << std::endl;
         }                                            
	 models->Notify();
         if (fit->renormType() == Fit::AUTO)
         {
            // OK, now let's print them out.
            tcout << "\nParameters have been automatically renormalized." 
                  << std::endl;
            newModel->printHeading();                               
            for (size_t j = 0; j < nShow; ++j)
            {
               size_t nGroup = j ? neededCopies[j-1] : newModel->dataGroupNumber();
               tcout << *models->lookup(modelName,nGroup);   
            }
            newModel->printMixComp(); 
            tcout << string(72,'_') << '\n' << std::endl; 
         }
         
         if (warnMagDataGroup)
         {
            tcout <<"\n***Warning: Magnitudes of parameters in model for data group " <<warnMagDataGroup
                <<"\n     have been found to differ by more than " <<MAG_WARNING_LEVEL<<".\n"
                <<"     This can lead to significant roundoff errors during fit calculation.\n"<<std::endl;       
         }
         
         return  0;
      }
      catch (YellowAlert&)
      {
         if (added) models->remove(modelName);      
         else delete newModel;
         return -1;
      }                      


   }
}
