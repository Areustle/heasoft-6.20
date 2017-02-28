#include <XSUser/Handler/XSinterface.h>

namespace XSGlobal
{
   std::map<string, stdInterface> stdInterfaceCmdMap;
}

void XSGlobal::createStdInterfaceCmdMap()
{
   stdInterfaceCmdMap["abund"] = &doAbund;
   stdInterfaceCmdMap["addcomp"] = &doAddcomp;
   stdInterfaceCmdMap["arf"] = &doArf;
   stdInterfaceCmdMap["autosave"] = &doAutosave;
   stdInterfaceCmdMap["backgrnd"] = &doBackgrnd;
   stdInterfaceCmdMap["bayes"] = &doBayes;
   stdInterfaceCmdMap["chain"] = &doChain;
   stdInterfaceCmdMap["comsum"] = &doComsum;
   stdInterfaceCmdMap["corfile"] = &doCorfile;
   stdInterfaceCmdMap["cornorm"] = &doCornorm;
   stdInterfaceCmdMap["cosmo"] = &doCosmo;
   stdInterfaceCmdMap["cpd"] = &doCpd;
   stdInterfaceCmdMap["delcomp"] = &doDelcomp;
   stdInterfaceCmdMap["diagrsp"] = &doDiagrsp;
   stdInterfaceCmdMap["dummyrsp"] = &doDummyrsp;
   stdInterfaceCmdMap["editmod"] = &doEditmod;
   stdInterfaceCmdMap["energies"] = &doEnergies;
   stdInterfaceCmdMap["eqwidth"] = &doEqwidth;
   stdInterfaceCmdMap["error"] = &doError;
   stdInterfaceCmdMap["fakeit"] = &doFakeit;
   stdInterfaceCmdMap["fit"] = &doFit;
   stdInterfaceCmdMap["flux"] = &doFlux;
   stdInterfaceCmdMap["freeze"] = &doFreeze;
   stdInterfaceCmdMap["ftest"] = &doFtest;
   stdInterfaceCmdMap["gain"] = &doGain;
   stdInterfaceCmdMap["goodness"] = &doGoodness;
   stdInterfaceCmdMap["help"] = &doHelp;
   stdInterfaceCmdMap["identify"] = &doIdentify;
   stdInterfaceCmdMap["ignore"] = &doIgnore;
   stdInterfaceCmdMap["improve"] = &doImprove;
   stdInterfaceCmdMap["initpackage"] = &doInitpackage;
   stdInterfaceCmdMap["iplot"] = &doIplot;
   stdInterfaceCmdMap["lmod"] = &doLmod;
   stdInterfaceCmdMap["margin"] = &doMargin;
   stdInterfaceCmdMap["mdefine"] = &doMdefine;
   stdInterfaceCmdMap["method"] = &doMethod;
   stdInterfaceCmdMap["model"] = &doModel;
   stdInterfaceCmdMap["notice"] = &doNotice;
   stdInterfaceCmdMap["parallel"] = &doParallel;
   stdInterfaceCmdMap["renorm"] = &doRenorm;
   stdInterfaceCmdMap["response"] = &doResponse;
   stdInterfaceCmdMap["rmodel"] = &doRmodel;
   stdInterfaceCmdMap["save"] = &doSave;
   stdInterfaceCmdMap["setplot"] = &doSetplot;
   stdInterfaceCmdMap["statistic"] = &doStatistic;
   stdInterfaceCmdMap["steppar"] = &doSteppar;
   stdInterfaceCmdMap["systematic"] = &doSystematic;
   stdInterfaceCmdMap["thaw"] = &doThaw;
   stdInterfaceCmdMap["time"] = &doTime;
   stdInterfaceCmdMap["untie"] = &doUntie;
   stdInterfaceCmdMap["version"] = &doVersion;
   stdInterfaceCmdMap["weight"] = &doWeight;
   stdInterfaceCmdMap["xsect"] = &doXsect;
   stdInterfaceCmdMap["xset"] = &doXset;
}
