//
// Code auto-generated by cxsetup (XSPEC12 local 
// model package code generator).  Do not edit.
// Package: 
// Function body: functionMap.cxx

#include    "functionMap.h"

#include    <XSModelFunction.h>

void 
createFunctionMap()
{


	XSFunctionMap["agauss"]   = new XSCall<XSCCall>(agauss);
	XSFunctionMap["apec"]     = new XSCall<XSCCall>(apec);
	XSFunctionMap["bapec"]    = new XSCall<XSCCall>(bapec);
	XSFunctionMap["btapec"]   = new XSCall<XSCCall>(btapec);
	XSFunctionMap["bbody"]    = new XSCall<xsf77Call>(xsblbd_);
	XSFunctionMap["bbodyrad"] = new XSCall<xsf77Call>(xsbbrd_);
	XSFunctionMap["bexrav"]   = new XSCall<XSCCall>(xsbexrav);
	XSFunctionMap["bexriv"]   = new XSCall<XSCCall>(xsbexriv);
	XSFunctionMap["bknpower"] = new XSCall<XSCCall>(brokenPowerLaw);
	XSFunctionMap["bkn2pow"]  = new XSCall<XSCCall>(broken2PowerLaw);
	XSFunctionMap["bmc"]      = new XSCall<xsf77Call>(xsbmc_);
	XSFunctionMap["bremss"]   = new XSCall<xsf77Call>(xsbrms_);
	XSFunctionMap["bvapec"]   = new XSCall<XSCCall>(bvapec);
	XSFunctionMap["bvtapec"]  = new XSCall<XSCCall>(bvtapec);
	XSFunctionMap["bvvapec"]  = new XSCall<XSCCall>(bvvapec);
	XSFunctionMap["bvvtapec"] = new XSCall<XSCCall>(bvvtapec);
	XSFunctionMap["c6mekl"]   = new XSCall<xsf77Call>(c6mekl_);
	XSFunctionMap["c6pmekl"]  = new XSCall<xsf77Call>(c6pmekl_);
	XSFunctionMap["c6pvmkl"]  = new XSCall<xsf77Call>(c6pvmkl_);
	XSFunctionMap["c6vmekl"]  = new XSCall<xsf77Call>(c6vmekl_);
	XSFunctionMap["carbatm"]  = new XSCall<XSCCall>(carbatm);
	XSFunctionMap["cemekl"]   = new XSCall<xsf77Call>(cemekl_);
	XSFunctionMap["cevmkl"]   = new XSCall<XSCCall>(cemVMekal);
	XSFunctionMap["cflow"]    = new XSCall<XSCCall>(xscflw);
	XSFunctionMap["compbb"]   = new XSCall<xsf77Call>(compbb_);
	XSFunctionMap["compmag"]  = new XSCall<xsccCall>(xscompmag);
	XSFunctionMap["compLS"]   = new XSCall<xsf77Call>(compls_);
	XSFunctionMap["compPS"]   = new XSCall<XSCCall>(xscompps);
	XSFunctionMap["compST"]   = new XSCall<xsf77Call>(compst_);
	XSFunctionMap["comptb"]   = new XSCall<xsccCall>(xscomptb);
	XSFunctionMap["compth"]   = new XSCall<XSCCall>(xscompth);
	XSFunctionMap["compTT"]   = new XSCall<xsf77Call>(xstitg_);
	XSFunctionMap["cplinear"] = new XSCall<XSCCall>(cplinear);
	XSFunctionMap["cutoffpl"] = new XSCall<XSCCall>(cutoffPowerLaw);
	XSFunctionMap["disk"]     = new XSCall<xsf77Call>(disk_);
	XSFunctionMap["diskir"]   = new XSCall<xsf77Call>(diskir_);
	XSFunctionMap["diskbb"]   = new XSCall<xsf77Call>(xsdskb_);
	XSFunctionMap["diskline"] = new XSCall<xsf77Call>(xsdili_);
	XSFunctionMap["diskm"]    = new XSCall<xsf77Call>(diskm_);
	XSFunctionMap["disko"]    = new XSCall<xsf77Call>(disko_);
	XSFunctionMap["diskpbb"]  = new XSCall<xsf77Call>(diskpbb_);
	XSFunctionMap["diskpn"]   = new XSCall<xsf77Call>(xsdiskpn_);
	XSFunctionMap["eplogpar"] = new XSCall<xsf77Call>(eplogpar_);
	XSFunctionMap["eqpair"]   = new XSCall<XSCCall>(xseqpair);
	XSFunctionMap["eqtherm"]  = new XSCall<XSCCall>(xseqth);
	XSFunctionMap["equil"]    = new XSCall<XSCCall>(equil);
	XSFunctionMap["expdec"]   = new XSCall<xsf77Call>(xsxpdec_);
	XSFunctionMap["ezdiskbb"] = new XSCall<xsf77Call>(ezdiskbb_);
	XSFunctionMap["gaussian"] = new XSCall<XSCCall>(gaussianLine);
	XSFunctionMap["gadem"]    = new XSCall<XSCCall>(gaussDem);
	XSFunctionMap["gnei"]     = new XSCall<XSCCall>(gnei);
	XSFunctionMap["grad"]     = new XSCall<xsf77Call>(grad_);
	XSFunctionMap["grbm"]     = new XSCall<xsf77Call>(xsgrbm_);
	XSFunctionMap["hatm"]     = new XSCall<XSCCall>(hatm);
	XSFunctionMap["kerrbb"]   = new XSCall<XSCCall>(kerrbb);
	XSFunctionMap["kerrd"]    = new XSCall<XSCCall>(kerrdisk);
	XSFunctionMap["kerrdisk"] = new XSCall<xsf77Call>(spin_);
	XSFunctionMap["laor"]     = new XSCall<XSCCall>(xslaor);
	XSFunctionMap["laor2"]    = new XSCall<XSCCall>(laor2);
	XSFunctionMap["logpar"]   = new XSCall<xsf77Call>(logpar_);
	XSFunctionMap["lorentz"]  = new XSCall<XSCCall>(lorentzianLine);
	XSFunctionMap["meka"]     = new XSCall<XSCCall>(meka);
	XSFunctionMap["mekal"]    = new XSCall<XSCCall>(mekal);
	XSFunctionMap["mkcflow"]  = new XSCall<XSCCall>(xsmkcf);
	XSFunctionMap["nei"]      = new XSCall<XSCCall>(nei);
	XSFunctionMap["nlapec"]   = new XSCall<XSCCall>(nlapec);
	XSFunctionMap["npshock"]  = new XSCall<XSCCall>(npshock);
	XSFunctionMap["nsa"]      = new XSCall<xsf77Call>(nsa_);
	XSFunctionMap["nsagrav"]  = new XSCall<xsf77Call>(nsagrav_);
	XSFunctionMap["nsatmos"]  = new XSCall<xsf77Call>(nsatmos_);
	XSFunctionMap["nsmax"]    = new XSCall<xsf77Call>(nsmax_);
	XSFunctionMap["nsmaxg"]   = new XSCall<xsf77Call>(nsmaxg_);
	XSFunctionMap["nsx"]      = new XSCall<xsf77Call>(nsx_);
	XSFunctionMap["nteea"]    = new XSCall<XSCCall>(xsnteea);
	XSFunctionMap["nthComp"]  = new XSCall<XSCCall>(nthcomp);
	XSFunctionMap["optxagn"]  = new XSCall<xsf77Call>(optxagn_);
	XSFunctionMap["optxagnf"] = new XSCall<xsf77Call>(optxagnf_);
	XSFunctionMap["pegpwrlw"] = new XSCall<xsf77Call>(xspegp_);
	XSFunctionMap["pexmon"]   = new XSCall<xsf77Call>(pexmon_);
	XSFunctionMap["pexrav"]   = new XSCall<XSCCall>(xspexrav);
	XSFunctionMap["pexriv"]   = new XSCall<XSCCall>(xspexriv);
	XSFunctionMap["plcabs"]   = new XSCall<xsf77Call>(xsp1tr_);
	XSFunctionMap["powerlaw"] = new XSCall<XSCCall>(powerLaw);
	XSFunctionMap["posm"]     = new XSCall<xsf77Call>(xsposm_);
	XSFunctionMap["pshock"]   = new XSCall<XSCCall>(pshock);
	XSFunctionMap["raymond"]  = new XSCall<XSCCall>(raysmith);
	XSFunctionMap["redge"]    = new XSCall<xsf77Call>(xredge_);
	XSFunctionMap["refsch"]   = new XSCall<xsf77Call>(xsrefsch_);
	XSFunctionMap["rnei"]     = new XSCall<XSCCall>(rnei);
	XSFunctionMap["sedov"]    = new XSCall<XSCCall>(sedov);
	XSFunctionMap["sirf"]     = new XSCall<XSCCall>(sirf);
	XSFunctionMap["slimbh"]   = new XSCall<xsccCall>(slimbbmodel);
	XSFunctionMap["smaug"]    = new XSCall<xsccCall>(xsmaug);
	XSFunctionMap["snapec"]   = new XSCall<XSCCall>(snapec);
	XSFunctionMap["srcut"]    = new XSCall<xsf77Call>(srcut_);
	XSFunctionMap["sresc"]    = new XSCall<xsf77Call>(sresc_);
	XSFunctionMap["step"]     = new XSCall<xsf77Call>(xsstep_);
	XSFunctionMap["tapec"]    = new XSCall<XSCCall>(tapec);
	XSFunctionMap["vapec"]    = new XSCall<XSCCall>(vapec);
	XSFunctionMap["vbremss"]  = new XSCall<xsf77Call>(xsbrmv_);
	XSFunctionMap["vequil"]   = new XSCall<XSCCall>(vequil);
	XSFunctionMap["vgadem"]   = new XSCall<XSCCall>(vgaussDem);
	XSFunctionMap["vgnei"]    = new XSCall<XSCCall>(vgnei);
	XSFunctionMap["vmeka"]    = new XSCall<XSCCall>(vmeka);
	XSFunctionMap["vmekal"]   = new XSCall<XSCCall>(vmekal);
	XSFunctionMap["vmcflow"]  = new XSCall<XSCCall>(xsvmcf);
	XSFunctionMap["vnei"]     = new XSCall<XSCCall>(vnei);
	XSFunctionMap["vnpshock"] = new XSCall<XSCCall>(vnpshock);
	XSFunctionMap["voigt"]    = new XSCall<XSCCall>(voigtLine);
	XSFunctionMap["vpshock"]  = new XSCall<XSCCall>(vpshock);
	XSFunctionMap["vraymond"] = new XSCall<XSCCall>(vraysmith);
	XSFunctionMap["vrnei"]    = new XSCall<XSCCall>(vrnei);
	XSFunctionMap["vsedov"]   = new XSCall<XSCCall>(vsedov);
	XSFunctionMap["vtapec"]   = new XSCall<XSCCall>(vtapec);
	XSFunctionMap["vvapec"]   = new XSCall<XSCCall>(vvapec);
	XSFunctionMap["vvgnei"]   = new XSCall<XSCCall>(vvgnei);
	XSFunctionMap["vvnei"]    = new XSCall<XSCCall>(vvnei);
	XSFunctionMap["vvnpshock"] = new XSCall<XSCCall>(vvnpshock);
	XSFunctionMap["vvpshock"] = new XSCall<XSCCall>(vvpshock);
	XSFunctionMap["vvrnei"]   = new XSCall<XSCCall>(vvrnei);
	XSFunctionMap["vvsedov"]  = new XSCall<XSCCall>(vvsedov);
	XSFunctionMap["vvtapec"]  = new XSCall<XSCCall>(vvtapec);
	XSFunctionMap["zagauss"]  = new XSCall<XSCCall>(zagauss);
	XSFunctionMap["zbbody"]   = new XSCall<xsf77Call>(xszbod_);
	XSFunctionMap["zbremss"]  = new XSCall<xsf77Call>(xszbrm_);
	XSFunctionMap["zgauss"]   = new XSCall<XSCCall>(xszgau);
	XSFunctionMap["zpowerlw"] = new XSCall<XSCCall>(zpowerLaw);
	XSFunctionMap["absori"]   = new XSCall<XSCCall>(xsabsori);
	XSFunctionMap["acisabs"]  = new XSCall<xsf77Call>(acisabs_);
	XSFunctionMap["constant"] = new XSCall<xsf77Call>(xscnst_);
	XSFunctionMap["cabs"]     = new XSCall<xsf77Call>(xscabs_);
	XSFunctionMap["cyclabs"]  = new XSCall<xsf77Call>(xscycl_);
	XSFunctionMap["dust"]     = new XSCall<xsf77Call>(xsdust_);
	XSFunctionMap["edge"]     = new XSCall<xsf77Call>(xsedge_);
	XSFunctionMap["expabs"]   = new XSCall<xsf77Call>(xsabsc_);
	XSFunctionMap["expfac"]   = new XSCall<xsf77Call>(xsexp_);
	XSFunctionMap["gabs"]     = new XSCall<XSCCall>(gaussianAbsorptionLine);
	XSFunctionMap["heilin"]   = new XSCall<xsf77Call>(xsphei_);
	XSFunctionMap["highecut"] = new XSCall<xsf77Call>(xshecu_);
	XSFunctionMap["hrefl"]    = new XSCall<xsf77Call>(xshrfl_);
	XSFunctionMap["ismabs"]   = new XSCall<xsf77Call>(ismabs_);
	XSFunctionMap["lyman"]    = new XSCall<xsf77Call>(xslyman_);
	XSFunctionMap["notch"]    = new XSCall<xsf77Call>(xsntch_);
	XSFunctionMap["pcfabs"]   = new XSCall<xsf77Call>(xsabsp_);
	XSFunctionMap["phabs"]    = new XSCall<xsf77Call>(xsphab_);
	XSFunctionMap["plabs"]    = new XSCall<xsf77Call>(xsplab_);
	XSFunctionMap["pwab"]     = new XSCall<XSCCall>(xspwab);
	XSFunctionMap["redden"]   = new XSCall<xsf77Call>(xscred_);
	XSFunctionMap["smedge"]   = new XSCall<xsf77Call>(xssmdg_);
	XSFunctionMap["spexpcut"] = new XSCall<XSCCall>(superExpCutoff);
	XSFunctionMap["spline"]   = new XSCall<xsf77Call>(xsspln_);
	XSFunctionMap["SSS_ice"]  = new XSCall<xsf77Call>(xssssi_);
	XSFunctionMap["swind1"]   = new XSCall<xsf77Call>(swind1_);
	XSFunctionMap["TBabs"]    = new XSCall<XSCCall>(tbabs);
	XSFunctionMap["TBfeo"]    = new XSCall<XSCCall>(tbfeo);
	XSFunctionMap["TBgas"]    = new XSCall<XSCCall>(tbgas);
	XSFunctionMap["TBgrain"]  = new XSCall<XSCCall>(tbgrain);
	XSFunctionMap["TBvarabs"] = new XSCall<XSCCall>(tbvabs);
	XSFunctionMap["TBpcf"]    = new XSCall<XSCCall>(tbpcf);
	XSFunctionMap["TBrel"]    = new XSCall<XSCCall>(tbrel);
	XSFunctionMap["uvred"]    = new XSCall<xsf77Call>(xsred_);
	XSFunctionMap["varabs"]   = new XSCall<xsf77Call>(xsabsv_);
	XSFunctionMap["vphabs"]   = new XSCall<xsf77Call>(xsvphb_);
	XSFunctionMap["wabs"]     = new XSCall<xsf77Call>(xsabsw_);
	XSFunctionMap["wndabs"]   = new XSCall<xsf77Call>(xswnab_);
	XSFunctionMap["xion"]     = new XSCall<xsf77Call>(xsxirf_);
	XSFunctionMap["xscat"]    = new XSCall<XSCCall>(xscatmodel);
	XSFunctionMap["zbabs"]    = new XSCall<xsccCall>(xszbabs);
	XSFunctionMap["zdust"]    = new XSCall<xsf77Call>(mszdst_);
	XSFunctionMap["zedge"]    = new XSCall<xsf77Call>(xszedg_);
	XSFunctionMap["zhighect"] = new XSCall<xsf77Call>(xszhcu_);
	XSFunctionMap["zigm"]     = new XSCall<xsf77Call>(zigm_);
	XSFunctionMap["zpcfabs"]  = new XSCall<xsf77Call>(xszabp_);
	XSFunctionMap["zphabs"]   = new XSCall<xsf77Call>(xszphb_);
	XSFunctionMap["zxipcf"]   = new XSCall<xsf77Call>(zxipcf_);
	XSFunctionMap["zredden"]  = new XSCall<xsf77Call>(xszcrd_);
	XSFunctionMap["zsmdust"]  = new XSCall<xsf77Call>(msldst_);
	XSFunctionMap["zTBabs"]   = new XSCall<XSCCall>(ztbabs);
	XSFunctionMap["zvarabs"]  = new XSCall<xsf77Call>(xszvab_);
	XSFunctionMap["zvfeabs"]  = new XSCall<xsf77Call>(xszvfe_);
	XSFunctionMap["zvphabs"]  = new XSCall<xsf77Call>(xszvph_);
	XSFunctionMap["zwabs"]    = new XSCall<xsf77Call>(xszabs_);
	XSFunctionMap["zwndabs"]  = new XSCall<xsf77Call>(xszwnb_);
	XSFunctionMap["cflux"]    = new XSCall<XSCCall>(cflux);
	XSFunctionMap["clumin"]   = new XSCall<XSCCall>(clumin);
	XSFunctionMap["cpflux"]   = new XSCall<XSCCall>(cpflux);
	XSFunctionMap["gsmooth"]  = new XSCall<XSCCall>(xsgsmt);
	XSFunctionMap["ireflect"] = new XSCall<XSCCall>(ireflct);
	XSFunctionMap["kdblur"]   = new XSCall<XSCCall>(kdblur);
	XSFunctionMap["kdblur2"]  = new XSCall<XSCCall>(kdblur2);
	XSFunctionMap["kerrconv"] = new XSCall<XSCCall>(spinconv);
	XSFunctionMap["lsmooth"]  = new XSCall<XSCCall>(xslsmt);
	XSFunctionMap["partcov"]  = new XSCall<XSCCall>(PartialCovering);
	XSFunctionMap["rdblur"]   = new XSCall<XSCCall>(rdblur);
	XSFunctionMap["reflect"]  = new XSCall<XSCCall>(reflct);
	XSFunctionMap["rfxconv"]  = new XSCall<XSCCall>(rfxconv);
	XSFunctionMap["rgsxsrc"]  = new XSCall<xsf77Call>(rgsxsrc_);
	XSFunctionMap["simpl"]    = new XSCall<XSCCall>(simpl);
	XSFunctionMap["vashift"]  = new XSCall<XSCCall>(vashift);
	XSFunctionMap["vmshift"]  = new XSCall<XSCCall>(vmshift);
	XSFunctionMap["xilconv"]  = new XSCall<XSCCall>(xilconv);
	XSFunctionMap["zashift"]  = new XSCall<XSCCall>(zashift);
	XSFunctionMap["zmshift"]  = new XSCall<XSCCall>(zmshift);
	XSFunctionMap["pileup"]   = new XSCall<XSCCall>(pileup);

}

void
clearFunctionMap()
{
   ModelFunctionMap::iterator itFm = XSFunctionMap.begin();
   ModelFunctionMap::iterator itFmEnd = XSFunctionMap.end();
   while (itFm != itFmEnd)
   {
      delete itFm->second;
      ++itFm;
   }
   XSFunctionMap.clear();
}

