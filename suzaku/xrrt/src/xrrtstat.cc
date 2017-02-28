// xrrtstat.cc
//
// Member definition for Xrrt photon statistics
// Richard L Fink GSFC/631
// 1997/08/11
// 1997/09/26 Upgrade documentation. R. Fink

// Use RCS (Revision Control System) by HIDEYUKI MORI
// Revision 1.1  2000/10/19 06:36:23  mori
// Initial revision


// 2013/08/31 Y.ISHISAKI	version 6.5.5
//	modify to (total.impactBySourceRadialBin.begin() + binNumber) in XrrtStat::getCumPhotonCntByRadius()
//	modify declaration of char* ttype/tform/tunit[] -> const char*

#include "xrrtstat.hh"

XrrtStat::resultTotals::resultTotals():
    energy(0),
    tracedPhotonCount(0),
    tracedWithinPsfCoreRadiusCount(0),
    impactOnFocalPlaneCount(0),
    fpOnePerLayerCount(0),
    fpOneLayerOnlyCount(0),
    fpLayerOneOnlyCount(0),
    fpLayerTwoOnlyCount(0),
    fpNoReflectionCount(0),
    fpAbnormalPathCount(0),
    impactOnFPMaskCount(0),
    fpMaskOnePerLayerCount(0),
    fpMaskOneLayerOnlyCount(0),
    fpMaskLayerOneOnlyCount(0),
    fpMaskLayerTwoOnlyCount(0),
    fpMaskNoReflectionCount(0),
    fpMaskAbnormalPathCount(0),
    impactOnMirrorTop(0),
    impactOnObstruction(0),
    impactOnOuterHousingCount(0),
    impactOnInnerHousingCount(0),
    absorbedOnFrontMirrorFaceCount(0),
    absorbedOnBackMirrorFaceCount(0),
    photonCanNotLeaveMirrorCount(0),
    photonErrorCount(0),
	impactOnCollimatorTop(0),
	absorbedOnFrontCollimatorFaceCount(0),
	absorbedOnBackCollimatorFaceCount(0),
	photonCanNotLeaveCollimatorCount(0),
    impactBySourceRadialBin(),
	psfRadialBin(),
    image()
{
}

XrrtStat::XrrtStat():
    XrrtStatVersion("XrrtStat_V2.1a"),
    destroyExistingFITSFiles(false),
    collectResultStat(false),
    outputResultStat(false),
    suppressEnergyResultRows(false),
    resultStatFileName(" "),
    resultStatTableName(" "),
    resultStatFile(0),
    resultStatRowsWritten(0),
    sourceFocalPlaneXCenterMM(0),
    sourceFocalPlaneYCenterMM(0),
    designFocalLengthMM(0),
    focalPlaneMM2Arcmin(0),
    psfCoreRadiusLimitRadian(0),
    areaInMMSqr(0),
    focalPlaneMask(),
    sourceRadialProfileBins(0),
    psfRadialProfileBins(0),
    sourceRadialBinsPerArcmin(0),
    psfRadialProfileBinsPerMM(0),
    outputPhotonHistory(false),
    photonHistoryFileName(" "),
    photonHistoryTableName(" "),
    photonHistoryFile(0),
    photonHistoryRowsWritten(0),
    collectImage(false),
    saveImageByEnergy(false),
    imageFileName(" "),
    imageTableName(" "),
    imageFile(0),
    xyAxisBins(512),
    xyAxisMinMM(-10.0),
    xyAxisMaxMM(10.0),
    nominalRAAtCenter(0),
    nominalDECAtCenter(0),
    statByEnergy(),
    total()
{
//
// A complex constructor
//
    //
    // DEFAULTS FOR SOURCE RADIAL PROFILES HERE
    // WARNING: DON'T CHANGE THESE. THESE ARE TIED TO ASSUMPTIONS ABOUT ASCA
    // AND ASTRO-E EFFECTIVE AREA AND POINT SPREAD FUNCTION. THEY ARE INHERITED
    // FROM DECISIONS MADE AT NAGOYA LONG AGO.
    //
    sourceRadialProfileBins = 501;
    sourceRadialBinsPerArcmin = 20;
    psfRadialProfileBins = 501;
    psfRadialProfileBinsPerMM = 20;

    //
    // Fill up the bins with valid data
    //
    for (int i = 0; i < sourceRadialProfileBins; i++)
        {
        statByEnergy.impactBySourceRadialBin.push_back(0);
        }
    //
    // Fill up the bins with valid data
    //
    for (int i = 0; i < sourceRadialProfileBins; i++)
        {
        total.impactBySourceRadialBin.push_back(0);
        }
    //
    // Fill up the bins with valid data
    //
    for (int i = 0; i < psfRadialProfileBins; i++)
        {
        statByEnergy.psfRadialBin.push_back(0);
        }
    //
    // Fill up the bins with valid data
    //
    for (int i = 0; i < psfRadialProfileBins; i++)
        {
        total.psfRadialBin.push_back(0);
        }
    // NOTE THAT WE CAN NOT CREATE THE IMAGE(S) UNTIL WE KNOW THEIR SIZE
}

XrrtStat::XrrtStat( const XrrtStat& stat ):
    XrrtStatVersion("XrrtStat_V2.1"),
    collectResultStat(false),
    outputResultStat(false),
    suppressEnergyResultRows(false),
    resultStatFileName(" "),
    resultStatTableName(" "),
    resultStatFile(0),
    resultStatRowsWritten(0),
    sourceFocalPlaneXCenterMM(0),
    sourceFocalPlaneYCenterMM(0),
    designFocalLengthMM(0),
    focalPlaneMM2Arcmin(0),
    psfCoreRadiusLimitRadian(0),
    areaInMMSqr(0),
    focalPlaneMask(),
    sourceRadialProfileBins(0),
    psfRadialProfileBins(0),
    sourceRadialBinsPerArcmin(0),
    psfRadialProfileBinsPerMM(0),
    outputPhotonHistory(false),
    photonHistoryFileName(" "),
    photonHistoryTableName(" "),
    photonHistoryFile(0),
    photonHistoryRowsWritten(0),
    collectImage(false),
    saveImageByEnergy(false),
    imageFileName(" "),
    imageTableName(" "),
    imageFile(0),
    xyAxisBins(512),
    xyAxisMinMM(-10.0),
    xyAxisMaxMM(10.0),
    nominalRAAtCenter(0),
    nominalDECAtCenter(0),
    statByEnergy(),
    total()
{
//
// A not so bad copy constructor
//
    collectResultStat = stat.collectResultStat;
    outputResultStat = stat.outputResultStat;
    suppressEnergyResultRows = stat.suppressEnergyResultRows;
    resultStatFileName = stat.resultStatFileName;
    resultStatTableName = stat.resultStatTableName;
    resultStatFile = stat.resultStatFile;
    resultStatRowsWritten = stat.resultStatRowsWritten;
    sourceFocalPlaneXCenterMM = stat.sourceFocalPlaneXCenterMM;
    sourceFocalPlaneYCenterMM = stat.sourceFocalPlaneYCenterMM;
    designFocalLengthMM = stat.designFocalLengthMM;
    focalPlaneMM2Arcmin = stat.focalPlaneMM2Arcmin;
    psfCoreRadiusLimitRadian = stat.psfCoreRadiusLimitRadian;
    areaInMMSqr = stat.areaInMMSqr;
    focalPlaneMask = stat.focalPlaneMask;
    sourceRadialProfileBins = stat.sourceRadialProfileBins;
    sourceRadialBinsPerArcmin = stat.sourceRadialBinsPerArcmin;
    psfRadialProfileBins = stat.psfRadialProfileBins;
    psfRadialProfileBinsPerMM = stat.psfRadialProfileBinsPerMM;

    outputPhotonHistory = stat.outputPhotonHistory;
    photonHistoryFileName = stat.photonHistoryFileName;
    photonHistoryTableName = stat.photonHistoryTableName;
    photonHistoryFile = stat.photonHistoryFile;
    photonHistoryRowsWritten = stat.photonHistoryRowsWritten;
    
    collectImage = stat.collectImage;
    saveImageByEnergy = stat.saveImageByEnergy;
    imageFileName = stat.imageFileName;
    imageTableName = stat.imageTableName;
    imageFile = stat.imageFile;
    xyAxisBins = stat.xyAxisBins;
    xyAxisMinMM = stat.xyAxisMinMM;
    xyAxisMaxMM = stat.xyAxisMaxMM;
    nominalRAAtCenter = stat.nominalRAAtCenter;
    nominalDECAtCenter= stat.nominalDECAtCenter;
    
    statByEnergy.energy = stat.statByEnergy.energy;
    statByEnergy.tracedPhotonCount = stat.statByEnergy.tracedPhotonCount;
    statByEnergy.tracedWithinPsfCoreRadiusCount = 
                               stat.statByEnergy.tracedWithinPsfCoreRadiusCount;
    statByEnergy.impactOnFocalPlaneCount = 
                                      stat.statByEnergy.impactOnFocalPlaneCount;
    statByEnergy.fpOnePerLayerCount = stat.statByEnergy.fpOnePerLayerCount;
    statByEnergy.fpOneLayerOnlyCount = stat.statByEnergy.fpOneLayerOnlyCount;
    statByEnergy.fpLayerOneOnlyCount = stat.statByEnergy.fpLayerOneOnlyCount;
    statByEnergy.fpLayerTwoOnlyCount = stat.statByEnergy.fpLayerTwoOnlyCount;
    statByEnergy.fpNoReflectionCount = stat.statByEnergy.fpNoReflectionCount;
    statByEnergy.fpAbnormalPathCount = stat.statByEnergy.fpAbnormalPathCount;
    statByEnergy.impactOnFPMaskCount = stat.statByEnergy.impactOnFPMaskCount;
    statByEnergy.fpMaskOnePerLayerCount = 
                                       stat.statByEnergy.fpMaskOnePerLayerCount;
    statByEnergy.fpMaskOneLayerOnlyCount = 
                                      stat.statByEnergy.fpMaskOneLayerOnlyCount;
    statByEnergy.fpMaskLayerOneOnlyCount = 
                                      stat.statByEnergy.fpMaskLayerOneOnlyCount;
    statByEnergy.fpMaskLayerTwoOnlyCount = 
                                      stat.statByEnergy.fpMaskLayerTwoOnlyCount;
    statByEnergy.fpMaskNoReflectionCount = 
                                      stat.statByEnergy.fpMaskNoReflectionCount;
    statByEnergy.fpMaskAbnormalPathCount = 
                                      stat.statByEnergy.fpMaskAbnormalPathCount;
    statByEnergy.impactOnMirrorTop = stat.statByEnergy.impactOnMirrorTop;
    statByEnergy.impactOnObstruction = stat.statByEnergy.impactOnObstruction;
    statByEnergy.impactOnOuterHousingCount = 
                                    stat.statByEnergy.impactOnOuterHousingCount;
    statByEnergy.impactOnInnerHousingCount = 
                                    stat.statByEnergy.impactOnInnerHousingCount;
    statByEnergy.absorbedOnFrontMirrorFaceCount = 
                               stat.statByEnergy.absorbedOnFrontMirrorFaceCount;
    statByEnergy.absorbedOnBackMirrorFaceCount = 
                                stat.statByEnergy.absorbedOnBackMirrorFaceCount;
    statByEnergy.photonCanNotLeaveMirrorCount = 
                                 stat.statByEnergy.photonCanNotLeaveMirrorCount;
    statByEnergy.photonErrorCount = stat.statByEnergy.photonErrorCount;
    copy(stat.statByEnergy.impactBySourceRadialBin.begin(),
         stat.statByEnergy.impactBySourceRadialBin.end(),
         statByEnergy.impactBySourceRadialBin.begin());
    copy(stat.statByEnergy.psfRadialBin.begin(),
         stat.statByEnergy.psfRadialBin.end(),
         statByEnergy.psfRadialBin.begin());
    for (int i = 0; i < (int) xyAxisBins; i++)
        {
        statByEnergy.image.push_back(stat.statByEnergy.image[i]);
        }
    total.energy = stat.total.energy;
    total.tracedPhotonCount = stat.total.tracedPhotonCount;
    total.tracedWithinPsfCoreRadiusCount = 
                                      stat.total.tracedWithinPsfCoreRadiusCount;
    total.impactOnFocalPlaneCount = stat.total.impactOnFocalPlaneCount;
    total.fpOnePerLayerCount = stat.total.fpOnePerLayerCount;
    total.fpOneLayerOnlyCount = stat.total.fpOneLayerOnlyCount;
    total.fpLayerOneOnlyCount = stat.total.fpLayerOneOnlyCount;
    total.fpLayerTwoOnlyCount = stat.total.fpLayerTwoOnlyCount;
    total.fpNoReflectionCount = stat.total.fpNoReflectionCount;
    total.fpAbnormalPathCount = stat.total.fpAbnormalPathCount;
    total.impactOnFPMaskCount = stat.total.impactOnFPMaskCount;
    total.fpMaskOnePerLayerCount = stat.total.fpMaskOnePerLayerCount;
    total.fpMaskOneLayerOnlyCount = stat.total.fpMaskOneLayerOnlyCount;
    total.fpMaskLayerOneOnlyCount = stat.total.fpMaskLayerOneOnlyCount;
    total.fpMaskLayerTwoOnlyCount = stat.total.fpMaskLayerTwoOnlyCount;
    total.fpMaskNoReflectionCount = stat.total.fpMaskNoReflectionCount;
    total.fpMaskAbnormalPathCount = stat.total.fpMaskAbnormalPathCount;
    total.impactOnMirrorTop = stat.total.impactOnMirrorTop;
    total.impactOnObstruction = stat.total.impactOnObstruction;
    total.impactOnOuterHousingCount = stat.total.impactOnOuterHousingCount;
    total.impactOnInnerHousingCount = stat.total.impactOnInnerHousingCount;
    total.absorbedOnFrontMirrorFaceCount = 
                                      stat.total.absorbedOnFrontMirrorFaceCount;
    total.absorbedOnBackMirrorFaceCount = 
                                       stat.total.absorbedOnBackMirrorFaceCount;
    total.photonCanNotLeaveMirrorCount = stat.total.photonCanNotLeaveMirrorCount;
    total.photonErrorCount = stat.total.photonErrorCount;
    copy(stat.total.impactBySourceRadialBin.begin(),
         stat.total.impactBySourceRadialBin.end(),
         total.impactBySourceRadialBin.begin());
    copy(stat.total.psfRadialBin.begin(),
         stat.total.psfRadialBin.end(),
         total.psfRadialBin.begin());
    for (int i = 0; i < (int) xyAxisBins; i++)
        {
        total.image.push_back(stat.total.image[i]);
        }
}

string
XrrtStat::errorMessage(XrrtStatErrorCode errorCode) const
{
//
// Convert error codes to error messages
//
string errorMessage;

    switch (errorCode)
        {
        case fitsFileNotOpen:
            errorMessage = 
            "Attempt to write to not-open FITS file in XrrtStat";
            break;
        case imageFileMayNotExist:
            errorMessage = 
            "You can not write an image to the primary array of an existing FITS file";
            break;
        case unknownPhotonStatus:
            errorMessage = 
            "New Photon Status created without XrrtStat being updated";
            break;
        case unknownPhotonReflectionClass:
            errorMessage = 
            "New Photon Reflection Class created without XrrtStat being updated";
            break;
        case psfCoreIsZero:
            errorMessage = "A PSF with zero photon counts was detected by calculatePSF()";
            break;
        default:
             char charNumber[1024];
             sprintf(charNumber, "%d",errorCode);
             errorMessage = "Unknown error code: ";
             errorMessage.append(charNumber);
             break;
        }
     return errorMessage;
}

void 
XrrtStat::startResultCollection()
{
//
// Do everything necessary to set up for Results Statistics recording:
// 1) Zero counters
// 2) Open FITS file for output
//

    //
    // Just in case we are called inappropriately
    //
    if (!collectResultStat)
       {
       return;
       }

    //
    // zero out the counters
    //
    statByEnergy.energy=0;
    statByEnergy.tracedPhotonCount=0;
    statByEnergy.tracedWithinPsfCoreRadiusCount=0;
    statByEnergy.impactOnFocalPlaneCount=0;
    statByEnergy.fpOnePerLayerCount=0;
    statByEnergy.fpOneLayerOnlyCount=0;
    statByEnergy.fpLayerOneOnlyCount=0;
    statByEnergy.fpLayerTwoOnlyCount=0;
    statByEnergy.fpNoReflectionCount=0;
    statByEnergy.fpAbnormalPathCount=0;
    statByEnergy.impactOnFPMaskCount=0;
    statByEnergy.fpMaskOnePerLayerCount=0;
    statByEnergy.fpMaskOneLayerOnlyCount=0;
    statByEnergy.fpMaskLayerOneOnlyCount=0;
    statByEnergy.fpMaskLayerTwoOnlyCount=0;
    statByEnergy.fpMaskNoReflectionCount=0;
    statByEnergy.fpMaskAbnormalPathCount=0;
    statByEnergy.impactOnMirrorTop=0;
    statByEnergy.impactOnObstruction=0;
    statByEnergy.impactOnOuterHousingCount=0;
    statByEnergy.impactOnInnerHousingCount=0;
    statByEnergy.absorbedOnFrontMirrorFaceCount=0;
    statByEnergy.absorbedOnBackMirrorFaceCount=0;
    statByEnergy.photonCanNotLeaveMirrorCount=0;
    statByEnergy.photonErrorCount=0;
    fill(statByEnergy.impactBySourceRadialBin.begin(),
         statByEnergy.impactBySourceRadialBin.end(), 0);
    fill(statByEnergy.psfRadialBin.begin(),
         statByEnergy.psfRadialBin.end(), 0);
    total.energy=0;
    total.tracedPhotonCount=0;
    total.tracedWithinPsfCoreRadiusCount=0;
    total.impactOnFocalPlaneCount=0;
    total.fpOnePerLayerCount=0;
    total.fpOneLayerOnlyCount=0;
    total.fpLayerOneOnlyCount=0;
    total.fpLayerTwoOnlyCount=0;
    total.fpNoReflectionCount=0;
    total.fpAbnormalPathCount=0;
    total.impactOnFPMaskCount=0;
    total.fpMaskOnePerLayerCount=0;
    total.fpMaskOneLayerOnlyCount=0;
    total.fpMaskLayerOneOnlyCount=0;
    total.fpMaskLayerTwoOnlyCount=0;
    total.fpMaskNoReflectionCount=0;
    total.fpMaskAbnormalPathCount=0;
    total.impactOnMirrorTop=0;
    total.impactOnObstruction=0;
    total.impactOnOuterHousingCount=0;
    total.impactOnInnerHousingCount=0;
    total.absorbedOnFrontMirrorFaceCount=0;
    total.absorbedOnBackMirrorFaceCount=0;
    total.photonCanNotLeaveMirrorCount=0;
    total.photonErrorCount=0;
    fill(total.impactBySourceRadialBin.begin(),
         total.impactBySourceRadialBin.end(), 0);
    fill(total.psfRadialBin.begin(),
         total.psfRadialBin.end(), 0);

    // 
    // It is possible (and necessary) for results statustics to be collected
    // without their being written to FITS.
    // This is all that must be done if the FITS output is not to be written
    if (!outputResultStat)
       {
       return;
       }

    //
    // Standard FITSIO calling sequence
    //
    int      fitsStatus;        // Standard error code return from FITSIO
    int      fitsReturn;        // Standard return type from a FITSIO function
    int      fitsHduType;       // Std FITS HDU type return
    long     fitsRow;

    //
    // Defaulted fits file format control
    //
    int tfields = 73;
    const char* ttype[73];
    const char* tform[73];
    const char* tunit[73];
    //
    // FITS file structure
    // If you modify this code, remember to modify writeResultStatEnergyRow()
    //  and writeResultStatTotalRow() or you will be sorry.
    //
    ttype[0] = "energy";
    tform[0] = "1E";
    tunit[0] = "keV";
    //
    ttype[1] = "photons";
    tform[1] = "1J";
    tunit[1] = " ";
    //
    ttype[2] = "corephtn";
    tform[2] = "1J";
    tunit[2] = " ";
    //
    ttype[3] = "corefrac";
    tform[3] = "1E";
    tunit[3] = " ";
    //
    ttype[4] = "corearea";
    tform[4] = "1E";
    tunit[4] = "mm**2";
    //
    ttype[5] = "fptotal";
    tform[5] = "1J";
    tunit[5] = " ";
    //
    ttype[6] = "fptotalf";
    tform[6] = "1E";
    tunit[6] = " ";
    //
    ttype[7] = "fptotala";
    tform[7] = "1E";
    tunit[7] = "mm**2";
    //
    ttype[8] = "fpopl";
    tform[8] = "1J";
    tunit[8] = " ";
    //
    ttype[9] = "fpoplf";
    tform[9] = "1E";
    tunit[9] = " ";
    //
    ttype[10] = "fpopla";
    tform[10] = "1E";
    tunit[10] = "mm**2";
    //
    ttype[11] = "fpolo";
    tform[11] = "1J";
    tunit[11] = " ";
    //
    ttype[12] = "fpolof";
    tform[12] = "1E";
    tunit[12] = " ";
    //
    ttype[13] = "fpoloa";
    tform[13] = "1E";
    tunit[13] = "mm**2";
    //
    ttype[14] = "fploo";
    tform[14] = "1J";
    tunit[14] = " ";
    //
    ttype[15] = "fploof";
    tform[15] = "1E";
    tunit[15] = " ";
    //
    ttype[16] = "fplooa";
    tform[16] = "1E";
    tunit[16] = "mm**2";
    //
    ttype[17] = "fplto";
    tform[17] = "1J";
    tunit[17] = " ";
    //
    ttype[18] = "fpltof";
    tform[18] = "1E";
    tunit[18] = " ";
    //
    ttype[19] = "fpltoa";
    tform[19] = "1E";
    tunit[19] = "mm**2";
    //
    ttype[20] = "fpnr";
    tform[20] = "1J";
    tunit[20] = " ";
    //
    ttype[21] = "fpnrf";
    tform[21] = "1E";
    tunit[21] = "mm**2";
    //
    ttype[22] = "fpnra";
    tform[22] = "1E";
    tunit[22] = " ";
    //
    ttype[23] = "fpabn";
    tform[23] = "1J";
    tunit[23] = " ";
    //
    ttype[24] = "fpabnf";
    tform[24] = "1E";
    tunit[24] = " ";
    //
    ttype[25] = "fpabna";
    tform[25] = "1E";
    tunit[25] = "mm**2";
    //
    ttype[26] = "fpmtotal";
    tform[26] = "1J";
    tunit[26] = " ";
    //
    ttype[27] = "fpmtotf";
    tform[27] = "1E";
    tunit[27] = " ";
    //
    ttype[28] = "fpmtota";
    tform[28] = "1E";
    tunit[28] = "mm**2";
    //
    ttype[29] = "fpmopl";
    tform[29] = "1J";
    tunit[29] = " ";
    //
    ttype[30] = "fpmoplf";
    tform[30] = "1E";
    tunit[30] = " ";
    //
    ttype[31] = "fpmopla";
    tform[31] = "1E";
    tunit[31] = "mm**2";
    //
    ttype[32] = "fpmolo";
    tform[32] = "1J";
    tunit[32] = " ";
    //
    ttype[33] = "fpmolof";
    tform[33] = "1E";
    tunit[33] = " ";
    //
    ttype[34] = "fpmoloa";
    tform[34] = "1E";
    tunit[34] = "mm**2";
    //
    ttype[35] = "fpmloo";
    tform[35] = "1J";
    tunit[35] = " ";
    //
    ttype[36] = "fpmloof";
    tform[36] = "1E";
    tunit[36] = " ";
    //
    ttype[37] = "fpmlooa";
    tform[37] = "1E";
    tunit[37] = "mm**2";
    //
    ttype[38] = "fpmlto";
    tform[38] = "1J";
    tunit[38] = " ";
    //
    ttype[39] = "fpmltof";
    tform[39] = "1E";
    tunit[39] = " ";
    //
    ttype[40] = "fpmltoa";
    tform[40] = "1E";
    tunit[40] = "mm**2";
    //
    ttype[41] = "fpmnr";
    tform[41] = "1J";
    tunit[41] = " ";
    //
    ttype[42] = "fpmnrf";
    tform[42] = "1E";
    tunit[42] = " ";
    //
    ttype[43] = "fpmnra";
    tform[43] = "1E";
    tunit[43] = "mm**2";
    //
    ttype[44] = "fpmabn";
    tform[44] = "1J";
    tunit[44] = " ";
    //
    ttype[45] = "fpmabnf";
    tform[45] = "1E";
    tunit[45] = " ";
    //
    ttype[46] = "fpmabna";
    tform[46] = "1E";
    tunit[46] = "mm**2";
    //
    ttype[47] = "mirtop";
    tform[47] = "1J";
    tunit[47] = " ";
    //
    ttype[48] = "mirtopf";
    tform[48] = "1E";
    tunit[48] = " ";
    //
    ttype[49] = "mirtopa";
    tform[49] = "1E";
    tunit[49] = "mm**2";
    //
    ttype[50] = "shadow";
    tform[50] = "1J";
    tunit[50] = " ";
    //
    ttype[51] = "shadowf";
    tform[51] = "1E";
    tunit[51] = " ";
    //
    ttype[52] = "shadowa";
    tform[52] = "1E";
    tunit[52] = "mm**2";
    //
    ttype[53] = "ohousing";
    tform[53] = "1J";
    tunit[53] = " ";
    //
    ttype[54] = "ohousinf";
    tform[54] = "1E";
    tunit[54] = " ";
    //
    ttype[55] = "ohousina";
    tform[55] = "1E";
    tunit[55] = "mm**2";
    //
    ttype[56] = "ihousing";
    tform[56] = "1J";
    tunit[56] = " ";
    //
    ttype[57] = "ihousinf";
    tform[57] = "1E";
    tunit[57] = " ";
    //
    ttype[58] = "ihousina";
    tform[58] = "1E";
    tunit[58] = "mm**2";
    //
    ttype[59] = "absfront";
    tform[59] = "1J";
    tunit[59] = " ";
    //
    ttype[60] = "absfronf";
    tform[60] = "1E";
    tunit[60] = " ";
    //
    ttype[61] = "absfrona";
    tform[61] = "1E";
    tunit[61] = "mm**2";
    //
    ttype[62] = "absback";
    tform[62] = "1J";
    tunit[62] = " ";
    //
    ttype[63] = "absbackf";
    tform[63] = "1E";
    tunit[63] = " ";
    //
    ttype[64] = "absbacka";
    tform[64] = "1E";
    tunit[64] = "mm**2";
    //
    ttype[65] = "noleave";
    tform[65] = "1J";
    tunit[65] = " ";
    //
    ttype[66] = "noleavef";
    tform[66] = "1E";
    tunit[66] = " ";
    //
    ttype[67] = "noleavea";
    tform[67] = "1E";
    tunit[67] = "mm**2";
    //
    ttype[68] = "error";
    tform[68] = "1J";
    tunit[68] = " ";
    //
    ttype[69] = "errorf";
    tform[69] = "1E";
    tunit[69] = " ";
    //
    ttype[70] = "errora";
    tform[70] = "1E";
    tunit[70] = "mm**2";
    //
    ttype[71] = "psfrad";
    tform[71] = "501J";
    tunit[71] = " ";
    //
    ttype[72] = "psfradbn";
    tform[72] = "501E";
    tunit[72] = "arcmin";

    //
    // Try to open the output file
    //

    //
    // Determine whether the file names are "clean"; i.e. lack [#]
    //     strip them if so, and convert to char
    //
    size_t cleanLength = resultStatFileName.find_last_not_of('[');
    cleanLength++;
    resultStatFileName.resize(cleanLength);
    if (resultStatTableName.length() == 0)
       {
       // Create a default name
       resultStatTableName = "xrrtstat";
       }

    //
    // Open the file
    //
    fitsStatus = 0;
    //
    // 
    //
    string openFITSName;
    if (destroyExistingFITSFiles == true)
       {
       // Magic value to force CFITSIO to destroy existing file
       openFITSName = "!" + resultStatFileName;
       }
    else
       {
       openFITSName = resultStatFileName;
       }
    fitsReturn = fits_create_file(&resultStatFile,
                                  (char*) openFITSName.c_str(), &fitsStatus);
    //
    // The file may not already exist; if so create it
    //
    if (fitsStatus == NO_ERROR)
       {
       // Create a dummy header
       int bitpix = LONG_IMG;
       int naxis = 0;
       long* naxes = 0;
       fitsReturn = fits_write_imghdr(resultStatFile, bitpix, naxis, naxes,
                                      &fitsStatus);
       if (fitsStatus != NO_ERROR)
          {
          throw fitsStatus;
          }
       }
    else if (fitsStatus == FILE_NOT_CREATED)
       {
       //
       // The file already exists
       //
       fitsStatus = 0;
       fitsReturn = fits_open_file(&resultStatFile,
                                  (char*) openFITSName.c_str(), 
                                  READWRITE, &fitsStatus);
       if (fitsStatus != NO_ERROR)
          {
          throw fitsStatus;
          }
       }
    //
    // Position at the end of the file
    //
    for (int i=2; i<10000; i++)
       {
       fitsReturn = fits_movabs_hdu(resultStatFile, i, &fitsHduType, 
                                    &fitsStatus);
       if (fitsStatus == END_OF_FILE)
          {
          fitsStatus = 0;
          break;
          }
       if (fitsStatus != NO_ERROR)
          {
          throw fitsStatus;
          }
       }
    //
    // Write a default header here
    //
    fitsStatus = 0; // Reset END_OF_FILE
    fitsRow = 0;
    fitsReturn = fits_insert_btbl(resultStatFile, fitsRow, tfields,
                                 (char**)ttype, (char**)tform, (char**)tunit,
                                 (char*) resultStatTableName.c_str(),
                                 0L, &fitsStatus);
    if (fitsStatus != NO_ERROR)
       {
       throw fitsStatus;
       }
    //
    // Write Date
    //
    fitsReturn = fits_write_date(resultStatFile, &fitsStatus);
    if (fitsStatus != NO_ERROR)
       {
       throw fitsStatus;
       }
    //
    // Long string Warning
    //
    fitsReturn = fits_write_key_longwarn(resultStatFile, &fitsStatus);
    if (fitsStatus != NO_ERROR)
       {
       throw fitsStatus;
       }

    //
    // Write description of row columns
    //
    string comment;
    comment = "Description of row columns:";
    resultStatCommentWrite(comment);
    comment = "energy = the keV energy of the traced photons";
    resultStatCommentWrite(comment);
    comment = "photons = count of all photon traced";
    resultStatCommentWrite(comment);
    comment = "corephtn = count of PSF core photons traced";
    resultStatCommentWrite(comment);
    comment = "corefrac = corephtn / photons";
    resultStatCommentWrite(comment);
    comment = "corearea = corefrac * area of telescope used";
    resultStatCommentWrite(comment);
    comment = "fptotal = total count of photons that hit the focal plane";
    resultStatCommentWrite(comment);
    comment = "fptotalf = fptotal / photons";
    resultStatCommentWrite(comment);
    comment = "fptotala = fptotalf * area of telescope used";
    resultStatCommentWrite(comment);
    comment = "fpopl = total count of photons that hit the focal plane";
    resultStatCommentWrite(comment);
    comment = "        with One Per Layer mirror impacts";
    resultStatCommentWrite(comment);
    comment = "fpoplf = fpopl / photons";
    resultStatCommentWrite(comment);
    comment = "fpopla = fpoplf * area of telescope used";
    resultStatCommentWrite(comment);
    comment = "fpolo = total count of photons that hit the focal plane";
    resultStatCommentWrite(comment);
    comment = "        with One Layer Only mirror impacts";
    resultStatCommentWrite(comment);
    comment = "fpolof = fpolo / photons";
    resultStatCommentWrite(comment);
    comment = "fpoloa = fpolof * area of telescope used";
    resultStatCommentWrite(comment);
    comment = "fploo = total count of photons that hit the focal plane";
    resultStatCommentWrite(comment);
    comment = "        with Layer One Only mirror impacts";
    resultStatCommentWrite(comment);
    comment = "fploof = fploo / photons";
    resultStatCommentWrite(comment);
    comment = "fplooa = fploof * area of telescope used";
    resultStatCommentWrite(comment);
    comment = "fplto = total count of photons that hit the focal plane";
    resultStatCommentWrite(comment);
    comment = "        with Layer Two Only mirror impacts";
    resultStatCommentWrite(comment);
    comment = "fpltof = fplto / photons";
    resultStatCommentWrite(comment);
    comment = "fpltoa = fpltof * area of telescope used";
    resultStatCommentWrite(comment);
    comment = "fpnr = total count of photons that hit the focal plane";
    resultStatCommentWrite(comment);
    comment = "        with No Reflections on any mirror layer";
    resultStatCommentWrite(comment);
    comment = "fpnrf = fpnr / photons";
    resultStatCommentWrite(comment);
    comment = "fpnra = fpnrf * area of telescope used";
    resultStatCommentWrite(comment);
    comment = "fpabn = total count of photons that hit the focal plane";
    resultStatCommentWrite(comment);
    comment = "        with an ABNormal number of reflections";
    resultStatCommentWrite(comment);
    comment = "fpabnf = fpabn / photons";
    resultStatCommentWrite(comment);
    comment = "fpabna = fpabnf * area of telescope used";
    resultStatCommentWrite(comment);
    comment = "fpmtotal = total count of photons that hit the focal plane";
    resultStatCommentWrite(comment);
    comment = "           inside the declared focal plane mask";
    resultStatCommentWrite(comment);
    comment = "fpmtotf = fpmtotal / photons";
    resultStatCommentWrite(comment);
    comment = "fpmtota = fpmtotf * area of telescope used";
    resultStatCommentWrite(comment);
    comment = "fpmopl = total count of photons that hit the focal plane mask";
    resultStatCommentWrite(comment);
    comment = "         with One Per Layer mirror impacts";
    resultStatCommentWrite(comment);
    comment = "fpmoplf = fpmopl / photons";
    resultStatCommentWrite(comment);
    comment = "fpmopla = fpmoplf * area of telescope used";
    resultStatCommentWrite(comment);
    comment = "fpmolo = total count of photons that hit the focal plane mask";
    resultStatCommentWrite(comment);
    comment = "        with One Layer Only mirror impacts";
    resultStatCommentWrite(comment);
    comment = "fpmolof = fpmolo / photons";
    resultStatCommentWrite(comment);
    comment = "fpmoloa = fpmolof * area of telescope used";
    resultStatCommentWrite(comment);
    comment = "fpmloo = total count of photons that hit the focal plane mask";
    resultStatCommentWrite(comment);
    comment = "        with Layer One Only mirror impacts";
    resultStatCommentWrite(comment);
    comment = "fpmloof = fpmloo / photons";
    resultStatCommentWrite(comment);
    comment = "fpmlooa = fpmloof * area of telescope used";
    resultStatCommentWrite(comment);
    comment = "fpmlto = total count of photons that hit the focal plane mask";
    resultStatCommentWrite(comment);
    comment = "        with Layer Two Only mirror impacts";
    resultStatCommentWrite(comment);
    comment = "fpmltof = fpmlto / photons";
    resultStatCommentWrite(comment);
    comment = "fpmltoa = fpmltof * area of telescope used";
    resultStatCommentWrite(comment);
    comment = "fpmnr = total count of photons that hit the focal plane mask";
    resultStatCommentWrite(comment);
    comment = "        with No Reflections on any mirror layer";
    resultStatCommentWrite(comment);
    comment = "fpmnrf = fpmnr / photons";
    resultStatCommentWrite(comment);
    comment = "fpmnra = fpmnrf * area of telescope used";
    resultStatCommentWrite(comment);
    comment = "fpmabn = total count of photons that hit the focal plane mask";
    resultStatCommentWrite(comment);
    comment = "        with an ABNormal number of reflections";
    resultStatCommentWrite(comment);
    comment = "fpmabnf = fpmabn / photons";
    resultStatCommentWrite(comment);
    comment = "fpmabna = fpmabnf * area of telescope used";
    resultStatCommentWrite(comment);
    comment = "mirtop = count of impacts on the top cross-section of mirrors";
    resultStatCommentWrite(comment);
    comment = "mirtopf = mirtop / photons";
    resultStatCommentWrite(comment);
    comment = "mirtopa = mirtopf * area of telescope used";
    resultStatCommentWrite(comment);
    comment = "shadow = count of impacts on obstructions";
    resultStatCommentWrite(comment);
    comment = "shadowf = shadow / photons";
    resultStatCommentWrite(comment);
    comment = "shadowa = shadowf * area of telescope used";
    resultStatCommentWrite(comment);
    comment = "ohousing = count of impacts on the outer housing";
    resultStatCommentWrite(comment);
    comment = "ohousinf = ohousing / photons";
    resultStatCommentWrite(comment);
    comment = "ohousina = ohousinf * area of telescope used";
    resultStatCommentWrite(comment);
    comment = "ihousing = count of impacts on the inner housing";
    resultStatCommentWrite(comment);
    comment = "ihousinf = ihousing / photons";
    resultStatCommentWrite(comment);
    comment = "ihousina = ihousinf * area of telescope used";
    resultStatCommentWrite(comment);
    comment = "absfront = count of photons absorbed on the front face of a mirror";
    resultStatCommentWrite(comment);
    comment = "absfronf = absfront / photons";
    resultStatCommentWrite(comment);
    comment = "absfrona = absfronf * area of telescope used";
    resultStatCommentWrite(comment);
    comment = "absback = count of photons absorbed on the back face of a mirror";
    resultStatCommentWrite(comment);
    comment = "absbackf = absback / photons";
    resultStatCommentWrite(comment);
    comment = "absbacka = absbackf * area of telescope used";
    resultStatCommentWrite(comment);
    comment = "noleave = count of photons not allowed to cross a mirror edge";
    resultStatCommentWrite(comment);
    comment = "noleavef = noleave / photons";
    resultStatCommentWrite(comment);
    comment = "noleavea = noleavef * area of telescope used";
    resultStatCommentWrite(comment);
    comment = "error = count of photons that fell in the error condition";
    resultStatCommentWrite(comment);
    comment = "      Examples: Photon had >= 0 Z unit vector";
    resultStatCommentWrite(comment);
    comment = "                Photon had no outer mirror";
    resultStatCommentWrite(comment);
    comment = "                Photon fell between mirror fragments";
    resultStatCommentWrite(comment);
    comment = "errorf = error / photons";
    resultStatCommentWrite(comment);
    comment = "errora = errorf * area of telescope used";
    resultStatCommentWrite(comment);
    comment = "psfrad = Point Spread Function Radial profile";
    resultStatCommentWrite(comment);
    comment = "         Count by radial bin of the number of photons seen";
    resultStatCommentWrite(comment);
    comment = "psfradbn = Point Spread Function Radial profile";
    resultStatCommentWrite(comment);
    comment = "         bin lower edge in arc minutes";
    resultStatCommentWrite(comment);

    //
    // Write the XrrtStat keywords
    //
    writeHistoricalKeywords(resultStatFile);

    //
    // Save a block of space for keywords
    //
    fitsReturn = fits_set_hdrsize(resultStatFile, 100, &fitsStatus);
    if (fitsStatus != NO_ERROR)
       {
       throw fitsStatus;
       }
}

void 
XrrtStat::stopResultCollection()
{
int      fitsStatus;        // Standard error code return from FITSIO
int      fitsReturn;        // Standard return type from a FITSIO function

    if (!collectResultStat)
       {
       return;
       }
    if (!outputResultStat)
       {
       // We are not writting to FITS
       addByEnergyToTotal();
       zeroByEnergy();
       return;
       }
    // write last energy row
    if (! suppressEnergyResultRows)
       {
       writeResultStatEnergyRow();
       }
    addByEnergyToTotal();
    writeResultStatTotalRow();
    // update row number and close
    fitsStatus = 0;
    fitsReturn = fits_modify_key_lng(resultStatFile, "NAXIS2",
                                     resultStatRowsWritten, "&",
                                     &fitsStatus);
    if (fitsStatus != NO_ERROR)
       {
       throw fitsStatus;
       }
    fits_close_file(resultStatFile, &fitsStatus);
    if (fitsStatus != NO_ERROR)
       {
       throw fitsStatus;
       }
    resultStatFile = 0;
}

void
XrrtStat::startPhotonHistory()
{
//
// Open a FITS file to receive the Photon History records
//
// Standard FITSIO calling sequence
int      fitsStatus;        // Standard error code return from FITSIO
int      fitsReturn;        // Standard return type from a FITSIO function
int      fitsHduType;       // Std FITS HDU type return
long     fitsRow;

    //
    // Just in case we are called when not needed
    //
    if (!outputPhotonHistory)
       {
       return;
       }

    //
    // Defaulted fits file format control
    //
    int tfields = 15;
    const char* ttype[15];
    const char* tform[15];
    const char* tunit[15];

    //
    // Photon History FITS file structure
    //
    ttype[0] = "photon";
    tform[0] = "1J";
    tunit[0] = " ";
    //
    ttype[1] = "energy";
    tform[1] = "1E";
    tunit[1] = "keV";
    //
    ttype[2] = "oradius";
    tform[2] = "1E";
    tunit[2] = "mm";
    //
    ttype[3] = "oangle";
    tform[3] = "1E";
    tunit[3] = "degree";
    //
    ttype[4] = "telex";
    tform[4] = "1E";
    tunit[4] = "mm";
    //
    ttype[5] = "teley";
    tform[5] = "1E";
    tunit[5] = "mm";
    //
    ttype[6] = "otheta";
    tform[6] = "1E";
    tunit[6] = "arcmin";
    //
    ttype[7] = "ophi";
    tform[7] = "1E";
    tunit[7] = "degree";
    //
    ttype[8] = "result";
    tform[8] = "1J";
    tunit[8] = " ";
    //
    ttype[9] = "class";
    tform[9] = "1J";
    tunit[9] = " ";
    //
    ttype[10] = "x";
    tform[10] = "1E";
    tunit[10] = "mm";
    //
    ttype[11] = "y";
    tform[11] = "1E";
    tunit[11] = "mm";
    //
    ttype[12] = "psfx";
    tform[12] = "1E";
    tunit[12] = "mm";
    //
    ttype[13] = "psfy";
    tform[13] = "1E";
    tunit[13] = "mm";
    //
    // add photon pass for stray investigation (modified by HIDEYUKI MORI)
    ttype[14] = "path";
    tform[14] = "1J";
    tunit[14] = " ";
    //

    //
    // Try to open the output file
    //

    //
    // Determine whether the file names are "clean"; i.e. lack [#]
    //     strip them if so, and convert to char
    //
    size_t cleanLength = photonHistoryFileName.find_last_not_of('[');
    cleanLength++;
    photonHistoryFileName.resize(cleanLength);
    if (photonHistoryTableName.length() == 0)
       {
       // Create a default name
       photonHistoryTableName = "xrrthist";
       }
 

    //
    // Open the file
    //
    fitsStatus = 0;
    string openFITSName;
    if (destroyExistingFITSFiles == true)
       {
       // Magic value to force CFITSIO to destroy existing file
       openFITSName = "!" + photonHistoryFileName;
       }
    else
       {
       openFITSName = photonHistoryFileName;
       }
    fitsReturn = fits_create_file(&photonHistoryFile,
                                  (char*) openFITSName.c_str(), &fitsStatus);

    //
    // The file may not already exist; if so create it
    //
    if (fitsStatus == NO_ERROR)
       {
       // Create a dummy header
       int bitpix = LONG_IMG;
       int naxis = 0;
       long* naxes = 0;
       fitsReturn = fits_write_imghdr(photonHistoryFile, bitpix, naxis, naxes,
                                      &fitsStatus);
       if (fitsStatus != NO_ERROR)
          {
          throw fitsStatus;
          }
       }
    else if (fitsStatus == FILE_NOT_CREATED)
       {
       //
       // The file already exists
       //
       fitsStatus = 0;
       fitsReturn = fits_open_file(&photonHistoryFile,
                                  (char*) openFITSName.c_str(), 
                                  READWRITE, &fitsStatus);
       if (fitsStatus != NO_ERROR)
          {
          throw fitsStatus;
          }
       }
    //
    // Position at the end of the file
    //
    for (int i=2; i<10000; i++)
       {
       fitsReturn = fits_movabs_hdu(photonHistoryFile, i, &fitsHduType, 
                                    &fitsStatus);
       if (fitsStatus == END_OF_FILE)
          {
          break;
          }
       if (fitsStatus != NO_ERROR)
          {
          throw fitsStatus;
          }
       }
    //
    // Write a default header here
    //
    fitsStatus = 0; // Reset END_OF_FILE
    fitsRow = 0;
    fitsReturn = fits_insert_btbl(photonHistoryFile, fitsRow, tfields,
                                 (char**)ttype, (char**)tform, (char**)tunit,
                                 (char*) photonHistoryTableName.c_str(),
                                 0L, &fitsStatus);
    if (fitsStatus != NO_ERROR)
       {
       throw fitsStatus;
       }
    //
    // Write Date
    //
    fitsReturn = fits_write_date(photonHistoryFile, &fitsStatus);
    if (fitsStatus != NO_ERROR)
       {
       throw fitsStatus;
       }
    //
    // Long string Warning
    //
    fitsReturn = fits_write_key_longwarn(photonHistoryFile, &fitsStatus);
    if (fitsStatus != NO_ERROR)
       {
       throw fitsStatus;
       }

    //
    // Write the description of the fields
    //
    string comment;
    comment = "Description of the row fields:";
    photonHistoryCommentWrite(comment);
    comment = "photon = traced photon counter";
    photonHistoryCommentWrite(comment);
    comment = "energy = traced photon energy in keV";
    photonHistoryCommentWrite(comment);
    comment = "oradius = original telescope radius of the photon";
    photonHistoryCommentWrite(comment);
    comment = "oangle = original telescope angle of the photon";
    // add comment for incident position (added by HIDEYUKI MORI)
    photonHistoryCommentWrite(comment);
    comment = "telex = original telescope x coordinate of the photon";
    photonHistoryCommentWrite(comment);
    comment = "teley = original telescope y coordinate of the photon";
    photonHistoryCommentWrite(comment);
    comment = "otheta = original incident angle of the photon";
    photonHistoryCommentWrite(comment);
    comment = "result = result code for tracing the photon";
    photonHistoryCommentWrite(comment);
    comment = "result = 0 = PHOTON_CONTINUES";
    photonHistoryCommentWrite(comment);
    comment = "result = 1 = PHOTON_HITS_OBSTRUCTION";
    photonHistoryCommentWrite(comment);
    comment = "result = 2 = PHOTON_HITS_OUTER_HOUSING";
    photonHistoryCommentWrite(comment);
    comment = "result = 3 = PHOTON_HITS_INNER_HOUSING";
    photonHistoryCommentWrite(comment);
    comment = "result = 4 = PHOTON_HITS_TOP_OF_MIRROR";
    photonHistoryCommentWrite(comment);
    comment = "result = 5 = PHOTON_ABSORBED_ON_OUTER_MIRROR";
    photonHistoryCommentWrite(comment);
    comment = "result = 6 = PHOTON_ABSORBED_ON_INNER_MIRROR";
    photonHistoryCommentWrite(comment);
    comment = "result = 7 = PHOTON_REVERSES_Z_DIRECTION";
    photonHistoryCommentWrite(comment);
    comment = "result = 8 = PHOTON_CAN_NOT_LEAVE_MIRROR";
    photonHistoryCommentWrite(comment);
    comment = "result = 9 = PHOTON_HIT_FOCAL_PLANE";
    photonHistoryCommentWrite(comment);
    comment = "result =10 = ERROR";
    photonHistoryCommentWrite(comment);
    comment = "class = the photon reflection classification";
    photonHistoryCommentWrite(comment);
    comment = "class = 0 = NO_REFLECTION_CLASS";
    photonHistoryCommentWrite(comment);
    comment = "class = 1 = ONE_LAYER_ONLY";
    photonHistoryCommentWrite(comment);
    comment = "class = 2 = ONE_PER_LAYER";
    photonHistoryCommentWrite(comment);
    comment = "class = 3 = NO_REFLECTIONS";
    photonHistoryCommentWrite(comment);
    comment = "class = 4 = ABNORMAL_PATH";
    photonHistoryCommentWrite(comment);
    comment = "class = 5 = FIRST_LAYER_ONLY";
    photonHistoryCommentWrite(comment);
    comment = "class = 6 = SECOND_LAYER_ONLY";
    photonHistoryCommentWrite(comment);
    comment = "x = impact coordinate on focal plane";
    photonHistoryCommentWrite(comment);
    comment = "y = impact coordinate on focal plane";
    photonHistoryCommentWrite(comment);
    comment = "psfx = impact coordinate on focal plane";
    photonHistoryCommentWrite(comment);
    comment = "       relative to center of point spread function";
    photonHistoryCommentWrite(comment);
    comment = "psfy = impact coordinate on focal plane";
    photonHistoryCommentWrite(comment);
    comment = "       relative to center of point spread function";
    photonHistoryCommentWrite(comment);
    // add comment for photon path (modified by HIDEYUKI MORI)
    comment = "path = photon reflection path";
    photonHistoryCommentWrite(comment);
    comment = "       in terms of integer array";
    photonHistoryCommentWrite(comment);
    //
    // Write the XrrtStat keywords
    //
    writeHistoricalKeywords(photonHistoryFile);

    //
    // Save a block of space for keywords
    //
    fitsReturn = fits_set_hdrsize(photonHistoryFile, 100, &fitsStatus);
    if (fitsStatus != NO_ERROR)
       {
       throw fitsStatus;
       }
}

void
XrrtStat::stopPhotonHistory()
{
//
// Close the Photon History FITS file
//
int      fitsStatus;        // Standard error code return from FITSIO
int      fitsReturn;        // Standard return type from a FITSIO function

    //
    // In case we are called when not needed
    //
    if (!outputPhotonHistory)
       {
       return;
       }
    //
    // update row number and close
    //
    fitsStatus = 0;
    fitsReturn = fits_modify_key_lng(photonHistoryFile, "NAXIS2",
                                     photonHistoryRowsWritten, "&",
                                     &fitsStatus);
    if (fitsStatus != NO_ERROR)
       {
       throw fitsStatus;
       }
    //
    fits_close_file(photonHistoryFile, &fitsStatus);
    if (fitsStatus != NO_ERROR)
       {
       throw fitsStatus;
       }
    //
    photonHistoryFile = 0;
}

void
XrrtStat::startImageCollection()
{
//
// Prepare for collecting Image data of the focal plane
//
// Standard FITSIO calling sequence
int      fitsStatus;        // Standard error code return from FITSIO
int      fitsReturn;        // Standard return type from a FITSIO function

    //
    // In case we are called when not needed
    //
    if (!collectImage)
       {
       return;
       }

    //
    // Allocate the image storage areas
    //
    if (saveImageByEnergy)
       {
       // we will need the statByEnergy.image area
       for (int i = 0; i < xyAxisBins; i++)
          {
          vector<int> zero;
          for (int j = 0; j <  xyAxisBins; j++)
             {
             zero.push_back(0);
             }
          statByEnergy.image.push_back(zero);
          }
       }
    for (int i = 0; i < xyAxisBins; i++)
       {
       vector<int> zero;
       for (int j = 0; j <  xyAxisBins; j++)
          {
          zero.push_back(0);
          }
       total.image.push_back(zero);
       }
    
    //
    // Try to open the output file
    //

    //
    // Determine whether the file names are "clean"; i.e. lack [#]
    //     strip them if so, and convert to char
    //
    size_t cleanLength = imageFileName.find_last_not_of('[');
    cleanLength++;
    imageFileName.resize(cleanLength);
    if (resultStatTableName.length() == 0)
       {
       // Create a default name
       imageTableName = "PRIMARY";
       }

    //
    // Open the file
    //
    fitsStatus = 0;
    string openFITSName;
    if (destroyExistingFITSFiles == true)
       {
       // Magic value to force CFITSIO to destroy existing file
       openFITSName = "!" + imageFileName;
       }
    else
       {
       openFITSName = imageFileName;
       }
    fitsReturn = fits_create_file(&imageFile,
                                  (char*) openFITSName.c_str(), &fitsStatus);

    //
    // If the requested output location is the primary array, then
    // the file must not already exist
    //
    if (imageTableName == "PRIMARY" && fitsStatus == FILE_NOT_CREATED)
       {
       throw imageFileMayNotExist;
       }
    //
    // Use error codes to condition response
    //
    if (fitsStatus == NO_ERROR)
       {
       // Create a dummy header
       int bitpix = LONG_IMG;
       int naxis = 2;
       long naxes[2];
       naxes[0] = xyAxisBins;
       naxes[1] = xyAxisBins;
       fitsReturn = fits_write_imghdr(imageFile, bitpix, naxis, naxes,
                                      &fitsStatus);
       if (fitsStatus != NO_ERROR)
          {
          throw fitsStatus;
          }
       // Write Date
       fitsReturn = fits_write_date(imageFile, &fitsStatus);
       if (fitsStatus != NO_ERROR)
          {
          throw fitsStatus;
          }
       // Long string Warning
       fitsReturn = fits_write_key_longwarn(imageFile, &fitsStatus);
       if (fitsStatus != NO_ERROR)
          {
          throw fitsStatus;
          }
       writeHistoricalKeywords(imageFile);
       // Save space for future keywords
       fitsReturn = fits_set_hdrsize(imageFile, 100, &fitsStatus);
       if (fitsStatus != NO_ERROR)
          {
          throw fitsStatus;
          }
       }
    else if (fitsStatus == FILE_NOT_CREATED)
       {
       //
       // The file already exists
       //
       fitsStatus = 0;
       fitsReturn = fits_open_file(&resultStatFile,
                                  (char*) openFITSName.c_str(), 
                                  READWRITE, &fitsStatus);
       if (fitsStatus != NO_ERROR)
          {
          throw fitsStatus;
          }
       }
    if (fitsStatus != NO_ERROR)
       {
       throw fitsStatus;
       }
}

void
XrrtStat::addPhotonToImage(const XrrtPhoton& photon)
{
//
// Add data from the given photon to the images as necessary
// Watch out! This routine adds the photon without checking whether it SHOULD
// be added or not. That is the callers responsibility.
//
// Standard FITSIO calling sequence
int      fitsStatus;        // Standard error code return from FITSIO
int      fitsReturn;        // Standard return type from a FITSIO function
int      fitsHduType;       // Standard FITSIO parameter
    //
    // In case we are called when not necessary
    //
    if (!collectImage)
       {
       return;
       }

    // Always 
    fitsStatus = 0;

    //
    // 1st check whether the energy of the photon has changed
    //
    if (statByEnergy.energy == 0.0e0)
       {
       // 1st time thru
       statByEnergy.energy = photon.getEnergy();
       }
    if ((statByEnergy.energy != photon.getEnergy()) && saveImageByEnergy)
       {
       // We are supposed to save the individual images as a function of energy
       //
       // Save the current image to FITS and zero it out
       // We have to flip C++ arrays to F77 array order
       //
       long* fitsImage;
       fitsImage = new long[xyAxisBins*xyAxisBins];
       int k = 0;
       for (int j = xyAxisBins-1; j >= 0; j--)
          {
          for (int i = 0; i < (int) xyAxisBins; i++)
             {
             fitsImage[k] = statByEnergy.image[i][j];
             k++;
             statByEnergy.image[i][j] = 0;
             }
          }
       // Define FITS array x,y axis size
       long naxes[2];
       naxes[0] = xyAxisBins;
       naxes[1] = xyAxisBins;
       // Position at the end of the FITS file
       for (int i=2; i<10000; i++)
          {
          fitsReturn = fits_movabs_hdu(imageFile, i, &fitsHduType, 
                                       &fitsStatus);
          if (fitsStatus == END_OF_FILE)
             {
             break;
             }
          if (fitsStatus != NO_ERROR)
             {
             throw fitsStatus;
             }
          }
       fitsStatus = 0; // Reset END_OF_FILE
       // Create a complete FITS image extension
       fitsReturn = fits_create_img(imageFile, LONG_IMG, 2, naxes,
                                    &fitsStatus);
       if (fitsStatus != NO_ERROR)
          {
          throw fitsStatus;
          }
       //
       // Write header keywords to the Image extension
       //
       // Label the extension as in "KEV1_000" for a 1.0 keV image
       string keyword = "EXTNAME";
       char extname[255];
       sprintf(extname,"KEV%f",statByEnergy.energy);
       string extName = extname;
       extName.replace(extName.find('.'),1,"_");
       string comment = "Single energy image (XRRT)";
       imageKeywordWrite(keyword, extName, comment);
       //
       // Write Date
       fitsReturn = fits_write_date(imageFile, &fitsStatus);
       if (fitsStatus != NO_ERROR)
          {
          throw fitsStatus;
          }
       //
       // Long string Warning
       fitsReturn = fits_write_key_longwarn(imageFile, &fitsStatus);
       if (fitsStatus != NO_ERROR)
          {
          throw fitsStatus;
          }
       //
       // Write whatever XrrtStat knows about metadata
       writeHistoricalKeywords(imageFile);
       //
       // Save a block of space for keywords
       fitsReturn = fits_set_hdrsize(imageFile, 100, &fitsStatus);
       if (fitsStatus != NO_ERROR)
          {
          throw fitsStatus;
          }
       //
       // Now actually write the image to the FITS extension
       //
       fitsReturn = fits_write_2d_lng(imageFile, 0L, (long) xyAxisBins,
                                      (long) xyAxisBins, (long) xyAxisBins, 
                                      (long*) fitsImage, &fitsStatus);
       if (fitsStatus != NO_ERROR)
          {
          throw fitsStatus;
          }
       delete [] fitsImage;
       }
    //
    // Determine if the photon falls on the total image and add it there
    // Watch out! If the photon is not on the image, we RETURN early.
    //
    // So we number the image just like it was a picture of an x/y plane
    //   0  0  1  2  3  4  5  6  7                      xyAxisMaxMM
    //   1                                                   |
    //   2                                                   |
    //   3                                                   |
    //   4 with xyAxisMinMM   ------>    xyAxisMaxMM    xyAxisMinMM
    // Also remember that images are numbered when you write them out
    //   4
    //   3
    //   2
    //   1
    //   0  0  1  2  3  4  5  6
    double x,y;
    photon.getXY(&x,&y);
    int intIndex = 0;
    double floatIndex = 0.0e0;
    double fraction = 0.0e0;
    int yIndex = 0;
    int xIndex = 0;
    // Position Y in array with nearest Integer function
    floatIndex = (y-xyAxisMinMM) * (double)(xyAxisBins) /
                 (xyAxisMaxMM-xyAxisMinMM) + 0.5e0;
    intIndex = (int) floatIndex;
    fraction = floatIndex - (double) intIndex;
    if (fraction >= 0.5e0)
       {
       yIndex = intIndex + 1;
       }
    else
       {
       yIndex = intIndex;
       }
    // reverse the order in Y and adjust for 0-(N-1) in place of 1-N
    yIndex = (xyAxisBins-1) - yIndex;
    if (yIndex < 0 || yIndex > ((int)xyAxisBins-1))
       {
       // off the image
       return;
       }
    // Position X in array with nearest Integer function
    floatIndex = (x-xyAxisMinMM) * (double) xyAxisBins /
                 (xyAxisMaxMM-xyAxisMinMM) + 0.5e0;
    intIndex = (int) floatIndex;
    fraction = floatIndex - (double) intIndex;
    if (fraction >= 0.5e0)
       {
       xIndex = intIndex + 1;
       }
    else
       {
       xIndex = intIndex;
       }
    // Adjust xIndex for 0-(N-1) instead of 1-N
    xIndex--;
    if (xIndex < 0 || xIndex > ((int)xyAxisBins-1))
       {
       // off the image
       return;
       }
    //
    // If we are saving images by energy, add the photon to the energy image
    //
    if (saveImageByEnergy)
       {
       // Add photon to statByEnergy.image
       statByEnergy.image[xIndex][yIndex]++;
       }
    // Add photon to total.image
    total.image[xIndex][yIndex]++;
}

void
XrrtStat::stopImageCollection()
{
//
// Close out and write any partial images to the FITS file
// and then close the FITS file itself
//
// Standard FITSIO calling sequence
int      fitsStatus;        // Standard error code return from FITSIO
int      fitsReturn;        // Standard return type from a FITSIO function
int      fitsHduType;       // Std FITS HDU type return

    //
    // In case we are called when not needed
    //
    if (!collectImage)
       {
       return;
       }

    // Allocate the transfer array to CFITSIO
    long* fitsImage;
    fitsImage = new long[xyAxisBins*xyAxisBins];

    // Always
    fitsStatus = 0;

    //
    // Write last image if needed
    // Since energy images write when the energy changes, there is always
    // an incomplete image waiting for close to handle.
    //
    if (saveImageByEnergy)
       {
       // Save the current image to FITS and zero it out
       // Remember to flip C++ array order to F77/FITS order
       //
       int k=0;
       for (int j = xyAxisBins-1; j >= 0; j--)
          {
          for (int i = 0; i < (int) xyAxisBins; i++)
             {
             fitsImage[k] = statByEnergy.image[i][j];
             k++;
             statByEnergy.image[i][j] = 0;
             }
          }
       // Set up FITS array size
       long naxes[2];
       naxes[0] = xyAxisBins;
       naxes[1] = xyAxisBins;
       //
       // Position at the end of the FITS file
       //
       for (int i=2; i<10000; i++)
          {
          fitsReturn = fits_movabs_hdu(imageFile, i, &fitsHduType, 
                                       &fitsStatus);
          if (fitsStatus == END_OF_FILE)
             {
             break;
             }
          if (fitsStatus != NO_ERROR)
             {
             throw fitsStatus;
             }
          }
       fitsStatus = 0; // Reset END_OF_FILE
       //
       // Create a complete FITS Image extension in the file
       fitsReturn = fits_create_img(imageFile, LONG_IMG, 2, naxes,
                                    &fitsStatus);
       if (fitsStatus != NO_ERROR)
          {
          throw fitsStatus;
          }
       //
       // Label the extension with the energy value (e.g. KEV1_000)
       string keyword = "EXTNAME";
       char extname[255];
       sprintf(extname,"KEV%f",statByEnergy.energy);
       string extName = extname;
       extName.replace(extName.find('.'),1,"_");
       string comment = "Single energy image (XRRT)";
       imageKeywordWrite(keyword, extName, comment);
       //
       // Write Date
       fitsReturn = fits_write_date(imageFile, &fitsStatus);
       if (fitsStatus != NO_ERROR)
          {
          throw fitsStatus;
          }
       //
       // Long string Warning
       fitsReturn = fits_write_key_longwarn(imageFile, &fitsStatus);
       if (fitsStatus != NO_ERROR)
          {
          throw fitsStatus;
          }
       //
       // Write metadata keywords that XrrtStat knows about
       writeHistoricalKeywords(imageFile);
       //
       // Save a block of space for keywords
       fitsReturn = fits_set_hdrsize(imageFile, 100, &fitsStatus);
       if (fitsStatus != NO_ERROR)
          {
          throw fitsStatus;
          }
       //
       // Actually write the image data to the FITS extension
       fitsReturn = fits_write_2d_lng(imageFile, 0L, (long) xyAxisBins,
                                      (long) xyAxisBins, (long) xyAxisBins,
                                      (long*) fitsImage, &fitsStatus);
       if (fitsStatus != NO_ERROR)
          {
          throw fitsStatus;
          }
       }
    //
    // Total image processing
    //
    // Convert the C++ array to F77/FITS array order
    int k = 0;
    for (int j = (xyAxisBins-1); j >= 0; j--)
       {
       for (int i = 0; i < (int) xyAxisBins; i++)
          {
          fitsImage[k] = total.image[i][j];
          k++;
          }
       }
    // Set up the FITS array/image size
    long naxes[2];
    naxes[0] = xyAxisBins;
    naxes[1] = xyAxisBins;
    //
    // write total image
    //
    if (imageTableName == "PRIMARY")
       {
       // We want the image stored as a FITS PRIMARY array at the front
       // of the FITS file. This form works best with older software.
       //
       // Position at the primary array
       fitsReturn = fits_movabs_hdu(imageFile, 1, &fitsHduType, &fitsStatus);
       if (fitsStatus != NO_ERROR)
          {
          throw fitsStatus;
          }
       }
    else
       {
       // We want the image stored as a new-type IMAGE extension
       // Position at the end of the FITS file
       for (int i=2; i<10000; i++)
          {
          fitsReturn = fits_movabs_hdu(imageFile, i, &fitsHduType, 
                                       &fitsStatus);
          if (fitsStatus == END_OF_FILE)
             {
             break;
             }
          if (fitsStatus != NO_ERROR)
             {
             throw fitsStatus;
             }
          }
       fitsStatus = 0; // Reset END_OF_FILE
       //
       // Create a FITS IMAGE extension
       fitsReturn = fits_create_img(imageFile, LONG_IMG, 2, naxes,
                                   &fitsStatus);
       if (fitsStatus != NO_ERROR)
          {
          throw fitsStatus;
          }
       //
       // Label the extension 
       string keyword = "EXTNAME";
       string extName = imageTableName;
       string comment = "Total ray trace image (XRRT)";
       imageKeywordWrite(keyword, extName, comment);
       //
       // Write Date
       fitsReturn = fits_write_date(imageFile, &fitsStatus);
       if (fitsStatus != NO_ERROR)
          {
          throw fitsStatus;
          }
       //
       // Long string Warning
       fitsReturn = fits_write_key_longwarn(imageFile, &fitsStatus);
       if (fitsStatus != NO_ERROR)
          {
          throw fitsStatus;
          }
       //
       // Write the metadata that XrrtStat knows about
       writeHistoricalKeywords(imageFile);
       }
    //
    // For either a PRIMARY or IMAGE extension we are positioned correctly
    // to write the image data
    //
    fitsReturn = fits_write_2d_lng(imageFile, 0L, (long) xyAxisBins,
                                   (long) xyAxisBins, (long) xyAxisBins,
                                   (long*) fitsImage, &fitsStatus);
    if (fitsStatus != NO_ERROR)
       {
       throw fitsStatus;
       }
    //
    // Close the FITS file
    //
    fits_close_file(imageFile, &fitsStatus);
    if (fitsStatus != NO_ERROR)
       {
       throw fitsStatus;
       }
    //
    // Don't leave anyone with the impression that a file might be open
    imageFile = 0;
    
    delete [] fitsImage;
}

void 
XrrtStat::collectResultStatistics(const XrrtPhoton& photon)
{
//
// Parse out the status of a photon and add it to its various status counters
// If its energy has changed, write out and clear the counters
//
const double DEGREE2RADIANS = 0.017453292519943295769e0;
const double ARCMIN2DEGREE = 1.0e0/60.0e0;
const double ARCMIN2RADIAN = ARCMIN2DEGREE * DEGREE2RADIANS;

    //
    // In case we are called when we shouldn't be
    //
    if (!collectResultStat)
       {
       return;
       }

    //
    // 1st check whether the energy of the photon has changed
    // if it has we will dump a FITS record with the old counters
    //
    if (statByEnergy.energy == 0.0e0)
       {
       // 1st time thru
       statByEnergy.energy = photon.getEnergy();
       }
    if (statByEnergy.energy != photon.getEnergy())
       {
       // Since the energy changed,
       // Write the statistics for the old energy if we were supposed to
       if (outputResultStat && (! suppressEnergyResultRows) )
          {
          writeResultStatEnergyRow();
          }
       // Add the old energy counters to the running total
       addByEnergyToTotal();
       // zero out the old energy values
       zeroByEnergy();
       }
       
    //
    // Handle photon by its status
    //
    // These are up here because of the new Ansi rule about CASE jumps
    // over variable defines
    double x;
    double y;
    double psfX;
    double psfY;
    double psfRadialArcmin;
    double psfRadialMM;
    unsigned int radialBin;
    //
    // Count all photons
    //
    statByEnergy.tracedPhotonCount++;
    //
    // Handle photon by its status
    switch (photon.photonStatus())
        {
        case PHOTON_CONTINUES:
            // We should not see this case but we ignore it rather than
            // make a fuss.
            break;
        case PHOTON_HITS_OBSTRUCTION:
            statByEnergy.impactOnObstruction++;
            break;
        case PHOTON_HITS_OUTER_HOUSING:
            statByEnergy.impactOnOuterHousingCount++;
            break;
        case PHOTON_HITS_INNER_HOUSING:
            statByEnergy.impactOnInnerHousingCount++;
            break;
        case PHOTON_HITS_TOP_OF_MIRROR:
            statByEnergy.impactOnMirrorTop++;
            break;
        case PHOTON_ABSORBED_ON_OUTER_MIRROR:
            statByEnergy.absorbedOnFrontMirrorFaceCount++;
            break;
        case PHOTON_ABSORBED_ON_INNER_MIRROR:
            statByEnergy.absorbedOnBackMirrorFaceCount++;
            break;
        case PHOTON_REVERSES_Z_DIRECTION:
            statByEnergy.photonErrorCount++;
            break;
        case PHOTON_CAN_NOT_LEAVE_MIRROR:
            statByEnergy.photonCanNotLeaveMirrorCount++;
            break;
        case PHOTON_HIT_FOCAL_PLANE:
            // 
            // 1st do the counts for all photons that reach the focal plane
            //
            // Count all of them
            statByEnergy.impactOnFocalPlaneCount++;
            //
            // Break counts down by the pattern of hits on mirror layers
            switch (photon.getPhotonReflectionClass())
                {
                case NO_REFLECTION_CLASS:
                    // 
                    // This should not happen but we count them anyways.
                    statByEnergy.fpAbnormalPathCount++;
                    break;
                case ONE_LAYER_ONLY:
                    statByEnergy.fpOneLayerOnlyCount++;
                    break;
                case ONE_PER_LAYER:
                    statByEnergy.fpOnePerLayerCount++;
                    break;
                case NO_REFLECTIONS:
                    statByEnergy.fpNoReflectionCount++;
                    break;
                case ABNORMAL_PATH:
                    statByEnergy.fpAbnormalPathCount++;
                    break;
                case FIRST_LAYER_ONLY:
                    statByEnergy.fpOneLayerOnlyCount++;
                    statByEnergy.fpLayerOneOnlyCount++;
                    break;
                case SECOND_LAYER_ONLY:
                    statByEnergy.fpOneLayerOnlyCount++;
                    statByEnergy.fpLayerTwoOnlyCount++;
                    break;
                default:
                    throw unknownPhotonReflectionClass;
                    break;
                }
            //
            // 2nd do the counts for photons that fall in the focal plane mask
            //
            // Compute the photon location on the focal plane
            photon.getXY(&x,&y);
            //
            // Compute the photon location relative to the center of the
            // Point Spread Function
            psfX = x - sourceFocalPlaneXCenterMM;
            psfY = y - sourceFocalPlaneYCenterMM;
            //
            // Compute the radially symmetric photon radius within the PSF
            psfRadialMM = sqrt(psfX*psfX + psfY*psfY);
            psfRadialArcmin = psfRadialMM*focalPlaneMM2Arcmin;
            //
            // Is the photon within the core of the PSF?
            if (psfRadialArcmin*ARCMIN2RADIAN < psfCoreRadiusLimitRadian)
               {
               // Yes, count it in a special trace counter
               statByEnergy.tracedWithinPsfCoreRadiusCount++;
               }
            // 
            // Compute a bin for tracking the radial profile as a histogram
            // If the bin number falls inside the array size, add one to
            // the histogram bin, if outside the array, add one to the last
            // bin.
            radialBin = 
                   (unsigned int) (psfRadialArcmin*sourceRadialBinsPerArcmin);
            if (radialBin >= statByEnergy.impactBySourceRadialBin.size())
               {
               radialBin = statByEnergy.impactBySourceRadialBin.size()-1;
               }
            statByEnergy.impactBySourceRadialBin[radialBin]++;
            // Compute the PSF radial bin
            radialBin =
                   (unsigned int) (psfRadialMM*psfRadialProfileBinsPerMM);
            if (radialBin >= statByEnergy.psfRadialBin.size())
               {
               radialBin = statByEnergy.psfRadialBin.size()-1;
               }
            statByEnergy.psfRadialBin[radialBin]++;
            //
            // Is the photon inside the focal plane mask?
            if (focalPlaneMask.pointInside(x,y))
               {
               //
               // Keep track of the photon count in the focal plane mask
               // exactly as above.
               //
               statByEnergy.impactOnFPMaskCount++;
               switch (photon.getPhotonReflectionClass())
                   {
                   case NO_REFLECTION_CLASS:
                       // Again this shouldn't happen
                       statByEnergy.fpMaskAbnormalPathCount++;
                       break;
                   case ONE_LAYER_ONLY:
                       statByEnergy.fpMaskOneLayerOnlyCount++;
                       break;
                   case ONE_PER_LAYER:
                       statByEnergy.fpMaskOnePerLayerCount++;
                       break;
                   case NO_REFLECTIONS:
                       statByEnergy.fpMaskNoReflectionCount++;
                       break;
                   case ABNORMAL_PATH:
                       statByEnergy.fpMaskAbnormalPathCount++;
                       break;
                   case FIRST_LAYER_ONLY:
                       statByEnergy.fpMaskOneLayerOnlyCount++;
                       statByEnergy.fpMaskLayerOneOnlyCount++;
                       break;
                   case SECOND_LAYER_ONLY:
                       statByEnergy.fpMaskOneLayerOnlyCount++;
                       statByEnergy.fpMaskLayerTwoOnlyCount++;
                       break;
                   default:
                       throw unknownPhotonReflectionClass;
                       break;
                   }
               }
            break;
        case PHOTON_HITS_TOP_OF_COLLIMATOR:
            statByEnergy.impactOnCollimatorTop++;
            break;
	case PHOTON_CAN_NOT_LEAVE_COLLIMATOR:
	    statByEnergy.photonCanNotLeaveCollimatorCount++;
	    break;
        case PHOTON_ABSORBED_ON_OUTER_COLLIMATOR:
            statByEnergy.absorbedOnFrontCollimatorFaceCount++;
            break;
        case PHOTON_ABSORBED_ON_INNER_COLLIMATOR:
            statByEnergy.absorbedOnBackCollimatorFaceCount++;
            break;
        case ERROR:
            statByEnergy.photonErrorCount++;
            break;
        default:
            throw unknownPhotonStatus;
            break;
        }
}

void
XrrtStat::writeResultStatEnergyRow()
{
//
// Write the contents of a struct to the Results Statistics FITS file
//
// THERE ARE TWO OF THESE BECAUSE GCC 2.7.2.2 IS BROKEN
// When I tried to pass the struct reference as a parameter, g++ was
// profoundly unhappy. As I write this, I don't remember exactly why.
// I do know that g++2.7.2.2 has difficulties at times recognizing
// things that look like namespaces, probably due to the hack of
// namespaces in this complier version. At some point, this
// function and its twin below should be merged into one.

const int energyFitsColNum   =  1;
const int photonsFitsColNum  =  2;
const int corephtnFitsColNum =  3;
const int corefracFitsColNum =  4;
const int coreareaFitsColNum =  5;
const int fptotalFitsColNum  =  6;
const int fptotalfFitsColNum =  7;
const int fptotalaFitsColNum =  8;
const int fpoplFitsColNum    =  9;
const int fpoplfFitsColNum   = 10;
const int fpoplaFitsColNum   = 11;
const int fpoloFitsColNum    = 12;
const int fpolofFitsColNum   = 13;
const int fpoloaFitsColNum   = 14;
const int fplooFitsColNum    = 15;
const int fploofFitsColNum   = 16;
const int fplooaFitsColNum   = 17;
const int fpltoFitsColNum    = 18;
const int fpltofFitsColNum   = 19;
const int fpltoaFitsColNum   = 20;
const int fpnrFitsColNum     = 21;
const int fpnrfFitsColNum    = 22;
const int fpnraFitsColNum    = 23;
const int fpabnFitsColNum    = 24;
const int fpabnfFitsColNum   = 25;
const int fpabnaFitsColNum   = 26;
const int fpmtotalFitsColNum = 27;
const int fpmtotfFitsColNum  = 28;
const int fpmtotaFitsColNum  = 29;
const int fpmoplFitsColNum   = 30;
const int fpmoplfFitsColNum  = 31;
const int fpmoplaFitsColNum  = 32;
const int fpmoloFitsColNum   = 33;
const int fpmolofFitsColNum  = 34;
const int fpmoloaFitsColNum  = 35;
const int fpmlooFitsColNum   = 36;
const int fpmloofFitsColNum  = 37;
const int fpmlooaFitsColNum  = 38;
const int fpmltoFitsColNum   = 39;
const int fpmltofFitsColNum  = 40;
const int fpmltoaFitsColNum  = 41;
const int fpmnrFitsColNum    = 42;
const int fpmnrfFitsColNum   = 43;
const int fpmnraFitsColNum   = 44;
const int fpmabnFitsColNum   = 45;
const int fpmabnfFitsColNum  = 46;
const int fpmabnaFitsColNum  = 47;
const int mirtopFitsColNum   = 48;
const int mirtopfFitsColNum  = 49;
const int mirtopaFitsColNum  = 50;
const int shadowFitsColNum   = 51;
const int shadowfFitsColNum  = 52;
const int shadowaFitsColNum  = 53;
const int ohousingFitsColNum = 54;
const int ohousinfFitsColNum = 55;
const int ohousinaFitsColNum = 56;
const int ihousingFitsColNum = 57;
const int ihousinfFitsColNum = 58;
const int ihousinaFitsColNum = 59;
const int absfrontFitsColNum = 60;
const int absfronfFitsColNum = 61;
const int absfronaFitsColNum = 62;
const int absbackFitsColNum  = 63;
const int absbackfFitsColNum = 64;
const int absbackaFitsColNum = 65;
const int noleaveFitsColNum  = 66;
const int noleavefFitsColNum = 67;
const int noleaveaFitsColNum = 68;
const int errorFitsColNum    = 69;
const int errorfFitsColNum   = 70;
const int erroraFitsColNum   = 71;
const int psfradFitsColNum   = 72;
const int psfradbnFitsColNum = 73;

// Standard FITSIO calling sequence
int      fitsStatus;        // Standard error code return from FITSIO
int      fitsReturn;        // Standard return type from a FITSIO function

float floatArray[1];
long  longArray[1];

     if (!outputResultStat)
        {
        return;
        }

     // Update the row to write
     resultStatRowsWritten++;

     // Always
     fitsStatus = 0;

    // Output the contents of statByEnergy

    // energy
    float energy = (float) statByEnergy.energy;
    floatArray[0] = energy;
    fitsReturn = fits_write_col(resultStatFile, TFLOAT, energyFitsColNum,
                                resultStatRowsWritten, 1L, 1L,
                                floatArray, &fitsStatus);
     if (fitsStatus != NO_ERROR)
        {
        throw fitsStatus;
        }
     
     // photons
     long photons = (long) statByEnergy.tracedPhotonCount;
     longArray[0] = photons;
     fitsReturn = fits_write_col(resultStatFile, TLONG, photonsFitsColNum,
                                resultStatRowsWritten, 1L, 1L,
                                longArray, &fitsStatus);
     if (fitsStatus != NO_ERROR)
        {
        throw fitsStatus;
        }
     
     // corephtn
     long corephtn = (long) statByEnergy.tracedWithinPsfCoreRadiusCount;
     longArray[0] = corephtn;
     fitsReturn = fits_write_col(resultStatFile, TLONG, corephtnFitsColNum,
                                resultStatRowsWritten, 1L, 1L,
                                longArray, &fitsStatus);
     if (fitsStatus != NO_ERROR)
        {
        throw fitsStatus;
        }
     
     // corefrac
     float corefrac =
                (float) ((double) statByEnergy.tracedWithinPsfCoreRadiusCount /
                         (double) statByEnergy.tracedPhotonCount);
     floatArray[0] = corefrac;
     fitsReturn = fits_write_col(resultStatFile, TFLOAT, corefracFitsColNum,
                                resultStatRowsWritten, 1L, 1L,
                                floatArray, &fitsStatus);
     if (fitsStatus != NO_ERROR)
        {
        throw fitsStatus;
        }

     // corearea
     float corearea = corefrac * areaInMMSqr;
     floatArray[0] = corearea;
     fitsReturn = fits_write_col(resultStatFile, TFLOAT, coreareaFitsColNum,
                                resultStatRowsWritten, 1L, 1L,
                                floatArray, &fitsStatus);
     if (fitsStatus != NO_ERROR)
        {
        throw fitsStatus;
        }

     // fptotal
     long fptotal = (long) statByEnergy.impactOnFocalPlaneCount;
     longArray[0] = fptotal;
     fitsReturn = fits_write_col(resultStatFile, TLONG, fptotalFitsColNum,
                                resultStatRowsWritten, 1L, 1L,
                                longArray, &fitsStatus);
     if (fitsStatus != NO_ERROR)
        {
        throw fitsStatus;
        }

     // fptotalf
     float fptotalf =
                (float) ((double) statByEnergy.impactOnFocalPlaneCount /
                         (double) statByEnergy.tracedPhotonCount);
     floatArray[0] = fptotalf;
     fitsReturn = fits_write_col(resultStatFile, TFLOAT, fptotalfFitsColNum,
                                resultStatRowsWritten, 1L, 1L,
                                floatArray, &fitsStatus);
     if (fitsStatus != NO_ERROR)
        {
        throw fitsStatus;
        }

     // fptotala
     float fptotala = fptotalf * areaInMMSqr;
     floatArray[0] = fptotala;
     fitsReturn = fits_write_col(resultStatFile, TFLOAT, fptotalaFitsColNum,
                                resultStatRowsWritten, 1L, 1L,
                                floatArray, &fitsStatus);
     if (fitsStatus != NO_ERROR)
        {
        throw fitsStatus;
        }

     // fpopl
     long fpopl = (long) statByEnergy.fpOnePerLayerCount;
     longArray[0] = fpopl;
     fitsReturn = fits_write_col(resultStatFile, TLONG, fpoplFitsColNum,
                                resultStatRowsWritten, 1L, 1L,
                                longArray, &fitsStatus);
     if (fitsStatus != NO_ERROR)
        {
        throw fitsStatus;
        }

     // fpoplf
     float fpoplf =
                (float) ((double) statByEnergy.fpOnePerLayerCount /
                         (double) statByEnergy.tracedPhotonCount);
     floatArray[0] = fpoplf;
     fitsReturn = fits_write_col(resultStatFile, TFLOAT, fpoplfFitsColNum,
                                resultStatRowsWritten, 1L, 1L,
                                floatArray, &fitsStatus);
     if (fitsStatus != NO_ERROR)
        {
        throw fitsStatus;
        }

     // fpopla
     float fpopla = fpoplf * areaInMMSqr;
     floatArray[0] = fpopla;
     fitsReturn = fits_write_col(resultStatFile, TFLOAT, fpoplaFitsColNum,
                                resultStatRowsWritten, 1L, 1L,
                                floatArray, &fitsStatus);
     if (fitsStatus != NO_ERROR)
        {
        throw fitsStatus;
        }

     // fpolo
     long fpolo = (long) statByEnergy.fpOneLayerOnlyCount;
     longArray[0] = fpolo;
     fitsReturn = fits_write_col(resultStatFile, TLONG, fpoloFitsColNum,
                                resultStatRowsWritten, 1L, 1L,
                                longArray, &fitsStatus);
     if (fitsStatus != NO_ERROR)
        {
        throw fitsStatus;
        }

     // fpolof
     float fpolof =
                (float) ((double) statByEnergy.fpOneLayerOnlyCount /
                         (double) statByEnergy.tracedPhotonCount);
     floatArray[0] = fpolof;
     fitsReturn = fits_write_col(resultStatFile, TFLOAT, fpolofFitsColNum,
                                resultStatRowsWritten, 1L, 1L,
                                floatArray, &fitsStatus);
     if (fitsStatus != NO_ERROR)
        {
        throw fitsStatus;
        }

     // fpoloa
     float fpoloa = fpolof * areaInMMSqr;
     floatArray[0] = fpoloa;
     fitsReturn = fits_write_col(resultStatFile, TFLOAT, fpoloaFitsColNum,
                                resultStatRowsWritten, 1L, 1L,
                                floatArray, &fitsStatus);
     if (fitsStatus != NO_ERROR)
        {
        throw fitsStatus;
        }

     // fploo
     long fploo = (long) statByEnergy.fpLayerOneOnlyCount;
     longArray[0] = fploo;
     fitsReturn = fits_write_col(resultStatFile, TLONG, fplooFitsColNum,
                                resultStatRowsWritten, 1L, 1L,
                                longArray, &fitsStatus);
     if (fitsStatus != NO_ERROR)
        {
        throw fitsStatus;
        }

     // fploof
     float fploof =
                (float) ((double) statByEnergy.fpLayerOneOnlyCount /
                         (double) statByEnergy.tracedPhotonCount);
     floatArray[0] = fploof;
     fitsReturn = fits_write_col(resultStatFile, TFLOAT, fploofFitsColNum,
                                resultStatRowsWritten, 1L, 1L,
                                floatArray, &fitsStatus);
     if (fitsStatus != NO_ERROR)
        {
        throw fitsStatus;
        }

     // fplooa
     float fplooa = fploof * areaInMMSqr;
     floatArray[0] = fplooa;
     fitsReturn = fits_write_col(resultStatFile, TFLOAT, fplooaFitsColNum,
                                resultStatRowsWritten, 1L, 1L,
                                floatArray, &fitsStatus);
     if (fitsStatus != NO_ERROR)
        {
        throw fitsStatus;
        }

     // fplto
     long fplto = (long) statByEnergy.fpLayerTwoOnlyCount;
     longArray[0] = fplto;
     fitsReturn = fits_write_col(resultStatFile, TLONG, fpltoFitsColNum,
                                resultStatRowsWritten, 1L, 1L,
                                longArray, &fitsStatus);
     if (fitsStatus != NO_ERROR)
        {
        throw fitsStatus;
        }

     // fpltof
     float fpltof =
                (float) ((double) statByEnergy.fpLayerTwoOnlyCount /
                         (double) statByEnergy.tracedPhotonCount);
     floatArray[0] = fpltof;
     fitsReturn = fits_write_col(resultStatFile, TFLOAT, fpltofFitsColNum,
                                resultStatRowsWritten, 1L, 1L,
                                floatArray, &fitsStatus);
     if (fitsStatus != NO_ERROR)
        {
        throw fitsStatus;
        }

     // fpltoa
     float fpltoa = fpltof * areaInMMSqr;
     floatArray[0] = fpltoa;
     fitsReturn = fits_write_col(resultStatFile, TFLOAT, fpltoaFitsColNum,
                                resultStatRowsWritten, 1L, 1L,
                                floatArray, &fitsStatus);
     if (fitsStatus != NO_ERROR)
        {
        throw fitsStatus;
        }

     // fpnr
     long fpnr = (long) statByEnergy.fpNoReflectionCount;
     longArray[0] = fpnr;
     fitsReturn = fits_write_col(resultStatFile, TLONG, fpnrFitsColNum,
                                resultStatRowsWritten, 1L, 1L,
                                longArray, &fitsStatus);
     if (fitsStatus != NO_ERROR)
        {
        throw fitsStatus;
        }

     // fpnrf
     float fpnrf =
                (float) ((double) statByEnergy.fpNoReflectionCount /
                         (double) statByEnergy.tracedPhotonCount);
     floatArray[0] = fpnrf;
     fitsReturn = fits_write_col(resultStatFile, TFLOAT, fpnrfFitsColNum,
                                resultStatRowsWritten, 1L, 1L,
                                floatArray, &fitsStatus);
     if (fitsStatus != NO_ERROR)
        {
        throw fitsStatus;
        }

     // fpnra
     float fpnra = fpnrf * areaInMMSqr;
     floatArray[0] = fpnra;
     fitsReturn = fits_write_col(resultStatFile, TFLOAT, fpnraFitsColNum,
                                resultStatRowsWritten, 1L, 1L,
                                floatArray, &fitsStatus);
     if (fitsStatus != NO_ERROR)
        {
        throw fitsStatus;
        }

     // fpabn
     long fpabn = (long) statByEnergy.fpAbnormalPathCount;
     longArray[0] = fpabn;
     fitsReturn = fits_write_col(resultStatFile, TLONG, fpabnFitsColNum,
                                resultStatRowsWritten, 1L, 1L,
                                longArray, &fitsStatus);
     if (fitsStatus != NO_ERROR)
        {
        throw fitsStatus;
        }

     // fpabnf
     float fpabnf =
                (float) ((double) statByEnergy.fpAbnormalPathCount /
                         (double) statByEnergy.tracedPhotonCount);
     floatArray[0] = fpabnf;
     fitsReturn = fits_write_col(resultStatFile, TFLOAT, fpabnfFitsColNum,
                                resultStatRowsWritten, 1L, 1L,
                                floatArray, &fitsStatus);
     if (fitsStatus != NO_ERROR)
        {
        throw fitsStatus;
        }

     // fpabna
     float fpabna = fpabnf * areaInMMSqr;
     floatArray[0] = fpabna;
     fitsReturn = fits_write_col(resultStatFile, TFLOAT, fpabnaFitsColNum,
                                resultStatRowsWritten, 1L, 1L,
                                floatArray, &fitsStatus);
     if (fitsStatus != NO_ERROR)
        {
        throw fitsStatus;
        }

     // fpmtotal
     long fpmtotal = (long) statByEnergy.impactOnFPMaskCount;
     longArray[0] = fpmtotal;
     fitsReturn = fits_write_col(resultStatFile, TLONG, fpmtotalFitsColNum,
                                resultStatRowsWritten, 1L, 1L,
                                longArray, &fitsStatus);
     if (fitsStatus != NO_ERROR)
        {
        throw fitsStatus;
        }

     // fpmtotf
     float fpmtotf =
                (float) ((double) statByEnergy.impactOnFPMaskCount /
                         (double) statByEnergy.tracedPhotonCount);
     floatArray[0] = fpmtotf;
     fitsReturn = fits_write_col(resultStatFile, TFLOAT, fpmtotfFitsColNum,
                                resultStatRowsWritten, 1L, 1L,
                                floatArray, &fitsStatus);
     if (fitsStatus != NO_ERROR)
        {
        throw fitsStatus;
        }

     // fpmtota
     float fpmtota = fpmtotf * areaInMMSqr;
     floatArray[0] = fpmtota;
     fitsReturn = fits_write_col(resultStatFile, TFLOAT, fpmtotaFitsColNum,
                                resultStatRowsWritten, 1L, 1L,
                                floatArray, &fitsStatus);
     if (fitsStatus != NO_ERROR)
        {
        throw fitsStatus;
        }

     // fpmopl
     long fpmopl = (long) statByEnergy.fpMaskOnePerLayerCount;
     longArray[0] = fpmopl;
     fitsReturn = fits_write_col(resultStatFile, TLONG, fpmoplFitsColNum,
                                resultStatRowsWritten, 1L, 1L,
                                longArray, &fitsStatus);
     if (fitsStatus != NO_ERROR)
        {
        throw fitsStatus;
        }

     // fpmoplf
     float fpmoplf =
                (float) ((double) statByEnergy.fpMaskOnePerLayerCount /
                         (double) statByEnergy.tracedPhotonCount);
     floatArray[0] = fpmoplf;
     fitsReturn = fits_write_col(resultStatFile, TFLOAT, fpmoplfFitsColNum,
                                resultStatRowsWritten, 1L, 1L,
                                floatArray, &fitsStatus);
     if (fitsStatus != NO_ERROR)
        {
        throw fitsStatus;
        }

     // fpmopla
     float fpmopla = fpmoplf * areaInMMSqr;
     floatArray[0] = fpmopla;
     fitsReturn = fits_write_col(resultStatFile, TFLOAT, fpmoplaFitsColNum,
                                resultStatRowsWritten, 1L, 1L,
                                floatArray, &fitsStatus);
     if (fitsStatus != NO_ERROR)
        {
        throw fitsStatus;
        }

     // fpmolo
     long fpmolo = (long) statByEnergy.fpMaskOneLayerOnlyCount;
     longArray[0] = fpmolo;
     fitsReturn = fits_write_col(resultStatFile, TLONG, fpmoloFitsColNum,
                                resultStatRowsWritten, 1L, 1L,
                                longArray, &fitsStatus);
     if (fitsStatus != NO_ERROR)
        {
        throw fitsStatus;
        }

     // fpmolof
     float fpmolof =
                (float) ((double) statByEnergy.fpMaskOneLayerOnlyCount /
                         (double) statByEnergy.tracedPhotonCount);
     floatArray[0] = fpmolof;
     fitsReturn = fits_write_col(resultStatFile, TFLOAT, fpmolofFitsColNum,
                                resultStatRowsWritten, 1L, 1L,
                                floatArray, &fitsStatus);
     if (fitsStatus != NO_ERROR)
        {
        throw fitsStatus;
        }

     // fpmoloa
     float fpmoloa = fpmolof * areaInMMSqr;
     floatArray[0] = fpmoloa;
     fitsReturn = fits_write_col(resultStatFile, TFLOAT, fpmoloaFitsColNum,
                                resultStatRowsWritten, 1L, 1L,
                                floatArray, &fitsStatus);
     if (fitsStatus != NO_ERROR)
        {
        throw fitsStatus;
        }

     // fpmloo
     long fpmloo = (long) statByEnergy.fpMaskLayerOneOnlyCount;
     longArray[0] = fpmloo;
     fitsReturn = fits_write_col(resultStatFile, TLONG, fpmlooFitsColNum,
                                resultStatRowsWritten, 1L, 1L,
                                longArray, &fitsStatus);
     if (fitsStatus != NO_ERROR)
        {
        throw fitsStatus;
        }

     // fpmloof
     float fpmloof =
                (float) ((double) statByEnergy.fpMaskLayerOneOnlyCount /
                         (double) statByEnergy.tracedPhotonCount);
     floatArray[0] = fpmloof;
     fitsReturn = fits_write_col(resultStatFile, TFLOAT, fpmloofFitsColNum,
                                resultStatRowsWritten, 1L, 1L,
                                floatArray, &fitsStatus);
     if (fitsStatus != NO_ERROR)
        {
        throw fitsStatus;
        }

     // fpmlooa
     float fpmlooa = fpmloof * areaInMMSqr;
     floatArray[0] = fpmlooa;
     fitsReturn = fits_write_col(resultStatFile, TFLOAT, fpmlooaFitsColNum,
                                resultStatRowsWritten, 1L, 1L,
                                floatArray, &fitsStatus);
     if (fitsStatus != NO_ERROR)
        {
        throw fitsStatus;
        }

     // fpmlto
     long fpmlto = (long) statByEnergy.fpMaskLayerTwoOnlyCount;
     longArray[0] = fpmlto;
     fitsReturn = fits_write_col(resultStatFile, TLONG, fpmltoFitsColNum,
                                resultStatRowsWritten, 1L, 1L,
                                longArray, &fitsStatus);
     if (fitsStatus != NO_ERROR)
        {
        throw fitsStatus;
        }

     // fpmltof
     float fpmltof =
                (float) ((double) statByEnergy.fpMaskLayerTwoOnlyCount /
                         (double) statByEnergy.tracedPhotonCount);
     floatArray[0] = fpmltof;
     fitsReturn = fits_write_col(resultStatFile, TFLOAT, fpmltofFitsColNum,
                                resultStatRowsWritten, 1L, 1L,
                                floatArray, &fitsStatus);
     if (fitsStatus != NO_ERROR)
        {
        throw fitsStatus;
        }

     // fpmltoa
     float fpmltoa = fpmltof * areaInMMSqr;
     floatArray[0] = fpmltoa;
     fitsReturn = fits_write_col(resultStatFile, TFLOAT, fpmltoaFitsColNum,
                                resultStatRowsWritten, 1L, 1L,
                                floatArray, &fitsStatus);
     if (fitsStatus != NO_ERROR)
        {
        throw fitsStatus;
        }

     // fpmnr
     long fpmnr = (long) statByEnergy.fpMaskNoReflectionCount;
     longArray[0] = fpmnr;
     fitsReturn = fits_write_col(resultStatFile, TLONG, fpmnrFitsColNum,
                                resultStatRowsWritten, 1L, 1L,
                                longArray, &fitsStatus);
     if (fitsStatus != NO_ERROR)
        {
        throw fitsStatus;
        }

     // fpmnrf
     float fpmnrf =
                (float) ((double) statByEnergy.fpMaskNoReflectionCount /
                         (double) statByEnergy.tracedPhotonCount);
     floatArray[0] = fpmnrf;
     fitsReturn = fits_write_col(resultStatFile, TFLOAT, fpmnrfFitsColNum,
                                resultStatRowsWritten, 1L, 1L,
                                floatArray, &fitsStatus);
     if (fitsStatus != NO_ERROR)
        {
        throw fitsStatus;
        }

     // fpmnra
     float fpmnra = fpmnrf * areaInMMSqr;
     floatArray[0] = fpmnra;
     fitsReturn = fits_write_col(resultStatFile, TFLOAT, fpmnraFitsColNum,
                                resultStatRowsWritten, 1L, 1L,
                                floatArray, &fitsStatus);
     if (fitsStatus != NO_ERROR)
        {
        throw fitsStatus;
        }

     // fpmabn
     long fpmabn = (long) statByEnergy.fpMaskAbnormalPathCount;
     longArray[0] = fpmabn;
     fitsReturn = fits_write_col(resultStatFile, TLONG, fpmabnFitsColNum,
                                resultStatRowsWritten, 1L, 1L,
                                longArray, &fitsStatus);
     if (fitsStatus != NO_ERROR)
        {
        throw fitsStatus;
        }

     // fpmabnf
     float fpmabnf =
                (float) ((double) statByEnergy.fpMaskAbnormalPathCount /
                         (double) statByEnergy.tracedPhotonCount);
     floatArray[0] = fpmabnf;
     fitsReturn = fits_write_col(resultStatFile, TFLOAT, fpmabnfFitsColNum,
                                resultStatRowsWritten, 1L, 1L,
                                floatArray, &fitsStatus);
     if (fitsStatus != NO_ERROR)
        {
        throw fitsStatus;
        }

     // fpmabna
     float fpmabna = fpmabnf * areaInMMSqr;
     floatArray[0] = fpmabna;
     fitsReturn = fits_write_col(resultStatFile, TFLOAT, fpmabnaFitsColNum,
                                resultStatRowsWritten, 1L, 1L,
                                floatArray, &fitsStatus);
     if (fitsStatus != NO_ERROR)
        {
        throw fitsStatus;
        }

     // mirtop
     long mirtop = (long) statByEnergy.impactOnMirrorTop;
     longArray[0] = mirtop;
     fitsReturn = fits_write_col(resultStatFile, TLONG, mirtopFitsColNum,
                                resultStatRowsWritten, 1L, 1L,
                                longArray, &fitsStatus);
     if (fitsStatus != NO_ERROR)
        {
        throw fitsStatus;
        }

     // mirtopf
     float mirtopf =
                (float) ((double) statByEnergy.impactOnMirrorTop /
                         (double) statByEnergy.tracedPhotonCount);
     floatArray[0] = mirtopf;
     fitsReturn = fits_write_col(resultStatFile, TFLOAT, mirtopfFitsColNum,
                                resultStatRowsWritten, 1L, 1L,
                                floatArray, &fitsStatus);
     if (fitsStatus != NO_ERROR)
        {
        throw fitsStatus;
        }

     // mirtopa
     float mirtopa = mirtopf * areaInMMSqr;
     floatArray[0] = mirtopa;
     fitsReturn = fits_write_col(resultStatFile, TFLOAT, mirtopaFitsColNum,
                                resultStatRowsWritten, 1L, 1L,
                                floatArray, &fitsStatus);
     if (fitsStatus != NO_ERROR)
        {
        throw fitsStatus;
        }

     // shadow
     long shadow = (long) statByEnergy.impactOnObstruction;
     longArray[0] = shadow;
     fitsReturn = fits_write_col(resultStatFile, TLONG, shadowFitsColNum,
                                resultStatRowsWritten, 1L, 1L,
                                longArray, &fitsStatus);
     if (fitsStatus != NO_ERROR)
        {
        throw fitsStatus;
        }

     // shadowf
     float shadowf =
                (float) ((double) statByEnergy.impactOnObstruction /
                         (double) statByEnergy.tracedPhotonCount);
     floatArray[0] = shadowf;
     fitsReturn = fits_write_col(resultStatFile, TFLOAT, shadowfFitsColNum,
                                resultStatRowsWritten, 1L, 1L,
                                floatArray, &fitsStatus);
     if (fitsStatus != NO_ERROR)
        {
        throw fitsStatus;
        }

     // shadowa
     float shadowa = shadowf * areaInMMSqr;
     floatArray[0] = shadowa;
     fitsReturn = fits_write_col(resultStatFile, TFLOAT, shadowaFitsColNum,
                                resultStatRowsWritten, 1L, 1L,
                                floatArray, &fitsStatus);
     if (fitsStatus != NO_ERROR)
        {
        throw fitsStatus;
        }

     // ohousing
     long ohousing = (long) statByEnergy.impactOnOuterHousingCount;
     longArray[0] = ohousing;
     fitsReturn = fits_write_col(resultStatFile, TLONG, ohousingFitsColNum,
                                resultStatRowsWritten, 1L, 1L,
                                longArray, &fitsStatus);
     if (fitsStatus != NO_ERROR)
        {
        throw fitsStatus;
        }

     // ohousinf
     float ohousinf =
                (float) ((double) statByEnergy.impactOnOuterHousingCount /
                         (double) statByEnergy.tracedPhotonCount);
     floatArray[0] = ohousinf;
     fitsReturn = fits_write_col(resultStatFile, TFLOAT, ohousinfFitsColNum,
                                resultStatRowsWritten, 1L, 1L,
                                floatArray, &fitsStatus);
     if (fitsStatus != NO_ERROR)
        {
        throw fitsStatus;
        }

     // ohousina
     float ohousina = ohousinf * areaInMMSqr;
     floatArray[0] = ohousina;
     fitsReturn = fits_write_col(resultStatFile, TFLOAT, ohousinaFitsColNum,
                                resultStatRowsWritten, 1L, 1L,
                                floatArray, &fitsStatus);
     if (fitsStatus != NO_ERROR)
        {
        throw fitsStatus;
        }

     // ihousing
     long ihousing = (long) statByEnergy.impactOnInnerHousingCount;
     longArray[0] = ihousing;
     fitsReturn = fits_write_col(resultStatFile, TLONG, ihousingFitsColNum,
                                resultStatRowsWritten, 1L, 1L,
                                longArray, &fitsStatus);
     if (fitsStatus != NO_ERROR)
        {
        throw fitsStatus;
        }

     // ihousinf
     float ihousinf =
                (float) ((double) statByEnergy.impactOnInnerHousingCount /
                         (double) statByEnergy.tracedPhotonCount);
     floatArray[0] = ihousinf;
     fitsReturn = fits_write_col(resultStatFile, TFLOAT, ihousinfFitsColNum,
                                resultStatRowsWritten, 1L, 1L,
                                floatArray, &fitsStatus);
     if (fitsStatus != NO_ERROR)
        {
        throw fitsStatus;
        }

     // ihousina
     float ihousina = ihousinf * areaInMMSqr;
     floatArray[0] = ihousina;
     fitsReturn = fits_write_col(resultStatFile, TFLOAT, ihousinaFitsColNum,
                                resultStatRowsWritten, 1L, 1L,
                                floatArray, &fitsStatus);
     if (fitsStatus != NO_ERROR)
        {
        throw fitsStatus;
        }

     // absfront
     long absfront = (long) statByEnergy.absorbedOnFrontMirrorFaceCount;
     longArray[0] = absfront;
     fitsReturn = fits_write_col(resultStatFile, TLONG, absfrontFitsColNum,
                                resultStatRowsWritten, 1L, 1L,
                                longArray, &fitsStatus);
     if (fitsStatus != NO_ERROR)
        {
        throw fitsStatus;
        }

     // absfronf
     float absfronf =
                (float) ((double) statByEnergy.absorbedOnFrontMirrorFaceCount /
                         (double) statByEnergy.tracedPhotonCount);
     floatArray[0] = absfronf;
     fitsReturn = fits_write_col(resultStatFile, TFLOAT, absfronfFitsColNum,
                                resultStatRowsWritten, 1L, 1L,
                                floatArray, &fitsStatus);
     if (fitsStatus != NO_ERROR)
        {
        throw fitsStatus;
        }

     // absfrona
     float absfrona = absfronf * areaInMMSqr;
     floatArray[0] = absfrona;
     fitsReturn = fits_write_col(resultStatFile, TFLOAT, absfronaFitsColNum,
                                resultStatRowsWritten, 1L, 1L,
                                floatArray, &fitsStatus);
     if (fitsStatus != NO_ERROR)
        {
        throw fitsStatus;
        }

     // absback
     long absback = (long) statByEnergy.absorbedOnBackMirrorFaceCount;
     longArray[0] = absback;
     fitsReturn = fits_write_col(resultStatFile, TLONG, absbackFitsColNum,
                                resultStatRowsWritten, 1L, 1L,
                                longArray, &fitsStatus);
     if (fitsStatus != NO_ERROR)
        {
        throw fitsStatus;
        }

     // absbackf
     float absbackf =
                (float) ((double) statByEnergy.absorbedOnBackMirrorFaceCount /
                         (double) statByEnergy.tracedPhotonCount);
     floatArray[0] = absbackf;
     fitsReturn = fits_write_col(resultStatFile, TFLOAT, absbackfFitsColNum,
                                resultStatRowsWritten, 1L, 1L,
                                floatArray, &fitsStatus);
     if (fitsStatus != NO_ERROR)
        {
        throw fitsStatus;
        }

     // absbacka
     float absbacka = absbackf * areaInMMSqr;
     floatArray[0] = absbacka;
     fitsReturn = fits_write_col(resultStatFile, TFLOAT, absbackaFitsColNum,
                                resultStatRowsWritten, 1L, 1L,
                                floatArray, &fitsStatus);
     if (fitsStatus != NO_ERROR)
        {
        throw fitsStatus;
        }

     // noleave
     long noleave = (long) statByEnergy.photonCanNotLeaveMirrorCount;
     longArray[0] = noleave;
     fitsReturn = fits_write_col(resultStatFile, TLONG, noleaveFitsColNum,
                                resultStatRowsWritten, 1L, 1L,
                                longArray, &fitsStatus);
     if (fitsStatus != NO_ERROR)
        {
        throw fitsStatus;
        }

     // noleavef
     float noleavef =
                (float) ((double) statByEnergy.photonCanNotLeaveMirrorCount /
                         (double) statByEnergy.tracedPhotonCount);
     floatArray[0] = noleavef;
     fitsReturn = fits_write_col(resultStatFile, TFLOAT, noleavefFitsColNum,
                                resultStatRowsWritten, 1L, 1L,
                                floatArray, &fitsStatus);
     if (fitsStatus != NO_ERROR)
        {
        throw fitsStatus;
        }

     // noleavea
     float noleavea = noleavef * areaInMMSqr;
     floatArray[0] = noleavea;
     fitsReturn = fits_write_col(resultStatFile, TFLOAT, noleaveaFitsColNum,
                                resultStatRowsWritten, 1L, 1L,
                                floatArray, &fitsStatus);
     if (fitsStatus != NO_ERROR)
        {
        throw fitsStatus;
        }

     // error
     long error = (long) statByEnergy.photonErrorCount;
     longArray[0] = error;
     fitsReturn = fits_write_col(resultStatFile, TLONG, errorFitsColNum,
                                resultStatRowsWritten, 1L, 1L,
                                longArray, &fitsStatus);
     if (fitsStatus != NO_ERROR)
        {
        throw fitsStatus;
        }

     // errorf
     float errorf =
                (float) ((double) statByEnergy.photonErrorCount /
                         (double) statByEnergy.tracedPhotonCount);
     floatArray[0] = errorf;
     fitsReturn = fits_write_col(resultStatFile, TFLOAT, errorfFitsColNum,
                                resultStatRowsWritten, 1L, 1L,
                                floatArray, &fitsStatus);
     if (fitsStatus != NO_ERROR)
        {
        throw fitsStatus;
        }

     // errora
     float errora = errorf * areaInMMSqr;
     floatArray[0] = errora;
     fitsReturn = fits_write_col(resultStatFile, TFLOAT, erroraFitsColNum,
                                resultStatRowsWritten, 1L, 1L,
                                floatArray, &fitsStatus);
     if (fitsStatus != NO_ERROR)
        {
        throw fitsStatus;
        }

     // psfrad
     long *psfData = new long[sourceRadialProfileBins];
     for (int i = 0; i < sourceRadialProfileBins; i++)
        {
        psfData[i]  = (long) statByEnergy.impactBySourceRadialBin[i];
        }
     fitsReturn = fits_write_col(resultStatFile, TLONG, psfradFitsColNum,
                                resultStatRowsWritten, 1L, 
                                sourceRadialProfileBins,
                                psfData, &fitsStatus);
     delete [] psfData;
     if (fitsStatus != NO_ERROR)
        {
        throw fitsStatus;
        }

    // psfradbn
    float *psfDataBin = new float [sourceRadialProfileBins];
    for (int i = 0; i < sourceRadialProfileBins; i++)
        {
        psfDataBin[i]  = (float) i / (float) sourceRadialBinsPerArcmin;
        }
     fitsReturn = fits_write_col(resultStatFile, TFLOAT, psfradbnFitsColNum,
                                resultStatRowsWritten, 1L,
                                sourceRadialProfileBins,
                                psfDataBin, &fitsStatus);
     delete [] psfDataBin;
     if (fitsStatus != NO_ERROR)
        {
        throw fitsStatus;
        }
}

void
XrrtStat::writeResultStatTotalRow()
{
//
// Write the contents of a struct to the Results Statistics FITS file
//
// THERE ARE TWO OF THESE BECAUSE GCC 2.7.2.2 IS BROKEN
// When I tried to pass the struct reference as a parameter, g++ was
// profoundly unhappy. As I write this, I don't remember exactly why.
// I do know that g++2.7.2.2 has difficulties at times recognizing
// things that look like namespaces, probably due to the hack of
// namespaces in this complier version. At some point, this
// function and its twin above should be merged into one.
// 
const int energyFitsColNum   =  1;
const int photonsFitsColNum  =  2;
const int corephtnFitsColNum =  3;
const int corefracFitsColNum =  4;
const int coreareaFitsColNum =  5;
const int fptotalFitsColNum  =  6;
const int fptotalfFitsColNum =  7;
const int fptotalaFitsColNum =  8;
const int fpoplFitsColNum    =  9;
const int fpoplfFitsColNum   = 10;
const int fpoplaFitsColNum   = 11;
const int fpoloFitsColNum    = 12;
const int fpolofFitsColNum   = 13;
const int fpoloaFitsColNum   = 14;
const int fplooFitsColNum    = 15;
const int fploofFitsColNum   = 16;
const int fplooaFitsColNum   = 17;
const int fpltoFitsColNum    = 18;
const int fpltofFitsColNum   = 19;
const int fpltoaFitsColNum   = 20;
const int fpnrFitsColNum     = 21;
const int fpnrfFitsColNum    = 22;
const int fpnraFitsColNum    = 23;
const int fpabnFitsColNum    = 24;
const int fpabnfFitsColNum   = 25;
const int fpabnaFitsColNum   = 26;
const int fpmtotalFitsColNum = 27;
const int fpmtotfFitsColNum  = 28;
const int fpmtotaFitsColNum  = 29;
const int fpmoplFitsColNum   = 30;
const int fpmoplfFitsColNum  = 31;
const int fpmoplaFitsColNum  = 32;
const int fpmoloFitsColNum   = 33;
const int fpmolofFitsColNum  = 34;
const int fpmoloaFitsColNum  = 35;
const int fpmlooFitsColNum   = 36;
const int fpmloofFitsColNum  = 37;
const int fpmlooaFitsColNum  = 38;
const int fpmltoFitsColNum   = 39;
const int fpmltofFitsColNum  = 40;
const int fpmltoaFitsColNum  = 41;
const int fpmnrFitsColNum    = 42;
const int fpmnrfFitsColNum   = 43;
const int fpmnraFitsColNum   = 44;
const int fpmabnFitsColNum   = 45;
const int fpmabnfFitsColNum  = 46;
const int fpmabnaFitsColNum  = 47;
const int mirtopFitsColNum   = 48;
const int mirtopfFitsColNum  = 49;
const int mirtopaFitsColNum  = 50;
const int shadowFitsColNum   = 51;
const int shadowfFitsColNum  = 52;
const int shadowaFitsColNum  = 53;
const int ohousingFitsColNum = 54;
const int ohousinfFitsColNum = 55;
const int ohousinaFitsColNum = 56;
const int ihousingFitsColNum = 57;
const int ihousinfFitsColNum = 58;
const int ihousinaFitsColNum = 59;
const int absfrontFitsColNum = 60;
const int absfronfFitsColNum = 61;
const int absfronaFitsColNum = 62;
const int absbackFitsColNum  = 63;
const int absbackfFitsColNum = 64;
const int absbackaFitsColNum = 65;
const int noleaveFitsColNum  = 66;
const int noleavefFitsColNum = 67;
const int noleaveaFitsColNum = 68;
const int errorFitsColNum    = 69;
const int errorfFitsColNum   = 70;
const int erroraFitsColNum   = 71;
const int psfradFitsColNum   = 72;
const int psfradbnFitsColNum = 73;

// Standard FITSIO calling sequence
int      fitsStatus;        // Standard error code return from FITSIO
int      fitsReturn;        // Standard return type from a FITSIO function

float floatArray[1];
long  longArray[1];

     if (!outputResultStat)
        {
        return;
        }

     // Update the row to write
     resultStatRowsWritten++;

     // Always
     fitsStatus = 0;

    // Output the contents of total

    // energy
    float energy = (float) total.energy;
    floatArray[0] = energy;
    fitsReturn = fits_write_col(resultStatFile, TFLOAT, energyFitsColNum,
                                resultStatRowsWritten, 1L, 1L,
                                floatArray, &fitsStatus);
     if (fitsStatus != NO_ERROR)
        {
        throw fitsStatus;
        }
     
     // photons
     long photons = (long) total.tracedPhotonCount;
     longArray[0] = photons;
     fitsReturn = fits_write_col(resultStatFile, TLONG, photonsFitsColNum,
                                resultStatRowsWritten, 1L, 1L,
                                longArray, &fitsStatus);
     if (fitsStatus != NO_ERROR)
        {
        throw fitsStatus;
        }
     
     // corephtn
     long corephtn = (long) total.tracedWithinPsfCoreRadiusCount;
     longArray[0] = corephtn;
     fitsReturn = fits_write_col(resultStatFile, TLONG, corephtnFitsColNum,
                                resultStatRowsWritten, 1L, 1L,
                                longArray, &fitsStatus);
     if (fitsStatus != NO_ERROR)
        {
        throw fitsStatus;
        }
     
     // corefrac
     float corefrac =
                (float) ((double) total.tracedWithinPsfCoreRadiusCount /
                         (double) total.tracedPhotonCount);
     floatArray[0] = corefrac;
     fitsReturn = fits_write_col(resultStatFile, TFLOAT, corefracFitsColNum,
                                resultStatRowsWritten, 1L, 1L,
                                floatArray, &fitsStatus);
     if (fitsStatus != NO_ERROR)
        {
        throw fitsStatus;
        }

     // corearea
     float corearea = corefrac * areaInMMSqr;
     floatArray[0] = corearea;
     fitsReturn = fits_write_col(resultStatFile, TFLOAT, coreareaFitsColNum,
                                resultStatRowsWritten, 1L, 1L,
                                floatArray, &fitsStatus);
     if (fitsStatus != NO_ERROR)
        {
        throw fitsStatus;
        }

     // fptotal
     long fptotal = (long) total.impactOnFocalPlaneCount;
     longArray[0] = fptotal;
     fitsReturn = fits_write_col(resultStatFile, TLONG, fptotalFitsColNum,
                                resultStatRowsWritten, 1L, 1L,
                                longArray, &fitsStatus);
     if (fitsStatus != NO_ERROR)
        {
        throw fitsStatus;
        }

     // fptotalf
     float fptotalf =
                (float) ((double) total.impactOnFocalPlaneCount /
                         (double) total.tracedPhotonCount);
     floatArray[0] = fptotalf;
     fitsReturn = fits_write_col(resultStatFile, TFLOAT, fptotalfFitsColNum,
                                resultStatRowsWritten, 1L, 1L,
                                floatArray, &fitsStatus);
     if (fitsStatus != NO_ERROR)
        {
        throw fitsStatus;
        }

     // fptotala
     float fptotala = fptotalf * areaInMMSqr;
     floatArray[0] = fptotala;
     fitsReturn = fits_write_col(resultStatFile, TFLOAT, fptotalaFitsColNum,
                                resultStatRowsWritten, 1L, 1L,
                                floatArray, &fitsStatus);
     if (fitsStatus != NO_ERROR)
        {
        throw fitsStatus;
        }

     // fpopl
     long fpopl = (long) total.fpOnePerLayerCount;
     longArray[0] = fpopl;
     fitsReturn = fits_write_col(resultStatFile, TLONG, fpoplFitsColNum,
                                resultStatRowsWritten, 1L, 1L,
                                longArray, &fitsStatus);
     if (fitsStatus != NO_ERROR)
        {
        throw fitsStatus;
        }

     // fpoplf
     float fpoplf =
                (float) ((double) total.fpOnePerLayerCount /
                         (double) total.tracedPhotonCount);
     floatArray[0] = fpoplf;
     fitsReturn = fits_write_col(resultStatFile, TFLOAT, fpoplfFitsColNum,
                                resultStatRowsWritten, 1L, 1L,
                                floatArray, &fitsStatus);
     if (fitsStatus != NO_ERROR)
        {
        throw fitsStatus;
        }

     // fpopla
     float fpopla = fpoplf * areaInMMSqr;
     floatArray[0] = fpopla;
     fitsReturn = fits_write_col(resultStatFile, TFLOAT, fpoplaFitsColNum,
                                resultStatRowsWritten, 1L, 1L,
                                floatArray, &fitsStatus);
     if (fitsStatus != NO_ERROR)
        {
        throw fitsStatus;
        }

     // fpolo
     long fpolo = (long) total.fpOneLayerOnlyCount;
     longArray[0] = fpolo;
     fitsReturn = fits_write_col(resultStatFile, TLONG, fpoloFitsColNum,
                                resultStatRowsWritten, 1L, 1L,
                                longArray, &fitsStatus);
     if (fitsStatus != NO_ERROR)
        {
        throw fitsStatus;
        }

     // fpolof
     float fpolof =
                (float) ((double) total.fpOneLayerOnlyCount /
                         (double) total.tracedPhotonCount);
     floatArray[0] = fpolof;
     fitsReturn = fits_write_col(resultStatFile, TFLOAT, fpolofFitsColNum,
                                resultStatRowsWritten, 1L, 1L,
                                floatArray, &fitsStatus);
     if (fitsStatus != NO_ERROR)
        {
        throw fitsStatus;
        }

     // fpoloa
     float fpoloa = fpolof * areaInMMSqr;
     floatArray[0] = fpoloa;
     fitsReturn = fits_write_col(resultStatFile, TFLOAT, fpoloaFitsColNum,
                                resultStatRowsWritten, 1L, 1L,
                                floatArray, &fitsStatus);
     if (fitsStatus != NO_ERROR)
        {
        throw fitsStatus;
        }

     // fploo
     long fploo = (long) total.fpLayerOneOnlyCount;
     longArray[0] = fploo;
     fitsReturn = fits_write_col(resultStatFile, TLONG, fplooFitsColNum,
                                resultStatRowsWritten, 1L, 1L,
                                longArray, &fitsStatus);
     if (fitsStatus != NO_ERROR)
        {
        throw fitsStatus;
        }

     // fploof
     float fploof =
                (float) ((double) total.fpLayerOneOnlyCount /
                         (double) total.tracedPhotonCount);
     floatArray[0] = fploof;
     fitsReturn = fits_write_col(resultStatFile, TFLOAT, fploofFitsColNum,
                                resultStatRowsWritten, 1L, 1L,
                                floatArray, &fitsStatus);
     if (fitsStatus != NO_ERROR)
        {
        throw fitsStatus;
        }

     // fplooa
     float fplooa = fploof * areaInMMSqr;
     floatArray[0] = fplooa;
     fitsReturn = fits_write_col(resultStatFile, TFLOAT, fplooaFitsColNum,
                                resultStatRowsWritten, 1L, 1L,
                                floatArray, &fitsStatus);
     if (fitsStatus != NO_ERROR)
        {
        throw fitsStatus;
        }

     // fplto
     long fplto = (long) total.fpLayerTwoOnlyCount;
     longArray[0] = fplto;
     fitsReturn = fits_write_col(resultStatFile, TLONG, fpltoFitsColNum,
                                resultStatRowsWritten, 1L, 1L,
                                longArray, &fitsStatus);
     if (fitsStatus != NO_ERROR)
        {
        throw fitsStatus;
        }

     // fpltof
     float fpltof =
                (float) ((double) total.fpMaskLayerTwoOnlyCount /
                         (double) total.tracedPhotonCount);
     floatArray[0] = fpltof;
     fitsReturn = fits_write_col(resultStatFile, TFLOAT, fpltofFitsColNum,
                                resultStatRowsWritten, 1L, 1L,
                                floatArray, &fitsStatus);
     if (fitsStatus != NO_ERROR)
        {
        throw fitsStatus;
        }

     // fpltoa
     float fpltoa = fpltof * areaInMMSqr;
     floatArray[0] = fpltoa;
     fitsReturn = fits_write_col(resultStatFile, TFLOAT, fpltoaFitsColNum,
                                resultStatRowsWritten, 1L, 1L,
                                floatArray, &fitsStatus);
     if (fitsStatus != NO_ERROR)
        {
        throw fitsStatus;
        }

     // fpnr
     long fpnr = (long) total.fpNoReflectionCount;
     longArray[0] = fpnr;
     fitsReturn = fits_write_col(resultStatFile, TLONG, fpnrFitsColNum,
                                resultStatRowsWritten, 1L, 1L,
                                longArray, &fitsStatus);
     if (fitsStatus != NO_ERROR)
        {
        throw fitsStatus;
        }

     // fpnrf
     float fpnrf =
                (float) ((double) total.fpNoReflectionCount /
                         (double) total.tracedPhotonCount);
     floatArray[0] = fpnrf;
     fitsReturn = fits_write_col(resultStatFile, TFLOAT, fpnrfFitsColNum,
                                resultStatRowsWritten, 1L, 1L,
                                floatArray, &fitsStatus);
     if (fitsStatus != NO_ERROR)
        {
        throw fitsStatus;
        }

     // fpnra
     float fpnra = fpnrf * areaInMMSqr;
     floatArray[0] = fpnra;
     fitsReturn = fits_write_col(resultStatFile, TFLOAT, fpnraFitsColNum,
                                resultStatRowsWritten, 1L, 1L,
                                floatArray, &fitsStatus);
     if (fitsStatus != NO_ERROR)
        {
        throw fitsStatus;
        }

     // fpabn
     long fpabn = (long) total.fpAbnormalPathCount;
     longArray[0] = fpabn;
     fitsReturn = fits_write_col(resultStatFile, TLONG, fpabnFitsColNum,
                                resultStatRowsWritten, 1L, 1L,
                                longArray, &fitsStatus);
     if (fitsStatus != NO_ERROR)
        {
        throw fitsStatus;
        }

     // fpabnf
     float fpabnf =
                (float) ((double) total.fpAbnormalPathCount /
                         (double) total.tracedPhotonCount);
     floatArray[0] = fpabnf;
     fitsReturn = fits_write_col(resultStatFile, TFLOAT, fpabnfFitsColNum,
                                resultStatRowsWritten, 1L, 1L,
                                floatArray, &fitsStatus);
     if (fitsStatus != NO_ERROR)
        {
        throw fitsStatus;
        }

     // fpabna
     float fpabna = fpabnf * areaInMMSqr;
     floatArray[0] = fpabna;
     fitsReturn = fits_write_col(resultStatFile, TFLOAT, fpabnaFitsColNum,
                                resultStatRowsWritten, 1L, 1L,
                                floatArray, &fitsStatus);
     if (fitsStatus != NO_ERROR)
        {
        throw fitsStatus;
        }

     // fpmtotal
     long fpmtotal = (long) total.impactOnFPMaskCount;
     longArray[0] = fpmtotal;
     fitsReturn = fits_write_col(resultStatFile, TLONG, fpmtotalFitsColNum,
                                resultStatRowsWritten, 1L, 1L,
                                longArray, &fitsStatus);
     if (fitsStatus != NO_ERROR)
        {
        throw fitsStatus;
        }

     // fpmtotf
     float fpmtotf =
                (float) ((double) total.impactOnFPMaskCount /
                         (double) total.tracedPhotonCount);
     floatArray[0] = fpmtotf;
     fitsReturn = fits_write_col(resultStatFile, TFLOAT, fpmtotfFitsColNum,
                                resultStatRowsWritten, 1L, 1L,
                                floatArray, &fitsStatus);
     if (fitsStatus != NO_ERROR)
        {
        throw fitsStatus;
        }

     // fpmtota
     float fpmtota = fpmtotf * areaInMMSqr;
     floatArray[0] = fpmtota;
     fitsReturn = fits_write_col(resultStatFile, TFLOAT, fpmtotaFitsColNum,
                                resultStatRowsWritten, 1L, 1L,
                                floatArray, &fitsStatus);
     if (fitsStatus != NO_ERROR)
        {
        throw fitsStatus;
        }

     // fpmopl
     long fpmopl = (long) total.fpMaskOnePerLayerCount;
     longArray[0] = fpmopl;
     fitsReturn = fits_write_col(resultStatFile, TLONG, fpmoplFitsColNum,
                                resultStatRowsWritten, 1L, 1L,
                                longArray, &fitsStatus);
     if (fitsStatus != NO_ERROR)
        {
        throw fitsStatus;
        }

     // fpmoplf
     float fpmoplf =
                (float) ((double) total.fpMaskOnePerLayerCount /
                         (double) total.tracedPhotonCount);
     floatArray[0] = fpmoplf;
     fitsReturn = fits_write_col(resultStatFile, TFLOAT, fpmoplfFitsColNum,
                                resultStatRowsWritten, 1L, 1L,
                                floatArray, &fitsStatus);
     if (fitsStatus != NO_ERROR)
        {
        throw fitsStatus;
        }

     // fpmopla
     float fpmopla = fpmoplf * areaInMMSqr;
     floatArray[0] = fpmopla;
     fitsReturn = fits_write_col(resultStatFile, TFLOAT, fpmoplaFitsColNum,
                                resultStatRowsWritten, 1L, 1L,
                                floatArray, &fitsStatus);
     if (fitsStatus != NO_ERROR)
        {
        throw fitsStatus;
        }

     // fpmolo
     long fpmolo = (long) total.fpMaskOneLayerOnlyCount;
     longArray[0] = fpmolo;
     fitsReturn = fits_write_col(resultStatFile, TLONG, fpmoloFitsColNum,
                                resultStatRowsWritten, 1L, 1L,
                                longArray, &fitsStatus);
     if (fitsStatus != NO_ERROR)
        {
        throw fitsStatus;
        }

     // fpmolof
     float fpmolof =
                (float) ((double) total.fpMaskOneLayerOnlyCount /
                         (double) total.tracedPhotonCount);
     floatArray[0] = fpmolof;
     fitsReturn = fits_write_col(resultStatFile, TFLOAT, fpmolofFitsColNum,
                                resultStatRowsWritten, 1L, 1L,
                                floatArray, &fitsStatus);
     if (fitsStatus != NO_ERROR)
        {
        throw fitsStatus;
        }

     // fpmoloa
     float fpmoloa = fpmolof * areaInMMSqr;
     floatArray[0] = fpmoloa;
     fitsReturn = fits_write_col(resultStatFile, TFLOAT, fpmoloaFitsColNum,
                                resultStatRowsWritten, 1L, 1L,
                                floatArray, &fitsStatus);
     if (fitsStatus != NO_ERROR)
        {
        throw fitsStatus;
        }

     // fpmloo
     long fpmloo = (long) total.fpMaskLayerOneOnlyCount;
     longArray[0] = fpmloo;
     fitsReturn = fits_write_col(resultStatFile, TLONG, fpmlooFitsColNum,
                                resultStatRowsWritten, 1L, 1L,
                                longArray, &fitsStatus);
     if (fitsStatus != NO_ERROR)
        {
        throw fitsStatus;
        }

     // fpmloof
     float fpmloof =
                (float) ((double) total.fpMaskLayerOneOnlyCount /
                         (double) total.tracedPhotonCount);
     floatArray[0] = fpmloof;
     fitsReturn = fits_write_col(resultStatFile, TFLOAT, fpmloofFitsColNum,
                                resultStatRowsWritten, 1L, 1L,
                                floatArray, &fitsStatus);
     if (fitsStatus != NO_ERROR)
        {
        throw fitsStatus;
        }

     // fpmlooa
     float fpmlooa = fpmloof * areaInMMSqr;
     floatArray[0] = fpmlooa;
     fitsReturn = fits_write_col(resultStatFile, TFLOAT, fpmlooaFitsColNum,
                                resultStatRowsWritten, 1L, 1L,
                                floatArray, &fitsStatus);
     if (fitsStatus != NO_ERROR)
        {
        throw fitsStatus;
        }

     // fpmlto
     long fpmlto = (long) total.fpMaskLayerTwoOnlyCount;
     longArray[0] = fpmlto;
     fitsReturn = fits_write_col(resultStatFile, TLONG, fpmltoFitsColNum,
                                resultStatRowsWritten, 1L, 1L,
                                longArray, &fitsStatus);
     if (fitsStatus != NO_ERROR)
        {
        throw fitsStatus;
        }

     // fpmltof
     float fpmltof =
                (float) ((double) total.fpMaskLayerTwoOnlyCount /
                         (double) total.tracedPhotonCount);
     floatArray[0] = fpmltof;
     fitsReturn = fits_write_col(resultStatFile, TFLOAT, fpmltofFitsColNum,
                                resultStatRowsWritten, 1L, 1L,
                                floatArray, &fitsStatus);
     if (fitsStatus != NO_ERROR)
        {
        throw fitsStatus;
        }

     // fpmltoa
     float fpmltoa = fpmltof * areaInMMSqr;
     floatArray[0] = fpmltoa;
     fitsReturn = fits_write_col(resultStatFile, TFLOAT, fpmltoaFitsColNum,
                                resultStatRowsWritten, 1L, 1L,
                                floatArray, &fitsStatus);
     if (fitsStatus != NO_ERROR)
        {
        throw fitsStatus;
        }

     // fpmnr
     long fpmnr = (long) total.fpMaskNoReflectionCount;
     longArray[0] = fpmnr;
     fitsReturn = fits_write_col(resultStatFile, TLONG, fpmnrFitsColNum,
                                resultStatRowsWritten, 1L, 1L,
                                longArray, &fitsStatus);
     if (fitsStatus != NO_ERROR)
        {
        throw fitsStatus;
        }

     // fpmnrf
     float fpmnrf =
                (float) ((double) total.fpMaskNoReflectionCount /
                         (double) total.tracedPhotonCount);
     floatArray[0] = fpmnrf;
     fitsReturn = fits_write_col(resultStatFile, TFLOAT, fpmnrfFitsColNum,
                                resultStatRowsWritten, 1L, 1L,
                                floatArray, &fitsStatus);
     if (fitsStatus != NO_ERROR)
        {
        throw fitsStatus;
        }

     // fpmnra
     float fpmnra = fpmnrf * areaInMMSqr;
     floatArray[0] = fpmnra;
     fitsReturn = fits_write_col(resultStatFile, TFLOAT, fpmnraFitsColNum,
                                resultStatRowsWritten, 1L, 1L,
                                floatArray, &fitsStatus);
     if (fitsStatus != NO_ERROR)
        {
        throw fitsStatus;
        }

     // fpmabn
     long fpmabn = (long) total.fpMaskAbnormalPathCount;
     longArray[0] = fpmabn;
     fitsReturn = fits_write_col(resultStatFile, TLONG, fpmabnFitsColNum,
                                resultStatRowsWritten, 1L, 1L,
                                longArray, &fitsStatus);
     if (fitsStatus != NO_ERROR)
        {
        throw fitsStatus;
        }

     // fpmabnf
     float fpmabnf =
                (float) ((double) total.fpMaskAbnormalPathCount /
                         (double) total.tracedPhotonCount);
     floatArray[0] = fpmabnf;
     fitsReturn = fits_write_col(resultStatFile, TFLOAT, fpmabnfFitsColNum,
                                resultStatRowsWritten, 1L, 1L,
                                floatArray, &fitsStatus);
     if (fitsStatus != NO_ERROR)
        {
        throw fitsStatus;
        }

     // fpmabna
     float fpmabna = fpmabnf * areaInMMSqr;
     floatArray[0] = fpmabna;
     fitsReturn = fits_write_col(resultStatFile, TFLOAT, fpmabnaFitsColNum,
                                resultStatRowsWritten, 1L, 1L,
                                floatArray, &fitsStatus);
     if (fitsStatus != NO_ERROR)
        {
        throw fitsStatus;
        }

     // mirtop
     long mirtop = (long) total.impactOnMirrorTop;
     longArray[0] = mirtop;
     fitsReturn = fits_write_col(resultStatFile, TLONG, mirtopFitsColNum,
                                resultStatRowsWritten, 1L, 1L,
                                longArray, &fitsStatus);
     if (fitsStatus != NO_ERROR)
        {
        throw fitsStatus;
        }

     // mirtopf
     float mirtopf =
                (float) ((double) total.impactOnMirrorTop /
                         (double) total.tracedPhotonCount);
     floatArray[0] = mirtopf;
     fitsReturn = fits_write_col(resultStatFile, TFLOAT, mirtopfFitsColNum,
                                resultStatRowsWritten, 1L, 1L,
                                floatArray, &fitsStatus);
     if (fitsStatus != NO_ERROR)
        {
        throw fitsStatus;
        }

     // mirtopa
     float mirtopa = mirtopf * areaInMMSqr;
     floatArray[0] = mirtopa;
     fitsReturn = fits_write_col(resultStatFile, TFLOAT, mirtopaFitsColNum,
                                resultStatRowsWritten, 1L, 1L,
                                floatArray, &fitsStatus);
     if (fitsStatus != NO_ERROR)
        {
        throw fitsStatus;
        }

     // shadow
     long shadow = (long) total.impactOnObstruction;
     longArray[0] = shadow;
     fitsReturn = fits_write_col(resultStatFile, TLONG, shadowFitsColNum,
                                resultStatRowsWritten, 1L, 1L,
                                longArray, &fitsStatus);
     if (fitsStatus != NO_ERROR)
        {
        throw fitsStatus;
        }

     // shadowf
     float shadowf =
                (float) ((double) total.impactOnObstruction /
                         (double) total.tracedPhotonCount);
     floatArray[0] = shadowf;
     fitsReturn = fits_write_col(resultStatFile, TFLOAT, shadowfFitsColNum,
                                resultStatRowsWritten, 1L, 1L,
                                floatArray, &fitsStatus);
     if (fitsStatus != NO_ERROR)
        {
        throw fitsStatus;
        }

     // shadowa
     float shadowa = shadowf * areaInMMSqr;
     floatArray[0] = shadowa;
     fitsReturn = fits_write_col(resultStatFile, TFLOAT, shadowaFitsColNum,
                                resultStatRowsWritten, 1L, 1L,
                                floatArray, &fitsStatus);
     if (fitsStatus != NO_ERROR)
        {
        throw fitsStatus;
        }

     // ohousing
     long ohousing = (long) total.impactOnOuterHousingCount;
     longArray[0] = ohousing;
     fitsReturn = fits_write_col(resultStatFile, TLONG, ohousingFitsColNum,
                                resultStatRowsWritten, 1L, 1L,
                                longArray, &fitsStatus);
     if (fitsStatus != NO_ERROR)
        {
        throw fitsStatus;
        }

     // ohousinf
     float ohousinf =
                (float) ((double) total.impactOnOuterHousingCount /
                         (double) total.tracedPhotonCount);
     floatArray[0] = ohousinf;
     fitsReturn = fits_write_col(resultStatFile, TFLOAT, ohousinfFitsColNum,
                                resultStatRowsWritten, 1L, 1L,
                                floatArray, &fitsStatus);
     if (fitsStatus != NO_ERROR)
        {
        throw fitsStatus;
        }

     // ohousina
     float ohousina = ohousinf * areaInMMSqr;
     floatArray[0] = ohousina;
     fitsReturn = fits_write_col(resultStatFile, TFLOAT, ohousinaFitsColNum,
                                resultStatRowsWritten, 1L, 1L,
                                floatArray, &fitsStatus);
     if (fitsStatus != NO_ERROR)
        {
        throw fitsStatus;
        }

     // ihousing
     long ihousing = (long) total.impactOnInnerHousingCount;
     longArray[0] = ihousing;
     fitsReturn = fits_write_col(resultStatFile, TLONG, ihousingFitsColNum,
                                resultStatRowsWritten, 1L, 1L,
                                longArray, &fitsStatus);
     if (fitsStatus != NO_ERROR)
        {
        throw fitsStatus;
        }

     // ihousinf
     float ihousinf =
                (float) ((double) total.impactOnInnerHousingCount /
                         (double) total.tracedPhotonCount);
     floatArray[0] = ihousinf;
     fitsReturn = fits_write_col(resultStatFile, TFLOAT, ihousinfFitsColNum,
                                resultStatRowsWritten, 1L, 1L,
                                floatArray, &fitsStatus);
     if (fitsStatus != NO_ERROR)
        {
        throw fitsStatus;
        }

     // ihousina
     float ihousina = ihousinf * areaInMMSqr;
     floatArray[0] = ihousina;
     fitsReturn = fits_write_col(resultStatFile, TFLOAT, ihousinaFitsColNum,
                                resultStatRowsWritten, 1L, 1L,
                                floatArray, &fitsStatus);
     if (fitsStatus != NO_ERROR)
        {
        throw fitsStatus;
        }

     // absfront
     long absfront = (long) total.absorbedOnFrontMirrorFaceCount;
     longArray[0] = absfront;
     fitsReturn = fits_write_col(resultStatFile, TLONG, absfrontFitsColNum,
                                resultStatRowsWritten, 1L, 1L,
                                longArray, &fitsStatus);
     if (fitsStatus != NO_ERROR)
        {
        throw fitsStatus;
        }

     // absfronf
     float absfronf =
                (float) ((double) total.absorbedOnFrontMirrorFaceCount /
                         (double) total.tracedPhotonCount);
     floatArray[0] = absfronf;
     fitsReturn = fits_write_col(resultStatFile, TFLOAT, absfronfFitsColNum,
                                resultStatRowsWritten, 1L, 1L,
                                floatArray, &fitsStatus);
     if (fitsStatus != NO_ERROR)
        {
        throw fitsStatus;
        }

     // absfrona
     float absfrona = absfronf * areaInMMSqr;
     floatArray[0] = absfrona;
     fitsReturn = fits_write_col(resultStatFile, TFLOAT, absfronaFitsColNum,
                                resultStatRowsWritten, 1L, 1L,
                                floatArray, &fitsStatus);
     if (fitsStatus != NO_ERROR)
        {
        throw fitsStatus;
        }

     // absback
     long absback = (long) total.absorbedOnBackMirrorFaceCount;
     longArray[0] = absback;
     fitsReturn = fits_write_col(resultStatFile, TLONG, absbackFitsColNum,
                                resultStatRowsWritten, 1L, 1L,
                                longArray, &fitsStatus);
     if (fitsStatus != NO_ERROR)
        {
        throw fitsStatus;
        }

     // absbackf
     float absbackf =
                (float) ((double) total.absorbedOnBackMirrorFaceCount /
                         (double) total.tracedPhotonCount);
     floatArray[0] = absbackf;
     fitsReturn = fits_write_col(resultStatFile, TFLOAT, absbackfFitsColNum,
                                resultStatRowsWritten, 1L, 1L,
                                floatArray, &fitsStatus);
     if (fitsStatus != NO_ERROR)
        {
        throw fitsStatus;
        }

     // absbacka
     float absbacka = absbackf * areaInMMSqr;
     floatArray[0] = absbacka;
     fitsReturn = fits_write_col(resultStatFile, TFLOAT, absbackaFitsColNum,
                                resultStatRowsWritten, 1L, 1L,
                                floatArray, &fitsStatus);
     if (fitsStatus != NO_ERROR)
        {
        throw fitsStatus;
        }

     // noleave
     long noleave = (long) total.photonCanNotLeaveMirrorCount;
     longArray[0] = noleave;
     fitsReturn = fits_write_col(resultStatFile, TLONG, noleaveFitsColNum,
                                resultStatRowsWritten, 1L, 1L,
                                longArray, &fitsStatus);
     if (fitsStatus != NO_ERROR)
        {
        throw fitsStatus;
        }

     // noleavef
     float noleavef =
                (float) ((double) total.photonCanNotLeaveMirrorCount /
                         (double) total.tracedPhotonCount);
     floatArray[0] = noleavef;
     fitsReturn = fits_write_col(resultStatFile, TFLOAT, noleavefFitsColNum,
                                resultStatRowsWritten, 1L, 1L,
                                floatArray, &fitsStatus);
     if (fitsStatus != NO_ERROR)
        {
        throw fitsStatus;
        }

     // noleavea
     float noleavea = noleavef * areaInMMSqr;
     floatArray[0] = noleavea;
     fitsReturn = fits_write_col(resultStatFile, TFLOAT, noleaveaFitsColNum,
                                resultStatRowsWritten, 1L, 1L,
                                floatArray, &fitsStatus);
     if (fitsStatus != NO_ERROR)
        {
        throw fitsStatus;
        }

     // error
     long error = (long) total.photonErrorCount;
     longArray[0] = error;
     fitsReturn = fits_write_col(resultStatFile, TLONG, errorFitsColNum,
                                resultStatRowsWritten, 1L, 1L,
                                longArray, &fitsStatus);
     if (fitsStatus != NO_ERROR)
        {
        throw fitsStatus;
        }

     // errorf
     float errorf =
                (float) ((double) total.photonErrorCount /
                         (double) total.tracedPhotonCount);
     floatArray[0] = errorf;
     fitsReturn = fits_write_col(resultStatFile, TFLOAT, errorfFitsColNum,
                                resultStatRowsWritten, 1L, 1L,
                                floatArray, &fitsStatus);
     if (fitsStatus != NO_ERROR)
        {
        throw fitsStatus;
        }

     // errora
     float errora = errorf * areaInMMSqr;
     floatArray[0] = errora;
     fitsReturn = fits_write_col(resultStatFile, TFLOAT, erroraFitsColNum,
                                resultStatRowsWritten, 1L, 1L,
                                floatArray, &fitsStatus);
     if (fitsStatus != NO_ERROR)
        {
        throw fitsStatus;
        }

     // psfrad
     long *psfData = new long[sourceRadialProfileBins];
     for (int i = 0; i < sourceRadialProfileBins; i++)
        {
        psfData[i]  = (long) total.impactBySourceRadialBin[i];
        }
     fitsReturn = fits_write_col(resultStatFile, TLONG, psfradFitsColNum,
                                resultStatRowsWritten, 1L, 
                                sourceRadialProfileBins,
                                psfData, &fitsStatus);
     delete [] psfData;
     if (fitsStatus != NO_ERROR)
        {
        throw fitsStatus;
        }

    // psfradbn
    float *psfDataBin = new float [sourceRadialProfileBins];
    for (int i = 0; i < sourceRadialProfileBins; i++)
        {
        psfDataBin[i]  = (float) i / (float) sourceRadialBinsPerArcmin;
        }
     fitsReturn = fits_write_col(resultStatFile, TFLOAT, psfradbnFitsColNum,
                                resultStatRowsWritten, 1L,
                                sourceRadialProfileBins,
                                psfDataBin, &fitsStatus);
     delete [] psfDataBin;
     if (fitsStatus != NO_ERROR)
        {
        throw fitsStatus;
        }
}

void
XrrtStat::outputPhotonData(const XrrtPhoton& photon)
{
//
// Write the Photon History FITS table row for a photon
//
const double RADIAN2DEGREE =  1.0e0/0.017453292519943295769e0;
//
// I wish this was cleaner
// Maybe these should be in the private object members or the constructor?
//
const int photonFitsColNum = 1;
const int energyFitsColNum = 2;
const int oradiusFitsColNum = 3;
const int oangleFitsColNum = 4;
const int telexFitsColNum = 5;
const int teleyFitsColNum = 6;
const int othetaFitsColNum = 7;
const int ophiFitsColNum = 8;
const int resultFitsColNum = 9;
const int classFitsColNum = 10;
const int xFitsColNum = 11;
const int yFitsColNum = 12;
const int psfxFitsColNum = 13;
const int psfyFitsColNum = 14;
const int pathFitsColNum = 15;

int      fitsStatus;        // Standard error code return from FITSIO
int      fitsReturn;        // Standard return type from a FITSIO function

long  longArray[1];
float floatArray[1];

     //
     // In case we are called when we shouldn't be
     //
     if (!outputPhotonHistory)
        {
        return;
        }

     // Update the row to write
     photonHistoryRowsWritten++;

     // Always
     fitsStatus = 0;

     // Output the data to the row

     // photon number
     long photonNumber =  (long) (statByEnergy.tracedPhotonCount +
                                  total.tracedPhotonCount);
     longArray[0] = photonNumber;
     fitsReturn = fits_write_col(photonHistoryFile, TLONG, photonFitsColNum,
                                 photonHistoryRowsWritten, 1L, 1L,
                                 longArray, &fitsStatus);
     if (fitsStatus != NO_ERROR)
        {
        throw fitsStatus;
        }
     
     // energy
     float energy =  (float) statByEnergy.energy;
     floatArray[0] = energy;
     fitsReturn = fits_write_col(photonHistoryFile, TFLOAT, energyFitsColNum,
                                 photonHistoryRowsWritten, 1L, 1L,
                                 floatArray, &fitsStatus);
     if (fitsStatus != NO_ERROR)
        {
        throw fitsStatus;
        }
     
     // original photon direction
     XrrtVector initialPhotonLocation = photon.getInitialVector();
     float radius = (float) initialPhotonLocation.getRadius();
     floatArray[0] = radius;
     fitsReturn = fits_write_col(photonHistoryFile, TFLOAT, oradiusFitsColNum,
                                 photonHistoryRowsWritten, 1L, 1L,
                                 floatArray, &fitsStatus);
     if (fitsStatus != NO_ERROR)
        {
        throw fitsStatus;
        }
     double rotationAngleRadian = initialPhotonLocation.getRotationAngle();
     float rotationAngleDegree = (float) (rotationAngleRadian*RADIAN2DEGREE);
     floatArray[0] = rotationAngleDegree;
     fitsReturn = fits_write_col(photonHistoryFile, TFLOAT, oangleFitsColNum,
                                 photonHistoryRowsWritten, 1L, 1L,
                                 floatArray, &fitsStatus);
     if (fitsStatus != NO_ERROR)
        {
        throw fitsStatus;
        }

     // original photon direction (orthogonal coordinate (x, y))
     // (added by H. MORI)
     double telescopeX, telescopeY; 
     initialPhotonLocation.getXY(&telescopeX, &telescopeY);
     float TelescopeX = (float) telescopeX;
     floatArray[0] = TelescopeX;
     fitsReturn = fits_write_col(photonHistoryFile, TFLOAT, 
                                 telexFitsColNum,
                                 photonHistoryRowsWritten, 1L, 1L,
                                 floatArray, &fitsStatus);
     if (fitsStatus != NO_ERROR)
        {
        throw fitsStatus;
        }
 
     float TelescopeY = (float) telescopeY; 
     floatArray[0] = TelescopeY;
     fitsReturn = fits_write_col(photonHistoryFile, TFLOAT, 
                                 teleyFitsColNum,
                                 photonHistoryRowsWritten, 1L, 1L,
                                 floatArray, &fitsStatus);
     if (fitsStatus != NO_ERROR)
        {
        throw fitsStatus;
        }

     // write photon information of incident angle (added by H. Mori)
     // column name is otheta(number 7) [arcmin]
     UnitVectorMag unitRadius, unitZ;
     AngleInRadians unitPhi;
     initialPhotonLocation.getVectorDirection(unitRadius, unitPhi, unitZ);
     float incidentAngleArcmin = (float) (-atan(unitRadius/unitZ)
     					  *RADIAN2DEGREE*60.0e0);
     floatArray[0] = incidentAngleArcmin;
     fitsReturn = fits_write_col(photonHistoryFile, TFLOAT, othetaFitsColNum,
                                 photonHistoryRowsWritten, 1L, 1L,
                                 floatArray, &fitsStatus);
     if (fitsStatus != NO_ERROR)
        {
        throw fitsStatus;
        }

     // write photon information of phase angle for incident X-ray
     // column name is ophi(number 8) [arcmin] (added by H. Mori)
     float incidentPhaseAngleDegree = (float) (unitPhi * RADIAN2DEGREE);
     floatArray[0] = incidentPhaseAngleDegree;
     fitsReturn = fits_write_col(photonHistoryFile, TFLOAT, ophiFitsColNum,
                                 photonHistoryRowsWritten, 1L, 1L,
                                 floatArray, &fitsStatus);
     if (fitsStatus != NO_ERROR)
        {
        throw fitsStatus;
        }

     // photon status code
     PhotonStatusCodes photonStatus =  photon.photonStatus();

     // result code
     long result = (long) photonStatus;
     longArray[0] = result;
     fitsReturn = fits_write_col(photonHistoryFile, TLONG, resultFitsColNum,
                                 photonHistoryRowsWritten, 1L, 1L,
                                 longArray, &fitsStatus);
     if (fitsStatus != NO_ERROR)
        {
        throw fitsStatus;
        }

     // photon reflection class
     long fitsClass = (long) photon.getPhotonReflectionClass();
     longArray[0] = fitsClass;
     fitsReturn = fits_write_col(photonHistoryFile, TLONG, classFitsColNum,
                                 photonHistoryRowsWritten, 1L, 1L,
                                 longArray, &fitsStatus);
     if (fitsStatus != NO_ERROR)
        {
        throw fitsStatus;
        }

     // current photon location 
     XrrtVector currentPhotonLocation = photon.getCurrentVector();

     if (photonStatus == PHOTON_HIT_FOCAL_PLANE)
        {
        // focal plane x,y
        double x,y;
        currentPhotonLocation.getXY(&x,&y);
        float floatX = (float) x;
        floatArray[0] = floatX;
        fitsReturn = fits_write_col(photonHistoryFile, TFLOAT, xFitsColNum,
                                    photonHistoryRowsWritten, 1L, 1L,
                                    floatArray, &fitsStatus);
        if (fitsStatus != NO_ERROR)
           {
           throw fitsStatus;
           }
        float floatY = (float) y;
        floatArray[0] = floatY;
        fitsReturn = fits_write_col(photonHistoryFile, TFLOAT, yFitsColNum,
                                    photonHistoryRowsWritten, 1L, 1L,
                                    floatArray, &fitsStatus);
        if (fitsStatus != NO_ERROR)
           {
           throw fitsStatus;
           }
        // focal plane psfx,psfy
        double psfX, psfY;
        psfX = x - sourceFocalPlaneXCenterMM;
        psfY = y - sourceFocalPlaneYCenterMM;
        floatX = (float) psfX;
        floatArray[0] = floatX;
        fitsReturn = fits_write_col(photonHistoryFile, TFLOAT, psfxFitsColNum,
                                    photonHistoryRowsWritten, 1L, 1L,
                                    floatArray, &fitsStatus);
        if (fitsStatus != NO_ERROR)
           {
           throw fitsStatus;
           }
        floatY = (float) psfY;
        floatArray[0] = floatY;
        fitsReturn = fits_write_col(photonHistoryFile, TFLOAT, psfyFitsColNum,
                                    photonHistoryRowsWritten, 1L, 1L,
                                    floatArray, &fitsStatus);
        if (fitsStatus != NO_ERROR)
           {
           throw fitsStatus;
           }
	// photon reflection path (modified by HIDEYUKI MORI)
	// some routine needed
	long reflectionPathNumber = (long) photon.getReflectionPath();
	longArray[0] = reflectionPathNumber;
	fitsReturn = fits_write_col(photonHistoryFile, TLONG, pathFitsColNum,
				    photonHistoryRowsWritten, 1L, 1L,
				    longArray, &fitsStatus);
        if (fitsStatus != NO_ERROR)
           {
           throw fitsStatus;
           }
        }
     else
        {
        //
        // For those photon where these valuse are meaniless, we write zero
        // An alternate would be to declare a null value.
        //
        float zero = 0.0e0;
        floatArray[0] = zero;
	long l_zero = 0;
	longArray[0] = l_zero;
        fitsReturn = fits_write_col(photonHistoryFile, TFLOAT, xFitsColNum,
                                    photonHistoryRowsWritten, 1L, 1L,
                                    floatArray, &fitsStatus);
        if (fitsStatus != NO_ERROR)
           {
           throw fitsStatus;
           }
        fitsReturn = fits_write_col(photonHistoryFile, TFLOAT, yFitsColNum,
                                    photonHistoryRowsWritten, 1L, 1L,
                                    floatArray, &fitsStatus);
        if (fitsStatus != NO_ERROR)
           {
           throw fitsStatus;
           }
        fitsReturn = fits_write_col(photonHistoryFile, TFLOAT, psfxFitsColNum,
                                    photonHistoryRowsWritten, 1L, 1L,
                                    floatArray, &fitsStatus);
        if (fitsStatus != NO_ERROR)
           {
           throw fitsStatus;
           }
        fitsReturn = fits_write_col(photonHistoryFile, TFLOAT, psfyFitsColNum,
                                    photonHistoryRowsWritten, 1L, 1L,
                                    floatArray, &fitsStatus);
        if (fitsStatus != NO_ERROR)
           {
           throw fitsStatus;
           }
	// add photon reflection path for no photon hit focal plane
	// (modified by HIDEYUKI MORI)
        fitsReturn = fits_write_col(photonHistoryFile, TLONG, pathFitsColNum,
                                    photonHistoryRowsWritten, 1L, 1L,
                                    longArray, &fitsStatus);
        if (fitsStatus != NO_ERROR)
           {
           throw fitsStatus;
           }
	}
}

void
XrrtStat::writeHistoricalKeywords(fitsfile* fitsFile)
{
//
// Write metadata keywords to the supplied FITS file. We don't check that
// the file is properly positioned.
//
int      fitsStatus;        // Standard error code return from FITSIO
int      fitsReturn;        // Standard return type from a FITSIO function
string comment;
string keyword;
    
    // Always
    fitsStatus = 0;

    // Store historical info in the requested FITS file
    keyword  = "CREATOR";
    comment = "XrrtStat version";
    fitsFileKeywordWrite(fitsFile, keyword, XrrtStatVersion, comment);

    // Glossary
    comment = "MM = millimeters";
    fitsFileCommentWrite(fitsFile, comment);
    comment = "PSF = Point Spread Function";
    fitsFileCommentWrite(fitsFile, comment);

    // collectResultStat
    if (collectResultStat)
       {
       comment = "Result statistics were collected";
       }
    else
       {
       comment = "Result statistics were NOT collected";
       }
    fitsFileHistoryWrite(fitsFile, comment);
    
    // outputResultStat
    if (outputResultStat)
       {
       comment = "Result statistics were output";
       fitsFileHistoryWrite(fitsFile, comment);
       keyword  = "RSFN";
       comment = "Result Statistics File Name";
       fitsFileKeywordWrite(fitsFile, keyword, resultStatFileName, comment);
       keyword  = "RSTN";
       comment = "Result Statistics Table Name";
       fitsFileKeywordWrite(fitsFile, keyword, resultStatTableName, comment);
       }
    else
       {
       comment = "Result statistics were NOT output";
       fitsFileHistoryWrite(fitsFile, comment);
       }

    // CTYPE1
    keyword = "CTYPE1";
    string ctype1 = "RA--TAN";
    comment = "Plane tangent Projection";
    fitsFileKeywordWrite(fitsFile, keyword, ctype1, comment);

    // CTYPE2
    keyword = "CTYPE2";
    string ctype2 = "DEC--TAN";
    comment = "Plane tangent Projection";
    fitsFileKeywordWrite(fitsFile, keyword, ctype2, comment);

    // EPOCH
    keyword = "EPOCH";
    string epoch = "2000";
    comment = "Launch year";
    fitsFileKeywordWrite(fitsFile, keyword, epoch, comment);

    // EQUINOX
    keyword = "EQUINOX";
    string equinox = "2000";
    comment = "Equinox of coordinate system";
    fitsFileKeywordWrite(fitsFile, keyword, equinox, comment);

    // RADECSYS
    keyword = "RADECSYS";
    string radecsys = "FK5";
    comment = "Coordinate system";
    fitsFileKeywordWrite(fitsFile, keyword, radecsys, comment);

    // CRPIX1
    double crpix1 = (double) xyAxisBins/2.0e0 + 0.5e0;
    keyword = "CRPIX1";
    comment = "Reference pixel";
    fitsFileKeywordWrite(fitsFile, keyword, crpix1, comment);

    // CRPIX2
    double crpix2 = (double) xyAxisBins/2.0e0 + 0.5e0;
    keyword = "CRPIX2";
    comment = "Reference pixel";
    fitsFileKeywordWrite(fitsFile, keyword, crpix2, comment);
    
    // CDELT1
    double cdelt1 =   (xyAxisMaxMM-xyAxisMinMM) // millimeters
                    / (double) xyAxisBins       // millimeters/bin
                    * focalPlaneMM2Arcmin       // arcmin/bin
                    / 60;                       // degrees/bin
    keyword = "CDELT1";
    comment = "Plate scale in degrees per pixel";
    fitsFileKeywordWrite(fitsFile, keyword, cdelt1, comment);
    
    // CDELT2
    double cdelt2 = cdelt1;
    keyword = "CDELT2";
    comment = "Plate scale in degrees per pixel";
    fitsFileKeywordWrite(fitsFile, keyword, cdelt2, comment);
    
    // CRVAL1
    double crval1 = nominalRAAtCenter;
    //if ((crpix1 % 2) == 1)
    //   {
    //   // xybins is odd and so there is a center pixel
    //   crval1 = nominalRAAtCenter;
    //   }
    //else
    //   {
    //   // There is no center pixel
    //   crval1 = nominalRAAtCenter + cdelt1/2.0e0;
    //   }
    keyword = "CRVAL1";
    comment = "Center coordinate in degrees";
    fitsFileKeywordWrite(fitsFile, keyword, crval1, comment);
    
    // CRVAL2
    double crval2 = nominalDECAtCenter;
    //if ((crpix2 % 2) == 1)
    //   {
    //   // xybins is odd and so there is a center pixel
    //   crval2 = nominalDECAtCenter;
    //   }
    //else
    //   {
    //   // There is no center pixel
    //   crval2 = nominalDECAtCenter + cdelt2/2.0e0;
    //   }
    keyword = "CRVAL2";
    comment = "Center coordinate in degrees";
    fitsFileKeywordWrite(fitsFile, keyword, crval2, comment);
    
    // CROTA1
    double crota1 = 0;
    keyword = "CROTA1";
    comment = "Axis rotation in degrees";
    fitsFileKeywordWrite(fitsFile, keyword, crota1, comment);
    
    // CROTA2
    double crota2 = 0;
    keyword = "CROTA2";
    comment = "Axis rotation in degrees";
    fitsFileKeywordWrite(fitsFile, keyword, crota2, comment);
    
    // sourceFocalPlaneXCenterMM
    keyword = "SFPXC";
    comment = "Source Focal Plane X Center in MM";
    fitsFileKeywordWrite(fitsFile, keyword, sourceFocalPlaneXCenterMM, comment);

    // sourceFocalPlaneYCenterMM
    keyword = "SFPYC";
    comment = "Source Focal Plane Y Center in MM";
    fitsFileKeywordWrite(fitsFile, keyword, sourceFocalPlaneYCenterMM, comment);

    // designFocalLengthMM
    keyword = "DFLMM";
    comment = "Design Focal length in MM";
    fitsFileKeywordWrite(fitsFile, keyword, designFocalLengthMM, comment);

    // focalPlaneMM2Arcmin
    keyword = "FPMM2AM";
    comment = "Focal Plane MM 2 ArcMinutes scale factor";
    fitsFileKeywordWrite(fitsFile, keyword, focalPlaneMM2Arcmin, comment);

    // psfCoreRadiusLimitRadian
    keyword = "PSFCRLR";
    comment = "PSF Core Radius Limit in Radians";
    fitsFileKeywordWrite(fitsFile, keyword, psfCoreRadiusLimitRadian, comment);

    // areaInMMSqr
    keyword = "AREAMM2";
    comment = "Telescope AREA in MM**2 for this ray trace";
    fitsFileKeywordWrite(fitsFile, keyword, areaInMMSqr, comment);

    // sourceRadialProfileBins
    keyword = "SRPB";
    comment = "Source Radial Profile Bins";
    fitsFileKeywordWrite(fitsFile, keyword, sourceRadialProfileBins, comment);

    // sourceRadialBinsPerArcmin
    keyword = "SRBPAM";
    comment = "Source Radial Bins Per ArcMinute";
    fitsFileKeywordWrite(fitsFile, keyword, sourceRadialBinsPerArcmin, comment);

    // focalPlaneMask
    keyword = "FPMX";
    comment = "Focal Plane Mask polygon X coords&";
    char* commentArray[1];
    commentArray[0] = (char*) comment.c_str();
    int numberOfVertexes = focalPlaneMask.getPolygonSize();
    double *xArray = new double [numberOfVertexes];
    double *yArray = new double [numberOfVertexes];
    for (int i = 0; i < numberOfVertexes; i++)
        {
        xArray[i] = focalPlaneMask.getPolygonX(i);
        yArray[i] = focalPlaneMask.getPolygonY(i);
        }
    fitsReturn = fits_write_keys_dbl(fitsFile, (char*) keyword.c_str(), 1, 
                                     (int) numberOfVertexes,
                                     xArray, 9, commentArray, &fitsStatus);
    delete [] xArray;
    if (fitsStatus != NO_ERROR)
       {
       delete [] yArray;
       throw fitsStatus;
       }
    keyword = "FPMY";
    comment = "Focal Plane Mask polygon Y coords&";
    commentArray[0] = (char*) comment.c_str();
    fitsReturn = fits_write_keys_dbl(fitsFile, (char*) keyword.c_str(), 1, 
                                     (int) numberOfVertexes,
                                     yArray, 9, commentArray, &fitsStatus);
    delete [] yArray;
    if (fitsStatus != NO_ERROR)
       {
       throw fitsStatus;
       }

    // outputPhotonHistory
    if (outputPhotonHistory)
       {
       comment = "Photon History statistics were collected";
       fitsFileHistoryWrite(fitsFile, comment);
       keyword  = "PHFN";
       comment = "Photon History file name";
       fitsFileKeywordWrite(fitsFile, keyword, photonHistoryFileName, comment);
       keyword = "PHTN";
       comment = "Photon History table name";
       fitsFileKeywordWrite(fitsFile, keyword, photonHistoryTableName, comment);
       }
    else
       {
       comment = "Photon History statistics were NOT collected";
       fitsFileHistoryWrite(fitsFile, comment);
       }
    
    // collectImage
    if (collectImage)
       {
       comment = "Images were output";
       fitsFileHistoryWrite(fitsFile, comment);
       keyword = "IMAGEFN";
       comment = "Image file name";
       fitsFileKeywordWrite(fitsFile, keyword, imageFileName, comment);
       keyword = "IMAGETN";
       comment = "Image table name";
       fitsFileKeywordWrite(fitsFile, keyword, imageTableName, comment);
       if (saveImageByEnergy)
          {
          comment = "Images were output by energy";
          fitsFileHistoryWrite(fitsFile, comment);
          }
       keyword = "IMXYBINS";
       comment = "Image X/Y bins";
       fitsFileKeywordWrite(fitsFile, keyword, xyAxisBins, comment);
       keyword = "IMXYMINM";
       comment = "Image X/Y Minimum value in MM";
       fitsFileKeywordWrite(fitsFile, keyword, xyAxisMinMM, comment);
       keyword = "IMXYMAXM";
       comment = "Image X/Y Maximum value in MM";
       fitsFileKeywordWrite(fitsFile, keyword, xyAxisMaxMM, comment);
       }
    else
       {
       comment = "Images were NOT output";
       fitsFileHistoryWrite(fitsFile, comment);
       }
}

void
XrrtStat::calculatePSF(vector<double>& psfArray)
{
//
// Based on code in MKXRTRSPv6.1/psf_tools/org2psf_960419.f
//                  MKXRTRSPv6.1/psf_tools/psfconv1.f
//                  MKXRTRSPv6.1/psf_tools/wtrpf1_1.f
//
const double XRRT_PI = 3.141592653e0;

// 
// Calculate a point spread function array based on the counts in the
// statByEnergy.impactBySourceRadialBin
//

// 
// Local copy of statByEnergy.psfRadialBin converted for 
// cumulative format
vector<int> cumulativePsfRadialBin;

//
// bin number for 6 arc min 
int sixArcminBin;

//
// Convert statByEnergy.psfRadialBin to local cumulative format
int cumulativeCount = 0;
cumulativePsfRadialBin.push_back(cumulativeCount);
for (unsigned int i=0; i < statByEnergy.psfRadialBin.size(); i++)
    {
    cumulativeCount = cumulativeCount + statByEnergy.psfRadialBin[i];
    cumulativePsfRadialBin.push_back(cumulativeCount);
    }

sixArcminBin = (int) (6.0e0 / focalPlaneMM2Arcmin * psfRadialProfileBinsPerMM + 0.5);

//
// Temp variables for PSF calculation
double encircledEnergyFraction = 0.0e0;
double lastEEF = 0.0e0;
double binPSF = 0.0e0;
double binArea = 0.0e0;
//
// Width of a radial bin 
double binWidth = 1.0e0/psfRadialProfileBinsPerMM;

//
// Check for divide by zero
if (cumulativePsfRadialBin[sixArcminBin] == 0)
   {
   throw psfCoreIsZero;
   }
//
// Calculate the PSF
//
for (unsigned int index=0; index < cumulativePsfRadialBin.size(); index++)
    {
    encircledEnergyFraction = cumulativePsfRadialBin[index] /
                              cumulativePsfRadialBin[sixArcminBin];
    // PI*outerRadius**2 - PI*innerRadius**2
    binArea = XRRT_PI * (double) (index*index - (index-1)*(index-1)) *
              binWidth*binWidth;
    binPSF = (encircledEnergyFraction - lastEEF) / binArea;
    psfArray.push_back(binPSF);
    lastEEF = encircledEnergyFraction;
    }

//
// Smooth the PSF into a working array
//
int smoothCenterIndex;
int smoothMaxIndex;
int smoothMinIndex;
int smoothHalfWidth = 3;
double workIndexArray[20];
double workPSFArray[20];
int workArrayIndex = 0;
double smoothPSFArray[1024];
for (smoothCenterIndex=0; (unsigned int)smoothCenterIndex < psfArray.size();
     smoothCenterIndex++)
    {
    smoothMinIndex = min(2, (smoothCenterIndex-smoothHalfWidth));
    if (smoothMinIndex == 2)
       {
       smoothMaxIndex = 2 * smoothHalfWidth;
       }
    else
       {
       smoothMaxIndex = max((int) psfArray.size(), 
                            (smoothCenterIndex + smoothHalfWidth));
       }
    if ((unsigned int)smoothMaxIndex == psfArray.size())
       {
       smoothMinIndex = psfArray.size() - 2 * smoothHalfWidth;
       }
    workArrayIndex = 0;
    for (int smoothIndex=smoothMinIndex; smoothIndex < smoothMaxIndex;
         smoothIndex++)
       {
       workIndexArray[workArrayIndex] = workArrayIndex;
       if (psfArray[smoothIndex] == 0.0e0)
          {
          psfArray[smoothIndex] = 1.0e-7;
          }
       workPSFArray[workArrayIndex] = log(psfArray[smoothIndex]);
       workArrayIndex++;
       }
    smoothPSF(workIndexArray, workPSFArray, smoothMaxIndex-smoothMinIndex+1,
              (double) smoothCenterIndex, 
              &smoothPSFArray[smoothCenterIndex], 1);
    smoothPSFArray[smoothCenterIndex] = exp(smoothPSFArray[smoothCenterIndex]);
    }
    //
    // Apply the smoothed PSF to the area OUTSIDE the 6 arcmin central zone
    //
    for (unsigned int smoothIndex=sixArcminBin; smoothIndex < psfArray.size(); 
         smoothIndex++)
        {
        psfArray[smoothIndex] = smoothPSFArray[smoothIndex];
        if (psfArray[smoothIndex] == 0.0e0)
           {
           psfArray[smoothIndex] = 1.0e-7;
           }
        }
}
void
XrrtStat::smoothPSF(double* X, double* Y, int NY, double XX, 
                    double* SMY, int IMT)
{
int i;
double sumy;
double sumx2;
double sumx;
double sumxy;
double wghty;
double sumx4;
double sumx3;
double sumx2y;
double deta;
double detb;
double detc;
double det;
double a;
double b;
double c;
double x8[1000];
double y8[1000];

sumy = 0.0;
sumx2= 0.0;
sumx = 0.0;
sumxy= 0.0;
wghty= 0.0;
sumx4= 0.0;
sumx3= 0.0;
sumx2y=0.0;

for (i=0; i < NY; i++)
    {
    x8[i] = X[i];
    y8[i] = Y[i];
    }

if (IMT == 0)
   {
   for (i=0; i < NY; i++)
       {
       sumy = sumy + y8[i];
       wghty = wghty + 1.0e0;
       }
   *SMY = sumy/wghty;
   }
else if (IMT == 1)
   {
   for (i=0; i < NY; i++)
       {
       sumy = sumy + y8[i];
       sumx = sumx + x8[i];
       sumxy= sumxy+ x8[i]*y8[i];
       sumx2= sumx2+ x8[i]*x8[i];
       }
   deta = NY*sumxy - sumx*sumy;
   detb = sumy*sumx2 - sumx*sumy;
   det  = NY*sumx2 - sumx*sumx;
   a = deta/det;
   b = detb/det;
   *SMY = a*XX+b;
   }
else if (IMT == 2)
   {
   for (i=0; i < NY; i++)
       {
       sumy  = sumy  + y8[i];
       sumx  = sumx  + x8[i];
       sumxy = sumxy + x8[i]*y8[i];
       sumx2 = sumx2 + x8[i]*x8[i];
       sumx2y= sumx2y+ x8[i]*x8[i]*y8[i];
       sumx3 = sumx3 + x8[i]*x8[i]*x8[i];
       sumx4 = sumx4 + x8[i]*x8[i]*x8[i]*x8[i];
       }
   det = sumx4*sumx2*NY + 2.0e0*sumx3*sumx*sumx2 - sumx2*sumx2*sumx2 - 
         sumx4*sumx*sumx - NY*sumx3*sumx3;
   deta = sumx2y*sumx2*NY + sumx3*sumx*sumy + sumx2*sumxy*sumx -
          sumx2*sumx2*sumy - sumx*sumx*sumx2y - sumx3*sumxy*NY;
   detb = sumx4*sumxy*NY + sumx2y*sumx*sumx2 + sumx2*sumx3*sumy - 
          sumx2*sumxy*sumx2 - sumx*sumy*sumx4 - NY*sumx2y*sumx3;
   detc = sumx4*sumx2*sumy + sumx3*sumxy*sumx2 + sumx2y*sumx3*sumx - 
          sumx2y*sumx2*sumx2 - sumx3*sumx3*sumy - sumx4*sumxy*sumx;
   a = deta/det;
   b = detb/det;
   c = detc/det;
   *SMY = a * XX*XX + b*XX + c;
   }
else
   {
   *SMY = 0.0e0;
   }

}


void 
XrrtStat::setONDestroyExistingFITSFiles()
{
    destroyExistingFITSFiles = true;
}

void
XrrtStat::collectStatistics(const XrrtPhoton& photon)
{
    if (collectResultStat)
       {
       collectResultStatistics(photon);
       }
    if (outputPhotonHistory)
       {
       outputPhotonData(photon);
       }
    if (collectImage)
       {
       // Fix bug in XRRT 2.0 that allowd ALL photons into the image
       // rather than just the ones that reached the Focal Plane
       if (photon.photonStatus() == PHOTON_HIT_FOCAL_PLANE)
          {
          addPhotonToImage(photon);
          }
       }
    // Here is the ONLY place this may occur
    statByEnergy.energy = photon.getEnergy();
}

void 
XrrtStat::addByEnergyToTotal()
{
    //
    // Roll up the trace results totals for a single energy into the
    // ALL energy totals
    // 
    total.tracedPhotonCount = total.tracedPhotonCount + 
                              statByEnergy.tracedPhotonCount;
    total.tracedWithinPsfCoreRadiusCount = total.tracedWithinPsfCoreRadiusCount+
                              statByEnergy.tracedWithinPsfCoreRadiusCount;
    total.impactOnFocalPlaneCount = total.impactOnFocalPlaneCount +
                              statByEnergy.impactOnFocalPlaneCount;
    total.fpOnePerLayerCount = total.fpOnePerLayerCount +
                              statByEnergy.fpOnePerLayerCount;
    total.fpOneLayerOnlyCount = total.fpOneLayerOnlyCount +
                              statByEnergy.fpOneLayerOnlyCount;
    total.fpLayerOneOnlyCount = total.fpLayerOneOnlyCount +
                              statByEnergy.fpLayerOneOnlyCount;
    total.fpLayerTwoOnlyCount = total.fpLayerTwoOnlyCount +
                              statByEnergy.fpLayerTwoOnlyCount;
    total.fpNoReflectionCount = total.fpNoReflectionCount +
                              statByEnergy.fpNoReflectionCount;
    total.fpAbnormalPathCount = total.fpAbnormalPathCount +
                              statByEnergy.fpAbnormalPathCount;
    total.impactOnFPMaskCount = total.impactOnFPMaskCount +
                              statByEnergy.impactOnFPMaskCount;
    total.fpMaskOnePerLayerCount = total.fpMaskOnePerLayerCount +
                              statByEnergy.fpMaskOnePerLayerCount;
    total.fpMaskOneLayerOnlyCount = total.fpMaskOneLayerOnlyCount +
                              statByEnergy.fpMaskOneLayerOnlyCount;
    total.fpMaskLayerOneOnlyCount = total.fpMaskLayerOneOnlyCount +
                              statByEnergy.fpMaskLayerOneOnlyCount;
    total.fpMaskLayerTwoOnlyCount = total.fpMaskLayerTwoOnlyCount +
                              statByEnergy.fpMaskLayerTwoOnlyCount;
    total.fpMaskNoReflectionCount = total.fpMaskNoReflectionCount +
                              statByEnergy.fpMaskNoReflectionCount;
    total.fpMaskAbnormalPathCount = total.fpMaskAbnormalPathCount +
                              statByEnergy.fpMaskAbnormalPathCount;
    total.impactOnMirrorTop = total.impactOnMirrorTop +
                              statByEnergy.impactOnMirrorTop;
    total.impactOnObstruction = total.impactOnObstruction +
                              statByEnergy.impactOnObstruction;
    total.impactOnOuterHousingCount = total.impactOnOuterHousingCount +
                              statByEnergy.impactOnOuterHousingCount;
    total.impactOnInnerHousingCount = total.impactOnInnerHousingCount +
                              statByEnergy.impactOnInnerHousingCount;
    total.absorbedOnFrontMirrorFaceCount = total.absorbedOnFrontMirrorFaceCount+
                              statByEnergy.absorbedOnFrontMirrorFaceCount;
    total.absorbedOnBackMirrorFaceCount = total.absorbedOnBackMirrorFaceCount +
                              statByEnergy.absorbedOnBackMirrorFaceCount;
    total.photonCanNotLeaveMirrorCount = total.photonCanNotLeaveMirrorCount +
                              statByEnergy.photonCanNotLeaveMirrorCount;
    total.photonErrorCount = total.photonErrorCount +
                              statByEnergy.photonErrorCount;
    for (int i = 0; i < sourceRadialProfileBins; i++)
        {
        total.impactBySourceRadialBin[i] = total.impactBySourceRadialBin[i] +
                                        statByEnergy.impactBySourceRadialBin[i];
        }
    for (int i = 0; i < psfRadialProfileBins; i++)
        {
        total.psfRadialBin[i] = total.psfRadialBin[i] +
                                statByEnergy.psfRadialBin[i];
        }

}

void 
XrrtStat::zeroByEnergy()
{
    statByEnergy.tracedPhotonCount = 0;
    statByEnergy.tracedWithinPsfCoreRadiusCount = 0;
    statByEnergy.impactOnFocalPlaneCount = 0;
    statByEnergy.fpOnePerLayerCount = 0;
    statByEnergy.fpOneLayerOnlyCount = 0;
    statByEnergy.fpLayerOneOnlyCount = 0;
    statByEnergy.fpLayerTwoOnlyCount = 0;
    statByEnergy.fpNoReflectionCount = 0;
    statByEnergy.fpAbnormalPathCount = 0;
    statByEnergy.impactOnFPMaskCount = 0;
    statByEnergy.fpMaskOnePerLayerCount = 0;
    statByEnergy.fpMaskOneLayerOnlyCount = 0;
    statByEnergy.fpMaskLayerOneOnlyCount = 0;
    statByEnergy.fpMaskLayerTwoOnlyCount = 0;
    statByEnergy.fpMaskNoReflectionCount = 0;
    statByEnergy.fpMaskAbnormalPathCount = 0;
    statByEnergy.impactOnMirrorTop = 0;
    statByEnergy.impactOnObstruction = 0;
    statByEnergy.impactOnOuterHousingCount = 0;
    statByEnergy.impactOnMirrorTop = 0;
    statByEnergy.impactOnInnerHousingCount = 0;
    statByEnergy.absorbedOnFrontMirrorFaceCount = 0;
    statByEnergy.absorbedOnBackMirrorFaceCount = 0;
    statByEnergy.photonCanNotLeaveMirrorCount = 0;
    statByEnergy.photonErrorCount = 0;
    for (int i = 0; i < sourceRadialProfileBins; i++)
        {
        statByEnergy.impactBySourceRadialBin[i] = 0;
        }
    for (int i = 0; i < psfRadialProfileBins; i++)
        {
        statByEnergy.psfRadialBin[i] = 0;
        }
}

void 
XrrtStat::setFocalPlaneScale(const double& sourceOffsetAngleRadian,
                             const double& sourceRotationAngleRadian,
                             const double& nominalFocalLengthMM)
{
const double RADIAN2ARCMIN = 2.0e0*3.14159265358979323846e0/360.0e0/60.0e0;

    designFocalLengthMM = nominalFocalLengthMM;
    focalPlaneMM2Arcmin = asin(1.0/nominalFocalLengthMM)/RADIAN2ARCMIN;
    sourceFocalPlaneXCenterMM = sourceOffsetAngleRadian*nominalFocalLengthMM*
                                cos(sourceRotationAngleRadian);
    sourceFocalPlaneYCenterMM = sourceOffsetAngleRadian*nominalFocalLengthMM*
                                sin(sourceRotationAngleRadian);
}

void 
XrrtStat::setResultCollectionON()
{
    collectResultStat = true;
}

void 
XrrtStat::setResultCollectionOFF()
{
    collectResultStat = false;
}

bool 
XrrtStat::getResultCollectionStatus() const
{
    return collectResultStat;
}

void 
XrrtStat::setResultStatFITSOutputON()
{
    outputResultStat = true;
    collectResultStat = true;
}

void 
XrrtStat::setResultStatFITSOutputOFF()
{
    outputResultStat = false;
}

bool 
XrrtStat::getResultStatFITSOutputStatus() const
{
    return outputResultStat;
}

int 
XrrtStat::getCoreTraceCount() const
{
    return statByEnergy.tracedWithinPsfCoreRadiusCount;
}


void 
XrrtStat::setPhotonHistoryON()
{
    outputPhotonHistory = true;
}

void 
XrrtStat::setPhotonHistoryOFF()
{
    outputPhotonHistory = false;
}

bool 
XrrtStat::getPhotonHistoryStatus() const
{
    return outputPhotonHistory;
}

void 
XrrtStat::setImageCollectionON()
{
    collectImage = true;
}

void 
XrrtStat::setImageCollectionOFF()
{
    collectImage = false;
}

bool 
XrrtStat::getImageCollectionStatus() const
{
    return collectImage;
}

void 
XrrtStat::setImageSaveByEnergyON()
{
    saveImageByEnergy = true;
}

void 
XrrtStat::setImageSaveByEnergyOFF()
{
    saveImageByEnergy = false;
}

bool 
XrrtStat::getImageSaveByEnergyStatus() const
{
    return saveImageByEnergy;
}


void 
XrrtStat::setResultStatFileData(const string& fileName, 
                                const string& tableName)
{
    resultStatFileName = fileName;
    resultStatTableName = tableName;
}

void 
XrrtStat::setPhotonHistoryFileData(const string& fileName, 
                                   const string& tableName)
{
    photonHistoryFileName = fileName;
    photonHistoryTableName = tableName;
}

void 
XrrtStat::setImageFileData(const string& fileName, 
                           const string& tableName)
{
    imageFileName = fileName;
    imageTableName = tableName;
}

void 
XrrtStat::setAreaInMMSqr(const double& area)
{
    areaInMMSqr = area;
}

void 
XrrtStat::setImageSize(const int& xyBins,
                       const double&       xyMinMM,
                       const double&       xyMaxMM)
{
    xyAxisBins  = xyBins;
    xyAxisMinMM = xyMinMM;
    xyAxisMaxMM = xyMaxMM;
}

fitsfile* 
XrrtStat::getPhotonHistoryFilePtr()
{
    return photonHistoryFile;
}

fitsfile* 
XrrtStat::getResultStatFilePtr()
{
    return resultStatFile;
}

fitsfile* 
XrrtStat::getImageFilePtr()
{
    return imageFile;
}

void 
XrrtStat::fitsFileHistoryWrite(fitsfile* fitsFile, string& comment)
{
int fitsStatus = 0;
    if (fitsFile != 0)
       {
       fits_write_history(fitsFile, (char*) comment.c_str(), &fitsStatus);
       if (fitsStatus != 0)
          {
          throw fitsStatus;
          }
       }
    else
       {
       throw fitsFileNotOpen;
       }
}

void 
XrrtStat::fitsFileCommentWrite(fitsfile* fitsFile, string& comment)
{
int fitsStatus = 0;
    if (fitsFile != 0)
       {
       fits_write_comment(fitsFile, (char*) comment.c_str(), &fitsStatus);
       if (fitsStatus != 0)
          {
          throw fitsStatus;
          }
       }
    else
       {
       throw fitsFileNotOpen;
       }
}

void 
XrrtStat::fitsFileKeywordWrite(fitsfile* fitsFile, string& keyword, 
                               int& value, string& comment)
{
int fitsStatus = 0;
int passValue = value;
    if (fitsFile != 0)
       {
       fits_write_key(fitsFile, TINT, (char*) keyword.c_str(), &passValue,
                      (char*) comment.c_str(), &fitsStatus);
       if (fitsStatus != 0)
          {
          throw fitsStatus;
          }
       }
    else
       {
       throw fitsFileNotOpen;
       }
}
void 
XrrtStat::fitsFileKeywordWrite(fitsfile* fitsFile, string& keyword, 
                               unsigned int& value, string& comment)
{
int fitsStatus = 0;
long passValue = value;
    if (fitsFile != 0)
       {
       fits_write_key(fitsFile, TULONG, (char*) keyword.c_str(), &passValue,
                      (char*) comment.c_str(), &fitsStatus);
       if (fitsStatus != 0)
          {
          throw fitsStatus;
          }
       }
    else
       {
       throw fitsFileNotOpen;
       }
}

void 
XrrtStat::fitsFileKeywordWrite(fitsfile* fitsFile, string& keyword, 
                               float& value, string& comment)
{
int fitsStatus = 0;
float passValue = value;
    if (fitsFile != 0)
       {
       fits_write_key(fitsFile, TFLOAT, (char*) keyword.c_str(), &passValue,
                      (char*) comment.c_str(), &fitsStatus);
       if (fitsStatus != 0)
          {
          throw fitsStatus;
          }
       }
    else
       {
       throw fitsFileNotOpen;
       }
}
void 
XrrtStat::fitsFileKeywordWrite(fitsfile* fitsFile, string& keyword, 
                               double& value, string& comment)
{
int fitsStatus = 0;
double passValue = value;
    if (fitsFile != 0)
       {
       fits_write_key(fitsFile, TDOUBLE, (char*) keyword.c_str(), &passValue,
                      (char*) comment.c_str(), &fitsStatus);
       if (fitsStatus != 0)
          {
          throw fitsStatus;
          }
       }
    else
       {
       throw fitsFileNotOpen;
       }
}

void 
XrrtStat::fitsFileKeywordWrite(fitsfile* fitsFile, string& keyword, 
                               bool& value, string& comment)
{
int fitsStatus = 0;
int passValue = value;
    if (fitsFile != 0)
       {
       fits_write_key(fitsFile, TLOGICAL, (char*) keyword.c_str(), &passValue,
                      (char*) comment.c_str(), &fitsStatus);
       if (fitsStatus != 0)
          {
          throw fitsStatus;
          }
       }
    else
       {
       throw fitsFileNotOpen;
       }
}

void 
XrrtStat::fitsFileKeywordWrite(fitsfile* fitsFile, string& keyword, 
                               string& value, string& comment)
{
int fitsStatus = 0;
    if (fitsFile != 0)
       {
       fits_write_key_longstr(fitsFile, (char*) keyword.c_str(),
                      (char*) value.c_str(),
                      (char*) comment.c_str(), &fitsStatus);
       if (fitsStatus != 0)
          {
          throw fitsStatus;
          }
       }
    else
       {
       throw fitsFileNotOpen;
       }
}


void 
XrrtStat::resultStatHistoryWrite(string& comment)
{
     fitsFileHistoryWrite(resultStatFile, comment);
}

void 
XrrtStat::resultStatCommentWrite(string& comment)
{
    fitsFileCommentWrite(resultStatFile, comment);
}

void 
XrrtStat::resultStatKeywordWrite(string& keyword, unsigned int& value, 
                                      string& comment)
{
    fitsFileKeywordWrite(resultStatFile, keyword, value, comment);
}

void 
XrrtStat::resultStatKeywordWrite(string& keyword, int& value, 
                                      string& comment)
{
    fitsFileKeywordWrite(resultStatFile, keyword, value, comment);
}

void 
XrrtStat::resultStatKeywordWrite(string& keyword, float& value, 
                                      string& comment)
{
     fitsFileKeywordWrite(resultStatFile, keyword, value, comment);
}

void 
XrrtStat::resultStatKeywordWrite(string& keyword, double& value, 
                                      string& comment)
{
    fitsFileKeywordWrite(resultStatFile, keyword, value, comment);
}

void 
XrrtStat::resultStatKeywordWrite(string& keyword, bool& value, 
                                      string& comment)
{
    fitsFileKeywordWrite(resultStatFile, keyword, value, comment);
}

void 
XrrtStat::resultStatKeywordWrite(string& keyword, string& value, 
                                 string& comment)
{
    fitsFileKeywordWrite(resultStatFile, keyword, value, comment);
}


void 
XrrtStat::photonHistoryHistoryWrite(string& comment)
{
    fitsFileHistoryWrite(photonHistoryFile, comment);
}

void 
XrrtStat::photonHistoryCommentWrite(string& comment)
{
    fitsFileCommentWrite(photonHistoryFile, comment);
}

void 
XrrtStat::photonHistoryKeywordWrite(string& keyword, unsigned int& value, 
                                         string& comment)
{
    fitsFileKeywordWrite(photonHistoryFile, keyword, value, comment);
}

void 
XrrtStat::photonHistoryKeywordWrite(string& keyword, int& value, 
                                         string& comment)
{
    fitsFileKeywordWrite(photonHistoryFile, keyword, value, comment);
}

void 
XrrtStat::photonHistoryKeywordWrite(string& keyword, float& value, 
                                         string& comment)
{
    fitsFileKeywordWrite(photonHistoryFile, keyword, value, comment);
}

void 
XrrtStat::photonHistoryKeywordWrite(string& keyword, double& value, 
                                         string& comment)
{
    fitsFileKeywordWrite(photonHistoryFile, keyword, value, comment);
}

void 
XrrtStat::photonHistoryKeywordWrite(string& keyword, bool& value, 
                                         string& comment)
{
    fitsFileKeywordWrite(photonHistoryFile, keyword, value, comment);
}

void 
XrrtStat::photonHistoryKeywordWrite(string& keyword, string& value, 
                                 string& comment)
{
    fitsFileKeywordWrite(photonHistoryFile, keyword, value, comment);
}


void 
XrrtStat::imageHistoryWrite(string& comment)
{
    fitsFileHistoryWrite(imageFile, comment);
}

void 
XrrtStat::imageCommentWrite(string& comment)
{
    fitsFileCommentWrite(imageFile, comment);
}

void 
XrrtStat::imageKeywordWrite(string& keyword, unsigned int& value, 
                                 string& comment)
{
    fitsFileKeywordWrite(imageFile, keyword, value, comment);
}

void 
XrrtStat::imageKeywordWrite(string& keyword, int& value, 
                                 string& comment)
{
    fitsFileKeywordWrite(imageFile, keyword, value, comment);
}

void 
XrrtStat::imageKeywordWrite(string& keyword, float& value, 
                                 string& comment)
{
    fitsFileKeywordWrite(imageFile, keyword, value, comment);
}

void 
XrrtStat::imageKeywordWrite(string& keyword, double& value, 
                                 string& comment)
{
    fitsFileKeywordWrite(imageFile, keyword, value, comment);
}

void 
XrrtStat::imageKeywordWrite(string& keyword, bool& value, 
                                 string& comment)
{
    fitsFileKeywordWrite(imageFile, keyword, value, comment);
}

void 
XrrtStat::imageKeywordWrite(string& keyword, string& value, 
                                 string& comment)
{
    fitsFileKeywordWrite(imageFile, keyword, value, comment);
}

void 
XrrtStat::setFocalPlaneMask(XrrtPolygon& mask)
{
    focalPlaneMask = mask;
}

void 
XrrtStat::setPsfCoreRadiusLimitRadian(const double& angle)
{
    psfCoreRadiusLimitRadian = angle;
}

double 
XrrtStat::getGeometricalAreaMM2()
{
    return areaInMMSqr;
}

long int 
XrrtStat::getCumPhotonCntByRadius(double& arcminRadius)
{
int binNumber;
int sum = 0;

    binNumber = (int) (arcminRadius * sourceRadialBinsPerArcmin);
    if (binNumber > sourceRadialProfileBins)
       {
       binNumber = sourceRadialProfileBins;
       }
    accumulate(total.impactBySourceRadialBin.begin(),
	       // &total.impactBySourceRadialBin[binNumber],
	       // static_cast<vector<int>::iterator>(&total.impactBySourceRadialBin[binNumber]),
			   total.impactBySourceRadialBin.begin() + binNumber,
               sum);
    return sum;
}

double 
XrrtStat::getPsfRadialBinSizeMM()
{
     return  1.0e0 / psfRadialProfileBinsPerMM;
}

double 
XrrtStat::getPsfRadialBinSizeArcmin()
{
     return  1.0e0 / psfRadialProfileBinsPerMM * focalPlaneMM2Arcmin;
}

void
XrrtStat::setRADEC(const double& ra, const double& dec)
{
    nominalRAAtCenter = ra;
    nominalDECAtCenter = dec;
}
