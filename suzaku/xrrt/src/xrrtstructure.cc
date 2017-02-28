// xrrtstructure.cc
//
// Member functions for Structure class
//
// Richard L Fink GSFC/631
// 1997/06/04
// 1997/09/25 Upgrade documentation. R. Fink

// Use RCS (Revision Control System) by HIDEYUKI MORI
// Revision 2.2  2000/11/21 08:54:02  mori
// Include ID and LOG macro

#include "xrrtstructure.hh"

XrrtStructure& theStructure()
{
//
// Return the static structure object for the program.
//
static XrrtStructure theStructure;
return theStructure;
}


XrrtStructure::XrrtStructure():
    numberOfLayers(0),
    innerHousingRadius(0),
    outerHousingRadius(0),
    focalLengthInMM(0),
    mirrorList(),
    mirrorOuterRadiusList(),
    shadowList(),
    // Added collimator structure for initialization (modified by HIDEYUKI MORI)
    collimatorList(),
    collimatorOuterRadiusList(),
    layerDistance(),
    layerType(),
    layerStart(),
    layerEnd()
{
// A simple constructor
}

string
XrrtStructure::errorMessage(XrrtStructureErrorCode errorCode)
{
//
// Convert error codes to error messages
//
string errorMessage;

    switch (errorCode)
        {
        case badPointerInObstructionList:
            errorMessage = 
            "Bad obstruction pointer found in XrrtStructure";
            break;
        default:
                {
                char charNumber[1024];
                sprintf(charNumber, "%d",errorCode);
                errorMessage = 
                           "XrrtStructure::errorMessage Unknown error code: ";
                errorMessage.append(charNumber);
                }
             break;
        }
     return errorMessage;
}

string
XrrtStructure::errorMessage(XrrtMirrorErrorCode errorCode)
{
//
// A pass thru to get XrrtMirror error messages.
//
string errorMessage;

     errorMessage = mirrorList[0]->errorMessage(errorCode);
     return errorMessage;
}

string
XrrtStructure::errorMessage(XrrtObstructionErrorCode errorCode)
{
//
// A pass thru to get XrrtObstruction error messages.
//
string errorMessage;

     errorMessage = shadowList[0]->errorMessage(errorCode);
     return errorMessage;
}


void
XrrtStructure::organizeStructure()
{
//
// This code works out the telescope structure from the relative
// positions of the parts of the telescope.
//

    // Sort the mirrors and obstructions into descending order by distance
    // from focal plane
    // If this code looks strange, it is the fault of gcc and the stl.
//    sort(mirrorList.begin(), mirrorList.end()); 
//    reverse(mirrorList.begin(), mirrorList.end());
//    sort(shadowList.begin(), shadowList.end()); 
//    reverse(shadowList.begin(), shadowList.end());

    // mirrorList is now ordered (furtherest from focal plane >> closest),
    //      (furtherest from center >> closest to center)
    // shadowList is now ordered (furtherest from focal plane >> closest)
    //      and random across the field
// I wish it were but this code does not yet work as you can tell since it
// is commented out. So the user must supply properly sorted FITS files
// until it does work.

    // Here we determine the layer structure of the telescope. 
    // This is essentially a merge of the mirrors and obstructions
    // except that since they are different kinds of objects we 
    // can't stick them directly into a single list. So we assign
    // layer numbers to them individually. We do have a problem in that
    // we want the layers be used to accelerate the program (i.e. if
    // a photon impacts a mirror in a layer, no other mirror in that
    // layer need be examined).
  //
  // Add collimator in telescope structure
  // To do this, we add collimator list in this routine. So layer number
  // is increased for the modification, and collimator layer is fixed by 0.
  // Mirrors and obstructions code which construct telescope structure 
  // are not almost modified because this is not relate to collimators code.
  // (modified by HIDEYUKI MORI)

    Count layer = 0;
    vector<XrrtMirror*>::iterator mirrorIterator = mirrorList.begin();
    vector<XrrtObstruction*>::iterator shadowIterator = shadowList.begin();
    vector<XrrtCollimator*>::iterator collimatorIterator = collimatorList.begin();
    bool lastWasShadow = false;
    double shadowDistance = 0;
    double mirrorDistance = 0;
    // Add collimator distance from focal plane for later use
    // (modified by HIDEYUKI MORI)
    double collimatorDistance = 0;
    double bottomOfLastMirror = 0;
    double bottomOfLastCollimator = 0;
    // Add collimator case for ASTRO-E-II pre-collimator
    // (modified by HIDEYUKI MORI)
    //
    // Now, how to start this thing up?
    // There are two cases:
    // 1) the first object in the telescope is an obstruction (default case)
    // 2) the first object is a mirror (less likely but not impossible)
    // 3) the first object is a collimator (use ASTRO-E-II pre-collimator)
    // Identify which case we have and set the defaults going in for that
    // case so the code works correctly.
    //
    // Set the distance of the 1st obstruction and mirror
    //
    if (shadowIterator < shadowList.end())
       {
       shadowDistance = (*shadowIterator)->getFPDistance();

       // OKADA (20061106)
       //       fprintf(stdout,"shadowDistance = %lf\n",shadowDistance);

       }
    else
       {
       shadowDistance = 0;
       }
    if (mirrorIterator < mirrorList.end())
       {
       mirrorDistance = (*mirrorIterator)->getTopDistance();
       }
    else
       {
       mirrorDistance = 0;
       }
    if (collimatorIterator < collimatorList.end())
       {
       collimatorDistance = (*collimatorIterator)->getColTopDistance();
       }
    //
    // Which comes 1st?
    //
    if (shadowDistance >= mirrorDistance && shadowDistance >= collimatorDistance)
       {
       //
       // Obstructions come 1st
       //
       // The 1st obstruction is layer 0
       //
	 // Layer number is modified by adding collimator
	 // So first mirror or obstruction layer number is 1
	 // (modified by HIDEYUKI MORI)
       layer=0;
       //
       // The bottom of the last mirror is defaulted to zero since it won't
       //     be needed
       //
       bottomOfLastMirror=0;
       bottomOfLastCollimator=0;
       //
       // lastWasShadow true pretends we already handled a shadow
       //
       lastWasShadow=true;
       //
       // The layerType[layer=0] is an Obstruction
       //
       // Change to layerType[layer=1] (modified by HIDEYUKI MORI)
       layerType.push_back(OBSTRUCTION);
       //
       // layerStart[layer=0] is the 1st entry in the shadowList
       //
       // Change to layerStart[layer=1] (modified by HIDEYUKI MORI)
       layerStart.push_back(0);
       //
       // Dummy end of layer position is inserted
       //
       layerEnd.push_back(0);
       //
       // The layer distance is the distance of the 1st member
       //
       layerDistance.push_back(shadowDistance);
       }
    else if(mirrorDistance >= shadowDistance && mirrorDistance >= collimatorDistance)
       {
       //
       // Mirror comes 1st
       //
       // The 1st Mirror is in layer zero
       //
	 // Layer number is modified by adding collimator
	 // So first mirror or obstruction layer number is 1
	 // (modified by HIDEYUKI MORI)
       layer = 0;
       //
       // We imply that mirrors are already being handled
       //
       lastWasShadow=false;
       //
       // We default the mirror bottom so no layer change can occur
       //
       bottomOfLastMirror = (*mirrorIterator)->getBottomDistance();
       //
       // The layerType[layer=0] is a Mirror
       //
       // Change to layerType[layer=1] (modified by HIDEYUKI MORI)
       layerType.push_back(MIRROR);
       //
       // layerStart[layer=0] is the 1st entry in the mirrorList
       // 
       // Change to layerStart[layer=1] (modified by HIDEYUKI MORI)
       layerStart.push_back(0);
       //
       // Dummy end of layer position is inserted
       //
       layerEnd.push_back(0);
       //
       // The layer distance is the distance of the 1st member
       //
       layerDistance.push_back(mirrorDistance);
       }
    else
       {
       //
       // Pre-Collimator comes 1st
       //
       // The 1st Collimator is in layer zero
       //
	 // Layer number is modified by adding collimator
	 // So first mirror or obstruction layer number is 1
	 // (modified by HIDEYUKI MORI)
       layer = 0;
       //
       // We imply that collimators are already being handled
       //
       lastWasShadow=false;
       //
       // We default the collimator bottom so no layer change can occur
       //
       bottomOfLastCollimator = (*collimatorIterator)->getColBottomDistance();
       //
       // The layerType[layer=0] is a Mirror
       //
       // Change to layerType[layer=1] (modified by HIDEYUKI MORI)
       layerType.push_back(COLLIMATOR);
       //
       // layerStart[layer=0] is the 1st entry in the mirrorList
       // 
       // Change to layerStart[layer=1] (modified by HIDEYUKI MORI)
       layerStart.push_back(0);
       //
       // Dummy end of layer position is inserted
       //
       layerEnd.push_back(0);
       //
       // The layer distance is the distance of the 1st member
       //
       layerDistance.push_back(collimatorDistance);
       }
    
    //
    // Loop until all mirrors and all obstructions have been handled
    //
    while ( (shadowDistance != 0) || (mirrorDistance != 0) || (collimatorDistance != 0))
        {
        if (shadowDistance >= mirrorDistance && shadowDistance >= collimatorDistance)
           {
           // The obstruction is further away than the mirror top and collimator top
           // Was the last object an obstruction?
           if (!lastWasShadow)
              {
              // We change layers
              layer++;
              lastWasShadow=true;
              layerDistance.push_back(shadowDistance);
              layerType.push_back(OBSTRUCTION);
              layerStart.push_back(shadowIterator-shadowList.begin());
              layerEnd.push_back(0);
              }
           // Mark the obstruction as belonging to the current layer
           (*shadowIterator)->setLayer(layer);
           // Mark this obstruction as the last for the layer
           // since who knows? may be it is.
           layerEnd[layer] = shadowIterator-shadowList.begin();
           shadowIterator++;
           }
        else if(mirrorDistance >= shadowDistance && mirrorDistance >= collimatorDistance)
           {
           // The mirror is further away than the obstruction
           if (lastWasShadow)
              {
              // We have changed layers
              lastWasShadow=false;
              layer++;
              bottomOfLastMirror = (*mirrorIterator)->getBottomDistance();
              layerDistance.push_back(mirrorDistance);
              layerType.push_back(MIRROR);
              layerStart.push_back(mirrorIterator-mirrorList.begin());
              layerEnd.push_back(0);
              }
           else
              {
              //The last object was a mirror
              // But mirrors MAY have layers also
              // Mirror layers increment when the top of the current mirror
              // is less than the bottom of the previous mirror
              if (mirrorDistance < bottomOfLastMirror)
                  {
                  layer++;
                  bottomOfLastMirror = (*mirrorIterator)->getBottomDistance();
                  layerDistance.push_back(mirrorDistance);
                  layerType.push_back(MIRROR);
                  layerStart.push_back(mirrorIterator-mirrorList.begin());
                  layerEnd.push_back(0);
                  }
              }
           // Mark the mirror as belonging to the current layer
           (*mirrorIterator)->setLayer(layer);
           // Mark this mirror as the last for the layer
           // as it always could be.
           layerEnd[layer] = mirrorIterator-mirrorList.begin();
           mirrorIterator++;
           }
	else
           {
           // The colliamtor is further away than the others
           if (lastWasShadow)
              {
              // We have changed layers
              lastWasShadow=false;
              layer++;
              bottomOfLastCollimator = (*collimatorIterator)->getColBottomDistance();
              layerDistance.push_back(collimatorDistance);
              layerType.push_back(COLLIMATOR);
              layerStart.push_back(collimatorIterator-collimatorList.begin());
              layerEnd.push_back(0);
              }
           // Mark the mirror as belonging to the current layer
           (*collimatorIterator)->setColLayer(layer);
           // Mark this mirror as the last for the layer
           // as it always could be.
           layerEnd[layer] = collimatorIterator-collimatorList.begin();
           collimatorIterator++;
	   }

        // Set the distance of the current obstruction, mirror and collimator
        if (shadowIterator < shadowList.end())
           {
           shadowDistance = (*shadowIterator)->getFPDistance();
           }
        else
           {
           shadowDistance = 0;
           }
        if (mirrorIterator < mirrorList.end())
           {
           mirrorDistance = (*mirrorIterator)->getTopDistance();
           }
        else
           {
           mirrorDistance = 0;
           }
	if (collimatorIterator < collimatorList.end())
	  {
	  collimatorDistance = (*collimatorIterator)->getColTopDistance();
	  }
	else
	  {
	  collimatorDistance = 0;
	  }

        }
    //
    // We now know how the telescope is organized
    //
    // Save the number of layers created for later use.
    //
    layer++; // We started with 0, remember
    numberOfLayers = layer;

    // 
    // Add this code copied by mirror section (modified by HIDEYUKI MORI)
    // Create the list of collimator outer top radii
    for (int i = 0; (unsigned int)i < collimatorList.size(); i++)
        {
        collimatorOuterRadiusList.push_back(collimatorList[i]->getColTopOuterRadius());
        }
    //
    // Add this code copied by mirror section (modified by HIDEYUKI MORI)
    // Construct the collimator inner/outer relationship
    //
    createInnerOuterCollimatorStructure();
    // 
    // Create the list of mirror outer top radii
    for (int i = 0; (unsigned int)i < mirrorList.size(); i++)
        {
        mirrorOuterRadiusList.push_back(mirrorList[i]->getTopOuterRadius());
        }

    //
    // Construct the mirror inner/outer relationship
    //
    createInnerOuterMirrorStructure();
    //
    // Go thru the obstruction layers and link them together
    // First link all multiQuadrant types together;
    // Second link all firstQuadrant types together and then to the multiQuad;
    // Then secondQuadrant, thirdQuadrant, fourthQuadrant
    //
    // Check all layers
    for (int loopLayer=0; loopLayer < numberOfLayers; loopLayer++)
       {
       vector<XrrtObstruction*> byQuadrant;
       if (layerType[loopLayer] == MIRROR)
          {
          // Skip mirror layers
          byQuadrant.push_back(0); // noQuadrant
          byQuadrant.push_back(0); // firstQuadrant
          byQuadrant.push_back(0); // secondQuadrant
          byQuadrant.push_back(0); // thirdQuadrant
          byQuadrant.push_back(0); // fourthQuadrant
          byQuadrant.push_back(0); // multiQuadrant
          shadowByQuadrantBylayer.push_back(byQuadrant);
          continue;
          }
       else if (layerType[loopLayer] == COLLIMATOR)
          {
          // Skip collimator layers
          byQuadrant.push_back(0); // noQuadrant
          byQuadrant.push_back(0); // firstQuadrant
          byQuadrant.push_back(0); // secondQuadrant
          byQuadrant.push_back(0); // thirdQuadrant
          byQuadrant.push_back(0); // fourthQuadrant
          byQuadrant.push_back(0); // multiQuadrant
          shadowByQuadrantBylayer.push_back(byQuadrant);
          continue;
          }
       //
       // Find the 1st multiQuadrant obstruction
       //
       int shadowIndex = layerStart[loopLayer];
       int firstMultiQuadrant = -1;
       int lastMultiQuadrant  = -1;
       //
       // Search the list until a multiQuadrant obstruction is found or the
       // end of the layer occurs
       //
       while (shadowList[shadowIndex]->getTelescopeQuadrant() != multiQuadrant)
           {
           shadowIndex++;
           if (shadowIndex > layerEnd[loopLayer])
              {
              break;
              }
           }
       if (shadowIndex <= layerEnd[loopLayer])
           {
           // multiQuadrant obstruction found
           // Since at least one exists we must now try to link any following
           // ones to it to form a chain
           firstMultiQuadrant = shadowIndex;
           lastMultiQuadrant = shadowIndex;
           shadowIndex++;
           for (int multiSearchIndex = shadowIndex; 
                multiSearchIndex <= layerEnd[loopLayer]; 
                multiSearchIndex++)
               {
               if (shadowList[multiSearchIndex]->getTelescopeQuadrant() ==
                   multiQuadrant)
                  {
                  // Link the previous multiQuadrant obstruction to the current
                  shadowList[lastMultiQuadrant]->setNextInLayer(
                                                  shadowList[multiSearchIndex]);
                  lastMultiQuadrant = multiSearchIndex;
                  }
               }
           }
       //
       // All the multiQuadrant obstructions (if any) have been chained together
       //
       // Now chain all the firstQuadrant obstructions together
       //
       shadowIndex = layerStart[loopLayer];
       int firstFirstQuadrant = -1;
       int lastFirstQuadrant  = -1;
       //
       // Search the list until a firstQuadrant obstruction is found
       //
       while (shadowList[shadowIndex]->getTelescopeQuadrant() != firstQuadrant)
           {
           shadowIndex++;
           if (shadowIndex > layerEnd[loopLayer])
              {
              break;
              }
           }
       if (shadowIndex <= layerEnd[loopLayer])
           {
           // firstQuadrant obstruction found
           // Since at least one exists we must now try to link any following
           // ones to it to form a chain
           firstFirstQuadrant = shadowIndex;
           lastFirstQuadrant = shadowIndex;
           shadowIndex++;
           for (int firstSearchIndex = shadowIndex; 
                firstSearchIndex <= layerEnd[loopLayer]; 
                firstSearchIndex++)
               {
               if (shadowList[firstSearchIndex]->getTelescopeQuadrant() ==
                   firstQuadrant)
                  {
                  // Link the previous firstQuadrant obstruction to the current
                  shadowList[lastFirstQuadrant]->setNextInLayer(
                                                  shadowList[firstSearchIndex]);
                  lastFirstQuadrant = firstSearchIndex;
                  }
               }
           // As we drop out here, we must link the last firstQuadrant 
           // obstruction to the first multiQuadrant type (if any)
           if (firstMultiQuadrant != -1)
              {
              // At least one multiQuadrant obstruction exists
              if (lastFirstQuadrant != -1)
                 {
                 // At least one firstQuadrant obstruction exists
                 shadowList[lastFirstQuadrant]->setNextInLayer(
                                                shadowList[firstMultiQuadrant]);
                 }
              else
                 {
                 // Since no firstQuadrant obstructions exist, replace it
                 // with the multiQuadrant entry
                 firstFirstQuadrant = firstMultiQuadrant;
                 }
              }
           }
       //
       // All the firstQuadrant obstructions (if any) have been chained together
       //
       // Now chain all the secondQuadrant obstructions together
       //
       shadowIndex = layerStart[loopLayer];
       int firstSecondQuadrant = -1;
       int lastSecondQuadrant  = -1;
       //
       // Search the list until a secondQuadrant obstruction is found
       //
       while (shadowList[shadowIndex]->getTelescopeQuadrant() != secondQuadrant)
           {
           shadowIndex++;
           if (shadowIndex > layerEnd[loopLayer])
              {
              break;
              }
           }
       if (shadowIndex <= layerEnd[loopLayer])
           {
           // secondQuadrant obstruction found
           // Since at least one exists we must now try to link any following
           // ones to it to form a chain
           firstSecondQuadrant = shadowIndex;
           lastSecondQuadrant = shadowIndex;
           shadowIndex++;
           for (int secondSearchIndex = shadowIndex; 
                secondSearchIndex <= layerEnd[loopLayer]; 
                secondSearchIndex++)
               {
               if (shadowList[secondSearchIndex]->getTelescopeQuadrant() ==
                   secondQuadrant)
                  {
                  // Link the previous secondQuadrant obstruction to the current
                  shadowList[lastSecondQuadrant]->setNextInLayer(
                                                 shadowList[secondSearchIndex]);
                  lastSecondQuadrant = secondSearchIndex;
                  }
               }
           // As we drop out here, we must link the last secondQuadrant 
           // obstruction to the first multiQuadrant type (if any)
           if (firstMultiQuadrant != -1)
              {
              // At least one multiQuadrant obstruction exists
              if (lastSecondQuadrant != -1)
                 {
                 // At least one secondQuadrant obstruction exists
                 shadowList[lastSecondQuadrant]->setNextInLayer(
                                               shadowList[firstMultiQuadrant]);
                 }
              else
                 {
                 // Since no secondQuadrant obstructions exist, replace it
                 // with the multiQuadrant entry
                 firstSecondQuadrant = firstMultiQuadrant;
                 }
              }
           }
       //
       // All the secondQuadrant obstructions (if any) have been chained together
       //
       // Now chain all the thirdQuadrant obstructions together
       //
       shadowIndex = layerStart[loopLayer];
       int firstThirdQuadrant = -1;
       int lastThirdQuadrant  = -1;
       //
       // Search the list until a thirdQuadrant obstruction is found
       //
       while (shadowList[shadowIndex]->getTelescopeQuadrant() != thirdQuadrant)
           {
           shadowIndex++;
           if (shadowIndex > layerEnd[loopLayer])
              {
              break;
              }
           }
       if (shadowIndex <= layerEnd[loopLayer])
           {
           // thirdQuadrant obstruction found
           // Since at least one exists we must now try to link any following
           // ones to it to form a chain
           firstThirdQuadrant = shadowIndex;
           lastThirdQuadrant = shadowIndex;
           shadowIndex++;
           for (int thirdSearchIndex = shadowIndex; 
                thirdSearchIndex <= layerEnd[loopLayer]; 
                thirdSearchIndex++)
               {
               if (shadowList[thirdSearchIndex]->getTelescopeQuadrant() ==
                   thirdQuadrant)
                  {
                  // Link the previous thirdQuadrant obstruction to the current
                  shadowList[lastThirdQuadrant]->setNextInLayer(
                                                 shadowList[thirdSearchIndex]);
                  lastThirdQuadrant = thirdSearchIndex;
                  }
               }
           // As we drop out here, we must link the last thirdQuadrant 
           // obstruction to the first multiQuadrant type (if any)
           if (firstMultiQuadrant != -1)
              {
              // At least one multiQuadrant obstruction exists
              if (lastThirdQuadrant != -1)
                 {
                 // At least one thirdQuadrant obstruction exists
                 shadowList[lastThirdQuadrant]->setNextInLayer(
                                               shadowList[firstMultiQuadrant]); 
                }
              else
                 {
                 // Since no thirdQuadrant obstructions exist, replace it
                 // with the multiQuadrant entry
                 firstThirdQuadrant = firstMultiQuadrant;
                 }
              }
           }
       //
       // All the thirdQuadrant obstructions (if any) have been chained together
       //
       // Now chain all the fourthQuadrant obstructions together
       //
       shadowIndex = layerStart[loopLayer];
       int firstFourthQuadrant = -1;
       int lastFourthQuadrant  = -1;
       //
       // Search the list until a fourthQuadrant obstruction is found
       //
       while (shadowList[shadowIndex]->getTelescopeQuadrant() != fourthQuadrant)
           {
           shadowIndex++;
           if (shadowIndex > layerEnd[loopLayer])
              {
              break;
              }
           }
       if (shadowIndex <= layerEnd[loopLayer])
           {
           // fourthQuadrant obstruction found
           // Since at least one exists we must now try to link any following
           // ones to it to form a chain
           firstFourthQuadrant = shadowIndex;
           lastFourthQuadrant = shadowIndex;
           shadowIndex++;
           for (int fourthSearchIndex = shadowIndex; 
                fourthSearchIndex <= layerEnd[loopLayer]; 
                fourthSearchIndex++)
               {
               if (shadowList[fourthSearchIndex]->getTelescopeQuadrant() ==
                   fourthQuadrant)
                  {
                  // Link the previous fourthQuadrant obstruction to the current
                  shadowList[lastFourthQuadrant]->setNextInLayer(
                                                 shadowList[fourthSearchIndex]);
                  lastFourthQuadrant = fourthSearchIndex;
                  }
               }
           // As we drop out here, we must link the last fourthQuadrant 
           // obstruction to the first multiQuadrant type (if any)
           if (firstMultiQuadrant != -1)
              {
              // At least one multiQuadrant obstruction exists
              if (lastFourthQuadrant != -1)
                 {
                 // At least one fourthQuadrant obstruction exists
                 shadowList[lastFourthQuadrant]->setNextInLayer(
                                               shadowList[firstMultiQuadrant]); 
                }
              else
                 {
                 // Since no fourthQuadrant obstructions exist, replace it
                 // with the multiQuadrant entry
                 firstFourthQuadrant = firstMultiQuadrant;
                 }
              }
           }
       // Create an accellerator by tracking the XrrtObstruction* for the
       // first obstruction per quadrant
       byQuadrant.push_back(0); // noQuadrant case
       if (firstFirstQuadrant != -1)
          {
          byQuadrant.push_back(shadowList[firstFirstQuadrant]);
          }
       else
          {
          byQuadrant.push_back(0);
          }
       if (firstSecondQuadrant != -1)
          {
          byQuadrant.push_back(shadowList[firstSecondQuadrant]);
          }
       else
          {
          byQuadrant.push_back(0);
          }
       if (firstThirdQuadrant != -1)
          {
          byQuadrant.push_back(shadowList[firstThirdQuadrant]);
          }
       else
          {
          byQuadrant.push_back(0);
          }
       if (firstFourthQuadrant != -1)
          {
          byQuadrant.push_back(shadowList[firstFourthQuadrant]);
          }
       else
          {
          byQuadrant.push_back(0);
          }
       if (firstMultiQuadrant != -1)
          {
          byQuadrant.push_back(shadowList[firstMultiQuadrant]);
          }
       else
          {
          byQuadrant.push_back(0);
          }
       shadowByQuadrantBylayer.push_back(byQuadrant);
       }
}

void
XrrtStructure::createInnerOuterMirrorStructure()
{
//
// For each mirror, determine if it has an outer mirror and and inner
// mirror relative to itself. To speed the code, this relationship has
// been made fixed. That is, all mirrors must have exactly the same
// rotational structure as their inner/outer neighbors.
//
//Note: This code will fail if a mirror start angle is less than an end angle
//
const double TWOPI = 3.14159265358979323846e0*2.0e0;

    // For each layer
    for (Count layer=0; layer<numberOfLayers; layer++)
       {
       if (layerType[layer] == OBSTRUCTION)
          {
          continue;
          }
       // We only need concern ourselves with mirrors that belong to the 
       // same layer
       Count lowerLimit = layerStart[layer];
       Count upperLimit = layerEnd[layer];
       // For each mirror in the layer
       for (Count mirror = lowerLimit; mirror <= upperLimit; mirror++)
           {
           // BUILD THE MIRROR NEIGHBOR STRUCTURE
           if (mirrorList[mirror]->canPhotonCrossStartAngle())
              {
              // Find the neighbor mirror on the start angle side
              // The mirrors are supposed to be sorted so that we can find
              // neighbors nearby
              for (Count nextMirror = mirror; nextMirror <= upperLimit;mirror++)
                 {
                 if (mirrorList[mirror]->getTopInnerRadius() != 
                     mirrorList[nextMirror]->getTopInnerRadius())
                    {
                    // we have gone beyond possible matches so quit looking
                    break;
                    }
                 // check for an exact match
                 if (mirrorList[mirror]->getStartAngle() == 
                     mirrorList[nextMirror]->getEndAngle())
                    {
                    // A Match
                    mirrorList[mirror]->setStartAngleNeighbor(
                                                        mirrorList[nextMirror]);
                    break;
                    }
                 // check for a rotational match (2PI = 0)
                 if (
                    (mirrorList[mirror]->getStartAngle() >= 
                     fmod(mirrorList[nextMirror]->getEndAngle(),TWOPI)-0.0001e0)
                 && (mirrorList[mirror]->getStartAngle() <=
                     fmod(mirrorList[nextMirror]->getEndAngle(),TWOPI)+0.0001e0)
                    )
                    {
                    // A Match
                    mirrorList[mirror]->setStartAngleNeighbor(
                                                        mirrorList[nextMirror]);
                    break;
                    }
                 }
              for (Count nextMirror = mirror; nextMirror >= lowerLimit;mirror--)
                 {
                 if (mirrorList[mirror]->getTopInnerRadius() != 
                     mirrorList[nextMirror]->getTopInnerRadius())
                    {
                    // we have gone beyond possible matches so quit looking
                    break;
                    }
                 // check for an exact match
                 if (mirrorList[mirror]->getStartAngle() == 
                     mirrorList[nextMirror]->getEndAngle())
                    {
                    // A Match
                    mirrorList[mirror]->setStartAngleNeighbor(
                                                        mirrorList[nextMirror]);
                    break;
                    }
                 // check for a rotational match (2PI = 0)
                 if (
                    (mirrorList[mirror]->getStartAngle() >= 
                     fmod(mirrorList[nextMirror]->getEndAngle(),TWOPI)-0.0001e0)
                 && (mirrorList[mirror]->getStartAngle() <=
                     fmod(mirrorList[nextMirror]->getEndAngle(),TWOPI)+0.0001e0)
                    )
                    {
                    // A Match
                    mirrorList[mirror]->setStartAngleNeighbor(
                                                        mirrorList[nextMirror]);
                    break;
                    }
                 }
              }
           if (mirrorList[mirror]->canPhotonCrossEndAngle())
              {
              // Find the mirror on the end angle side
              // The mirrors are supposed to be sorted so that we can find
              // end angle neighbors nearby
              for (Count nextMirror = mirror; nextMirror <= upperLimit;mirror++)
                 {
                 if (mirrorList[mirror]->getTopInnerRadius() != 
                     mirrorList[nextMirror]->getTopInnerRadius())
                    {
                    // we have gone beyond possible matches so quit looking
                    break;
                    }
                 // check for an exact match
                 if (mirrorList[mirror]->getEndAngle() == 
                     mirrorList[nextMirror]->getStartAngle())
                    {
                    // A Match
                    mirrorList[mirror]->setEndAngleNeighbor(
                                                        mirrorList[nextMirror]);
                    break;
                    }
                 // check for a rotational match (2PI = 0)
                 if (
                    (mirrorList[mirror]->getEndAngle() >= 
                     fmod(mirrorList[nextMirror]->getStartAngle(),TWOPI)-0.0001e0)
                 && (mirrorList[mirror]->getEndAngle() <=
                     fmod(mirrorList[nextMirror]->getStartAngle(),TWOPI)+0.0001e0)
                    )
                    {
                    // A Match
                    mirrorList[mirror]->setEndAngleNeighbor(
                                                        mirrorList[nextMirror]);
                    break;
                    }
                 }
              for (Count nextMirror = mirror; nextMirror >= lowerLimit;mirror--)
                 {
                 if (mirrorList[mirror]->getTopInnerRadius() != 
                     mirrorList[nextMirror]->getTopInnerRadius())
                    {
                    // we have gone beyond possible matches so quit looking
                    break;
                    }
                 // check for an exact match
                 if (mirrorList[mirror]->getStartAngle() == 
                     mirrorList[nextMirror]->getEndAngle())
                    {
                    // A Match
                    mirrorList[mirror]->setStartAngleNeighbor(
                                                        mirrorList[nextMirror]);
                    break;
                    }
                 // check for a rotational match (2PI = 0)
                 if (
                    (mirrorList[mirror]->getStartAngle() >= 
                     fmod(mirrorList[nextMirror]->getEndAngle(),TWOPI)-0.0001e0)
                 && (mirrorList[mirror]->getStartAngle() <=
                     fmod(mirrorList[nextMirror]->getEndAngle(),TWOPI)+0.0001e0)
                    )
                    {
                    // A Match
                    mirrorList[mirror]->setStartAngleNeighbor(
                                                        mirrorList[nextMirror]);
                    break;
                    }
                 }
              }
           // BUILD THE INNER MIRROR STRUCTURE
           // Find 1 mirror that has radius less than the current mirror
           // and overlaps it in rotation angle space
           for (Count match=mirror+1; match<= upperLimit; match++)
              {
              if (mirrorList[match]->getEndAngle() <=
                                    mirrorList[mirror]->getStartAngle())
                 {
                 // The mirror is not related to the current mirror
                 continue;
                 }
              if (mirrorList[match]->getStartAngle() >=
                                    mirrorList[mirror]->getEndAngle())
                 {
                 // The mirror is not related to the current mirror
                 continue;
                 }

              // We have found an inner mirror
              mirrorList[mirror]->setInnerMirror(mirrorList[match]);
              break;
              }
           // BUILD THE OUTER MIRROR STRUCTURE
           // Find 1 mirror that has radius greater than the current mirror
           // and overlaps it in rotation angle space
           if (mirror != lowerLimit)
              {
              for (int match=mirror-1; match>= (int)lowerLimit; match--)
                 {
                 if (mirrorList[match]->getEndAngle() <=
                                       mirrorList[mirror]->getStartAngle())
                    {
                    // The mirror is not related to the current mirror
                    continue;
                    }
                 if (mirrorList[match]->getStartAngle() >=
                                       mirrorList[mirror]->getEndAngle())
                    {
                    // The mirror is not related to the current mirror
                    continue;
                    }
   
                 // We have found an outer mirror
                 mirrorList[mirror]->setOuterMirror(mirrorList[match]);
                 break;
                 }
              }
           }
       }
}

// Add the case of collimator routine 
// This code is copied by createInnerOuterMirrorStructure()
// (added by HIDEYUKI MORI)
void
XrrtStructure::createInnerOuterCollimatorStructure()
{
//
// For each collimator, determine if it has an outer collimator and and inner
// collimator relative to itself. To speed the code, this relationship has
// been made fixed. That is, all collimators must have exactly the same
// rotational structure as their inner/outer neighbors.
//
//Note: This code will fail if a collimator start angle is less than an end angle
//
const double TWOPI = 3.14159265358979323846e0*2.0e0;

    // For each layer
    for (Count layer=0; layer<numberOfLayers; layer++)
       {
       if (layerType[layer] == OBSTRUCTION)
          {
          continue;
          }
       if(layerType[layer] == MIRROR)
	 {
	   continue;
	 }
       // We only need concern ourselves with collimators that belong to the 
       // same layer
       Count lowerLimit = layerStart[layer];
       Count upperLimit = layerEnd[layer];
       // For each collimator in the layer
       for (Count collimator = lowerLimit; collimator <= upperLimit; collimator++)
           {
           // BUILD THE COLLIMATOR NEIGHBOR STRUCTURE
           if (mirrorList[collimator]->canPhotonCrossStartAngle())
              {
              // Find the neighbor collimator on the start angle side
              // The collimators are supposed to be sorted so that we can find
              // neighbors nearby
              for (Count nextCollimator = collimator; nextCollimator <= upperLimit; collimator++)
                 {
                 if (collimatorList[collimator]->getColTopInnerRadius() != 
                     collimatorList[nextCollimator]->getColTopInnerRadius())
                    {
                    // we have gone beyond possible matches so quit looking
                    break;
                    }
                 // check for an exact match
                 if (collimatorList[collimator]->getColStartAngle() == 
                     collimatorList[nextCollimator]->getColEndAngle())
                    {
                    // A Match
                    collimatorList[collimator]->setColStartAngleNeighbor(
                                                        collimatorList[nextCollimator]);
                    break;
                    }
                 // check for a rotational match (2PI = 0)
                 if (
                    (collimatorList[collimator]->getColStartAngle() >= 
                     fmod(collimatorList[nextCollimator]->getColEndAngle(),TWOPI)-0.0001e0)
                 && (collimatorList[collimator]->getColStartAngle() <=
                     fmod(collimatorList[nextCollimator]->getColEndAngle(),TWOPI)+0.0001e0)
                    )
                    {
                    // A Match
                    collimatorList[collimator]->setColStartAngleNeighbor(
                                                        collimatorList[nextCollimator]);
                    break;
                    }
                 }
              for (Count nextCollimator = collimator; nextCollimator >= lowerLimit; collimator--)
                 {
                 if (collimatorList[collimator]->getColTopInnerRadius() != 
                     collimatorList[nextCollimator]->getColTopInnerRadius())
                    {
                    // we have gone beyond possible matches so quit looking
                    break;
                    }
                 // check for an exact match
                 if (collimatorList[collimator]->getColStartAngle() == 
                     collimatorList[nextCollimator]->getColEndAngle())
                    {
                    // A Match
                    collimatorList[collimator]->setColStartAngleNeighbor(
                                                        collimatorList[nextCollimator]);
                    break;
                    }
                 // check for a rotational match (2PI = 0)
                 if (
                    (collimatorList[collimator]->getColStartAngle() >= 
                     fmod(collimatorList[nextCollimator]->getColEndAngle(),TWOPI)-0.0001e0)
                 && (collimatorList[collimator]->getColStartAngle() <=
                     fmod(collimatorList[nextCollimator]->getColEndAngle(),TWOPI)+0.0001e0)
                    )
                    {
                    // A Match
                    collimatorList[collimator]->setColStartAngleNeighbor(
                                                        collimatorList[nextCollimator]);
                    break;
                    }
                 }
              }
           if (collimatorList[collimator]->canPhotonCrossColEndAngle())
              {
              // Find the collimator on the end angle side
              // The collimators are supposed to be sorted so that we can find
              // end angle neighbors nearby
              for (Count nextCollimator = collimator; nextCollimator <= upperLimit; collimator++)
                 {
                 if (collimatorList[collimator]->getColTopInnerRadius() != 
                     collimatorList[nextCollimator]->getColTopInnerRadius())
                    {
                    // we have gone beyond possible matches so quit looking
                    break;
                    }
                 // check for an exact match
                 if (collimatorList[collimator]->getColEndAngle() == 
                     collimatorList[nextCollimator]->getColStartAngle())
                    {
                    // A Match
                    collimatorList[collimator]->setColEndAngleNeighbor(
                                                        collimatorList[nextCollimator]);
                    break;
                    }
                 // check for a rotational match (2PI = 0)
                 if (
                    (collimatorList[collimator]->getColEndAngle() >= 
                     fmod(collimatorList[nextCollimator]->getColStartAngle(),TWOPI)-0.0001e0)
                 && (collimatorList[collimator]->getColEndAngle() <=
                     fmod(collimatorList[nextCollimator]->getColStartAngle(),TWOPI)+0.0001e0)
                    )
                    {
                    // A Match
                    collimatorList[collimator]->setColEndAngleNeighbor(
                                                        collimatorList[nextCollimator]);
                    break;
                    }
                 }
              for (Count nextCollimator = collimator; nextCollimator >= lowerLimit; collimator--)
                 {
                 if (collimatorList[collimator]->getColTopInnerRadius() != 
                     collimatorList[nextCollimator]->getColTopInnerRadius())
                    {
                    // we have gone beyond possible matches so quit looking
                    break;
                    }
                 // check for an exact match
                 if (collimatorList[collimator]->getColStartAngle() == 
                     collimatorList[nextCollimator]->getColEndAngle())
                    {
                    // A Match
                    collimatorList[collimator]->setColStartAngleNeighbor(
                                                        collimatorList[nextCollimator]);
                    break;
                    }
                 // check for a rotational match (2PI = 0)
                 if (
                    (collimatorList[collimator]->getColStartAngle() >= 
                     fmod(collimatorList[nextCollimator]->getColEndAngle(),TWOPI)-0.0001e0)
                 && (collimatorList[collimator]->getColStartAngle() <=
                     fmod(collimatorList[nextCollimator]->getColEndAngle(),TWOPI)+0.0001e0)
                    )
                    {
                    // A Match
                    collimatorList[collimator]->setColStartAngleNeighbor(
                                                        collimatorList[nextCollimator]);
                    break;
                    }
                 }
              }
           // BUILD THE INNER COLLIMATOR STRUCTURE
           // Find 1 collimator that has radius less than the current collimator
           // and overlaps it in rotation angle space
           for (Count match=collimator+1; match<= upperLimit; match++)
              {
              if (collimatorList[match]->getColEndAngle() <=
                                    collimatorList[collimator]->getColStartAngle())
                 {
                 // The collimator is not related to the current collimator
                 continue;
                 }
              if (collimatorList[match]->getColStartAngle() >=
                                    collimatorList[collimator]->getColEndAngle())
                 {
                 // The collimator is not related to the current collimator
                 continue;
                 }

              // We have found an inner collimator
              collimatorList[collimator]->setInnerCollimator(collimatorList[match]);
              break;
              }
           // BUILD THE OUTER COLLIMATOR STRUCTURE
           // Find 1 collimator that has radius greater than the current collimator
           // and overlaps it in rotation angle space
           if (collimator != lowerLimit)
              {
              for (int match=collimator-1; match>= (int)lowerLimit; match--)
                 {
                 if (collimatorList[match]->getColEndAngle() <=
                                       collimatorList[collimator]->getColStartAngle())
                    {
                    // The collimator is not related to the current collimator
                    continue;
                    }
                 if (collimatorList[match]->getColStartAngle() >=
                                       collimatorList[collimator]->getColEndAngle())
                    {
                    // The collimator is not related to the current collimator
                    continue;
                    }
   
                 // We have found an outer collimator
                 collimatorList[collimator]->setOuterCollimator(collimatorList[match]);
                 break;
                 }
              }
           }
       }
}

XrrtMirror* 
XrrtStructure::getOuterMirror(const Count& layer,
                              const RadiusInMM& radius,
                              const AngleInRadians& angle)
{
//
// Return a mirror which is just slightly further from the z-axis than the
// given radius and overlaps the given angle.
//
XrrtMirror* outerMirror;
int firstLayerMirror;
int lastLayerMirror;

      // First mirror in the layer
      firstLayerMirror = layerStart[layer];
      // last Mirror in the layer
      lastLayerMirror  = layerEnd[layer];

      // Search the mirror range to find one that overlaps the angle we need

      outerMirror = mirrorList[lastLayerMirror];
      //
      // Find the 1st innermost mirror in the rotation angle range
      double anglePlusTwoPI = angle + 6.283185307e0;
      do {
         // Check whether we have a mirror that overlaps the required angle
         if (outerMirror->startAngle <= angle &&
             outerMirror->endAngle >= angle)
            {
            break;
            }
         if (outerMirror->startAngle <= anglePlusTwoPI &&
             outerMirror->endAngle >=   anglePlusTwoPI)
            {
            break;
            }
         if (lastLayerMirror == firstLayerMirror)
            {
            // Somehow there is no mirror for this angle
            return 0;
            }
         lastLayerMirror--;
         outerMirror = mirrorList[lastLayerMirror];
         } while(true);
      // Dropping out here  assures us we have a mirror that matches the
      // rotation
      // Now follow out the chain until one has a radius > the needed one
      while (outerMirror != 0)
          {
          if (outerMirror->topOuterRadius > radius)
             {
             // Return this mirror
             return outerMirror;
             }
          outerMirror = outerMirror->outerMirror;
          }
      return outerMirror;

}

// Add collimator code based on getOuterMirror()
// (added by HIDEYUKI MORI)
XrrtCollimator* 
XrrtStructure::getOuterCollimator(const Count& layer,
				  const ColRadiusInMM& radius,
				  const ColAngleInRadians& angle)
{
//
// Return a collimator which is just slightly further from the z-axis than the
// given radius and overlaps the given angle.
//
XrrtCollimator* outerCollimator;
int firstLayerCollimator;
int lastLayerCollimator;

      // First collimator in the layer
      firstLayerCollimator = layerStart[layer];
      // last collimator in the layer
      lastLayerCollimator = layerEnd[layer];

      // Search the collimator range to find one that overlaps the angle we need

      outerCollimator = collimatorList[lastLayerCollimator];
      //
      // Find the 1st innermost collimator in the rotation angle range
      double anglePlusTwoPI = angle + 6.283185307e0;
      do {
         // Check whether we have a collimator that overlaps the required angle
         if (outerCollimator->colStartAngle <= angle &&
             outerCollimator->colEndAngle >= angle)
            {
            break;
            }
         if (outerCollimator->colStartAngle <= anglePlusTwoPI &&
             outerCollimator->colEndAngle >=   anglePlusTwoPI)
            {
            break;
            }
         if (lastLayerCollimator == firstLayerCollimator)
            {
            // Somehow there is no collimator for this angle
            return 0;
            }
         lastLayerCollimator--;
         outerCollimator = collimatorList[lastLayerCollimator];
         } while(true);
      // Dropping out here  assures us we have a collimator that matches the
      // rotation
      // Now follow out the chain until one has a radius > the needed one
      while (outerCollimator != 0)
          {
          if (outerCollimator->colTopOuterRadius > radius)
             {
             // Return this collimator
             return outerCollimator;
             }
          outerCollimator = outerCollimator->outerCollimator;
          }
      return outerCollimator;

}

XrrtMirror*
XrrtStructure::createMirror()
{
XrrtMirror* mirror;
     mirror = new XrrtMirror;
     mirrorList.push_back(mirror);
     return mirror;
}

XrrtObstruction*
XrrtStructure::createObstruction()
{
XrrtObstruction* obstruction;
     obstruction = new XrrtObstruction;
     shadowList.push_back(obstruction);
     return obstruction;
}

// Add collimator creation code (modified by HIDEYUKI MORI)
XrrtCollimator*
XrrtStructure::createCollimator()
{
XrrtCollimator* collimator;
     collimator = new XrrtCollimator;
     collimatorList.push_back(collimator);
     return collimator;
}

Count 
XrrtStructure::getNumberOfLayers()
{
     return numberOfLayers;
}


FPDistanceInMM 
XrrtStructure::getLayerDistance(const Count layer)
{
     return layerDistance[layer];
}

LayerType
XrrtStructure::getLayerType(const Count layer)
{
     return layerType[layer];
}

RadiusInMM 
XrrtStructure::getOuterHousingRadius() const
{
    return outerHousingRadius;
}

 RadiusInMM 
XrrtStructure::getInnerHousingRadius() const
{
    return innerHousingRadius;
}

void 
XrrtStructure::setOuterHousingRadius(const RadiusInMM& parameter)
{
     outerHousingRadius = parameter;
}

void 
XrrtStructure::setInnerHousingRadius(const RadiusInMM& parameter)
{
     innerHousingRadius = parameter;
}

double 
XrrtStructure::getFocalLengthMM() const
{
    return focalLengthInMM;
}

void
XrrtStructure::setFocalLengthMM(const double& nominalFocalLengthMM)
{
    focalLengthInMM = nominalFocalLengthMM;
}




